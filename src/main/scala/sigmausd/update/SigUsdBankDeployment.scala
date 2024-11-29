package sigmausd.update

import org.ergoplatform.{ErgoAddressEncoder, Pay2SAddress}
import org.ergoplatform.kiosk.ergo.KioskType
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import sigmastate.Values.ErgoTree
import sigmastate.eval.CompiletimeIRContext
import sigmastate.lang.{CompilerSettings, SigmaCompiler, TransformingSigmaBuilder}

object SigUsdBankDeployment extends App with SubstitutionUtils {

  override val mode = mainnetIndex
  override val substitutionMap = super.substitutionMap ++ Map.empty

  val networkPrefix = if(mode == mainnetIndex) {
    ErgoAddressEncoder.MainnetNetworkPrefix
  } else {
    ErgoAddressEncoder.TestnetNetworkPrefix
  }

  implicit val eae = new ErgoAddressEncoder(networkPrefix)

  private val compiler = SigmaCompiler(CompilerSettings(networkPrefix, TransformingSigmaBuilder, lowerMethodCalls = true))

  def compile(ergoScript: String, env: Map[String, KioskType[_]] = Map.empty): ErgoTree = {
    import sigmastate.lang.Terms._
    implicit val irContext = new CompiletimeIRContext
    compiler.compile(env.view.mapValues(_.value).toMap, ergoScript).buildTree.asBoolValue.asSigmaProp
  }

  val bankV1Script =
    s"""
      |{
      |      // this box
      |      // R4: Number of stable-coins in circulation
      |      // R5: Number of reserve-coins in circulation
      |
      |      val feePercent = 2 // in percent, so 2% fee
      |
      |      // Base-64 version of ERG/USD oracle pool NFT 011d3364de07e5a26f0c4eef0852cddb387039a921b7154ef3cab22c6eda887f
      |      // UI at https://explorer.ergoplatform.com/en/oracle-pool-state/ergusd
      |      // Got via http://tomeko.net/online_tools/hex_to_base64.php
      |      val oraclePoolNFT = fromBase16("${subst("poolNft")}")
      |
      |      // Base-64 version of bank update NFT 239c170b7e82f94e6b05416f14b8a2a57e0bfff0e3c93f4abbcd160b6a5b271a
      |      // Got via http://tomeko.net/online_tools/hex_to_base64.php
      |      val updateNFT = fromBase16("${subst("bankUpdateNft")}")
      |
      |      val coolingOffHeight = 460000
      |
      |      val minStorageRent = 10000000L
      |      val minReserveRatioPercent = 400L // percent
      |      val defaultMaxReserveRatioPercent = 800L // percent
      |      val INF = 1000000000L
      |      val LongMax = 9223372036854775807L
      |      val rcDefaultPrice = 1000000L
      |
      |      val isExchange = if (CONTEXT.dataInputs.size > 0) {  // oracle input exists
      |
      |        val dataInput = CONTEXT.dataInputs(0)
      |        val validDataInput = dataInput.tokens(0)._1 == oraclePoolNFT
      |
      |        val bankBoxIn = SELF
      |        val bankBoxOut = OUTPUTS(0)
      |
      |        val rateBox = dataInput
      |        val receiptBox = OUTPUTS(1)
      |
      |        val rate = rateBox.R4[Long].get / 100 // calculate nanoERG per US cent
      |
      |        val scCircIn = bankBoxIn.R4[Long].get
      |        val rcCircIn = bankBoxIn.R5[Long].get
      |        val bcReserveIn = bankBoxIn.value
      |
      |        val scTokensIn = bankBoxIn.tokens(0)._2
      |        val rcTokensIn = bankBoxIn.tokens(1)._2
      |
      |        val scCircOut = bankBoxOut.R4[Long].get
      |        val rcCircOut = bankBoxOut.R5[Long].get
      |        val bcReserveOut = bankBoxOut.value
      |
      |        val scTokensOut = bankBoxOut.tokens(0)._2
      |        val rcTokensOut = bankBoxOut.tokens(1)._2
      |
      |        val totalScIn = scTokensIn + scCircIn
      |        val totalScOut = scTokensOut + scCircOut
      |
      |        val totalRcIn = rcTokensIn + rcCircIn
      |        val totalRcOut = rcTokensOut + rcCircOut
      |
      |        val rcExchange = rcTokensIn != rcTokensOut
      |        val scExchange = scTokensIn != scTokensOut
      |
      |        val rcExchangeXorScExchange = (rcExchange || scExchange) && !(rcExchange && scExchange)
      |
      |        val circDelta = receiptBox.R4[Long].get
      |        val bcReserveDelta = receiptBox.R5[Long].get
      |
      |        val rcCircDelta = if (rcExchange) circDelta else 0L
      |        val scCircDelta = if (rcExchange) 0L else circDelta
      |
      |        val validDeltas = (scCircIn + scCircDelta == scCircOut) &&
      |                           (rcCircIn + rcCircDelta == rcCircOut) &&
      |                           (bcReserveIn + bcReserveDelta == bcReserveOut) &&
      |                           scCircOut >= 0 && rcCircOut >= 0
      |
      |        val coinsConserved = totalRcIn == totalRcOut && totalScIn == totalScOut
      |
      |        val tokenIdsConserved = bankBoxOut.tokens(0)._1 == bankBoxIn.tokens(0)._1 && // also ensures that at least one token exists
      |                                bankBoxOut.tokens(1)._1 == bankBoxIn.tokens(1)._1 && // also ensures that at least one token exists
      |                                bankBoxOut.tokens(2)._1 == bankBoxIn.tokens(2)._1    // also ensures that at least one token exists
      |
      |        val mandatoryRateConditions = rateBox.tokens(0)._1 == oraclePoolNFT
      |        val mandatoryBankConditions = bankBoxOut.value >= minStorageRent &&
      |                                      bankBoxOut.propositionBytes == bankBoxIn.propositionBytes &&
      |                                      rcExchangeXorScExchange &&
      |                                      coinsConserved &&
      |                                      validDeltas &&
      |                                      tokenIdsConserved
      |
      |        // exchange equations
      |        val bcReserveNeededOut = scCircOut * rate
      |        val bcReserveNeededIn = scCircIn * rate
      |        val liabilitiesIn = max(min(bcReserveIn, bcReserveNeededIn), 0)
      |        val maxReserveRatioPercent = if (HEIGHT > coolingOffHeight) defaultMaxReserveRatioPercent else INF
      |
      |        val reserveRatioPercentOut = if (bcReserveNeededOut == 0) maxReserveRatioPercent else bcReserveOut * 100 / bcReserveNeededOut
      |
      |        val validReserveRatio = if (scExchange) {
      |          if (scCircDelta > 0) {
      |            reserveRatioPercentOut >= minReserveRatioPercent
      |          } else true
      |        } else {
      |          if (rcCircDelta > 0) {
      |            reserveRatioPercentOut <= maxReserveRatioPercent
      |          } else {
      |            reserveRatioPercentOut >= minReserveRatioPercent
      |          }
      |        }
      |
      |        val brDeltaExpected = if (scExchange) { // sc
      |          val liableRate = if (scCircIn == 0) LongMax else liabilitiesIn / scCircIn
      |          val scNominalPrice = min(rate, liableRate)
      |          scNominalPrice * scCircDelta
      |        } else { // rc
      |          val equityIn = bcReserveIn - liabilitiesIn
      |          val equityRate = if (rcCircIn == 0) rcDefaultPrice else equityIn / rcCircIn
      |          val rcNominalPrice = if (equityIn == 0) rcDefaultPrice else equityRate
      |          rcNominalPrice * rcCircDelta
      |        }
      |
      |        val fee = brDeltaExpected * feePercent / 100
      |        val actualFee = if (fee < 0) {-fee} else fee
      |
      |        // actualFee is always positive, irrespective of brDeltaExpected
      |        val brDeltaExpectedWithFee = brDeltaExpected + actualFee
      |
      |        mandatoryRateConditions &&
      |         mandatoryBankConditions &&
      |         bcReserveDelta == brDeltaExpectedWithFee &&
      |         validReserveRatio &&
      |         validDataInput
      |      } else false
      |
      |      val isUpdate = INPUTS(0).tokens(0)._1 == updateNFT
      |
      |      sigmaProp(isExchange || isUpdate)
      |}
      |""".stripMargin

  val ballotScript =
    s"""
      |{
      |  // This box (ballot box):
      |  // R4 the group element of the owner of the ballot token [GroupElement]
      |  // R5 dummy Int due to AOTC non-lazy evaluation (since bank box has Long at R5). Due to the line marked ****
      |  // R6 the box id of the update box [Coll[Byte]]
      |  // R7 the value voted for [Coll[Byte]]
      |
      |  // Base-64 version of bank update NFT 239c170b7e82f94e6b05416f14b8a2a57e0bfff0e3c93f4abbcd160b6a5b271a
      |  // Got via http://tomeko.net/online_tools/hex_to_base64.php
      |  val updateNFT = fromBase16("${subst("bankUpdateNft")}")
      |
      |  val pubKey = SELF.R4[GroupElement].get
      |
      |  val index = INPUTS.indexOf(SELF, 0)
      |
      |  val output = OUTPUTS(index)
      |
      |  val isBasicCopy = output.R4[GroupElement].get == pubKey &&
      |                    output.propositionBytes == SELF.propositionBytes &&
      |                    output.tokens == SELF.tokens &&
      |                    output.value >= SELF.value
      |
      |  val castVote = proveDlog(pubKey)
      |
      |  val notVoted = output.R7[Coll[Byte]].isDefined == false
      |
      |  val update = INPUTS(0).tokens(0)._1 == updateNFT && notVoted
      |
      |  // Note: We want that during an update, a new valid vote should not be generated
      |  // notVoted ensures that the update action does not generate a new vote.
      |  // This is already prevented by having R6 contain the id of the update box,
      |  // This guarantees that R6 can never contain this box Id (because it depends on the outputs)
      |  // However, notVoted is used for additional security
      |
      |  sigmaProp(
      |    isBasicCopy && (castVote || update)
      |  )
      |}
      |""".stripMargin

  val updateScript =
    s"""
      |{
      |  // This box (update box):
      |  // Registers empty
      |  //
      |  // ballot boxes (Inputs)
      |  // R4 the pub key of voter [GroupElement] (not used here, type is [Long])
      |  // R5 dummy [Long] due to AOTC non-lazy evaluation (from the line marked **** below)
      |  // R6 the box id of this box [Coll[Byte]]
      |  // R7 the value voted for [Coll[Byte]]
      |
      |  // Base-64 version of the stablecoin bank NFT 7d672d1def471720ca5782fd6473e47e796d9ac0c138d9911346f118b2f6d9d9
      |  // Issued in https://explorer.ergoplatform.com/en/transactions/134db72906d84ea8f6d5b4dc0bbfeaed880836f36dffc4bda8254071b519000a
      |  // Got via http://tomeko.net/online_tools/hex_to_base64.php
      |  val bankNFT = fromBase16("${subst("bankNft")}")
      |
      |  // Base-64 version of the ballot token ID f7995f212216fcf21854f56df7a9a0a9fc9b7ae4c0f1cc40f5b406371286a5e0
      |  // Issued in https://explorer.ergoplatform.com/en/transactions/744f7080e0373c754f9c8174989c1307e92f4a3937799f15628b1d434d70afb9
      |  // Got via http://tomeko.net/online_tools/hex_to_base64.php
      |  val ballotTokenId = fromBase16("${subst("bankBallotTokenId")}")
      |
      |  val minVotes = ${subst("minBankUpdateVotes")}
      |
      |  // collect and update in one step
      |  val updateBoxOut = OUTPUTS(0) // copy of this box is the 1st output
      |  val validUpdateIn = SELF.id == INPUTS(0).id // this is 1st input
      |
      |  val bankBoxIn = INPUTS(1) // bank box is 2nd input
      |  val bankBoxOut = OUTPUTS(1) // copy of bank box is the 2nd output
      |
      |  // compute the hash of the bank output box. This should be the value voted for
      |  val bankBoxOutHash = blake2b256(bankBoxOut.propositionBytes)
      |
      |  val validBankIn = bankBoxIn.tokens.size == 3 && bankBoxIn.tokens(2)._1 == bankNFT
      |  val validBankOut = bankBoxIn.tokens == bankBoxOut.tokens &&
      |                     bankBoxIn.value == bankBoxOut.value &&
      |                     bankBoxIn.R4[Long].get == bankBoxOut.R4[Long].get &&
      |                     bankBoxIn.R5[Long].get == bankBoxOut.R5[Long].get
      |
      |
      |  val validUpdateOut = SELF.tokens == updateBoxOut.tokens &&
      |                       SELF.propositionBytes == updateBoxOut.propositionBytes &&
      |                       updateBoxOut.value >= SELF.value
      |
      |  def isValidBallot(b:Box) = {
      |    b.tokens.size > 0 &&
      |    b.tokens(0)._1 == ballotTokenId &&
      |    b.R6[Coll[Byte]].get == SELF.id && // ensure vote corresponds to this box ****
      |    b.R7[Coll[Byte]].get == bankBoxOutHash // check value voted for
      |  }
      |
      |  val ballotBoxes = INPUTS.filter(isValidBallot)
      |
      |  val votesCount = ballotBoxes.fold(0L, {(accum: Long, b: Box) => accum + b.tokens(0)._2})
      |
      |  sigmaProp(validBankIn && validBankOut && validUpdateIn && validUpdateOut && votesCount >= minVotes)
      |}
      |""".stripMargin

  val bankV1Tree = compile(bankV1Script)
  val bankV1TreeHash = Base16.encode(Blake2b256.hash(bankV1Tree.bytes))
  val bankV1Address = Pay2SAddress(bankV1Tree)

  val ballotTree = compile(ballotScript)
  val ballotAddress = Pay2SAddress(ballotTree)

  val bankUpdateTree = compile(updateScript)
  val bankUpdateAddress = Pay2SAddress(bankUpdateTree)

  println("Bank V1 address: " + bankV1Address)
  println("Ballot address: " + ballotAddress)
  println("Bank update address: " + bankUpdateAddress)
}

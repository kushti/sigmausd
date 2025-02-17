package sigmausd.update

import org.ergoplatform.ErgoBox.{R4, R5, R6, R7}
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress, Pay2SAddress}
import org.ergoplatform.kiosk.ergo.KioskType
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import sigmastate.Values.{ByteArrayConstant, ErgoTree, GroupElementConstant, IntConstant, LongConstant, Tuple}
import sigmastate.eval.CompiletimeIRContext
import sigmastate.lang.{CompilerSettings, SigmaCompiler, TransformingSigmaBuilder}
import sigmastate.serialization.ValueSerializer
import sigmausd.ScanUtils
import sigmausd.update.TestnetPoolV1DeploymentAndUpdate.serializeValue

object SigUsdBankDeploymentAndUpdate extends App with ScanUtils with SubstitutionUtils {

  override val mode = testnetIndex
  override val substitutionMap = super.substitutionMap

  val networkPrefix = if(mode == mainnetIndex) {
    ErgoAddressEncoder.MainnetNetworkPrefix
  } else {
    ErgoAddressEncoder.TestnetNetworkPrefix
  }

  val serverUrl: String = if (mode == mainnetIndex) {
    "http://127.0.0.1:9053"
  } else {
    "http://127.0.0.1:9053"
  }

  val bankBoxScanId: Int = if (mode == mainnetIndex) {
    0 // todo : set
  } else {
    29
  }

  val updateBoxScanId: Int = if (mode == mainnetIndex) {
    0 // todo : set
  } else {
    30
  }

  val ballotBoxScanId: Int = if (mode == mainnetIndex) {
    0 // todo : set
  } else {
    31
  }


  implicit val eae = new ErgoAddressEncoder(networkPrefix)

  private val compiler = SigmaCompiler(CompilerSettings(networkPrefix, TransformingSigmaBuilder, lowerMethodCalls = true))

  def compile(ergoScript: String, env: Map[String, KioskType[_]] = Map.empty): ErgoTree = {
    import sigmastate.lang.Terms._
    implicit val irContext = new CompiletimeIRContext
    compiler.compile(env.view.mapValues(_.value).toMap, ergoScript).buildTree.asBoolValue.asSigmaProp
  }

  val bankV2Script =
    s"""
      |{
      |      // this box
      |      // R4: Number of stable-coins in circulation
      |      // R5: Number of reserve-coins in circulation
      |
      |      val feePercent = 2 // in percent, so 2% fee
      |
      |      val oraclePoolNFT = fromBase16("${subst("poolNft")}")
      |
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
      |  //
      |  // Tokens:
      |  //   bank update NFT
      |  //
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
      |  val minVotes = 3
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

  val bankV2Tree = compile(bankV2Script)
  val bankV2TreeHash = Base16.encode(Blake2b256.hash(bankV2Tree.bytes))
  val bankV2Address = Pay2SAddress(bankV2Tree)

  val ballotTree = compile(ballotScript)
  val ballotAddress = Pay2SAddress(ballotTree)

  val bankUpdateTree = compile(updateScript)
  val bankUpdateAddress = Pay2SAddress(bankUpdateTree)

  val bankV3Script =
    s"""
      |{
      |      // SigmaUSD bank V2 contract.
      |      // Ballot & update contracts are the same for V2.
      |
      |      // diff from V1:
      |      //  * no cooling off period
      |      //  * per-epoch minting limits are added for both SigUSD and SigRSV. For that, new register, R7, is used, to
      |      //    store number of SigUSD and SigRSV tokens still allowed minted in the epoch (oracle update epoch). Also,
      |      //    register R6 is used to store height of current oracle epoch start.
      |
      |
      |      // this box
      |      // R4: Long : Number of stable-coins in circulation
      |      // R5: Long : Number of reserve-coins in circulation
      |      // R6: Int  : Last oracle update height (to have limits per cycle)
      |      // R7: (Long, Long) : Remaining limit for SigUsd and SigRSV minting per oracle update (added in V2)
      |
      |      val feePercent = 2 // in percent, so 2% fee
      |
      |      val oraclePoolNFT = fromBase16("${subst("poolNft")}")
      |
      |      val updateNFT = fromBase16("${subst("bankUpdateNft")}")
      |
      |      val minStorageRent = 10000000L
      |      val minReserveRatioPercent = 400L // percent
      |      val maxReserveRatioPercent = 800L // percent
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
      |        // allowed to exchange stablecoins or reservecoins but not both
      |        val rcExchangeXorScExchange = (rcExchange || scExchange) && !(rcExchange && scExchange)
      |
      |        val circDelta = receiptBox.R4[Long].get
      |        val bcReserveDelta = receiptBox.R5[Long].get
      |
      |        val rcCircDelta = if (rcExchange) circDelta else 0L
      |        val scCircDelta = if (rcExchange) 0L else circDelta
      |
      |        // V2 code below
      |        val oracleUpdateHeight = rateBox.R5[Int].get
      |        val limitFactor = 200 // 1 / 200, so 0.5% per oracle update
      |        val limitsReg = bankBoxIn.R7[(Long, Long)].get
      |        val limits = if (bankBoxIn.R6[Int].get != oracleUpdateHeight) {
      |          val limit = bankBoxIn.value / limitFactor
      |          (limit, limit)
      |        } else {
      |          limitsReg
      |        }
      |
      |        val updLimits = if (scExchange && scCircDelta > 0) { // SC mint
      |          (limits._1 - bcReserveDelta, limits._2)
      |        } else if (rcExchange && rcCircDelta > 0) { // RC mint
      |          (limits._1, limits._2 - bcReserveDelta)
      |        } else {
      |          (limits._1, limits._2)
      |        }
      |
      |        val properLimit = (updLimits._1 >= 0 && updLimits._2 >= 0) && (bankBoxOut.R7[(Long, Long)].get == updLimits)
      |
      |        val validDeltas = (scCircIn + scCircDelta == scCircOut) &&
      |                           (rcCircIn + rcCircDelta == rcCircOut) &&
      |                           (bcReserveIn + bcReserveDelta == bcReserveOut) &&
      |                           scCircOut >= 0 && rcCircOut >= 0 &&
      |                           properLimit
      |
      |        val coinsConserved = totalRcIn == totalRcOut && totalScIn == totalScOut
      |
      |        val tokenIdsConserved = bankBoxOut.tokens(0)._1 == bankBoxIn.tokens(0)._1 && // also ensures that at least one token exists
      |                                bankBoxOut.tokens(1)._1 == bankBoxIn.tokens(1)._1 && // also ensures that at least one token exists
      |                                bankBoxOut.tokens(2)._1 == bankBoxIn.tokens(2)._1    // also ensures that at least one token exists
      |
      |        val mandatoryRateConditions = rateBox.tokens(0)._1 == oraclePoolNFT
      |        val mandatoryBankConditions = bankBoxOut.value >= minStorageRent &&
      |                                      bankBoxOut.R6[Int].get == oracleUpdateHeight &&
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

  val bankV3Tree = compile(bankV3Script)
  val bankV3TreeHash = Blake2b256.hash(bankV3Tree.bytes)
  val bankV3Address = Pay2SAddress(bankV3Tree)

  val updatedBankTreeHash = bankV2TreeHash
  val updatedBankAddress = bankV2Address

  def bankV2DeploymentRequest(): String = {
    val zero = Base16.encode(ValueSerializer.serialize(LongConstant(0L)))
    s"""
       |  [
       |    {
       |      "address": "$bankV2Address",
       |      "value": 1000000000,
       |      "assets": [
       |        {
       |          "tokenId": "${subst("sigUSD")}",
       |          "amount": 10000000000001
       |        },
       |        {
       |          "tokenId": "${subst("sigRSV")}",
       |          "amount": 10000000000001
       |        },
       |        {
       |          "tokenId": "${subst("bankNft")}",
       |          "amount": 1
       |        }
       |      ],
       |      "registers": {
       |        "R4": "$zero",
       |        "R5": "$zero"
       |      }
       |    }
       |  ]
       |""".stripMargin
  }

  def bankUpdateDeploymentRequest(): String = {
    s"""
       |  [
       |    {
       |      "address": "$bankUpdateAddress",
       |      "value": 10000000000,
       |      "assets": [
       |        {
       |          "tokenId": "${subst("bankUpdateNft")}",
       |          "amount": 1
       |        }
       |      ]
       |    }
       |  ]
       |""".stripMargin
  }

  def voteForUpdateToV3Request(voterAddress: String): String = {
    val updateBox = fetchSingleBox(serverUrl, updateBoxScanId, includeUnconfirmed = false)
    val updateBoxId = serializeValue(ByteArrayConstant(updateBox.get.id))

    val voterPubKey = serializeValue(GroupElementConstant(eae.fromString(voterAddress).get.asInstanceOf[P2PKAddress].pubkey.value))
    val zero = serializeValue(LongConstant(0L))
    val encodedUpdatedBankTreeHash = serializeValue(ByteArrayConstant(Base16.decode(updatedBankTreeHash).get))

    s"""
       |  [
       |    {
       |      "address": "$ballotAddress",
       |      "value": 10000000000,
       |      "assets": [
       |        {
       |          "tokenId": "${subst("bankBallotTokenId")}",
       |          "amount": 1
       |        }
       |      ],
       |      "registers": {
       |        "R4": "$voterPubKey",
       |        "R5": "$zero",
       |        "R6": "$updateBoxId",
       |        "R7": "$encodedUpdatedBankTreeHash"
       |      }
       |    }
       |  ]
       |""".stripMargin
  }

  def updatetoV3DeploymentRequest(): String = {
    val bankBox = fetchSingleBox(serverUrl, bankBoxScanId, includeUnconfirmed = false)
    val updateBox = fetchSingleBox(serverUrl, updateBoxScanId, includeUnconfirmed = false)
    val ballotBoxes = fetchScanBoxes(serverUrl, ballotBoxScanId, includeUnconfirmed = false)
    // inputs:
    // #0 - update
    // #1 - bank
    // #2,3,4 - votes
    // #5 - fee provider
    // outputs:
    // #0 - update
    // #1 - bank
    // #2,3,4 - votes
    // #5 - fee

    val updateInput = updateBox.get
    val bankInput = bankBox.get

    val feeProviderInput = "80f591a5250008cd024cea00b0c06a80f49c233a8b25217a14c5be53df1bc04630caf3241ec2b145a99fd75b000033dc0447ff0e62e3eec3b8c5a2419db54fe131d3e6087310386cc0a0d2b54b5800"

    val inputs = (Seq(updateInput, bankInput) ++ ballotBoxes).map(_.bytes).map(Base16.encode) ++ Seq(feeProviderInput)
    val inputsString = inputs.map(s => "\"" + s + "\"").mkString(", ")

    s"""
      |{
      |  "requests": [
      |    {
      |      "address": "${Pay2SAddress(updateInput.ergoTree)}",
      |      "value": ${updateInput.value + 10000000000L},
      |      "assets": [
      |        {
      |          "tokenId": "${Base16.encode(updateInput.additionalTokens(0)._1.toArray)}",
      |          "amount": ${updateInput.additionalTokens(0)._2}
      |        }
      |      ]
      |    },
      |    {
      |      "address": "${updatedBankAddress}",
      |      "value": ${bankInput.value},
      |      "assets": [
      |        {
      |          "tokenId": "${Base16.encode(bankInput.additionalTokens(0)._1.toArray)}",
      |          "amount": ${bankInput.additionalTokens(0)._2}
      |        },
      |        {
      |          "tokenId": "${Base16.encode(bankInput.additionalTokens(1)._1.toArray)}",
      |          "amount": ${bankInput.additionalTokens(1)._2}
      |        },
      |        {
      |          "tokenId": "${Base16.encode(bankInput.additionalTokens(2)._1.toArray)}",
      |          "amount": ${bankInput.additionalTokens(2)._2}
      |        }
      |      ],
      |      "registers": {
      |        "R4": "${serializeValue(bankInput.additionalRegisters(R4))}",
      |        "R5": "${serializeValue(bankInput.additionalRegisters(R5))}",
      |        "R6_COMMENTED_OUT": "${serializeValue(IntConstant(0))}",
      |        "R7_COMMENTED_OUT": "${serializeValue(Tuple(LongConstant(0), LongConstant(0)))}"
      |      }
      |    },
      |    {
      |      "address": "${Pay2SAddress(ballotBoxes(0).ergoTree)}",
      |      "value": ${ballotBoxes(0).value},
      |      "assets": [
      |        {
      |          "tokenId": "${Base16.encode(ballotBoxes(0).additionalTokens(0)._1.toArray)}",
      |          "amount": ${ballotBoxes(0).additionalTokens(0)._2}
      |        }
      |      ],
      |      "registers": {
      |        "R4": "${serializeValue(ballotBoxes(0).additionalRegisters(R4))}"
      |      }
      |    },
      |    {
      |      "address": "${Pay2SAddress(ballotBoxes(1).ergoTree)}",
      |      "value": ${ballotBoxes(1).value},
      |      "assets": [
      |        {
      |          "tokenId": "${Base16.encode(ballotBoxes(1).additionalTokens(0)._1.toArray)}",
      |          "amount": ${ballotBoxes(1).additionalTokens(0)._2}
      |        }
      |      ],
      |      "registers": {
      |        "R4": "${serializeValue(ballotBoxes(1).additionalRegisters(R4))}"
      |      }
      |    },
      |    {
      |      "address": "${Pay2SAddress(ballotBoxes(2).ergoTree)}",
      |      "value": ${ballotBoxes(2).value},
      |      "assets": [
      |        {
      |          "tokenId": "${Base16.encode(ballotBoxes(2).additionalTokens(0)._1.toArray)}",
      |          "amount": ${ballotBoxes(2).additionalTokens(0)._2}
      |        }
      |      ],
      |      "registers": {
      |        "R4": "${serializeValue(ballotBoxes(2).additionalRegisters(R4))}"
      |      }
      |    }
      |  ],
      |  "fee": 10000000,
      |  "inputsRaw": [
      |    $inputsString
      |  ],
      |  "dataInputsRaw": []
      |}
      |""".stripMargin
  }

  def voteForUpdateToTheSameVersionRequest(voterAddress: String): String = {
    val updateBox = fetchSingleBox(serverUrl, updateBoxScanId, includeUnconfirmed = false)
    val updateBoxId = serializeValue(ByteArrayConstant(updateBox.get.id))

    val voterPubKey = serializeValue(GroupElementConstant(eae.fromString(voterAddress).get.asInstanceOf[P2PKAddress].pubkey.value))
    val zero = serializeValue(LongConstant(0L))
    val encodedUpdatedBankTreeHash = serializeValue(ByteArrayConstant(Base16.decode(updatedBankTreeHash).get))

    s"""
       |  [
       |    {
       |      "address": "$ballotAddress",
       |      "value": 10000000000,
       |      "assets": [
       |        {
       |          "tokenId": "${subst("bankBallotTokenId")}",
       |          "amount": 1
       |        }
       |      ],
       |      "registers": {
       |        "R4": "$voterPubKey",
       |        "R5": "$zero",
       |        "R6": "$updateBoxId",
       |        "R7": "$encodedUpdatedBankTreeHash"
       |      }
       |    }
       |  ]
       |""".stripMargin
  }

  def updateToTheSameVersionDeploymentRequest(): String = {
    val bankBox = fetchSingleBox(serverUrl, bankBoxScanId, includeUnconfirmed = false)
    val updateBox = fetchSingleBox(serverUrl, updateBoxScanId, includeUnconfirmed = false)
    val ballotBoxes = fetchScanBoxes(serverUrl, ballotBoxScanId, includeUnconfirmed = false)
    // inputs:
    // #0 - update
    // #1 - bank
    // #2,3,4 - votes
    // #5 - fee provider
    // outputs:
    // #0 - update
    // #1 - bank
    // #2,3,4 - votes
    // #5 - fee

    val updateInput = updateBox.get
    val bankInput = bankBox.get

    val feeProviderInput = "80f591a5250008cd024cea00b0c06a80f49c233a8b25217a14c5be53df1bc04630caf3241ec2b145a99fd75b000033dc0447ff0e62e3eec3b8c5a2419db54fe131d3e6087310386cc0a0d2b54b5800"

    val inputs = (Seq(updateInput, bankInput) ++ ballotBoxes).map(_.bytes).map(Base16.encode) ++ Seq(feeProviderInput)
    val inputsString = inputs.map(s => "\"" + s + "\"").mkString(", ")

    s"""
       |{
       |  "requests": [
       |    {
       |      "address": "${Pay2SAddress(updateInput.ergoTree)}",
       |      "value": ${updateInput.value + 10000000000L},
       |      "assets": [
       |        {
       |          "tokenId": "${Base16.encode(updateInput.additionalTokens(0)._1.toArray)}",
       |          "amount": ${updateInput.additionalTokens(0)._2}
       |        }
       |      ]
       |    },
       |    {
       |      "address": "${updatedBankAddress}",
       |      "value": ${bankInput.value},
       |      "assets": [
       |        {
       |          "tokenId": "${Base16.encode(bankInput.additionalTokens(0)._1.toArray)}",
       |          "amount": ${bankInput.additionalTokens(0)._2}
       |        },
       |        {
       |          "tokenId": "${Base16.encode(bankInput.additionalTokens(1)._1.toArray)}",
       |          "amount": ${bankInput.additionalTokens(1)._2}
       |        },
       |        {
       |          "tokenId": "${Base16.encode(bankInput.additionalTokens(2)._1.toArray)}",
       |          "amount": ${bankInput.additionalTokens(2)._2}
       |        }
       |      ],
       |      "registers": {
       |        "R4": "${serializeValue(bankInput.additionalRegisters(R4))}",
       |        "R5": "${serializeValue(bankInput.additionalRegisters(R5))}",
       |        "R6_COMMENTED_OUT": "${serializeValue(IntConstant(0))}",
       |        "R7_COMMENTED_OUT": "${serializeValue(Tuple(LongConstant(0), LongConstant(0)))}"
       |      }
       |    },
       |    {
       |      "address": "${Pay2SAddress(ballotBoxes(0).ergoTree)}",
       |      "value": ${ballotBoxes(0).value},
       |      "assets": [
       |        {
       |          "tokenId": "${Base16.encode(ballotBoxes(0).additionalTokens(0)._1.toArray)}",
       |          "amount": ${ballotBoxes(0).additionalTokens(0)._2}
       |        }
       |      ],
       |      "registers": {
       |        "R4": "${serializeValue(ballotBoxes(0).additionalRegisters(R4))}"
       |      }
       |    },
       |    {
       |      "address": "${Pay2SAddress(ballotBoxes(1).ergoTree)}",
       |      "value": ${ballotBoxes(1).value},
       |      "assets": [
       |        {
       |          "tokenId": "${Base16.encode(ballotBoxes(1).additionalTokens(0)._1.toArray)}",
       |          "amount": ${ballotBoxes(1).additionalTokens(0)._2}
       |        }
       |      ],
       |      "registers": {
       |        "R4": "${serializeValue(ballotBoxes(1).additionalRegisters(R4))}"
       |      }
       |    },
       |    {
       |      "address": "${Pay2SAddress(ballotBoxes(2).ergoTree)}",
       |      "value": ${ballotBoxes(2).value},
       |      "assets": [
       |        {
       |          "tokenId": "${Base16.encode(ballotBoxes(2).additionalTokens(0)._1.toArray)}",
       |          "amount": ${ballotBoxes(2).additionalTokens(0)._2}
       |        }
       |      ],
       |      "registers": {
       |        "R4": "${serializeValue(ballotBoxes(2).additionalRegisters(R4))}"
       |      }
       |    }
       |  ],
       |  "fee": 10000000,
       |  "inputsRaw": [
       |    $inputsString
       |  ],
       |  "dataInputsRaw": []
       |}
       |""".stripMargin
  }


  println("Updated bank tree hash: " + updatedBankTreeHash)
  println("Bank V2 address: " + bankV2Address)
  println("Ballot address: " + ballotAddress)
  println("Bank update address: " + bankUpdateAddress)
  println("===========================================")
  println("Bank V2 deployment request: ")
  println(bankV2DeploymentRequest())
  println("Bank update deployment request: ")
  println(bankUpdateDeploymentRequest())

  println("Vote for update to V3 deployment requests: ")
  println("kushti: ")
  println(voteForUpdateToV3Request("3WwC5mGC717y3ztqRS7asAUoUdci8BBKDnJt98vxetHDUAMABLNd"))

  println("Bank update to V3 (for /wallet/transaction/send ): ")
  println(updatetoV3DeploymentRequest())

  println("Vote for update to the same version deployment requests: ")
  println("kushti: ")
  println(voteForUpdateToTheSameVersionRequest("3WwC5mGC717y3ztqRS7asAUoUdci8BBKDnJt98vxetHDUAMABLNd"))

  println("Bank update to the same version (for /wallet/transaction/send ): ")
  println(updateToTheSameVersionDeploymentRequest())

}

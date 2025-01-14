package sigmausd.update

import org.ergoplatform.ErgoBox.{R4, R5}
import org.ergoplatform.kiosk.ergo.KioskType
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress, Pay2SAddress}
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import sigmastate.SType
import sigmastate.Values.{ByteArrayConstant, ErgoTree, EvaluatedValue, GroupElementConstant, IntConstant, LongConstant, Tuple}
import sigmastate.eval.CompiletimeIRContext
import sigmastate.lang.{CompilerSettings, SigmaCompiler, TransformingSigmaBuilder}
import sigmastate.serialization.ValueSerializer
import sigmausd.update.SigUsdBankDeploymentAndUpdate.{fetchScanBoxes, fetchSingleBox}


object TestnetPoolV1DeploymentAndUpdate extends App with SubstitutionUtils {

  override val mode = testnetIndex // mainnet mode

  val serverUrl: String = if (mode == mainnetIndex) {
    null
  } else {
    "http://127.0.0.1:9053"
  }

  val networkPrefix = if(mode == mainnetIndex) {
    ErgoAddressEncoder.MainnetNetworkPrefix
  } else {
    ErgoAddressEncoder.TestnetNetworkPrefix
  }

  val updateBoxScanId: Int = if (mode == mainnetIndex) {
    0 // todo : set
  } else {
    0  // todo : set
  }

  val poolBoxScanId: Int = if (mode == mainnetIndex) {
    0 // todo : set
  } else {
    29
  }

  val ballotBoxScanId: Int = if (mode == mainnetIndex) {
    0 // todo : set
  } else {
    30
  }

  implicit val eae = new ErgoAddressEncoder(networkPrefix)

  private val compiler = SigmaCompiler(CompilerSettings(networkPrefix, TransformingSigmaBuilder, lowerMethodCalls = true))

  def compile(ergoScript: String, env: Map[String, KioskType[_]] = Map.empty): ErgoTree = {
    import sigmastate.lang.Terms._
    implicit val irContext = new CompiletimeIRContext
    compiler.compile(env.view.mapValues(_.value).toMap, ergoScript).buildTree.asBoolValue.asSigmaProp
  }

  val liveEpochContract =
    s"""
      |{ // This box:
      |      // R4: The latest finalized datapoint (from the previous epoch)
      |      // R5: Block height that the current epoch will finish on
      |      // R6: Address of the "Epoch Preparation" stage contract.
      |
      |      // Oracle box:
      |      // R4: Public key (group element)
      |      // R5: Epoch box Id (this box's Id)
      |      // R6: Data point
      |
      |      // Base-64 version of the oracle (participant) token 8c27dd9d8a35aac1e3167d58858c0a8b4059b277da790552e37eba22df9b9035
      |      // Got via http://tomeko.net/online_tools/hex_to_base64.php
      |      val oracleTokenId = fromBase16("${subst("oracleTokenId")}")
      |
      |      // Note that in the next update, the current reward of 250000 should be increased to at least 5000000 to cover various costs
      |
      |      val oracleBoxes = CONTEXT.dataInputs.filter{(b:Box) =>
      |        b.R5[Coll[Byte]].get == SELF.id &&
      |        b.tokens(0)._1 == oracleTokenId
      |      }
      |
      |      val pubKey = oracleBoxes.map{(b:Box) => proveDlog(b.R4[GroupElement].get)}(OUTPUTS(1).R4[Int].get)
      |
      |      val sum = oracleBoxes.fold(0L, { (t:Long, b: Box) => t + b.R6[Long].get })
      |
      |      val average = sum / oracleBoxes.size
      |
      |      val firstOracleDataPoint = oracleBoxes(0).R6[Long].get
      |
      |      def getPrevOracleDataPoint(index:Int) = if (index <= 0) firstOracleDataPoint else oracleBoxes(index - 1).R6[Long].get
      |
      |      val rewardAndOrderingCheck = oracleBoxes.fold((1, true), {
      |          (t:(Int, Boolean), b:Box) =>
      |             val currOracleDataPoint = b.R6[Long].get
      |             val prevOracleDataPoint = getPrevOracleDataPoint(t._1 - 1)
      |
      |             (t._1 + 1, t._2 &&
      |                        OUTPUTS(t._1).propositionBytes == proveDlog(b.R4[GroupElement].get).propBytes &&
      |                        OUTPUTS(t._1).value >= 250000 &&  // oracleReward = 250000
      |                        prevOracleDataPoint >= currOracleDataPoint
      |             )
      |         }
      |      )
      |
      |      val lastDataPoint = getPrevOracleDataPoint(rewardAndOrderingCheck._1 - 1)
      |      val firstDataPoint = oracleBoxes(0).R6[Long].get
      |
      |      val delta = firstDataPoint * 5 / 100  // maxDeviation = 5
      |
      |      val epochPrepScriptHash = SELF.R6[Coll[Byte]].get
      |
      |      sigmaProp(
      |        blake2b256(OUTPUTS(0).propositionBytes) == epochPrepScriptHash &&
      |        oracleBoxes.size >= ${subst("minOracleBoxes")} && // minOracleBoxes = 4
      |        OUTPUTS(0).tokens == SELF.tokens &&
      |        OUTPUTS(0).R4[Long].get == average &&
      |        OUTPUTS(0).R5[Int].get == SELF.R5[Int].get + 6 && // epochPeriod = 6  = 4 (live) + 2 (prep) blocks
      |        OUTPUTS(0).value >= SELF.value - (oracleBoxes.size + 1) * 250000 &&  // oracleReward = 250000
      |        rewardAndOrderingCheck._2 &&
      |        lastDataPoint >= firstDataPoint - delta
      |      ) && pubKey
      |    }
      |""".stripMargin

  def epochPreparationScript(poolUpdateNft: String): String =
    s"""
      |  {
      |      // This box:
      |      // R4: The finalized data point from collection
      |      // R5: Height the epoch will end
      |
      |      // Base-64 version of the hash of the live-epoch script (above) 77dffd47b690caa52fe13345aaf64ecdf7d55f2e7e3496e8206311f491aa46cd
      |      val liveEpochScriptHash = fromBase16("${subst("liveEpochTreeHash")}")
      |
      |      // Base-64 version of the update NFT 720978c041239e7d6eb249d801f380557126f6324e12c5ba9172d820be2e1dde
      |      // Got via http://tomeko.net/online_tools/hex_to_base64.php
      |      val updateNFT = fromBase16("$poolUpdateNft")
      |
      |      val canStartEpoch = HEIGHT > SELF.R5[Int].get - 4 // livePeriod = 4 blocks
      |      val epochNotOver = HEIGHT < SELF.R5[Int].get
      |      val epochOver = HEIGHT >= SELF.R5[Int].get
      |      val enoughFunds = SELF.value >= 5000000 // minPoolBoxValue = 5000000
      |
      |      val maxNewEpochHeight = HEIGHT + 6 + 2 // epochPeriod = 6 = 4 (live) + 2 (prep) blocks; buffer = 2 blocks
      |      val minNewEpochHeight = HEIGHT + 6 // epochPeriod = 6 = 4 (live) + 2 (prep) blocks
      |
      |      val poolAction = if (OUTPUTS(0).R6[Coll[Byte]].isDefined) {
      |        val isliveEpochOutput = OUTPUTS(0).R6[Coll[Byte]].get == blake2b256(SELF.propositionBytes) &&
      |                                blake2b256(OUTPUTS(0).propositionBytes) == liveEpochScriptHash
      |        ( // start next epoch
      |          epochNotOver && canStartEpoch && enoughFunds &&
      |          OUTPUTS(0).tokens == SELF.tokens &&
      |          OUTPUTS(0).value >= SELF.value &&
      |          OUTPUTS(0).R4[Long].get == SELF.R4[Long].get &&
      |          OUTPUTS(0).R5[Int].get == SELF.R5[Int].get &&
      |          isliveEpochOutput
      |        ) || ( // create new epoch
      |          epochOver &&
      |          enoughFunds &&
      |          OUTPUTS(0).tokens == SELF.tokens &&
      |          OUTPUTS(0).value >= SELF.value &&
      |          OUTPUTS(0).tokens == SELF.tokens &&
      |          OUTPUTS(0).value >= SELF.value &&
      |          OUTPUTS(0).R4[Long].get == SELF.R4[Long].get &&
      |          OUTPUTS(0).R5[Int].get >= minNewEpochHeight &&
      |          OUTPUTS(0).R5[Int].get <= maxNewEpochHeight &&
      |          isliveEpochOutput
      |        )
      |      } else {
      |        ( // collect funds
      |          OUTPUTS(0).propositionBytes == SELF.propositionBytes &&
      |          OUTPUTS(0).tokens == SELF.tokens &&
      |          OUTPUTS(0).value > SELF.value &&
      |          OUTPUTS(0).R4[Long].get == SELF.R4[Long].get &&
      |          OUTPUTS(0).R5[Int].get == SELF.R5[Int].get
      |        )
      |      }
      |
      |      val updateAction = INPUTS(0).tokens(0)._1 == updateNFT
      |
      |      sigmaProp(poolAction || updateAction)
      |    }
      |""".stripMargin


  def poolDepositScript =
    s"""
      |     {
      |      val allFundingBoxes = INPUTS.filter{(b:Box) =>
      |        b.propositionBytes == SELF.propositionBytes
      |      }
      |
      |      // Base-64 version of the pool NFT 011d3364de07e5a26f0c4eef0852cddb387039a921b7154ef3cab22c6eda887f
      |      // Got via http://tomeko.net/online_tools/hex_to_base64.php
      |      val poolNFT = fromBase16("${subst("poolNft")}")
      |
      |      val totalFunds = allFundingBoxes.fold(0L, { (t:Long, b: Box) => t + b.value })
      |
      |      sigmaProp(
      |        INPUTS(0).tokens(0)._1 == poolNFT &&
      |        OUTPUTS(0).propositionBytes == INPUTS(0).propositionBytes &&
      |        OUTPUTS(0).value >= INPUTS(0).value + totalFunds &&
      |        OUTPUTS(0).tokens == INPUTS(0).tokens
      |      )
      |    }
      |""".stripMargin

  def datapointScript =
    s"""
      | {
      |      // This box:
      |      // R4: The address of the oracle (never allowed to change after bootstrap).
      |      // R5: The box id of the latest Live Epoch box.
      |      // R6: The oracle's datapoint.
      |
      |      // Base-64 version of the pool NFT 011d3364de07e5a26f0c4eef0852cddb387039a921b7154ef3cab22c6eda887f
      |      // Got via http://tomeko.net/online_tools/hex_to_base64.php
      |      val poolNFT = fromBase16("${subst("poolNft")}")
      |
      |      val pubKey = SELF.R4[GroupElement].get
      |
      |      val poolBox = CONTEXT.dataInputs(0)
      |
      |      // Allow datapoint box to contain box id of any box with pool NFT (i.e., either Live Epoch or Epoch Prep boxes)
      |      // Earlier we additionally required that the box have the live epoch script.
      |      // In summary:
      |      //    Earlier: (1st data-input has pool NFT) && (1st data-input has live epoch script)
      |      //    Now:     (1st data-input has pool NFT)
      |      //
      |      val validPoolBox = poolBox.tokens(0)._1 == poolNFT
      |
      |      sigmaProp(
      |        OUTPUTS(0).R4[GroupElement].get == pubKey &&
      |        OUTPUTS(0).R5[Coll[Byte]].get == poolBox.id &&
      |        OUTPUTS(0).R6[Long].get > 0 &&
      |        OUTPUTS(0).propositionBytes == SELF.propositionBytes &&
      |        OUTPUTS(0).tokens == SELF.tokens &&
      |        validPoolBox
      |      ) && proveDlog(pubKey)
      |    }
      |""".stripMargin

  def ballotScript(poolUpdateNft: String): String =
    s"""
       | { // This box (ballot box):
       |      // R4 the group element of the owner of the ballot token [GroupElement]
       |      // R5 dummy Int due to AOTC non-lazy evaluation (since pool box has Int at R5). Due to the line marked ****
       |      // R6 the box id of the update box [Coll[Byte]]
       |      // R7 the value voted for [Coll[Byte]]
       |
       |      // Base-64 version of the update NFT 720978c041239e7d6eb249d801f380557126f6324e12c5ba9172d820be2e1dde
       |      // Got via http://tomeko.net/online_tools/hex_to_base64.php
       |      val updateNFT = fromBase64("$poolUpdateNft")
       |
       |      val pubKey = SELF.R4[GroupElement].get
       |
       |      val index = INPUTS.indexOf(SELF, 0)
       |
       |      val output = OUTPUTS(index)
       |
       |      val isBasicCopy = output.R4[GroupElement].get == pubKey &&
       |                        output.propositionBytes == SELF.propositionBytes &&
       |                        output.tokens == SELF.tokens &&
       |                        output.value >= 10000000 // minStorageRent
       |
       |      sigmaProp(
       |        isBasicCopy && (
       |          proveDlog(pubKey) || (
       |             INPUTS(0).tokens(0)._1 == updateNFT &&
       |             output.value >= SELF.value
       |          )
       |        )
       |      )
       |    }
       |""".stripMargin

  def updateScript =
    s"""
      | { // This box (update box):
      |      // Registers empty
      |      //
      |      // ballot boxes (Inputs)
      |      // R4 the pub key of voter [GroupElement] (not used here)
      |      // R5 dummy int due to AOTC non-lazy evaluation (from the line marked ****)
      |      // R6 the box id of this box [Coll[Byte]]
      |      // R7 the value voted for [Coll[Byte]]
      |
      |      // Base-64 version of the pool NFT 011d3364de07e5a26f0c4eef0852cddb387039a921b7154ef3cab22c6eda887f
      |      // Got via http://tomeko.net/online_tools/hex_to_base64.php
      |      val poolNFT = fromBase16("${subst("poolNft")}")
      |
      |      // Base-64 version of the ballot token ID 053fefab5477138b760bc7ae666c3e2b324d5ae937a13605cb766ec5222e5518
      |      // Got via http://tomeko.net/online_tools/hex_to_base64.php
      |      val ballotTokenId = fromBase64("${subst("ballotToken")}")
      |
      |      // collect and update in one step
      |      val updateBoxOut = OUTPUTS(0) // copy of this box is the 1st output
      |      val validUpdateIn = SELF.id == INPUTS(0).id // this is 1st input
      |
      |      val poolBoxIn = INPUTS(1) // pool box is 2nd input
      |      val poolBoxOut = OUTPUTS(1) // copy of pool box is the 2nd output
      |
      |      // compute the hash of the pool output box. This should be the value voted for
      |      val poolBoxOutHash = blake2b256(poolBoxOut.propositionBytes)
      |
      |      val validPoolIn = poolBoxIn.tokens(0)._1 == poolNFT
      |      val validPoolOut = poolBoxIn.tokens == poolBoxOut.tokens &&
      |                         poolBoxIn.value == poolBoxOut.value &&
      |                         poolBoxIn.R4[Long].get == poolBoxOut.R4[Long].get &&
      |                         poolBoxIn.R5[Int].get == poolBoxOut.R5[Int].get
      |
      |
      |      val validUpdateOut = SELF.tokens == updateBoxOut.tokens &&
      |                           SELF.propositionBytes == updateBoxOut.propositionBytes &&
      |                           SELF.value >= updateBoxOut.value // ToDo: change in next update
      |      // Above line contains a (non-critical) bug:
      |      // Instead of
      |      //    SELF.value >= updateBoxOut.value
      |      // we should have
      |      //    updateBoxOut.value >= SELF.value
      |      //
      |      // In the next oracle pool update, this should be fixed
      |      // Until then, this has no impact because this box can only be spent in an update
      |      // In summary, the next update will involve (at the minimum)
      |      //    1. New update contract (with above bugfix)
      |      //    2. New updateNFT (because the updateNFT is locked to this contract)
      |
      |      def isValidBallot(b:Box) = {
      |        b.tokens.size > 0 &&
      |        b.tokens(0)._1 == ballotTokenId &&
      |        b.R6[Coll[Byte]].get == SELF.id && // ensure vote corresponds to this box ****
      |        b.R7[Coll[Byte]].get == poolBoxOutHash // check value voted for
      |      }
      |
      |      val ballotBoxes = INPUTS.filter(isValidBallot)
      |
      |      val votesCount = ballotBoxes.fold(0L, {(accum: Long, b: Box) => accum + b.tokens(0)._2})
      |
      |      sigmaProp(validPoolIn && validPoolOut && validUpdateIn && validUpdateOut && votesCount >= 8) // minVotes = 8
      |    }
      |""".stripMargin

  def updateScriptPreV2 =
    s"""
      | { // This box (update box):
      |      // Registers empty
      |      //
      |      // ballot boxes (Inputs)
      |      // R4 the pub key of voter [GroupElement] (not used here)
      |      // R5 dummy int due to AOTC non-lazy evaluation (from the line marked ****)
      |      // R6 the box id of this box [Coll[Byte]]
      |      // R7 the value voted for [Coll[Byte]]
      |
      |      // Base-64 version of the pool NFT 011d3364de07e5a26f0c4eef0852cddb387039a921b7154ef3cab22c6eda887f
      |      // Got via http://tomeko.net/online_tools/hex_to_base64.php
      |      val poolNFT = fromBase64("${subst("poolNft")}")
      |
      |      // Base-64 version of the ballot token ID 053fefab5477138b760bc7ae666c3e2b324d5ae937a13605cb766ec5222e5518
      |      // Got via http://tomeko.net/online_tools/hex_to_base64.php
      |      val ballotTokenId = fromBase64("${subst("ballotToken")}")
      |
      |      // collect and update in one step
      |      val updateBoxOut = OUTPUTS(0) // copy of this box is the 1st output
      |      val validUpdateIn = SELF.id == INPUTS(0).id // this is 1st input
      |
      |      val poolBoxIn = INPUTS(1) // pool box is 2nd input
      |      val poolBoxOut = OUTPUTS(1) // copy of pool box is the 2nd output
      |
      |      // compute the hash of the pool output box. This should be the value voted for
      |      val poolBoxOutHash = blake2b256(poolBoxOut.propositionBytes)
      |
      |      val validPoolIn = poolBoxIn.tokens(0)._1 == poolNFT
      |      val validPoolOut = poolBoxOut.tokens(0)._1 == poolNFT && // changed line
      |                         poolBoxIn.value == poolBoxOut.value &&
      |                         poolBoxIn.R4[Long].get == poolBoxOut.R4[Long].get &&
      |                         poolBoxIn.R5[Int].get == poolBoxOut.R5[Int].get
      |
      |
      |      val validUpdateOut = SELF.tokens == updateBoxOut.tokens &&
      |                           SELF.propositionBytes == updateBoxOut.propositionBytes &&
      |                           updateBoxOut.value >= SELF.value // changed line
      |
      |      def isValidBallot(b:Box) = {
      |        b.tokens.size > 0 &&
      |        b.tokens(0)._1 == ballotTokenId &&
      |        b.R6[Coll[Byte]].get == SELF.id && // ensure vote corresponds to this box ****
      |        b.R7[Coll[Byte]].get == poolBoxOutHash // check value voted for
      |      }
      |
      |      val ballotBoxes = INPUTS.filter(isValidBallot)
      |
      |      val votesCount = ballotBoxes.fold(0L, {(accum: Long, b: Box) => accum + b.tokens(0)._2})
      |
      |      sigmaProp(validPoolIn && validPoolOut && validUpdateIn && validUpdateOut && votesCount >= 8) // minVotes = 8
      |    }
      |""".stripMargin

  val liveEpochTree = compile(liveEpochContract)
  val liveEpochTreeHash = Base16.encode(Blake2b256.hash(liveEpochTree.bytes))
  println("Live epoch tree hash: " + liveEpochTreeHash)
  val liveEpochAddress = Pay2SAddress(liveEpochTree)

  val epochPreparationTree = compile(epochPreparationScript(subst("poolUpdateNft")))
  val epochPreparationTreeHash = Base16.encode(Blake2b256.hash(epochPreparationTree.bytes))
  val epochPreparationAddress = Pay2SAddress(epochPreparationTree)

  val poolDepositTree = compile(poolDepositScript)
  val poolDepositAddress = Pay2SAddress(poolDepositTree)

  val datapointTree = compile(datapointScript)
  val datapointAddress = Pay2SAddress(datapointTree)

  val updateTree = compile(updateScript)
  val updateAddress = Pay2SAddress(updateTree)

  println("Live epoch address: " + liveEpochAddress)
  println("Epoch preparation address: " + epochPreparationAddress)
  println("Pool deposit address: " + poolDepositAddress)
  println("Datapoint address: " + datapointAddress)
  println("Pool update address: " + updateAddress)

  val oracleTokenId = if(mode == mainnetIndex){
    substitutionMap("oracleTokenId")._1
  } else {
    substitutionMap("oracleTokenId")._2
  }

  def serializeValue(v: EvaluatedValue[_ <: SType]) = {
    Base16.encode(ValueSerializer.serialize(v))
  }
  val dummyLiveEpochBoxId = serializeValue(ByteArrayConstant(Array(1.toByte)))
  val dummyDatapoint = serializeValue(LongConstant(531049159L))

  def datapointContractDeploymentRequest(participantAddress: String): String = {
    val participantPubKey = serializeValue(GroupElementConstant(eae.fromString(participantAddress).get.asInstanceOf[P2PKAddress].pubkey.value))

    s"""
       |  [
       |    {
       |      "address": "$datapointAddress",
       |      "value": 1000000000,
       |      "assets": [
       |        {
       |          "tokenId": "${subst("oracleTokenId")}",
       |          "amount": 1
       |        }
       |      ],
       |      "registers": {
       |        "R4": "$participantPubKey",
       |        "R5": "$dummyLiveEpochBoxId",
       |        "R6": "$dummyDatapoint"
       |      }
       |    }
       |  ]
       |""".stripMargin
  }


  def epochPreparationDeploymentRequest(): String = {
    s"""
       |  [
       |    {
       |      "address": "$epochPreparationAddress",
       |      "value": 10000000000,
       |      "assets": [
       |        {
       |          "tokenId": "${subst("poolNft")}",
       |          "amount": 1
       |        }
       |      ],
       |      "registers": {
       |        "R4": "$dummyDatapoint",
       |        "R5": "0480b518"
       |      }
       |    }
       |  ]
       |""".stripMargin
  }

  def poolUpdateDeploymentRequest(): String = {
    s"""
       |  [
       |    {
       |      "address": "$updateAddress",
       |      "value": 1000000000,
       |      "assets": [
       |        {
       |          "tokenId": "${subst("poolUpdateNft")}",
       |          "amount": 1
       |        }
       |      ]
       |    }
       |  ]
       |""".stripMargin
  }


  println("Epoch Preparation deployment request: ")
  println(epochPreparationDeploymentRequest())

  println("------------------------------")
  println("Datapoint deployment requests: ")
  println(datapointContractDeploymentRequest("3WvjmwdM9Lupn7fXPMB2uojweHwQQiLzdLSo1XRo3tgVCoBfL4ny"))
  println(datapointContractDeploymentRequest("3WwC5mGC717y3ztqRS7asAUoUdci8BBKDnJt98vxetHDUAMABLNd"))

  println("------------------------------")
  println("Pool update deployment request: ")
  println(poolUpdateDeploymentRequest())

  println("=================Pool v1 update data: ================================")
  val updateTreePreV2 = compile(updateScriptPreV2)
  val updateAddressPreV2 = Pay2SAddress(updateTreePreV2)
  println("Pool update address pre V2: " + updateAddressPreV2)

  val epochPreparationPreV2Tree = compile(epochPreparationScript(subst("poolUpdatePreV2Nft")))
  val epochPreparationTreePreV2Hash = Base16.encode(Blake2b256.hash(epochPreparationPreV2Tree.bytes))
  val epochPreparationPreV2Address = Pay2SAddress(epochPreparationPreV2Tree)

  def voteForPreV2UpdateDeploymentRequest(voterAddress: String): String = {

    val ballotTree = compile(ballotScript(subst("poolUpdateNft")))
    val ballotAddress = Pay2SAddress(ballotTree)

    val updateBox = fetchSingleBox(serverUrl, updateBoxScanId, includeUnconfirmed = false)
    val updateBoxId = serializeValue(ByteArrayConstant(updateBox.get.id))

    val voterPubKey = serializeValue(GroupElementConstant(eae.fromString(voterAddress).get.asInstanceOf[P2PKAddress].pubkey.value))
    val zero = serializeValue(LongConstant(0L))
    val encodedEpochPreparationTreeHash = serializeValue(ByteArrayConstant(Base16.decode(epochPreparationTreePreV2Hash).get))

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
       |        "R7": "$encodedEpochPreparationTreeHash"
       |      }
       |    }
       |  ]
       |""".stripMargin
  }


  println("Epoch preparation address pre V2: " + epochPreparationPreV2Address)

  def poolUpdatePreV2DeploymentRequest(): String = {
    s"""
       |  [
       |    {
       |      "address": "$updateAddressPreV2",
       |      "value": 1000000000,
       |      "assets": [
       |        {
       |          "tokenId": "${subst("poolUpdatePreV2Nft")}",
       |          "amount": 1
       |        }
       |      ]
       |    }
       |  ]
       |""".stripMargin
  }


  def updateDeploymentRequest(): String = {
    val epochPrepBox = fetchSingleBox(serverUrl, poolBoxScanId, includeUnconfirmed = false)
    val updateBox = fetchSingleBox(serverUrl, updateBoxScanId, includeUnconfirmed = false)
    val ballotBoxes = fetchScanBoxes(serverUrl, ballotBoxScanId, includeUnconfirmed = false)
    // inputs:
    // #0 - update
    // #1 - epoch prep
    // #2,3,4 - votes
    // #5 - fee provider
    // outputs:
    // #0 - update
    // #1 - epoch prep
    // #2,3,4 - votes
    // #5 - fee

    val updateInput = updateBox.get
    val poolInput = epochPrepBox.get

    val feeProviderInput = "80f591a5250008cd024cea00b0c06a80f49c233a8b25217a14c5be53df1bc04630caf3241ec2b145a99fd75b000033dc0447ff0e62e3eec3b8c5a2419db54fe131d3e6087310386cc0a0d2b54b5800"

    val inputs = (Seq(updateInput, poolInput) ++ ballotBoxes).map(_.bytes).map(Base16.encode) ++ Seq(feeProviderInput)
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
       |      "address": "${epochPreparationPreV2Address}",
       |      "value": ${poolInput.value},
       |      "assets": [
       |        {
       |          "tokenId": "${Base16.encode(poolInput.additionalTokens(0)._1.toArray)}",
       |          "amount": ${poolInput.additionalTokens(0)._2}
       |        }
       |      ],
       |      "registers": {
       |        "R4": "${serializeValue(poolInput.additionalRegisters(R4))}",
       |        "R5": "${serializeValue(poolInput.additionalRegisters(R5))}"
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

  println("------------------------------")
  println("Pool update PreV2 deployment request: ")
  println(poolUpdatePreV2DeploymentRequest())

  println("Vote for pool update to preV2 deployment requests: ")

  println("kushti: ")
  println(voteForPreV2UpdateDeploymentRequest("3WwC5mGC717y3ztqRS7asAUoUdci8BBKDnJt98vxetHDUAMABLNd"))

  println("Michael: ")
  println(voteForPreV2UpdateDeploymentRequest("3Wvd1hML9DLxNJEbS1VuDuwgsZeNcyoBtyGqvheiQodFxpZBoz2b"))

  println("Pool update (for /wallet/transaction/send ): ")
  println(updateDeploymentRequest())
}

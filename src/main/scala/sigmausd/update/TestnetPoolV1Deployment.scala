package sigmausd.update

import org.ergoplatform.kiosk.ergo.KioskType
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress, Pay2SAddress}
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16
import sigmastate.SType
import sigmastate.Values.{ByteArrayConstant, Constant, ErgoTree, GroupElementConstant, LongConstant}
import sigmastate.eval.CompiletimeIRContext
import sigmastate.lang.{CompilerSettings, SigmaCompiler, TransformingSigmaBuilder}
import sigmastate.serialization.ValueSerializer


object TestnetPoolV1Deployment extends App with SubstitutionUtils {

  override val mode = testnetIndex // mainnet mode

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

  val epochPreparationScript =
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
      |      val updateNFT = fromBase16("${subst("poolUpdateNft")}")
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

  val liveEpochTree = compile(liveEpochContract)
  val liveEpochTreeHash = Base16.encode(Blake2b256.hash(liveEpochTree.bytes))
  println("Live epoch tree hash: " + liveEpochTreeHash)
  val liveEpochAddress = Pay2SAddress(liveEpochTree)

  val epochPreparationTree = compile(epochPreparationScript)
  val epochPreparationTreeHash = Base16.encode(Blake2b256.hash(epochPreparationTree.bytes))
  val epochPreparationAddress = Pay2SAddress(epochPreparationTree)

  val poolDepositTree = compile(poolDepositScript)
  val poolDepositAddress = Pay2SAddress(poolDepositTree)

  val datapointTree = compile(datapointScript)
  val datapointAddress = Pay2SAddress(datapointTree)

  println("Live epoch address: " + liveEpochAddress)
  println("Epoch preparation address: " + epochPreparationAddress)
  println("Pool deposit address: " + poolDepositAddress)
  println("Datapoint address: " + datapointAddress)

  val oracleTokenId = if(mode == mainnetIndex){
    substitutionMap("oracleTokenId")._1
  } else {
    substitutionMap("oracleTokenId")._2
  }

  def serializeValue(v: Constant[_ <: SType]) = {
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

  println("Epoch Preparation deployment request: ")
  println(epochPreparationDeploymentRequest())
  println("------------------------------")
  println("Datapoint deployment requests: ")
  println(datapointContractDeploymentRequest("3WvjmwdM9Lupn7fXPMB2uojweHwQQiLzdLSo1XRo3tgVCoBfL4ny"))
  println(datapointContractDeploymentRequest("3WwC5mGC717y3ztqRS7asAUoUdci8BBKDnJt98vxetHDUAMABLNd"))
  println("------------------------------")
}

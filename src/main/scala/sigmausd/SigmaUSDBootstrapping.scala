package sigmausd

import org.ergoplatform.kiosk.encoding.ScalaErgoConverters.{getAddressFromErgoTree, getStringFromAddress}
import org.ergoplatform.ErgoAddressEncoder.MainnetNetworkPrefix
import org.ergoplatform.kiosk.script.ScriptUtil
import scorex.util.encode.Base64
import sigmausd.ScriptUtil.compile
import org.ergoplatform.kiosk.ergo._

object SigmaUSDBootstrapping extends ContractUtils {
  import TokenIds.Mainnet._
  override def defaultSubstitutionMap: Map[String, String] = Map(
    "bankNFT"       -> bankNFT,
    "ballotTokenId" -> ballotTokenId
  ).view.mapValues(hex => Base64.encode(hex.decodeHex)).toMap

  val networkPrefix = MainnetNetworkPrefix

  val bankV1Script   = readContract("v1/bank.es")
  val bankV1ErgoTree = compile(Map(), bankV1Script)
  val bankV1Address  = getStringFromAddress(getAddressFromErgoTree(bankV1ErgoTree))

  val bankV2Script   = readContract("v2/bank.es")
  val bankV2ErgoTree = ScriptUtil.compile(Map(), bankV2Script)
  val bankV2Address  = getStringFromAddress(getAddressFromErgoTree(bankV2ErgoTree))

  val updateScript   = readContract("v1/update.es")
  val updateErgoTree = ScriptUtil.compile(Map(), updateScript)
  val updateAddress  = getStringFromAddress(getAddressFromErgoTree(updateErgoTree))

  val ballotScript   = readContract("v1/ballot.es")
  val ballotErgoTree = ScriptUtil.compile(Map(), ballotScript)
  val ballotAddress  = getStringFromAddress(getAddressFromErgoTree(ballotErgoTree))
}

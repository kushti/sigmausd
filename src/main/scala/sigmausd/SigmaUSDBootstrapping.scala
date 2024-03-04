package sigmausd

import sigmausd.ScriptUtil.{getAddressFromErgoTree, getStringFromAddress}
import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix

object SigmaUSDBootstrapping extends ContractUtils with App {
  override def defaultSubstitutionMap: Map[String, String] = Map.empty

  val networkPrefix = TestnetNetworkPrefix

  val bankV1Script = readContract("contracts/v1/bank.es")
  val bankV1ErgoTree = ScriptUtil.compile(Map(), bankV1Script)
  val bankV1Address = getStringFromAddress(getAddressFromErgoTree(bankV1ErgoTree))

  val bankV2Script = readContract("contracts/v2/bank.es")
  val bankV2ErgoTree = ScriptUtil.compile(Map(), bankV2Script)
  val bankV2Address = getStringFromAddress(getAddressFromErgoTree(bankV2ErgoTree))

  val updateScript = readContract("contracts/v1/update.es")
  val updateErgoTree = ScriptUtil.compile(Map(), updateScript)
  val updateAddress = getStringFromAddress(getAddressFromErgoTree(updateErgoTree))

  val ballotScript = readContract("contracts/v1/ballot.es")
  val ballotErgoTree = ScriptUtil.compile(Map(), ballotScript)
  val ballotAddress = getStringFromAddress(getAddressFromErgoTree(ballotErgoTree))
}

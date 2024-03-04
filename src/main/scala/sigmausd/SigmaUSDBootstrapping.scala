package sigmausd

import sigmausd.ScriptUtil.{getAddressFromErgoTree, getStringFromAddress}
import org.ergoplatform.ErgoAddressEncoder.TestnetNetworkPrefix

object SigmaUSDBootstrapping extends ContractUtils with App {
  override def defaultSubstitutionMap: Map[String, String] = Map.empty

  val networkPrefix = TestnetNetworkPrefix

  val bankV1Script = readContract("contracts/v1/bank.es")
  val bankV1ErgoTree = ScriptUtil.compile(Map(), bankV1Script)
  val bankV1Address = getStringFromAddress(getAddressFromErgoTree(bankV1ErgoTree))

  val bankV2Script = readContract("contracts/v1/bank.es")

  val updateScript = readContract("contracts/v1/update.es")
  val ballotScript = readContract("contracts/v1/ballot.es")
}

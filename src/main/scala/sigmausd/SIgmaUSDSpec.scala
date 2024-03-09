package sigmausd

import org.ergoplatform.kiosk.script.ScriptUtil
import org.ergoplatform.kiosk.encoding.ScalaErgoConverters.{getAddressFromErgoTree, getStringFromAddress}
import sigmausd.SigmaUSDBootstrapping.readContract

object SigmaUSDSpec {
  val bankScript = readContract("bank/bank.es")
  val bankErgoTree = ScriptUtil.compile(Map(), bankScript)
  val bankAddress = getStringFromAddress(getAddressFromErgoTree(bankErgoTree))

  val ballotScript = readContract("bank/update/ballot.es")
  val ballotErgoTree = ScriptUtil.compile(Map(), ballotScript)
  val ballotAddress = getStringFromAddress(getAddressFromErgoTree(ballotErgoTree))

  val updateScript = readContract("bank/update/update.es")
  val updateErgoTree = ScriptUtil.compile(Map(), updateScript)
  val updateAddress = getStringFromAddress(getAddressFromErgoTree(updateErgoTree))
}

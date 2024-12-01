package sigmausd

import io.circe.parser.parse
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoTreePredef}
import scalaj.http.{Http, HttpOptions}
import org.ergoplatform.http.api.ApiCodecs

trait ScanUtils extends ApiCodecs {

  val apiPass: String = "hello"

  def getJsonAsString(url: String): String = {
    Http(s"$url")
      .header("Content-Type", "application/json")
      .header("Accept", "application/json")
      .header("Charset", "UTF-8")
      .header("api_key", apiPass)
      .option(HttpOptions.readTimeout(10000))
      .asString
      .body
  }

  def postString(url: String, data: String): String = {
    Http(s"$url")
      .header("Content-Type", "application/json")
      .header("Accept", "application/json")
      .header("Charset", "UTF-8")
      .header("api_key", apiPass)
      .option(HttpOptions.readTimeout(10000))
      .postData(data)
      .asString
      .body
  }

  def feeOut(creationHeight: Int): ErgoBoxCandidate = {
    new ErgoBoxCandidate(2000000, ErgoTreePredef.feeProposition(720), creationHeight) // 0.002 ERG
  }

  def fetchScanBoxes(serverUrl: String, scanId: Int, minHeight: Int, maxHeight: Int, includeUnconfirmed: Boolean = false): Seq[ErgoBox] = {
    val minConf = if (includeUnconfirmed) -1 else 0
    val scanUnspentUrl = s"$serverUrl/scan/unspentBoxes/$scanId?minConfirmations=$minConf&maxConfirmations=-1&minInclusionHeight=$minHeight&maxInclusionHeight=$maxHeight"
    val boxesUnspentJson = parse(getJsonAsString(scanUnspentUrl)).toOption.get
    boxesUnspentJson.\\("box").map(_.as[ErgoBox].toOption.get)
  }

  def fetchScanBoxes(serverUrl: String, scanId: Int, includeUnconfirmed: Boolean): Seq[ErgoBox] = {
    fetchScanBoxes(serverUrl, scanId, 0, -1, includeUnconfirmed)
  }

  def fetchSingleBox(serverUrl: String, scanId: Int, includeUnconfirmed: Boolean = false): Option[ErgoBox] = {
    val bs = fetchScanBoxes(serverUrl, scanId, includeUnconfirmed)
    println(bs.map(_.creationHeight))
    bs.headOption
  }

  def currentHeight(serverUrl: String): Int = {
    val infoUrl = s"$serverUrl/info"
    val json = parse(getJsonAsString(infoUrl)).toOption.get
    json.\\("fullHeight").head.asNumber.get.toInt.get
  }

}

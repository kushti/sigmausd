package sigmausd.update

import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress, Pay2SAddress}
import scorex.util.encode.Base16
import sigmastate.SType
import sigmastate.Values.{ByteArrayConstant, Constant, GroupElementConstant, LongConstant}
import sigmastate.serialization.ValueSerializer

object TestnetPoolV1Deployment extends App {
  val mae = new ErgoAddressEncoder(ErgoAddressEncoder.MainnetNetworkPrefix)
  val tae = new ErgoAddressEncoder(ErgoAddressEncoder.TestnetNetworkPrefix)

  def mainToTestnet(p2s: String): Pay2SAddress = {
    val script = mae.fromString(p2s).get.asInstanceOf[Pay2SAddress].script
    Pay2SAddress(script)(tae)
  }

  // todo : testnet id
  val participantTokenId = ""

  val datapointMainnetAddress = "AucEQEJ3Y5Uhmu4o8dnoztRiAKKTErrhugq6EyACQ4SrK7NFAds5u9B93Xvb7heGC9oGL88F8muu6T6MARfqKpHS2ce1jZ6x8Ju6j9n4AvWkcQxBaLUq36wHGKmiCqRDtKT5tbhZ7hQbK7WuMAejKD7aW91yTBrBNHAXDVmZznmYjzJqDQGuPMsRHQSYGGyW5H2p"
  val dataPointTestnetAddress = mainToTestnet(datapointMainnetAddress)

  def serializeValue(v: Constant[_ <: SType]) = {
    Base16.encode(ValueSerializer.serialize(v))
  }
  val participantAddress = "3WvjmwdM9Lupn7fXPMB2uojweHwQQiLzdLSo1XRo3tgVCoBfL4ny"
  val participantPubKey = serializeValue(GroupElementConstant(tae.fromString(participantAddress).get.asInstanceOf[P2PKAddress].pubkey.value))

  val dummyBoxId = serializeValue(ByteArrayConstant(Array(1.toByte)))

  val dummyDatapoint = serializeValue(LongConstant(0))

  def datapointContractDeploymentRequest(): String = {
    s"""
       |  [
       |    {
       |      "address": "$dataPointTestnetAddress",
       |      "value": 1000000000,
       |      "assets": [
       |        {
       |          "tokenId": "$participantTokenId",
       |          "amount": 1
       |        }
       |      ],
       |      "registers": {
       |        "R4": "$participantPubKey",
       |        "R5": "$dummyBoxId",
       |        "R6": "$dummyDatapoint"
       |      }
       |    }
       |  ]
       |""".stripMargin
  }

  println(datapointContractDeploymentRequest())

}

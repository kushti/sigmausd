package sigmausd.update

trait SubstitutionUtils {

  val mainnetIndex = 0
  val testnetIndex = 1

  def mode: Int

  // mainnet - testnet
  def substitutionMap: Map[String, (String, String)] = Map(
    "oracleTokenId" -> ("8c27dd9d8a35aac1e3167d58858c0a8b4059b277da790552e37eba22df9b9035", "02a0e09e3e0d576613f5dc2d70002b92f0ee26d74c0cd536b030f16c1a13e485"),
    "minOracleBoxes" -> ("4", "1"),
    "poolNft" -> ("011d3364de07e5a26f0c4eef0852cddb387039a921b7154ef3cab22c6eda887f", "319ed40043bdded7bad9cf20bb3feff9a56533c9b80d004c79a535b6cf5141d0"),
    "poolUpdateNft" -> ("720978c041239e7d6eb249d801f380557126f6324e12c5ba9172d820be2e1dde", "00052cfea4e20f035368796a5269d38302a491141c02f0e22b8319753b61eb90"),
    "bankUpdateNft" -> ("239c170b7e82f94e6b05416f14b8a2a57e0bfff0e3c93f4abbcd160b6a5b271a", "01edc7f077a77c7b3bbc939bdb450d1f706c5e5111b660702d818cefb61f1e34"),
    "ballotToken" -> ("", "0317ecf0303dc2f2a4a828e6758ee706813b022f6d8f7ef02651865e4e80e859"),
    "liveEpochTreeHash" -> ("77dffd47b690caa52fe13345aaf64ecdf7d55f2e7e3496e8206311f491aa46cd", "700db65fdc4cacbc62cc8e7e53755095f13d65e7f256a0181b70774542883246"),
    "bankNft" -> ("7d672d1def471720ca5782fd6473e47e796d9ac0c138d9911346f118b2f6d9d9","009aa15a042877fca3f661d8358b27c01aeac984c99317e4bd6e0272fe885688"),
    "bankBallotTokenId" -> ("f7995f212216fcf21854f56df7a9a0a9fc9b7ae4c0f1cc40f5b406371286a5e0","02918759a7a9a141238d0774192deef35b28f5b74bd5162bd0195726ccdec1bd"),
    "sigUSD" -> ("", "03a5ad7df46d21ebdfd92525f3408ceb327eb1b59ea3028dd56c05867bec58ea"),
    "sigRSV" -> ("" , "03c9bec68a6653c8c555b37cd51154457474f2ceb566dbb2211dab92ca52ea7c")
  )

  def subst(name: String): String = {
    val t = substitutionMap.apply(name)
    if(mode == mainnetIndex) {
      t._1
    } else {
      t._2
    }
  }
}
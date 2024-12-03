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
    "ballotToken" -> ("", "0317ecf0303dc2f2a4a828e6758ee706813b022f6d8f7ef02651865e4e80e859"),
    "liveEpochTreeHash" -> ("77dffd47b690caa52fe13345aaf64ecdf7d55f2e7e3496e8206311f491aa46cd", "700db65fdc4cacbc62cc8e7e53755095f13d65e7f256a0181b70774542883246"),
    "bankNft" -> ("7d672d1def471720ca5782fd6473e47e796d9ac0c138d9911346f118b2f6d9d9","0263999560febd4fd10415756cb3ed80ef635ee95fe0d9ff155727bb222611ac"),
    "bankUpdateNft" -> ("239c170b7e82f94e6b05416f14b8a2a57e0bfff0e3c93f4abbcd160b6a5b271a", "02da13c5f686f3dd7de87d6b9b1cba78948202e2a717e330ec54b77497b8e966"),
    "bankBallotTokenId" -> ("f7995f212216fcf21854f56df7a9a0a9fc9b7ae4c0f1cc40f5b406371286a5e0","04638725712fe3247f2f0a37d892fcd28f2c30dd99cdade09405feef6e4f2b29"),
    "sigUSD" -> ("", "02ed1301626dcf8b65fb8431789fdf2e23b0904abcaff10cf5d9efe050e9e2d5"),
    "sigRSV" -> ("" , "044d568da5a3cc4fd675c0ebf9bbfa7ad0cd44a0fc24cd58fa1e60b046b2c973")
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
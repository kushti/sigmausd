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
     // todo: change poolUpdatePreV2Nft for the mainnet
    "poolUpdatePreV2Nft" -> ("720978c041239e7d6eb249d801f380557126f6324e12c5ba9172d820be2e1dde", "1954f12ef8eecc10cea74819fd6af7195da9668f480b5e7a0b82d53de489daf8"), //todo: update
    "ballotToken" -> ("", "0317ecf0303dc2f2a4a828e6758ee706813b022f6d8f7ef02651865e4e80e859"),
    "liveEpochTreeHash" -> ("77dffd47b690caa52fe13345aaf64ecdf7d55f2e7e3496e8206311f491aa46cd", "700db65fdc4cacbc62cc8e7e53755095f13d65e7f256a0181b70774542883246"),
    "bankNft" -> ("7d672d1def471720ca5782fd6473e47e796d9ac0c138d9911346f118b2f6d9d9","07081517aab35aebf3db36b5509e85927ae3f52236c8f6def5b02d00261e4015"),
    "bankUpdateNft" -> ("239c170b7e82f94e6b05416f14b8a2a57e0bfff0e3c93f4abbcd160b6a5b271a", "07456e775941c7d51281e951f087e78ba2b7db877ac2a9a6f62e9a327797dcbe"),
    "bankBallotTokenId" -> ("f7995f212216fcf21854f56df7a9a0a9fc9b7ae4c0f1cc40f5b406371286a5e0","074db8edaf8a60b86940c2bdf59fc62f0f407e5a5776a4dc20d1a3b9ba628f48"),
    "sigUSD" -> ("", "0762c7455c8a2309954e74899939bb8ce567b968346c4ec99b3e1d7bce1b55e3"),
    "sigRSV" -> ("" , "098437daa2edcf0933db47aa135d3195a2477a1e581f9f0ffbe96803a5690cbb")
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
package sigmausd.update

trait SubstitutionUtils {

  val mainnetIndex = 0
  val testnetIndex = 1

  def mode: Int

  // mainnet - testnet
  def substitutionMap: Map[String, (String, String)] = Map(
    "oracleTokenId" -> ("8c27dd9d8a35aac1e3167d58858c0a8b4059b277da790552e37eba22df9b9035", "00915ed57851c4dbf7a898d392178d2cdbbc4ca2c6c886e34e7be7b33f5e9a9c"),
    "minOracleBoxes" -> ("4", "1"),
    "poolNft" -> ("011d3364de07e5a26f0c4eef0852cddb387039a921b7154ef3cab22c6eda887f", "00edf3dda53d41fe6784923f5875db6410c788eb5fa8d10fa38d5f874ff405e8"),
    "poolUpdateNft" -> ("720978c041239e7d6eb249d801f380557126f6324e12c5ba9172d820be2e1dde", "0114a69a9a3e848c0b8aa1e6c8c62d9041f7d67ff3c15714ace15ce841230441"),
     // todo: change poolUpdatePreV2Nft for the mainnet
    "poolUpdatePreV2Nft" -> ("720978c041239e7d6eb249d801f380557126f6324e12c5ba9172d820be2e1dde", "020e49b69492434f3f08c05227a303570d28767f510bfed7afed861bc2dc0cb2"), //todo: update
    "ballotToken" -> ("", "011aeb1b030af40d1717985f708a31b9e5d00c41ce476f4429b6eaac15b52230"),
    "liveEpochTreeHash" -> ("77dffd47b690caa52fe13345aaf64ecdf7d55f2e7e3496e8206311f491aa46cd", "b9c2bef328e233bcea5195592e011e5fda55045e37c043c7208c0877bdd5405b"),
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
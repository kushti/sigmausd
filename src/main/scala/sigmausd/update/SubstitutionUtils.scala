package sigmausd.update

trait SubstitutionUtils {

  val mainnetIndex = 0
  val testnetIndex = 1

  def mode: Int

  // mainnet - testnet
  def substitutionMap: Map[String, (String, String)] = Map(
    // pool related tokens
    "oracleTokenId" -> ("8c27dd9d8a35aac1e3167d58858c0a8b4059b277da790552e37eba22df9b9035", "006120a981a74d79b273fe7f25b7d1410bcabb7d74d7603a4e9f577520b6c792"),
    "minOracleBoxes" -> ("4", "1"),
    "poolNft" -> ("011d3364de07e5a26f0c4eef0852cddb387039a921b7154ef3cab22c6eda887f", "0e42bac1f979edcc1e019aa33e4eedd783967ef4ed6bbb4375ee069ac126a076"),
    "poolUpdateNft" -> ("720978c041239e7d6eb249d801f380557126f6324e12c5ba9172d820be2e1dde", "0fc3f2ca467f89f5aeb173f01354ca42a6692e30df352aa3e88116765380044e"),
    "poolUpdatePreV2Nft" -> ("d332043f73cfc772ee5e263e739d3b5cf7480431b0c86fc2bbb2c0b0865b2443", "111e55ccba2bc64e36c4dbbd445bf1bfb6370dd89f0f6ec839daf9a0a200b516"),
    "ballotToken" -> ("053fefab5477138b760bc7ae666c3e2b324d5ae937a13605cb766ec5222e5518", "1317f4d9eea21dce0eeeb0fbfb6c34120178af4f76800c1c1aa35662862d7320"),
    "liveEpochTreeHash" -> ("77dffd47b690caa52fe13345aaf64ecdf7d55f2e7e3496e8206311f491aa46cd", "a063359380dc1193edcd81af7425b99e50f3c9098be128440a6aa4c41c93aeba"),

    // bank related tokens
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
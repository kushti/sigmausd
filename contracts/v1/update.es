{
  // This box (update box):
  // Registers empty
  //
  // ballot boxes (Inputs)
  // R4 the pub key of voter [GroupElement] (not used here)
  // R5 dummy int due to AOTC non-lazy evaluation (from the line marked **** below)
  // R6 the box id of this box [Coll[Byte]]
  // R7 the value voted for [Coll[Byte]]

  // Base-64 version of the stablecoin bank NFT 7d672d1def471720ca5782fd6473e47e796d9ac0c138d9911346f118b2f6d9d9
  // Issued in https://explorer.ergoplatform.com/en/transactions/134db72906d84ea8f6d5b4dc0bbfeaed880836f36dffc4bda8254071b519000a
  // Got via http://tomeko.net/online_tools/hex_to_base64.php
  val bankNFT = fromBase64("$bankNFT")

  // Base-64 version of the ballot token ID f7995f212216fcf21854f56df7a9a0a9fc9b7ae4c0f1cc40f5b406371286a5e0
  // Issued in https://explorer.ergoplatform.com/en/transactions/744f7080e0373c754f9c8174989c1307e92f4a3937799f15628b1d434d70afb9
  // Got via http://tomeko.net/online_tools/hex_to_base64.php
  val ballotTokenId = fromBase64("$ballotTokenId")

  val minVotes = 3

  // collect and update in one step
  val updateBoxOut = OUTPUTS(0) // copy of this box is the 1st output
  val validUpdateIn = SELF.id == INPUTS(0).id // this is 1st input

  val bankBoxIn = INPUTS(1) // bank box is 2nd input
  val bankBoxOut = OUTPUTS(1) // copy of bank box is the 2nd output

  // compute the hash of the bank output box. This should be the value voted for
  val bankBoxOutHash = blake2b256(bankBoxOut.propositionBytes)

  val validBankIn = bankBoxIn.tokens.size == 3 && bankBoxIn.tokens(2)._1 == bankNFT
  val validBankOut = bankBoxIn.tokens == bankBoxOut.tokens &&
                     bankBoxIn.value == bankBoxOut.value &&
                     bankBoxIn.R4[Long].get == bankBoxOut.R4[Long].get &&
                     bankBoxIn.R5[Long].get == bankBoxOut.R5[Long].get


  val validUpdateOut = SELF.tokens == updateBoxOut.tokens &&
                       SELF.propositionBytes == updateBoxOut.propositionBytes &&
                       updateBoxOut.value >= SELF.value

  def isValidBallot(b:Box) = {
    b.tokens.size > 0 &&
    b.tokens(0)._1 == ballotTokenId &&
    b.R6[Coll[Byte]].get == SELF.id && // ensure vote corresponds to this box ****
    b.R7[Coll[Byte]].get == bankBoxOutHash // check value voted for
  }

  val ballotBoxes = INPUTS.filter(isValidBallot)

  val votesCount = ballotBoxes.fold(0L, {(accum: Long, b: Box) => accum + b.tokens(0)._2})

  sigmaProp(validBankIn && validBankOut && validUpdateIn && validUpdateOut && votesCount >= minVotes)
}
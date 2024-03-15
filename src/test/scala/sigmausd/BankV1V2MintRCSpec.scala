package sigmausd

import org.ergoplatform.appkit.{BlockchainContext, ConstantsBuilder}
import org.ergoplatform.kiosk.appkit.HttpClientTesting.createMockedErgoClient
import org.ergoplatform.kiosk.ergo.{
  DhtData,
  KioskBox,
  KioskInt,
  KioskLong,
  KioskTupleLong
}
import org.ergoplatform.kiosk.tx.TxUtil
import org.ergoplatform.sdk.ErgoToken
import org.scalacheck._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import sigmausd.SigmaUSDBootstrapping._

class BankV1V2MintRCSpec
    extends AnyPropSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with TestHelpers
    with Common {
  import TokenIds.Mainnet._

  override lazy val minStorageRent = 10_000_000L
  val NANO                         = 1_000_000_000L
  val transactionFee               = 1_500_000L
  val BANK_FEE                     = 2L
  val MAX_RESERVE_RATIO            = 800L

  val receiptBoxErgValue = 1_000L * NANO // enough for fees
  val scReceiptInGen     = Gen.const(0)
  val rcReceiptInGen     = Gen.const(0)
  val stableCoinBankIn   = 1_000L

  val stableCoinCirculationIn  = 15_000L
  val stableCoinBankOut        = stableCoinBankIn
  val stableCoinCirculationOut = stableCoinCirculationIn
  val reserveCoinBankIn        = stableCoinBankIn * 5
  val reserveCoinCirculationIn = stableCoinCirculationIn * 5

  val oracleRateXy         = 1L * NANO // x/100 nanoErgs per US Cent
  val bankBoxNanoErgsInGen = Gen.const(1000L * NANO)

  def bankV1RCMintingTx(
      bankBoxNanoErgsIn: Long,
      exchangeDelta: Long
  )(implicit ctx: BlockchainContext) = {
    val reserveCoinBankIn        = stableCoinBankIn * 5
    val reserveCoinBankOut       = reserveCoinBankIn - exchangeDelta
    val reserveCoinCirculationIn = stableCoinCirculationIn * 5
    val reserveCoinCirculationOut =
      reserveCoinCirculationIn + exchangeDelta

    val bankReservesNeededIn =
      stableCoinCirculationIn * (oracleRateXy / 100) // lower than bank reserves
    val bankDeltaExpected =
      ((bankBoxNanoErgsIn - bankReservesNeededIn) / reserveCoinCirculationIn) * exchangeDelta
    val bankReserveActualFee = bankDeltaExpected * BANK_FEE / 100
    val bankReserveDeltaAfterFee =
      bankDeltaExpected + bankReserveActualFee

    val receiptBox =
      ctx
        .newTxBuilder()
        .outBoxBuilder
        .value(receiptBoxErgValue)
        .contract(
          ctx.compileContract(ConstantsBuilder.empty(), fakeScript)
        )
        .build()
        .convertToInputWith(fakeTxId1, fakeIndex)

    val oracleBox =
      ctx
        .newTxBuilder()
        .outBoxBuilder
        .value(minStorageRent)
        .tokens(new ErgoToken(oraclePoolNFT, 1))
        .registers(KioskLong(oracleRateXy).getErgoValue)
        .contract(
          ctx.compileContract(ConstantsBuilder.empty(), fakeScript)
        )
        .build()
        .convertToInputWith(fakeTxId2, fakeIndex)

    val bankBox =
      ctx
        .newTxBuilder()
        .outBoxBuilder
        .value(bankBoxNanoErgsIn)
        .registers(
          KioskLong(stableCoinCirculationIn).getErgoValue,
          KioskLong(reserveCoinCirculationIn).getErgoValue
        )
        .tokens(
          new ErgoToken(sigmaUSD, stableCoinBankIn),
          new ErgoToken(sigmaRSV, reserveCoinBankIn),
          new ErgoToken(bankNFT, 1)
        )
        .contract(
          ctx.compileContract(ConstantsBuilder.empty(), bankV1Script)
        )
        .build()
        .convertToInputWith(fakeTxId3, fakeIndex)

    val validBankOutBox = KioskBox(
      bankV1Address,
      bankBoxNanoErgsIn + bankReserveDeltaAfterFee,
      registers = Array(
        KioskLong(stableCoinCirculationOut),
        KioskLong(reserveCoinCirculationOut)
      ),
      tokens = Array(
        (sigmaUSD, stableCoinBankOut),
        (sigmaRSV, reserveCoinBankOut),
        (bankNFT, 1)
      )
    )

    val receiptOutBox = KioskBox(
      changeAddress,
      receiptBoxErgValue - bankReserveDeltaAfterFee - transactionFee,
      registers = Array(
        KioskLong(exchangeDelta),
        KioskLong(bankReserveDeltaAfterFee)
      ),
      tokens = Array((sigmaRSV, exchangeDelta))
    )

    TxUtil
      .createTx(
        Array(bankBox, receiptBox),
        Array(oracleBox),
        Array(validBankOutBox, receiptOutBox),
        transactionFee,
        changeAddress,
        Array[String](),
        Array[DhtData](),
        false
      )
  }

  def bankV2RCMintingTx(
      bankBoxNanoErgsIn: Long,
      exchangeDelta: Long
  )(implicit ctx: BlockchainContext) = {
    val reserveCoinBankIn        = stableCoinBankIn * 5
    val reserveCoinBankOut       = reserveCoinBankIn - exchangeDelta
    val reserveCoinCirculationIn = stableCoinCirculationIn * 5
    val reserveCoinCirculationOut =
      reserveCoinCirculationIn + exchangeDelta

    val bankReservesNeededIn =
      stableCoinCirculationIn * (oracleRateXy / 100)
    val bankDeltaExpected =
      ((bankBoxNanoErgsIn - bankReservesNeededIn) / reserveCoinCirculationIn) * exchangeDelta
    val bankReserveActualFee = bankDeltaExpected * BANK_FEE / 100
    val bankReserveDeltaAfterFee =
      bankDeltaExpected + bankReserveActualFee

    assert(
      bankBoxNanoErgsIn * 100 / bankReservesNeededIn <= MAX_RESERVE_RATIO
    )

    // update height should be lower for bank to trigger limit calculation
    val updateHeightBank   = 99
    val updateHeightOracle = 100
    val limitRateOld =
      (100L, 100L) // ignored if updateHeightBank != updateHeightOracle
    val V2_LIMIT_FACTOR = 200
    val limit           = bankBoxNanoErgsIn / V2_LIMIT_FACTOR
    val updatedLimits =
      (limit, limit - bankReserveDeltaAfterFee) // rc exchange

    val receiptBox =
      ctx
        .newTxBuilder()
        .outBoxBuilder
        .value(receiptBoxErgValue)
        .contract(
          ctx.compileContract(ConstantsBuilder.empty(), fakeScript)
        )
        .build()
        .convertToInputWith(fakeTxId1, fakeIndex)

    val oracleBox =
      ctx
        .newTxBuilder()
        .outBoxBuilder
        .value(minStorageRent)
        .tokens(new ErgoToken(oraclePoolNFT, 1))
        .registers(
          KioskLong(oracleRateXy).getErgoValue,
          KioskInt(updateHeightOracle).getErgoValue
        )
        .contract(
          ctx.compileContract(ConstantsBuilder.empty(), fakeScript)
        )
        .build()
        .convertToInputWith(fakeTxId2, fakeIndex)

    val bankBox =
      ctx
        .newTxBuilder()
        .outBoxBuilder
        .value(bankBoxNanoErgsIn)
        .registers(
          KioskLong(stableCoinCirculationIn).getErgoValue,
          KioskLong(reserveCoinCirculationIn).getErgoValue,
          KioskInt(updateHeightBank).getErgoValue,
          KioskTupleLong(limitRateOld).getErgoValue // old limit rate
        )
        .tokens(
          new ErgoToken(sigmaUSD, stableCoinBankIn),
          new ErgoToken(sigmaRSV, reserveCoinBankIn),
          new ErgoToken(bankNFT, 1)
        )
        .contract(
          ctx.compileContract(ConstantsBuilder.empty(), bankV2Script)
        )
        .build()
        .convertToInputWith(fakeTxId3, fakeIndex)

    val validBankOutBox = KioskBox(
      bankV2Address,
      bankBoxNanoErgsIn + bankReserveDeltaAfterFee,
      registers = Array(
        KioskLong(stableCoinCirculationOut),
        KioskLong(reserveCoinCirculationOut),
        KioskInt(updateHeightOracle),
        KioskTupleLong(updatedLimits)
      ),
      tokens = Array(
        (sigmaUSD, stableCoinBankOut),
        (sigmaRSV, reserveCoinBankOut),
        (bankNFT, 1)
      )
    )

    val receiptOutBox = KioskBox(
      changeAddress,
      receiptBoxErgValue - bankReserveDeltaAfterFee - transactionFee,
      registers = Array(
        KioskLong(exchangeDelta),
        KioskLong(bankReserveDeltaAfterFee)
      ),
      tokens = Array((sigmaRSV, exchangeDelta))
    )

      TxUtil.createTx(
        Array(bankBox, receiptBox),
        Array(oracleBox),
        Array(validBankOutBox, receiptOutBox),
        transactionFee,
        changeAddress,
        Array[String](),
        Array[DhtData](),
        false
      )
  }

  /* SMALL MINTING RESERVE COINS */
  val exchangeDeltaSmall = 1L

  property("Small amount minting V1 should work") {
    createMockedErgoClient().execute { implicit ctx: BlockchainContext =>
      forAll(
        bankBoxNanoErgsInGen
      ) { (bankBoxNanoErgsIn) =>
        assertTry(bankV1RCMintingTx(bankBoxNanoErgsIn, exchangeDeltaSmall))
      }
    }
  }

  property("Small amount minting V2 should work") {
    createMockedErgoClient().execute { implicit ctx: BlockchainContext =>
      forAll(
        bankBoxNanoErgsInGen
      ) { (bankBoxNanoErgsIn) =>
        assertTry(bankV2RCMintingTx(bankBoxNanoErgsIn, exchangeDeltaSmall))
      }
    }
  }

  /* BIG MINTING RESERVE COINS */
  val exchangeDeltaBig = 433L

  property(">0.5% amount minting V1 should work") {
    createMockedErgoClient().execute { implicit ctx: BlockchainContext =>
      forAll(
        bankBoxNanoErgsInGen
      ) { (bankBoxNanoErgsIn) =>
        assertTry(bankV1RCMintingTx(bankBoxNanoErgsIn, exchangeDeltaSmall))
      }
    }
  }

  property(">0.5% amount minting V2 should fail") {
    createMockedErgoClient().execute { implicit ctx: BlockchainContext =>
      forAll(
        bankBoxNanoErgsInGen
      ) { (bankBoxNanoErgsIn) =>
        assertTryNeg(bankV2RCMintingTx(bankBoxNanoErgsIn, exchangeDeltaBig))
      }
    }
  }

}

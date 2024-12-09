package sigmausd

import org.ergoplatform.appkit.{BlockchainContext, ConstantsBuilder}
import org.ergoplatform.kiosk.appkit.HttpClientTesting.{
  MockData,
  createMockedErgoClient
}
import org.ergoplatform.kiosk.appkit
import org.ergoplatform.kiosk.ergo.{DhtData, KioskBox, KioskLong}
import org.ergoplatform.kiosk.tx.TxUtil
import org.ergoplatform.sdk.ErgoToken
import sigmausd.SigmaUSDBootstrapping._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class BankMintSpec
    extends AnyPropSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with TestHelpers
    with Common {
  import TokenIds.Mainnet._

  override lazy val minStorageRent = 10_000_000L

  val NANO           = 1_000_000_000L
  val transactionFee = 1_500_000L
  val BANK_FEE       = 2L

  val oracleRateGen             = Gen.choose(1, 100L)
  val exchangeDeltaGen          = Gen.choose(1, 1000L)
  val receiptBoxNanoErgValueGen = Gen.choose(1000L * NANO, 2000L * NANO)
  val bankBoxNanoErgsInGen      = Gen.choose(2000L * NANO, 10_000L * NANO)
  val scReceiptInGen            = Gen.choose(1000L, 1000L)
  val rcReceiptInGen            = Gen.choose(1000L, 2000L)

  property("Minting stable coin should work") {
    createMockedErgoClient().execute { implicit ctx: BlockchainContext =>
      forAll(
        oracleRateGen,
        exchangeDeltaGen,
        receiptBoxNanoErgValueGen,
        bankBoxNanoErgsInGen
      ) {
        (oracleRateXy, exchangeDelta, receiptBoxErgValue, bankBoxNanoErgsIn) =>
          val bankReserveDelta         = exchangeDelta * (oracleRateXy / 100)
          val bankReserveActualFee     = bankReserveDelta * BANK_FEE / 100
          val bankReserveDeltaAfterFee = bankReserveDelta + bankReserveActualFee

          val stableCoinBankIn         = 1000L
          val stableCoinBankOut        = stableCoinBankIn - exchangeDelta
          val stableCoinCirculationIn  = 1000L
          val stableCoinCirculationOut = stableCoinCirculationIn + exchangeDelta

          val reserveCoinBankIn         = stableCoinBankIn * 5
          val reserveCoinBankOut        = stableCoinBankIn * 5
          val reserveCoinCirculationIn  = stableCoinCirculationIn * 5
          val reserveCoinCirculationOut = stableCoinCirculationIn * 5

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
                ctx.compileContract(ConstantsBuilder.empty(), bankV2Script)
              )
              .build()
              .convertToInputWith(fakeTxId3, fakeIndex)

          val validBankOutBox = KioskBox(
            bankV2Address,
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
            tokens = Array((sigmaUSD, exchangeDelta))
          )

          assertTry {
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
      }
    }
  }

  property("Burning stable coin should work") {
    createMockedErgoClient().execute { implicit ctx: BlockchainContext =>
      forAll(
        oracleRateGen,
        exchangeDeltaGen,
        receiptBoxNanoErgValueGen,
        bankBoxNanoErgsInGen,
        scReceiptInGen
      ) {
        (
            oracleRateXy,
            exchangeDeltaPos,
            receiptBoxErgValue,
            bankBoxNanoErgsIn,
            scReceiptIn
        ) =>
          val exchangeDelta        = exchangeDeltaPos * -1
          val bankReserveDelta     = exchangeDelta * (oracleRateXy / 100)
          val bankReserveActualFee = bankReserveDelta * BANK_FEE / 100
          val bankReserveDeltaAfterFee =
            bankReserveDelta + bankReserveActualFee

          val stableCoinReceiptIn  = scReceiptIn
          val stableCoinReceiptOut = stableCoinReceiptIn + exchangeDelta

          val stableCoinBankIn        = 1000L
          val stableCoinBankOut       = stableCoinBankIn - exchangeDelta
          val stableCoinCirculationIn = stableCoinBankIn + stableCoinReceiptIn
          val stableCoinCirculationOut =
            stableCoinCirculationIn + exchangeDelta

          val reserveCoinBankIn         = stableCoinBankIn * 5
          val reserveCoinBankOut        = stableCoinBankIn * 5
          val reserveCoinCirculationIn  = stableCoinCirculationIn * 5
          val reserveCoinCirculationOut = stableCoinCirculationIn * 5

          val receiptBox =
            ctx
              .newTxBuilder()
              .outBoxBuilder
              .value(receiptBoxErgValue)
              .tokens(new ErgoToken(sigmaUSD, stableCoinReceiptIn))
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
                ctx.compileContract(ConstantsBuilder.empty(), bankV2Script)
              )
              .build()
              .convertToInputWith(fakeTxId3, fakeIndex)

          val validBankOutBox = KioskBox(
            bankV2Address,
            bankBoxNanoErgsIn - bankReserveDeltaAfterFee,
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
            receiptBoxErgValue + bankReserveDeltaAfterFee - transactionFee,
            registers = Array(
              KioskLong(exchangeDelta),
              KioskLong(bankReserveDeltaAfterFee)
            ),
            tokens = Array((sigmaUSD, stableCoinReceiptOut))
          )

          assertTry(
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
          )
      }
    }
  }

  property("Minting reserve coin should work") {
    createMockedErgoClient().execute { implicit ctx: BlockchainContext =>
      forAll(
        oracleRateGen,
        exchangeDeltaGen,
        receiptBoxNanoErgValueGen,
        bankBoxNanoErgsInGen
      ) {
        (oracleRateXy, exchangeDelta, receiptBoxErgValue, bankBoxNanoErgsIn) =>
          val stableCoinBankIn         = 1000L
          val stableCoinBankOut        = stableCoinBankIn
          val stableCoinCirculationIn  = 1000L
          val stableCoinCirculationOut = stableCoinCirculationIn

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
                ctx.compileContract(ConstantsBuilder.empty(), bankV2Script)
              )
              .build()
              .convertToInputWith(fakeTxId3, fakeIndex)

          val validBankOutBox = KioskBox(
            bankV2Address,
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

          assertTry {
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
      }
    }
  }

  property("Burning reserve coin should work") {
    createMockedErgoClient().execute { implicit ctx: BlockchainContext =>
      forAll(
        oracleRateGen,
        exchangeDeltaGen,
        receiptBoxNanoErgValueGen,
        bankBoxNanoErgsInGen,
        rcReceiptInGen
      ) {
        (
            oracleRateXy,
            exchangeDeltaPos,
            receiptBoxErgValue,
            bankBoxNanoErgsIn,
            rcReceiptIn
        ) =>
          val exchangeDelta            = exchangeDeltaPos * -1
          val stableCoinBankIn         = 5000L
          val stableCoinBankOut        = stableCoinBankIn
          val stableCoinCirculationIn  = 5000L
          val stableCoinCirculationOut = stableCoinCirculationIn

          val reserveCoinBankIn        = stableCoinBankIn * 5
          val reserveCoinBankOut       = reserveCoinBankIn - exchangeDelta
          val reserveCoinCirculationIn = stableCoinCirculationIn * 5
          val reserveCoinCirculationOut =
            reserveCoinCirculationIn + exchangeDelta

          val bankReservesNeededIn =
            stableCoinCirculationIn * (oracleRateXy / 100) // lower than bank reserves
          val bankDeltaExpected =
            ((bankBoxNanoErgsIn - bankReservesNeededIn) / reserveCoinCirculationIn) * exchangeDeltaPos
          val bankReserveActualFee = bankDeltaExpected * BANK_FEE / 100
          val bankReserveDeltaAfterFee =
            bankDeltaExpected - bankReserveActualFee

          val receiptBox =
            ctx
              .newTxBuilder()
              .outBoxBuilder
              .value(receiptBoxErgValue)
              .tokens(new ErgoToken(sigmaRSV, rcReceiptIn))
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
                ctx.compileContract(ConstantsBuilder.empty(), bankV2Script)
              )
              .build()
              .convertToInputWith(fakeTxId3, fakeIndex)

          val validBankOutBox = KioskBox(
            bankV2Address,
            bankBoxNanoErgsIn - bankReserveDeltaAfterFee,
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
            receiptBoxErgValue + bankReserveDeltaAfterFee - transactionFee,
            registers = Array(
              KioskLong(exchangeDelta),
              KioskLong(-bankReserveDeltaAfterFee)
            ),
            tokens = Array((sigmaRSV, rcReceiptIn + exchangeDelta))
          )

          assertTry {
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
      }
    }
  }
}

package test.classes

import classes.{Client, ClientWithOrdersBuffer, Order, StockExchange}

import collection.mutable
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test

import scala.io.Source

class ApplicationTest extends AssertionsForJUnit{

  private val ordersPath = "src/test/resources/ordersTest.txt"
  private val clientsPath = "src/test/resources/clientsTest.txt"

  val stockExchange = new StockExchange(clientsPath, ordersPath)

  @Test  def testCreateOrderList(): Unit = {
    val one = new Order("C1",	'b', 'C',	15,	4)
    val two = new Order("C2",	's', 'C',	14,	5)
    val three = new Order("C3",	's', 'C',	13,	2)

    val testOrdersList:mutable.Buffer[Order] = mutable.Buffer(one, two, three)

    val seqOrder = Source.fromFile(ordersPath).getLines().toSeq
    val ordersBuffer = Order.createOrdersList(seqOrder)

    for(i <- 0 to 2) {
      assertTrue(ordersBuffer(i).getName.equals(testOrdersList(i).getName) && ordersBuffer(i).getOperation.equals(testOrdersList(i).getOperation) &&
        ordersBuffer(i).getStockName.equals(testOrdersList(i).getStockName) && ordersBuffer(i).getAmountStock.equals(testOrdersList(i).getAmountStock) &&
        ordersBuffer(i).getPriceStock.equals(testOrdersList(i).getPriceStock))
    }
  }

  @Test def testCreateClients: Unit = {
    val one = new Client("C1", 1000, 130, 240, 760, 320)
    val two = new Client("C2",4350, 370, 120, 950, 560)
    val three = new Client("C3",2760, 0, 0, 0, 0)

    val testClientsList:mutable.Buffer[Client] = mutable.Buffer(one, two, three)
    val seqClients = Source.fromFile(clientsPath).getLines().toSeq
    val clientsBuffer = Client.createClients(seqClients)

    for(i <- 0 to 2) {
      assertTrue(clientsBuffer(i).getName.equals(testClientsList(i).getName) && clientsBuffer(i).getMoney.equals(testClientsList(i).getMoney) &&
        clientsBuffer(i).getA.equals(testClientsList(i).getA) && clientsBuffer(i).getB.equals(testClientsList(i).getB) &&
        clientsBuffer(i).getC.equals(testClientsList(i).getC) && clientsBuffer(i).getD.equals(testClientsList(i).getD))
    }
  }

  @Test def testCreateOwnOrderBuffer(): Unit = {
    val client = new Client("C1", 1000, 130, 240, 760, 320)

    val one = new Order("C1",	'b', 'C',	15,	4)
    val two = new Order("C1",	's', 'A',	9,	1)


    val testOwnOrdersBuffer:mutable.Buffer[Order] = mutable.Buffer()
    testOwnOrdersBuffer.append(one, two)

    val seqOrder = Source.fromFile(ordersPath).getLines().toSeq
    val ordersBuffer = Order.createOrdersList(seqOrder)

    val ownOrdersBuffer = ClientWithOrdersBuffer.createOwnOrderBuffer(client, ordersBuffer)

    for(i <- ownOrdersBuffer.indices) {
      assertTrue(testOwnOrdersBuffer(i).getName.equals(ownOrdersBuffer(i).getName) &&
        testOwnOrdersBuffer(i).getOperation.equals(ownOrdersBuffer(i).getOperation) &&
        testOwnOrdersBuffer(i).getStockName.equals(ownOrdersBuffer(i).getStockName) &&
        testOwnOrdersBuffer(i).getAmountStock.equals(ownOrdersBuffer(i).getAmountStock) &&
          testOwnOrdersBuffer(i).getPriceStock.equals(ownOrdersBuffer(i).getPriceStock))
    }
  }

  @Test def testCalculateClientsBalance(): Unit = {

    stockExchange.calculateClientsBalance()

    var clientsPath = "src/test/resources/resultTest.txt"
    var seqClients = Source.fromFile(clientsPath).getLines().toSeq
    val testClientsBuffer = Client.createClients(seqClients)

    clientsPath = "src/main/resources/result.txt"
    seqClients = Source.fromFile(clientsPath).getLines().toSeq
    val clientsBuffer = Client.createClients(seqClients)

    for(i <- clientsBuffer.indices) {
      assertTrue(clientsBuffer(i).getName.equals(testClientsBuffer(i).getName) && clientsBuffer(i).getMoney.equals(testClientsBuffer(i).getMoney) &&
        clientsBuffer(i).getA.equals(testClientsBuffer(i).getA) && clientsBuffer(i).getB.equals(testClientsBuffer(i).getB) &&
        clientsBuffer(i).getC.equals(testClientsBuffer(i).getC) && clientsBuffer(i).getD.equals(testClientsBuffer(i).getD))
    }
  }
}


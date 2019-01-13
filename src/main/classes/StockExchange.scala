package classes

import java.io.PrintWriter

import scala.io.Source
import collection.mutable

class StockExchange(clientsPath:String, ordersPath:String) {

  private val seqOrder = Source.fromFile(ordersPath).getLines().toSeq
  private val ordersBuffer = Order.createOrdersList(seqOrder)

  private val seqClients = Source.fromFile(clientsPath).getLines().toSeq
  private val clientsBuffer = Client.createClients(seqClients)

  private val clientsWithOrdersBuffer = ClientWithOrdersBuffer.createClientsWithOrdersBuffer(ordersBuffer,clientsBuffer)

  private val buySellOrdersBuffer:mutable.Buffer[Order] = mutable.Buffer()

  def calculateClientsBalance(): Unit = {

    var reverseCounter = 1
    var numFrom = 0
    var numTo = 0
    var numMediator = 0
    var counter = 0
    var stopCounter = 0

    while (stopCounter != seqClients.size){

      for(j <- seqClients.indices) {
        if(counter < clientsWithOrdersBuffer(j).ownOrdersBuffer.size) {
          buySellOrdersBuffer.append(clientsWithOrdersBuffer(j).ownOrdersBuffer(counter))
        }else{
          stopCounter += 1
        }
      }

      if(stopCounter != seqClients.size) {
        stopCounter = 0
      }

      if(stopCounter == seqClients.size) {

        val writer = new PrintWriter("src/main/resources/result.txt")
        for(i <- clientsWithOrdersBuffer.indices) {
          val client = clientsWithOrdersBuffer(i)
          writer.write(client.getName + "\t" + client.getMoney + "\t" + client.getA + "\t" + client.getB + "\t" + client.getC + "\t" + client.getD + "\n")
        }
        writer.close()
      }

      counter += 1

      if(reverseCounter == 1) {
        numFrom = 0
        numTo = seqClients.size - 1
        numMediator = 1
        reverseCounter = 0
      }else{
        numFrom = seqClients.size - 1
        numTo = 0
        numMediator = -1
        reverseCounter = 1
      }

      for(j <- numFrom to (numTo, numMediator)) {
        val ownBuySellOrdersBuffer = buySellOrdersBuffer.filter(x => x.getName == clientsWithOrdersBuffer(j).getName)
        val client = clientsWithOrdersBuffer(j)

        for(k <- ownBuySellOrdersBuffer.indices) {
          pickSellOrBuy(ownBuySellOrdersBuffer(k), client)
        }
      }
    }
  }

  private def pickSellOrBuy(ownOrder:Order, client:ClientWithOrdersBuffer): Unit = {

      if(ownOrder.getOperation == 's') {

        val stockName: String = ownOrder.getStockName.toString
        stockName match {
          case "A" if client.getA == 0 => return
          case "B" if client.getB == 0 => return
          case "C" if client.getC == 0 => return
          case "D" if client.getD == 0 => return
          case _ =>
        }

        if(!sellOperation(ownOrder, buySellOrdersBuffer, client))
          return

      } else{
        if(client.getMoney == 0)
          return
        if(!buyOperation(ownOrder, buySellOrdersBuffer, client))
          return
      }
  }

  private def buyOperation(order: Order, buySellOrdersBuffer:mutable.Buffer[Order], client:ClientWithOrdersBuffer): Boolean = {

    for(i <- buySellOrdersBuffer.indices) {

      if(i >= buySellOrdersBuffer.size)
        return false

      val candidateOrder = buySellOrdersBuffer(i)
      if(!order.getName.equals(candidateOrder.getName) && !order.getOperation.equals(candidateOrder.getOperation) &&
        order.getStockName.equals(candidateOrder.getStockName) &&
          order.getPriceStock.compareTo(candidateOrder.getPriceStock) != -1) {

        var candidate:ClientWithOrdersBuffer = null
        for(j <- clientsWithOrdersBuffer.indices) {
          if(candidateOrder.getName.equals(clientsWithOrdersBuffer(j).getName))
            candidate = clientsWithOrdersBuffer(j)
        }
        return switchWhenBuy(client, order, candidate, candidateOrder)
      }
    }
    false
  }

  private def changeAndDeleteOrders(orderToChange:Order, amountStock:Int, orderToDelete:Order): Unit = {

    orderToChange.minusStock(amountStock)
    for(i <- buySellOrdersBuffer.indices) {
      if(buySellOrdersBuffer(i) == orderToDelete){
        buySellOrdersBuffer.remove(i)
        return
      }
    }
  }

  private def deleteTwoOrders(orderToDeleteOne:Order, orderToDeleteTwo:Order): Unit ={

    var returnCounter = 0
    var oneId = 0
    var twoId = 0

    for(i <- buySellOrdersBuffer.indices) {
      if(buySellOrdersBuffer(i) == orderToDeleteOne){
        returnCounter += 1
        oneId = i
      }
      if(returnCounter == 2) {
        if(oneId > twoId){
          buySellOrdersBuffer.remove(twoId)
          buySellOrdersBuffer.remove(oneId - 1)
        }else{
          buySellOrdersBuffer.remove(oneId)
          buySellOrdersBuffer.remove(twoId - 1)
        }
        return
      }

      if(buySellOrdersBuffer(i) == orderToDeleteTwo) {
        returnCounter += 1
        twoId = i
      }
      if(returnCounter == 2) {
        if(oneId > twoId){
          buySellOrdersBuffer.remove(twoId)
          buySellOrdersBuffer.remove(oneId - 1)
        }else{
          buySellOrdersBuffer.remove(oneId)
          buySellOrdersBuffer.remove(twoId - 1)
        }
        return
      }
    }
  }

  private def deleteZeroOrOneOrTwoOrdersWhenBuy(buyer:ClientWithOrdersBuffer, buyerOrder:Order, candidate:ClientWithOrdersBuffer,
                                        candidateOrder:Order, amountStock:Int, numberOfAction:Int): Boolean = {

    buyer.minusMoney(amountStock * candidateOrder.getPriceStock)
    candidate.plusMoney(amountStock * candidateOrder.getPriceStock)

    changeAmountStock(candidate, buyer, buyerOrder.getStockName, amountStock)

    numberOfAction match {
      case 1 =>
        buyerOrder.minusStock(amountStock)
        candidateOrder.minusStock(amountStock)
        true
      case 2 =>
        changeAndDeleteOrders(candidateOrder, amountStock, buyerOrder)
        false
      case 3 =>
        changeAndDeleteOrders(buyerOrder, amountStock, candidateOrder)
        true
      case 4 =>
        deleteTwoOrders(buyerOrder, candidateOrder)
        false
    }
  }

  private def sellOperation(order: Order, buySellOrdersBuffer:mutable.Buffer[Order], seller:ClientWithOrdersBuffer): Boolean = {

    for(i <- buySellOrdersBuffer.indices) {

      if(i >= buySellOrdersBuffer.size)
        return false

      val candidateOrder = buySellOrdersBuffer(i)
      if(!order.getName.equals(candidateOrder.getName) && !order.getOperation.equals(candidateOrder.getOperation) &&
        order.getStockName.equals(candidateOrder.getStockName) &&
        order.getPriceStock.compareTo(candidateOrder.getPriceStock) != 1) {

        var candidate:ClientWithOrdersBuffer = null
        for(j <- clientsWithOrdersBuffer.indices) {
          if(candidateOrder.getName.equals(clientsWithOrdersBuffer(j).getName))
            candidate = clientsWithOrdersBuffer(j)
        }
        return switchWhenSell(seller, order, candidate, candidateOrder)
      }
    }
    false
  }

  private def deleteZeroOrOneOrTwoOrdersWhenSell(seller:ClientWithOrdersBuffer, sellerOrder:Order, candidate:ClientWithOrdersBuffer,
                                         candidateOrder:Order, amountStock:Int, numberOfAction:Int): Boolean = {

    seller.plusMoney(amountStock * candidateOrder.getPriceStock)
    candidate.minusMoney(amountStock * candidateOrder.getPriceStock)

    changeAmountStock(seller, candidate, candidateOrder.getStockName, amountStock)

    numberOfAction match {
      case 1 =>
        sellerOrder.minusStock(amountStock)
        candidateOrder.minusStock(amountStock)
        true
      case 2 =>
        changeAndDeleteOrders(candidateOrder, amountStock, sellerOrder)
        false
      case 3 =>
        changeAndDeleteOrders(sellerOrder, amountStock, candidateOrder)
        true
      case 4 =>
        deleteTwoOrders(sellerOrder, candidateOrder)
        false
    }
  }

  private def changeAmountStock(clientMinus:ClientWithOrdersBuffer, clientPlus:ClientWithOrdersBuffer,
                        stockName:Char, amountStock:Int): Unit = {

    stockName match {
      case 'A' =>
        clientMinus.minusA(amountStock)
        clientPlus.plusA(amountStock)
      case 'B' =>
        clientMinus.minusB(amountStock)
        clientPlus.plusB(amountStock)
      case 'C' =>
        clientMinus.minusC(amountStock)
        clientPlus.plusC(amountStock)
      case 'D' =>
        clientMinus.minusD(amountStock)
        clientPlus.plusD(amountStock)
    }
  }

  private def switchWhenBuy(buyer:ClientWithOrdersBuffer, buyerOrder:Order, candidate:ClientWithOrdersBuffer, candidateOrder:Order): Boolean = {

    val amountWantBuy = buyerOrder.getAmountStock
    val amountWantSell = candidateOrder.getAmountStock

    val amountCanBuy = buyer.getMoney / candidateOrder.getPriceStock
    val amountCanSell = findAmountStockToSell(candidate, candidateOrder)

    if(amountWantBuy <= amountCanBuy && amountWantSell <= amountCanSell) {
      if(amountWantBuy == amountWantSell) {
        return deleteZeroOrOneOrTwoOrdersWhenBuy(buyer, buyerOrder, candidate, candidateOrder, amountWantBuy, 4)
      }else if(amountWantBuy < amountWantSell) {
        return deleteZeroOrOneOrTwoOrdersWhenBuy(buyer, buyerOrder, candidate, candidateOrder, amountWantBuy, 2)
      }else if(amountWantBuy > amountWantSell){
        return deleteZeroOrOneOrTwoOrdersWhenBuy(buyer, buyerOrder, candidate, candidateOrder, amountWantSell, 3)
      }
    }

    if(amountWantBuy > amountCanBuy && amountWantSell <= amountCanSell) {
      if(amountCanBuy == amountWantSell) {
        return deleteZeroOrOneOrTwoOrdersWhenBuy(buyer, buyerOrder, candidate, candidateOrder, amountWantBuy, 3)
      }else if(amountCanBuy < amountWantSell) {
        return deleteZeroOrOneOrTwoOrdersWhenBuy(buyer, buyerOrder, candidate, candidateOrder, amountCanBuy, 1)
      }else if (amountCanBuy > amountWantSell) {
        return deleteZeroOrOneOrTwoOrdersWhenBuy(buyer, buyerOrder, candidate, candidateOrder, amountWantSell, 3)
      }
    }

    if(amountWantBuy <= amountCanBuy && amountWantSell > amountCanSell) {
      if(amountWantBuy == amountCanSell) {
        return deleteZeroOrOneOrTwoOrdersWhenBuy(buyer, buyerOrder, candidate, candidateOrder, amountWantBuy, 2)
      }else if(amountWantBuy < amountCanSell) {
        return deleteZeroOrOneOrTwoOrdersWhenBuy(buyer, buyerOrder, candidate, candidateOrder, amountWantBuy, 2)
      }else if(amountWantBuy > amountCanSell) {
        return deleteZeroOrOneOrTwoOrdersWhenBuy(buyer, buyerOrder, candidate, candidateOrder, amountCanSell, 1)
      }
    }

    if(amountWantBuy > amountCanBuy && amountWantSell > amountCanSell) {
      if(amountCanBuy == amountCanSell) {
        return deleteZeroOrOneOrTwoOrdersWhenBuy(buyer, buyerOrder, candidate, candidateOrder, amountCanBuy, 1)
      }else if(amountCanBuy < amountCanSell) {
        return deleteZeroOrOneOrTwoOrdersWhenBuy(buyer, buyerOrder, candidate, candidateOrder, amountCanBuy, 1)
      }else if(amountCanBuy > amountCanSell){
        return deleteZeroOrOneOrTwoOrdersWhenBuy(buyer, buyerOrder, candidate, candidateOrder, amountCanSell, 1)
      }
    }
    false
  }

  private def findAmountStockToSell(client:ClientWithOrdersBuffer, clientOrder:Order): Int = {

    val stockName = clientOrder.getStockName
    stockName match {
      case 'A' => client.getA
      case 'B' => client.getB
      case 'C' => client.getC
      case 'D' => client.getD
    }
  }

  private def switchWhenSell(seller:ClientWithOrdersBuffer, sellerOrder:Order, candidate:ClientWithOrdersBuffer, candidateOrder:Order): Boolean = {

    val amountWantBuy = candidateOrder.getAmountStock
    val amountWantSell = sellerOrder.getAmountStock

    val amountCanBuy = candidate.getMoney / candidateOrder.getPriceStock
    val amountCanSell = findAmountStockToSell(seller, sellerOrder)

    if(amountWantBuy <= amountCanBuy && amountWantSell <= amountCanSell) {
      if(amountWantBuy == amountWantSell) {
        return deleteZeroOrOneOrTwoOrdersWhenSell(seller, sellerOrder, candidate, candidateOrder, amountWantBuy, 4)
      }else if(amountWantBuy < amountWantSell) {
        return deleteZeroOrOneOrTwoOrdersWhenSell(seller, sellerOrder, candidate, candidateOrder, amountWantBuy, 3)
      }else if(amountWantBuy > amountWantSell){
        return deleteZeroOrOneOrTwoOrdersWhenSell(seller, sellerOrder, candidate, candidateOrder, amountWantSell, 2)
      }
    }

    if(amountWantBuy > amountCanBuy && amountWantSell <= amountCanSell) {
      if(amountCanBuy == amountWantSell) {
        return deleteZeroOrOneOrTwoOrdersWhenSell(seller, sellerOrder, candidate, candidateOrder, amountCanBuy, 2)
      }else if(amountCanBuy < amountWantSell) {
        return deleteZeroOrOneOrTwoOrdersWhenSell(seller, sellerOrder, candidate, candidateOrder, amountCanBuy, 1)
      }else if (amountCanBuy > amountWantSell) {
        return deleteZeroOrOneOrTwoOrdersWhenSell(seller, sellerOrder, candidate, candidateOrder, amountWantSell, 2)
      }
    }

    if(amountWantBuy <= amountCanBuy && amountWantSell > amountCanSell) {
      if(amountWantBuy == amountCanSell) {
        return deleteZeroOrOneOrTwoOrdersWhenSell(seller, sellerOrder, candidate, candidateOrder, amountWantBuy, 3)
      }else if(amountWantBuy < amountCanSell) {
        return deleteZeroOrOneOrTwoOrdersWhenSell(seller, sellerOrder, candidate, candidateOrder, amountWantBuy, 3)
      }else if(amountWantBuy > amountCanSell) {
        return deleteZeroOrOneOrTwoOrdersWhenSell(seller, sellerOrder, candidate, candidateOrder, amountCanSell, 1)
      }
    }

    if(amountWantBuy > amountCanBuy && amountWantSell > amountCanSell) {
      if(amountCanBuy == amountCanSell) {
        return deleteZeroOrOneOrTwoOrdersWhenSell(seller, sellerOrder, candidate, candidateOrder, amountCanBuy, 1)
      }else if(amountCanBuy < amountCanSell) {
        return deleteZeroOrOneOrTwoOrdersWhenSell(seller, sellerOrder, candidate, candidateOrder, amountCanBuy, 1)
      }else if(amountCanBuy > amountCanSell){
        return deleteZeroOrOneOrTwoOrdersWhenSell(seller, sellerOrder, candidate, candidateOrder, amountCanSell, 1)
      }
    }
    false
  }
}

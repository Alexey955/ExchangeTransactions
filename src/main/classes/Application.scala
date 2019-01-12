package main.classes

import scala.io.Source
import collection.mutable
object Application extends App{

  var theMainCounter:Long = 0
  private val ordersPath = "d:ExchangeTransactions/orders.txt"
  val seqOrder = Source.fromFile(ordersPath).getLines().toSeq
  val ordersBuffer = Order.createOrdersList(seqOrder)

  private val clientsPath = "d:ExchangeTransactions/clients.txt"
  val seqClients = Source.fromFile(clientsPath).getLines().toSeq
  val clientsBuffer = Client.createClients(seqClients)

  val clientsWithOrdersBuffer = ClientWithOrdersBuffer.createClientsWithOrdersBuffer(ordersBuffer,clientsBuffer)

  val buySellOrdersBuffer:mutable.Buffer[Order] = mutable.Buffer()

  var reverseCounter = 1
  var numFrom = 5
  var numTo = 1
  var numMediator = 1
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
      println("STOP")
      println("TheMainCounter = " + theMainCounter)
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
      val ownBuySellOrdersBuffer = buySellOrdersBuffer.filter(x => x.name == clientsWithOrdersBuffer(j).name)
      val client = clientsWithOrdersBuffer(j)

      pickSellOrBuy(ownBuySellOrdersBuffer, client)
    }
  }

  def pickSellOrBuy(ownBuySellOrdersBuffer:mutable.Buffer[Order], client:ClientWithOrdersBuffer): Unit = {

    for(k <- ownBuySellOrdersBuffer.indices) {

      if(ownBuySellOrdersBuffer(k).operation == 's') {

        val stockName: String = ownBuySellOrdersBuffer(k).stockName.toString
        stockName match {
          case "A" if client.A == 0 => return
          case "B" if client.B == 0 => return
          case "C" if client.C == 0 => return
          case "D" if client.D == 0 => return
          case _ =>
        }

        if(!sellOperation(ownBuySellOrdersBuffer(k), buySellOrdersBuffer, client))
          return
        if (k == ownBuySellOrdersBuffer.size - 1)
          return

      } else{
        if(client.money == 0)
          return
        if(!buyOperation(ownBuySellOrdersBuffer(k), buySellOrdersBuffer, client))
          return
        if(k == ownBuySellOrdersBuffer.size - 1) {
          return
        }
      }
  }
}

  def buyOperation(order: Order, buySellOrdersBuffer:mutable.Buffer[Order], client:ClientWithOrdersBuffer): Boolean = {

    for(i <- buySellOrdersBuffer.indices) {

      theMainCounter += 1

      if(i >= buySellOrdersBuffer.size)
        return false

      val candidateOrder = buySellOrdersBuffer(i)
      if(!order.name.equals(candidateOrder.name) && !order.operation.equals(candidateOrder.operation) &&
        order.stockName.equals(candidateOrder.stockName) &&
        order.priceStock.compareTo(candidateOrder.priceStock) != -1) {

        var candidate:ClientWithOrdersBuffer = null
        for(j <- clientsWithOrdersBuffer.indices) {
          if(candidateOrder.name.equals(clientsWithOrdersBuffer(j).name))
            candidate = clientsWithOrdersBuffer(j)
        }
        return switchWhenBuy(client, order, candidate, candidateOrder)
      }
    }
    false
  }

  def changeAndDeleteOrders(orderToChange:Order, amountStock:Int, orderToDelete:Order): Unit = {

    orderToChange.amountStock -= amountStock
    for(i <- buySellOrdersBuffer.indices) {
      if(buySellOrdersBuffer(i) == orderToDelete){
        buySellOrdersBuffer.remove(i)
        return
      }
    }
  }

  def deleteTwoOrders(orderToDeleteOne:Order, orderToDeleteTwo:Order): Unit ={

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

  def deleteZeroOrOneOrTwoOrdersWhenBuy(buyer:ClientWithOrdersBuffer, buyerOrder:Order, candidate:ClientWithOrdersBuffer,
                                        candidateOrder:Order, amountStock:Int, numberOfAction:Int): Boolean = {

    buyer.money -= amountStock * candidateOrder.priceStock
    candidate.money += amountStock * candidateOrder.priceStock

    theMainCounter += 1

    changeAmountStock(candidate, buyer, buyerOrder.stockName, amountStock)

    numberOfAction match {
      case 1 =>
        buyerOrder.amountStock -= amountStock
        candidateOrder.amountStock -= amountStock
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

  def sellOperation(order: Order, buySellOrdersBuffer:mutable.Buffer[Order], seller:ClientWithOrdersBuffer): Boolean = {

    for(i <- buySellOrdersBuffer.indices) {

      theMainCounter += 1

      if(i >= buySellOrdersBuffer.size)
        return false

      val candidateOrder = buySellOrdersBuffer(i)
      if(!order.name.equals(candidateOrder.name) && !order.operation.equals(candidateOrder.operation) &&
          order.stockName.equals(candidateOrder.stockName) &&
          order.priceStock.compareTo(candidateOrder.priceStock) != 1) {

        var candidate:ClientWithOrdersBuffer = null
        for(j <- clientsWithOrdersBuffer.indices) {
          if(candidateOrder.name.equals(clientsWithOrdersBuffer(j).name))
            candidate = clientsWithOrdersBuffer(j)
        }
        return switchWhenSell(seller, order, candidate, candidateOrder)
      }
    }
    false
  }

  def deleteZeroOrOneOrTwoOrdersWhenSell(seller:ClientWithOrdersBuffer, sellerOrder:Order, candidate:ClientWithOrdersBuffer,
                                         candidateOrder:Order, amountStock:Int, numberOfAction:Int): Boolean = {

    seller.money += amountStock * candidateOrder.priceStock
    candidate.money -= amountStock * candidateOrder.priceStock

    theMainCounter += 1

    changeAmountStock(seller, candidate, candidateOrder.stockName, amountStock)

    numberOfAction match {
      case 1 =>
        sellerOrder.amountStock -= amountStock
        candidateOrder.amountStock -= amountStock
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

  def changeAmountStock(clientMinus:ClientWithOrdersBuffer, clientPlus:ClientWithOrdersBuffer,
                        stockName:Char, amountStock:Int): Unit = {

    stockName match {
      case 'A' =>
        clientMinus.A -= amountStock
        clientPlus.A += amountStock
      case 'B' =>
        clientMinus.B -= amountStock
        clientPlus.B += amountStock
      case 'C' =>
        clientMinus.C -= amountStock
        clientPlus.C += amountStock
      case 'D' =>
        clientMinus.D -= amountStock
        clientPlus.D += amountStock
    }
  }

  def switchWhenBuy(buyer:ClientWithOrdersBuffer, buyerOrder:Order, candidate:ClientWithOrdersBuffer, candidateOrder:Order): Boolean = {

    val amountWantBuy = buyerOrder.amountStock
    val amountWantSell = candidateOrder.amountStock

    val amountCanBuy = buyer.money / candidateOrder.priceStock
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

  def findAmountStockToSell(client:ClientWithOrdersBuffer, clientOrder:Order): Int = {

    val stockName = clientOrder.stockName
    stockName match {
      case 'A' => client.A
      case 'B' => client.B
      case 'C' => client.C
      case 'D' => client.D
    }
  }

  def switchWhenSell(seller:ClientWithOrdersBuffer, sellerOrder:Order, candidate:ClientWithOrdersBuffer, candidateOrder:Order): Boolean = {

    val amountWantBuy = candidateOrder.amountStock
    val amountWantSell = sellerOrder.amountStock

    val amountCanBuy = candidate.money / candidateOrder.priceStock
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

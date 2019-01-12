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
      var ownBuySellOrdersBuffer = buySellOrdersBuffer.filter(x => x.name == clientsWithOrdersBuffer(j).name)
      var client = clientsWithOrdersBuffer(j)

      pickSellOrBuy(ownBuySellOrdersBuffer, client)
    }
  }

  def pickSellOrBuy(ownBuySellOrdersBuffer:mutable.Buffer[Order], client:ClientWithOrdersBuffer): Unit = {
    for(k <- ownBuySellOrdersBuffer.indices) {
    if(ownBuySellOrdersBuffer(k).operation == 's') {

      var stockName:String = ownBuySellOrdersBuffer(k).stockName.toString
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

    }else{
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

      var candidateOrder = buySellOrdersBuffer(i)
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

  def changeIfWantBuyMore(buyer:ClientWithOrdersBuffer, buyerOrder:Order, candidate:ClientWithOrdersBuffer,
                          candidateOrder:Order, amountStock:Int): Boolean = {

    buyer.money -= amountStock * candidateOrder.priceStock
    candidate.money += amountStock * candidateOrder.priceStock

    if(buyer.money < 0) {
      println("<0MF")
    }
    theMainCounter += 1

    changeAmountStock(candidate, buyer, buyerOrder.stockName, amountStock)

    changeAndDeleteOrders(buyerOrder, amountStock, candidateOrder)

    true
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

  def changeIfWantBuyLess(buyer:ClientWithOrdersBuffer, buyerOrder:Order, candidate:ClientWithOrdersBuffer,
                          candidateOrder:Order, amountStock:Int): Boolean = {

    buyer.money -= amountStock * candidateOrder.priceStock
    candidate.money += amountStock * candidateOrder.priceStock

    if(buyer.money < 0) {
      println("<0MF")
    }
    theMainCounter += 1

    changeAmountStock(candidate, buyer, buyerOrder.stockName, amountStock)

    candidateOrder.amountStock -= amountStock
    for(i <- buySellOrdersBuffer.indices) {
      if(buySellOrdersBuffer(i) == buyerOrder){
        buySellOrdersBuffer.remove(i)
        return false
      }
    }
    false
  }

  def buyWhenStockEqual(buyer:ClientWithOrdersBuffer, buyerOrder:Order,
                           candidate:ClientWithOrdersBuffer, candidateOrder:Order): Boolean = {

    buyer.money -= candidateOrder.amountStock * candidateOrder.priceStock
    candidate.money += candidateOrder.amountStock * candidateOrder.priceStock

    if(buyer.money < 0) {
      println("<0MF")
    }
    theMainCounter += 1

    changeAmountStock(candidate, buyer, buyerOrder.stockName, buyerOrder.amountStock)

    deleteTwoOrders(buyerOrder, candidateOrder)

    false
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

  def changeWhenCantBuyAll(buyer:ClientWithOrdersBuffer, buyerOrder:Order,
                           candidate:ClientWithOrdersBuffer, candidateOrder:Order, amountToBuy:Int): Boolean = {

    buyer.money -= amountToBuy * candidateOrder.priceStock
    candidate.money += amountToBuy * candidateOrder.priceStock

    if(buyer.money < 0) {
      println("<0MF")
    }
    theMainCounter += 1

    changeAmountStock(candidate, buyer, buyerOrder.stockName, amountToBuy)

    buyerOrder.amountStock -= amountToBuy
    candidateOrder.amountStock -= amountToBuy

    true
  }

  def sellOperation(order: Order, buySellOrdersBuffer:mutable.Buffer[Order], seller:ClientWithOrdersBuffer): Boolean = {
    for(i <- buySellOrdersBuffer.indices) {
      theMainCounter += 1
      if(i >= buySellOrdersBuffer.size)
        return false

      var candidateOrder = buySellOrdersBuffer(i)
      if(!order.name.equals(candidateOrder.name) && !order.operation.equals(candidateOrder.operation) &&
          order.stockName.equals(candidateOrder.stockName) &&
          order.priceStock.compareTo(candidateOrder.priceStock) != 1) {
        var candidate:ClientWithOrdersBuffer = null
        for(j <- clientsWithOrdersBuffer.indices) {
          if(candidateOrder.name.equals(clientsWithOrdersBuffer(j).name))
            candidate = clientsWithOrdersBuffer(j)
        }
        return switchWhensell(seller, order, candidate, candidateOrder)
      }
    }
    false
  }

  def changeIfWantSellMore(seller:ClientWithOrdersBuffer, sellerOrder:Order, candidate:ClientWithOrdersBuffer,
                           candidateOrder:Order, amountStock:Int): Boolean = {

    seller.money += amountStock * candidateOrder.priceStock
    candidate.money -= amountStock * candidateOrder.priceStock

    if(candidate.money < 0) {
      println("<0MF")
    }
    theMainCounter += 1

    changeAmountStock(seller, candidate, candidateOrder.stockName, amountStock)

    changeAndDeleteOrders(sellerOrder, amountStock, candidateOrder)

    true
  }

  def changeIfWantSellLess(seller:ClientWithOrdersBuffer, sellerOrder:Order, candidate:ClientWithOrdersBuffer,
                           candidateOrder:Order, amountStock:Int): Boolean = {

    seller.money += amountStock * candidateOrder.priceStock
    candidate.money -= amountStock * candidateOrder.priceStock

    if(candidate.money < 0) {
      println("<0MF")
    }
    theMainCounter += 1

    changeAmountStock(seller, candidate, candidateOrder.stockName, candidateOrder.priceStock)

    changeAndDeleteOrders(candidateOrder, amountStock, sellerOrder)

    false
  }

  def changeWhenCantSellAll(seller:ClientWithOrdersBuffer, sellerOrder:Order,
                           candidate:ClientWithOrdersBuffer, candidateOrder:Order, amountToSell:Int): Boolean = {

    seller.money += amountToSell * candidateOrder.priceStock
    candidate.money -= amountToSell * candidateOrder.priceStock

    if(candidate.money < 0) {
      println("<0MF")
    }
    theMainCounter += 1

    changeAmountStock(seller, candidate, candidateOrder.stockName, amountToSell)

    sellerOrder.amountStock -= amountToSell
    candidateOrder.amountStock -= amountToSell

    true
  }

  def sellWhenStockEqual(seller:ClientWithOrdersBuffer, sellerOrder:Order,
                           candidate:ClientWithOrdersBuffer, candidateOrder:Order): Boolean = {

    seller.money += candidateOrder.amountStock * candidateOrder.priceStock
    candidate.money -= candidateOrder.amountStock * candidateOrder.priceStock

    if(candidate.money < 0) {
      println("<0MF")
    }
    theMainCounter += 1

    changeAmountStock(seller, candidate, candidateOrder.stockName, candidateOrder.amountStock)

    deleteTwoOrders(sellerOrder, candidateOrder)

    false
  }

  def changeAmountStock(clientMinus:ClientWithOrdersBuffer, clientPlus:ClientWithOrdersBuffer,
                        stockName:Char, amountStock:Int): Unit = {
    if(stockName == 'A') {
      clientMinus.A -= amountStock
      clientPlus.A += amountStock
    }else if(stockName == 'B') {
      clientMinus.B -= amountStock
      clientPlus.B += amountStock
    }else if(stockName == 'C') {
      clientMinus.C -= amountStock
      clientPlus.C += amountStock
    }else {
      clientMinus.D -= amountStock
      clientPlus.D += amountStock
    }
  }

  def switchWhenBuy(buyer:ClientWithOrdersBuffer, buyerOrder:Order,
                    candidate:ClientWithOrdersBuffer, candidateOrder:Order): Boolean = {

    var amountWantBuy = buyerOrder.amountStock
    var amountWantSell = candidateOrder.amountStock

    var amountCanBuy = buyer.money / candidateOrder.priceStock
    var amountCanSell = findAmountStockToSell(candidate, candidateOrder)

    if(amountWantBuy <= amountCanBuy && amountWantSell <= amountCanSell) {
      if(amountWantBuy == amountWantSell) {
        //EqualBuy => return false
        return buyWhenStockEqual(buyer, buyerOrder, candidate, candidateOrder)
      }else if(amountWantBuy < amountWantSell) {
        //wantBuyLess => false
        return changeIfWantBuyLess(buyer, buyerOrder, candidate, candidateOrder, amountWantBuy)
      }else if(amountWantBuy > amountWantSell){
        //wantBuyMore => true
        return changeIfWantBuyMore(buyer, buyerOrder, candidate, candidateOrder, amountWantSell)
      }
    }

    if(amountWantBuy > amountCanBuy && amountWantSell <= amountCanSell) {
      if(amountCanBuy == amountWantSell) {
        //wantBuyMore => true
        return changeIfWantBuyMore(buyer, buyerOrder, candidate, candidateOrder, amountWantBuy)
      }else if(amountCanBuy < amountWantSell) {
        //cantBuyAll => true
        return changeWhenCantBuyAll(buyer, buyerOrder, candidate, candidateOrder, amountCanBuy)
      }else if (amountCanBuy > amountWantSell) {
        //wantBuyMore => true
        return changeIfWantBuyMore(buyer, buyerOrder, candidate, candidateOrder, amountWantSell)
      }
    }

    if(amountWantBuy <= amountCanBuy && amountWantSell > amountCanSell) {
      if(amountWantBuy == amountCanSell) {
        //wnatBuyLess => false
        return changeIfWantBuyLess(buyer, buyerOrder, candidate, candidateOrder, amountWantBuy)
      }else if(amountWantBuy < amountCanSell) {
        //wantBuyLess => false
        return changeIfWantBuyLess(buyer, buyerOrder, candidate, candidateOrder, amountWantBuy)
      }else if(amountWantBuy > amountCanSell) {
        //cantBuyAll => true
        return changeWhenCantBuyAll(buyer, buyerOrder, candidate, candidateOrder, amountCanSell)
      }
    }

    if(amountWantBuy > amountCanBuy && amountWantSell > amountCanSell) {
      if(amountCanBuy == amountCanSell) {
        //CantBuyAll => true
        return changeWhenCantBuyAll(buyer, buyerOrder, candidate, candidateOrder, amountCanBuy)
      }else if(amountCanBuy < amountCanSell) {
        //CantBuyAll => true
        return changeWhenCantBuyAll(buyer, buyerOrder, candidate, candidateOrder, amountCanBuy)
      }else if(amountCanBuy > amountCanSell){
        //CantBuyAll => true
        return changeWhenCantBuyAll(buyer, buyerOrder, candidate, candidateOrder, amountCanSell)
      }
    }
    false
  }

  def findAmountStockToSell(client:ClientWithOrdersBuffer, clientOrder:Order): Int = {
    var stockName = clientOrder.stockName
    stockName match {
      case 'A' => client.A
      case 'B' => client.B
      case 'C' => client.C
      case 'D' => client.D
    }
  }

  def switchWhensell(seller:ClientWithOrdersBuffer, sellerOrder:Order,
                     candidate:ClientWithOrdersBuffer, candidateOrder:Order): Boolean = {
    var amountWantBuy = candidateOrder.amountStock
    var amountWantSell = sellerOrder.amountStock

    var amountCanBuy = candidate.money / candidateOrder.priceStock
    var amountCanSell = findAmountStockToSell(seller, sellerOrder)

    if(amountWantBuy <= amountCanBuy && amountWantSell <= amountCanSell) {
      if(amountWantBuy == amountWantSell) {
        //EqualSell => return false
        return sellWhenStockEqual(seller,sellerOrder,candidate,candidateOrder)
      }else if(amountWantBuy < amountWantSell) {
        //wantSellMore => true
        return changeIfWantSellMore(seller, sellerOrder, candidate, candidateOrder, amountWantBuy)
      }else if(amountWantBuy > amountWantSell){
        //wantSellLess => false
        return changeIfWantSellLess(seller,sellerOrder,candidate,candidateOrder, amountWantSell)
      }
    }

    if(amountWantBuy > amountCanBuy && amountWantSell <= amountCanSell) {
      if(amountCanBuy == amountWantSell) {
        //wantSellLess => false
        return changeIfWantSellLess(seller, sellerOrder, candidate, candidateOrder, amountCanBuy)
      }else if(amountCanBuy < amountWantSell) {
        //cantSellAll => true
        return changeWhenCantSellAll(seller,sellerOrder,candidate,candidateOrder,amountCanBuy)
      }else if (amountCanBuy > amountWantSell) {
        //wantSellLess => false
        return changeIfWantSellLess(seller, sellerOrder, candidate, candidateOrder, amountWantSell)
      }
    }

    if(amountWantBuy <= amountCanBuy && amountWantSell > amountCanSell) {
      if(amountWantBuy == amountCanSell) {
        //wantSellMore => true
        return changeIfWantSellMore(seller, sellerOrder, candidate, candidateOrder, amountWantBuy)
      }else if(amountWantBuy < amountCanSell) {
        //wantSellMore => true
        return changeIfWantSellMore(seller, sellerOrder, candidate, candidateOrder, amountWantBuy)
      }else if(amountWantBuy > amountCanSell) {
        //cantSellAll => true
        return changeWhenCantSellAll(seller, sellerOrder, candidate, candidateOrder, amountCanSell)
      }
    }

    if(amountWantBuy > amountCanBuy && amountWantSell > amountCanSell) {
      if(amountCanBuy == amountCanSell) {
        //CantSellAll => true
        return changeWhenCantSellAll(seller, sellerOrder, candidate, candidateOrder, amountCanBuy)
      }else if(amountCanBuy < amountCanSell) {
        //CantSellAll => true
        return changeWhenCantSellAll(seller, sellerOrder, candidate, candidateOrder, amountCanBuy)
      }else if(amountCanBuy > amountCanSell){
        //CantSellAll => true
        return changeWhenCantSellAll(seller, sellerOrder, candidate, candidateOrder, amountCanSell)
      }
    }
    false
  }
}

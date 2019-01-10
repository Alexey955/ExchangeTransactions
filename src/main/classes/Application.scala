package main.classes

import scala.io.Source

object Application extends App{

  val clientsAmount = Source.fromFile("d:ExchangeTransactions/clients.txt").getLines().size
  val seqClients = Source.fromFile("d:ExchangeTransactions/clients.txt").getLines().toSeq
  val clientsBuffer = Client.createClients(seqClients, clientsAmount)

  Source.fromFile("d:ExchangeTransactions/orders.txt").getLines().foreach(println)

  val ordersAmount = Source.fromFile("d:ExchangeTransactions/orders.txt").getLines().size
  val seqOrder = Source.fromFile("d:ExchangeTransactions/orders.txt").getLines().toSeq
  val ordersBuffer = Order.createOrdersList(seqOrder, ordersAmount)

  val clientsThreadBuffer = ClientThread.createClientThread(clientsBuffer, clientsAmount)
  for(i <- 0 until clientsAmount)
    clientsThreadBuffer(i)

  clientsThreadBuffer(8).join()

  println("End")

}

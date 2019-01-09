package main.classes

import scala.io.Source

object Application extends App{
  
  val clientsAmount = Source.fromFile("d:ExchangeTransactions/clients.txt").getLines().size
  val seq = Source.fromFile("d:ExchangeTransactions/clients.txt").getLines().toSeq
  val clientsSeq = Client.createClients(seq, clientsAmount)

  Source.fromFile("d:ExchangeTransactions/clients.txt").getLines().foreach(println)

}

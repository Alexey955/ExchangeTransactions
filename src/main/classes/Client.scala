package classes

import scala.collection.mutable

class Client(private val name:String, private var money:Int, private var A:Int, private var B:Int, private var C:Int, private var D:Int) {

  def getName:String = {
    name
  }

  def getMoney:Int = {
    money
  }

  def getA:Int = {
    A
  }

  def getB:Int = {
    B
  }

  def getC:Int = {
    C
  }

  def getD:Int = {
    D
  }

  def plusMoney(amount:Int): Unit = {
    money += amount
  }

  def minusMoney(amount:Int): Unit = {
    money -= amount
  }

  def plusA(amount:Int): Unit = {
    A += amount
  }

  def minusA(amount:Int): Unit = {
    A -= amount
  }

  def plusB(amount:Int): Unit = {
    B += amount
  }

  def minusB(amount:Int): Unit = {
    B += amount
  }

  def plusC(amount:Int): Unit = {
    C += amount
  }

  def minusC(amount:Int): Unit = {
    C += amount
  }

  def plusD(amount:Int): Unit = {
    D += amount
  }

  def minusD(amount:Int): Unit = {
    D += amount
  }
}

object Client{

  def createClients(seq:Seq[String]):mutable.Buffer[Client] = {

    val clientsBuffert:mutable.Buffer[Client] = mutable.Buffer()

    for(i <- seq.indices) {
      val strFromSeq = seq(i)
      val subStr = strFromSeq.split("\t")
      val client = new Client(subStr(0), subStr(1).toInt, subStr(2).toInt, subStr(3).toInt, subStr(4).toInt, subStr(5).toInt)

      clientsBuffert.append(client)
    }
    clientsBuffert
  }
}
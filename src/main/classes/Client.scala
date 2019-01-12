package main.classes

import scala.collection.mutable

class Client(val name:String, var money:Int, var A:Int, var B:Int, var C:Int, var D:Int) {

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
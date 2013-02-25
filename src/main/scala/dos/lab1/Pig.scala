package dos.lab1
import scala.actors.Actor
import scala.actors.AbstractActor
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._

case object Ready

class Pig(val computer : String) extends Actor {
	def act() {
		val master = select(Node(Config.master.address, Config.master.port), Symbol(Config.master.address))
		val (name : String, port : Int, to : PigConfig) = (master !? Connect(computer))
		println("Pig: " + name + " reporting for duty on " + computer + " at port: " + port)
		alive(port)
		register(Symbol(name), self)
		var neighbor : AbstractActor = null
		if(to.port != 0) {
			println("Connecting to peer: " + to.name + " at computer: " + to.address + " at port: " + to.port)
			neighbor = select(Node(to.address, to.port), Symbol(to.name))
		}
		while(neighbor == null) {
			if( (master !? Nodes == Config.N) ) {
				val to : PigConfig = (master !? First).asInstanceOf[PigConfig]
				println("Completing the ring by connecting to peer: " + to.name + " at computer: " + to.address + " at port: " + to.port)
				neighbor = select(Node(to.address, to.port), Symbol(to.name))
				Thread.sleep(100)
			} 
		}
		(master ! Ready)
		loop {
      		react {
        		case Ready =>
        			println("Pinged")
      		}
    	}
	}
}

object Pig {
	def main(args : Array[String]) {
		Config.fromFile(args(0))
		RemoteActor.classLoader = getClass().getClassLoader()
		val pig = new Pig(args(1))
		pig.start()
	}
}
package dos.lab1
import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import scala.collection.mutable.ArrayBuffer

case class Connect(computer : String)
case object Nodes
case object First

class Master extends Actor {
	val connections = ArrayBuffer[PigConfig]()
	var upConnections = 0
	def ready : Boolean = (upConnections == Config.N)
	def getPig(address : String) : PigConfig = {
		assert(Config.computers.contains(address))
		val computer = Config.computers(address)
		val pig = computer.pigs.filter( x => !x.connected ).head
		pig.connected = true
		connections += pig
		pig
	}

	def getPrevious() : PigConfig = {
		if(connections.size == 0) new PigConfig("", "", 0)
		else connections.last
	}

	def act() {
		alive(Config.master.port)
		register(Symbol(Config.master.address), self)
		loop {
      		react {
        		case Connect(computer) => {
        			println("Connection Requested from " + computer)
        			val c2 = getPrevious()
        			val p = getPig(computer)
        			sender ! (p.name, p.port, c2)
        		}
        		case Nodes => {
        			sender ! connections.size
        		}
        		case First => {
        			sender ! connections.last
        		}
        		case Ready => {
        			upConnections += 1
        			if(upConnections == Config.N) println("All systems go!")
        		}
      		}
    	}
	}
}

object Master {
	def main(args : Array[String]) {
		RemoteActor.classLoader = getClass().getClassLoader()
		if(args.length < 1) {
			println("Usage: Master [config file]")
			System.exit(1)
		}
		Config.fromFile(args(0))
		val master = new Master()
		master.start()
	}
}
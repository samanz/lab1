package dos.lab1
import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.actors.AbstractActor
import scala.util.Random

case class Connect(computer : String)
case object Nodes
case object First

class Master extends Actor {
	val connections = ArrayBuffer[PigConfig]()
	val pigToCon = HashMap[Int, AbstractActor]()
	var upConnections = 0
	var game : Game  = null
	val rand = new Random
	def ready : Boolean = (upConnections == Config.N)
	def getPig(address : String) : PigConfig = {
		assert(Config.computers.contains(address))
		val computer = Config.computers(address)
		val pig = computer.pigs.filter( x => !x.connected ).head
		pig.connected = true
		connections += pig
		pig.idNumber = connections.size
		pig
	}

	def getPrevious() : PigConfig = {
		if(connections.size == 0) new PigConfig("", "", 0)
		else connections.last
	}
  
  	def affected(location : Int) : Array[Int] = {
  	  val stone = Config.N+1
  	  if(location >= 0 && location < game.board.size) {
  	    if(game.board(location) == stone) {
  	      if(location == game.landing)
  	        return affected(location+1) ++ affected(location-1)
  	      else if(location > game.landing)
  	        return affected(location+1)
  	      else
  	        return affected(location-1)
  	    }
  	    else if(game.finalBoard.values.toSet.contains(location)) {
  	      if(game.landing > location) return Array(location) ++ affected(location-1)
  	      else return Array(location) ++ affected(location+1)
  	    } else return Array()
  	  } else Array()
  	}


	def act() {
		alive(Config.master.port)
		register(Symbol(Config.master.address), self)
		var piggy : AbstractActor = null
		loop {
      		react {
        		case Connect(computer) => {
        			println("Connection Requested from " + computer)
        			val c2 = getPrevious()
        			val p = getPig(computer)
        			sender ! (p, c2)
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
        			for(con <- connections) {
        				pigToCon(con.idNumber) = select(Node(con.address, con.port), Symbol(con.name))
        			}
        		}
        		case SendGame(board) => {
        			for(piggy <- pigToCon.values)
        				piggy ! SendGame(board)
        		}
        		case Where => {
        			sender ! game.landing + (Config.game.error*(rand.nextDouble.round+(-1*rand.nextDouble.round)))
        		}
        		case Hit(landing) => {
        			for(piggy <- pigToCon.values) {
        				val (p : Int,l : Int) = (piggy !? Hit(landing))
        				game.updates += 1
        				game.finalBoard(p) = l
           			}
        		}
        		case Done(numHit) => {
        			game.success = numHit
        			game.done = true
        		}
        		case Final(status) => {
        			val aff = affected(game.landing).toSet
    				println("affed: " + aff.mkString(";"))
        			for(p <- pigToCon) {
        				val piggy = p._2
        				piggy ! Final(aff.contains(game.finalBoard(p._1)))
           			}
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
package dos.lab1
import scala.actors.Actor
import scala.actors.AbstractActor
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._

case object Ready
case object Where
case class ForwardConnect(pig : PigConfig)
case class BirdApproaching(position : Int, hopcount:Int)
case class Move(position : Int, hopcount:Int)
case class TakeShelter(pigId : Int, loc : Int, hopcount:Int)

class Pig(val computer : String) extends Actor {
	var pigBoard = new Array[Int](Config.game.size)
	var pConfig : PigConfig = null
	var nConfig : PigConfig = null
	var me : PigConfig = null
	var stone = Config.N+1
	var myLocation = 0


  def willHitMe(location : Int, fromPig : Boolean = false) = {
    if(location == myLocation) true
    if(pigBoard(location) == stone) {
      if( (location-1 >= 0 && willHitMe(location-1)) || (location+1 < pigBoard.size && willHitMe(location+1))) true else false  
    } else if(pigBoard(location) >= 0 && !fromPig) {
      if(location+1 == pigBoard.size) false else(willHitMe(location+1, true))
    }
  }

  def simpleWillHitMe(location : Int) = {
    if(location == myLocation) true
    if(pigBoard(location) == stone) {
      if( (location-1 >= 0 && willHitMe(location-1)) || (location+1 < pigBoard.size && willHitMe(location+1))) true else false  
    }
  }


  def moveToSafety(location : Int) : Array[Int] { 
    val neighbors = ArrayBuffer[Int]()
    if(pigBoard(myLocation-1) > 0 && pigBoard(myLocation-1) < stone) neighbors += pigBoard(myLocation-1)
    if(myLocation+1 < pigBoard.size && pigBoard(myLocation+1) > 0 && pigBoard(myLocation+1) < stone) neighbors += pigBoard(myLocation+1)
    if(myLocation <= location) {
      if(myLocation-1 >=0 && pigBoard(myLocation-1) != stone) {
        myLocation -= 1
      } else if( myLocation+1 < pigBoard.size && pigBoard(myLocation+1) != stone) {
        myLocation += 1
      }
    } else {
      if(myLocation-1 >=0 && pigBoard(myLocation-1) != stone) {
        myLocation -= 1
      } else if( myLocation+1 < pigBoard.size && pigBoard(myLocation+1) != stone) {
        myLocation += 1
      }
    }
  }

	def act() {
		val master = select(Node(Config.master.address, Config.master.port), Symbol(Config.master.address))
		val (p : PigConfig, to : PigConfig) = (master !? Connect(computer))
		me = p
		println("Pig: " + me.name + " reporting for duty on " + computer + " at port: " + me.port)
		alive(me.port)
		register(Symbol(me.name), self)
		var neighbor : AbstractActor = null
		var next : AbstractActor = null
		if(to.port != 0) {
			println("Connecting to peer: " + to.name + " at computer: " + to.address + " at port: " + to.port)
			neighbor = select(Node(to.address, to.port), Symbol(to.name))
			pConfig = to
		}
		while(neighbor == null) {
			Thread.sleep(100)
			if( (master !? Nodes == Config.N) ) {
				val to : PigConfig = (master !? First).asInstanceOf[PigConfig]
				println("Completing the ring by connecting to peer: " + to.name + " at computer: " + to.address + " at port: " + to.port)
				neighbor = select(Node(to.address, to.port), Symbol(to.name))
				pConfig = to
				(neighbor ! ForwardConnect(me))
			} 
		}
		loop {
      		react {
      			case ForwardConnect(pig) => {
      				if(next == null) {
      					next = select(Node(pig.address, pig.port), Symbol(pig.name))
      					nConfig = pig
      					println("Connecting also to peer: " + pig.name + " at computer: " + pig.address + " at port: " + pig.port)
      					(neighbor ! ForwardConnect(me))
      					(master ! Ready)
      				}
      			}
        		case BirdApproaching(location, hopcount) => {
        			( neighbor ! BirdApproaching(location, Config.N) )
        			if(willHitMe(location) {
                val pigsToMove = moveToSafety(location)
                for(p <- pigsToMove) {
                  Scheduler.schedule( { Actor.actor { neighbor ! TakeShelter(p, location, Config.N/2+1) }; () }, Config.game.messageDelay)
                  Scheduler.schedule( { Actor.actor { next ! TakeShelter(p, location, Config.N/2+1) }; () }, Config.game.messageDelay)
                }
              }
        			if(hopcount > 1) {
						    Scheduler.schedule( { Actor.actor { neighbor ! BirdApproaching(location, hopcount-1) }; () }, Config.game.messageDelay)
        			  Scheduler.schedule( { Actor.actor { next ! BirdApproaching(location, hopcount-1) }; () }, Config.game.messageDelay)
              }
        		}
            case TakeShelter(pigId, loc, hopcount) => {
              if(me.idNumber==pigId) {
                if(location <= myLocation) {
                  if(location+1 < pigBoard.size && pigBoard(location+1) != stone) myLocation += 1
                  else if(location-1 >0 && pigBoard(location-1) != stone) myLocation -= 1
                } else {
                  if(location-1 >0 && pigBoard(location-1) != stone) myLocation -= 1
                  else if(location+1 < pigBoard.size && pigBoard(location+1) != stone) myLocation += 1
                }
              }
              if(hopcount > 1) {
                Scheduler.schedule( { Actor.actor { neighbor ! TakeShelter(pigId, loc, hopcount) }; () }, Config.game.messageDelay)
                Scheduler.schedule( { Actor.actor { next ! TakeShelter(pigId, loc, hopcount) }; () }, Config.game.messageDelay)
              }
            }
        		case SendGame(board) => {
        			pigBoard = board
        			Game.printBoard(pigBoard)
        			var firstPig = 0
        			while(pigBoard(firstPig) == 0 || pigBoard(firstPig) == stone) firstPig += 1
        			myLocation = 0
        			while(pigBoard(myLocation) != me.idNumber) myLocation += 1
        			if(me.idNumber==pigBoard(firstPig)) {
        				println("I'm the closest pig to the launch pad! Secret information gathering session commencing!")
        				var hitLocation : Int = (master !? Where).toString.toInt
        				println("The probable hit location: " + hitLocation)
        				println("Proprogating")
						    Scheduler.schedule( { neighbor ! BirdApproaching(location, Config.N/2+1) }; () }, Config.game.messageDelay)
        			  Scheduler.schedule( { next ! BirdApproaching(location, Config.N/2+1) }; () }, Config.game.messageDelay)
              }
        		}
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

object Scheduler {
  import java.util.concurrent.Executors
  import scala.compat.Platform
  import java.util.concurrent.TimeUnit
  private lazy val sched = Executors.newSingleThreadScheduledExecutor();
  def schedule(f: => Unit, time: Long) {
    sched.schedule(new Runnable {
      def run = f
    }, time , TimeUnit.MILLISECONDS);
  }
}
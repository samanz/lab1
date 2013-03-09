package dos.lab1
import scala.actors.Actor
import scala.actors.AbstractActor
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import java.util.UUID
import scala.collection.mutable.{ArrayBuffer, HashMap}


case object Ready
case object Where
case class ForwardConnect(pig : PigConfig)
case class BirdApproaching(position : Int, messageId : UUID, hopcount:Int)
case class Move(position : Int, hopcount:Int)
case class TakeShelter(pigId : Int, loc : Int, messageId : UUID)
case class StatusAll(hopcount : Int, trav : Array[Int], messageId : UUID)
case class WasHit(pigId : Int, status : Boolean, trav : Array[Int])
case class Done(numHit : Int)

class Pig(val computer : String) extends Actor {
	var pigBoard = new Array[Int](Config.game.size)
	var pConfig : PigConfig = null
	var nConfig : PigConfig = null
	var me : PigConfig = null
	var stone = Config.N+1
	var myLocation = 0
  val seenMessages = new HashMap[UUID, Boolean]()
  var sheltered = false
  var statusHit = false
  var isFirst = false
  var finalBoard : HashMap[Int, Int] = null
  var landedLoc : Int = 0
  var numHit = 0
  var responses = 0

  def simpleWillHitMe(location : Int) : Boolean = {
    if(location < 0 || location >= pigBoard.size) return false
    if(location == myLocation) return true
    var m = location
    while(m >= 1 && (pigBoard(m)==stone)) m -= 1
    if(m == myLocation) return true
    else if(pigBoard(m) > 0 && pigBoard(m) < stone) return false

    m = location
    while(m < pigBoard.size && (pigBoard(m)==stone)) m += 1
    (m==myLocation)
  }

  def moveToSafety(location : Int) : Array[Int] = { 
    val neighbors = ArrayBuffer[Int]()
    var ns = ArrayBuffer[Int]()
    var l = myLocation
    if(myLocation-1 >= 0 && pigBoard(myLocation-1) > 0 && pigBoard(myLocation-1) < stone) ns += pigBoard(myLocation-1)
    if(myLocation+1 < pigBoard.size && pigBoard(myLocation+1) > 0 && pigBoard(myLocation+1) < stone) ns += pigBoard(myLocation+1)
    while(l >=0 && pigBoard(l) > 0) {
      if(pigBoard(l) != stone ) ns += pigBoard(l)
      l -= 1
    }
    l=myLocation
    while(l < pigBoard.size && pigBoard(l) > 0) {
      if(pigBoard(l) != stone ) ns += pigBoard(l)
      l += 1
    }
    if(location < myLocation) {
      if(myLocation+1 < pigBoard.size && pigBoard(myLocation+1) != stone) myLocation += 1
    } else if(location > myLocation) {
        if(myLocation-1 >0 && pigBoard(myLocation-1) != stone) myLocation -= 1
    } else {
        if(myLocation+1 < pigBoard.size && pigBoard(myLocation+1) != stone) myLocation += 1
        else if(myLocation-1 >0 && pigBoard(myLocation-1) != stone) myLocation -= 1
    }
    ns.toSet.toArray
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
            case Hit(landing) => {
              println("I know where it hit!")
              sender ! (me.idNumber, myLocation)
              landedLoc = landing
            }
      			case ForwardConnect(pig) => {
      				if(next == null) {
      					next = select(Node(pig.address, pig.port), Symbol(pig.name))
      					nConfig = pig
      					println("Connecting also to peer: " + pig.name + " at computer: " + pig.address + " at port: " + pig.port)
      					(neighbor ! ForwardConnect(me))
      					(master ! Ready)
      				}
      			}
        		case BirdApproaching(location, messageId, hopcount) => {
        			if(!seenMessages.contains(messageId)) {
                println("accepting BirdApproaching message")
                seenMessages(messageId) = true
                if(simpleWillHitMe(location)) {
                  println("It will probably hit me! Taking evasive action.")
                  val pigsToMove = moveToSafety(location)
                  for(p <- pigsToMove) {
                    println("Sending TakeShelter")
                    val messageId = UUID.randomUUID
                    seenMessages(messageId) = true
                    Actor.actor { 
                      Thread.sleep(Config.game.messageDelay)
                      neighbor ! TakeShelter(p, location, messageId)
                      next ! TakeShelter(p, location, messageId)
                    }
                  }
                }
        			 if(hopcount > 1) {
                 println("Proprogating BirdApproaching")
						     Actor.actor { 
                  Thread.sleep(Config.game.messageDelay)
                  neighbor ! BirdApproaching(location, messageId, hopcount-1) 
        			    next ! BirdApproaching(location, messageId, hopcount-1)
                }
              }
        		}
          }
            case TakeShelter(pigId, loc, messageId) => {
              if(!seenMessages.contains(messageId)) {
                seenMessages(messageId) = true
                if(me.idNumber==pigId && sheltered == false) {
                  println("accepting TakeShelter message, attempting to take evasive action.")
                  sheltered = true
                  if(loc < myLocation) {
                    if(myLocation+1 < pigBoard.size && pigBoard(myLocation+1) != stone) myLocation += 1
                  } else if(loc > myLocation) {
                    if(myLocation-1 >0 && pigBoard(myLocation-1) != stone) myLocation -= 1
                  } else {
                    if(myLocation+1 < pigBoard.size && pigBoard(myLocation+1) != stone) myLocation += 1
                    else if(myLocation-1 >0 && pigBoard(myLocation-1) != stone) myLocation -= 1
                  }
                } else {
                  Actor.actor { 
                    Thread.sleep(Config.game.messageDelay)
                    neighbor ! TakeShelter(pigId, loc, messageId)
                    next ! TakeShelter(pigId, loc, messageId)
                  }
                }
              }
            }
            case Final(status) => {
              println("And the final word is?")
              //finalBoard = bd
              statusHit = status//wasIHit()
              if(statusHit) println("Im hit!") else println("I'm safe!")
              if(isFirst) {
                val messageId = UUID.randomUUID
                seenMessages(messageId) = true
                println("Querying statusAll")
                Actor.actor { 
                  Thread.sleep(Config.game.messageDelay)
                  neighbor ! StatusAll(Config.N/2+1, Array(me.idNumber), messageId)
                  next ! StatusAll(Config.N/2+1, Array(me.idNumber), messageId)
                }
              }
            }
            case WasHit(pigId, status, trav) => {
              println("Got Was Hit message with trav: " + trav.mkString(","))
              if(trav.length==0) {
                if(status) numHit += 1
                responses += 1
                if(responses == Config.N-1) {
                  if(statusHit) numHit += 1
                  master ! Done(numHit)
                }
              } else {
                if(pConfig.idNumber==trav.last) {
                  Actor.actor { 
                    Thread.sleep(Config.game.messageDelay)
                    neighbor ! WasHit(pigId, status, trav.take(trav.length-1))
                  }
                }
                if(nConfig.idNumber==trav.last) {
                  Actor.actor { 
                    Thread.sleep(Config.game.messageDelay)
                    next ! WasHit(pigId, status, trav.take(trav.length-1))
                  }
                }
              }
            }
            case StatusAll(hopcount, trav, messageId) => {
              println("Got StatusAll message: with trav " + trav.mkString(","))
              if(!seenMessages.contains(messageId)) {
                println("accepting StatusAll message")
                seenMessages(messageId) = true
                if(pConfig.idNumber==trav.last) {
                  Actor.actor {
                    Thread.sleep(Config.game.messageDelay)
                    neighbor ! WasHit(me.idNumber, statusHit, trav.take(trav.length-1))
                  }
                }
                if(nConfig.idNumber==trav.last) {
                  Actor.actor { 
                    Thread.sleep(Config.game.messageDelay)
                    next ! WasHit(me.idNumber, statusHit, trav.take(trav.length-1)) 
                  }
                }
                Actor.actor { 
                  Thread.sleep(Config.game.messageDelay)
                  neighbor ! StatusAll(hopcount-1, trav ++ Array(me.idNumber), messageId) 
                  next ! StatusAll(hopcount-1, trav ++ Array(me.idNumber), messageId)
                }
              }
            }
        		case SendGame(board) => {
              sheltered = false
              statusHit = false
        			pigBoard = board
              finalBoard = null
              landedLoc = 0
        			Game.printBoard(pigBoard)
        			var firstPig = 0
        			while(pigBoard(firstPig) == 0 || pigBoard(firstPig) == stone) firstPig += 1
        			myLocation = 0
              numHit = 0
              responses = 0
        			while(pigBoard(myLocation) != me.idNumber) myLocation += 1
              isFirst = me.idNumber==pigBoard(firstPig)
        			if(me.idNumber==pigBoard(firstPig)) {
        				println("I'm the closest pig to the launch pad! Secret information gathering session commencing!")
        				var hitLocation : Int = (master !? Where).toString.toInt
        				println("The probable hit location: " + hitLocation)
        				println("Proprogating")
                val messageId = UUID.randomUUID
                //seenMessages(messageId) = true
						    Actor.actor { 
                  Thread.sleep(Config.game.messageDelay)
                  neighbor ! BirdApproaching(hitLocation, messageId, Config.N/2+1) 
                  next ! BirdApproaching(hitLocation, messageId, Config.N/2+1)
                }
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
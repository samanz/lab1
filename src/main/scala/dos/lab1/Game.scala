package dos.lab1
import scala.actors.Actor
import scala.actors.AbstractActor
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import scala.util.Random
import scala.collection.mutable.HashMap

case class SendGame(board : Array[Int])
case class Hit(landing : Int)
case class Final(finalBoard : HashMap[Int,Int])

class Game(val master : Master) {
	master.game = this
    var updates = 0
	val rand = new Random
	val board = new Array[Int](Config.game.size)
    val finalBoard = new HashMap[Int, Int]()
	var landing = -1
    while(!master.ready) { Thread.sleep(1000) }
    println("Starting game")
    var success = 0
    var done = false

    var round = 0
    var ended = false
    while(true) {
    	round += 1
        updates = 0
        success = 0
        done = false
    	println("Playing round %d".format(round))
    	randomizeGame()
        loadFinalBoard()
    	landing = rand.nextInt(Config.game.size)
        println("Will hit location: " + landing)
    	Game.printBoard(board)
    	(master ! SendGame(board))
    	val speed = rand.nextInt(9900) + 100
        println("Will hit in: " +speed)
    	Thread.sleep(speed)
        println("Hitting!")
    	(master ! Hit(landing))
        println("Waiting for updates!")
        while(updates != Config.N) Thread.sleep(1000)
        println("Lets do it!")
        Game.printBoard(board, finalBoard)
        (master ! Final(finalBoard))
        println("Are we done yet?!")
        while(!done) Thread.sleep(1000)
    	println("Game Stats: num hit: " + success)
    }

    def randomizeGame() {
		(0 until board.length).foreach( board(_) = 0 )    	
		val stone = Config.N+1
    	val stones = rand.nextInt(7)
    	for( i <- 0 until stones) {
    		var placed = false
    		while(!placed) {
    			val place = rand.nextInt(Config.game.size)
    			if(board(place) == 0) { board(place) = stone; placed = true }
    		}
    	}
    	for(i <- master.connections) {
    		var placed = false
    		while(!placed) {
    			val place = rand.nextInt(Config.game.size)
    			if(board(place) == 0) { 
    				board(place) = i.idNumber; placed = true; 
    				if(place-1 > 0 && place-1 < Config.N && board(place) == 0 && rand.nextDouble > .25) board(place-1) = stone
    				else if(place+1 > 0 && place+1 < Config.N && board(place+1) == 0 && rand.nextDouble > .25) board(place+1) = stone
    			}
    		}
    	}
    }

    def loadFinalBoard() {
        for(i <- 0 until board.size) {
            if(board(i) < Config.N+1 && board(i) > 0) finalBoard(board(i)) = i
        }
    }
}

object Game {
	def printBoard(board : Array[Int]) {
    	println( board.map{ x => 
    		if(x==0) "."
    		else if(x==Config.N+1) "[]"
    		else "("+x+")" 
    		}.mkString("") )
    }

    def printBoard(board : Array[Int], fb : HashMap[Int,Int]) {
        fb.foreach( x => println(x._1 +"=>" + x._2))
        println( board.zipWithIndex.map{ x => 
            if(x._1==Config.N+1) "[]"
            else  {
                if(!fb.values.toSet.contains(x._2)) (".")
                else {
                    val as = fb.filter( i => i._2 == x._2 )
                    ("(" + as.keys.mkString("|") + ")")
                }
            } 
            }.mkString("") )
    }


	def main(args : Array[String]) {
		RemoteActor.classLoader = getClass().getClassLoader()
		if(args.length < 1) {
			println("Usage: Game [config file]")
			System.exit(1)
		}
		Config.fromFile(args(0))
		val master = new Master()
		master.start()
		val game = (new Game(master))
	}
}
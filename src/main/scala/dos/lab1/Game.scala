package dos.lab1
import scala.actors.Actor
import scala.actors.AbstractActor
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import scala.util.Random

case class SendGame(board : Array[Int])
case class Hit(landing : Int)

class Game(val master : Master) {
	master.game = this
	val rand = new Random
	val board = new Array[Int](Config.game.size)
	var landing = -1
    while(!master.ready) { Thread.sleep(1000) }
    println("Starting game")

    var round = 0
    var ended = false
    while(true) {
    	round += 1
    	println("Playing round %d".format(round))
    	randomizeGame()
    	landing = rand.nextInt(Config.game.size)
    	Game.printBoard(board)
    	(master ! SendGame(board)
    	val speed = rand.nextInt(9900) + 100
    	Thread.sleep(speed)
    	(master !? Hit(landing))
    	println("Game Stats: ")
    	Thread.sleep(10000)
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
}

object Game {
	def printBoard(board : Array[Int]) {
    	println( board.map{ x => 
    		if(x==0) "_"
    		else if(x==Config.N+1) "[]"
    		else "."
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
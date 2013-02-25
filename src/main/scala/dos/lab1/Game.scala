package dos.lab1

class Game(val master : Master) extends Actor {
	val board = new Array[Int](Config.game.size)
    while(!master.ready) { Thread.sleep(100) }


    def act() {

    }
}

object Game {
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
		game.start()
	}
}
package dos.lab1
import net.liftweb.json._
import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, HashMap}

/*
	Singleton Object containing the configuration information for the PigGame
*/
@scala.serializable
class PigConfig(val name : String, val address : String, val port : Int) {
	var connected = false
	var idNumber = -1
}
class GameConfig(val size : Int, val messageDelay : Int)
class Computer(val address : String, pig : PigConfig) {
	val pigs = ArrayBuffer[PigConfig]()
	pigs += pig 
}
class MasterConfig(val address : String, val port : Int)

object Config {
	val computers = HashMap[String,Computer]()
	var pigs : Array[(String, String, Int)] = null
	var master : MasterConfig = null
	var game : GameConfig = null
	var N = 0
	def fromFile(configFile : String) {
		val fileData = Source.fromFile(configFile).mkString
		val json = parse(fileData)
		val p = new ArrayBuffer[(String, String, Int)]()
		(json \ "pigs").children.foreach{ x => 
			val pig = new PigConfig((x \ "name").values.toString, (x \ "address").values.toString, (x \ "port").values.toString.toInt)
			if(computers.contains(pig.address)) computers(pig.address).pigs += pig
			else computers(pig.address) = new Computer(pig.address, pig)
			N += 1
		}
		pigs = p.toArray
		val m = (json \ "master")
		master = new MasterConfig( (m \ "address").values.toString,  (m \ "port").values.toString.toInt )
		val g = (json \ "game")
		game = new GameConfig( (g \ "boardSize").values.toString.toInt, (g \ "messageDelay").values.toString.toInt )
	}
}
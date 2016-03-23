package gspm

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.util._
import scala.io.Source
import Array._
import java.io._
import  DBgspm.CreateTDB._
import oscar.cp._
import ItemSetMining._
object gspm extends CPModel with App {
	val map = Map(1 -> "A," ,2 -> "B,", 3-> "C,", 4-> "D,",5 -> "E,", 6 ->"F, ");


	for(x <- (1 to 10)){
		var frequencyMultiplicator = x/100.0
				var M = 0
				var N = 1
				val out = 
				gspmArray("/Users/armandbosquillondejenlis/repos/dataminingusingcp/oscar-cp/src/main/scala/DBgspm/LEVIATHAN.txt")
				val SDB = out._1
				var all = out._2
				all += 0
				val l = out._3
				var frequency = (SDB.size*frequencyMultiplicator).round.toInt
				//printTDB(SDB)
				println("\n\nfrequency = "+frequencyMultiplicator+ ", M= "+M+", N= " +N)
				//all.foreach { x => print(" ,"+x) }

				val sigma = Array.fill(l)(CPIntVar(all))
				sigma(0).removeValue(0)


				add(new FILTER_GAP_SEQ(SDB,sigma,frequency,M,N,l,all))

				var nSol = 0
				onSolution { nSol += 1 
				//sigma.foreach { x => print(map.getOrElse(x.value, 0)) }

		}

		search {
			binaryFirstFail(sigma)
		}
		val stats = start()
				//print some statistics
				println()
				println("#sol:" + nSol)
				println("Time = "+stats.time + "\n" + "nNodes = "+stats.nNodes)

	}
}


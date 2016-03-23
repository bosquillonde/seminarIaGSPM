package ItemSetMining

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.util._
import oscar.cp._
import oscar.util._
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPOutcome
import oscar.algo.reversible._
class FILTER_GAP_SEQ(SDB:Array[Array[Int]],sigma : Array[CPIntVar],frequency:Int,M:Int,N:Int,L:Int,All:Set[Int]) extends Constraint(sigma(1).store, "Frequency") {
	//TODO maintain a j
	val map = Map(1 -> "A," ,2 -> "B,", 3-> "C,", 4-> "D,",5 -> "E,", 6 ->"F, ");
	var ALLOCC : Array[Set[(Int,Set[Array[Int]])]] = Array.fill(L+1)(Set())



			override def setup(l: CPPropagStrength): CPOutcome =  {
		(0 until sigma.length).foreach { i => sigma(i).callValBindIdxWhenBind(this,i+1) }
		var i:Int = 1
				var ALLOCC0 : Set[(Int,Set[Array[Int]])] = Set()
				while(i<=SDB.size){          
					ALLOCC0 = ALLOCC0 + ((i,Set(Array(0,0))))
							i += 1
				}
		ALLOCC.update(0, ALLOCC0)
		//print("\nfirst allocc")
		//pa(ALLOCC0)
		//println(ALLOCC)
		var element = sigma(0)
		var sizeSigma = 0

		var out = getRightExt(sizeSigma,0)
		var all = out._1
		var Extr = out._2


		if(Extr.size<frequency){
			return Failure
		}
		if(sizeSigma >=2 && element == 0){
			for(e <- sigma ; if(!e.isBound)){
				e.assign(0)//TODO ça marche?
			}
		}else{
			var rf = getUnFrequentItems(Extr,all,All)
					rf.foreach { x => sigma(sizeSigma).removeValue(x) }
		}
		return Suspend
	}


	override def valBindIdx(element: CPIntVar, sizeSigma: Int): CPOutcome = {


		//println("\n\n\nvalBind, sizeSigma = "+sizeSigma)
		//print("variables are set to: ")
		//println()
		//sigma.foreach { x => print(x.toString()) }
		var allBound = true
				for(v <- sigma){
					if(!v.isBound)
						allBound = false
				}
		if(allBound){
			//println("\n\nsuccess all bound")
			//sigma.foreach { x => print(x.toString()+", ") }
			return Success

		}

		if(sizeSigma >=2 && element.value == 0){
			//println("put every item to 0")
			for(e <- sigma ; if(!e.isBound)){
				e.assign(0)//TODO ça marche?
			}
			//println("\n\nsuccess zero")
			//sigma.foreach { x => print(x.toString()+", ") }
			return Success
		}else{
			var out = getRightExt(sizeSigma,element.value)
					var all = out._1
					var Extr = out._2
					//pre(Extr)

					var rf = getUnFrequentItems(Extr,all,All)
					//println("unfrequent are")
					//rf.foreach { x => print(" "+x) }
					rf.foreach { x => sigma(sizeSigma).removeValue(x) }
		}
		return Suspend
	}

	def getUnFrequentItems( Extr : Set[(Int,Set[Int])], all : Set[Int], All:Set[Int]) : Set[Int] = {
		var unfrequentItems : Set[Int] = Set()
				All.filter { x => !all.contains(x) && x!= 0  }.foreach { x => unfrequentItems +=x }
				for(e <- all){
					var count = 0
							for(pair <- Extr){
								if(pair._2.contains(e)){
									count +=1
								}
							}
					if(count < frequency){
						unfrequentItems += e
					} 
				}
				return unfrequentItems
	}

	def getRightExt(sizeSigma:Int , element:Int ) : (Set[Int],Set[(Int,Set[Int])]) = {//TODO alloc en général

		var Extr : Set[(Int,Set[Int])] = Set();
	var all : Set[Int] = Set();
	if(sizeSigma ==0){//TODO vérifie que ça fonctionne
		var  sid = 1;
		for(a <- SDB){
			var set : Set[Int] = Set() ;
		for(e <- a){
			all += e;
			set += e;
		}
		Extr += ((sid,set));
		sid +=1;
		}         
		return (all,Extr);
	}
	//println("\nold allocc, sizeSigma = "+sizeSigma)
	//pa(ALLOCC(sizeSigma-1))
	ALLOCC(sizeSigma)= GETALLOCC(sizeSigma , element )
			//print("\nnew allocc")
			//pa(ALLOCC(sizeSigma))
			for(p <- ALLOCC(sizeSigma)){var sid = p._1; var OccSet = p._2;
			var s = SDB(sid-1); var set : Set[Int] = Set();
			for(a<-OccSet){ var j1 = a(0); var jm = a(1);
			var jprim1 = jm+M+1; var jprimM = math.min(jm+N+1,s.length)
					if(jprim1 <= jprimM){
						var i =jprim1
								while(i<=jprimM){
									all += s(i-1);
									set += s(i-1)
											i+=1
								}
					}    
			}
			Extr += ((sid,set))
			}
	return (all,Extr)
	}





	def GETALLOCC( sizeSigma:Int , element:Int) : Set[(Int,Set[Array[Int]])] ={
		var newALLOCC : Set[(Int,Set[Array[Int]])] = Set(); var inf = 0; var sup = 0;
		for(pair <- ALLOCC(sizeSigma-1)){val sid = pair._1; var OccSet = pair._2; //println();print("id = "+pair._1 + "  ")
		var s = SDB(sid-1); var newOccSet : Set[Array[Int]] = Set(); var redundant = false; //var i = 1;
		for(a <- OccSet;if(!redundant)){ //print("["+a(0)+","+a(1)+"], "); //redundant = false //TODO remove ça
			var j1 = a(0); var jm = a(1);
			if(sizeSigma == 1){///////
				inf = 1; sup = s.length;
			}else{ //println("min("+(jm+N+1)+","+s.length+")")
				inf = jm + M + 1; sup = math.min(jm+N+1,s.length)
						//print(" = " + sup)
			}
			var k = inf;
			//println("inf = "  +inf +" sup = " + sup)
			while(k<=sup && !redundant){
				if(s(k-1)==element){
					if(sizeSigma == 1){//////////
						newOccSet += Array(k,k)
					}else{
						newOccSet += Array(j1,k)
					}
					if((sup == s.length && sizeSigma > 1) || N>s.length){
						redundant = true; //print("here")
					}
				}
				k+=1
			}
		}       
		if(!newOccSet.isEmpty){
			newALLOCC += ((sid,newOccSet))
		}
		}
		return newALLOCC
	}

	def pre(Extr : Set[(Int,Set[Int])]){
		println("Right pattern extention")
		for(pair <- Extr){
			print("id = "+pair._1 + "  ")
			for(e <- pair._2){
				print(map.getOrElse(e, 7))
			}
			println()
		}
	}
	def pa(ALLOCC : Set[(Int,Set[Array[Int]])]){
		println()
		for(pair <- ALLOCC){
			print("id = "+pair._1 + "  ")
			for(a <- pair._2){
				print("["+a(0)+","+a(1)+"], ")
			}
			println()
		}
	}
}

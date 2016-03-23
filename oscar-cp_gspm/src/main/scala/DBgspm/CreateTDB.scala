package DBgspm
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.util._
import scala.io.Source
import Array._
import java.io._


object CreateTDB {


	def from01Files(fileName: String): Array[Array[Int]] = {val fileLines = Source
			.fromFile( fileName).getLines.toList
			val myArray = fileLines.filterNot(_.isEmpty).map { line =>
			(line.toArray).filter(e => e != ' ')
	}.toArray
	var TDB = ofDim[Int](myArray.length,myArray(0).length)
	for(i <- 0 until myArray.length){
		println()
		for(j <- 0 until myArray(0).length){
			TDB(i)(j) = myArray(i)(j).toInt - 48
					//print(TDB(i)(j))
		}
	} 
	println()
	return TDB

	}  

	//Items id start at 1
	def fromNormalFiles(fileName: String): Array[Array[Int]] = {

			val fileLines = Source
					.fromFile(fileName).getLines

					val myArray = fileLines.map { line => line.mkString.split("\\s+").map(_.toInt) }.toArray
					var max =0
					val list: List[Array[Int]] = List()
					for(e <- myArray){
						for(j <- 0 until e.length){      
							//print(e(j)+ " ")
							if(e(j)>max)
								max = e(j)
						}
					}
					//println(max)
					var TDB = ofDim[Int](myArray.length,max)
							var pointer = 0
							var i = 0
							for(e <- myArray){
								pointer = 0
										for(x <- 0 until e.length){ 
											for(j <- pointer to e(x)-1){
												if(j == e(x)-1){
													TDB(i)(j) = 1 
												}else{
													TDB(i)(j) = 0 
												}
											}
											pointer = e(x)
										}
								for(j <- pointer until max){
									TDB(i)(j) = 0
								}
								i +=1
							}

					/*val writer = new PrintWriter(new File("/Users/armandbosquillondejenlis/Documents/master1/q8/constraint/oscar-cp-project-1.1.0/src/ItemSetMining/cheout.txt" ))

      writer.write("Hello Scala")
      writer.write("\n")
      writer.write("Hello Scala")
      writer.close()*/
					return TDB

	}  
	//Items id start at 0 with a class label at the end of each lines
	def fromNormalFiles2(fileName: String): Array[Array[Int]] = {

			val fileLines = Source
					.fromFile(fileName).getLines
					// fileLines.foreach { x => println(x.mkString) }
					val myArray = fileLines.map { line => line.mkString.split("\\s+").map(_.toInt) }.toArray

					var max =0
					val list: List[Array[Int]] = List()
					var trans = 1
					for(e <- myArray){
						//println()
						for(j <- 0 until e.length){      
							// print(e(j)+ " ")
							if(e(j)>max)
								max = e(j)
								//println("new max = "+e(j)  )
						}
						// print(" " + trans)         
						trans += 1
					}
					max +=1
							// println("max = " + max)
							var TDB = ofDim[Int](myArray.length,max)
							//print(TDB(0).length)
							var pointer = 0
							var i = 0
							for(e <- myArray){
								//println()
								pointer = 0
										for(x <- 0 until e.length-1){ 
											for(j <- pointer to e(x)){
												if(j == e(x)){
													TDB(i)(j) = 1 
															//print(1)
												}else{
													TDB(i)(j) = 0
															//print(0)
												}
											}
											pointer = e(x)+1
										}
								for(j <- pointer until max){
									TDB(i)(j) = 0
											//print(0)
								}
								i +=1
							}
					/*val writer = new PrintWriter(new File("/Users/armandbosquillondejenlis/Documents/master1/q8/constraint/oscar-cp-project-1.1.0/src/ItemSetMining/cheout.txt" ))

      writer.write("Hello Scala")
      writer.write("\n")
      writer.write("Hello Scala")
      writer.close()*/
					return TDB

	}  
	//Items id start at 1
	def fromNormalFilesToNormalArray(fileName: String): (Array[Set[Int]],Int) = {
		//////////////////////////////////////////noooooooooo
		val fileLines = Source
				.fromFile(fileName).getLines

				val myArray = fileLines.map { line => line.mkString.split("\\s+").map(_.toInt) }.toArray
				var max =0
				val list: List[Array[Int]] = List()
				for(e <- myArray){
					//println()
					for(j <- 0 until e.length){      
						//print(e(j)+ " ")
						if(e(j)>max)
							max = e(j)
					}
				}
				//println(max)
				var TDB = ofDim[Int](myArray.length,max)
						var pointer = 0
						var i = 0
						for(e <- myArray){
							pointer = 0
									for(x <- 0 until e.length){ 
										for(j <- pointer to e(x)-1){
											if(j == e(x)-1){
												TDB(i)(j) = 1 
											}else{
												TDB(i)(j) = 0 
											}
										}
										pointer = e(x)
									}
							for(j <- pointer until max){
								TDB(i)(j) = 0
							}
							i +=1
						}
				//printTDB(TDB)
				var TDBNormal = new Array[Set[Int]](TDB(0).length)
						for(col <- 0 until TDB(0).length){    //TODO if empty set
							TDBNormal(col) = Set()
									for(t <- 0 until TDB.length){
										if( TDB(t)(col) == 1)
											TDBNormal(col) += t+1
									}
						}

				//printTDBNormal(TDBNormal)
				return (TDBNormal,TDB.length)


	}
	//Items id start at 0 with a class label at the end of each lines
	def fromNormalFilesToNormalArray2(fileName: String): (Array[Set[Int]],Int) = {
		//YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY
		val fileLines = Source
				.fromFile(fileName).getLines
				// fileLines.foreach { x => println(x.mkString) }
				val myArray = fileLines.map { line => line.mkString.split("\\s+").map(_.toInt) }.toArray

				var max =0
				val list: List[Array[Int]] = List()
				var trans = 1
				for(e <- myArray){
					//println()
					for(j <- 0 until e.length){      
						// print(e(j)+ " ")
						if(e(j)>max)
							max = e(j)
							//println("new max = "+e(j)  )
					}
					// print(" " + trans)         
					trans += 1
				}
				max +=1
						//println("max = " + max)
						var TDB = ofDim[Int](myArray.length,max)
						//print(TDB(0).length)
						var pointer = 0
						var i = 0
						for(e <- myArray){
							//println()
							pointer = 0
									for(x <- 0 until e.length-1){ 
										for(j <- pointer to e(x)){
											if(j == e(x)){
												TDB(i)(j) = 1 
														//print(1)
											}else{
												TDB(i)(j) = 0
														//print(0)
											}
										}
										pointer = e(x)+1
									}
							for(j <- pointer until max){
								TDB(i)(j) = 0
										//print(0)
							}
							i +=1
						}
				//printTDB(TDB)
				var Empty = true
						var count = 0
						//println("lol")
						for(col <- 0 until TDB(0).length){    //TODO if empty set
							Empty = true
									for(t <- 0 until TDB.length ; if Empty){
										if( TDB(t)(col) == 1){
											Empty = false
													count += 1
													// println("ici")
										}
									}
						}
				//println("nonEmpty "+ count)
				var TDBNormal = new Array[Set[Int]](TDB(0).length)
						for(col <- 0 until TDB(0).length){    //TODO if empty set
							TDBNormal(col) = Set()
									for(t <- 0 until TDB.length){
										if( TDB(t)(col) == 1)
											TDBNormal(col) += t+1
									}
						}
				//printTDBNormal(TDBNormal)
				return (TDBNormal,TDB.length)
	}


	//  def fromNormalFilesToNormalArray(fileName: String): (Array[Set[Int]],Int) = {
	//
	//    val fileLines = Source
	//        .fromFile(fileName).getLines
	//
	//        val myArray = fileLines.map { line => line.mkString.split("\\s+").map(_.toInt) }.toArray
	//        var max =0
	//        val list: List[Array[Int]] = List()
	//        for(e <- myArray){
	//          println()
	//          for(j <- 0 until e.length){      
	//            //print(e(j)+ " ")
	//            if(e(j)>max)
	//              max = e(j)
	//          }
	//        }
	//        println(max)
	//        var TDB = ofDim[Int](myArray.length,max)
	//        var pointer = 0
	//        var i = 0
	//        for(e <- myArray){
	//          pointer = 0
	//              for(x <- 0 until e.length){ 
	//                for(j <- pointer to e(x)-1){
	//                  if(j == e(x)-1){
	//                    TDB(i)(j) = 1 
	//                  }else{
	//                    TDB(i)(j) = 0 
	//                  }
	//                }
	//                pointer = e(x)
	//              }
	//          for(j <- pointer until max){
	//            TDB(i)(j) = 0
	//          }
	//          i +=1
	//        }
	//        //printTDB(TDB)
	//        var TDBNormal = new Array[Set[Int]](TDB(0).length)
	//        for(col <- 0 until TDB(0).length){    //TODO if empty set
	//          TDBNormal(col) = Set()
	//              for(t <- 0 until TDB.length){
	//                if( TDB(t)(col) == 1)
	//                  TDBNormal(col) += t+1
	//              }
	//        }
	//
	//        //printTDBNormal(TDBNormal)
	//        return (TDBNormal,TDB.length)
	//
	//
	//  }


	def printTDB(TDB :Array[Array[Int]]) {
		for(i <- 0 until TDB.length)
		{println()
			for(j <- 0 until TDB(i).length){
				print(TDB(i)(j))
			}
		}
		println()
		println(TDB.length)
		println(TDB(0).length)
	}


	def printTDBNormal(TDB :Array[Set[Int]]) {
		for(i <- 0 until TDB.length)
		{
			println((TDB(i)))
		}

	}
  
  def gspmArray(fileName: String): (Array[Array[Int]],Set[Int],Int) = {
    //////////////////////////////////////////noooooooooo
    val fileLines = Source
        .fromFile(fileName).getLines

        val myArray = fileLines.map { line => line.mkString.split("\\s+").map(_.toInt) }.toArray
        var max =0
        val list: List[Array[Int]] = List()
        var all : Set[Int] = Set()
        for(e <- myArray){
          //println()
          for(j <- 0 until e.length){      
            //print(e(j)+ " ")
            all += e(j)
            if(e.length>max)
              max = e.length
          }
        }
        return (myArray,all,max)
        /*//println(max)
        var TDB = ofDim[Int](myArray.length,max)
            var pointer = 0
            var i = 0
            for(e <- myArray){
              pointer = 0
                  for(x <- 0 until e.length){ 
                    for(j <- pointer to e(x)-1){
                      if(j == e(x)-1){
                        TDB(i)(j) = 1 
                      }else{
                        TDB(i)(j) = 0 
                      }
                    }
                    pointer = e(x)
                  }
              for(j <- pointer until max){
                TDB(i)(j) = 0
              }
              i +=1
            }
        //printTDB(TDB)
        var TDBNormal = new Array[Set[Int]](TDB(0).length)
            for(col <- 0 until TDB(0).length){    //TODO if empty set
              TDBNormal(col) = Set()
                  for(t <- 0 until TDB.length){
                    if( TDB(t)(col) == 1)
                      TDBNormal(col) += t+1
                  }
            }

        //printTDBNormal(TDBNormal)
        return (TDBNormal,TDB.length)*/


  }

}
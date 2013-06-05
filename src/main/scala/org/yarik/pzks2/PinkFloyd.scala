package org.yarik.pzks2

import SchedUtils._

object PinkFloyd extends App {

  val sortAlgs = Sorter.algs
  val schedAlgs = Modeller.algsWithNames

  val systemGenerator = new Generator(false)
  val tasksGenerator = new Generator(true)

  implicit val maxIo = 3
  implicit val duplex = true
  
  val dir = "/tmp/graphs"
  
  val testData = for {
      i <- (1 to 10)
      task = tasksGenerator.generateGraph(100, 3, 20, 0.5)
      system = systemGenerator.generateGraph(15, 5, 20, 0.5)
  } yield {
	UiHelper.buildImage(task, true, s"$dir/t_$i.png") 
	UiHelper.buildImage(system, false, s"$dir/s_$i.png") 
    (task, system)
  }

  println("start calculating")
  val results = for {
    (schedAlg, schName) <- schedAlgs
    sorterAlg <- sortAlgs
  } yield {
    val allTimes = for {
      (task, system) <- testData
      sorter = new Sorter(task, sorterAlg)
      env = transformAndSchedule(system, task, sorter, schedAlg)
      _ = println("calced")
    } yield env.cpuMax
    
    val average = allTimes.sum.toDouble / allTimes.size

    s"sorter $sorterAlg, scheduller $schName give $average"
  }
  
  results.seq.foreach(println)

}
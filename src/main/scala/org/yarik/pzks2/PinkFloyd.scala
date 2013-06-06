package org.yarik.pzks2

import SchedUtils._

object PinkFloyd extends App {

  val startTime = System.currentTimeMillis()
  val sortAlgs = Sorter.algs.par
  val schedAlgs = Modeller.algsWithNames.par

  val systemGenerator = new Generator(false)
  val tasksGenerator = new Generator(true)

  implicit val maxIo = 3
  implicit val duplex = true

  val imagesDir = "/tmp/graphs"

  println("start calculating")

  for {
    connectivityKoef <- (1 to 9).par
    connectivity = connectivityKoef.toDouble/10
    size <- (10 to 50 by 10).par
  } yield {

    val testData = for {
      i <- (1 to 10).par
      task = tasksGenerator.generateGraph(size, 3, 20, connectivity)
      system = systemGenerator.generateGraph(size/4, 5, 20, connectivity/4)
    } yield {
      //	UiHelper.buildImage(task, true, s"$imagesDir/t_$i.png") 
      //	UiHelper.buildImage(system, false, s"$imagesDir/s_$i.png") 
      (task, system)
    }

    val results = for {
      (schedAlg, schName) <- schedAlgs
      sorterAlg <- sortAlgs
    } yield {
      val allTimes = for {
        (task, system) <- testData
        sorter = new Sorter(task, sorterAlg)
        env = transformAndSchedule(system, task, sorter, schedAlg)
      } yield env.cpuMax

      val average = allTimes.sum.toDouble / allTimes.size

      s"conn $connectivity, size $size: sorter $sorterAlg, scheduller $schName give $average"
    }

    val spendedTime = System.currentTimeMillis() - startTime

    results.seq.foreach(println)

  }

  val spendedTime = System.currentTimeMillis - startTime
  
  println(s"spended time $spendedTime")

}
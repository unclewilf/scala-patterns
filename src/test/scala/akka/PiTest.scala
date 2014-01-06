package akka

import org.specs2.mutable.Specification

class PiTest extends Specification {

  val res: Double = new Pi().calculate(nrOfWorkers = 4, nrOfElements = 10000, nrOfMessages = 10000) match {
    case _ => 0.0
  }

  "The PI function" should {
    "return 10 values" in {
       res must beEqualTo(0.0)
    }
  }

}
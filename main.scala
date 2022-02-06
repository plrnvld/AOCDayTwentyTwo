import scala.io.Source
import java.util.regex.Pattern

object Main {
    def main(args: Array[String]): Unit = {
        val lines = Source.fromFile("Input.txt").getLines.toList
        println(s"Lines count: ${lines.size}")

        val minLimit = -50
        val maxLimit = 50
        val rangeToCheck = minLimit to maxLimit

        val bootRules = lines.map(parse)

        for (rule <- bootRules)
            println(rule)

        var onCount = 0;

        for (x <- rangeToCheck) {
            for (y <- rangeToCheck) {
                for (z <- rangeToCheck) {
                    var isOn = false;

                    for (rule <- bootRules) {
                        if (isOn != rule.switchOn && rule.isInside(x, y, z)) {
                            isOn = rule.switchOn
                        }
                    }

                    if (isOn) {
                        onCount += 1
                    }
                }
            }
        }

        println(s"On count = $onCount")
    }

    def parse(line: String): BootRule = {
        val switchOn = line.startsWith("on")
        val coordLimits = line.split(" ")(1).split(",").map(parseCoordPart)
        BootRule(switchOn, 
            coordLimits(0)._1, coordLimits(0)._2, coordLimits(1)._1, 
            coordLimits(1)._2, coordLimits(2)._1, coordLimits(2)._2)
    }

    def parseCoordPart(coordPart: String): (Int, Int) = {
        val coords = coordPart.substring(2).split(Pattern.quote("..")).map(_.toInt)
        (coords(0), coords(1))
    }



    
}

case class BootRule(val switchOn: Boolean, val minX: Int, val maxX: Int, val minY: Int, val maxY: Int, val minZ: Int, val maxZ: Int) {
    def isInside(x: Int, y: Int, z: Int): Boolean = x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ
    
}

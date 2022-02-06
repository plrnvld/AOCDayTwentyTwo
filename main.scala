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

        var minX = bootRules.head.minX
        var maxX = bootRules.head.maxX
        var minY = bootRules.head.minY
        var maxY = bootRules.head.maxY
        var minZ = bootRules.head.minZ
        var maxZ = bootRules.head.maxZ

        for (rule <- bootRules.drop(1)) {
            minX = minX.min(rule.minX)
            maxX = maxX.max(rule.maxX)
            minY = minY.min(rule.minY)
            maxY = maxY.max(rule.maxY)
            minZ = minZ.min(rule.minZ)
            maxZ = maxZ.max(rule.maxZ)
        }
            
        println(s"$minX <= x <= $maxX, $minY <= y <= $maxY, $minZ <= z <= $maxZ")

        var onCount = 0;

        for (x <- minX to maxX) {
            println()
            println(s"Calculate x = $x")
            val matchingBootRules = bootRules.filter(b => x >= b.minX && x <= b.maxX)

            for (y <- minY to maxY) {
                val twiceMatchingBootRules = matchingBootRules.filter(b => y >= b.minY && y <= b.maxY)
                
                if (y % 1000 == 0)
                    print(s"$y,")

                for (z <- minZ to maxZ) {
                    var isOn = false;

                    for (rule <- twiceMatchingBootRules) {
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

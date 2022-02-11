import scala.io.Source
import java.util.regex.Pattern
import scala.annotation.switch

object Main {
    def main(args: Array[String]): Unit = {
        val lines = Source.fromFile("Input.txt").getLines.toList
        println(s"Lines count: ${lines.size}")

        val bootRules = lines.map(parse)

        val (minX, maxX, minY, maxY, minZ, maxZ) = ranges(bootRules)

        println(s"$minX <= x <= $maxX, $minY <= y <= $maxY, $minZ <= z <= $maxZ")

        var onCount = 0;

        var activeRules = bootRules

        for (x <- minX to maxX) {
            if (x % 1000 == 0)
                println(s"Calculate slice for x = $x")
                
            activeRules = bootRules.filter(b => x >= b.minX && x <= b.maxX)          

            val activeSlices = activeRules.map(_.sliceYZ())
            onCount += slicesOnCount(x, activeSlices)
        }

        println(s"On count = $onCount")
    }

    def slicesOnCount(sliceNum: Int, activeSlices: Seq[BootRuleSlice]): Int = {
        val switchOn = ruleOutcome(1, 2, activeSlices)   
        
        if (activeSlices.size == 1) {
            if (switchOn) {
                throw new Exception("Does this even happen: " + sliceNum)
                1
            }
            else {
                0
            }
        } else {
            println(s"  x = $sliceNum has ${activeSlices.size} slices")

            if (switchOn) {
                activeSlices.size
            }
            else {
               
                0
            }
        }
    }

    def ruleOutcome(x: Int, y: Int, z: Int, rules: Seq[BootRule]): Boolean = {
        rules.filter(_.contains(x, y, z)).map(_.switchOn).lastOption.getOrElse(false)
    }

    def ruleOutcome(x: Int, y: Int, rules: Seq[BootRuleSlice]): Boolean = {
        rules.filter(_.contains(x, y)).map(_.switchOn).lastOption.getOrElse(false)
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

    def ranges(rules: Seq[BootRule]): (Int, Int, Int, Int, Int, Int) = {
        var minX = rules.head.minX
        var maxX = rules.head.maxX
        var minY = rules.head.minY
        var maxY = rules.head.maxY
        var minZ = rules.head.minZ
        var maxZ = rules.head.maxZ

        for (rule <- rules.drop(1)) {
            minX = minX.min(rule.minX)
            maxX = maxX.max(rule.maxX)
            minY = minY.min(rule.minY)
            maxY = maxY.max(rule.maxY)
            minZ = minZ.min(rule.minZ)
            maxZ = maxZ.max(rule.maxZ)
        }

        (minX, maxX, minY, maxY, minZ, maxZ)
    }     
}

case class BootRule(val switchOn: Boolean, val minX: Int, val maxX: Int, val minY: Int, val maxY: Int, val minZ: Int, val maxZ: Int) {
    def contains(x: Int, y: Int, z: Int): Boolean = x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ

    def sliceYZ(): BootRuleSlice = BootRuleSlice(switchOn, Rect(minY, maxY, minZ, maxZ))
}

case class BootRuleSlice(val switchOn: Boolean, val rect: Rect) {
     def contains(x: Int, y: Int): Boolean = x >= rect.minX && x <= rect.maxX && y >= rect.minY && y <= rect.maxY    
}

case class Rect(val minX: Int, val maxX: Int, val minY: Int, val maxY: Int) {
    def surfaceArea: Int = (maxX - minX + 1) * (maxY - minY + 1)
    def overlapsWith(other: Rect): Boolean = {
        maxX >= other.minX && minX <= other.maxX && maxY >= other.minY && minY <= other.maxY
    }
}

import scala.io.Source
import java.util.regex.Pattern

object Main {
    def main(args: Array[String]): Unit = {
        val lines = Source.fromFile("Input.txt").getLines.toList
        val bootRules = lines.map(parse)

        val (minX, maxX, minY, maxY, minZ, maxZ) = ranges(bootRules)

        println(s"$minX <= x <= $maxX, $minY <= y <= $maxY, $minZ <= z <= $maxZ")

        var onCount = 0L;
        var prevResult = (Seq[BootRuleSlice](), 0L)

        for (x <- minX to maxX) {
            val activeSlices = bootRules.filter(b => b.minX <= x && b.maxX >= x).map(_.sliceYZ())

            val score = slicesScore(x, activeSlices, prevResult)
            prevResult = (activeSlices, score)

            onCount += score
        }

        println(s"On count = $onCount")
    }

    def slicesScore(sliceNum: Int, activeSlices: Seq[BootRuleSlice], prevResult: (Seq[BootRuleSlice], Long) ): Long = {
        if (activeSlices equals prevResult._1) {
            prevResult._2
        } else {
            val border = slicesBorder(activeSlices)

            var score = 0L
            println(s"Calculate slice for x = $sliceNum")
            println("[                                         ]")
            print(" ")

            val widthPart = border.width / 40
            var progress = 0
            var prevResult = (Seq[BootRuleColumn](), 0L)

            for (x <- border.minX to border.maxX) {
                if (progress % widthPart == 0)
                    print("#")
                
                val activeColumns = activeSlices
                    .filter(a => a.rect.minX <= x && a.rect.maxX >= x)
                    .map(_.toColumn())

                var columnsScore = 0L

                if (!activeColumns.isEmpty) {
                    columnsScore = columnScore(x, activeColumns, prevResult)
                    score += columnsScore
                }

                prevResult = (activeColumns, columnsScore)              
                progress += 1
            }

            println()
            println(s"  Score = $score")
            score
        }
    }

    def columnScore(x: Int, activeColumns: Seq[BootRuleColumn], prevResult: (Seq[BootRuleColumn], Long)): Long = {
        if (activeColumns equals prevResult._1) {
            prevResult._2
        } else {
            val (minY, maxY) = columnsBorder(activeColumns)
            toSwitchRanges(minY, maxY, activeColumns).map(_.switchOnCount).sum
        }
    }

    def toSwitchRanges(min: Int, max: Int, activeColumns: Seq[BootRuleColumn]): Seq[SwitchRange] = {
        val allSwitches = (activeColumns
            .flatMap(c => List[Int](c.minY, c.maxY + 1))
            .filter(y => y >= min && y <= max) ++ List(min, max + 1))
            .distinct
            .sorted

        val pairs = allSwitches zip allSwitches.tail

        pairs.map(t => SwitchRange(ruleOutcome(t._1, activeColumns), t._1, t._2 - 1))
    }

    def ruleOutcome(y: Int, columns: Seq[BootRuleColumn]): Boolean = {
        columns.filter(_.contains(y)).map(_.switchOn).lastOption.getOrElse(false)
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

    def slicesBorder(slices: Seq[BootRuleSlice]): Rect = {
        var minX = slices.head.rect.minX
        var maxX = slices.head.rect.maxX
        var minY = slices.head.rect.minY
        var maxY = slices.head.rect.maxY
        
        for (slice <- slices.drop(1)) {
            minX = minX.min(slice.rect.minX)
            maxX = maxX.max(slice.rect.maxX)
            minY = minY.min(slice.rect.minY)
            maxY = maxY.max(slice.rect.maxY)
        }

        Rect(minX, maxX, minY, maxY)
    }

    def columnsBorder(columns: Seq[BootRuleColumn]): (Int, Int) = {
        columns
            .map(c => (c.minY, c.maxY))
            .reduce((t1, t2) => (t1._1 min t2._1, t1._2 max t2._2))
    }
}

case class BootRule(val switchOn: Boolean, val minX: Int, val maxX: Int, val minY: Int, val maxY: Int, val minZ: Int, val maxZ: Int) {
    def contains(x: Int, y: Int, z: Int): Boolean = x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ
    def sliceYZ(): BootRuleSlice = BootRuleSlice(switchOn, Rect(minY, maxY, minZ, maxZ))
}

case class BootRuleSlice(val switchOn: Boolean, val rect: Rect) {
     def contains(x: Int, y: Int): Boolean = x >= rect.minX && x <= rect.maxX && y >= rect.minY && y <= rect.maxY
     def toColumn(): BootRuleColumn = BootRuleColumn(switchOn, rect.minY, rect.maxY)
}

case class BootRuleColumn(val switchOn: Boolean, val minY: Int, val maxY: Int) {
    def contains(y: Int): Boolean = y >= minY && y <= maxY
}

case class Rect(val minX: Int, val maxX: Int, val minY: Int, val maxY: Int) {
    def width = maxX - minX + 1
}

case class SwitchRange(val switchOn: Boolean, val min: Int, val max: Int) {
    def switchOnCount = if (switchOn) max - min + 1 else 0
}
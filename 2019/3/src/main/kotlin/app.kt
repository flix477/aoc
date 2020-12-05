import java.io.File
import java.lang.Exception
import kotlin.math.abs

enum class Direction {
    UP, DOWN, LEFT, RIGHT
}

data class Movement(val direction: Direction, val amount: Int)
data class Position(val x: Int = 0, val y: Int = 0)
data class Intersection(val position: Position, val steps: Pair<Int, Int>)
data class WireInfo(val line: Line, val steps: Int)

typealias Line = Pair<Position, Position>

fun main() {
    val inputs = File("input.txt").readLines()
    val distance = solve2(inputs)
    print(distance)
}

fun solve1(inputs: List<String>): Int =
    solveIntersections(inputs)
        .map { it.position }
        .map { calcDistance(Position(), it) }
        .min()!!

fun solve2(inputs: List<String>): Int =
    solveIntersections(inputs)
        .map { it.steps.first + it.steps.second }
        .min()!!

fun solveIntersections(inputs: List<String>): List<Intersection> {
    val wires = inputs.map(::parseInput)
    return getIntersections(wires[0], wires[1])
}

fun parseInput(input: String): List<Movement> =
    input.split(",").map(::parseMovement)

fun calcDistance(p1: Position, p2: Position): Int =
    abs(p2.x - p1.x) + abs(p2.y - p1.y)

fun parseMovement(movement: String): Movement {
    val direction = parseDirection(movement[0])
    val amount = movement.slice(1 until movement.length).toInt()
    return Movement(direction, amount)
}

fun parseDirection(a: Char): Direction =
    when (a) {
        'U' -> Direction.UP
        'D' -> Direction.DOWN
        'L' -> Direction.LEFT
        'R' -> Direction.RIGHT
        else -> throw Exception("Invalid direction character: $a")
    }

// yikes
fun getIntersections(wire1: List<Movement>, wire2: List<Movement>): List<Intersection> {
    val intersections = mutableListOf<Intersection>()
    var wire1Position = Position()
    var wire1Steps = 0
    for (m in wire1) {
        val newWire1Position = movedPosition(wire1Position, m)
        val wire1Line = Line(wire1Position, newWire1Position)
        var wire2Position = Position()
        wire1Steps += m.amount
        var wire2Steps = 0

        for (n in wire2) {
            val newWire2Position = movedPosition(wire2Position, n)
            val wire2Line = Line(wire2Position, newWire2Position)
            wire2Steps += n.amount
            val intersection = getIntersection(
                WireInfo(wire1Line, wire1Steps),
                WireInfo(wire2Line, wire2Steps)
            )
            if (intersection != null) intersections.add(intersection)
            wire2Position = newWire2Position
        }

        wire1Position = newWire1Position
    }

    return intersections
}

fun movedPosition(acc: Position, movement: Movement): Position =
    when (movement.direction) {
        Direction.UP -> Position(acc.x, acc.y + movement.amount)
        Direction.DOWN -> Position(acc.x, acc.y - movement.amount)
        Direction.LEFT -> Position(acc.x - movement.amount, acc.y)
        Direction.RIGHT -> Position(acc.x + movement.amount, acc.y)
    }

fun getIntersection(wire1: WireInfo, wire2: WireInfo): Intersection? {
    val position = (if (!isHorizontal(wire1.line) && isHorizontal(wire2.line))
        getIntersectionPosition(wire2.line, wire1.line)
    else if (!isHorizontal(wire2.line) && isHorizontal(wire1.line))
        getIntersectionPosition(wire1.line, wire2.line)
    else null)
        ?: return null

    val wire1Diff = stepsDifference(position, wire1.line.second)
    val wire2Diff = stepsDifference(position, wire2.line.second)
    return Intersection(
        position,
        Pair(wire1.steps - wire1Diff, wire2.steps - wire2Diff)
    )
}

fun getIntersectionPosition(horLine: Line, verLine: Line): Position? {
    val y = horLine.first.y
    val x = verLine.first.x

    if (y in orderedRange(verLine.first.y, verLine.second.y)
        && x in orderedRange(horLine.first.x, horLine.second.x))
        return Position(x, y)
    return null
}

fun isHorizontal(line: Line): Boolean =
    line.first.y == line.second.y

fun orderedRange(a: Int, b: Int): IntRange =
    if (a > b) b + 1 until a
    else a + 1 until b

fun stepsDifference(intersection: Position, wirePosition: Position): Int {
    val difference = if (intersection.y == wirePosition.y)
        wirePosition.x - intersection.x
    else wirePosition.y - intersection.y

    return abs(difference)
}
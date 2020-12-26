import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class AppKtTest {
    @Test
    fun solve1Inputs() {
        val inputs = listOf(
            "R8,U5,L5,D3",
            "U7,R6,D4,L4"
        )
        val result = solve1(inputs)
        assertEquals(6, result)
    }

    @Test
    fun solve1Inputs2() {
        val inputs = listOf(
            "R75,D30,R83,U83,L12,D49,R71,U7,L72",
            "U62,R66,U55,R34,D71,R55,D58,R83"
        )
        val result = solve1(inputs)
        assertEquals(159, result)
    }

    @Test
    fun solve1Inputs3() {
        val inputs = listOf(
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
            "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
        )
        val result = solve1(inputs)
        assertEquals(135, result)
    }

    @Test
    fun solve2Inputs() {
        val inputs = listOf(
            "R8,U5,L5,D3",
            "U7,R6,D4,L4"
        )
        val result = solve2(inputs)
        assertEquals(30, result)
    }

    @Test
    fun solve2Inputs2() {
        val inputs = listOf(
            "R75,D30,R83,U83,L12,D49,R71,U7,L72",
            "U62,R66,U55,R34,D71,R55,D58,R83"
        )
        val result = solve2(inputs)
        assertEquals(610, result)
    }

    @Test
    fun solve2Inputs3() {
        val inputs = listOf(
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
            "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
        )
        val result = solve2(inputs)
        assertEquals(410, result)
    }
}
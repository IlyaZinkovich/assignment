import Solution._
import org.scalatest.FlatSpec

class SolutionSpec extends FlatSpec {

	"A Solution" should "correctly identify available positions" in {
		val width = 10
		val height = 10

		assert(rightPosition((0, 0), width) == Some(0, 3))
		assert(leftPosition((0, 3)) == Some(0, 0))
		assert(upPosition((3, 0)) == Some(0, 0))
		assert(downPosition((0, 0), height) == Some(3, 0))
		assert(upRightPosition((2, 0), width) == Some(0, 2))
		assert(upLeftPosition((2, 2)) == Some(0, 0))
		assert(downRightPosition((0, 0), width, height) == Some(2, 2))
		assert(downLeftPosition((0, 2), height) == Some(2, 0))
		assert(rightPosition((0, 9), width) isEmpty)
		assert(leftPosition((0, 0)) isEmpty)
		assert(upPosition((0, 0)) isEmpty)
		assert(downPosition((9, 0), height) isEmpty)
		assert(upRightPosition((0, 0), width) isEmpty)
		assert(upLeftPosition((0, 0)) isEmpty)
		assert(downRightPosition((7, 9), width, height) isEmpty)
		assert(downLeftPosition((7, 1), height) isEmpty)
	}

	"A Solution" should "visit layer" in {
		val width = 10
		val height = 10
		val matrix: Array[Array[Boolean]] = Array.ofDim[Boolean](height, width)

		val updatedMatrix = visitLayer(matrix, width, height)

		updatedMatrix(0).foreach(cell => assert(cell))
		updatedMatrix(2).foreach(cell => assert(cell))
	}
}

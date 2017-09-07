object Solution {


	def main(args: Array[String]): Unit = {
		val width = 10
		val height = 4
		val matrix: Array[Array[Boolean]] = Array.ofDim[Boolean](height, width)

		visitTwoLayers(matrix, width, height)
	}

	def visitTwoLayers(matrix: Array[Array[Boolean]], width: Int, height: Int): Array[Array[Boolean]] = {
		var position = (0, 0)

		matrix(0)(0) = true

		printMatrix(matrix)

		while (rightPosition(position, width).nonEmpty) {
			rightPosition(position, width) match {
				case Some(nextPosition) => matrix(nextPosition._1)(nextPosition._2) = true
					printMatrix(matrix)
					position = nextPosition
				case None =>
			}
		}

		position = downLeftPosition(position, height).get

		matrix(position._1)(position._2) = true

		printMatrix(matrix)

		while (leftPosition(position).nonEmpty) {
			leftPosition(position) match {
				case Some(nextPosition) => matrix(nextPosition._1)(nextPosition._2) = true
					printMatrix(matrix)
					position = nextPosition
				case None =>
			}
		}

		position = upRightPosition(position, width).get

		position = rightPosition(position, width).get

		position = downRightPosition(position, width, height).get

		matrix(position._1)(position._2) = true

		printMatrix(matrix)

		while (leftPosition(position).nonEmpty) {
			leftPosition(position) match {
				case Some(nextPosition) => matrix(nextPosition._1)(nextPosition._2) = true
					printMatrix(matrix)
					position = nextPosition
				case None =>
			}
		}

		position = upRightPosition(position, width).get

		matrix(position._1)(position._2) = true

		printMatrix(matrix)

		position = rightPosition(position, width).get

		matrix(position._1)(position._2) = true

		printMatrix(matrix)

		position = downRightPosition(position, width, height).get

		matrix(position._1)(position._2) = true

		printMatrix(matrix)

		while (leftPosition(position).nonEmpty) {
			leftPosition(position) match {
				case Some(nextPosition) => matrix(nextPosition._1)(nextPosition._2) = true
					printMatrix(matrix)
					position = nextPosition
				case None =>
			}
		}

		position = upRightPosition(position, width).get

		matrix(position._1)(position._2) = true

		printMatrix(matrix)

		while (rightPosition(position, width).nonEmpty) {
			rightPosition(position, width) match {
				case Some(nextPosition) => matrix(nextPosition._1)(nextPosition._2) = true
					printMatrix(matrix)
					position = nextPosition
				case None =>
			}
		}

		position = downLeftPosition(position, height).get

		matrix(position._1)(position._2) = true

		printMatrix(matrix)

		position = leftPosition(position).get

		matrix(position._1)(position._2) = true

		printMatrix(matrix)

		position = upLeftPosition(position).get

		matrix(position._1)(position._2) = true

		printMatrix(matrix)

		matrix
	}

	private def printMatrix(matrix: Array[Array[Boolean]]) = {
		println(matrix.map(_.map(b => if (b) "x" else "o").mkString("|")).mkString("\n"))
		println
	}

	def upRightPosition(position: (Int, Int), width: Int): Option[(Int, Int)] = {
		if (position._1 - 2 >= 0 && position._2 + 2 < width)
			Option((position._1 - 2, position._2 + 2))
		else
			None
	}

	def downRightPosition(position: (Int, Int), width: Int, height: Int): Option[(Int, Int)] = {
		if (position._1 + 2 < height && position._2 + 2 < width)
			Option((position._1 + 2, position._2 + 2))
		else
			None
	}

	def upLeftPosition(position: (Int, Int)): Option[(Int, Int)] = {
		if (position._1 - 2 >= 0 && position._2 - 2 >= 0)
			Option((position._1 - 2, position._2 - 2))
		else
			None
	}

	def downLeftPosition(position: (Int, Int), height: Int): Option[(Int, Int)] = {
		if (position._1 + 2 < height && position._2 - 2 >= 0)
			Option((position._1 + 2, position._2 - 2))
		else
			None
	}

	def upPosition(position: (Int, Int)): Option[(Int, Int)] = {
		if (position._1 - 3 >= 0)
			Option((position._1 - 3, position._2))
		else
			None
	}

	def downPosition(position: (Int, Int), height: Int): Option[(Int, Int)] = {
		if (position._1 + 3 < height)
			Option((position._1 + 3, position._2))
		else
			None
	}

	def leftPosition(position: (Int, Int)): Option[(Int, Int)] = {
		if (position._2 - 3 >= 0)
			Option((position._1, position._2 - 3))
		else
			None
	}

	def rightPosition(position: (Int, Int), width: Int): Option[(Int, Int)] = {
		if (position._2 + 3 < width)
			Option((position._1, position._2 + 3))
		else
			None
	}
}

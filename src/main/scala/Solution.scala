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

		var movedToLowerLayer = false
		while (!movedToLowerLayer) {
			val moveDownRight = downRightPosition(position, width, height)
			val moveDownLeft = downLeftPosition(position, height)
			if (moveDownRight.nonEmpty && !matrix(moveDownRight.get._1)(moveDownRight.get._2)) {
				position = moveDownRight.get
				movedToLowerLayer = true
				matrix(position._1)(position._2) = true
			} else if (moveDownLeft.nonEmpty && !matrix(moveDownLeft.get._1)(moveDownLeft.get._2)) {
				position = moveDownLeft.get
				movedToLowerLayer = true
				matrix(position._1)(position._2) = true
			} else {
				val moveLeft = leftPosition(position)
				if (moveLeft.nonEmpty) {
					position = moveLeft.get
					matrix(position._1)(position._2) = true
				} else {
					position = moveDownRight.get
					movedToLowerLayer = true
					matrix(position._1)(position._2) = true
				}
			}
		}

		printMatrix(matrix)

		while (leftPosition(position).nonEmpty) {
			leftPosition(position) match {
				case Some(nextPosition) => matrix(nextPosition._1)(nextPosition._2) = true
					printMatrix(matrix)
					position = nextPosition
				case None =>
			}
		}

		position = moveToUpperLayer(position, width, height, matrix)

		printMatrix(matrix)

		while (rightPosition(position, width).nonEmpty) {
			rightPosition(position, width) match {
				case Some(nextPosition) => matrix(nextPosition._1)(nextPosition._2) = true
					printMatrix(matrix)
					position = nextPosition
				case None =>
			}
		}

		movedToLowerLayer = false
		while (!movedToLowerLayer) {
			val moveDownRight = downRightPosition(position, width, height)
			val moveDownLeft = downLeftPosition(position, height)
			if (moveDownRight.nonEmpty && !matrix(moveDownRight.get._1)(moveDownRight.get._2)) {
				position = moveDownRight.get
				movedToLowerLayer = true
				matrix(position._1)(position._2) = true
			} else if (moveDownLeft.nonEmpty && !matrix(moveDownLeft.get._1)(moveDownLeft.get._2)) {
				position = moveDownLeft.get
				movedToLowerLayer = true
				matrix(position._1)(position._2) = true
			} else {
				val moveLeft = leftPosition(position)
				if (moveLeft.nonEmpty) {
					position = moveLeft.get
					matrix(position._1)(position._2) = true
				} else {
					position = moveDownRight.get
					movedToLowerLayer = true
					matrix(position._1)(position._2) = true
				}
			}
		}

		printMatrix(matrix)

		while (leftPosition(position).nonEmpty) {
			leftPosition(position) match {
				case Some(nextPosition) => matrix(nextPosition._1)(nextPosition._2) = true
					printMatrix(matrix)
					position = nextPosition
				case None =>
			}
		}

		position = moveToUpperLayer(position, width, height, matrix)

		printMatrix(matrix)

		while (rightPosition(position, width).nonEmpty) {
			rightPosition(position, width) match {
				case Some(nextPosition) => matrix(nextPosition._1)(nextPosition._2) = true
					printMatrix(matrix)
					position = nextPosition
				case None =>
			}
		}

		movedToLowerLayer = false
		while (!movedToLowerLayer) {
			val moveDownRight = downRightPosition(position, width, height)
			val moveDownLeft = downLeftPosition(position, height)
			if (moveDownRight.nonEmpty && !matrix(moveDownRight.get._1)(moveDownRight.get._2)) {
				position = moveDownRight.get
				movedToLowerLayer = true
				matrix(position._1)(position._2) = true
			} else if (moveDownLeft.nonEmpty && !matrix(moveDownLeft.get._1)(moveDownLeft.get._2)) {
				position = moveDownLeft.get
				movedToLowerLayer = true
				matrix(position._1)(position._2) = true
			} else {
				val moveLeft = leftPosition(position)
				if (moveLeft.nonEmpty) {
					position = moveLeft.get
					matrix(position._1)(position._2) = true
				} else {
					position = moveDownRight.get
					movedToLowerLayer = true
					matrix(position._1)(position._2) = true
				}
			}
		}

		printMatrix(matrix)

		while (leftPosition(position).nonEmpty) {
			leftPosition(position) match {
				case Some(nextPosition) => matrix(nextPosition._1)(nextPosition._2) = true
					printMatrix(matrix)
					position = nextPosition
				case None =>
			}
		}

		position = moveToUpperLayer(position, width, height, matrix)

		printMatrix(matrix)

		movedToLowerLayer = false
		while (!movedToLowerLayer) {
			val moveDownRight = downRightPosition(position, width, height)
			val moveDownLeft = downLeftPosition(position, height)
			if (moveDownRight.nonEmpty && !matrix(moveDownRight.get._1)(moveDownRight.get._2)) {
				position = moveDownRight.get
				movedToLowerLayer = true
				matrix(position._1)(position._2) = true
			} else if (moveDownLeft.nonEmpty && !matrix(moveDownLeft.get._1)(moveDownLeft.get._2)) {
				position = moveDownLeft.get
				movedToLowerLayer = true
				matrix(position._1)(position._2) = true
			} else {
				val moveLeft = leftPosition(position)
				if (moveLeft.nonEmpty) {
					position = moveLeft.get
					matrix(position._1)(position._2) = true
				} else {
					position = moveDownRight.get
					movedToLowerLayer = true
					matrix(position._1)(position._2) = true
				}
			}
		}

		printMatrix(matrix)

		while (leftPosition(position).nonEmpty) {
			leftPosition(position) match {
				case Some(nextPosition) => matrix(nextPosition._1)(nextPosition._2) = true
					printMatrix(matrix)
					position = nextPosition
				case None =>
			}
		}

		position = moveToUpperLayer(position, width, height, matrix)

		printMatrix(matrix)

		matrix
	}

	private def moveToUpperLayer(position: (Int, Int), width: Int, height: Int, matrix: Array[Array[Boolean]]): (Int, Int) = {
		var movedToUpperLayer = false
		var movingPosition = position
		while (!movedToUpperLayer) {
			val moveUpLeft = upLeftPosition(movingPosition)
			val moveUpRight = upRightPosition(movingPosition, width)
			if (moveUpLeft.nonEmpty && !matrix(moveUpLeft.get._1)(moveUpLeft.get._2)) {
				movingPosition = moveUpLeft.get
				movedToUpperLayer = true
				matrix(movingPosition._1)(movingPosition._2) = true
			} else if (moveUpRight.nonEmpty && !matrix(moveUpRight.get._1)(moveUpRight.get._2)) {
				movingPosition = moveUpRight.get
				movedToUpperLayer = true
				matrix(movingPosition._1)(movingPosition._2) = true
			} else {
				val moveRight = rightPosition(movingPosition, width)
				if (moveRight.nonEmpty) {
					movingPosition = moveRight.get
					matrix(movingPosition._1)(movingPosition._2) = true
				} else {
					movingPosition = moveUpLeft.get
					movedToUpperLayer = true
					matrix(movingPosition._1)(movingPosition._2) = true
				}
			}
		}
		movingPosition
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

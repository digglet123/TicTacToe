package tictactoe

object Solution extends App {

  def solution(board: Array[Char]): Int = {
    var xwins = false
    var owins = false
    //All winning streak combinations in a tuple array
    var winstreaks: Array[(Int, Int, Int)] = Array((0, 1, 2), (3, 4, 5), (6, 7, 8), (0, 3, 6), (1, 4, 7), (2, 5, 8), (0, 4, 8), (2, 4, 6))
    //Calculate the difference of the amount of X:s and O:s
    var difference = board.filter(x => x == 'X').length - board.filter(x => x == 'O').length
    //Check that board is not invalid = right size, right characters, right amount of characters (X always starts)
    if (!(board.length == 9 && board.forall(x => List('X', 'O', ' ') contains x) && difference >= 0 && difference <= 1)) {
      return 4
    }
    //Check if X has a win streak 
    if (winstreaks.exists(x => board(x._1) == 'X' && board(x._1) == board(x._2) && board(x._2) == board(x._3))) {
      xwins = true
    }
    //Check if O has a win streak 
    if (winstreaks.exists(x => board(x._1) == 'O' && board(x._1) == board(x._2) && board(x._2) == board(x._3))) {
      owins = true
    }
    //Check for winner
    (xwins, owins) match {
      //X wins
      case (true, false) => return 1
      //O wins
      case (false, true) => return 0
      //2 winners = invalid board 
      case (true, true)  => return 4
      //No winner
      case (false, false) => {
        //Check if the situation is a tie (All squares have been occupied but no winner)
        if (!board.exists(x => x == ' ')) {
          return 2
        }
        //Game is unfinished unfinished
        else {
          return 3
        }
      }
    }
  }

  //Read user input in the console
  var game = true
  while (game) {
    val input = scala.io.StdIn.readLine("Give a TicTacToe board as a string using 'X' 'O' and ' ' (space):\n")
    println("Result: " + solution(input.toCharArray()) + "\n")
    println("Give q to exit and any other character to try again:")
    val next = scala.io.StdIn.readLine()
    if (next.take(1) == "q") {
      game = false
    }
    println("-" * 70 + "\n")
  }

}
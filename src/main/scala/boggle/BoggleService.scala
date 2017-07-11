package boggle

import java.io.File

import scala.annotation.tailrec
import scala.io.Source

case class Letter(value: String, row: Int, col: Int)

trait Status

case object Unvisited extends Status

case object Visited extends Status

trait BoggleConfiguration {

  // make prefixes of words in word list up to this many initial letters for quick lookup
  val numNgrams: Int = 6

  // 370100 words from https://github.com/dwyl/english-words
  val dictionaryUri: String = s"${new File(".").getCanonicalPath}/src/main/resources/words_alpha.txt"

  // nine seems to be the magic number for word length
  // at 10 not many more words are found and performance starts to decrease
  val maxWordLength: Int = 9

  // boggle only take 3+ letter words
  val minWordLength: Int = 3

}

class BoggleService() extends BoggleConfiguration {

  private val wordList: List[String] = {
    val source = Source.fromFile(dictionaryUri)
    source.getLines().toList
  }

  private val wordMap: Map[String, Set[String]] = {
    val filteredList = wordList.filter(_.length >= minWordLength)

    filteredList.foldLeft[Map[String, Set[String]]](Map.empty[String, Set[String]])((acc, word) => {
      val nextWord = word.toUpperCase
      val nGrams = nextWord.foldLeft[List[String]](List.empty[String])((grams, nextLetter) => {
        if (grams.isEmpty) {
          List(s"$nextLetter")
        } else {
          grams :+ s"${grams.last}$nextLetter"
        }
      })

      nGrams.slice(1, numNgrams).foldLeft(acc)((accumulator, gram) => {
        val toAdd = acc.getOrElse(gram, Set.empty[String])
        // only add the word to the set if its the final set that will be used to search to save memory
        val setOfFullWords = if (splitWordAtMaxGramSize(nextWord) == gram) {
          toAdd + nextWord
        } else {
          toAdd
        }
        accumulator + (gram -> setOfFullWords)
      })
    })
  }

  private def splitWordAtMaxGramSize(word: String): String = {
    if (word.length >= numNgrams) {
      word.splitAt(numNgrams)._1
    } else {
      word
    }
  }

  private def validateLetter(letter: Letter): Unit = {
    letter match {
      case Letter("", _, _) => throw new Exception("Boggle cells cannot be empty")
      case Letter(value, _, _) if value.length > 1 && value != "QU" =>
        throw new Exception("Boggle cells can only contain one letter unless it's Qu")
      case _ =>
    }
  }

  /**
    * Build the adjacency list for the game board
    * @param board
    * @return
    */
  private[boggle] def generateGraph(board: List[List[String]]): Map[Letter, List[Letter]] = {
    val dim = board.size

    def generateNeighbors(row: Int, col: Int): List[(Int, Int)] = {
      List(
        (row, col + 1),
        (row, col - 1),
        (row + 1, col),
        (row - 1, col),
        (row + 1, col + 1),
        (row + 1, col - 1),
        (row - 1, col + 1),
        (row - 1, col - 1)
      ).filter(pos => pos._1 > -1 && pos._2 > -1 && pos._1 < dim && pos._2 < dim)
    }

    @tailrec
    def makeAdjList(matrix: List[List[String]], row: Int = 0, col: Int = 0,
                    adjList: Map[Letter, List[Letter]] = Map.empty[Letter, List[Letter]]): Map[Letter, List[Letter]] = {
      if (row == dim) {
        adjList
      } else {
        val currentLetter = Letter(matrix(row)(col).toUpperCase, row, col)

        validateLetter(currentLetter)

        val neighbors = generateNeighbors(row, col).map(pos => {
          val (r, c) = pos
          val nextLetter = Letter(matrix(r)(c).toUpperCase, r, c)

          validateLetter(nextLetter)

          nextLetter
        })
        val updatedList = adjList + (currentLetter -> neighbors)

        if (col == dim - 1) {
          makeAdjList(matrix, row + 1, 0, updatedList)
        } else {
          makeAdjList(matrix, row, col + 1, updatedList)
        }
      }
    }

    makeAdjList(board)
  }

  /**
    * Find all possible words given the dictionary and word length constraints in the configuration
    * @param board
    * @return
    */
  def solve(board: List[List[String]]): Set[String] = {
    board.foreach(row => if (row.size != board.size) {
      throw new Exception("Boggle board must have the same number of rows and columns!")
    })

    val graph = generateGraph(board)

    def traverse(node: Letter, status: Map[Letter, Status] = Map.empty[Letter, Status],
                 wordsFound: List[String] = List.empty[String]): List[String] = {
      val notVisited = graph(node).filter(letter => {
        status.get(letter).isEmpty
      })

      if (notVisited.isEmpty) {
        wordsFound
      } else {
        val (toVisit, grams) = {
          // add the current letter to the last word found and check to see if any more words are possible
          val nextWord = s"${wordsFound.lastOption.getOrElse("")}${node.value}"

          val nextNodes = if (nextWord.length >= numNgrams && nextWord.length <= maxWordLength) {
            notVisited
          } else {
            notVisited.filter(letter => {
              wordMap.get(s"$nextWord${letter.value}").isDefined
            })
          }

          (nextNodes, wordsFound :+ nextWord)
        }

        if (toVisit.isEmpty) {
          wordsFound
        } else {
          toVisit.flatMap(nextNode => traverse(nextNode, status + (node -> Visited), grams))
            .filter(_.length >= minWordLength)
        }
      }
    }

    graph.keys.flatMap(traverse(_)).toSet.filter(result => {
      val key = splitWordAtMaxGramSize(result)

      wordMap.get(key) match {
        case Some(set) => set.contains(result)
        case None => false
      }
    })
  }

}

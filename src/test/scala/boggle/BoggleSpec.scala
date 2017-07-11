package boggle

import org.scalatest._

class BoggleSpec extends FlatSpec with Matchers {
  private val littleBoggle = List(
    List("U", "N", "L"),
    List("M", "N", "E"),
    List("A", "Y", "M")
  )

  private val regularBoggle = List(
    List("G", "U", "O", "C"),
    List("F", "P", "B", "N"),
    List("S", "U", "T", "O"),
    List("A", "H", "A", "G")
  )

  private val bigBoggle = List(
    List("O", "F", "O", "V", "I", "E", "A"),
    List("R", "E", "G", "I", "B", "V", "D"),
    List("D", "G", "X", "I", "C", "U", "F"),
    List("U", "O", "U", "G", "E", "H", "Z"),
    List("U", "E", "L", "I", "S", "B", "Y"),
    List("N", "R", "A", "D", "J", "B", "E"),
    List("A", "B", "C", "D", "E", "F", "G")
  )

  private val hugeBoggle = List(
    List("H", "O", "D", "B", "A", "M", "O", "C", "A", "R"),
    List("N", "I", "P", "O", "O", "U", "E", "I", "O", "O"),
    List("Z", "U", "U", "L", "S", "I", "O", "S", "I", "Y"),
    List("T", "P", "R", "R", "E", "F", "Z", "U", "O", "T"),
    List("U", "H", "A", "E", "I", "N", "S", "D", "C", "U"),
    List("U", "V", "C", "QU", "E", "L", "R", "R", "I", "S"),
    List("W", "U", "S", "A", "W", "V", "A", "Y", "I", "I"),
    List("U", "I", "U", "T", "S", "R", "T", "P", "D", "E"),
    List("O", "K", "O", "O", "D", "U", "N", "O", "L", "S"),
    List("N", "H", "L", "U", "U", "Z", "O", "X", "I", "L")
  )

  private val boggleService = new BoggleService

  "boggleService" should "make the graph with the right values" in {
    val adjList = boggleService.generateGraph(littleBoggle)

    assert(adjList.size == Math.pow(littleBoggle.size, 2))

    assert(adjList(Letter("U", 0, 0)) == List(
      Letter("N", 0, 1),
      Letter("M", 1, 0),
      Letter("N", 1, 1)
    ))
  }

  "boggleService" should "make the right graph" in {
    val twoByTwo = List(
      List("A", "B"),
      List("D", "E")
    )

    val threeByThree = List(
      List("A", "B", "C"),
      List("D", "E", "F"),
      List("G", "H", "I")
    )

    var count = 0

    // modified traversal code that is designed to fully explore the graph and count the number of possible paths
    def traverse(graph: Map[Letter, List[Letter]], node: Letter,
                 status: Map[Letter, Boolean] = Map.empty[Letter, Boolean]): String = {
      count += 1
      val notVisited = graph(node).filter(letter => {
        status.get(letter).isEmpty
      })

      if (notVisited.isEmpty) {
        "i dont care"
      } else {
        notVisited.map(nextNode => traverse(graph, nextNode, status + (node -> true))).mkString(", ")
      }
    }

    val graphOfOne = boggleService.generateGraph(List(List("A")))
    val graphOfTwo = boggleService.generateGraph(twoByTwo)
    val graphOfThree = boggleService.generateGraph(threeByThree)

    count = 0
    graphOfOne.keys.map(key => traverse(graphOfOne, key))
    assert(count == 1)

    count = 0
    graphOfTwo.keys.map(key => traverse(graphOfTwo, key))
    assert(count == 64)

    count = 0
    graphOfThree.keys.map(key => traverse(graphOfThree, key))
    assert(count == 10305)
  }

  // tests will start to fail if dictionary or test boards are changed
  "boggleService" should "solve boggle" in {
    // val time = System.currentTimeMillis()
    val littleSolution = boggleService.solve(littleBoggle)
    // println(System.currentTimeMillis() - time)

    assert(littleSolution.size == 27)
    assert(littleSolution.contains("UNMAN"))

    // val time1 = System.currentTimeMillis()
    val regularSolution = boggleService.solve(regularBoggle)
    // println(System.currentTimeMillis() - time1)

    assert(regularSolution.size == 97)
    assert(regularSolution.contains("GOATBUSH"))

    // val time2 = System.currentTimeMillis()
    val bigSolution = boggleService.solve(bigBoggle)
    // println(System.currentTimeMillis() - time2)

    assert(bigSolution.size == 577)
    assert(bigSolution.contains("NEURALGIC"))
  }

  "boggleService" should "solve giant boggle including Qu tile" in {
    // val time3 = System.currentTimeMillis()
    val omgGiantBoggle = boggleService.solve(hugeBoggle)
    // println(System.currentTimeMillis() - time3)

    assert(omgGiantBoggle.size == 1563)
    assert(omgGiantBoggle.contains("EQUINE"))
  }

}

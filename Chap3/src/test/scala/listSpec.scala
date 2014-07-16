package fpinscala.datastructures
import org.specs2.mutable.Specification
import org.specs2.specification.{Scope, After, BeforeAfterEach}

object ListSpec extends Specification {
  "sum" should {
    "sum a list of Int" in {
     List.sum(List(1,2,3)) === 6
    }
  }

  "product" should {
    "multiply a list of Double" in {
      List.product(List(1,2,3)) === 6
    }
  }

  "tail" should {
    "return all but the head of a list" in {
      List.tail(List(1,2,3)) === List(2,3)
    }
  }

  "setHead" should {
    "relplace the head of a list" in {
      List.setHead(3, List(1,2,3)) === List(3,2,3)
    }
  }

  "drop" should {
    "drop n amount of elements" in {
      List.drop(List(1,2,3), 2) === List(3)
    }
  }

  "dropWhile" should {
    "drop elements while they pass f" in {
      List.dropWhile(List(1,2,3,4))(_ != 3) === List(3,4)
    }
  }

  "init" should {
    "return a list with all but the last element" in {
      List.init(List(1,2,3,4)) === List(1,2,3)
    }
  }

  "sum2" should {
    "return the sum of a list" in {
      List.sum2(List(1,2,3,4,5)) === 15
    }
  }

  "product2" should {
    "return the produt of elements in a list" in {
      List.product2(List(1,2,3)) === 6
    }
  }

  "length" should {
    "return the length of a list" in {
      List.length(List(1,2,3)) === 3
    }
  }

  "sumLeft" should {
    "return the sum of a list" in {
      List.sumLeft(List(1,2,3,4,5)) === 15
    }
  }

  "productLeft" should {
    "return the produt of elements in a list" in {
      List.productLeft(List(1,2,3)) === 6
    }
  }

  "lengthLeft" should {
    "return the length of a list" in {
      List.lengthLeft(List(1,2,3)) === 3
    }
  }

  "reverse" should {
    "return a reverse list" in {
      List.reverse(List(1,2,3)) === List(3,2,1)
    }
  }

  "append" should {
    "return a list append to another list" in {
      List.append(List(1,2,3), List(4,5)) === List(1,2,3,4,5)
    }
  }

  "concatenate" should {
    "join a list of lists into one list" in {
      List.concatenate(List(List(1,2), List(3,4), List(5))) === List(1,2,3,4,5)
    }
  }

  "addOneToAll" should {
    "add one to each element of a list of Int" in {
      List.addOneToAll(List(1,2,3,4)) === List(2,3,4,5)
    }
  }

  "doubleToString" should {
    "return a list of strings of the previous doubles" in {
      List.doubleToString(List(1.0,2.0,3.0)) === List("1.0","2.0","3.0")
    }
  }

  "map" should {
    "add one to every elem in the List" in {
      List.map(List(1,2))(_+1) === List(2,3)
    }
  }

  "filter" should {
    "only return odd numbers" in {
      List.filter(List(1,2,3,4,5))(x => (x % 2 != 0)) === List(1,3,5)
    }
  }

  "flatMap" should {
    "return a list flattened for some function" in {
      List.flatMap(List(1,2,3,4))(i => List(i,i)) === List(1,1,2,2,3,3,4,4)
    }
  }

  "filterWithFlatMap" should {
    "do the same thing as the previous filter" in {
      List.filterWithFlatMap(List(1,2,3,4,5))(x => (x % 2 != 0)) === List(1,3,5)
    }
  }

  "mergeAdd" should {
    "merge two Int lists together by adding corresponding elements" in {
      List.mergeAdd(List(2,3,4), List(3,4,5)) === List(5,7,9)
    }
  }

  "merge" should {
    "merge any two lists together with some function f" in {
      List.merge(List(2,4,6),List(2,2,2), (x: Int, y: Int) => x / y) === List(1,2,3)
    }
  }

  "hasSubsequence" should {
    "return a boolean if a list contains the subsequence" in {
      List.hasSubsequence(List(2,3,4,5,6), List(4,5)) === true
      List.hasSubsequence(List(2,3,4,5,6), List(3,5)) === false
      List.hasSubsequence(List(2,3,4,5,6), List(1)) === false
    }
  }
}
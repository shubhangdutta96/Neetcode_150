import scala.collection.mutable.{HashMap, ListBuffer}
object ArrayandHashing extends App{

  // CONTAINS DUPLICATE
  def containsDuplicate(nums: Array[Int]): Boolean = {
    val map = HashMap[Int, Int]()
    for (i <- 0 to nums.length - 1 by +1) {
      map(nums(i)) = map.getOrElse(nums(i), 0) + 1
    }
    map.values.exists(_ > 1)
  }


  // VALID ANAGRAM
  def validAnagram(s: String, t: String): Boolean = {
    var isAnagram = true
    var map = HashMap[Character, Int]()
    if (s.length != t.length) {
      isAnagram = false
    }
    else {
      val sArr = s.toCharArray()
      var tArr = t.toCharArray()
      var a = sArr.sorted
      var b = tArr.sorted
      for (i <- sArr.indices) {
        if (a(i) != b(i)) {
          isAnagram = false
        }
      }
    }
    isAnagram
  }


  // TWO SUM
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    var map = HashMap[Int, Int]()
    var arr = Array(0, 0)
    for (i <- nums.indices) {
      map(nums(i)) = map.getOrElse(nums(i), 0) + i
    }
    for (i <- nums.indices) {
      val complement = target - nums(i)
      if (map.contains(complement) && map(complement) != i) {
        arr(0) = map(complement)
        arr(1) = i
      }
      map(nums(i)) = i
    }
    arr
  }


  // GROUP ANAGRAMS
  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    var map = HashMap[String, Int]()
    for (i <- strs.indices) {
      map(strs(i)) = map.getOrElse(strs(i), 0) + 1
    }
    val internalList = ListBuffer[String]()
    val finalList = ListBuffer[List[String]]()
    if (strs.length < 2) {
      internalList += strs(0)
      finalList += internalList.toList
    }
    else {
      for (i <- strs.indices) {
        var s = strs(i)
        for (j <- i + 1 to strs.length - 1) {
          var t = strs(j)
          if (map(s) > 0 && map(t) > 0 && isAnagram(s, t)) {
            internalList += t
            map(t) = map.getOrElse(t, 0) - 1
          }

        }
        if (map(s) > 0) {
          internalList += s
        }
        map(s) = map.getOrElse(s, 0) - 1

        if (internalList.length > 0) {
          finalList += internalList.toList
        }
        internalList.clear()
      }
    }
    return finalList.toList
  }

  def isAnagram(s: String, t: String): Boolean = {
    var anagram = true
    if (s.length != t.length) {
      anagram = false
    }
    else {
      var a = s.sorted
      var b = t.sorted
      for (i <- s.indices) {
        if (a(i) != b(i)) {
          anagram = false
        }
      }
    }
    anagram
  }


  // TOP K FREQUENT ELEMENTS
  def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {
    var list = ListBuffer[Int]()
    var m = HashMap[Int, Int]()
    var l = k
    if (nums.length == k) {
      for (i <- nums) {
        list += i
      }
    }
    else {
      for (i <- nums.indices) {
        m(nums(i)) = m.getOrElse(nums(i), 0) + 1
      }
      val sortedVal = m.toSeq.sortWith(_._2 > _._2)
      for ((key, value) <- sortedVal) {
        if (l > 0) {
          l = l - 1
          list += key
        }
      }
    }
    list.toArray
  }


  // PRODUCT OF ARRAY EXCEPT SELF
  def productExceptSelf(nums: Array[Int]): Array[Int] = {
    var leftPro = 1
    var rightPro = 1
    val arr = new Array[Int](nums.length)
    for (i <- 0 to nums.length - 1 by +1) {
      arr(i) = leftPro
      leftPro *= nums(i)
    }
    for (i <- nums.length - 1 to 0 by -1) {
      arr(i) *= rightPro
      rightPro *= nums(i)
    }
    arr
  }


  // VALID SUDOKU
  import scala.util.control.Breaks._
  def isValidSudoku(board: Array[Array[Char]]): Boolean = {
    var valid = true
    breakable {
      for (i <- board.indices) {
        for (j <- board(i).indices) {
          if (board(i)(j) != '.') {
            if (!isValid(i, j, board, board(i)(j))) {
              valid = false
              break
            }
          }
        }
      }
    }
    valid
  }

  def isValid(row: Int, col: Int, board: Array[Array[Char]], digit: Char): Boolean = {
    var valid = true
    breakable {
      // rows
      for (i <- board.indices) {
        if (i != col && board(row)(i) == digit) {
          valid = false
          break
        }
      }
      // cols
      for (i <- board(row).indices) {
        if (i != row && board(i)(col) == digit) {
          valid = false
          break
        }
      }
      // grid
      var gridR = row - row % 3
      var gridC = col - col % 3
      for (i <- gridR to gridR + 2) {
        for (j <- gridC to gridC + 2) {
          if (i != row && j != col && board(i)(j) == digit) {
            valid = false
            break
          }
        }
      }
    }
    valid
  }


  // LONGEST CONSECUTIVE SUBSEQUENCE
  def longestConsecutive(nums: Array[Int]): Int = {
    var set = Set[Int]()
    var longest = 0
    for (i <- nums.indices) {
      set += (nums(i))
    }
    for (i <- nums.indices) {
      var num = nums(i)
      if (!set.contains(num - 1)) {
        var start = num
        var current = num
        while (set.contains(current + 1)) {
          current += 1
        }
        longest = math.max(longest, current - num + 1)
      }
    }
    longest
  }


}

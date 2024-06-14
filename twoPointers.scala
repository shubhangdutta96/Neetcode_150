import scala.collection.mutable.{ListBuffer,Set}
object twoPointers extends App{

  // VALID PALINDROME
  def isPalindrome(s: String): Boolean = {
    var newS = s.toLowerCase.filter(_.isLetterOrDigit)
    newS == newS.reverse
  }


  // TWO SUM II INPUT ARRAY IS SORTED
  import scala.util.control.Breaks._
  def twoSum(numbers: Array[Int], target: Int): Array[Int] = {
    var lp = 0
    var rp = numbers.length - 1
    var ans = Array(0, 0)
    breakable {
      while (lp < rp) {
        var sum = numbers(lp) + numbers(rp)
        if (sum == target) {
          ans(0) = lp + 1
          ans(1) = rp + 1
          break()
        }
        if (sum > target) {
          rp -= 1
        }
        else {
          lp += 1
        }
      }
    }
    ans
  }


  //TRAPPING RAIN WATER
  def trap(height: Array[Int]): Int = {
    var ans = 0

    //leftmax
    var left = Array[Int](height.length)
    left(0) = height(0)
    for (i <- 1 to height.length - 1 by +1) {
      left(i) = math.max(left(i - 1), height(i))
    }

    //rightmax
    var right = Array[Int](height.length)
    right(height.length - 1) = height(height.length - 1)
    for (i <- height.length - 2 to 0 by -1) {
      right(i) = math.max(right(i + 1), height(i))
    }

    //water stored at each index
    var waterStored = 0
    var totalWater = 0
    for (i <- height.indices) {
      waterStored = math.min(left(i), right(i))
      totalWater += waterStored - height(i)
      waterStored = 0
    }

    //total water stored
    totalWater
  }


  // CONTAINER WITH MOST WATER
  def maxArea(height: Array[Int]): Int = {
    var lp = 0
    var rp = height.length - 1
    var waterStored = 0
    while (lp < rp) {
      var h = math.min(height(lp), height(rp))
      var w = rp - lp
      var water = h * w
      waterStored = math.max(waterStored, water)
      if (height(lp) < height(rp)) {
        lp += 1
      }
      else {
        rp -= 1
      }
    }
    waterStored
  }


  // 3 SUM
  import scala.util.control.Breaks._

  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val numbers = nums.sorted
    val ans = Set[List[Int]]()
    var finalAns = ListBuffer[List[Int]]()
    for (i <- 0 until numbers.length - 1) {
      var third = numbers(i)
      var lp = i + 1
      var rp = numbers.length - 1
      while (lp < rp) {
        val sum = numbers(lp) + numbers(rp) + third
        if (sum == 0) {
          var list = List(numbers(lp), numbers(rp), third).sorted
          ans += list
          lp += 1
          rp -= 1
        }
        else if (sum > 0) {
          rp -= 1
        }
        else {
          lp += 1
        }
      }
    }
    for (i <- ans) {
      finalAns += i
    }
    finalAns.toList
  }

}

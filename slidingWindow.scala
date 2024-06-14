import scala.collection.mutable.{HashMap, ListBuffer, ArrayDeque}

object slidingWindow extends App{

  // BEST TIME TO BUY AND SELL STOCK
  def maxProfit(prices: Array[Int]): Int = {
    var lp = 0 // a day to buy a stock
    var rp = 1 // a day to sell a stock
    var profit = 0
    var maxProfit = 0
    while (rp < prices.length) {
      if (prices(rp) < prices(lp)) {
        lp = rp
      }
      else {
        profit = prices(rp) - prices(lp)
        maxProfit = math.max(profit, maxProfit)
        profit = 0
      }
      rp += 1
    }
    maxProfit
  }


  // LONGEST SUBSTRING WITHOUT REPEATING CHARACTERS
  def lengthOfLongestSubstring(s: String): Int = {
    var m = HashMap[Char, Int]()
    var len = 0
    var l = 0
    var r = 0

    while (r < s.length) {
      if (m.contains(s(r)))
        l = math.max(m(s(r)) + 1, l)
      m += (s(r) -> r)
      len = math.max(len, r - l + 1)
      r += 1
    }

    len
  }
  
  // SLIDING WINDOW MAXIMUM
  def maxSlidingWindow(nums: Array[Int], k: Int): Array[Int] = {
    var deque = ArrayDeque[Int]()
    var ans = ListBuffer[Int]()
    var lp = 0
    var rp = k - 1
    for (i <- 0 to rp by +1) {
      if (deque.isEmpty) {
        deque.append(i)
      }
      else if (nums(deque.last) < nums(i)) {
        while (!deque.isEmpty && nums(deque.last) < nums(i)) {
          deque.removeLast()
        }
        deque.append(i)
      }
      else {
        deque.append(i)
      }
    }
    ans += nums(deque.head)

    for (i <- rp + 1 to nums.length - 1 by +1) {
      lp += 1
      if (nums(deque.head) == nums(lp - 1)) {
        deque.removeHead()
      }

      if (!deque.isEmpty && nums(deque.last) < nums(i)) {
        while (!deque.isEmpty && nums(deque.last) < nums(i)) {
          deque.removeLast()
        }
        deque.append(i)
      }
      else {
        deque.append(i)
      }
      ans += nums(deque.head)
    }
    ans.toArray
  }

}

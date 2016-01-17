package reverse

object ReversePhrase {
  /**
    * Reverse array in place from start to end inclusive
   */
  def reverse(arr: Array[Char], start: Int, end: Int) = {
    if (end > start) {
      (0 to (end - start)/2) foreach { i =>
        val iStart = i + start
        val iEnd = end -i
        val b = arr(iStart)
        arr(iStart) = arr(iEnd)
        arr(iEnd) = b
      }
    }
  }

  /*
   * "I love you" becomes "you love I"
   * Preserves whitespaces
  */
  def reverseWordsOrderInPlace(phrase: String) = {
    val arr = phrase.toCharArray
    val l = phrase.length - 1
    reverse(arr, 0, l)
    var startWordIndex: Option[Int] = None
    (0 to l) foreach { i =>
      if (startWordIndex.isDefined) {
        // Ending phrase or new space after word
        if (i == l || arr(i) == ' ') {
          val endIndex = if (i == l && arr(i) != ' ') i else i - 1
          reverse(arr, startWordIndex.get, endIndex)
          startWordIndex = None
        }
      } else if (arr(i) != ' ') {
        /*First word char*/
        startWordIndex = Some(i)
      }
    }
    arr.mkString
  }

 def reverseWordsOrder(phrase: String) =
   (phrase.reverse.split(" ", -1) map (_.reverse)).mkString(" ")
}
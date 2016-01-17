package reverse

import org.specs2.Specification
import org.specs2.ScalaCheck

import ReversePhrase._

class ReversePhraseTest extends Specification with ScalaCheck {
  def is = s2"""
   This is a specification to check the 'reverse phrase but words'
   The reverse function is correct                                                         $e1
   The phrase should be reversed in place but the words chars remain in order              $e10
   The phrase should be reversed but the words chars remain in order                       $e11
   The phrase should be reversed in place but the words chars remain in order with spaces  $e20
   The phrase should be reversed but the words chars remain in order with spaces           $e21
   """
   
  def e1 = prop {
    (s: String) => s.reverse == { 
      val chars = s.toCharArray
      reverse(chars, 0 , s.length - 1)
      chars.mkString
    }
  }                                                                    
  def e10 = reverseWordsOrderInPlace("I am a legend") must_== "legend a am I"
  def e11 = reverseWordsOrder("I am a legend") must_== "legend a am I"
  def e20 = reverseWordsOrderInPlace(" I am a  legend  ") must_== "  legend  a am I "
  def e21 = reverseWordsOrder(" I am a  legend  ") must_== "  legend  a am I "
}
package io.lambdaworks.workshop.functions
import com.lightbend.emoji.ShortCodes.Defaults._
import com.lightbend.emoji.ShortCodes.Implicits._

import scala.util.Try

object EmojifyText {

  def emojify(sentence: String): String = {
    val words          = sentence.split(" ")
    val wordsEmojified = words.map(w => emojiOrWord(w.filter(c => isLetter(c))))
    wordsEmojified.reduce((w1, w2) => w1.concat(" ").concat(w2))
  }

  private def emojiOrWord(word: String): String =
    Try(word.toLowerCase.emoji.toString).getOrElse(word)

  private def isLetter(char: Char) = char.isLetter

}

package com.chatwork.quiz.misc

/**
 * ワードをカウントするオブジェクト。
  * val words = List("apple banana", "orange apple mango", "kiwi papaya orange","mango orange muscat apple")
 */
object WordCounter {

  /**
   * 文字列から単語数をカウントする。
   *
   * @param words 文字列
   * @return 単語がキー、単語数がヴァリューのマップ
   */
  def countWords(words: List[String]): Map[String, Int] = {
    def count(lst: List[String], map: Map[String, Int] = Map()): Map[String, Int] = {
      if(lst == Nil) {
        map
      } else {
        if( lst.head.contains(" ")) {
          count(lst.head.split(" ").toList ++ lst.tail, map)
        } else if( lst.head == "" ){
          count(lst.tail, map)
        } else {
          count(lst.tail, map.updated(lst.head, map.getOrElse(lst.head, 0) + 1))
        }
      }
    }
    count(words)
  }
}

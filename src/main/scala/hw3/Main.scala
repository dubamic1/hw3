package hw3

import scala.collection.mutable.ArrayBuffer
import scala.math.{pow, sqrt}

object Main extends App {
  def int2bin(i: Int, numPos: Int): String = {
    var res: ArrayBuffer[Char] = new ArrayBuffer[Char]()
    val binaryString: String = i.toBinaryString

    if (numPos - binaryString.length < 0) {
      throw new IllegalArgumentException("Decimal number " + i + " has more than " + numPos + " digit in binary.")
    }
    for (j <- 0 until (numPos - binaryString.length)) {
      res += '0'
    }

    res.mkString("") + binaryString
  }

  def bin2Gray(str: String): String = {
    val res: ArrayBuffer[Char] = new ArrayBuffer[Char]()

    for (i <- 0 until str.length) {
      if (i == 0) {
        res.append(str(i))
      }
      else {
        val sum: Int = ((str(i).asDigit + str(i - 1).asDigit) % 2) + 48 // 48 for ascii value of '0'
        res.append(sum.toChar)
      }
    }
    res.mkString("")
  }

  def standardDeviation(vector: List[Double]): Double = {
    if (vector == null) {
      throw new IllegalArgumentException("The input vector does not exist")
    }

    if (vector.isEmpty) {
      throw new IllegalArgumentException("The input vector is empty")
    }

    val averageX: Double = vector.sum / vector.size

    val xMinusAverageXPow: List[Double] = vector.map(x =>
      pow(x - averageX, 2))

    val avg: Double = xMinusAverageXPow.sum / xMinusAverageXPow.size

    sqrt(avg)
  }

  def getClearCorpus(corpus: String): String = {
    if (corpus == null) {
      throw new IllegalArgumentException("The corpus does not exist")
    }
    corpus.replaceAll("[^a-zA-Z]+", "").toLowerCase()
  }

  def getMapOfCorpus(cleanCorpus: String): scala.collection.mutable.Map[Char, Int] = {
    var mapOfRecords: scala.collection.mutable.Map[Char, Int] = scala.collection.mutable.Map[Char, Int]()

    cleanCorpus.foreach(x =>
      if (mapOfRecords.contains(x)) {
        mapOfRecords(x) += 1
      }
      else {
        mapOfRecords += (x -> 1)
      }
    )
    mapOfRecords
  }

  def letterFrequencyRanking(corpus: String): String = {
    val onlySmallLettersCorpus: String = getClearCorpus(corpus)
    if (onlySmallLettersCorpus == "") {
      new String("")
    }
    val mapOfRecords: scala.collection.mutable.Map[Char, Int] = getMapOfCorpus(onlySmallLettersCorpus)
    val res: ArrayBuffer[Char] = new ArrayBuffer[Char]()

    while (mapOfRecords.nonEmpty) {
      var layerRes: ArrayBuffer[Char] = new ArrayBuffer[Char]()
      var currentMax = 0

      mapOfRecords.foreach(x =>
        if (x._2 > currentMax) {
          currentMax = x._2
        }
      )

      mapOfRecords.foreach(x =>
        if (x._2 == currentMax) {
          layerRes += x._1
          mapOfRecords.remove(x._1)
        }
      )

      layerRes.sorted.foreach(x =>
        res.append(x)
      )
    }

    res.mkString("")
  }

  def gray(bits: Int): List[String] = {
    if (bits < 0) {
      throw new IllegalArgumentException("Negative amount of bits")
    }
    if (bits == 0) {
      throw new IllegalArgumentException("None bits required from input")
    }

    val res: ArrayBuffer[String] = new ArrayBuffer[String]()

    for (i <- 0 until math.pow(2.0, bits.toDouble).toInt) {
      val binary: String = int2bin(i, bits)
      res += bin2Gray(binary)
    }

    res.toList
  }

  def romanji(katakana: String): String = {
    val res: ArrayBuffer[Char] = new ArrayBuffer[Char]()
    var iterator: Int = 0
    val obj = Katakana

    var doublesConsonant = false
    var doublesConsonantOfNa = false


    katakana.foreach {
      x =>
        if (x == 'ン' || x == 'ア' || x == 'イ' || x == 'ウ' || x == 'エ' || x == 'オ') {
          res += obj.symbols(x)(0)
          iterator += 1
        }
        else if (x == 'ヤ' || x == 'ヨ' || x == 'ユ' || x == 'ッ' || x == 'ー' || x == 'ン') {
          x match {
            case 'ー' => {
              res(iterator - 1) = obj.longVowels(res(iterator - 1))
            }
            case 'ッ' => {
              doublesConsonant = true
            }
            case 'ヤ' => {
              if (res(iterator - 1) != 'i') throw new IllegalArgumentException("Previous symbol must be 'i'!")
              res(iterator - 1) = 'y'
              res += 'a'
              iterator += 1
            }

            case 'ヨ' => {
              if (res(iterator - 1) != 'i') throw new IllegalArgumentException("Previous symbol must be 'i'!")

              res(iterator - 1) = 'y'
              res += 'o'
              iterator += 1
            }

            case 'ユ' => {
              if (res(iterator - 1) != 'i') throw new IllegalArgumentException("Previous symbol must be 'i'!")

              res(iterator - 1) = 'y'
              res += 'u'
              iterator += 1
            }
            case 'ン' => {
              doublesConsonantOfNa = true
            }
            case _ => throw new IllegalArgumentException("Symbol " + x + " not found!")

          }
        }
        else {
          if (!obj.symbols.contains(x)) throw new IllegalArgumentException("Symbol not found")
          else {
            if (doublesConsonant) {
              res += obj.symbols(x)(0)
              res += obj.symbols(x)(0)
              res += obj.symbols(x)(1)
              iterator += 3
              doublesConsonant = false
            }
            else if (doublesConsonantOfNa) {
              if (res(iterator - 2) != 'n') throw new IllegalArgumentException("Wrong format")
              res += obj.symbols(x)(0)
              res += obj.symbols(x)(0)
              res += obj.symbols(x)(1)
              iterator += 3
              doublesConsonantOfNa = false


            }
            else {
              res += obj.symbols(x)(0)
              res += obj.symbols(x)(1)
              iterator += 2
            }
          }

        }
    }
    res.mkString("")
  }

}

object Katakana {
  val symbols = Map(
    'ア' -> List('a'), 'イ' -> List('i'), 'ウ' -> List('u'), 'エ' -> List('e'), 'オ' -> List('o'),
    'ン' -> List('n'),
    'カ' -> List('k', 'a'), 'キ' -> List('k', 'i'), 'ク' -> List('k', 'u'), 'ケ' -> List('k', 'e'), 'コ' -> List('k', 'o'),
    'ガ' -> List('g', 'a'), 'ギ' -> List('g', 'i'), 'グ' -> List('g', 'u'), 'ゲ' -> List('g', 'e'), 'ゴ' -> List('g', 'o'),
    'サ' -> List('s', 'a'), 'シ' -> List('s', 'i'), 'ス' -> List('s', 'u'), 'セ' -> List('s', 'e'), 'ソ' -> List('s', 'o'),
    'ザ' -> List('z', 'a'), 'ジ' -> List('z', 'i'), 'ズ' -> List('z', 'u'), 'ゼ' -> List('z', 'e'), 'ゾ' -> List('z', 'o'),
    'タ' -> List('t', 'a'), 'チ' -> List('t', 'i'), 'ツ' -> List('t', 'u'), 'テ' -> List('t', 'e'), 'ト' -> List('t', 'o'),
    'ダ' -> List('d', 'a'), 'ヂ' -> List('d', 'i'), 'ヅ' -> List('d', 'u'), 'デ' -> List('d', 'e'), 'ド' -> List('d', 'o'),
    'ナ' -> List('n', 'a'), 'ニ' -> List('n', 'i'), 'ヌ' -> List('n', 'u'), 'ネ' -> List('n', 'e'), 'ノ' -> List('n', 'o'),
    'ハ' -> List('h', 'a'), 'ヒ' -> List('h', 'i'), 'フ' -> List('h', 'u'), 'ヘ' -> List('h', 'e'), 'ホ' -> List('h', 'o'),
    'バ' -> List('b', 'a'), 'ビ' -> List('b', 'i'), 'ブ' -> List('b', 'u'), 'ベ' -> List('b', 'e'), 'ボ' -> List('b', 'o'),
    'パ' -> List('p', 'a'), 'ピ' -> List('p', 'i'), 'プ' -> List('p', 'u'), 'ペ' -> List('p', 'e'), 'ポ' -> List('p', 'o'),
    'マ' -> List('m', 'a'), 'ミ' -> List('m', 'i'), 'ム' -> List('m', 'u'), 'メ' -> List('m', 'e'), 'モ' -> List('m', 'o'),
    'ヤ' -> List('y', 'a'), 'ユ' -> List('y', 'u'), 'ヨ' -> List('y', 'o'),
    'ラ' -> List('r', 'a'), 'リ' -> List('r', 'i'), 'ル' -> List('r', 'u'), 'レ' -> List('r', 'e'), 'ロ' -> List('r', 'o'),
    'ワ' -> List('w', 'a'), 'ヰ' -> List('w', 'i'), 'ヱ' -> List('w', 'e'), 'ヲ' -> List('w', 'o'),
  )
  val longVowels = Map(
    'a' -> 'ā',
    'i' -> 'ī',
    'e' -> 'ē',
    'u' -> 'ū',
    'o' -> 'ō'
  )
}
import scala.io.StdIn.readLine
import scala.sys.exit

class Converter {
  def value(r: Char): Int = {
    r match {
      case 'I' => 1
      case 'V' => 5
      case 'X' => 10
      case 'L' => 50
      case 'C' => 100
      case 'D' => 500
      case 'M' => 1000
    }
  }

  val numeralsFor100s = List("C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM")
  val numeralsFor10s = List("X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC")
  val numeralsFor1s = List("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")

  def arabicToRoman(value: Int): String = {
    "M" * (value / 1000) + {
      if (value % 1000 > 99) numeralsFor100s(value % 1000 / 100 - 1) else ""
    } + {
      if (value % 100 > 9) numeralsFor10s(value % 100 / 10 - 1) else ""
    } + {
      if (value % 10 > 0) numeralsFor1s(value % 10 - 1) else ""
    }
  }

  def romanToArabic(str: String): Int = {
    var res = 0
    var i = 0
    while ( {
      i < str.length
    }) {
      val s1 = value(str.charAt(i))
      if (i + 1 < str.length) {
        val s2 = value(str.charAt(i + 1))
        if (s1 >= s2) {
          res = res + s1
        }
        else {
          res = res + s2 - s1
          i += 1
        }
      }
      else res = res + s1
      i += 1
    }
    res
  }
}

object Converter extends App {
  val converter = new Converter
  val option = readLine("Arabic or Roman? ")
  val arabic = "Arabic".toLowerCase()
  val roman = "Roman".toLowerCase()

  while (true) {
    if (option != roman && option != arabic) {
      println("Wrong operator")
      exit(0)
    } else if (option == arabic) {
      val str = readLine(s"Type number is $option: ")
      println("Roman number: " + converter.arabicToRoman(str.toInt))
      exit(0)
    }
    else if (option == roman) {
      val str = readLine(s"Type number is $option: ")
      println("Arabic number: " + converter.romanToArabic(str))
      exit(0)
    } else {
      println("Wrong operator")
    }
  }
}

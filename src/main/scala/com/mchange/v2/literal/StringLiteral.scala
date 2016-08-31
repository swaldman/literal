package com.mchange.sc.v2.literal

import scala.annotation.tailrec

object StringLiteral {

  final class BadStringLiteralException( msg : String, cause : Throwable = null ) extends Exception( msg, cause )

  final object Flag {
    val a = 1 << 0 // BEL,             0x07
    val b = 1 << 1 // backspace,       0x08
    val f = 1 << 2 // form feed,       0x0C
    val n = 1 << 3 // new line,        0X0A
    val r = 1 << 4 // carriage return, 0x0D
    val t = 1 << 5 // horizontal tab,  0x09
    val u = 1 << 6 // two byte hex unicode prefix
    val v = 1 << 7 // vertical tab,    0x0B
    val x = 1 << 8 // one byte hex prefix

    val backslash    = 1 <<  9
    val singlequote  = 1 << 10
    val doublequote  = 1 << 11
    val questionmark = 1 << 12
    val octal        = 1 << 13 // one byte octal prefix

    val e = 1 << 14 // gcc extension, escape, 0x1B
    val E = 1 << 15 // gcc extension, escape, 0x1B
  }

  val C_FLAGS = {
    import Flag._
    a | b | f | n | r | t | v | x | backslash | singlequote | doublequote | questionmark | octal
  }
  val JAVA_FLAGS = {
    import Flag._
    b | f | n | r | t | u | backslash | singlequote | doublequote | octal
  }

  val GCC_FLAGS = {
    import Flag._
    C_FLAGS | e | E
  }

  val SCALA_FLAGS = JAVA_FLAGS

  val PERMISSIVE_FLAGS = {
    import Flag._
    a | b | f | n | r | t | u | v | x | backslash | singlequote | doublequote | questionmark | octal | e | E
  }

  def parseStringLiteral( flags : Int, source : String, index : Int = 0 ) : String = {
    _parseStringLiteral( flags, source, index, QuoteState.NoQuote )
  }
  def parseCStringLiteral( source : String, index : Int = 0 ) : String = {
    parseStringLiteral( C_FLAGS, source : String, index : Int )
  }
  def parseGCCStringLiteral( source : String, index : Int = 0 ) : String = {
    parseStringLiteral( GCC_FLAGS, source : String, index : Int )
  }
  def parseJavaStringLiteral( source : String, index : Int = 0 ) : String = {
    parseStringLiteral( JAVA_FLAGS, source : String, index : Int )
  }
  def parseScalaStringLiteral( source : String, index : Int = 0 ) : String = {
    parseStringLiteral( SCALA_FLAGS, source : String, index : Int )
  }
  def parsePermissiveStringLiteral( source : String, index : Int = 0 ) : String = {
    parseStringLiteral( PERMISSIVE_FLAGS, source : String, index : Int )
  }

  private val SimpleSubstitutions = Map (
    'a' -> 0x07.toChar,
    'b' -> 0x08.toChar,
    'f' -> 0x0C.toChar,
    'n' -> 0x0A.toChar,
    'r' -> 0x0D.toChar,
    't' -> 0x09.toChar,
    'v' -> 0x0B.toChar,
    '\\' -> 0x5C.toChar,
    '\'' -> 0x27.toChar,
    '\"' -> 0x22.toChar,
    '?' -> 0x3F.toChar
  )

  private final object QuoteState {
    case object NoQuote extends QuoteState

    case class InQuote( reverseNascent : List[Char] ) extends QuoteState

    case class InQuoteAfterSlash( reverseNascant : List[Char]                                                ) extends QuoteState
    case class InQuoteAtOctal   ( reverseNascant : List[Char], reverseNascentOctal   : List[Char], len : Int ) extends QuoteState
    case class InQuoteAtHex     ( reverseNascant : List[Char], reverseNascentHex     : List[Char], len : Int ) extends QuoteState
    case class InQuoteAtUnicode ( reverseNascant : List[Char], reverseNascentUnicode : List[Char], len : Int ) extends QuoteState
  }
  private trait QuoteState

  import QuoteState._

  private val OctalChars = Set( '0', '1', '2', '3', '4', '5', '6', '7' )
  private val HexChars   = Set( '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F' )

  @tailrec
  private def _parseStringLiteral( flags : Int, source : String, index : Int, quoteState : QuoteState ) : String = {
    val current = source.charAt( index )

    quoteState match {
      case NoQuote => {
        if ( current == '\"' ) {
          _parseStringLiteral( flags, source, index + 1, InQuote(Nil) )
        } else {
          throw new BadStringLiteralException( s"A String literal must begin with '\042', not '${current}'. [source=${source}, index=${index}, quoteState=${quoteState}]" )
        }
      }
      case InQuote( reverseNascent ) => {
        current match {
          case '\"' => reverseNascent.reverse.mkString
          case '\\' => _parseStringLiteral( flags, source, index + 1, InQuoteAfterSlash( reverseNascent ) )
          case c    => _parseStringLiteral( flags, source, index + 1, InQuote( c :: reverseNascent ) )
        }
      }
      case InQuoteAfterSlash( reverseNascent ) => {
        current match {
          case 'a'  if ((flags & Flag.a)            != 0) => _parseStringLiteral( flags, source, index, InQuote( SimpleSubstitutions( current ) :: reverseNascent ) )
          case 'b'  if ((flags & Flag.b)            != 0) => _parseStringLiteral( flags, source, index, InQuote( SimpleSubstitutions( current ) :: reverseNascent ) )
          case 'f'  if ((flags & Flag.f)            != 0) => _parseStringLiteral( flags, source, index, InQuote( SimpleSubstitutions( current ) :: reverseNascent ) )
          case 'n'  if ((flags & Flag.n)            != 0) => _parseStringLiteral( flags, source, index, InQuote( SimpleSubstitutions( current ) :: reverseNascent ) )
          case 'r'  if ((flags & Flag.r)            != 0) => _parseStringLiteral( flags, source, index, InQuote( SimpleSubstitutions( current ) :: reverseNascent ) )
          case 't'  if ((flags & Flag.t)            != 0) => _parseStringLiteral( flags, source, index, InQuote( SimpleSubstitutions( current ) :: reverseNascent ) )
          case 'v'  if ((flags & Flag.v)            != 0) => _parseStringLiteral( flags, source, index, InQuote( SimpleSubstitutions( current ) :: reverseNascent ) )
          case '\\' if ((flags & Flag.backslash)    != 0) => _parseStringLiteral( flags, source, index, InQuote( SimpleSubstitutions( current ) :: reverseNascent ) )
          case '\'' if ((flags & Flag.singlequote)  != 0) => _parseStringLiteral( flags, source, index, InQuote( SimpleSubstitutions( current ) :: reverseNascent ) )
          case '\"' if ((flags & Flag.doublequote)  != 0) => _parseStringLiteral( flags, source, index, InQuote( SimpleSubstitutions( current ) :: reverseNascent ) )
          case '?'  if ((flags & Flag.questionmark) != 0) => _parseStringLiteral( flags, source, index, InQuote( SimpleSubstitutions( current ) :: reverseNascent ) )

          case 'u' if ((flags & Flag.u) != 0) => _parseStringLiteral( flags, source, index + 1, InQuoteAtUnicode( reverseNascent, Nil, 0 ) )
          case 'x' if ((flags & Flag.x) != 0) => _parseStringLiteral( flags, source, index + 1, InQuoteAtHex( reverseNascent, Nil, 0 ) )

          // note that we don't increment index, so that we can reinterpret this character
          case '0' | '1' | '2' | '3' if ((flags & Flag.octal) != 0) => _parseStringLiteral( flags, source, index, InQuoteAtOctal( reverseNascent, Nil, 0 ) )

          case _ => throw new BadStringLiteralException( s"Unsupported escape character: '${current}'. [source=${source}, index=${index}, quoteState=${quoteState}]" )
        }
      }
      case InQuoteAtHex( reverseNascent, reverseNascentHex, len ) => {
        assert( len == 0 || len == 1 )
        if ( HexChars( current ) ) {
          val newReverseNascentHex = current :: reverseNascentHex
          if ( len == 0 ) {
            _parseStringLiteral( flags, source, index + 1, InQuoteAtHex( reverseNascent, newReverseNascentHex, len + 1 ) )
          } else {
            val newChar = Integer.parseInt( newReverseNascentHex.reverse.mkString, 16 ).toChar
            _parseStringLiteral( flags, source, index + 1, InQuote( newChar :: reverseNascent ) )
          }
        } else {
          throw new BadStringLiteralException( s"Bad character found in hex escape, not hex digit: '${current}'. [source=${source}, index=${index}, quoteState=${quoteState}]" )
        }
      }
      case InQuoteAtOctal( reverseNascent, reverseNascentOctal, len ) => {
        assert( len >= 0 && len < 3 )
        if ( OctalChars( current ) ) {
          val newReverseNascentOctal = current :: reverseNascentOctal
          if ( len < 2 ) {
            _parseStringLiteral( flags, source, index + 1, InQuoteAtOctal( reverseNascent, newReverseNascentOctal, len + 1 ) )
          } else {
            val newChar = Integer.parseInt( newReverseNascentOctal.reverse.mkString, 8 ).toChar
            _parseStringLiteral( flags, source, index + 1, InQuote( newChar :: reverseNascent ) )
          }
        } else {
          throw new BadStringLiteralException( s"Bad character found in octal escape, not octal digit: '${current}'. [source=${source}, index=${index}, quoteState=${quoteState}]" )
        }
      }
      case InQuoteAtUnicode( reverseNascent, reverseNascentUnicode, len ) => {
        assert( len >= 0 && len < 4 )
        if ( HexChars( current ) ) {
          val newReverseNascentUnicode = current :: reverseNascentUnicode
          if ( len < 3 ) {
            _parseStringLiteral( flags, source, index + 1, InQuoteAtUnicode( reverseNascent, newReverseNascentUnicode, len + 1 ) )
          } else {
            val newChar = Integer.parseInt( newReverseNascentUnicode.reverse.mkString, 16 ).toChar
            _parseStringLiteral( flags, source, index + 1, InQuote( newChar :: reverseNascent ) )
          }
        } else {
          throw new BadStringLiteralException( s"Bad character found in unicoe escape, not hex digit: '${current}'. [source=${source}, index=${index}, quoteState=${quoteState}]" )
        }
      }
    }
  }
}

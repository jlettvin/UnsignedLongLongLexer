///////////////////////////////////////////////////////////////////////////////
/// @file goto_test.h.cpp
/// Copyright(c) 2008 Jonathan D. Lettvin, All Rights Reserved
/// @author Jonathan D. Lettvin
/// @date 20080505
/// @brief read numerical text data with optimization and error-handling.
///        Implement mathematically provable perfect implementations of
///        high-speed data lexers for all possible text lexemes.
///        Test edge-cases with built-in unit tests.
/// This code assumes 8-bit bytes.
///////////////////////////////////////////////////////////////////////////////
///   COMPILATION: (Express built-in unit-tests)
/// g++ -DLETTVIN_LEXERS_H_CPP_UNIT -o goto_test goto_test.h.cpp
///////////////////////////////////////////////////////////////////////////////
///   EXAMPLE USAGE: (fixed specified width lexer)
///
/// char source[ ] = "1234";
/// char *s = source;
/// unsigned long long error, target;
///
/// target = Lettvin::lexDecU64_Instance( s, target, error, digits );
/// target = Lettvin::lexDecU64_Instance( s, target, error, digits, hi );
/// target = Lettvin::lexDecU64_Instance( s, target, error, digits, hi, lo );
///
/// where digits is the number of digits to process.
/// where hi and lo are the highest and lowest permitted value.
/// where s changes to point into source immediately following non-error input.
/// where error is a flag indicating error.
/// If error is already set, no lexing is done.
/// If lexed number falls outside hi lo range, s is unchanged.
/// See UnitTest examples below for edge case analysis.
///////////////////////////////////////////////////////////////////////////////
///   METHODS:
/// Pass/return reference prevents data copying and stack construction cost.
/// Jump table eliminates typical switch case cost.
/// JIT (Just-In-Time) jump table filling for first-time cost only.
/// Jcond use designed to have 0 clock cost due to locality and UV pipes.
/// Prefilled data tables enabling column summing with minimum cost.
///////////////////////////////////////////////////////////////////////////////
///   RESTRICTIONS:
/// Jump table implementation is dependent on g++ syntax/semantics.
///////////////////////////////////////////////////////////////////////////////
///   IMPLEMENTED:
/// lexDecU64t: decimal representation into unsigned long long
///////////////////////////////////////////////////////////////////////////////
#if 0
// EXPECTED OUTPUT:
///////////////////////////////////////////////////////////////////////////////
goto_test.h.cpp May  8 2008 10:41:59 UNIT TEST: starts Alternate style
Lettvin::u08t  1 ==  1
Lettvin::s08t  1 ==  1
Lettvin::u16t  2 ==  2
Lettvin::s16t  2 ==  2
Lettvin::u32t  4 ==  4
Lettvin::s32t  4 ==  4
Lettvin::f32t  4 ==  4
Lettvin::u64t  8 ==  8
Lettvin::s64t  8 ==  8
Lettvin::f64t  8 ==  8
                   IN                  BAD                  OUT  N E
                    0                               42951965936  1 0
                    1                                         1  1 0
                    2                                         2  1 0
                   10                                        10  2 0
                  900                                       900  3 0
                 8000                                      8000  4 0
                70000                                     70000  5 0
               600000                                    600000  6 0
              5000000                                   5000000  7 0
             40000000                                  40000000  8 0
            300000000                                 300000000  9 0
           2000000000                                2000000000 10 0
          10000000000                               10000000000 11 0
         900000000000                              900000000000 12 0
        8000000000000                             8000000000000 13 0
       70000000000000                            70000000000000 14 0
      600000000000000                           600000000000000 15 0
     5000000000000000                          5000000000000000 16 0
    40000000000000000                         40000000000000000 17 0
   300000000000000000                        300000000000000000 18 0
  2000000000000000000                       2000000000000000000 19 0
 10000000000000000000                      10000000000000000000 20 0
 18446744073709551615                      18446744073709551615 20 0
 18446744073709551616 18446744073709551616                    0  0 1
 20000000000000000000 20000000000000000000                    0  0 1
Illegal request for digit count > 20
Illegal request for digit count == 0
                    2                    2                    0  0 1
 18446744073709551615                      18446744073709551615 20 0
                    0                                         0  1 0
 18446744073709551615                      18446744073709551615 20 0
                    0                                         0  1 0
 18446744073709551615                      18446744073709551615 20 0
                    0                                         0  1 0
lexers.h.cpp May  8 2008 10:41:59 UNIT TEST: ends Alternate style
///////////////////////////////////////////////////////////////////////////////
#endif

#define Alternate 1	///< Identical logic, different appearance.

#ifndef LETTVIN_LEXERS_H_CPP
#define LETTVIN_LEXERS_H_CPP

#include <iostream>
#include <sstream>
#include <string>
#include <iomanip>
#include <exception>
#include <cstring>

namespace Lettvin {

#ifndef LETTVIN_TYPES
#define LETTVIN_TYPES
  /// @brief Types to support coding conventions/simplified parameter passing.
  ///        The regularity of the naming convention aids in quick analysis.
  ///        These types should be moved to a file.  Perhaps LettvinTypes.h
  ///        u =  unsigned
  ///        s =    signed
  ///       08 =    8 bits
  ///       16 =   16 bits
  ///       32 =   32 bits
  ///       64 =   64 bits
  ///       96 =   96 bits
  ///      128 =  128 bits
  ///        f =     float
  ///        t =      type
  ///        p =   pointer
  ///        r = reference
  typedef unsigned long long  u64t, *u64p, &u64r, *&u64pr;
  typedef unsigned       int  u32t, *u32p, &u32r, *&u32pr;
  typedef unsigned     short  u16t, *u16p, &u16r, *&u16pr;
  typedef unsigned      char  u08t, *u08p, &u08r, *&u08pr;

  typedef          long long  s64t, *s64p, &s64r, *&s64pr;
  typedef                int  s32t, *s32p, &s32r, *&s32pr;
  typedef              short  s16t, *s16p, &s16r, *&s16pr;
  typedef               char  s08t, *s08p, &s08r, *&s08pr;

/// On AMD64 long double is 128.  On WIN32 it is 96
//typedef        long double  f96t, *f96p, &f96r, *&f96pr; //96 or 128
//typedef        long double f128t,*f128p,&f128r,*&f128pr; //96 or 128
  typedef             double  f64t, *f64p, &f64r, *&f64pr;
  typedef              float  f32t, *f32p, &f32r, *&f32pr;

  /// Until the long double size issue is addressed, 10 lexers must be made.
  /// One each for u08t, s08t, u16t, s16t, u32t, s32t, u64t, s64t, f32t, f64t.
  /// After addressing long doubles, one lexer for each size will be made
  /// for each useful representation such as decimal, base36.
  /// Other representations such as binary, octal, and hexadecimal are ignored
  /// since their use in financial data has not yet been established in FE-DEV.

/// Examine sample output above to observe what this pair of macros do.
/// CONFIRM_DATA_SIZE  enforces that the data types have the specified size.
/// CONFIRM_DATA_SIZES performs enforcement over all relevant data types.
/// Note the u0 and s0 in the first two instances of CONFIRM_DATA_SIZE.
/// These compensates for OCTAL numbers beginning with 0 such that
/// a compiler sees the number 08 as a lexical error.
/// Also note that the t argument is required to append the 't' to each type.
#define CONFIRM_DATA_SIZE(b,w,t) \
	std::cout << \
          "Lettvin::" #b #w #t << " " << \
          std::setw( 2 ) << sizeof( Lettvin::b##w##t ) << \
          " == " << \
          std::setw( 2 ) << (w/8) << \
          std::endl; \
	if( sizeof( Lettvin::b##w##t ) != (w/8) ) \
          throw( "Size expectation violated." );
#define CONFIRM_DATA_SIZES \
        CONFIRM_DATA_SIZE(u0,8,t) \
        CONFIRM_DATA_SIZE(s0,8,t) \
        CONFIRM_DATA_SIZE(u,16,t) \
        CONFIRM_DATA_SIZE(s,16,t) \
        CONFIRM_DATA_SIZE(u,32,t) \
        CONFIRM_DATA_SIZE(s,32,t) \
        CONFIRM_DATA_SIZE(f,32,t) \
        CONFIRM_DATA_SIZE(u,64,t) \
        CONFIRM_DATA_SIZE(s,64,t) \
        CONFIRM_DATA_SIZE(f,64,t)
     ///CONFIRM_DATA_SIZE(f,96,t)
     ///CONFIRM_DATA_SIZE(f,128,t)

#endif//LETTVIN_TYPES

  class lexDecU64t {
    public:

      inline u64t &
      operator( )(
	  u64r ull,
	  s08pr s,
	  u64t &e,
	  const size_t &d = 20,
	  u64t r = top,
	  u64t b = zip
	  ) {
#ifndef __GNUC__
#else
        static void *DIGITS[ 256 ];	/// column-count jump table
	s08p o = s;

	/// Do not perform this function if a prior error has been detected.
	if( ( e |= ( d > 20 ) ) ) goto err;
	if( !DIGITS[ 0 ] ) goto fill;
        ull=zip;
	u64t t;				/// temporary column value
	//std::cout << std::endl << s << ":" << *s << std::endl;

run:	goto *( DIGITS[ d ] );
	///********************************************************************
	/// Failure is slightly more expensive than success, as it should be.
	/// since the JC, JNC family consume 0 clocks if branch is not taken.
	/// This is a property of Intel iAPX86 prefetch queues and UV pipes.
	/// So, once the initialization takes place, this is 0 cost.
	/// Calculate number for the precise number of digits requested.
	/// This table of addresses permits inexpensive bypassing of
	/// unnecessary branchings and calculations.
	/// Idea: accumulate digits in each x10 column of the target number.
	/// Breakdown of operations in the next 20 lines:
	/// s is a pointer to the current character to lex.
	/// *s fetches that character.
	/// col[ 17 ][ *s ] indexes into the 17th table of 256 u64t values.
	///********************************************************************
	/// HOW TO READ THESE LINES:
	/// If not error
	/// Check to see if this digit is forbidden in this column
	///   Any digit other than 0 or 1 in column index 19 is forbidden.
	///   This is because 0xFFFFFFFFFFFFFFFFULL == 18446744073709551615
	///   so the most significant digit must never be greater than 1.
	///   It is also because a non-digit should cause an error.
	/// If not, check to see if digit would overflow the remainder
	/// If not, reduce remainder, add digit in column and advance pointer.
	///********************************************************************
#if Alternate
#define DECU64COLUMN(n) \
c##n: e||(e|=inv[n][*s])||(e|=((t=col[n][*s])>r))||((r-=t),(ull+=t),(++s))
	DECU64COLUMN(19);
	DECU64COLUMN(18);
	DECU64COLUMN(17);
	DECU64COLUMN(16);
	DECU64COLUMN(15);
	DECU64COLUMN(14);
	DECU64COLUMN(13);
	DECU64COLUMN(12);
	DECU64COLUMN(11);
	DECU64COLUMN(10);
	DECU64COLUMN( 9);
	DECU64COLUMN( 8);
	DECU64COLUMN( 7);
	DECU64COLUMN( 6);
	DECU64COLUMN( 5);
	DECU64COLUMN( 4);
	DECU64COLUMN( 3);
	DECU64COLUMN( 2);
	DECU64COLUMN( 1);
	DECU64COLUMN( 0);
#else
c19:	e||(e|=inv[19][*s])||(e|=((t=col[19][*s])>r))||((r-=t),(ull+=t),(++s));
c18:	e||(e|=inv[18][*s])||(e|=((t=col[18][*s])>r))||((r-=t),(ull+=t),(++s));
c17:	e||(e|=inv[17][*s])||(e|=((t=col[17][*s])>r))||((r-=t),(ull+=t),(++s));
c16:	e||(e|=inv[16][*s])||(e|=((t=col[16][*s])>r))||((r-=t),(ull+=t),(++s));
c15:	e||(e|=inv[15][*s])||(e|=((t=col[15][*s])>r))||((r-=t),(ull+=t),(++s));
c14:	e||(e|=inv[14][*s])||(e|=((t=col[14][*s])>r))||((r-=t),(ull+=t),(++s));
c13:	e||(e|=inv[13][*s])||(e|=((t=col[13][*s])>r))||((r-=t),(ull+=t),(++s));
c12:	e||(e|=inv[12][*s])||(e|=((t=col[12][*s])>r))||((r-=t),(ull+=t),(++s));
c11:	e||(e|=inv[11][*s])||(e|=((t=col[11][*s])>r))||((r-=t),(ull+=t),(++s));
c10:	e||(e|=inv[10][*s])||(e|=((t=col[10][*s])>r))||((r-=t),(ull+=t),(++s));
c09:	e||(e|=inv[ 9][*s])||(e|=((t=col[ 9][*s])>r))||((r-=t),(ull+=t),(++s));
c08:	e||(e|=inv[ 8][*s])||(e|=((t=col[ 8][*s])>r))||((r-=t),(ull+=t),(++s));
c07:	e||(e|=inv[ 7][*s])||(e|=((t=col[ 7][*s])>r))||((r-=t),(ull+=t),(++s));
c06:	e||(e|=inv[ 6][*s])||(e|=((t=col[ 6][*s])>r))||((r-=t),(ull+=t),(++s));
c05:	e||(e|=inv[ 5][*s])||(e|=((t=col[ 5][*s])>r))||((r-=t),(ull+=t),(++s));
c04:	e||(e|=inv[ 4][*s])||(e|=((t=col[ 4][*s])>r))||((r-=t),(ull+=t),(++s));
c03:	e||(e|=inv[ 3][*s])||(e|=((t=col[ 3][*s])>r))||((r-=t),(ull+=t),(++s));
c02:	e||(e|=inv[ 2][*s])||(e|=((t=col[ 2][*s])>r))||((r-=t),(ull+=t),(++s));
c01:	e||(e|=inv[ 1][*s])||(e|=((t=col[ 1][*s])>r))||((r-=t),(ull+=t),(++s));
c00:	e||(e|=inv[ 0][*s])||(e|=((t=col[ 0][*s])>r))||((r-=t),(ull+=t),(++s));
#endif
	e |= ( ull < b );	///< See if value fell below minimum
	if( !e ) return ull;	///< This return bypasses any further execution
err:	s = o;			///< On failure, restore the pointer
	return ull = zip;	///< This return took extra instructions
fill:
	/// For the cost of expanding 20 instructions
	/// the initialization of the inlined jump table is available
	/// to be performed by every potential client for its own table.
	/// In the style of JIT compilation, this is performed
	/// the first time through any client call of this code.
	for( size_t i = 0; i < 256; ++i ) DIGITS[ i ] = &&err;
#if Alternate
#define DECU64DIGITS(n) \
	DIGITS[ n+1 ] = &&c##n
	DECU64DIGITS(19);
	DECU64DIGITS(18);
	DECU64DIGITS(17);
	DECU64DIGITS(16);
	DECU64DIGITS(15);
	DECU64DIGITS(14);
	DECU64DIGITS(13);
	DECU64DIGITS(12);
	DECU64DIGITS(11);
	DECU64DIGITS(10);
	DECU64DIGITS( 9);
	DECU64DIGITS( 8);
	DECU64DIGITS( 7);
	DECU64DIGITS( 6);
	DECU64DIGITS( 5);
	DECU64DIGITS( 4);
	DECU64DIGITS( 3);
	DECU64DIGITS( 2);
	DECU64DIGITS( 1);
	DECU64DIGITS( 0);
#else
	DIGITS[ 20 ] = &&c19;
	DIGITS[ 19 ] = &&c18;
	DIGITS[ 18 ] = &&c17;
	DIGITS[ 17 ] = &&c16;
	DIGITS[ 16 ] = &&c15;
	DIGITS[ 15 ] = &&c14;
	DIGITS[ 14 ] = &&c13;
	DIGITS[ 13 ] = &&c12;
	DIGITS[ 12 ] = &&c11;
	DIGITS[ 11 ] = &&c10;
	DIGITS[ 10 ] = &&c09;
	DIGITS[  9 ] = &&c08;
	DIGITS[  8 ] = &&c07;
	DIGITS[  7 ] = &&c06;
	DIGITS[  6 ] = &&c05;
	DIGITS[  5 ] = &&c04;
	DIGITS[  4 ] = &&c03;
	DIGITS[  3 ] = &&c02;
	DIGITS[  2 ] = &&c01;
	DIGITS[  1 ] = &&c00;
#endif

	goto run;
#endif
      }

#ifdef LETTVIN_LEXERS_H_CPP_UNIT
      bool UnitTestShow( const char *t, const size_t digits ) {
	/// Efficiency is not at a premium in this unit test service function.
	/// So the code for outputting no more than 20 digits is clumsy.
	if( digits > 20 ) {
	  std::cout << "Illegal request for digit count > 20" << std::endl;
	  return true;
	} else if( digits == 0 ) {
	  std::cout << "Illegal request for digit count == 0" << std::endl;
	  return true;
	} else {
	  char c = 0;
	  size_t i, j;
	  std::string spaces( 21 - digits, ' ' ); ///< 1 extra leading space
	  std::cout << spaces;
	  /// Output characters as long as they are valid and fill 20 cols.
	  for( i = j = 0; j < digits; i += !!c, ++j ) {
	    std::cout << ( ( c = t[ i ] ) ? c : ' ' );
	  }
	  return false;
	}
      }
      void UnitTest(
	  const char * s,
	  size_t digits = 20,
	  const u64t hi = top,
	  const u64t lo = zip )
      {
	u64t error = zip;
	u64t ull;
	char buffer[ 32 ];
	strcpy( buffer, s );
	char *t = buffer;

	/// 1) Output the ASCII representation as given as 21 characters.
	if( UnitTestShow( t, digits ) ) return;
	(*this)( ull, t, error, digits, hi, lo );
	/// 2) Output the remaining characters after the lex.
	/// 3) Output the converted number after the lex.
	/// 4) Output the number of characters converted.
	/// 5) Output the error flag (1 or 0)
	/// The style of output will resemble these two lines
///                   IN                   BAD                   OUT  N E
/// 18446744073709551616  18446744073709551616                     0  0 1
	if( UnitTestShow( t, digits ) ) return;
	std::cout <<
	  std::setw( 21 ) <<        ull << " " <<
	  std::setw(  2 ) << (t-buffer) << " " <<
	  error <<
	  std::endl;
      }

      void UnitTest( const u64t &val ) {
	std::stringstream ss;
	ss << val; UnitTest( ss.str( ).c_str( ), ss.str( ).size( ) );
      }

      void UnitTest( ) {
	const char * test[ ] = {
	  "0",
	  "1",
	  "2",
	  "10",
	  "900",
	  "8000",
	  "70000",
	  "600000",
	  "5000000",
	  "40000000",
	  "300000000",
	  "2000000000",
	  "10000000000",
	  "900000000000",
	  "8000000000000",
	  "70000000000000",
	  "600000000000000",
	  "5000000000000000",
	  "40000000000000000",
	  "300000000000000000",
	  "2000000000000000000",
	  "10000000000000000000",	///< good
	  "18446744073709551615",	///< good (one less than bad)
	  "18446744073709551616",	///< bad, too high value
	  "20000000000000000000",	///< bad, too high value
	  "300000000000000000000",	///< bad, too many digits
	  "",				///< bad, too  few digits
	  0L				// terminate with null ptr
	};

	std::cout <<
	  std::setw( 21 ) <<  "IN" <<
	  std::setw( 21 ) << "BAD" <<
	  std::setw( 21 ) << "OUT" <<
	  "  N E" << std::endl;

	for( size_t i = 0; test[ i ]; ++i ) {
	  UnitTest( test[ i ], strlen( test[ i ] ) );
	}
	UnitTest( test[ 2 ], strlen( test[ 2 ] ), top, 3ULL );

	u64t val;
        val  = top; UnitTest( val );
	val += one; UnitTest( val );
	val -= one; UnitTest( val );
	val  = zip; UnitTest( val );
	val -= one; UnitTest( val );	///< This wraps around.  No error.
	val += one; UnitTest( val );

      }
#endif

    private:
  ///##########################################################################
      static const u64t zip =  0ULL;
      static const u64t one =  1ULL;
      static const u64t ten = 10ULL;
      static const u64t top = 0xFFFFFFFFFFFFFFFFULL;

      static const u64t p0 =      one;
      static const u64t p1 =      ten;
      static const u64t p2 = p1 * ten;
      static const u64t p3 = p2 * ten;
      static const u64t p4 = p3 * ten;
      static const u64t p5 = p4 * ten;
      static const u64t p6 = p5 * ten;
      static const u64t p7 = p6 * ten;
      static const u64t p8 = p7 * ten;
      static const u64t p9 = p8 * ten;
      static const u64t pA = p9 * ten;
      static const u64t pB = pA * ten;
      static const u64t pC = pB * ten;
      static const u64t pD = pC * ten;
      static const u64t pE = pD * ten;
      static const u64t pF = pE * ten;
      static const u64t pG = pF * ten;
      static const u64t pH = pG * ten;
      static const u64t pI = pH * ten;
      static const u64t pJ = pI * ten;

      static const bool inv[ 20 ][ 256 ];	///< Invalid character table
      static const u64t col[ 20 ][ 256 ];	///< column-value lookup table
  };

  ///PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
  /// These defines are used to fill the col table with BAD values
#define B00000010(x) x,x
#define B00000100(x) B00000010(x), B00000010(x)
#define B00001000(x) B00000100(x), B00000100(x)
#define B00010000(x) B00001000(x), B00001000(x)
#define B00100000(x) B00010000(x), B00010000(x)
#define B01000000(x) B00100000(x), B00100000(x)
#define B10000000(x) B01000000(x), B01000000(x)
#define B00110000(x) \
        B00100000(x),\
        B00010000(x)
#define B11000110(x) \
        B10000000(x),\
        B01000000(x),\
        B00000100(x),\
        B00000010(x)
#define Bad48   B00110000(zip)
#define Bad198  B11000110(zip)
#define True48  B00110000(false)
#define True198 B11000110(false)

  ///##########################################################################
  /// Table of digit validity for each column
  const bool lexDecU64t::inv[ 20 ][ 256 ] = {
#if Alternate
#define DECU64INVTABLE(a) \
    { True48, false,false,B00001000(a),True198}
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE(false),
    DECU64INVTABLE( true)	///< highest column has limited digits
#else
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000(false),True198},
    { True48, false,false,B00001000( true),True198}
#endif
  };

  ///##########################################################################
  /// Table of summable value for each column
  const u64t lexDecU64t::col[ 20 ][ 256 ] = {
#if Alternate
#define DECU64COLTABLE(n) \
{Bad48,0,1*p##n,2*p##n,3*p##n,4*p##n,5*p##n,6*p##n,7*p##n,8*p##n,9*p##n, Bad198}
    DECU64COLTABLE(0),
    DECU64COLTABLE(1),
    DECU64COLTABLE(2),
    DECU64COLTABLE(3),
    DECU64COLTABLE(4),
    DECU64COLTABLE(5),
    DECU64COLTABLE(6),
    DECU64COLTABLE(7),
    DECU64COLTABLE(8),
    DECU64COLTABLE(9),
    DECU64COLTABLE(A),
    DECU64COLTABLE(B),
    DECU64COLTABLE(C),
    DECU64COLTABLE(D),
    DECU64COLTABLE(E),
    DECU64COLTABLE(F),
    DECU64COLTABLE(G),
    DECU64COLTABLE(H),
    DECU64COLTABLE(I),
    DECU64COLTABLE(J)
#else
    { Bad48, 0, 1*p0,2*p0,3*p0,4*p0,5*p0,6*p0,7*p0,8*p0,9*p0, Bad198 },
    { Bad48, 0, 1*p1,2*p1,3*p1,4*p1,5*p1,6*p1,7*p1,8*p1,9*p1, Bad198 },
    { Bad48, 0, 1*p2,2*p2,3*p2,4*p2,5*p2,6*p2,7*p2,8*p2,9*p2, Bad198 },
    { Bad48, 0, 1*p3,2*p3,3*p3,4*p3,5*p3,6*p3,7*p3,8*p3,9*p3, Bad198 },
    { Bad48, 0, 1*p4,2*p4,3*p4,4*p4,5*p4,6*p4,7*p4,8*p4,9*p4, Bad198 },
    { Bad48, 0, 1*p5,2*p5,3*p5,4*p5,5*p5,6*p5,7*p5,8*p5,9*p5, Bad198 },
    { Bad48, 0, 1*p6,2*p6,3*p6,4*p6,5*p6,6*p6,7*p6,8*p6,9*p6, Bad198 },
    { Bad48, 0, 1*p7,2*p7,3*p7,4*p7,5*p7,6*p7,7*p7,8*p7,9*p7, Bad198 },
    { Bad48, 0, 1*p8,2*p8,3*p8,4*p8,5*p8,6*p8,7*p8,8*p8,9*p8, Bad198 },
    { Bad48, 0, 1*p9,2*p9,3*p9,4*p9,5*p9,6*p9,7*p9,8*p9,9*p9, Bad198 },
    { Bad48, 0, 1*pA,2*pA,3*pA,4*pA,5*pA,6*pA,7*pA,8*pA,9*pA, Bad198 },
    { Bad48, 0, 1*pB,2*pB,3*pB,4*pB,5*pB,6*pB,7*pB,8*pB,9*pB, Bad198 },
    { Bad48, 0, 1*pC,2*pC,3*pC,4*pC,5*pC,6*pC,7*pC,8*pC,9*pC, Bad198 },
    { Bad48, 0, 1*pD,2*pD,3*pD,4*pD,5*pD,6*pD,7*pD,8*pD,9*pD, Bad198 },
    { Bad48, 0, 1*pE,2*pE,3*pE,4*pE,5*pE,6*pE,7*pE,8*pE,9*pE, Bad198 },
    { Bad48, 0, 1*pF,2*pF,3*pF,4*pF,5*pF,6*pF,7*pF,8*pF,9*pF, Bad198 },
    { Bad48, 0, 1*pG,2*pG,3*pG,4*pG,5*pG,6*pG,7*pG,8*pG,9*pG, Bad198 },
    { Bad48, 0, 1*pH,2*pH,3*pH,4*pH,5*pH,6*pH,7*pH,8*pH,9*pH, Bad198 },
    { Bad48, 0, 1*pI,2*pI,3*pI,4*pI,5*pI,6*pI,7*pI,8*pI,9*pI, Bad198 },
    { Bad48, 0, 1*pJ,2*pJ,3*pJ,4*pJ,5*pJ,6*pJ,7*pJ,8*pJ,9*pJ, Bad198 }
#endif
  };

  static lexDecU64t lexDecU64_Instance;
}

#ifdef LETTVIN_LEXERS_H_CPP_UNIT
///****************************************************************************
int main( int argc, char *argv[ ] ) {
  int retval = 1;
  const char *whoami = __FILE__ " " __DATE__ " " __TIME__ " ";

  std::cout <<
    whoami <<
    "UNIT TEST: starts " <<
    (Alternate?"Alternate style":"Basic style") <<
    std::endl;

  try {
    CONFIRM_DATA_SIZES;
    Lettvin::lexDecU64_Instance.UnitTest( );
    retval = 0;
  }
#if 0
  catch(domain_error & e) {
    std::cerr << std::string("domain_error: ") + e.what() << std::endl;
  }
  catch(std::invalid_argument & e) {
    std::cerr << std::string("invalid_argument: ") + e.what() << std::endl;
  }
  catch(std::length_error & e) {
    std::cerr << std::string("length_error: ") + e.what() << std::endl;
  }
  catch(std::out_of_range & e) {
    std::cerr << std::string("out_of_range: ") + e.what() << std::endl;
  }
  catch(std::bad_cast & e) {
    std::cerr << std::string("bad_cast: ") + e.what() << std::endl;
  }
  catch(std::bad_typeid & e) {
    std::cerr << std::string("bad_typeid: ") + e.what() << std::endl;
  }
  catch(std::range_error & e) {
    std::cerr << std::string("range_error: ") + e.what() << std::endl;
  }
  catch(std::overflow_error & e) {
    std::cerr << std::string("overflow_error: ") + e.what() << std::endl;
  }
  catch(std::underflow_error & e) {
    std::cerr << std::string("underflow_error: ") + e.what() << std::endl;
  }
  catch(std::bad_alloc & e) {
    std::cerr << std::string("bad_alloc: ") + e.what() << std::endl;
  }
  catch(std::logic_error & e) {
    std::cerr << std::string("logic_error: ") + e.what() << std::endl;
  }
  catch(std::runtime_error & e) {
    std::cerr << std::string("runtime_error: ") + e.what() << std::endl;
  }
#endif
  catch(std::exception & e) {
    std::cerr <<
      whoami <<
      std::string("generic exept: ") + e.what() <<
      std::endl;
  }
  catch( const Lettvin::s08pr s ) {               ///< catch known errors
    std::cout << whoami << "Error: " << s << std::endl;
  }
  catch( const std::string &s ) {               ///< catch known errors
    std::cout << whoami << "Error: " << s << std::endl;
  }
  catch( ... ) {                                ///< catch unknown errors
    std::cout << whoami << "Error: unknown" << std::endl;
  }

  std::cout <<
    whoami <<
    "UNIT TEST: ends " <<
    (Alternate?"Alternate style":"Basic style") <<
    std::endl;


  return retval;
}

#endif//LETTVIN_LEXERS_H_CPP_UNIT
#endif//LETTVIN_LEXERS_H_CPP
///****************************************************************************
/// goto_test.h.cpp EOF
///****************************************************************************

## Floating-point constants
  
  
    Floating-point constants are positive or negative numbers and can
  contain fractions. Fractional parts of a number may be shown using a
  decimal point. Once again remember that commas are NOT used between
  numbers. If the plus sign (+) is left off the front of a number, the
  Commodore 64 assumes that the number is positive. If you leave off the
  decimal point the computer will assume that it follows the last digit of
  the number. And as with integers, zeros that come before a constant
  are ignored. Floating-point constants can be used in two ways:

    1) SIMPLE NUMBER
    2) SCIENTIFIC NOTATION

    Floating-point constants will show you up to nine digits on your
  screen. These digits can represent values between -999999999. and
  +999999999. If you enter more than nine digits the number will be
  rounded based on the tenth digit. if the tenth digit is greater than or
  equal to 5 the number will be rounded upward. Less than 5 the number
  be rounded downward. This could be important to the final totals of
  some numbers you may want to work with.
    Floating-point numbers are stored (using five bytes of memory) and
  are manipulated in calculations with ten places of accuracy. However,

                                                BASIC PROGRAMMING RULES   5
~


  the numbers are rounded to nine digits when results are printed. Some
  examples of simple floating-point numbers are:

                  1.23                 .7777777
                  -.998877         -333.
                 +3.1459               .01


    Numbers smaller than .01 or larger than 999999999. will be printed in
  scientific notation. In scientific notation a floating-point constant is
  made up of three parts:

    1) THE MANTISSA
    2) THE LETTER E
    3) THE EXPONENT

    The mantissa is a simple floating-point number. The letter E is used to
  tell you that you're seeing the number in exponential form. In other
  words E represents * 10 (eg., 3E3 = 3*10^3 = 3000). And the exponent is
  what multiplication power of 10 the number is raised to.
    Both the mantissa and the exponent are signed (+ or -) numbers. The
  exponent's range is from -39 to +38 and it indicates the number of places
  that the actual decimal point in the mantissa would be moved to the left
  (-) or right (+) if the value of the constant were represented as a
  simple number.
    There is a limit to the size of floating-point numbers that BASIC can
  handle, even in scientific notation: the largest number is
  +1.70141183E+38 and calculations which would result in a larger number
  will display the BASIC error message ?OVERFLOW ERROR. The smallest
  floating-point number is +2.93873588E-39 and calculations which result
  in a smaller value give you zero as an answer and NO error message. Some
  examples of floating-point numbers in scientific notation (and their
  decimal values) are:

 	235.988E-3	(.235988)
        2359E6          (2359000000.)
        -7.09E-12       (-.00000000000709)
	-3.14159E+5	(-314159.)


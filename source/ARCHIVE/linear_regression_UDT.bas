'linear regression UDT

#include "fbgfx.bi"

const as integer    scrW => 600
const as integer    scrH => 450
randomize TIMER


type XYFEEDER
   'allow linear regression on a static subset
    declare constructor()
    declare constructor(byval as integer, byval as integer)
    declare operator let(byval as integer)
    declare sub AddInstanceToArray()
    declare sub DrawRegLine()
    declare sub DrawRegLineAt(byval as integer, byval as integer)
        as integer  _x
        as integer  _y
    declare static sub CleanUpArray()
    declare static function RefreshSum() as boolean
    declare static function RefreshMean() as boolean
    declare static function ComputeLinearRegression() as boolean
    static as integer       sumX
    static as integer       sumY
    static as double      meanX
    static as double      meanY
    static as double        beta0
    static as double        slope
    static as double        regQualityParameter
    static as integer       arrayCount
    static as XYFEEDER ptr  xyFeederArrayOfPtr(any)
end type
dim as integer      XYFEEDER.sumX
dim as integer      XYFEEDER.sumY
dim as double      XYFEEDER.meanX
dim as double      XYFEEDER.meanY
dim as double      XYFEEDER.beta0
dim as double      XYFEEDER.slope
dim as double      XYFEEDER.regQualityParameter
dim as integer      XYFEEDER.arrayCount
dim as XYFEEDER ptr   XYFEEDER.xyFeederArrayOfPtr(any)
constructor XYFEEDER()
    with THIS
        ._x => 0
        ._y => 0
    end with
end constructor
constructor XYFEEDER(byval X as integer, byval Y as integer)
    with THIS
        ._x => X
        ._y => Y
    end with
end constructor
operator XYFEEDER.let(byval LetValue as integer)
    THIS._x = LetValue
    THIS._y = LetValue
end operator
sub XYFEEDER.AddInstanceToArray()
    XYFEEDER.arrayCount += 1
    redim preserve _
    XYFEEDER.xyFeederArrayOfPtr(uBound(XYFEEDER.xyFeederArrayOfPtr) + 1)
    XYFEEDER.xyFeederArrayOfPtr(uBound(XYFEEDER.xyFeederArrayOfPtr)) => @THIS
end sub
sub XYFEEDER.DrawRegLine()
   line (THIS._x, THIS._y)-_
       (THIS._x + 20, XYFEEDER.beta0 + XYFEEDER.slope*(THIS._x + 20)), _
       rgb(100,230,100)
end sub
sub XYFEEDER.DrawRegLineAt(byval X as integer, byval Y as integer)
   line (X, Y)-_
       (X + 180, XYFEEDER.beta0 + XYFEEDER.slope*(X + 180)), _
       rgb(200,230,100)
end sub
sub XYFEEDER.CleanUpArray()
    erase XYFEEDER.xyFeederArrayOfPtr
    XYFEEDER.arrayCount = 0
end sub
function XYFEEDER.RefreshSum() as boolean
   dim as boolean  returnValue
   '
    XYFEEDER.sumX = 0
    XYFEEDER.sumY = 0
    for index as integer = lBound(XYFEEDER.xyFeederArrayOfPtr) to _
                        uBound(XYFEEDER.xyFeederArrayOfPtr)
        sumX += XYFEEDER.xyFeederArrayOfPtr(index)->_x
        sumY += XYFEEDER.xyFeederArrayOfPtr(index)->_y
    next index
    '---->
    return returnValue
end function
function XYFEEDER.RefreshMean() as boolean
   dim as boolean  returnValue
   '
   XYFEEDER.RefreshSum()
   XYFEEDER.meanX = XYFEEDER.sumX/XYFEEDER.arrayCount
   XYFEEDER.meanY = XYFEEDER.sumY/XYFEEDER.arrayCount
    '---->
    return returnValue
end function
function XYFEEDER.ComputeLinearRegression() as boolean
    dim as boolean  returnValue
    XYFEEDER.RefreshMean()
    dim as double   num => 0
    for index as integer = lBound(XYFEEDER.xyFeederArrayOfPtr) to uBound(XYFEEDER.xyFeederArrayOfPtr)
       num += _
       (XYFEEDER.xyFeederArrayOfPtr(index)->_x - XYFEEDER.meanX)*XYFEEDER.xyFeederArrayOfPtr(index)->_y
    next index
    dim as double   den => 0
    for index as integer = lBound(XYFEEDER.xyFeederArrayOfPtr) to uBound(XYFEEDER.xyFeederArrayOfPtr)
       den += _
       (XYFEEDER.xyFeederArrayOfPtr(index)->_x - XYFEEDER.meanX)^2
    next index
    XYFEEDER.slope = num/den
    XYFEEDER.beta0 = XYFEEDER.meanY - XYFEEDER.slope*XYFEEDER.meanX
   '
   num = 0
    for index as integer = lBound(XYFEEDER.xyFeederArrayOfPtr) to uBound(XYFEEDER.xyFeederArrayOfPtr)
       num += (XYFEEDER.xyFeederArrayOfPtr(index)->_x - XYFEEDER.meanX)* _
             (XYFEEDER.xyFeederArrayOfPtr(index)->_y - XYFEEDER.meanY)
    next index   
   dim as double den1 => den
   dim as double den2 => 0
    for index as integer = lBound(XYFEEDER.xyFeederArrayOfPtr) to uBound(XYFEEDER.xyFeederArrayOfPtr)
       den2 += _
       (XYFEEDER.xyFeederArrayOfPtr(index)->_y - XYFEEDER.meanY)^2
    next index
   XYFEEDER.regQualityParameter = num/sqr(den1* den2)
    '---->
    return returnValue
end function


'___------------------------------------------------------------___
'___------------------------------------------------------------___
screenRes scrW, scrH, 32

dim as XYFEEDER       xy(1 to 10)

for index as integer = 1 to 10
   with xy(index)
      ._x => index*50
      ._y => scrH/2 + rnd()*(scrH/8) + 4*index
      circle (._x, ._y), 4
      xy(index).AddInstanceToArray()
      XYFEEDER.ComputeLinearRegression()
      xy(index).DrawRegLine()
      ? "beta0", XYFEEDER.beta0, "r", XYFEEDER.regQualityParameter
      draw string (._x - 8, ._y - 10), str(index) &","& left(str(XYFEEDER.slope*100), 4)
   end with
next index

xy(1).DrawRegLineAt(0, XYFEEDER.beta0)
circle (0, XYFEEDER.beta0), 8
circle (0, XYFEEDER.beta0), 4

XYFEEDER.CleanUpArray()

getKey()

'(eof)
#include once "file.bi"
#include "string.bi"
/'
  Number of claims
  Total payment for all the claims in thousands of Swedish Kronor
  for geographical zones in Sweden
'/
type InsuranceData
  as single _
    numberOfClaims, _
    totalPayment
end type

type InsuranceTable
  declare operator []( byval as uinteger ) byref as InsuranceData
  as InsuranceData row( any )
  as uinteger count
end type

TYPE COEFFICI
   AS DOUBLE _
   b0, _
   b1
END TYPE



DIM SHARED coeffici1 AS COEFFICI
DIM SHARED coefficiReturn AS COEFFICI

operator InsuranceTable.[]( byval index as uinteger ) byref as InsuranceData
  return( row( index ) )
end operator

sub add overload( byref t as InsuranceTable, byref d as InsuranceData )
  t.count += 1
  redim preserve t.row( 0 to t.count - 1 )
  
  t.row( t.count - 1 ) = d
end sub

function loadDataset( byref path as const string ) as InsuranceTable
  dim as InsuranceTable t
  
  if( fileExists( path ) ) then
    dim as long f = freeFile()
    
    open path for input as f
    
    do while( not eof( f ) )
      dim as InsuranceData d
      
      input #f, d.numberOfClaims
      input #f, d.totalPayment
      
      add( t, d )
    loop
  end if
  
  return( t )
end function

'SUB iAppend(arr() AS DOUBLE, item AS DOUBLE)
'   REDIM PRESERVE arr(LBOUND(arr) TO UBOUND(arr) +1)
'   arr(UBOUND(arr)) = item
'END SUB

SUB iAppend(arr() AS DOUBLE, item AS DOUBLE)
    dim as integer lbnd = LBOUND(arr), ubnd =  UBOUND(arr)
    REDIM PRESERVE arr(lbnd TO ubnd+1)
    arr(ubnd+1) = item
END SUB

' Calculate the mean value of a list of numbers
function sum(x() as double) as double
  dim as single result
  for i as integer = 0 to ubound(x) - 1
    result = result + x(i)
  next i
  return result
end FUNCTION

function sum2(x() as DOUBLE, mean2 AS DOUBLE) as double
  dim as single result
  for i as integer = 0 to ubound(x) - 1
    result = result + x(i) - mean2
  next i
  return result
end FUNCTION

'calculate mean
function mean(x() as double) as double
  return sum(x()) / cdbl(ubound(x) + 1)
end FUNCTION


' Calculate the variance of a list of numbers
function variance(values() AS double, BYVAL means AS DOUBLE) AS DOUBLE
   DIM resalt AS DOUBLE = 0
   FOR i AS INTEGER = LBOUND(values) TO UBOUND(values)
      resalt = resalt + (values(i) - means) * (values(i) - means)
   NEXT i
   Return resalt
END FUNCTION

FUNCTION covariance(x()as double, mean_x as double, y() as double, mean_y as double) as Double
    dim covar as Double
    for i as integer = 0 to UBOUND(x) - 1
        covar += (x(i) - mean_x) * (y(i) - mean_y)
    next
    return covar
end FUNCTION
' calculate cofficiants

'FUNCTION COEFFICIENTSb0 (x() AS DOUBLE,mean_x AS DOUBLE, y() AS DOUBLE, mean_y AS DOUBLE) AS DOUBLE
'   DIM coeffici AS COEFFICI
'   mean_x = MEAN(x())
'   mean_y = MEAN(y())
'   WITH coeffici
'      .b1 = COVARIANCE(x(), mean_x, y(), mean_y) / VARIANCE(x(), mean_x)
'      .b0 = mean_y - .b1 * mean_x
'   RETURN .b0
'   END WITH
'   
'END FUNCTION

SUB COEFFICIENTS (x() AS DOUBLE,mean_x AS DOUBLE, y() AS DOUBLE, mean_y AS DOUBLE)
   
   mean_x = MEAN(x())
   mean_y = MEAN(y())
   WITH coeffici1
      coeffici1.b1 = COVARIANCE(x(), mean_x, y(), mean_y) / VARIANCE(x(), mean_x)
      coeffici1.b0 = mean_y - .b1 * mean_x
   'RETURN coeffici1
   END WITH
   
END SUB
 
FUNCTION rmse_meteric(actual() AS DOUBLE, predicted() AS DOUBLE) AS DOUBLE
   DIM sum_error AS DOUBLE
   DIM prediction_error AS DOUBLE
   DIM mean_error AS DOUBLE  
   for i AS INTEGER = 0 to len(actual)-1
      prediction_error = predicted(i) - actual(i)
      sum_error += (prediction_error ^ 2)
   NEXT
   mean_error = sum_error / len(actual)
   return sqr(mean_error)
END FUNCTION
 
 
 REDIM SHARED test_set_x(0) AS DOUBLE
 REDIM SHARED test_set_y(0) AS DOUBLE

TYPE function_type AS FUNCTION(() As DOUBLE, () AS DOUBLE, () AS DOUBLE, () AS DOUBLE) As DOUBLE

FUNCTION elvaluate_algo(x() AS DOUBLE, y() AS DOUBLE, BYVAL algorithem AS function_type) AS DOUBLE
   
   FOR i AS INTEGER = 0 TO UBOUND(x) - 1
      IAPPEND test_set_x(), x(i)
      IAPPEND test_set_y(), y(i)
   NEXT
    REDIM actual(0) AS DOUBLE
    DIM AS DOUBLE predicted = algorithem(x(),y(), test_set_x(), test_set_y())
   FOR i AS INTEGER = 0 TO UBOUND(y) - 1
      
   NEXT
END FUNCTION

FUNCTION simple_linear_regression(train() AS DOUBLE, test() AS DOUBLE) AS DOUBLE
   REDIM prediction(0) AS DOUBLE
   
END FUNCTION


var t = loadDataset( "D:\repo\FB_libLINREG\datasets\test.csv" )

REDIM SHARED x(0) AS DOUBLE
REDIM SHARED y(0) AS DOUBLE

DIM AS DOUBLE mean_x, mean_y, covar

for i as integer = 0 to t.count - 1
  
  with t[ i ]
     
     IAPPEND x(), CDBL(.numberOfClaims)
     IAPPEND y(),  CDBL(.totalPayment)
    
  end WITH
  WITH t [ i ]
     
      ? .numberOfClaims, "means:", format(MEAN(x()), "0.00"), .totalPayment,  "means: ", format(MEAN(y()), "0.00")
  
  END WITH
NEXT


   ?  "convariance: ", format(COVARIANCE(x(), mean(x()), y(), mean(y())), "0.00")
   
      mean_x = MEAN(x())
      mean_y = MEAN(y())
      covar = COVARIANCE(x(), mean_x, y(), mean_y)
      
   ? "X colume:", FORMAT(mean_x,"0.00"), "Y colume:", FORMAT(mean_y, "0.00"), "CONVARIANCE:", FORMAT(covar, "0.00")

   ? "varients x:", FORMAT(VARIANCE(x(),mean_x), "0.00"), "VARIANCE y:",  FORMAT(VARIANCE(y(), mean_y), "0.00")
   'WITH coefficiReturn
   COEFFICIENTS(x(), mean_x, y(), mean_y)
   ? "COEFFICIENTS:", "b0: " & coeffici1.b0, "b1: " & coeffici1.b1 
   'END with
sleep()
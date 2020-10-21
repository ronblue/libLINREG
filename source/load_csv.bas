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

function mean(x() as double) as double
  return sum(x()) / cdbl(ubound(x) + 1)
end FUNCTION

function covariance(x()as double, mean_x as double, y() as double, mean_y as double) as Double
    dim covar as Double
    for i as integer = 0 to UBOUND(x) - 1
        covar += (x(i) - mean_x) * (y(i) - mean_y)
    next
    return covar
end FUNCTION

var t = loadDataset( "D:\repo\FB_libLINREG\datasets\dataset.csv" )
REDIM SHARED x(0) AS DOUBLE
REDIM SHARED y(0) AS DOUBLE
for i as integer = 0 to t.count - 1
  
  with t[ i ]
     
     IAPPEND x(), CDBL(.numberOfClaims)
     IAPPEND y(),  CDBL(.totalPayment)
    
  end WITH
  WITH t [ i ]
     
      ? .numberOfClaims, "means:", format(MEAN(x()), "0.00"), .totalPayment,  "varients: ", format(MEAN(y()), "0.00")
  
  END WITH
NEXT


   ?  "convariance: ", format(COVARIANCE(x(), mean(x()), y(), mean(y())), "0.00")


sleep()
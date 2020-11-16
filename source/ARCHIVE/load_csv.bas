#include once "file.bi"

/'
  Number of claims
  Total payment for all the claims in thousands of Swedish Kronor
  for geographical zones in Sweden
'/
type InsuranceData
  as single numberOfClaims, totalPayment
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
    LOOP
    CLOSE #f
  end if
  
  return( t )
end function

var t = loadDataset( "D:\repo\FB_libLINREG\datasets\dataset.csv" )

for i as integer = 0 to t.count - 1
  with t[ i ]
    ? .numberOfClaims, .totalPayment
  end with
next

sleep()
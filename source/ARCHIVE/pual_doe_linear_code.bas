#include once "fbgfx.bi"

const as double _
  MIN_DBL = 4.940656458412465E-324, _
  MAX_DBL = 1.797693134862316E+308

enum Colors
  White = rgba( 255, 255, 255, 255 )
  Black = rgba( 0, 0, 0, 255 )
  Red = rgba( 205, 80, 80, 255 )
  LightBlue = rgba( 130, 182, 208, 255 )
  LightGray = rgba( 214, 214, 214, 0 )
end enum

type Rect
  declare constructor()
  declare constructor( _
    byval as double, byval as double, byval as double, byval as double )
 
  as double x, y, w, h
end type

constructor Rect() : end constructor
constructor Rect( _
  byval nX as double, byval nY as double, _
  byval nW as double, byval nH as double )
 
  x = nX : y = nY
  w = nW : h = nH
end constructor

''' Linear regression stuff

type Values
  declare operator cast() as string
  declare operator []( byval as integer ) byref as double
 
  as double _values( any )
  as integer count
end type

operator Values.cast() as string
  dim as string s
 
  for i as integer = 0 to count - 1
    s += str( _values( i ) ) + iif( i < count - 1, ",", chr( 13, 10 ) )
  next
 
  return( s )
end operator

operator Values.[]( byval index as integer ) byref as double
  return( _values( index ) )
end operator

type Dataset
  declare operator cast() as string
  declare operator []( byval as integer ) byref as Values
 
  as Values _values( any )
  as integer count
end type

operator Dataset.cast() as string
  dim as string s = ""
 
  for i as integer = 0 to count - 1
    s += _values( i )
  next
 
  return( s )
end operator

operator Dataset.[]( byval index as integer ) byref as Values
  return( _values( index ) )
end operator

function add overload( byref ds as Values, byval v as double ) byref as Values
  ds.count += 1
  redim preserve ds._values( 0 to ds.count - 1 )
  ds._values( ds.count - 1 ) = v
 
  return( ds )
end function

function add( byref ds as Dataset, byref v as Values ) byref as Dataset
  ds.count += 1
  redim preserve ds._values( 0 to ds.count - 1 )
  ds._values( ds.count - 1 ) = v
 
  return( ds )
end function

type Coefs
  declare constructor()
  declare constructor( byval as double, byval as double )
 
  declare operator cast() as string
 
  as double b0, b1
end type

constructor Coefs() : end constructor
constructor Coefs( byval cB0 as double, byval cB1 as double )
  b0 = cB0 : b1 = cB1
end constructor

operator Coefs.cast() as string
  return( "B0=" & b0 & ",B1=" & b1 )
end operator

private function max overload( byval a as double, byval b as double ) as double
  return( iif( a > b, a, b ) )
end function

function max( byref ds as Values ) as double
  dim as double value = MIN_DBL
 
  for i as integer = 0 to ds.count - 1
    value = iif( ds[ i ] > value, ds[ i ], value )
  next
 
  return( value )
end function

private function min overload( byval a as double, byval b as double ) as double
  return( iif( a < b, a, b ) )
end function

function min( byref ds as Values ) as double
  dim as double value = MAX_DBL
 
  for i as integer = 0 to ds.count - 1
    value = iif( ds[ i ] < value, ds[ i ], value )
  next
 
  return( value )
end function

function mean( byref x as Values ) as double
  dim as double sum = 0.0d
 
  for i as integer = 0 to x.count - 1
    sum += x[ i ]
  next
 
  return( sum / x.count )
end function

function variance( byref x as Values, byval mean_x as double ) as double
  dim as double sum = 0.0d
 
  for i as integer = 0 to x.count - 1
    sum += ( x[ i ] - mean_x ) ^ 2
  next
 
  return( sum )
end function

function covariance( _
  byref x as Values, byval mean_x as double, _
  byref y as Values, byval mean_y as double ) as double
 
  dim as double covar = 0.0d
 
  for i as integer = 0 to min( x.count, y.count ) - 1
    covar += ( x[ i ] - mean_x ) * ( y[ i ] - mean_y )
  next
 
  return( covar )
end function

function coefficients( byref x as Values, byref y as Values ) as Coefs
  dim as double _
    mean_x = mean( x ), mean_y = mean( y ), _
    b1 = covariance( x, mean_x, y, mean_y ) / variance( x, mean_x ), _
    b0 = mean_y - b1 * mean_x
 
  return( Coefs( b0, b1 ) )
end function

function rmse_metric( byref actual as Values, byref predicted as Values ) as double
  dim as double sum_error = 0.0d
 
  for i as integer = 0 to actual.count - 1
    sum_error += ( predicted[ i ] - actual[ i ] ) ^ 2
  next
 
  return( sqr( sum_error / actual.count ) )
end function

type as function( byref as Dataset, byref as Values ) as Values _
  Algorithm

function evaluate_algorithm( _
  byref ds as Dataset, byval algorithm_func as Algorithm ) as Values
 
  dim as Values test_set = ds[ 0 ]
 
  return( algorithm_func( ds, test_set ) )
end function

function simple_linear_regression( _
  byref train as Dataset, byref test as Values ) as Values
 
  dim as Values predictions
  var c = coefficients( train[ 0 ], train[ 1 ] )
 
  for i as integer = 0 to test.count - 1
    add( predictions, c.b0 + c.b1 * test[ i ] )
  next
 
  return( predictions )
end function

'''

''' Visualization stuff
private function remap( _
    byval x as double, _
    byval start1 as double, _
    byval end1 as double, _
    byval start2 as double, _
    byval end2 as double ) _
  as double
 
  return( ( x - start1 ) * _
    ( end2 - start2 ) / ( end1 - start1 ) + start2 )
end function

sub drawRect( byref r as Rect, byval c as ulong )
  line( r.x, r.y ) - ( r.x + r.w - 1, r.y + r.h - 1 ), c, b
end sub

sub plot overload( _
  byref r as Rect, _
  byref xA as Values, byref yA as Values, _
  byval minX as double, byval maxX as double, _
  byval minY as double, byval maxY as double, _
  byval c as ulong )
 
  for i as integer = 0 to min( xA.count, yA.count ) - 1
    dim as double _
      x = remap( xA[ i ], minX, maxX, r.x, r.x + r.w - 1 ), _
      y = remap( yA[ i ], minY, maxY, r.y + r.h - 1, r.y )
   
    line( x - 5, y - 5 ) - ( x + 5, y + 5 ), c, bf
  next
end sub

sub plotLine( _
  byref r as Rect, _
  byref xA as Values, byref yA as Values, _
  byval minX as double, byval maxX as double, _
  byval minY as double, byval maxY as double, _
  byval c as ulong )
 
  for i as integer = 0 to min( xA.count, yA.count ) - 1
    if( i > 0 ) then
      dim as double _
        x1 = remap( xA[ i - 1 ], minX, maxX, r.x, r.x + r.w - 1 ), _
        y1 = remap( yA[ i - 1 ], minY, maxY, r.y + r.h - 1, r.y ), _
        x2 = remap( xA[ i ], minX, maxX, r.x, r.x + r.w - 1 ), _
        y2 = remap( yA[ i ], minY, maxY, r.y + r.h - 1, r.y )
     
      line( x1, y1 ) - ( x2, y2 ), c
    end if
  next
end sub

/'
  Test code
'/
dim as Values x, y

x = add( add( add( add( add( x, 1 ), 2 ), 3 ), 4 ), 5 )
y = add( add( add( add( add( y, 1 ), 3 ), 2 ), 3 ), 5 )

/'
  The dataset used for this example assumes x values in the 0 index, and
  y values in the 1 index.
'/
dim as Dataset ds

ds = add( add( ds, x ), y )

dim as integer _
  wW = 800, wH = 600, margin = 30

screenRes( 800, 600, 32, Fb.GFX_ALPHA_PRIMITIVES )
windowTitle( "Linear regression tutorial" )
color( Black, White )
cls()

var r = Rect( margin, margin, wW - margin * 2, wH - margin * 2 )
var predicted = evaluate_algorithm( ds, @simple_linear_regression )

dim as double _
  minX = 0, maxX = max( x ) + 1, _
  minY = 0, maxY = max( y ) + 1

minY = min( minY, min( predicted ) )
maxY = max( maxY, max( predicted ) )

drawRect( r, LightGray )

plot( r, x, y, minX, maxX, minY, maxY, LightBlue )
plot( r, x, predicted, minX, maxX, minY, maxY, Red )
plotLine( r, x, predicted, minX, maxX, minY, maxY, Black )

sleep()
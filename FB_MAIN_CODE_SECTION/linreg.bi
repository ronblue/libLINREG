#include once "file.bi"

TYPE ListPair
      As Double x,y
End TYPE



DECLARE SUB pAPPEND(arr() AS ListPair , Item AS ListPair)
DECLARE SUB loadDataset( byref path as const string , p() AS ListPair)
DECLARE Function mean(p() As ListPair) As ListPair
DECLARE FUNCTION Gradient(p() As ListPair) As DOUBLE
DECLARE Function intercept(p() As ListPair,grad As Double) As DOUBLE
DECLARE Function RMSerror(p() As ListPair,m As Double,c As Double,res() As Double) As DOUBLE
DECLARE Function minmax(p() As ListPair,flag As String="x") As ListPair
DECLARE Sub plot(p() As ListPair,pred() As Double,xres As Integer,yres As Integer)
DECLARE Sub GetRegressionLineAndShow(p() As ListPair,xres As Integer,yres As Integer)

#INCLIB "linreg"
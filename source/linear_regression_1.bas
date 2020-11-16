#include once "file.bi"

TYPE ListPair
      As Double x,y
End TYPE

'APPEND TO the listpair array the ListPair item
SUB pAPPEND(arr() AS ListPair , Item AS ListPair)
	REDIM PRESERVE arr(LBOUND(arr) TO UBOUND(arr) + 1) AS ListPair
	arr(UBOUND(arr)) = Item
END SUB

SUB loadDataset( byref path as const string , p() AS ListPair)
  'dim as ListPairTable t
   'Dim As ListPair p()
  
  
  if( fileExists( path ) ) then
    dim as long f = freeFile()
    
    open path for input as f
    
    do while( not eof( f ) )
      dim as ListPair d
      
      input #f, d.x
      input #f, d.y
      PAPPEND p(), d            
    LOOP
    CLOSE #f
  end if
  
end SUB


Function mean(p() As ListPair) As ListPair
      Dim As ListPair pt
      For n As Long=Lbound(p) To Ubound(p)
            pt.x+=p(n).x
            pt.y+=p(n).y
      Next n
      Var sz=(Ubound(p)-Lbound(p)+1)
      Return Type(pt.x/sz,pt.y/sz)
End Function

Function Gradient(p() As ListPair) As Double
      Dim As Double CoVariance,Variance
      Dim As ListPair m=mean(p())
      For n As Long=Lbound(p) To Ubound(p)
            CoVariance+=(p(n).x-m.x)*(p(n).y-m.y)
            Variance+=(p(n).x-m.x)^2           
      Next n
      Return CoVariance/Variance
End Function

Function intercept(p() As ListPair,grad As Double) As Double
      Var m=mean(p())
      Return  m.y-grad*m.x
End Function

Function RMSerror(p() As ListPair,m As Double,c As Double,res() As Double) As Double
      Dim As Double acc
      Redim res(Lbound(p) To Ubound(p))
      For n As Long=Lbound(p) To Ubound(p)
            res(n)=m*p(n).x+c
            acc+=(p(n).y-res(n))^2
      Next n
      acc/=(Ubound(p)-Lbound(p)+1)
      Return Sqr(acc)
End Function


Function minmax(p() As ListPair,flag As String="x") As ListPair 'for plotting
      Dim As ListPair result
      Dim As Double d(Lbound(p) To Ubound(p))
      For n As Long=Lbound(d) To Ubound(d)
            If flag="x" Then  d(n)=p(n).x Else d(n)=p(n).y
      Next
      For n1 As Long=Lbound(d) To Ubound(d)-1
            For n2 As Long=n1+1 To Ubound(d)
                  If d(n1)>d(n2) Then Swap d(n1),d(n2)
            Next
      Next
      Return Type(d(Lbound(d)),d(Ubound(d)))
End Function

Sub plot(p() As ListPair,pred() As Double,xres As Integer,yres As Integer)
      #define map(a,b,x,c,d) ((d)-(c))*((x)-(a))/((b)-(a))+(c)
      #define xmap(z) map(minx,maxx,z,k,(xres-k))
      #define ymap(z) map(miny,maxy,z,k,(yres-k))
      Var minx=minmax(p(),"x").x,maxx=minmax(p(),"x").y
      Var miny=minmax(p(),"y").x,maxy=minmax(p(),"y").y
      Var k=100
      Line(k,k)-(xres-k,yres-k),8,b
      Dim As Double lxpos,lypos
      For n As Long=Lbound(p) To Ubound(p)
            Circle(xmap(p(n).x),ymap(p(n).y)),5,15,,,,f
            Circle(xmap(p(n).x),ymap(pred(n))),5,5,,,,f
            If n>Lbound(p) Then Line(xmap(p(n).x),ymap(pred(n)))-(lxpos,lypos),5
            Line(xmap(p(n).x),ymap(p(n).y))-(xmap(p(n).x),ymap(pred(n))) ,8
            lxpos=xmap(p(n).x)
            lypos=ymap(pred(n))
      Next n
End Sub

Sub GetRegressionLineAndShow(p() As ListPair,xres As Integer,yres As Integer)
      Var M= Gradient(p())  'get the gradient and intercept
      Var C=intercept(p(),M)
      Redim As Double predictions()
      'get the regression line points (predictions) and root mean square error
      Dim As Double e=RMSerror(p(),M,C,predictions())
      COLOR 5
      'y=Mx+C
      Print "Regression line:   y = ";M;"*x";Iif(Sgn(C)=1," +","");C
      PRINT
      PRINT "Predictions"
      For n As Long=Lbound(predictions) To Ubound(predictions)
            Print predictions(n);" ";
      Next
      Print
      Color 8
      Print "RMSE: ";e
      SLEEP
      CLS
      PLOT(p(),predictions(),xres,yres)
End Sub


SCREEN 20
'SCREENRES 1000,950

Dim As Integer xres,yres
Screeninfo xres,yres
Window(0,0)-(xres,yres)

REDIM p(any) AS ListPair
loadDataset( "D:\repo\FB_libLINREG\datasets\dataset.csv", p() )
GetRegressionLineAndShow(p(),xres,yres)
'SLEEP
'CLS
'
'REDIM p2(ANY) AS ListPair
'LOADDATASET("D:\repo\FB_libLINREG\datasets\test.csv", p2())
'GETREGRESSIONLINEANDSHOW(p2(), xres, yres)

Sleep



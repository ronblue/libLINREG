'===============================================================
' Best Line Fit. y = mx+c, RMS = Root Mean Square error. This
'  algorithm will handles diagonal, vertical and horizontal lines.
'===============================================================
Dim As Integer i, n = 5     ' number of data points

' Initialise the x,y data points
Dim As Double x(1 To n) , y(1 To n)
Print " i     x(i)      y(i) "
For i = 1 To n
    x(i) = i
    y(i) = 2 * i + 3    ' 2 * i * i + 3
    Print Using "###  ###.####  ###.#### "; i; x(i); y(i)
Next i
Print

' Calculate sigma x, y, xy, xx and yy
Dim As Double SumX = 0, SumY = 0, SumXY = 0, SumXX = 0, SumYY = 0
For i = 1 To n
    SumX += x(i)
    SumY += y(i)
    SumXY += x(i)*y(i)
    SumXX += x(i)*x(i)
Next i

' compute coefficients, double solves division by zero and infinity
Dim As Double c, m, dx, dy
dx = SumXX - SumX * SumX / n
dy = SumXY - SumX * SumY / n
m = dy / dx
c = (SumY / n) - (m * SumX / n)

' precompute conjugate of unity vector
Dim as double Ux, Uy, scale
scale = sqr(dx*dx+dy*dy)
Ux = dx / scale
Uy = -dy / scale

' sum of squares = perpendicular distance from best fit line
Dim As Double e, sos = 0, RMS
For i = 1 To n
    e = Ux * (y(i) - c) + Uy * x(i)   ' vector rotation by slope
    sos += e * e
Next i
RMS = sqr(sos / n) ' hence the term Root of the Mean Square

Print Using " Y intercept, c =####.######"; c
Print Using " Slope dy/dx, m =####.######"; m
Print Using " Sum of squares =######.####"; sos
Print Using "    RMS  Error  =######.####"; RMS

SLEEP
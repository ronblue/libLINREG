Function segmentdistance(lx1 As double, _
    ly1 As double, _
    lx2 As double, _
    ly2 As double, _
    px As double,_
    py As double, _
    Byref ox As double=0,_
    Byref oy As double=0) As double
    Dim As double M1,M2,C1,C2,B
    B=(Lx2-Lx1):If B=0 Then B=1e-20
    M2=(Ly2-Ly1)/B:If M2=0 Then M2=1e-20
    M1=-1/M2
    C1=py-M1*px
    C2=(Ly1*Lx2-Lx1*Ly2)/B
    Var L1=((px-lx1)*(px-lx1)+(py-ly1)*(py-ly1)),L2=((px-lx2)*(px-lx2)+(py-ly2)*(py-ly2))
    Var a=((lx1-lx2)*(lx1-lx2) + (ly1-ly2)*(ly1-ly2))
    Var a1=a+L1
    Var a2=a+L2
    Var f1=a1>L2,f2=a2>L1
    If f1 Xor f2 Then
        Var d1=((px-Lx1)*(px-Lx1)+(py-Ly1)*(py-Ly1))
        Var d2=((px-Lx2)*(px-Lx2)+(py-Ly2)*(py-Ly2))
        If d1<d2 Then Ox=Lx1:Oy=Ly1 : Return Sqr(d1) Else  Ox=Lx2:Oy=Ly2:Return Sqr(d2)
    End If
    Var M=M1-M2:If M=0 Then M=1e-20
    Ox=(C2-C1)/(M1-M2)
    Oy=(M1*C2-M2*C1)/M
    Return Sqr((px-Ox)*(px-Ox)+(py-Oy)*(py-Oy))
End Function

sub rotate2d(pivotx as double,pivoty as double,px as double,py as double,a as double,byref rotx as double,byref roty as double)',scale)
     rotx=(Cos(a*.0174533)*(px-pivotx)-Sin(a*.0174533)*(py-pivoty))+pivotx
     roty=(Sin(a*.0174533)*(px-pivotx)+Cos(a*.0174533)*(py-pivoty))+pivoty
end sub

sub GetRegressionLine(x() as double,y() as double,ans() as double)
    redim ans (1 to 6)
    dim as double mx,my
    dim as double lx=1e9,ly=1e9,ux=-1e9,uy=-1e9
    dim as double lastux,lastuy,lastlx,lastly
    dim as double ix,iy 'intertcept points
    dim as double tot,length,a
    dim as double min=1e6,lastmin
   for n as long=lbound(x) to ubound(x)
    mx+=x(n)
    my+=y(n)
    if lx>x(n) then lx=x(n)
    if ly>y(n) then ly=y(n)
    if ux<x(n) then ux=x(n)
    if uy<y(n) then uy=y(n)
    next n
mx=mx/(ubound(x)-lbound(x)+1)'means
my=my/(ubound(x)-lbound(x)+1)
length=sqr((ux-lx)^2+(uy-ly)^2)
lx=mx-1.5*length/2
ux=mx+1.5*length/2
ly=my
uy=my

'test rotate direction
dim as double tlx=lx,tly=ly,tux=ux,tuy=uy,t1,t2
for k as long=1 to 2
rotate2d(mx,my,tlx,tly,.1,tlx,tly)
rotate2d(mx,my,tux,tuy,.1,tux,tuy)
for n as long=lbound(x) to ubound(x)
    var d=segmentdistance(tlx,tly,tux,tuy,x(n),y(n),ix,iy)
    if k=1 then t1+=d
    if k=2 then t2+=d
next n
next k
a=.001
if t2>t1 then a=-.001
do
    tot=0
    'swing the lines round the mean and test the total distance of the points from the line.
rotate2d(mx,my,lx,ly,a,lx,ly)
rotate2d(mx,my,ux,uy,a,ux,uy)
for n as long=lbound(x) to ubound(x)
    var d=segmentdistance(lx,ly,ux,uy,x(n),y(n),ix,iy)
    tot+=d
next n
if min>tot then min=tot
if lastmin=min then 'BINGO, you have passed the minimum distance
    ans(1)=lastlx
    ans(2)=lastly
    ans(3)=lastux
    ans(4)=lastuy
    ans(5)=(lastuy-lastly)/(lastux-lastlx) 'gradient(M)
    ans(6)=-lastlx*ans(5)+lastly  'intercept(C)
exit do
end if
lastmin=min
lastux=ux
lastuy=uy
lastlx=lx
lastly=ly
loop
end sub

screen 20
dim as integer xres,yres
screeninfo xres,yres
dim as long mx,my,flag,btn

window(0,0)-(xres,yres)
redim as double x(),y()
dim as long count
start:
do
    getmouse mx,my,,btn
    locate 4
    print "Mouse click some points on the screen, press escape when points are complete"
    locate 6
    print string(30," ")
    locate 6
    print mx;"  ";yres-my
    if flag=0 and btn=1 then
        count+=1
    circle(mx,yres-my),5:flag=1
   
    redim preserve x(1 to count)
    redim preserve y(1 to count)
    x(count)=mx
    y(count)=yres-my
    end if
   
    flag=btn
    sleep 50
loop until inkey=chr(27)
'=========  get result() =====
cls
redim as double result()
GetRegressionLine(x(),y(),result())


'show result
circle(result(1),result(2)),4,5 'end points, not always on the screen.
circle(result(3),result(4)),4,5
line(result(1),result(2))-(result(3),result(4))

dim as double ix,iy 'intercepts
for n as long=lbound(x) to ubound(x)
    var d=segmentdistance(result(1),result(2),result(3),result(4),x(n),y(n),ix,iy)
   circle(x(n),y(n)),3,,,,,f
    line(x(n),y(n))-(ix,iy)
next n
locate 7
print "Equation of regression line:" ''y=Mx+C
print "y = ";result(5);"*x";iif(sgn(result(6))=1," +","");result(6)
print "again  y/n"
count=0

if input(1)="y" then cls: goto start

 
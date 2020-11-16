#INCLUDE "linreg.bi"

SCREEN 20
Dim As Integer xres,yres
Screeninfo xres,yres
Window(0,0)-(xres,yres)


REDIM p(any) AS ListPair
loadDataset( ".\..\datasets\dataset.csv", p() )
GetRegressionLineAndShow(p(),xres,yres)
sleep
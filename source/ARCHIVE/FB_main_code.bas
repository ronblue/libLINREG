'base 0 array assumed
'#define definedataset(rowcount, init...)  dim datasetdefined(rowcount - 1 ,1) as double = init
'#define dataset(r,i) datasetdefined(r ,i)

'definedataset(5, {{1, 1}, {2, 3}, {4, 3}, {3, 2}, {5, 5}})    'close to "dataset = [[1, 1], [2, 3], [4, 3], [3, 2], [5, 5]]"
'
'for i as integer = 0 to ubound(datasetdefined, 1)
'    ? dataset(i,0), dataset(i,1)
'next i
'
'SLEEP()

type P2
    declare operator []( byval index As integer ) byref As double
    as double _d(1 to 2) = {1, 1}
end type

operator P2.[]( byval index As integer ) byref As double
    return _d(index)
end operator


function CharCount(byref S as const string) as integer
    dim as integer returnValue = 0
    for cursorIndex as integer = 1 to len(S)
        if chr(S[cursorIndex])="," then returnValue += 1
    next cursorIndex
    return returnValue
end function

#macro definedataset(init...)
    dim datasetdefined(CharCount(#init)\2,1) as double
    scope
        dim as string ss = #init       
        dim as boolean toggleValue
        dim as integer indexer = 0
        for i as integer = 0 to len(ss)
            if asc(mid(ss, i + 1, 1))>47 andAlso asc(mid(ss, i + 1, 1))<58 then
                toggleValue = not toggleValue
                dim as integer startPos = i + 1
                dim as integer endPos = startPos
                do
                    if (asc(mid(ss, endPos, 1))>47 andAlso asc(mid(ss, endPos, 1))<58) orElse (mid(ss, endPos, 1)=".") then
                        endPos += 1
                    else
                        exit do
                    end if
                loop
                if toggleValue then
                    datasetdefined(indexer,0) = cDbl(mid(ss, i + 1, (endPos - startPos)))
                else
                    datasetdefined(indexer,1) = cDbl(mid(ss, i + 1, (endPos - startPos)))
                    indexer += 1
                end if
                i += (endPos - startPos)
            end if
        next i
    end scope
#endMacro

#define dataset(r,i) datasetdefined(r ,i)

'-------------------------------------------------------------------------------

definedataset({{100, 11.333}, {2, 3}, {4, 3}, {3, 2}, {51223.3221, 9999.9999}, {1.8888, 8.11111})

for i as integer = 0 to ubound(datasetdefined, 1)
    ? dataset(i,0),, dataset(i,1)
next i

dim as P2 pp

pp._d(1) = 998
pp._d(2) = 887

pp[1] = 666.779

? pp[1], pp[2]

SLEEP()


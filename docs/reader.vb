Const debugFlag = False
Const rowOffset = 3
Const arrSizeMax& = 32767
Const idLength = 8
Const transList = "T8FG,T14SG,T18SZ"
Const T8FG = 0: Const T14SG = 1: Const T18SZ = 2
Const mySep = ","
Const t14mNameStart = 17: Const t14mNameLength = 10
Const t18mNameStart = 10: Const t18mNameLength = 15
Const hwOff& = -1   ' 16713211
Const chMax = 16: Const maxTrims = 7: Const maxConds = 8
Const Plane = 0: Const Heli = 1: Const Glider = 2: Const Multi = 3
Const acList = "Airplane,Helicopter,Glider,Multicopter"
Const tailList = "Normal,V-Tail,Ailevator,Normal Rudder,Winglet (2Rud)"
Const swashList = "H-1,H-2,H-4,HR3(120°),HN3(120°),H-3,HE3,H-4X"
Const wingList = "1 Aileron,2 Aileron,2 Ail + 1 Flp,2 Ail + 2 Flp,2 Ail + 4 Flp,4 Ail + 2 Flp,4 Ail + 4 Flp," & _
    "Flying wing 2Ail,Flying wing 2Ail+1Flp,Flying wing 2Ail+2Flp,Flying wing 2Ail+4Flp,Flying wing 4Ail+2Flp,Flying wing 4Ail+4Flp"

Const functionListAir = "Aileron,Elevator,Throttle,Rudder,Gear," & _
                        "Flap,Aileron2,Aileron3,Aileron4,Elevator2," & _
                        "Flap2,Air brake,Fuel mix,Gyro,Gyro2," & _
                        "Gyro3,Throttle2,Throttle3,Throttle4,Flap3," & _
                        "Flap4,Rudder2,Butterfly,Camber,Motor," & _
                        "Auxiliary7,Auxiliary6,Auxiliary5,Auxiliary4,Auxiliary3," & _
                        "Auxiliary2,Auxiliary1,--"
Const functionListHeli = "Aileron,Elevator,Throttle,Rudder,Gear,Pitch,Governor,Governor2,Aileron4,Elevator2,Flap2,Needle,Fuel mix,Gyro,Gyro2,Gyro3," & _
    "Throttle2,Throttle3,Throttle4,Flap3,Flap4,Rudder2,Butterfly,Camber,Auxiliary8,Auxiliary7,Auxiliary6,Auxiliary5,Auxiliary4,Auxiliary3,Auxiliary2,Auxiliary1,--"
Const functionListMulti = "Aileron,Elevator,Throttle,Rudder,Gear,Flap,Aileron2,Aileron3,Aileron4,Elevator2,Flap2,Air brake,Fuel mix,Gyro,Gyro2,Gyro3," & _
    "Camera roll,Camera tilt,Camera pan,Camera rec,Mode,Rudder2,Butterfly,Camber,Motor,Auxiliary7,Auxiliary6,Auxiliary5,Auxiliary4,Auxiliary3,Auxiliary2,Auxiliary1,--"
Const functionNumber = 33
Const hwCtrlList = "J1,J2,J4,J3,SC,SD,SG,SH,RD,RS,OA,0B,SA,SB,SE,SF,LD,11,LS,13,T1,T2,T4,T3,T5,T6,T7,1B,1C,1D,1E,--"
Const modulationList = "FASST 7CH,FASST MULTI,FASST MLT2,--,S-FHSS,--,--,--,FASSTest 18CH,--,FASSTest 12CH,--,T-FHSS,--,--,--"
Const telemTypeList = "0,0,0,0,0,0,0,0,1,0,1,0,2,0,0,0"
Const tfhssVoltList = "3.8V,--,4.0V,4.2V,4.4V,4.6V,4.8V,5.0V,5.3V,5.6V,5.9V,6.2V,6.5V,6.8V,7.1V,7.4V"
Const trainerChModeList = "8 Ch,12 Ch,16 Ch,--"
Const trainerModeList = "OFF,FUNC,NORM,MIX"
Const trainerChSwList = "OFF/OFF/ON,OFF/ON/OFF,ON/OFF/OFF,--"
Const t14drFuncList = "Aileron,Elevator,Rudder,Throttle,Flap,Flap3,Butterfly,Camber": Const t14drFuncCount = 8
Const curveList = "EXP 1,EXP 2,Point,Spline"
Const fineTuneModeList = "LIN.,ATL+,ATL-,SYM."
Const aTailNorLo = 0: Const aTailNorHi = 2: Const aTailDelLo = 3: Const aTailDelHi = 4
Const t14Channels = 12: Const t14ChannelsLow = 8: Const t18Channels = 16: Const t18ChannelsLow = 12: Const t18PointsMax = 17: Const t18fTunePoints = 11
Const addr14CondStart = 555: Const t14CondLength = 888: Const t14Conditions = 5
Const addr18CondStart = 640: Const t18CondLength = 3056: Const t18Conditions = 8
Const addr14CondName = 1700: Const addr14CondNameOffset = 9
Const addr18CondName = 28140: Const addr18CondNameOffset = 578
Const addr14CondSelect = 451: Const addr18CondSelect = 64
Const addr14Func = 178: Const addr18Func = 102
Const addr14logSw = 328: Const addr18logSw = 456
Const addr14RevLo = 268: Const addr14RevHi = 165: Const addr14RevDg = 154
Const addr18RevLo = 252: Const addr18RevHi = 253: Const addr18RevDg = 518
Const maskRevDg = 192
Const addr14sTrLo = 306: Const addr14sTrHi = 166: Const addr18sTrLo = 414: Const addr18sTrHi = 586
Const addr14TrlLo = 290: Const addr14TrlHi = 706: Const addr18TrlLo = 254: Const addr18TrlHi = 562
Const addr14LimLo = 664: Const addr14LimHi = 714: Const addr18LimLo = 278: Const addr18LimHi = 570
Const addr14sSpLo = 1812: Const addr14sSpHi = 1812: Const addr18sSpLo = 438: Const addr18sSpHi = 594
Const addr14fnXC = 190: Const addr14fnCtrl = 222: Const addr14fnTrim = 234: Const addr14fnTRt = 246: Const addr14fnTSg = 258
Const addr14fnTCn = 260: Const addr14fnTMd = 260: Const addr14fnTMr = 262
Const addr14dgCtrl = 322: Const addr14dgAlt = 681
Const addr14fnGrBfly = 1545: Const addr14fnGrCamb = 1453: Const addr14fnGrMot = 1539
Const addr18fnXC = 118: Const addr18fnTRt = 182: Const addr18fnTSg = 519
Const addr18fnTCn = 214: Const addr18fnTMd = 218: Const addr18fnTMr = 222
Const addr18dgCtrl = 450: Const addr18fnGrp = 25242: Const addr18hwRev = 98
Const addr14fsLo = 269: Const addr14fsHi = 697: Const addr18fsLo = 334: Const addr18fsHi = 335
Const addr14bfsLo = 286: Const addr14bfsHi = 164: Const addr18bfsLo = 360: Const addr18bfsHi = 361
Const addr14fsPosLo = 270: Const addr14fsPosHi = 698: Const addr18fsPosLo = 336: Const addr18fsPosHi = 578
Const addr14relBFS = 287: Const addr18relBFS = 362
Const addr14sysTyp = 154: Const addr14sysAr = 156: Const addr14rxQty = 2204: Const div14sysTyp = 1: Const mask14rxQty = 1
Const addr18sysTyp = 92: Const addr18sysAr = 0: Const addr18rxQty = 516: Const div18sysTyp = 16: Const mask18rxQty = 2
Const rxIDlen = 4
Const addr14IDRx1 = 2196: Const addr14IDRx2 = addr14IDRx1 + rxIDlen
Const addr18IDRx1 = 406: Const addr18IDRx2 = addr18IDRx1 + rxIDlen
Const addr14tAct = 2206: Const mask14tAct = 128
Const addr18tAct = 92: Const mask18tAct = 2
Const addr14dlI = 2206: Const div14dlI = 1
Const addr18dlI = 516: Const div18dlI = 4
Const addr14bfsvRx1 = 2208: Const addr14bfsvRx2 = 2209
Const addr18bfsvRx1 = 401: Const addr18bfsvRx2 = 402
Const addr14trUnit = 99: Const mask14trUnit = 1: Const mask14trMem = 8: Const addr14trStep = 264: Const numb14trims = 4: Const addr14trMode = 632
Const addr18trStep = 226: Const numb18trims = 7: Const addr18trMode = 2508
Const addr14Trims = 1624: Const addr18Trims = 25088
Const addr14tcAct = 386: Const addr14tcPos = 390: Const addr14tcSw = 387: Const addr14tcAlt = 386: Const mask14tcAct = 128: Const mask14tcAlt = 64
Const addr18tcAct = 365: Const addr18tcPos = 369: Const addr18tcSw = 366: Const addr18tcAlt = 365: Const mask18tcAct = 128: Const mask18tcAlt = 64
Const addr14idAct = 1363: Const addr14idPos = 1367: Const addr14idSw = 1364: Const addr14idAlt = 1363: Const mask14idAct = 128: Const mask14idAlt = 64
Const addr18idAct = 370: Const addr18idPos = 374: Const addr18idSw = 371: Const addr18idAlt = 370: Const mask18idAct = 128: Const mask18idAlt = 64
Const addr14stAl = 1876: Const addr18stAl = 27747
Const addr14tMdl = 106
Const addr14t1Flgs = 110: Const addr14t1Alrm = 111: Const addr14t1mValH = 135: Const addr14t1mValL = 150: Const addr14t1Vib = 1698
Const addr14t1StrtSw = 113: Const addr14t1StopSw = 144: Const addr14t1RstSw = 116
Const addr14t2Flgs = 119: Const addr14t2Alrm = 120: Const addr14t2mValH = 139: Const addr14t2mValL = 151: Const addr14t2Vib = 1699
Const addr14t2StrtSw = 122: Const addr14t2StopSw = 147: Const addr14t2RstSw = 125
Const addr18tMdl = 25200: Const addr18tMem = 25258
Const addr18t1Flgs = 25212: Const addr18t1Alrm = 25213: Const addr18t1mVal = 25204: Const addr18t1Vib = 25212
Const addr18t1StrtSw = 25215: Const addr18t1StopSw = 25219: Const addr18t1RstSw = 25223
Const addr18t2Flgs = 25227: Const addr18t2Alrm = 25228: Const addr18t2mVal = 25208: Const addr18t2Vib = 25227
Const addr18t2StrtSw = 25230: Const addr18t2StopSw = 25234: Const addr18t2RstSw = 25238
Const auxFunctionNumber = 8
Const addr18AuxName = 25300: Const len18AuxName = 10: Const offs18AuxName = (len18AuxName + 1) * 2
Const addr18AuxAbbr = 25476: Const len18AuxAbbr = 4: Const offs18AuxAbbr = (len18AuxAbbr + 1) * 2
Const addr14trnSet = 633: Const addr14trnHw = 634: Const addr14trnMd = 637: Const addr14trnRt = 652: Const addr14trnSC = 640
Const addr18trnSet = 523: Const addr18trnHw = 524: Const addr18trnMd = 527: Const addr18trnCS = 530: Const addr18trnRt = 542
Const addr18trnMdH = 598: Const addr18trnCSH = 599: Const addr18trnRtH = 603: Const addr18trnSC = 607
Const addr14CondDelay = 467: Const addr18fnDelGrp = 25246
Const addr18afrGrouping = 25248: Const addr18afrStart = 688: Const t18afrLength = 53: Const addr18afrSec = 28158: Const t18afrSecLength = 12
Const t18dualRates = 6: Const addr18drStart = 1536: Const t18drLength = 57: Const addr18drSec = 28350: Const t18drSecLength = 12
Const addr14pMixStart = 391: Const t14pMixLength = 12: Const t14pMixCount = 5: Const t18pMixCount = 10
Const addr18pMixGrouping = 25252: Const addr18pMixStart = 1878: Const t18pMixLength = 63: Const addr18pMixSec = 28422: Const t18pMixSecLength = 12
Const t18pMixOffset = 6: Const t18pMixOffsetLength = 10: Const t18pMixOffsets = 4: Const t18pMixOffsetOffLength = 2
Const addr18pCurGrp = 25254

' & -> Long
' % -> Integer
' # -> Double
' ! -> Single
' @ -> Decimal
' $ -> String

Public fName
Public exitFlag As Boolean, readFlag As Boolean, saveFlag As Boolean, newData As Boolean, otFlag As Boolean
Public myArr(arrSizeMax) As Byte, myArrSize&
Public chMask&(1 To chMax)
Public transDesc$(), modelName$, acDesc$(), tailDesc$(), wingDesc$(), swashDesc$(), hwCtrlDesc$()
Public fa$(), modulationDesc$(), telemTypeDesc$(), tfhssVoltDesc$(), curveDesc$()
Public trainerChModeDesc$(), trainerModeDesc$(), trainerChSwDesc$(), t14drFuncDesc$(), fineTuneModeDesc$(), t14drFuncSeq%(t14drFuncCount - 1)
Public transType As Byte, modelType As Byte, wingType As Byte, tailType As Byte, revLo As Byte, revHi As Byte, revDg As Byte
Public travel(1 To chMax, 1 To 2) As Byte, limit(1 To chMax, 1 To 2) As Byte, sSpeed(1 To chMax) As Byte, sTrim%(1 To chMax)
Public functn(1 To chMax) As Byte
Public fsMode&, fsBattery&, fsPosition%(1 To chMax), releaseBfs&
Public hwCtrl$, hwPos$, hwRev$, hwSym$, hwType%
Public sysModulation%, sysArea$, sysRxQty$, sysRx1ID$, sysRx2ID$, sysTelemAct As Boolean, sysTelemetry$, sysDLint$, sysRx1V$, sysRx2V$
Public auxFunctionName$(auxFunctionNumber - 1), auxFunctionAbbr$(auxFunctionNumber - 1)
Public conditionName$(1 To t18Conditions), conditionState%(1 To t18Conditions), conditionHw&(1 To t18Conditions), conditionList%(1 To t18Conditions)
Public currentPoint%(functionNumber - 2), afrSequence%(t18Channels - 1), afrFlag As Boolean
Dim posDiv#, pmnR As Boolean
Public pitchF%, addr18fTunePri&, addr18fTuneSec&, t18fTuneRate As Boolean, ftPosi#, ftRate#
Public gMix%

Sub auto_Open()
    Main
End Sub ' auto_Open

Sub exitProgram()
    ActiveWorkbook.Close SaveChanges:=False
End Sub ' exitProgram

Sub readBinaryFile()
    Dim fn%, r&, buffer() As Byte
    newData = False: fName = Application.GetOpenFilename("All Files (*.*),*.*,Model Files (*.MDL),*.MDL", 2)
    If fName = False Then Exit Sub
    fn = FreeFile: Open fName For Binary Access Read As fn
    myArrSize = LOF(fn): If myArrSize > arrSizeMax + 1 Then myArrSize = arrSizeMax + 1
    buffer = String(Int(myArrSize / 2), 0)
    Get fn, , buffer
    Close fn
    Cells.ClearContents
    For r = 0 To myArrSize - 1
        myArr(r) = buffer(r)
        If debugFlag Then Cells(r + rowOffset, 1) = myArr(r)
    Next r
    newData = True
End Sub ' readBinaryFile

Sub saveBinaryFile()
    Dim fn%, r&, buffer() As Byte, nfn
'    r = 0
'    Do While CStr(Cells(r + rowOffset, 1)) <> ""
'        myArr(r) = Cells(r + rowOffset, 1): r = r + 1
'    Loop    ' While CStr(Cells(r + rowOffset, 1)) <> ""
'    myArrSize = r
    buffer = String(Int(myArrSize / 2), 0)
    For r = 0 To UBound(buffer)
        buffer(r) = myArr(r)
    Next r
    nfn = Application.GetSaveAsFilename(fName, "All Files (*.*),*.*,Model Files (*.MDL),*.MDL", 2)
    If nfn = False Then Exit Sub
    If Dir(nfn) <> "" Then
        If MsgBox("File " & nfn & " exists! Do you wish to overwrite?", vbYesNo + vbQuestion + vbDefaultButton2, "Confirm Overwrite") = vbNo Then Exit Sub
        SetAttr nfn, vbNormal
        Kill nfn
    End If  ' Dir(nfn) <> ""
    fName = nfn: fn = FreeFile: Open fName For Binary Access Write As fn
    Put fn, , buffer
    Close fn
End Sub ' saveBinaryFile

Sub convertData()
    
End Sub ' convertData

Function Power&(ByVal a&, ByVal p&)
    Dim b&, i&
    b = 1
    For i = 1 To p
        b = b * a
    Next i
    Power = b
End Function    ' Power

Function normalRate(ByVal j%) As Boolean
    If transType = T18SZ Then normalRate = (j = 0) Or (j = 3) Or ((j >= 6) And (j <= 10)) Or ((j >= 12) And (j <= 15)) Or (j = 20) Or (j = 21) Or (j >= 25) Or _
                                           ((modelType = Heli) And (j >= 12)) Or ((modelType = Glider) And ((j = 5) Or (j = 19))) Or _
                                           ((modelType = Multi) And ((j >= 12) And (j <= 21))) _
                         Else normalRate = (j = 0) Or (j = 3) Or ((j >= 6) And (j <= 10)) Or ((j >= 12) And (j <= 15)) Or (j = 21) Or (j >= 26) Or _
                                           ((modelType = Plane) And (j = 20)) Or ((modelType = Heli) And (j >= 12)) Or _
                                           ((modelType = Glider) And (((j >= 5) And (j <= 21)) Or (j >= 25)))
End Function    ' normalRate

Function normalCtrlRate(ByVal j%) As Boolean
    normalCtrlRate = (j = 0) Or (j = 2) Or (j = 20) Or (j = 22) Or (j = 31)
End Function    ' normalCtrlRate

Function rate2String$(ByVal l&, ByVal d#)
    If l = 0 Then rate2String = "0.0" Else If l < 8 * 256 Then rate2String = Format(Round(l / (d / 2)) / 2, "+0.0;-0.0;0.0") Else rate2String = Format(Round((l - 256 * 16) / (d / 2)) / 2, "+0.0;-0.0;0.0")
End Function    ' rate2String

Function t14nRate$(ByVal a&)
    If myArr(a) < 128 Then t14nRate = "+" & CStr(myArr(a)) Else t14nRate = CStr(myArr(a) - 256)
End Function    ' t14nRate

Function t14rRate$(ByVal a&)
    If myArr(a) = 0 Then t14rRate = "+0": Exit Function
    If myArr(a) < 128 Then t14rRate = "-" & CStr(myArr(a)) Else t14rRate = "+" & CStr(Abs(myArr(a) - 256))
End Function    ' t14rRate

Function t14rPosition$(ByVal a&)
    If myArr(a) < 128 Then t14rPosition = Format(50 - myArr(a) / 2#, "0.0") Else t14rPosition = Format(178 - myArr(a) / 2#, "0.0")
End Function    ' t14Position

Function pointActive(ByVal a&, ByVal p%) As Boolean
    If p = 17 Then pointActive = True: Exit Function
    pointActive = (myArr(a + Int((p - 1) / 8)) And Power(2, (p - 1) Mod 8))
End Function    ' pointActive

Public Function getTransType() As Integer
    Dim i%, s$, buffer(idLength - 1) As Byte
    For i = 0 To idLength - 1
        buffer(i) = myArr(i)
    Next i
    If RTrim(StrConv(buffer, vbUnicode)) = transDesc(T18SZ) Then getTransType = T18SZ: Exit Function
    For i = 0 To idLength - 1
        buffer(i) = myArr(i * 2 + 2)
    Next i
    s = RTrim(StrConv(buffer, vbUnicode))
    For i = T8FG To T14SG
        If s = transDesc(i) Then getTransType = i: Exit Function
    Next i
    getTransType = 255
End Function    ' getTransType

Function getModelName() As String
    Dim i%, j%, k%, buffer(t18mNameLength - 1) As Byte
    If transType = T18SZ Then i = t18mNameStart: j = t18mNameLength Else i = t14mNameStart: j = t14mNameLength
    For k = 0 To j - 1
        buffer(k) = myArr(i + k * 2 + 1): If buffer(k) = 0 Then k = k - 1: Exit For
    Next k
    getModelName = RTrim(StrConv(buffer, vbUnicode))
End Function    ' getModelName

Sub getModelType()
    Dim i&
    If transType = T18SZ Then i = 93 Else i = 152
    modelType = Int(myArr(i + 1) / 16): If modelType > Multi Then modelType = 255
    wingType = myArr(i) And 15: tailType = Int((myArr(i) And 48) / 16)
End Sub ' getModelType

Sub getModulation()
    Dim am&, dm%
    If transType = T18SZ Then am = addr18sysTyp: dm = div18sysTyp Else am = addr14sysTyp: dm = div14sysTyp:
    If transType = T8FG Then
        Select Case ((myArr(am) And 48) + (myArr(am + 1) And 128)) / 16
        Case 1
            sysModulation = 1
        Case 3
            sysModulation = 0
        Case 9
            sysModulation = 2
        End Select  ' Case ((myArr(am) And 48) + (myArr(am + 1) And 128)) / 16
    Else
        sysModulation = Int(myArr(am) / dm) And 15
    End If  ' transType = T8FG
End Sub ' getModulation

Public Sub getHardware(ByVal a&)
    Dim hC&, hP&, hR&, hS&, i1&, i2&
    hwPos = "": hwRev = "": hwSym = ""
    hC = myArr(a)
    i1 = myArr(a + 1)
    i2 = myArr(a + 2)
    If hC = 255 Then
        hwType = -1: hwCtrl = "--"
        If (i1 * 256 + i2) Then hwPos = "OFF" Else hwPos = "ON"
        Exit Sub
    End If  ' hC = 255
    hR = hC And 64
    hC = hC And 63
    If hC >= 32 Then hwCtrl = "Logic": Exit Sub
    hwCtrl = hwCtrlDesc(hC)
    If i1 > 127 Then i1 = i1 - 256
    If i2 > 127 Then i2 = i2 - 256
    If (hC And 52) = 4 Then
        If (hC And 55) = 7 Then
            hwType = 2  ' 2-position switch
            If hR = 0 Then hwPos = "OFF/ON" Else hwPos = "ON/OFF"
            Exit Sub
        End If  ' (hC And 55) = 7
        hwType = 3  ' 3-position switch
        If hR <> 0 Then hwPos = "ON/OFF/ON": Exit Sub
        If (i1 > 0) And (i2 >= 64) Then hwPos = "OFF/OFF/ON": Exit Sub
        If (i1 < 0) And (i2 >= 64) Then hwPos = "OFF/ON/ON": Exit Sub
        If (i1 <= -64) And (i2 > 0) Then hwPos = "ON/ON/OFF": Exit Sub
        If (i1 <= -64) And (i2 < 0) Then hwPos = "ON/OFF/OFF": Exit Sub
        hwPos = "OFF/ON/OFF": Exit Sub
    End If  ' (hC And 52) = 4
    hwType = 0  ' Analog input
    If hR = 0 Then hwRev = "Normal" Else hwRev = "Reverse"
    If i1 + i2 = 0 Then hwSym = "Symmetry": hwPos = CStr(Round(i2 * 100 / 64)): Exit Sub
    If i1 - i2 = 1 Then hwSym = "Linear": hwPos = CStr(Round(i1 * 100 / 64)): Exit Sub
    hwPos = "Error!!!": hwRev = "": hwSym = ""
End Sub ' getHardware

Public Function logicSwitch$(ByVal a&)
    Dim i&, m%, l$, alt$, aa&
    If transType = T18SZ Then aa = addr18logSw Else aa = addr14logSw
    If (myArr(a) And 48) = 48 Then
        getHardware aa + ((myArr(a) And 7)) * 6
        alt = hwCtrl & " " & hwPos & " " & hwRev & " " & hwSym
        If (myArr(a + 1) And 128) Then alt = alt & " Alternate"
        Select Case myArr(a + 1) Mod 4
        Case 0
            l = "AND"
        Case 1
            l = "OR"
        Case 2
            l = "EX-OR"
        Case 3
            l = "!UNDEF!"
        End Select  ' Case myArr(a + 1) Mod 4
        alt = alt & "   " & l & "   "
        getHardware aa + ((myArr(a) And 7)) * 6 + 3
        alt = alt & hwCtrl & " " & hwPos & " " & hwRev & " " & hwSym
        If (myArr(a + 1) And 64) Then alt = alt & " Alternate"
        logicSwitch = alt
    Else
        getHardware a
        logicSwitch = hwCtrl & "  " & hwPos & "  " & hwRev & "  " & hwSym
    End If  ' (myArr(a) And 48) = 48
End Function    ' logicSwitch

Public Sub getFunction()
    Dim i%, j%, bName(len18AuxName - 1) As Byte, bAbbr(len18AuxAbbr - 1) As Byte, f As Boolean
    If transType = T18SZ Then j = t18Channels: k = addr18Func Else j = t14Channels: k = addr14Func
    For i = 1 To j
        functn(i) = myArr(k + i - 1)
    Next i
    Select Case modelType
    Case Plane, Glider
        fa = Split(functionListAir, mySep)
    Case Heli
        fa = Split(functionListHeli, mySep)
    Case Multi
        fa = Split(functionListMulti, mySep)
    End Select  ' modelType
    If transType <> T18SZ Then
        If modelType = Plane Then fa(25) = "VPP"
        Exit Sub
    End If  ' transType <> T18SZ
    For i = 0 To auxFunctionNumber - 1
        f = True
        For j = 0 To len18AuxName - 1
            If f Then
                bName(j) = myArr(addr18AuxName + i * offs18AuxName + j * 2 + 1): If bName(j) = 0 Then bName(j) = 32: f = False
            Else
                bName(j) = 32
            End If  ' f
        Next j
        auxFunctionName(i) = RTrim(StrConv(bName, vbUnicode))
        If auxFunctionName(i) = "" Then auxFunctionName(i) = fa(functionNumber - 2 - i) Else fa(functionNumber - 2 - i) = auxFunctionName(i)
        f = True
        For j = 0 To len18AuxAbbr - 1
            If f Then
                bAbbr(j) = myArr(addr18AuxAbbr + i * offs18AuxAbbr + j * 2 + 1): If bAbbr(j) = 0 Then bAbbr(j) = 32: f = False
            Else
                bAbbr(j) = 32
            End If  ' f
        Next j
        auxFunctionAbbr(i) = RTrim(StrConv(bAbbr, vbUnicode))
        If auxFunctionAbbr(i) = "" Then
            If (i = 7) And (modelType <> Heli) Then auxFunctionAbbr(i) = "MOT" Else auxFunctionAbbr(i) = "AUX" & CStr(i + 1)
        End If  ' auxFunctionAbbr(i) = ""
    Next i
End Sub ' getFunction

Function functionAssigned(ByVal func$) As Boolean
    Dim i%, ch%
    functionAssigned = False: If transType = T18SZ Then ch = t18Channels Else ch = t14Channels
    For i = 1 To ch
        If fa(functn(i)) = func Then functionAssigned = True: Exit Function
    Next i
End Function    ' functionAssigned

Public Sub getConditions()
    Dim i%, j%, l%, c%, a&, o&, ac&, b(7) As Byte, f As Boolean, cp%(1 To t18Conditions)
    If transType = T18SZ Then c = t18Conditions: ac = addr18CondSelect: a = addr18CondName: o = addr18CondNameOffset: l = 2 _
                         Else c = t14Conditions: ac = addr14CondSelect: a = addr14CondName: o = addr14CondNameOffset: l = 1
    For i = 1 To t18Conditions
        conditionName(i) = "": conditionState(i) = 0: conditionHw(i) = hwOff: conditionList(i) = 0: cp(i) = 0
    Next i
    conditionState(1) = 128 + 15: conditionList(1) = 1
    If transType = T8FG Then
        Select Case modelType
        Case Heli
            conditionName(1) = "NORMAL": conditionName(2) = "IDLEUP1": conditionName(3) = "IDLEUP2": conditionName(4) = "IDLEUP3": conditionName(5) = "HOLD"
        Case Glider
            conditionName(1) = "NORMAL": conditionName(2) = "START": conditionName(3) = "SPEED": conditionName(4) = "DISTANCE": conditionName(5) = "LANDING"
        End Select  ' Case modelType
    Else
        If (transType = T18SZ) Or (modelType = Heli) Or (modelType = Glider) Then
            For i = 1 To t18Conditions
                If i <= c Then
                    f = True
                    For j = 0 To 7
                        If f Then
                            b(j) = myArr(a + (i - 1) * o + j * l + l - 1): If b(j) = 0 Then b(j) = 32: f = False
                        Else
                            b(j) = 32
                        End If  ' f
                    Next j
                    conditionName(i) = RTrim(StrConv(b, vbUnicode))
                End If  ' i > c
            Next i
        End If  ' (transType = T18SZ) Or (modelType = Heli) Or (modelType = Glider)
    End If  ' transType = T8FG
    If (transType <> T18SZ) And ((modelType = Plane) Or (modelType = Multi)) Then Exit Sub
    For i = 1 To c - 1
        m = (myArr(ac + (i - 1) * 4) And 15) + 1: If myArr(ac + (i - 1) * 4) > 127 Then conditionState(m) = 128 + i: conditionHw(m) = ac + (i - 1) * 4 + 1
    Next i
    conditionState(1) = 128 + 15
    For i = 2 To t18Conditions
        If conditionState(i) > 128 Then cp(conditionState(i) - 128) = i
    Next i
    j = 2
    For i = t18Conditions To 1 Step -1
        If cp(i) Then conditionList(j) = cp(i): j = j + 1
    Next i
End Sub ' getConditions

Public Sub servoReversing()
    Dim uf As Object, i&, j&, k&, f As Boolean
    f = (transType = T18SZ)
    If f Then i = addr18RevLo: j = addr18RevHi: k = addr18RevDg Else i = addr14RevLo: j = addr14RevHi: k = addr14RevDg
    revLo = myArr(i): revHi = myArr(j): revDg = myArr(k) And maskRevDg
    Set uf = servoReverseForm
    uf.ch1Function = fa(functn(1)): uf.ch2Function = fa(functn(2)): uf.ch3Function = fa(functn(3)): uf.ch4Function = fa(functn(4))
    uf.ch5Function = fa(functn(5)): uf.ch6Function = fa(functn(6)): uf.ch7Function = fa(functn(7)): uf.ch8Function = fa(functn(8))
    uf.ch9Function = fa(functn(9)): uf.ch10Function = fa(functn(10)): uf.ch11Function = fa(functn(11)): uf.ch12Function = fa(functn(12))
    If (revLo And 1) = 0 Then uf.ch1Nor.Enabled = True: uf.ch1Rev.Enabled = False Else uf.ch1Nor.Enabled = False: uf.ch1Rev.Enabled = True
    If (revLo And 2) = 0 Then uf.ch2Nor.Enabled = True: uf.ch2Rev.Enabled = False Else uf.ch2Nor.Enabled = False: uf.ch2Rev.Enabled = True
    If (revLo And 4) = 0 Then uf.ch3Nor.Enabled = True: uf.ch3Rev.Enabled = False Else uf.ch3Nor.Enabled = False: uf.ch3Rev.Enabled = True
    If (revLo And 8) = 0 Then uf.ch4Nor.Enabled = True: uf.ch4Rev.Enabled = False Else uf.ch4Nor.Enabled = False: uf.ch4Rev.Enabled = True
    If (revLo And 16) = 0 Then uf.ch5Nor.Enabled = True: uf.ch5Rev.Enabled = False Else uf.ch5Nor.Enabled = False: uf.ch5Rev.Enabled = True
    If (revLo And 32) = 0 Then uf.ch6Nor.Enabled = True: uf.ch6Rev.Enabled = False Else uf.ch6Nor.Enabled = False: uf.ch6Rev.Enabled = True
    If (revLo And 64) = 0 Then uf.ch7Nor.Enabled = True: uf.ch7Rev.Enabled = False Else uf.ch7Nor.Enabled = False: uf.ch7Rev.Enabled = True
    If (revLo And 128) = 0 Then uf.ch8Nor.Enabled = True: uf.ch8Rev.Enabled = False Else uf.ch8Nor.Enabled = False: uf.ch8Rev.Enabled = True
    If (revHi And 1) = 0 Then uf.ch9Nor.Enabled = True: uf.ch9Rev.Enabled = False Else uf.ch9Nor.Enabled = False: uf.ch9Rev.Enabled = True
    If (revHi And 2) = 0 Then uf.ch10Nor.Enabled = True: uf.ch10Rev.Enabled = False Else uf.ch10Nor.Enabled = False: uf.ch10Rev.Enabled = True
    If (revHi And 4) = 0 Then uf.ch11Nor.Enabled = True: uf.ch11Rev.Enabled = False Else uf.ch11Nor.Enabled = False: uf.ch11Rev.Enabled = True
    If (revHi And 8) = 0 Then uf.ch12Nor.Enabled = True: uf.ch12Rev.Enabled = False Else uf.ch12Nor.Enabled = False: uf.ch12Rev.Enabled = True
    uf.ch13Label.Visible = f: uf.ch14Label.Visible = f: uf.ch15Label.Visible = f: uf.ch16Label.Visible = f
    uf.ch13Function.Visible = f: uf.ch14Function.Visible = f: uf.ch15Function.Visible = f: uf.ch16Function.Visible = f
    uf.ch13Nor.Visible = f: uf.ch14Nor.Visible = f: uf.ch15Nor.Visible = f: uf.ch16Nor.Visible = f
    uf.ch13Rev.Visible = f: uf.ch14Rev.Visible = f: uf.ch15Rev.Visible = f: uf.ch16Rev.Visible = f
    If f Then
        uf.ch13Function = fa(functn(13)): uf.ch14Function = fa(functn(14)): uf.ch15Function = fa(functn(15)): uf.ch16Function = fa(functn(16))
        If (revHi And 16) = 0 Then uf.ch13Nor.Enabled = True: uf.ch13Rev.Enabled = False Else uf.ch13Nor.Enabled = False: uf.ch13Rev.Enabled = True
        If (revHi And 32) = 0 Then uf.ch14Nor.Enabled = True: uf.ch14Rev.Enabled = False Else uf.ch14Nor.Enabled = False: uf.ch14Rev.Enabled = True
        If (revHi And 64) = 0 Then uf.ch15Nor.Enabled = True: uf.ch15Rev.Enabled = False Else uf.ch15Nor.Enabled = False: uf.ch15Rev.Enabled = True
        If (revHi And 128) = 0 Then uf.ch16Nor.Enabled = True: uf.ch16Rev.Enabled = False Else uf.ch16Nor.Enabled = False: uf.ch16Rev.Enabled = True
    End If  ' f
    If (revDg And 64) = 0 Then uf.dg1Nor.Enabled = True: uf.dg1Rev.Enabled = False Else uf.dg1Nor.Enabled = False: uf.dg1Rev.Enabled = True
    If (revDg And 128) = 0 Then uf.dg2Nor.Enabled = True: uf.dg2Rev.Enabled = False Else uf.dg2Nor.Enabled = False: uf.dg2Rev.Enabled = True
    uf.Show
End Sub ' servoReversing

Public Sub endPoints()
    Dim uf As Object, i&, j&, atl&, ath&, tln%, all&, alh&, lln%, ch%, st&, f As Boolean
    f = (transType = T18SZ)
    If f Then atl = addr18TrlLo: ath = addr18TrlHi: tln = t18ChannelsLow: all = addr18LimLo: alh = addr18LimHi: lln = t18ChannelsLow: ch = t18Channels _
         Else atl = addr14TrlLo: ath = addr14TrlHi: tln = t14ChannelsLow: all = addr14LimLo: alh = addr14LimHi: lln = t14ChannelsLow: ch = t14Channels
    For i = 1 To ch
        If i <= tln Then j = atl + (i - 1) * 2 Else j = ath + (i - tln - 1) * 2
        travel(i, 1) = myArr(j + 0): travel(i, 2) = myArr(j + 1)
        If i <= lln Then j = all + (i - 1) * 2 Else j = alh + (i - lln - 1) * 2
        limit(i, 1) = myArr(j + 0): limit(i, 2) = myArr(j + 1)
    Next i
    Set uf = endPointsForm
    uf.ch1Function = fa(functn(1)): uf.ch2Function = fa(functn(2)): uf.ch3Function = fa(functn(3)): uf.ch4Function = fa(functn(4))
    uf.ch5Function = fa(functn(5)): uf.ch6Function = fa(functn(6)): uf.ch7Function = fa(functn(7)): uf.ch8Function = fa(functn(8))
    uf.ch9Function = fa(functn(9)): uf.ch10Function = fa(functn(10)): uf.ch11Function = fa(functn(11)): uf.ch12Function = fa(functn(12))
    uf.ch1TrlL = CStr(travel(1, 1)): uf.ch1TrlR = CStr(travel(1, 2)): uf.ch2TrlL = CStr(travel(2, 1)): uf.ch2TrlR = CStr(travel(2, 2))
    uf.ch3TrlL = CStr(travel(3, 1)): uf.ch3TrlR = CStr(travel(3, 2)): uf.ch4TrlL = CStr(travel(4, 1)): uf.ch4TrlR = CStr(travel(4, 2))
    uf.ch5TrlL = CStr(travel(5, 1)): uf.ch5TrlR = CStr(travel(5, 2)): uf.ch6TrlL = CStr(travel(6, 1)): uf.ch6TrlR = CStr(travel(6, 2))
    uf.ch7TrlL = CStr(travel(7, 1)): uf.ch7TrlR = CStr(travel(7, 2)): uf.ch8TrlL = CStr(travel(8, 1)): uf.ch8TrlR = CStr(travel(8, 2))
    uf.ch9TrlL = CStr(travel(9, 1)): uf.ch9TrlR = CStr(travel(9, 2)): uf.ch10TrlL = CStr(travel(10, 1)): uf.ch10TrlR = CStr(travel(10, 2))
    uf.ch11TrlL = CStr(travel(11, 1)): uf.ch11TrlR = CStr(travel(11, 2)): uf.ch12TrlL = CStr(travel(12, 1)): uf.ch12TrlR = CStr(travel(12, 2))
    uf.ch1LimL = CStr(limit(1, 1)): uf.ch1LimR = CStr(limit(1, 2)): uf.ch2LimL = CStr(limit(2, 1)): uf.ch2LimR = CStr(limit(2, 2))
    uf.ch3LimL = CStr(limit(3, 1)): uf.ch3LimR = CStr(limit(3, 2)): uf.ch4LimL = CStr(limit(4, 1)): uf.ch4LimR = CStr(limit(4, 2))
    uf.ch5LimL = CStr(limit(5, 1)): uf.ch5LimR = CStr(limit(5, 2)): uf.ch6LimL = CStr(limit(6, 1)): uf.ch6LimR = CStr(limit(6, 2))
    uf.ch7LimL = CStr(limit(7, 1)): uf.ch7LimR = CStr(limit(7, 2)): uf.ch8LimL = CStr(limit(8, 1)): uf.ch8LimR = CStr(limit(8, 2))
    uf.ch9LimL = CStr(limit(9, 1)): uf.ch9LimR = CStr(limit(9, 2)): uf.ch10LimL = CStr(limit(10, 1)): uf.ch10LimR = CStr(limit(10, 2))
    uf.ch11LimL = CStr(limit(11, 1)): uf.ch11LimR = CStr(limit(11, 2)): uf.ch12LimL = CStr(limit(12, 1)): uf.ch12LimR = CStr(limit(12, 2))
    uf.ch13Label.Visible = f: uf.ch14Label.Visible = f: uf.ch15Label.Visible = f: uf.ch16Label.Visible = f
    uf.ch13Function.Visible = f: uf.ch14Function.Visible = f: uf.ch15Function.Visible = f: uf.ch16Function.Visible = f
    uf.ch13TrlL.Visible = f: uf.ch13TrlR.Visible = f: uf.ch14TrlL.Visible = f: uf.ch14TrlR.Visible = f
    uf.ch15TrlL.Visible = f: uf.ch15TrlR.Visible = f: uf.ch16TrlL.Visible = f: uf.ch16TrlR.Visible = f
    uf.ch13LimL.Visible = f: uf.ch13LimR.Visible = f: uf.ch14LimL.Visible = f: uf.ch14LimR.Visible = f
    uf.ch15LimL.Visible = f: uf.ch15LimR.Visible = f: uf.ch16LimL.Visible = f: uf.ch16LimR.Visible = f
    If f Then
        uf.ch13Function = fa(functn(13)): uf.ch14Function = fa(functn(14)): uf.ch15Function = fa(functn(15)): uf.ch16Function = fa(functn(16))
        uf.ch13TrlL = CStr(travel(13, 1)): uf.ch13TrlR = CStr(travel(13, 2)): uf.ch14TrlL = CStr(travel(14, 1)): uf.ch14TrlR = CStr(travel(14, 2))
        uf.ch15TrlL = CStr(travel(15, 1)): uf.ch15TrlR = CStr(travel(15, 2)): uf.ch16TrlL = CStr(travel(16, 1)): uf.ch16TrlR = CStr(travel(16, 2))
        uf.ch13LimL = CStr(limit(13, 1)): uf.ch13LimR = CStr(limit(13, 2)): uf.ch14LimL = CStr(limit(14, 1)): uf.ch14LimR = CStr(limit(14, 2))
        uf.ch15LimL = CStr(limit(15, 1)): uf.ch15LimR = CStr(limit(15, 2)): uf.ch16LimL = CStr(limit(16, 1)): uf.ch16LimR = CStr(limit(16, 2))
    End If  ' f
    uf.Show
End Sub ' endPoints

Function cServoSpeed%(ByVal y%)
    If y < 67 Then cServoSpeed = Int(y / 10): Exit Function
    If y < 78 Then cServoSpeed = Int((y - 67) / 4) + 7: Exit Function
    If y = 78 Then cServoSpeed = y - 68: Exit Function
    If y < 88 Then cServoSpeed = Int((y - 80) / 3) + 11: Exit Function
    cServoSpeed = y - 74
End Function    ' cServoSpeed

Public Sub servoSpeed()
    Dim uf As Object, i&, j&, k%, al&, ah&, ln%, ch%, st&, f As Boolean
    f = (transType = T18SZ)
    If f Then al = addr18sSpLo: ah = addr18sSpHi: ln = t18ChannelsLow: k = 1: ch = t18Channels _
         Else al = addr14sSpLo: ah = addr14sSpHi: ln = t14Channels: k = 2: ch = t14Channels
    For i = 1 To ch
        If i <= ln Then j = al + (i - 1) * k Else j = ah + (i - ln - 1) * k
        sSpeed(i) = myArr(j)
    Next i
    Set uf = servoSpeedForm
    uf.ch1Function = fa(functn(1)): uf.ch2Function = fa(functn(2)): uf.ch3Function = fa(functn(3)): uf.ch4Function = fa(functn(4))
    uf.ch5Function = fa(functn(5)): uf.ch6Function = fa(functn(6)): uf.ch7Function = fa(functn(7)): uf.ch8Function = fa(functn(8))
    uf.ch9Function = fa(functn(9)): uf.ch10Function = fa(functn(10)): uf.ch11Function = fa(functn(11)): uf.ch12Function = fa(functn(12))
    uf.ch1sSpeed = CStr(cServoSpeed(sSpeed(1))): uf.ch2sSpeed = CStr(cServoSpeed(sSpeed(2)))
    uf.ch3sSpeed = CStr(cServoSpeed(sSpeed(3))): uf.ch4sSpeed = CStr(cServoSpeed(sSpeed(4)))
    uf.ch5sSpeed = CStr(cServoSpeed(sSpeed(5))): uf.ch6sSpeed = CStr(cServoSpeed(sSpeed(6)))
    uf.ch7sSpeed = CStr(cServoSpeed(sSpeed(7))): uf.ch8sSpeed = CStr(cServoSpeed(sSpeed(8)))
    uf.ch9sSpeed = CStr(cServoSpeed(sSpeed(9))): uf.ch10sSpeed = CStr(cServoSpeed(sSpeed(10)))
    uf.ch11sSpeed = CStr(cServoSpeed(sSpeed(11))): uf.ch12sSpeed = CStr(cServoSpeed(sSpeed(12)))
    uf.ch13Label.Visible = f: uf.ch14Label.Visible = f: uf.ch15Label.Visible = f: uf.ch16Label.Visible = f
    uf.ch13Function.Visible = f: uf.ch14Function.Visible = f: uf.ch15Function.Visible = f: uf.ch16Function.Visible = f
    uf.ch13sSpeed.Visible = f: uf.ch14sSpeed.Visible = f: uf.ch15sSpeed.Visible = f: uf.ch16sSpeed.Visible = f
    If f Then
        uf.ch13Function = fa(functn(13)): uf.ch14Function = fa(functn(14)): uf.ch15Function = fa(functn(15)): uf.ch16Function = fa(functn(16))
        uf.ch13sSpeed = CStr(cServoSpeed(sSpeed(13))): uf.ch14sSpeed = CStr(cServoSpeed(sSpeed(14)))
        uf.ch15sSpeed = CStr(cServoSpeed(sSpeed(15))): uf.ch16sSpeed = CStr(cServoSpeed(sSpeed(16)))
    End If  ' f
    uf.Show
End Sub ' servoSpeed

Public Sub subTrim()
    Dim uf As Object, i&, j&, al&, ah&, ln%, ch%, st&, f As Boolean
    f = (transType = T18SZ)
    If f Then al = addr18sTrLo: ah = addr18sTrHi: ln = t18ChannelsLow: ch = t18Channels _
         Else al = addr14sTrLo: ah = addr14sTrHi: ln = t14ChannelsLow: ch = t14Channels
    For i = 1 To ch
        If i <= ln Then j = al + (i - 1) * 2 Else j = ah + (i - ln - 1) * 2
        st = myArr(j): st = st * 256 + myArr(j + 1): If st >= 32768 Then st = st - 65536
        sTrim(i) = st
    Next i
    Set uf = subTrimForm
    uf.ch1Function = fa(functn(1)): uf.ch2Function = fa(functn(2)): uf.ch3Function = fa(functn(3)): uf.ch4Function = fa(functn(4))
    uf.ch5Function = fa(functn(5)): uf.ch6Function = fa(functn(6)): uf.ch7Function = fa(functn(7)): uf.ch8Function = fa(functn(8))
    uf.ch9Function = fa(functn(9)): uf.ch10Function = fa(functn(10)): uf.ch11Function = fa(functn(11)): uf.ch12Function = fa(functn(12))
    uf.ch1sTrim = CStr(sTrim(1)): uf.ch2sTrim = CStr(sTrim(2)): uf.ch3sTrim = CStr(sTrim(3)): uf.ch4sTrim = CStr(sTrim(4))
    uf.ch5sTrim = CStr(sTrim(5)): uf.ch6sTrim = CStr(sTrim(6)): uf.ch7sTrim = CStr(sTrim(7)): uf.ch8sTrim = CStr(sTrim(8))
    uf.ch9sTrim = CStr(sTrim(9)): uf.ch10sTrim = CStr(sTrim(10)): uf.ch11sTrim = CStr(sTrim(11)): uf.ch12sTrim = CStr(sTrim(12))
    uf.ch13Label.Visible = f: uf.ch14Label.Visible = f: uf.ch15Label.Visible = f: uf.ch16Label.Visible = f
    uf.ch13Function.Visible = f: uf.ch14Function.Visible = f: uf.ch15Function.Visible = f: uf.ch16Function.Visible = f
    uf.ch13sTrim.Visible = f: uf.ch14sTrim.Visible = f: uf.ch15sTrim.Visible = f: uf.ch16sTrim.Visible = f
    If f Then
        uf.ch13Function = fa(functn(13)): uf.ch14Function = fa(functn(14)): uf.ch15Function = fa(functn(15)): uf.ch16Function = fa(functn(16))
        uf.ch13sTrim = CStr(sTrim(13)): uf.ch14sTrim = CStr(sTrim(14)): uf.ch15sTrim = CStr(sTrim(15)): uf.ch16sTrim = CStr(sTrim(16))
    End If  ' f
    uf.Show
End Sub ' subTrim

Function hwReverse$(ByVal i%)
    Dim j%, m%
    m = 1
    For j = 1 To i Mod 8
        m = m * 2
    Next j
    If (myArr(addr18hwRev + Int(i / 8)) And m) Then hwReverse = "Reverse" Else hwReverse = "Normal"
End Function    ' hwReverse

Function fnGrouping$(ByVal c%, ByVal o%)
    Dim j%, m%, x As Byte
    x = myArr(addr18fnXC + (o Mod 2) * (functionNumber - 1) + functn(c))
    m = 1
    For j = 1 To x Mod 8
        m = m * 2
    Next j
    If (myArr(addr18fnGrp + o * 2 + Int(x / 8)) And m) Then fnGrouping = "Grp." Else fnGrouping = "Sngl"
End Function    ' fnGrouping

Public Sub controlAssignment(ByVal cond%)
    Const ac = addr18CondStart: Const lc = t18CondLength: Const axc = addr18fnXC
    Dim uf As Object, i%, a1&, a2&, c$(1 To chMax), t$(1 To chMax), ag&(22 To 24)
    Set uf = functionForm
    If transType = T18SZ Then
        For i = 1 To chMax
            a1 = axc + functn(i): a2 = ac + lc * (conditionList(cond + 1) - 1) + myArr(a1)
            If myArr(a2) > 31 Then c(i) = "--" Else c(i) = hwCtrlDesc(myArr(a2))
            a1 = axc + functn(i) + functionNumber - 1: a2 = ac + lc * (conditionList(cond + 1) - 1) + chMax + myArr(a1)
            If myArr(a2) > 31 Then t(i) = "--" Else t(i) = hwCtrlDesc(myArr(a2))
        Next i
        uf.ch13Ctrl = c(13): uf.ch13Trim = t(13): uf.ch14Ctrl = c(14): uf.ch14Trim = t(14)
        uf.ch15Ctrl = c(15): uf.ch15Trim = t(15): uf.ch16Ctrl = c(16): uf.ch16Trim = t(16)
    Else
        ag(22) = addr14fnGrBfly: ag(23) = addr14fnGrCamb: ag(24) = addr14fnGrMot
        For i = 1 To 12
            a2 = addr14fnCtrl + myArr(addr14fnXC + functn(i))
            If (functn(i) >= 22) And (functn(i) <= 24) And (modelType = Glider) Then a1 = ag(functn(i)): If (myArr(a1) > 127) Then a2 = a1 + conditionList(cond + 1)
            If myArr(a2) > 31 Then c(i) = "--" Else c(i) = hwCtrlDesc(myArr(a2))
            a2 = addr14fnTrim + myArr(addr14fnXC + functn(i))
            If myArr(a2) > 31 Then t(i) = "--" Else t(i) = hwCtrlDesc(myArr(a2))
        Next i
    End If  ' transType = T18SZ
    uf.ch1Ctrl = c(1): uf.ch1Trim = t(1): uf.ch2Ctrl = c(2): uf.ch2Trim = t(2)
    uf.ch3Ctrl = c(3): uf.ch3Trim = t(3): uf.ch4Ctrl = c(4): uf.ch4Trim = t(4)
    uf.ch5Ctrl = c(5): uf.ch5Trim = t(5): uf.ch6Ctrl = c(6): uf.ch6Trim = t(6)
    uf.ch7Ctrl = c(7): uf.ch7Trim = t(7): uf.ch8Ctrl = c(8): uf.ch8Trim = t(8)
    uf.ch9Ctrl = c(9): uf.ch9Trim = t(9): uf.ch10Ctrl = c(10): uf.ch10Trim = t(10)
    uf.ch11Ctrl = c(11): uf.ch11Trim = t(11): uf.ch12Ctrl = c(12): uf.ch12Trim = t(12)
End Sub ' controlAssignment

Function trimRate$(ByVal ch%)
    Dim i&, m%, atr&, ats&, x As Byte
    If transType = T18SZ Then atr = addr18fnTRt: ats = addr18fnTSg: x = functn(ch) _
                         Else atr = addr14fnTRt: ats = addr14fnTSg: x = myArr(addr14fnXC + functn(ch))
    m = 1
    For i = 1 To x Mod 8
        m = m * 2
    Next i
    If (myArr(ats + Int(x / 8)) And m) Then trimRate = "-" & CStr(myArr(atr + x)) Else trimRate = "+" & CStr(myArr(atr + x))
End Function    ' trimRate

Function trimMode$(ByVal ch%)
    Dim i&, m%, atc&, atm&, atr&, x As Byte
    If transType = T18SZ Then atc = addr18fnTCn: atm = addr18fnTMd: atr = addr18fnTMr: x = functn(ch) _
                         Else atc = addr14fnTCn: atm = addr14fnTMd: atr = addr14fnTMr: x = myArr(addr14fnXC + functn(ch))
    m = 1
    For i = 1 To x Mod 8
        m = m * 2
    Next i
    If ((myArr(atc + Int(x / 8)) And m) = 0) And ((myArr(atm + Int(x / 8)) And m) = 0) Then trimMode = "Normal" Else If (myArr(atm + Int(x / 8)) And m) _
        Then If (myArr(atr + Int(x / 8)) And m) Then trimMode = "ATL  Reverse" Else trimMode = "ATL  Normal" Else trimMode = "Center"
End Function    ' trimMode

Function digiCtrl$(ByVal ch%)
    Const ls$ = "                        "
    Dim i&, m%, l$, alt$
    If transType = T18SZ Then
        If (myArr(addr18dgCtrl + (ch - 1) * 3) And 48) = 48 Then
            getHardware addr18dgCtrl + ((myArr(addr18dgCtrl + (ch - 1) * 3) And 7) + 1) * 6
            alt = hwCtrl & " " & hwPos & " " & hwRev & " " & hwSym
            If (myArr(addr18dgCtrl + (ch - 1) * 3 + 1) And 128) Then alt = alt & " Alternate"
            Select Case myArr(addr18dgCtrl + (ch - 1) * 3 + 1) Mod 4
            Case 0
                l = "AND"
            Case 1
                l = "OR"
            Case 2
                l = "EX-OR"
            Case 3
                l = "!UNDEF!"
            End Select  ' Case myArr(addr18dgCtrl + (ch - 1) * 3) Mod 4
            alt = alt & "   " & l & "   "
            getHardware addr18dgCtrl + ((myArr(addr18dgCtrl + (ch - 1) * 3) And 7) + 1) * 6 + 3
            alt = alt & hwCtrl & " " & hwPos & " " & hwRev & " " & hwSym
            If (myArr(addr18dgCtrl + (ch - 1) * 3 + 1) And 64) Then alt = alt & " Alternate"
            digiCtrl = alt
        Else
            getHardware addr18dgCtrl + (ch - 1) * 3
            digiCtrl = ls & hwCtrl & "  " & hwPos & "  " & hwRev & "  " & hwSym
        End If  ' (myArr(addr18dgCtrl + (ch - 1) * 3) And 48) = 48
    Else
        m = 16
        For i = 1 To ch - 1
            m = m * 2
        Next i
        If (myArr(addr14dgAlt) And m) Then alt = "Alternate" Else alt = ""
        getHardware addr14dgCtrl + (ch - 1) * 3
        digiCtrl = ls & hwCtrl & "  " & hwPos & "  " & hwRev & "  " & hwSym & "  " & alt
    End If  ' transType = T18SZ
End Function    ' digiCtrl

Public Sub functionAssignment()
    Dim uf As Object, i&, ax&, f As Boolean, cf(1 To 12) As Boolean, cList%(t18Conditions - 1), ag&(22 To 24), sg$(1 To 12)
    f = (transType = T18SZ): If f Then ax = addr18fnXC Else ax = addr14fnXC
    Set uf = functionForm
    uf.ch1Function = fa(functn(1)): uf.ch2Function = fa(functn(2)): uf.ch3Function = fa(functn(3)): uf.ch4Function = fa(functn(4))
    uf.ch5Function = fa(functn(5)): uf.ch6Function = fa(functn(6)): uf.ch7Function = fa(functn(7)): uf.ch8Function = fa(functn(8))
    uf.ch9Function = fa(functn(9)): uf.ch10Function = fa(functn(10)): uf.ch11Function = fa(functn(11)): uf.ch12Function = fa(functn(12))
    uf.ch13Label.Visible = f: uf.ch14Label.Visible = f: uf.ch15Label.Visible = f: uf.ch16Label.Visible = f
    uf.ch13Function.Visible = f: uf.ch14Function.Visible = f: uf.ch15Function.Visible = f: uf.ch16Function.Visible = f
    uf.ch13Ctrl.Visible = f: uf.ch14Ctrl.Visible = f: uf.ch15Ctrl.Visible = f: uf.ch16Ctrl.Visible = f
    uf.ch13cGrp.Visible = f: uf.ch14cGrp.Visible = f: uf.ch15cGrp.Visible = f: uf.ch16cGrp.Visible = f
    uf.ch13Trim.Visible = f: uf.ch14Trim.Visible = f: uf.ch15Trim.Visible = f: uf.ch16Trim.Visible = f
    uf.ch1tGrp.Visible = f: uf.ch2tGrp.Visible = f: uf.ch3tGrp.Visible = f: uf.ch4tGrp.Visible = f
    uf.ch5tGrp.Visible = f: uf.ch6tGrp.Visible = f: uf.ch7tGrp.Visible = f: uf.ch8tGrp.Visible = f
    uf.ch9tGrp.Visible = f: uf.ch10tGrp.Visible = f: uf.ch11tGrp.Visible = f: uf.ch12tGrp.Visible = f
    uf.ch13tGrp.Visible = f: uf.ch14tGrp.Visible = f: uf.ch15tGrp.Visible = f: uf.ch16tGrp.Visible = f
    uf.ch13Rate.Visible = f: uf.ch14Rate.Visible = f: uf.ch15Rate.Visible = f: uf.ch16Rate.Visible = f
    uf.ch13Mode.Visible = f: uf.ch14Mode.Visible = f: uf.ch15Mode.Visible = f: uf.ch16Mode.Visible = f
    uf.conditionLabel.Visible = False: uf.conditionSelect.Visible = False: uf.hwReverseLabel.Visible = f
    uf.j1Label.Visible = f: uf.j2Label.Visible = f: uf.j3Label.Visible = f: uf.j4Label.Visible = f
    uf.saLabel.Visible = f: uf.sbLabel.Visible = f: uf.scLabel.Visible = f: uf.sdLabel.Visible = f
    uf.seLabel.Visible = f: uf.sfLabel.Visible = f: uf.sgLabel.Visible = f: uf.shLabel.Visible = f
    uf.ldLabel.Visible = f: uf.lsLabel.Visible = f: uf.rdLabel.Visible = f: uf.rsLabel.Visible = f
    uf.t1Label.Visible = f: uf.t2Label.Visible = f: uf.t3Label.Visible = f: uf.t4Label.Visible = f
    uf.t5Label.Visible = f: uf.t6Label.Visible = f
    uf.j1Reverse.Visible = f: uf.j2Reverse.Visible = f: uf.j3Reverse.Visible = f: uf.j4Reverse.Visible = f
    uf.saReverse.Visible = f: uf.sbReverse.Visible = f: uf.scReverse.Visible = f: uf.sdReverse.Visible = f
    uf.seReverse.Visible = f: uf.sfReverse.Visible = f: uf.sgReverse.Visible = f: uf.shReverse.Visible = f
    uf.ldReverse.Visible = f: uf.lsReverse.Visible = f: uf.rdReverse.Visible = f: uf.rsReverse.Visible = f
    uf.t1Reverse.Visible = f: uf.t2Reverse.Visible = f: uf.t3Reverse.Visible = f: uf.t4Reverse.Visible = f
    uf.t5Reverse.Visible = f: uf.t6Reverse.Visible = f
    uf.ch1Rate = trimRate(1): uf.ch2Rate = trimRate(2): uf.ch3Rate = trimRate(3): uf.ch4Rate = trimRate(4)
    uf.ch5Rate = trimRate(5): uf.ch6Rate = trimRate(6): uf.ch7Rate = trimRate(7): uf.ch8Rate = trimRate(8)
    uf.ch9Rate = trimRate(9): uf.ch10Rate = trimRate(10): uf.ch11Rate = trimRate(11): uf.ch12Rate = trimRate(12)
    uf.ch1Mode = trimMode(1): uf.ch2Mode = trimMode(2): uf.ch3Mode = trimMode(3): uf.ch4Mode = trimMode(4)
    uf.ch5Mode = trimMode(5): uf.ch6Mode = trimMode(6): uf.ch7Mode = trimMode(7): uf.ch8Mode = trimMode(8)
    uf.ch9Mode = trimMode(9): uf.ch10Mode = trimMode(10): uf.ch11Mode = trimMode(11): uf.ch12Mode = trimMode(12)
    uf.dg1Ctrl = digiCtrl(1): uf.dg2Ctrl = digiCtrl(2)
    If f Then
        For i = 1 To t14Channels
            cf(i) = True
        Next i
        uf.ch13Function = fa(functn(13)): uf.ch14Function = fa(functn(14)): uf.ch15Function = fa(functn(15)): uf.ch16Function = fa(functn(16))
        uf.ch13Rate = trimRate(13): uf.ch14Rate = trimRate(14): uf.ch15Rate = trimRate(15): uf.ch16Rate = trimRate(16)
        uf.ch13Mode = trimMode(13): uf.ch14Mode = trimMode(14): uf.ch15Mode = trimMode(15): uf.ch16Mode = trimMode(16)
        uf.ch1cGrp = fnGrouping(1, 0): uf.ch2cGrp = fnGrouping(2, 0): uf.ch3cGrp = fnGrouping(3, 0): uf.ch4cGrp = fnGrouping(4, 0)
        uf.ch5cGrp = fnGrouping(5, 0): uf.ch6cGrp = fnGrouping(6, 0): uf.ch7cGrp = fnGrouping(7, 0): uf.ch8cGrp = fnGrouping(8, 0)
        uf.ch9cGrp = fnGrouping(9, 0): uf.ch10cGrp = fnGrouping(10, 0): uf.ch11cGrp = fnGrouping(11, 0): uf.ch12cGrp = fnGrouping(12, 0)
        uf.ch13cGrp = fnGrouping(13, 0): uf.ch14cGrp = fnGrouping(14, 0): uf.ch15cGrp = fnGrouping(15, 0): uf.ch16cGrp = fnGrouping(16, 0)
        uf.ch1tGrp = fnGrouping(1, 1): uf.ch2tGrp = fnGrouping(2, 1): uf.ch3tGrp = fnGrouping(3, 1): uf.ch4tGrp = fnGrouping(4, 1)
        uf.ch5tGrp = fnGrouping(5, 1): uf.ch6tGrp = fnGrouping(6, 1): uf.ch7tGrp = fnGrouping(7, 1): uf.ch8tGrp = fnGrouping(8, 1)
        uf.ch9tGrp = fnGrouping(9, 1): uf.ch10tGrp = fnGrouping(10, 1): uf.ch11tGrp = fnGrouping(11, 1): uf.ch12tGrp = fnGrouping(12, 1)
        uf.ch13tGrp = fnGrouping(13, 1): uf.ch14tGrp = fnGrouping(14, 1): uf.ch15tGrp = fnGrouping(15, 1): uf.ch16tGrp = fnGrouping(16, 1)
        uf.j1Reverse = hwReverse(0): uf.j2Reverse = hwReverse(1): uf.j3Reverse = hwReverse(3): uf.j4Reverse = hwReverse(2)
        uf.saReverse = hwReverse(12): uf.sbReverse = hwReverse(13): uf.scReverse = hwReverse(4): uf.sdReverse = hwReverse(5)
        uf.seReverse = hwReverse(14): uf.sfReverse = hwReverse(15): uf.sgReverse = hwReverse(6): uf.shReverse = hwReverse(7)
        uf.ldReverse = hwReverse(16): uf.lsReverse = hwReverse(18): uf.rdReverse = hwReverse(8): uf.rsReverse = hwReverse(9)
        uf.t1Reverse = hwReverse(20): uf.t2Reverse = hwReverse(21): uf.t3Reverse = hwReverse(23): uf.t4Reverse = hwReverse(22)
        uf.t5Reverse = hwReverse(24): uf.t6Reverse = hwReverse(25)
    Else
        ag(22) = addr14fnGrBfly: ag(23) = addr14fnGrCamb: ag(24) = addr14fnGrMot
        For i = 1 To t14Channels
            cf(i) = (functn(i) >= 22) And (functn(i) <= 24) And (modelType = Glider)
            If cf(i) Then
                If (myArr(ag(functn(i))) > 127) Then sg(i) = "Sngl" Else sg(i) = "Grp."
            Else
                sg(i) = ""
            End If  ' cf(i)
        Next i
        uf.ch1cGrp = sg(1): uf.ch2cGrp = sg(2): uf.ch3cGrp = sg(3): uf.ch4cGrp = sg(4): uf.ch5cGrp = sg(5): uf.ch6cGrp = sg(6)
        uf.ch7cGrp = sg(7): uf.ch8cGrp = sg(8): uf.ch9cGrp = sg(9): uf.ch10cGrp = sg(10): uf.ch11cGrp = sg(11): uf.ch12cGrp = sg(12)
        uf.ch13Function = fa(functionNumber - 1): uf.ch14Function = fa(functionNumber - 1)
        uf.ch15Function = fa(functionNumber - 1): uf.ch16Function = fa(functionNumber - 1)
    End If  ' f
    uf.ch1cGrp.Visible = cf(1): uf.ch2cGrp.Visible = cf(2): uf.ch3cGrp.Visible = cf(3): uf.ch4cGrp.Visible = cf(4)
    uf.ch5cGrp.Visible = cf(5): uf.ch6cGrp.Visible = cf(6): uf.ch7cGrp.Visible = cf(7): uf.ch8cGrp.Visible = cf(8)
    uf.ch9cGrp.Visible = cf(9): uf.ch10cGrp.Visible = cf(10): uf.ch11cGrp.Visible = cf(11): uf.ch12cGrp.Visible = cf(12)
    If f Or (modelType = Glider) Then
        uf.conditionLabel.Visible = True: uf.conditionSelect.Visible = True: uf.conditionSelect.Clear
        For i = 1 To t18Conditions
            If conditionList(i) Then uf.conditionSelect.AddItem conditionName(conditionList(i))
        Next i
        uf.conditionSelect.ListIndex = 0
    Else
        controlAssignment 0
    End If  ' f Or (modelType = Glider)
    uf.Show
End Sub ' functionAssignment

Function cFailSafe$(ByVal c&)
    If (fsMode And chMask(c)) = 0 Then cFailSafe = "Hold" Else cFailSafe = "F/S"
End Function    ' cFailSafe

Function cBatteryFS$(ByVal c&)
    If (fsBattery And chMask(c)) = 0 Then cBatteryFS = "Off" Else cBatteryFS = "B.F/S"
End Function    ' cBatteryFS

Function cFSPosition%(ByVal i&)
    Const midPos = 1024: Const fullPos = 672: cFSPosition = Round((i - midPos) * 100 / fullPos)
End Function    ' cFSPosition

Function fsEnabled(ByVal c&) As Boolean
    If ((fsMode Or fsBattery) And chMask(c)) = 0 Then fsEnabled = False Else fsEnabled = True
End Function    ' fsEnabled

Public Sub failSafe()
    Dim uf As Object, i&, j&, al&, ah&, ln%, ch%, st&, f As Boolean
    f = (transType = T18SZ)
    If f Then al = addr18fsLo: ah = addr18fsHi Else al = addr14fsLo: ah = addr14fsHi
    fsMode = myArr(ah): fsMode = fsMode * 256 + myArr(al)
    If f Then al = addr18bfsLo: ah = addr18bfsHi Else al = addr14bfsLo: ah = addr14bfsHi
    fsBattery = myArr(ah): fsBattery = fsBattery * 256 + myArr(al)
    If f Then al = addr18fsPosLo: ah = addr18fsPosHi: ln = t18ChannelsLow: ch = t18Channels _
         Else al = addr14fsPosLo: ah = addr14fsPosHi: ln = t14ChannelsLow: ch = t14Channels
    For i = 1 To ch
        If i <= ln Then j = al + (i - 1) * 2 Else j = ah + (i - ln - 1) * 2
        st = myArr(j): st = st * 256 + myArr(j + 1)
        fsPosition(i) = cFSPosition(st)
    Next i
    If f Then releaseBfs = addr18relBFS Else releaseBfs = addr14relBFS
    Set uf = failSafeForm
    uf.ch1Function = fa(functn(1)): uf.ch2Function = fa(functn(2)): uf.ch3Function = fa(functn(3)): uf.ch4Function = fa(functn(4))
    uf.ch5Function = fa(functn(5)): uf.ch6Function = fa(functn(6)): uf.ch7Function = fa(functn(7)): uf.ch8Function = fa(functn(8))
    uf.ch9Function = fa(functn(9)): uf.ch10Function = fa(functn(10)): uf.ch11Function = fa(functn(11)): uf.ch12Function = fa(functn(12))
    uf.ch1Mode = cFailSafe(1): uf.ch2Mode = cFailSafe(2): uf.ch3Mode = cFailSafe(3): uf.ch4Mode = cFailSafe(4)
    uf.ch5Mode = cFailSafe(5): uf.ch6Mode = cFailSafe(6): uf.ch7Mode = cFailSafe(7): uf.ch8Mode = cFailSafe(8)
    uf.ch9Mode = cFailSafe(9): uf.ch10Mode = cFailSafe(10): uf.ch11Mode = cFailSafe(11): uf.ch12Mode = cFailSafe(12)
    uf.ch1BFS = cBatteryFS(1): uf.ch2BFS = cBatteryFS(2): uf.ch3BFS = cBatteryFS(3): uf.ch4BFS = cBatteryFS(4)
    uf.ch5BFS = cBatteryFS(5): uf.ch6BFS = cBatteryFS(6): uf.ch7BFS = cBatteryFS(7): uf.ch8BFS = cBatteryFS(8)
    uf.ch9BFS = cBatteryFS(9): uf.ch10BFS = cBatteryFS(10): uf.ch11BFS = cBatteryFS(11): uf.ch12BFS = cBatteryFS(12)
    uf.ch1fsPos = CStr(fsPosition(1)): uf.ch2fsPos = CStr(fsPosition(2)): uf.ch3fsPos = CStr(fsPosition(3)): uf.ch4fsPos = CStr(fsPosition(4))
    uf.ch5fsPos = CStr(fsPosition(5)): uf.ch6fsPos = CStr(fsPosition(6)): uf.ch7fsPos = CStr(fsPosition(7)): uf.ch8fsPos = CStr(fsPosition(8))
    uf.ch9fsPos = CStr(fsPosition(9)): uf.ch10fsPos = CStr(fsPosition(10)): uf.ch11fsPos = CStr(fsPosition(11)): uf.ch12fsPos = CStr(fsPosition(12))
    uf.ch1fsPos.Enabled = fsEnabled(1): uf.ch2fsPos.Enabled = fsEnabled(2): uf.ch3fsPos.Enabled = fsEnabled(3): uf.ch4fsPos.Enabled = fsEnabled(4)
    uf.ch5fsPos.Enabled = fsEnabled(5): uf.ch6fsPos.Enabled = fsEnabled(6): uf.ch7fsPos.Enabled = fsEnabled(7): uf.ch8fsPos.Enabled = fsEnabled(8)
    uf.ch9fsPos.Enabled = fsEnabled(9): uf.ch10fsPos.Enabled = fsEnabled(10): uf.ch11fsPos.Enabled = fsEnabled(11): uf.ch12fsPos.Enabled = fsEnabled(12)
    uf.ch13Label.Visible = f: uf.ch14Label.Visible = f: uf.ch15Label.Visible = f: uf.ch16Label.Visible = f
    uf.ch13Function.Visible = f: uf.ch14Function.Visible = f: uf.ch15Function.Visible = f: uf.ch16Function.Visible = f
    uf.ch13Mode.Visible = f: uf.ch14Mode.Visible = f: uf.ch15Mode.Visible = f: uf.ch16Mode.Visible = f
    uf.ch13BFS.Visible = f: uf.ch14BFS.Visible = f: uf.ch15BFS.Visible = f: uf.ch16BFS.Visible = f
    uf.ch13fsPos.Visible = f: uf.ch14fsPos.Visible = f: uf.ch15fsPos.Visible = f: uf.ch16fsPos.Visible = f
    If f Then
        uf.ch13fsPos.Enabled = fsEnabled(13): uf.ch14fsPos.Enabled = fsEnabled(14): uf.ch15fsPos.Enabled = fsEnabled(15): uf.ch16fsPos.Enabled = fsEnabled(16)
        uf.ch13Function = fa(functn(13)): uf.ch14Function = fa(functn(14)): uf.ch15Function = fa(functn(15)): uf.ch16Function = fa(functn(16))
        uf.ch13Mode = cFailSafe(13): uf.ch14Mode = cFailSafe(14): uf.ch15Mode = cFailSafe(15): uf.ch16Mode = cFailSafe(16)
        uf.ch13BFS = cBatteryFS(13): uf.ch14BFS = cBatteryFS(14): uf.ch15BFS = cBatteryFS(15): uf.ch16BFS = cBatteryFS(16)
        uf.ch13fsPos = CStr(fsPosition(13)): uf.ch14fsPos = CStr(fsPosition(14)): uf.ch15fsPos = CStr(fsPosition(15)): uf.ch16fsPos = CStr(fsPosition(16))
    End If  ' f
    getHardware releaseBfs
    uf.releaseBfsHW = hwCtrl & "    " & hwPos & "    " & hwRev & "    " & hwSym
    uf.Show
End Sub ' failSafe

Function getRxID$(ByVal a&)
    Dim i&, v&
    v = myArr(a)
    For i = 1 To rxIDlen - 1
        v = v * 256 + myArr(a + i)
    Next i
    If v = 0 Then getRxID = "-----" Else getRxID = CStr(v)
End Function    ' getRxID

Function getBFsVoltage$(ByVal a&)
    Dim st$
    If telemTypeDesc(sysModulation) = "0" Then getBFsVoltage = "": Exit Function
    If telemTypeDesc(sysModulation) = "1" Then
        st = CStr(myArr(a) / 10#)
        If InStr(st, ".") = 0 Then st = st + ".0"
        getBFsVoltage = st + "V"
        Exit Function
    End If  ' telemTypeDesc(sysModulation) = "1"
    getBFsVoltage = tfhssVoltDesc(myArr(a))
End Function    ' getBFsVoltage

Public Sub modulation()
    Dim uf As Object, i&, am&, aa&, ar&, ai1&, ai2&, ata&, adl&, ddl%, av1&, av2&, dm%, mr%, fs As Boolean, st$
    If transType = T18SZ Then
        am = addr18sysTyp: aa = addr18sysAr: ar = addr18rxQty: ai1 = addr18IDRx1: ai2 = addr18IDRx2: ata = addr18tAct: mta = mask18tAct
        adl = addr18dlI: ddl = div18dlI: av1 = addr18bfsvRx1: av2 = addr18bfsvRx2: dm = div18sysTyp: mr = mask18rxQty
    Else
        am = addr14sysTyp: aa = addr14sysAr: ar = addr14rxQty: ai1 = addr14IDRx1: ai2 = addr14IDRx2: ata = addr14tAct: mta = mask14tAct
        adl = addr14dlI: ddl = div14dlI: av1 = addr14bfsvRx1: av2 = addr14bfsvRx2: dm = div14sysTyp: mr = mask14rxQty
    End If  ' transType = T18SZ
    If transType = T8FG Then
        Select Case ((myArr(am) And 48) + (myArr(am + 1) And 128)) / 16
        Case 1
            sysModulation = 1
        Case 3
            sysModulation = 0
        Case 9
            sysModulation = 2
        End Select  ' Case ((myArr(am) And 48) + (myArr(am + 1) And 128)) / 16
    Else
        sysModulation = Int(myArr(am) / dm) And 15
    End If  ' transType = T8FG
    sysTelemAct = (myArr(ata) And mta) <> 0
    sysArea = "": sysRxQty = "": sysRx1V = "": sysRx2V = "": sysRx1ID = "": sysRx2ID = "": sysTelemetry = "": sysDLint = ""
    If (transType <> T18SZ) And ((sysModulation And 7) <> 4) Then
        sysArea = ", Area: ": If (myArr(aa) And 128) = 0 Then sysArea = sysArea + "G(eneral)" Else sysArea = sysArea + "F(rance)"
    End If  ' (transType <> T18SZ) And ((sysModulation And 7) <> 4)
    Set uf = modulationForm
    uf.modulationType = modulationDesc(sysModulation) & sysArea
    uf.receiverLabel = "": uf.receiverQty = "": uf.receiverIDLabel = "": uf.receiver1ID = "": uf.receiver2ID = ""
    uf.telemetryLabel = "": uf.telemetryActive = "": uf.dlIntervalLabel = "": uf.dlInterval = ""
    uf.bfsVoltageLabel = "": uf.bfsVoltageRx1 = "": uf.bfsVoltageRx2 = ""
    If telemTypeDesc(sysModulation) <> "0" Then
        uf.receiverLabel = "Receiver:"
        fs = ((myArr(ar) And mr) = 0) Or (sysModulation = 10)
        If fs Then sysRxQty = "Single" Else sysRxQty = "Dual"
        uf.receiverQty = sysRxQty
        uf.receiverIDLabel = "Receiver ID:": uf.bfsVoltageLabel = "B.F/S voltage:"
        sysRx1ID = "Rx1: " & getRxID(ai1): sysRx1V = "Rx1: " & getBFsVoltage(av1)
        If Not fs Then sysRx2ID = "Rx2: " & getRxID(ai2): sysRx2V = "Rx2: " & getBFsVoltage(av2)
        uf.receiver1ID = sysRx1ID: uf.receiver2ID = sysRx2ID
        uf.bfsVoltageRx1 = sysRx1V: uf.bfsVoltageRx2 = sysRx2V
        uf.telemetryLabel = "Telemetry:"
        If sysTelemAct Then sysTelemetry = "ACT" Else sysTelemetry = "INH"
        uf.telemetryActive = sysTelemetry
    End If  ' telemTypeDesc(sysModulation) <> "0"
    If telemTypeDesc(sysModulation) = "1" Then
        uf.dlIntervalLabel = "D/L interval:"
        sysDLint = CStr((Int(myArr(adl) / ddl) And 31) / 10#)
        If InStr(sysDLint, ".") = 0 Then sysDLint = sysDLint & ".0"
        sysDLint = sysDLint & "sec."
        uf.dlInterval = sysDLint
    End If  ' telemTypeDesc(sysModulation) = "1"
    uf.Show
End Sub ' modulation

Public Sub trimSettings()
    Dim uf As Object, i%, b As Byte, ats&, tn%, s$, f As Boolean
    Set uf = trimSetForm
    If transType = T18SZ Then
        ats = addr18trStep: tn = numb18trims
        For i = 1 To t18Conditions
            f = (conditionList(i))
            If f Then b = myArr(addr18trMode + (conditionList(i) - 1) * t18CondLength): s = conditionName(conditionList(i)) Else b = 0: s = ""
            Select Case i
            Case 1
                uf.cond1Label = s
                If (b And 1) Then uf.t1c1Mode = "Comb." Else uf.t1c1Mode = "Separ."
                If (b And 2) Then uf.t2c1Mode = "Comb." Else uf.t2c1Mode = "Separ."
                If (b And 8) Then uf.t3c1Mode = "Comb." Else uf.t3c1Mode = "Separ."
                If (b And 4) Then uf.t4c1Mode = "Comb." Else uf.t4c1Mode = "Separ."
                If (b And 16) Then uf.t5c1Mode = "Comb." Else uf.t5c1Mode = "Separ."
                If (b And 32) Then uf.t6c1Mode = "Comb." Else uf.t6c1Mode = "Separ."
                If (b And 64) Then uf.t7c1Mode = "Comb." Else uf.t7c1Mode = "Separ."
            Case 2
                uf.cond2Label = s
                If (b And 1) Then uf.t1c2Mode = "Comb." Else uf.t1c2Mode = "Separ."
                If (b And 2) Then uf.t2c2Mode = "Comb." Else uf.t2c2Mode = "Separ."
                If (b And 8) Then uf.t3c2Mode = "Comb." Else uf.t3c2Mode = "Separ."
                If (b And 4) Then uf.t4c2Mode = "Comb." Else uf.t4c2Mode = "Separ."
                If (b And 16) Then uf.t5c2Mode = "Comb." Else uf.t5c2Mode = "Separ."
                If (b And 32) Then uf.t6c2Mode = "Comb." Else uf.t6c2Mode = "Separ."
                If (b And 64) Then uf.t7c2Mode = "Comb." Else uf.t7c2Mode = "Separ."
                uf.cond2Label.Visible = f
                uf.t1c2Mode.Visible = f: uf.t2c2Mode.Visible = f: uf.t3c2Mode.Visible = f
                uf.t4c2Mode.Visible = f: uf.t5c2Mode.Visible = f: uf.t6c2Mode.Visible = f
                uf.t7c2Mode.Visible = f
            Case 3
                uf.cond3Label = s
                If (b And 1) Then uf.t1c3Mode = "Comb." Else uf.t1c3Mode = "Separ."
                If (b And 2) Then uf.t2c3Mode = "Comb." Else uf.t2c3Mode = "Separ."
                If (b And 8) Then uf.t3c3Mode = "Comb." Else uf.t3c3Mode = "Separ."
                If (b And 4) Then uf.t4c3Mode = "Comb." Else uf.t4c3Mode = "Separ."
                If (b And 16) Then uf.t5c3Mode = "Comb." Else uf.t5c3Mode = "Separ."
                If (b And 32) Then uf.t6c3Mode = "Comb." Else uf.t6c3Mode = "Separ."
                If (b And 64) Then uf.t7c3Mode = "Comb." Else uf.t7c3Mode = "Separ."
                uf.cond3Label.Visible = f
                uf.t1c3Mode.Visible = f: uf.t2c3Mode.Visible = f: uf.t3c3Mode.Visible = f
                uf.t4c3Mode.Visible = f: uf.t5c3Mode.Visible = f: uf.t6c3Mode.Visible = f
                uf.t7c3Mode.Visible = f
            Case 4
                uf.cond4Label = s
                If (b And 1) Then uf.t1c4Mode = "Comb." Else uf.t1c4Mode = "Separ."
                If (b And 2) Then uf.t2c4Mode = "Comb." Else uf.t2c4Mode = "Separ."
                If (b And 8) Then uf.t3c4Mode = "Comb." Else uf.t3c4Mode = "Separ."
                If (b And 4) Then uf.t4c4Mode = "Comb." Else uf.t4c4Mode = "Separ."
                If (b And 16) Then uf.t5c4Mode = "Comb." Else uf.t5c4Mode = "Separ."
                If (b And 32) Then uf.t6c4Mode = "Comb." Else uf.t6c4Mode = "Separ."
                If (b And 64) Then uf.t7c4Mode = "Comb." Else uf.t7c4Mode = "Separ."
                uf.cond4Label.Visible = f
                uf.t1c4Mode.Visible = f: uf.t2c4Mode.Visible = f: uf.t3c4Mode.Visible = f
                uf.t4c4Mode.Visible = f: uf.t5c4Mode.Visible = f: uf.t6c4Mode.Visible = f
                uf.t7c4Mode.Visible = f
            Case 5
                uf.cond5Label = s
                If (b And 1) Then uf.t1c5Mode = "Comb." Else uf.t1c5Mode = "Separ."
                If (b And 2) Then uf.t2c5Mode = "Comb." Else uf.t2c5Mode = "Separ."
                If (b And 8) Then uf.t3c5Mode = "Comb." Else uf.t3c5Mode = "Separ."
                If (b And 4) Then uf.t4c5Mode = "Comb." Else uf.t4c5Mode = "Separ."
                If (b And 16) Then uf.t5c5Mode = "Comb." Else uf.t5c5Mode = "Separ."
                If (b And 32) Then uf.t6c5Mode = "Comb." Else uf.t6c5Mode = "Separ."
                If (b And 64) Then uf.t7c5Mode = "Comb." Else uf.t7c5Mode = "Separ."
                uf.cond5Label.Visible = f
                uf.t1c5Mode.Visible = f: uf.t2c5Mode.Visible = f: uf.t3c5Mode.Visible = f
                uf.t4c5Mode.Visible = f: uf.t5c5Mode.Visible = f: uf.t6c5Mode.Visible = f
                uf.t7c5Mode.Visible = f
            Case 6
                uf.cond6Label = s
                If (b And 1) Then uf.t1c6Mode = "Comb." Else uf.t1c6Mode = "Separ."
                If (b And 2) Then uf.t2c6Mode = "Comb." Else uf.t2c6Mode = "Separ."
                If (b And 8) Then uf.t3c6Mode = "Comb." Else uf.t3c6Mode = "Separ."
                If (b And 4) Then uf.t4c6Mode = "Comb." Else uf.t4c6Mode = "Separ."
                If (b And 16) Then uf.t5c6Mode = "Comb." Else uf.t5c6Mode = "Separ."
                If (b And 32) Then uf.t6c6Mode = "Comb." Else uf.t6c6Mode = "Separ."
                If (b And 64) Then uf.t7c6Mode = "Comb." Else uf.t7c6Mode = "Separ."
                uf.cond6Label.Visible = f
                uf.t1c6Mode.Visible = f: uf.t2c6Mode.Visible = f: uf.t3c6Mode.Visible = f
                uf.t4c6Mode.Visible = f: uf.t5c6Mode.Visible = f: uf.t6c6Mode.Visible = f
                uf.t7c6Mode.Visible = f
            Case 7
                uf.cond7Label = s
                If (b And 1) Then uf.t1c7Mode = "Comb." Else uf.t1c7Mode = "Separ."
                If (b And 2) Then uf.t2c7Mode = "Comb." Else uf.t2c7Mode = "Separ."
                If (b And 8) Then uf.t3c7Mode = "Comb." Else uf.t3c7Mode = "Separ."
                If (b And 4) Then uf.t4c7Mode = "Comb." Else uf.t4c7Mode = "Separ."
                If (b And 16) Then uf.t5c7Mode = "Comb." Else uf.t5c7Mode = "Separ."
                If (b And 32) Then uf.t6c7Mode = "Comb." Else uf.t6c7Mode = "Separ."
                If (b And 64) Then uf.t7c7Mode = "Comb." Else uf.t7c7Mode = "Separ."
                uf.cond7Label.Visible = f
                uf.t1c7Mode.Visible = f: uf.t2c7Mode.Visible = f: uf.t3c7Mode.Visible = f
                uf.t4c7Mode.Visible = f: uf.t5c7Mode.Visible = f: uf.t6c7Mode.Visible = f
                uf.t7c7Mode.Visible = f
            Case 8
                uf.cond8Label = s
                If (b And 1) Then uf.t1c8Mode = "Comb." Else uf.t1c8Mode = "Separ."
                If (b And 2) Then uf.t2c8Mode = "Comb." Else uf.t2c8Mode = "Separ."
                If (b And 8) Then uf.t3c8Mode = "Comb." Else uf.t3c8Mode = "Separ."
                If (b And 4) Then uf.t4c8Mode = "Comb." Else uf.t4c8Mode = "Separ."
                If (b And 16) Then uf.t5c8Mode = "Comb." Else uf.t5c8Mode = "Separ."
                If (b And 32) Then uf.t6c8Mode = "Comb." Else uf.t6c8Mode = "Separ."
                If (b And 64) Then uf.t7c8Mode = "Comb." Else uf.t7c8Mode = "Separ."
                uf.cond8Label.Visible = f
                uf.t1c8Mode.Visible = f: uf.t2c8Mode.Visible = f: uf.t3c8Mode.Visible = f
                uf.t4c8Mode.Visible = f: uf.t5c8Mode.Visible = f: uf.t6c8Mode.Visible = f
                uf.t7c8Mode.Visible = f
            End Select  ' Case i
        Next i
    Else
        uf.Caption = "T1-T4 setting": uf.t5Label = "": uf.t6Label = "": uf.t7Label = "": uf.t5Step = "": uf.t6Step = "": uf.t7Step = ""
        ats = addr14trStep: tn = numb14trims
        If (modelType = Plane) Or (modelType = Multi) Then
            uf.cond1Label = "": uf.t1c1Mode = "": uf.t2c1Mode = "": uf.t3c1Mode = "": uf.t4c1Mode = ""
        Else
            uf.cond1Label = "Mode"
            If (myArr(addr14trMode) And 1) Then uf.t1c1Mode = "Comb." Else uf.t1c1Mode = "Separ."
            If (myArr(addr14trMode) And 2) Then uf.t2c1Mode = "Comb." Else uf.t2c1Mode = "Separ."
            If (myArr(addr14trMode) And 8) Then uf.t3c1Mode = "Comb." Else uf.t3c1Mode = "Separ."
            If (myArr(addr14trMode) And 4) Then uf.t4c1Mode = "Comb." Else uf.t4c1Mode = "Separ."
        End If  ' (modelType = Plane) Or (modelType = Multi)
        uf.t5c1Mode = "": uf.t6c1Mode = "": uf.t7c1Mode = "": uf.cond2Label = "Unit"
        If (myArr(addr14trUnit) And mask14trUnit) Then uf.t1c2Mode = "%" Else uf.t1c2Mode = "--"
        uf.t2c2Mode = "": uf.t3c2Mode = "": uf.t4c2Mode = "": uf.t5c2Mode = "": uf.t6c2Mode = "": uf.t7c2Mode = ""
        uf.cond3Label = "T1-T4 memory"
        If (myArr(addr14trUnit) And mask14trMem) Then uf.t1c3Mode = "ACT" Else uf.t1c3Mode = "INH"
        uf.t2c3Mode = "": uf.t3c3Mode = "": uf.t4c3Mode = "": uf.t5c3Mode = "": uf.t6c3Mode = "": uf.t7c3Mode = ""
        uf.cond4Label = "": uf.t1c4Mode = "": uf.t2c4Mode = "": uf.t3c4Mode = "": uf.t4c4Mode = "": uf.t5c4Mode = "": uf.t6c4Mode = "": uf.t7c4Mode = ""
        uf.cond5Label = "": uf.t1c5Mode = "": uf.t2c5Mode = "": uf.t3c5Mode = "": uf.t4c5Mode = "": uf.t5c5Mode = "": uf.t6c5Mode = "": uf.t7c5Mode = ""
        uf.cond6Label = "": uf.t1c6Mode = "": uf.t2c6Mode = "": uf.t3c6Mode = "": uf.t4c6Mode = "": uf.t5c6Mode = "": uf.t6c6Mode = "": uf.t7c6Mode = ""
        uf.cond7Label = "": uf.t1c7Mode = "": uf.t2c7Mode = "": uf.t3c7Mode = "": uf.t4c7Mode = "": uf.t5c7Mode = "": uf.t6c7Mode = "": uf.t7c7Mode = ""
        uf.cond8Label = "": uf.t1c8Mode = "": uf.t2c8Mode = "": uf.t3c8Mode = "": uf.t4c8Mode = "": uf.t5c8Mode = "": uf.t6c8Mode = "": uf.t7c8Mode = ""
    End If  ' transType = T18SZ
    For i = 1 To tn
        s = CStr(myArr(ats + i - 1))
        Select Case i
        Case 1
            uf.t1Step = s
        Case 2
            uf.t2Step = s
        Case 3
            uf.t4Step = s
        Case 4
            uf.t3Step = s
        Case 5
            uf.t5Step = s
        Case 6
            uf.t6Step = s
        Case 7
            uf.t7Step = s
        End Select  ' Case i
    Next i
    uf.Show
End Sub ' trimSettings

Public Sub throttleCut()
    Dim uf As Object, i%, aa&, ap&, ah&, al&, ma%, ml%
    If transType = T18SZ Then aa = addr18tcAct: ap = addr18tcPos: ah = addr18tcSw: al = addr18tcAlt: ma = mask18tcAct: ml = mask18tcAlt _
                         Else aa = addr14tcAct: ap = addr14tcPos: ah = addr14tcSw: al = addr14tcAlt: ma = mask14tcAct: ml = mask14tcAlt
    Set uf = tCutForm
    uf.Caption = "Throttle cut"
    uf.posLabel.Caption = "Cut position"
    If (myArr(aa) And ma) Then uf.tCutAct = "ACT" Else uf.tCutAct = "INH"
    uf.tCutPos = CStr(Int((100 - myArr(ap)) / 2))
    getHardware ah
    uf.tCutHw = hwCtrl & "   " & hwPos & "   " & hwRev & "   " & hwSym
    If (myArr(al) And ml) Then uf.tCutAlt = "Alternate" Else uf.tCutAlt = "Normal"
    uf.Show
End Sub ' throttleCut

Public Sub idleDown()
    Dim uf As Object, i%, aa&, ap&, ah&, al&, ma%, ml%
    If transType = T18SZ Then aa = addr18idAct: ap = addr18idPos: ah = addr18idSw: al = addr18idAlt: ma = mask18idAct: ml = mask18idAlt _
                         Else aa = addr14idAct: ap = addr14idPos: ah = addr14idSw: al = addr14idAlt: ma = mask14idAct: ml = mask14idAlt
    Set uf = tCutForm
    uf.Caption = "Idle down"
    uf.posLabel.Caption = "Offset"
    If (myArr(aa) And ma) Then uf.tCutAct = "ACT" Else uf.tCutAct = "INH"
    If myArr(ap) < 128 Then uf.tCutPos = "+" & CStr(myArr(ap)) Else uf.tCutPos = myArr(ap) - 256
    getHardware ah
    uf.tCutHw = hwCtrl & "   " & hwPos & "   " & hwRev & "   " & hwSym
    If (myArr(al) And ml) Then uf.tCutAlt = "Alternate" Else uf.tCutAlt = "Normal"
    uf.Show
End Sub ' idleDown

Public Sub swashRing()
    Dim uf As Object, aa&, f As Boolean
    If transType = T18SZ Then aa = 400 Else aa = 761
    Set uf = tCutForm
    uf.Caption = "Swash ring"
    If myArr(aa) = 255 Then uf.tCutAct = "INH": uf.posLabel = "": uf.tCutPos = "" Else uf.tCutAct = "ACT": uf.posLabel = "Rate": uf.tCutPos = CStr(myArr(aa))
    uf.hwLabel = "": uf.tCutHw = "": uf.altLabel = "": uf.tCutAlt = ""
    uf.Show
End Sub ' swashRing

Public Sub swash()
    Const addr14SwNPnt = 728: Const addr14SwAFR = 730: Const addr14SwMixL = 733: Const addr14SwMixH = 743: Const addr14SwLC = 751: Const addr14SwMov = 1280
    Const addr18SwNPnt = 388: Const addr18SwAFR = 375: Const addr18SwMixL = 378: Const addr18SwMixH = 554: Const addr18SwLC = 390
    Dim uf As Object, i%, anp&, aa&, aml&, amh&, alc&, f As Boolean
    f = (transType = T18SZ)
    If f Then anp = addr18SwNPnt: aa = addr18SwAFR: aml = addr18SwMixL: amh = addr18SwMixH: alc = addr18SwLC _
         Else anp = addr14SwNPnt: aa = addr14SwAFR: aml = addr14SwMixL: amh = addr14SwMixH: alc = addr14SwLC
    Set uf = swashForm
    np = myArr(anp + 0) + myArr(anp + 1) / 256: If myArr(anp) > 127 Then np = np - 256
    uf.neutralPoint = Format(50 - 2 * np, "0")
    If myArr(aa + 0) < 128 Then uf.aileronAFR = "+" & CStr(myArr(aa + 0)) Else uf.aileronAFR = CStr(myArr(aa + 0) - 256)
    If wingType < 2 Then uf.elevatorAFRLabel = "": uf.elevatorAFR = "" _
                    Else If myArr(aa + 1) < 128 Then uf.elevatorAFR = "+" & CStr(myArr(aa + 1)) Else uf.elevatorAFR = CStr(myArr(aa + 1) - 256)
    If myArr(aa + 2) < 128 Then uf.pitchAFR = "+" & CStr(myArr(aa + 2)) Else uf.pitchAFR = CStr(myArr(aa + 2) - 256)
    uf.pit2ailLeft = myArr(aml + 6): uf.pit2ailRight = myArr(aml + 7): uf.ail2pitLeft = myArr(aml + 0): uf.ail2pitRight = myArr(aml + 1)
    If wingType < 2 Then uf.pit2eleLabel = "": uf.pit2eleLeft = "": uf.pit2eleRight = "" _
                    Else uf.pit2eleLeft = myArr(aml + 8): uf.pit2eleRight = myArr(aml + 9)
    If (wingType = 2) Or (wingType = 7) Then
        uf.pit2ele2Left = myArr(amh + 6): uf.pit2ele2Right = myArr(amh + 7)
        If wingType = 2 Then uf.ail2ele2Label = "": uf.ail2ele2Left = "": uf.ail2ele2Right = "" Else uf.ail2ele2Left = myArr(amh + 2): uf.ail2ele2Right = myArr(amh + 3)
        uf.ele2ele2Left = myArr(amh + 4): uf.ele2ele2Right = myArr(amh + 5)
    Else
        uf.pit2ele2Label = "": uf.pit2ele2Left = "": uf.pit2ele2Right = ""
        uf.ail2ele2Label = "": uf.ail2ele2Left = "": uf.ail2ele2Right = ""
        uf.ele2ele2Label = "": uf.ele2ele2Left = "": uf.ele2ele2Right = ""
    End If  ' (wingType = 2) Or (wingType = 7)
    If (wingType = 4) Or (wingType = 7) Then
        uf.ail2eleLeft = myArr(amh + 0): uf.ail2eleRight = myArr(amh + 1)
    Else
        uf.ail2eleLabel = "": uf.ail2eleLeft = "": uf.ail2eleRight = ""
    End If  ' (wingType = 4) Or (wingType = 7)
    If (wingType = 3) Or (wingType = 5) Or (wingType = 7) Then
        uf.ele2ailLeft = myArr(aml + 2): uf.ele2ailRight = myArr(aml + 3)
    Else
        uf.ele2ailLabel = "": uf.ele2ailLeft = "": uf.ele2ailRight = ""
    End If  ' (wingType = 3) Or (wingType = 5) Or (wingType = 7)
    If (wingType > 2) And (wingType <> 6) Then
        uf.ele2pitLeft = myArr(aml + 4): uf.ele2pitRight = myArr(aml + 5)
    Else
        uf.ele2pitLabel = "": uf.ele2pitLeft = "": uf.ele2pitRight = ""
    End If  ' (wingType > 2) And (wingType <> 6)
    If (myArr(alc + 8) And 1) Then uf.aileronDirection = "-" Else uf.aileronDirection = "+"
    uf.aileronLowLeft = CStr(myArr(alc + 2)): uf.aileronLowRight = CStr(myArr(alc + 3))
    uf.aileronHighLeft = CStr(myArr(alc + 0)): uf.aileronHighRight = CStr(myArr(alc + 1))
    If (wingType = 1) Or (wingType = 6) Then
        uf.elevatorCompLabel = "": uf.elevatorDirection = "": uf.elevatorLowLeft = "": uf.elevatorLowRight = "": uf.elevatorHighLeft = "": uf.elevatorHighRight = ""
    Else
        If (myArr(alc + 8) And 2) Then uf.elevatorDirection = "-" Else uf.elevatorDirection = "+"
        uf.elevatorLowLeft = CStr(myArr(alc + 6)): uf.elevatorLowRight = CStr(myArr(alc + 7))
        uf.elevatorHighLeft = CStr(myArr(alc + 4)): uf.elevatorHighRight = CStr(myArr(alc + 5))
    End If  ' (wingType = 1) Or (wingType = 6)
    If (wingType >= 3) And (wingType <= 5) Then uf.speedCompensation = CStr(myArr(alc + 9)) Else uf.speedCompLabel = "": uf.speedCompensation = ""
    If f Then uf.movingLabel = "": uf.moving = "" Else uf.moving = CStr(myArr(addr14SwMov))
    uf.Show
End Sub ' swash

Public Sub stickAlarm()
    Dim uf As Object, i%, l%, a&, h&, f As Boolean
    Set uf = stickAlarmForm
    f = (transType = T18SZ)
    If f Then
        uf.Label2.Caption = "Position": uf.Label3.Caption = "Switch": uf.Label4.Caption = "Alternate"
        a = addr18stAl: l = 3
    Else
        uf.Label2.Caption = "Switch": uf.Label3.Caption = "Stick": uf.Label4.Caption = "Position"
        a = addr14stAl: l = 5
    End If  ' f
    If (myArr(a) And 128) Then uf.stickAlarmAct = "ACT" Else uf.stickAlarmAct = "INH"
    h = myArr(a + 1): If f Then myArr(a + 1) = (myArr(a + 1) And 223): If f And ((myArr(a + 1) And 31) = 31) Then myArr(a + 1) = 255
    getHardware a + 1
    s = hwCtrl & "   " & hwPos & "   " & hwRev & "   " & hwSym
    myArr(a + 1) = h
    If f Then
        uf.Value3 = s: uf.Value2 = myArr(a) And 127: If (myArr(a + 1) And 32) Then uf.Value4 = "Alternate" Else uf.Value4 = "Normal"
    Else
        uf.Value2 = s: If myArr(a + 5) Mod 2 Then i = -2 Else i = 2
        l = myArr(a + 4): If l > 127 Then l = l - 256
        uf.Value4 = CStr(Int(50 + l / i)) & "%": uf.Value3 = hwCtrlDesc(myArr(a + 5) Mod 4)
    End If  ' f
    uf.Show
End Sub ' stickAlarm

Public Sub timerSettings()
    Dim uf As Object, i%, l%, am&, af1&, af2&, aa1&, aa2&, atm1&, amh1&, aml1&, atm2&, amh2&, aml2&, av1&, av2&, _
        ab1&, ae1&, ar1&, ab2&, ae2&, ar2&, b As Boolean, t1Mem#, t2Mem#
    Set uf = timerSettingsForm
    If transType = T18SZ Then
        am = addr18tMdl: l = 4
        af1 = addr18t1Flgs: aa1 = addr18t1Alrm: atm1 = addr18tMem: amh1 = addr18t1mVal: aml1 = 0: av1 = addr18t1Vib
        ab1 = addr18t1StrtSw + 1: ae1 = addr18t1StopSw + 1: ar1 = addr18t1RstSw + 1
        af2 = addr18t2Flgs: aa2 = addr18t2Alrm: atm2 = addr18tMem: amh2 = addr18t2mVal: aml2 = 0: av2 = addr18t2Vib
        ab2 = addr18t2StrtSw + 1: ae2 = addr18t2StopSw + 1: ar2 = addr18t2RstSw + 1
    Else
        am = addr14tMdl: l = 3
        af1 = addr14t1Flgs: aa1 = addr14t1Alrm: atm1 = addr14t1Flgs: amh1 = addr14t1mValH: aml1 = addr14t1mValL: av1 = addr14t1Vib
        ab1 = addr14t1StrtSw: ae1 = addr14t1StopSw: ar1 = addr14t1RstSw
        af2 = addr14t2Flgs: aa2 = addr14t2Alrm: atm2 = addr14t2Flgs: amh2 = addr14t2mValH: aml2 = addr14t2mValL: av2 = addr14t2Vib
        ab2 = addr14t2StrtSw: ae2 = addr14t2StopSw: ar2 = addr14t2RstSw
    End If  ' transType = T18SZ
    b = (transType = T14SG): uf.t1SpeechLabel.Enabled = b: uf.t1Speech.Enabled = b: uf.t2SpeechLabel.Enabled = b: uf.t2Speech.Enabled = b
    b = (transType <> T8FG): uf.t1VibeLabel.Enabled = b: uf.t1Vibrator.Enabled = b: uf.t2VibeLabel.Enabled = b: uf.t2Vibrator.Enabled = b
    t1Mem = myArr(am)
    For i = 1 To 3
        t1Mem = t1Mem * 256 + myArr(am + i)
    Next i
    uf.modelTimer = Format(Int(t1Mem / 3600), "00") & ":" & Format(Int(t1Mem / 60) Mod 60, "00") & ":" & Format(t1Mem Mod 60, "00")
    t1Mem = myArr(amh1): t2Mem = myArr(amh2)
    For i = 1 To 3
        t1Mem = t1Mem * 256 + myArr(amh1 + i): t2Mem = t2Mem * 256 + myArr(amh2 + i)
    Next i
    If transType = T14SG Then t1 = myArr(aml1): t2 = myArr(aml2): l = 100: s = "00" Else t1 = 0: t2 = 0: l = 10: s = "0"
    If transType = T18SZ Then t1Mem = t1Mem / 1000: t2Mem = t2Mem / 1000 _
                         Else t1Mem = (t1Mem * 10 + t1) / 100: t2Mem = (t2Mem * 10 + t2) / 100
    uf.t1MemoryValue = Format(Int(t1Mem / 60), "00") & ":" & Format(Int(t1Mem) Mod 60, "00") & "." & Format(Int(t1Mem * l * 1.000000001) Mod l, s)
    uf.t2MemoryValue = Format(Int(t2Mem / 60), "00") & ":" & Format(Int(t2Mem) Mod 60, "00") & "." & Format(Int(t2Mem * l * 1.000000001) Mod l, s)
    If (myArr(af1) And 8) Then s1 = "Remainder (v)" Else s1 = "Elapsed (^)"
    If (myArr(af2) And 8) Then s2 = "Remainder (v)" Else s2 = "Elapsed (^)"
    t1 = myArr(aa1) * 256 + myArr(aa1 + 1): uf.t1Alarm = Format(Int(t1 / 60), "00") & ":" & Format(t1 Mod 60, "00") & "  " & s1
    t2 = myArr(aa2) * 256 + myArr(aa2 + 1): uf.t2Alarm = Format(Int(t2 / 60), "00") & ":" & Format(t2 Mod 60, "00") & "  " & s2
    If (myArr(af1) And 2) Then uf.t1Mode = "Down" Else uf.t1Mode = "Up"
    If (myArr(af2) And 2) Then uf.t2Mode = "Down" Else uf.t2Mode = "Up"
    t1 = 4: If transType = T18SZ Then t2 = 2 Else t2 = 4
    If (myArr(atm1) And t1) Then uf.t1Memory = "ON" Else uf.t1Memory = "OFF"
    If (myArr(atm2) And t2) Then uf.t2Memory = "ON" Else uf.t2Memory = "OFF"
    If transType = T14SG Then
        If (myArr(af1) And 128) Then uf.t1Speech = "Active" Else uf.t1Speech = "Inhibit"
        If (myArr(af2) And 128) Then uf.t2Speech = "Active" Else uf.t2Speech = "Inhibit"
    End If  ' transType = T14SG
    If transType <> T8FG Then
        If transType = T18SZ Then t1 = Int(myArr(av1) / 16) Mod 4: t2 = Int(myArr(av2) / 16) Mod 4 Else t1 = myArr(av1): t2 = myArr(av2)
        If t1 Then uf.t1Vibrator = "Type " & t1 Else uf.t1Vibrator = "Inhibit"
        If t2 Then uf.t2Vibrator = "Type " & t2 Else uf.t2Vibrator = "Inhibit"
    End If  ' transType <> T8FG
    getHardware ar1
    uf.t1ResetHw = hwCtrl & "      " & hwPos & "      " & hwRev & "      " & hwSym
    getHardware ar2
    uf.t2ResetHw = hwCtrl & "      " & hwPos & "      " & hwRev & "      " & hwSym
    getHardware ab1
    uf.t1StartHw = hwCtrl & "      " & hwPos & "      " & hwRev & "      " & hwSym
    getHardware ab2
    uf.t2StartHw = hwCtrl & "      " & hwPos & "      " & hwRev & "      " & hwSym
    getHardware ae1
    uf.t1StopHw = hwCtrl & "      " & hwPos & "      " & hwRev & "      " & hwSym
    getHardware ae2
    uf.t2StopHw = hwCtrl & "      " & hwPos & "      " & hwRev & "      " & hwSym
    If transType = T18SZ Then
        aa1 = ((myArr(ab1 - 1) And 64) / 16) Or ((myArr(ar1 - 1) And 64) / 32) Or ((myArr(ae1 - 1) And 64) / 64)
        aa2 = ((myArr(ab2 - 1) And 64) / 16) Or ((myArr(ar2 - 1) And 64) / 32) Or ((myArr(ae2 - 1) And 64) / 64)
    Else
        aa1 = (myArr(af1) And 112) / 16: aa2 = (myArr(af2) And 112) / 16
    End If  ' transType = T18SZ
    If (aa1 And 2) Then uf.t1ResetAlt = "Alternate" Else uf.t1ResetAlt = "Normal"
    If (aa1 And 4) Then uf.t1StartAlt = "Alternate" Else uf.t1StartAlt = "Normal"
    If (aa1 And 1) Then uf.t1StopAlt = "Alternate" Else uf.t1StopAlt = "Normal"
    If (aa2 And 2) Then uf.t2ResetAlt = "Alternate" Else uf.t2ResetAlt = "Normal"
    If (aa2 And 4) Then uf.t2StartAlt = "Alternate" Else uf.t2StartAlt = "Normal"
    If (aa2 And 1) Then uf.t2StopAlt = "Alternate" Else uf.t2StopAlt = "Normal"
    uf.Show
End Sub ' timerSettings

Public Sub functionName()
    Dim uf As Object
    If transType <> T18SZ Then Exit Sub
    Set uf = functionNameForm
    If modelType = Heli Then uf.aux8Label = "AUX8" Else uf.aux8Label = "MOT"
    uf.aux1Name = fa(31): uf.aux1Abbr = auxFunctionAbbr(0)
    uf.aux2Name = fa(30): uf.aux2Abbr = auxFunctionAbbr(1)
    uf.aux3Name = fa(29): uf.aux3Abbr = auxFunctionAbbr(2)
    uf.aux4Name = fa(28): uf.aux4Abbr = auxFunctionAbbr(3)
    uf.aux5Name = fa(27): uf.aux5Abbr = auxFunctionAbbr(4)
    uf.aux6Name = fa(26): uf.aux6Abbr = auxFunctionAbbr(5)
    uf.aux7Name = fa(25): uf.aux7Abbr = auxFunctionAbbr(6)
    uf.aux8Name = fa(24): uf.aux8Abbr = auxFunctionAbbr(7)
    uf.Show
End Sub ' functionName

Function trainerChSwitch$(ByVal b As Byte, ByVal f%)
    If f = 0 Then trainerChSwitch = "": Exit Function
    If b = 255 Then trainerChSwitch = "--" Else trainerChSwitch = hwCtrlDesc(b Mod 16) & "   " & trainerChSwDesc(Int(b / 64) Mod 4)
End Function    ' trainerChSwitch

Function trainerRate$(ByVal b As Byte, ByVal f%, ByVal s$)
    If f = 0 Then trainerRate = "": Exit Function
    If b < 128 Then trainerRate = CStr(b) & s Else trainerRate = CStr(b - 256) & s
End Function    ' trainerRate

Public Sub trainer()
    Dim uf As Object, i%, aa&, ah&, am&, ar&, ac&, mh&, f As Boolean, b As Boolean, s$(chMax - 1), md%(chMax - 1)
    Set uf = trainerForm
    f = (transType = T18SZ): uf.hwLabel.Enabled = f: uf.teacherLabel.Enabled = f: uf.teacherStudent.Enabled = f
    If f Then aa = addr18trnSet: ah = addr18trnHw: am = addr18trnMd: ar = addr18trnRt: ac = addr18trnSC _
         Else aa = addr14trnSet: ah = addr14trnHw: am = addr14trnMd: ar = addr14trnRt: ac = addr14trnSC
    b = (myArr(aa) And 1): uf.masterHw.Enabled = b: uf.masterAltLabel.Enabled = b: uf.masterAlt.Enabled = b
    If myArr(aa) < 128 Then
        uf.trainerActive = "INH"
    Else
        If f And Not b Then uf.trainerActive = "ACT" Else uf.trainerActive = "OFF"
    End If  ' b
    If b Then uf.teacherStudent = "Teacher" Else uf.teacherStudent = "Student": uf.masterHw = "--      ON": uf.masterAlt = "Normal"
    uf.channelMode = trainerChModeDesc(Int(myArr(aa) / 2) Mod 4)
    If b Then
        getHardware ah
        uf.masterHw = hwCtrl & "    " & hwPos & "    " & hwRev & "    " & hwSym
        If (myArr(aa) And 64) Then uf.masterAlt = "Alternate" Else uf.masterAlt = "Normal"
    End If  ' f
    uf.ch1Function = fa(functn(1)): uf.ch2Function = fa(functn(2)): uf.ch3Function = fa(functn(3)): uf.ch4Function = fa(functn(4))
    uf.ch5Function = fa(functn(5)): uf.ch6Function = fa(functn(6)): uf.ch7Function = fa(functn(7)): uf.ch8Function = fa(functn(8))
    uf.ch9Function = fa(functn(9)): uf.ch10Function = fa(functn(10)): uf.ch11Function = fa(functn(11)): uf.ch12Function = fa(functn(12))
    For i = 0 To 11
        If i Mod 4 Then j = j * 4 Else j = 1
        md(i) = Int(myArr(am + Int(i / 4)) / j) Mod 4
    Next i
    uf.ch1Mode = trainerModeDesc(md(0)): uf.ch2Mode = trainerModeDesc(md(1)): uf.ch3Mode = trainerModeDesc(md(2)): uf.ch4Mode = trainerModeDesc(md(3))
    uf.ch5Mode = trainerModeDesc(md(4)): uf.ch6Mode = trainerModeDesc(md(5)): uf.ch7Mode = trainerModeDesc(md(6)): uf.ch8Mode = trainerModeDesc(md(7))
    uf.ch9Mode = trainerModeDesc(md(8)): uf.ch10Mode = trainerModeDesc(md(9)): uf.ch11Mode = trainerModeDesc(md(10)): uf.ch12Mode = trainerModeDesc(md(11))
    If f Then ss = "" Else ss = "%"
    For i = 0 To 11
        s(i) = trainerRate(myArr(ar + i), md(i), ss)
    Next i
    uf.ch1Rate = s(0): uf.ch2Rate = s(1): uf.ch3Rate = s(2): uf.ch4Rate = s(3): uf.ch5Rate = s(4): uf.ch6Rate = s(5)
    uf.ch7Rate = s(6): uf.ch8Rate = s(7): uf.ch9Rate = s(8): uf.ch10Rate = s(9): uf.ch11Rate = s(10): uf.ch12Rate = s(11)
    For i = 0 To 11
        If md(i) = 0 Then s(i) = "" Else If myArr(ac + i) = 255 Then s(i) = "INH" Else s(i) = "Ch " & CStr(myArr(ac + i) + 1)
    Next i
    uf.ch1StuCh = s(0): uf.ch2StuCh = s(1): uf.ch3StuCh = s(2): uf.ch4StuCh = s(3): uf.ch5StuCh = s(4): uf.ch6StuCh = s(5)
    uf.ch7StuCh = s(6): uf.ch8StuCh = s(7): uf.ch9StuCh = s(8): uf.ch10StuCh = s(9): uf.ch11StuCh = s(10): uf.ch12StuCh = s(11)
    uf.ch13Label.Visible = f: uf.ch13Function.Visible = f: uf.ch13Mode.Visible = f: uf.ch13Hw.Visible = f: uf.ch13Rate.Visible = f: uf.ch13StuCh.Visible = f
    uf.ch14Label.Visible = f: uf.ch14Function.Visible = f: uf.ch14Mode.Visible = f: uf.ch14Hw.Visible = f: uf.ch14Rate.Visible = f: uf.ch14StuCh.Visible = f
    uf.ch15Label.Visible = f: uf.ch15Function.Visible = f: uf.ch15Mode.Visible = f: uf.ch15Hw.Visible = f: uf.ch15Rate.Visible = f: uf.ch15StuCh.Visible = f
    uf.ch16Label.Visible = f: uf.ch16Function.Visible = f: uf.ch16Mode.Visible = f: uf.ch16Hw.Visible = f: uf.ch16Rate.Visible = f: uf.ch16StuCh.Visible = f
    If f Then
        uf.ch13Function = fa(functn(13)): uf.ch14Function = fa(functn(14)): uf.ch15Function = fa(functn(15)): uf.ch16Function = fa(functn(16))
        For i = t14Channels To chMax - 1
            If i Mod 4 Then j = j * 4 Else j = 1
            md(i) = Int(myArr(addr18trnMdH + Int((i - 12) / 4)) / j) Mod 4
        Next i
        uf.ch13Mode = trainerModeDesc(md(12)): uf.ch14Mode = trainerModeDesc(md(13)): uf.ch15Mode = trainerModeDesc(md(14)): uf.ch16Mode = trainerModeDesc(md(15))
        For i = 0 To chMax - 1
            If i < 12 Then a = myArr(addr18trnCS + i) Else a = myArr(addr18trnCSH - 12 + i)
            s(i) = trainerChSwitch(a, md(i))
        Next i
        uf.ch1Hw = s(0): uf.ch2Hw = s(1): uf.ch3Hw = s(2): uf.ch4Hw = s(3): uf.ch5Hw = s(4): uf.ch6Hw = s(5): uf.ch7Hw = s(6): uf.ch8Hw = s(7)
        uf.ch9Hw = s(8): uf.ch10Hw = s(9): uf.ch11Hw = s(10): uf.ch12Hw = s(11): uf.ch13Hw = s(12): uf.ch14Hw = s(13): uf.ch15Hw = s(14): uf.ch16Hw = s(15)
        For i = t14Channels To chMax - 1
            s(i) = trainerRate(myArr(addr18trnRtH + i - 12), md(i), "")
        Next i
        uf.ch13Rate = s(12): uf.ch14Rate = s(13): uf.ch15Rate = s(14): uf.ch16Rate = s(15)
        For i = t14Channels To chMax - 1
            If md(i) = 0 Then s(i) = "" Else If myArr(ac + i) = 255 Then s(i) = "INH" Else s(i) = "Ch " & CStr(myArr(ac + i) + 1)
        Next i
        uf.ch13StuCh = s(12): uf.ch14StuCh = s(13): uf.ch15StuCh = s(14): uf.ch16StuCh = s(15)
    End If  ' f
    uf.Show
End Sub ' trainer

Public Sub warningSettings()
    Const af14 = 143: Const a14 = 1685: Const a18 = 25260
    Dim uf As Object, i%, j%, a&, s$(7), v$(7), f As Boolean
    Set uf = warningSettingsForm
    f = (transType = T18SZ): If f Then a = a18 Else a = a14
    If f Then
        For i = 0 To 7
            If (myArr(a + i) And 4) Then s(i) = "Buzzer" Else s(i) = "Inhibit"
            If (myArr(a + i) And 3) Then v(i) = "Type " & CStr(myArr(a + i) And 3) Else v(i) = "Inhibit"
        Next i
        uf.conditionLabel = "Condition": uf.conditionAlarm = s(0): uf.conditionVibes = v(0)
        uf.throttleCutLabel = "Throttle cut": uf.throttleCutAlarm = s(1): uf.throttleCutVibes = v(1)
        uf.idleDownLabel = "Idle down": uf.idleDownAlarm = s(2): uf.idleDownVibes = v(2)
        uf.throttlePositionLabel = "Throttle/motor position": uf.throttlePositionAlarm = s(3): uf.throttlePositionVibes = v(3)
        uf.motorLabel = "Motor": uf.motorAlarm = s(4): uf.motorVibes = v(4)
        Select Case modelType
        Case Plane
            uf.airBrakeLabel = "Air brake": uf.airBrakeAlarm = s(5): uf.airBrakeVibes = v(5)
            uf.snapRollLabel = "Snap roll": uf.snapRollAlarm = s(6): uf.snapRollVibes = v(6)
        Case Heli
            uf.airBrakeLabel = "Throttle hold": uf.airBrakeAlarm = s(5): uf.airBrakeVibes = v(5)
            uf.snapRollLabel = "": uf.snapRollAlarm = "": uf.snapRollVibes = ""
        Case Glider
            uf.airBrakeLabel = "Trim mix 1": uf.airBrakeAlarm = s(5): uf.airBrakeVibes = v(5)
            uf.snapRollLabel = "Trim mix 2": uf.snapRollAlarm = s(6): uf.snapRollVibes = v(6)
        Case Multi
            uf.airBrakeLabel = "": uf.airBrakeAlarm = "": uf.airBrakeVibes = ""
            uf.snapRollLabel = "": uf.snapRollAlarm = "": uf.snapRollVibes = ""
        End Select  ' Case modelType
        uf.lowBatteryLabel = "": uf.lowBatteryVibes = ""
    Else
        j = 1
        For i = 0 To 7
            If (myArr(af14) And j) Then s(i) = "ON" Else s(i) = "OFF"
            j = j * 2
            If transType <> T14SG Then v(i) = "" Else If (myArr(a + i) And 7) Then v(i) = "TYPE" & CStr(myArr(a + i) And 7) Else v(i) = "OFF"
        Next i
        Select Case modelType
        Case Plane
            uf.conditionLabel = "Throttle cut": uf.conditionAlarm = s(1): uf.conditionVibes = v(1)
            uf.throttleCutLabel = "Idle down": uf.throttleCutAlarm = s(2): uf.throttleCutVibes = v(2)
            uf.idleDownLabel = "Throttle position": uf.idleDownAlarm = s(3): uf.idleDownVibes = v(3)
            uf.throttlePositionLabel = "Snap roll": uf.throttlePositionAlarm = s(4): uf.throttlePositionVibes = v(4)
            uf.motorLabel = "Motor position": uf.motorAlarm = s(5): uf.motorVibes = v(5)
            uf.airBrakeLabel = "Air brake": uf.airBrakeAlarm = s(6): uf.airBrakeVibes = v(6)
            uf.snapRollLabel = "Motor": uf.snapRollAlarm = s(7): uf.snapRollVibes = v(7)
        Case Heli
            uf.conditionLabel = "Condition": uf.conditionAlarm = s(0): uf.conditionVibes = v(0)
            uf.throttleCutLabel = "Throttle cut": uf.throttleCutAlarm = s(1): uf.throttleCutVibes = v(1)
            uf.idleDownLabel = "Throttle position": uf.idleDownAlarm = s(3): uf.idleDownVibes = v(3)
            uf.throttlePositionLabel = "Throttle hold": uf.throttlePositionAlarm = s(5): uf.throttlePositionVibes = v(5)
            uf.motorLabel = "": uf.motorAlarm = "": uf.motorVibes = ""
            uf.airBrakeLabel = "": uf.airBrakeAlarm = "": uf.airBrakeVibes = ""
            uf.snapRollLabel = "": uf.snapRollAlarm = "": uf.snapRollVibes = ""
        Case Glider
            uf.conditionLabel = "Condition": uf.conditionAlarm = s(0): uf.conditionVibes = v(0)
            uf.throttleCutLabel = "Motor position": uf.throttleCutAlarm = s(5): uf.throttleCutVibes = v(5)
            uf.idleDownLabel = "Trim mix": uf.idleDownAlarm = s(6): uf.idleDownVibes = v(6)
            uf.throttlePositionLabel = "Motor": uf.throttlePositionAlarm = s(7): uf.throttlePositionVibes = v(7)
            uf.motorLabel = "": uf.motorAlarm = "": uf.motorVibes = ""
            uf.airBrakeLabel = "": uf.airBrakeAlarm = "": uf.airBrakeVibes = ""
            uf.snapRollLabel = "": uf.snapRollAlarm = "": uf.snapRollVibes = ""
        Case Multi
            uf.conditionLabel = "Throttle cut": uf.conditionAlarm = s(1): uf.conditionVibes = v(1)
            uf.throttleCutLabel = "Throttle position": uf.throttleCutAlarm = s(3): uf.throttleCutVibes = v(3)
            uf.idleDownLabel = "": uf.idleDownAlarm = "": uf.idleDownVibes = ""
            uf.throttlePositionLabel = "": uf.throttlePositionAlarm = "": uf.throttlePositionVibes = ""
            uf.motorLabel = "": uf.motorAlarm = "": uf.motorVibes = ""
            uf.airBrakeLabel = "": uf.airBrakeAlarm = "": uf.airBrakeVibes = ""
            uf.snapRollLabel = "": uf.snapRollAlarm = "": uf.snapRollVibes = ""
        End Select  ' Case modelType
        If transType = T14SG Then
            uf.lowBatteryLabel = "Low battery"
            If (myArr(a - 1) And 7) Then uf.lowBatteryVibes = "TYPE" & CStr(myArr(a - 1) And 7) Else uf.lowBatteryVibes = "OFF"
        Else
            uf.vibratorLabel = "": uf.lowBatteryLabel = "": uf.lowBatteryVibes = ""
        End If  ' transType = T14SG
    End If  ' f
    uf.Show
End Sub ' warningSettings

Public Sub userMenuSettings()
    Const aum14 = 1614: Const menuNumber14 = 10: Const aum18 = 25268: Const menuNumber18 = 18
    Const menuList18 = "---,Display,Sound volume,Date and Time,--<?????>--,Battery,Range check,S.Bus servo,Information,Servo monitor,Servo reverse," & _
                    "Model select,Model type,--<?????>--,End point,Servo speed,Sub-trim,Function,Fail safe,System type,T1-T6 setting,Throttle cut," & _
                    "Idle down,Swash ring,Swash,Stick alarm,Timer,Function name,Sensor,Telemetry,Tele. setting,Trainer,Warning setting,User menu setting," & _
                    "Data reset,Condition select,AFR,Dual rate,Program. mixes,Aileron differential,Flap setting,AIL -› Camber flap,AIL -› Brake flap," & _
                    "Aileron -› Rudder,Elevator -› Camber,Camber mixing,Airbrake -› ELE,Camber flap -› ELE,Rudder -› Aileron,Rudder -› Elevator,Snap roll," & _
                    "Butterfly,Air brake,Trim mix 1,Trim mix 2,Motor,Gyro,Ailevator,V-Tail,Winglet,Acceleration,Pitch curve,Throttle curve,Acceleration," & _
                    "Throttle hold,Swash mixing,Throttle mixing,Pitch -› Needle,Pitch -› Rudder,Gyro,Governor,H/W setting,---,---,---,---,---,---,---,---," & _
                    "---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---"
    Const menuList14 = "Servo monitor,Condition select,Dual rate,Program. mixes,--<?????>--,Throttle curve,Throttle delay,Aileron differential," & _
                    "Flap setting,AIL -› Camber flap,AIL -› Brake flap,Aileron -› Rudder,Camber mixing,Elevator -› Camber,Camber flap -› ELE," & _
                    "Rudder -› Aileron,Rudder -› Elevator,Snap roll,Air brake,Pitch curve,Throttle curve,Throttle hold,Swash mixing,Throttle mixing," & _
                    "Butterfly,Trim mix,Pitch -› Rudder,Fuel mixing,Fuel mixing,Gyro,Gyro,Governor,Motor,V-Tail,Ailevator,Winglet,Model select," & _
                    "Model type,System type,Function,Sub-trim,Servo reverse,Fail safe,End point,Servo speed,Throttle cut,Idle down,Swash ring,Swash," & _
                    "T1-T4 setting,Warning setting,Data reset,Trainer,Display,User name,H/W setting,Information,Sound setting,Start select," & _
                    "Auto lock,S.Bus servo,Sensor,Telemetry,Tele. setting,Stick alarm,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---," & _
                    "---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---"
    Const menuList8 = "Servo monitor,Condition select,Dual rate,Program. mixes,--<?????>--,Throttle curve,Throttle delay,Aileron differential," & _
                    "Flap setting,AIL -› Camber flap,AIL -› Brake flap,Aileron -› Rudder,Camber mixing,Elevator -› Camber,Camber flap -› ELE," & _
                    "Rudder -› Aileron,Rudder -› Elevator,Snap roll,Air brake,Pitch curve,Throttle curve,Throttle hold,Swash mixing,Throttle mixing," & _
                    "Butterfly,Trim mix,Pitch -› Rudder,Fuel mixing,Fuel mixing,Gyro,Gyro,Governor,Motor,V-Tail,Ailevator,Winglet,Model select," & _
                    "Model type,Frequency,Function,Sub-trim,Servo reverse,Fail safe,End point,Throttle cut,Idle down,Swash ring,Swash,T1-T4 setting," & _
                    "Warning setting,Data reset,Trainer,Display,User name,H/W setting,Information,Sound setting,Start select,Auto lock,---,---,---," & _
                    "---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---,---"
    Dim uf As Object, i%, d%, aum&, mn%, menuDesc$(), menuItem(menuNumber18 - 1), f As Boolean
    f = (transType = T18SZ)
    If f Then aum = aum18: mn = menuNumber18: d = 1: menuDesc = Split(menuList18, mySep) _
         Else aum = aum14: mn = menuNumber14: d = 5: If transType = T8FG Then menuDesc = Split(menuList8, mySep) Else menuDesc = Split(menuList14, mySep)
    Set uf = userMenuForm
    For i = 0 To mn - 1
        If myArr(aum + i) = 255 Then menuItem(Int(i / d) + (i Mod d) * 3) = "---" Else menuItem(Int(i / d) + (i Mod d) * 3) = menuDesc(myArr(aum + i))
    Next i
    uf.item1 = menuItem(0): uf.item2 = menuItem(1): uf.item3 = menuItem(2): uf.item4 = menuItem(3): uf.item5 = menuItem(4): uf.item6 = menuItem(5)
    uf.item7 = menuItem(6): uf.item8 = menuItem(7): uf.item9 = menuItem(8): uf.item10 = menuItem(9): uf.item11 = menuItem(10): uf.item12 = menuItem(11)
    uf.item13 = menuItem(12): uf.item14 = menuItem(13): uf.item15 = menuItem(14): uf.item16 = menuItem(15): uf.item17 = menuItem(16): uf.item18 = menuItem(17)
    uf.item3.Visible = f: uf.item6.Visible = f: uf.item9.Visible = f: uf.item12.Visible = f
    uf.item15.Visible = f: uf.item16.Visible = f: uf.item17.Visible = f: uf.item18.Visible = f
    uf.Show
End Sub ' userMenuSettings

Public Sub trims()
    Dim uf As Object, i%, j%, aa&, ab&, atr&, ats&, tn%, cn%, f As Boolean, stn$(1 To maxTrims), sts$(1 To maxTrims), tt$(1 To maxTrims, 1 To maxConds), scn$(1 To maxConds)
    Set uf = trimSetForm
    uf.Caption = "Trim"
    f = (transType = T18SZ)
    If f Then atr = addr18Trims: ats = addr18trStep: tn = numb18trims: cn = t18Conditions Else atr = addr14Trims: ats = addr14trStep: tn = numb14trims: cn = t14Conditions
    For i = 1 To maxTrims
        If i > tn Then stn(i) = "": sts(i) = "" Else stn(i) = hwCtrlDesc(i + 19): sts(i) = CStr(myArr(ats + i - 1))
    Next i
    For i = 1 To maxConds
        If i > cn Then f = False Else f = (conditionList(i))
        If f Then scn(i) = conditionName(conditionList(i)) Else scn(i) = ""
        For j = 1 To maxTrims
            If f And (j <= tn) Then
                aa = atr + ((conditionList(i) - 1) * tn + j - 1) * 2: ab = myArr(aa) * 256& + myArr(aa + 1): If ab > 32768 Then ab = ab - 65536
                If ab < 0 Then tt(j, i) = "" Else tt(j, i) = "+"
                tt(j, i) = tt(j, i) & CStr(Int(ab / 32 / myArr(ats + j - 1)))
            Else
                tt(j, i) = ""
            End If  ' f
        Next j
    Next i
    uf.t1Label = stn(1): uf.t2Label = stn(2): uf.t3Label = stn(4): uf.t4Label = stn(3): uf.t5Label = stn(5): uf.t6Label = stn(6): uf.t7Label = stn(7)
    uf.t1Step = sts(1): uf.t2Step = sts(2): uf.t3Step = sts(4): uf.t4Step = sts(3): uf.t5Step = sts(5): uf.t6Step = sts(6): uf.t7Step = sts(7)
    uf.cond1Label = scn(1): uf.cond2Label = scn(2): uf.cond3Label = scn(3): uf.cond4Label = scn(4)
    uf.cond5Label = scn(5): uf.cond6Label = scn(6): uf.cond7Label = scn(7): uf.cond8Label = scn(8)
    uf.t1c1Mode = tt(1, 1): uf.t2c1Mode = tt(2, 1): uf.t3c1Mode = tt(4, 1): uf.t4c1Mode = tt(3, 1): uf.t5c1Mode = tt(5, 1): uf.t6c1Mode = tt(6, 1): uf.t7c1Mode = tt(7, 1)
    uf.t1c2Mode = tt(1, 2): uf.t2c2Mode = tt(2, 2): uf.t3c2Mode = tt(4, 2): uf.t4c2Mode = tt(3, 2): uf.t5c2Mode = tt(5, 2): uf.t6c2Mode = tt(6, 2): uf.t7c2Mode = tt(7, 2)
    uf.t1c3Mode = tt(1, 3): uf.t2c3Mode = tt(2, 3): uf.t3c3Mode = tt(4, 3): uf.t4c3Mode = tt(3, 3): uf.t5c3Mode = tt(5, 3): uf.t6c3Mode = tt(6, 3): uf.t7c3Mode = tt(7, 3)
    uf.t1c4Mode = tt(1, 4): uf.t2c4Mode = tt(2, 4): uf.t3c4Mode = tt(4, 4): uf.t4c4Mode = tt(3, 4): uf.t5c4Mode = tt(5, 4): uf.t6c4Mode = tt(6, 4): uf.t7c4Mode = tt(7, 4)
    uf.t1c5Mode = tt(1, 5): uf.t2c5Mode = tt(2, 5): uf.t3c5Mode = tt(4, 5): uf.t4c5Mode = tt(3, 5): uf.t5c5Mode = tt(5, 5): uf.t6c5Mode = tt(6, 5): uf.t7c5Mode = tt(7, 5)
    uf.t1c6Mode = tt(1, 6): uf.t2c6Mode = tt(2, 6): uf.t3c6Mode = tt(4, 6): uf.t4c6Mode = tt(3, 6): uf.t5c6Mode = tt(5, 6): uf.t6c6Mode = tt(6, 6): uf.t7c6Mode = tt(7, 6)
    uf.t1c7Mode = tt(1, 7): uf.t2c7Mode = tt(2, 7): uf.t3c7Mode = tt(4, 7): uf.t4c7Mode = tt(3, 7): uf.t5c7Mode = tt(5, 7): uf.t6c7Mode = tt(6, 7): uf.t7c7Mode = tt(7, 7)
    uf.t1c8Mode = tt(1, 8): uf.t2c8Mode = tt(2, 8): uf.t3c8Mode = tt(4, 8): uf.t4c8Mode = tt(3, 8): uf.t5c8Mode = tt(5, 8): uf.t6c8Mode = tt(6, 8): uf.t7c8Mode = tt(7, 8)
    uf.Show
End Sub ' trims

Public Sub conditionDelay(ByVal cond%)
    Dim uf As Object, i%, ch%, ax&, ac&, d$(1 To chMax)
    If transType = T18SZ Then ch = t18Channels: ax = addr18fnXC: ac = addr18CondStart + t18CondLength * (conditionList(cond + 1) - 1) + ch * 2 _
                         Else ch = t14Channels: ax = addr14fnXC: ac = addr14CondDelay + ch * (conditionList(cond + 1) - 1)
    For i = 1 To chMax
        If (i > ch) Or ((transType = T18SZ) And ((functn(i) = 22) Or (functn(i) = 23))) Then d(i) = "" Else d(i) = CStr(cServoSpeed(myArr(ac + myArr(ax + functn(i)))))
    Next i
    Set uf = conditionForm
    uf.ch1Delay = d(1): uf.ch2Delay = d(2): uf.ch3Delay = d(3): uf.ch4Delay = d(4): uf.ch5Delay = d(5): uf.ch6Delay = d(6): uf.ch7Delay = d(7): uf.ch8Delay = d(8)
    uf.ch9Delay = d(9): uf.ch10Delay = d(10): uf.ch11Delay = d(11): uf.ch12Delay = d(12): uf.ch13Delay = d(13): uf.ch14Delay = d(14): uf.ch15Delay = d(15): uf.ch16Delay = d(16)
End Sub ' conditionDelay

Public Sub conditionSelect()
    Dim uf As Object, i%, cn%, ch%, s$(1 To 3, 1 To maxConds), d$(1 To 2, 1 To chMax)
    If transType = T18SZ Then cn = t18Conditions: ch = t18Channels Else cn = t14Conditions: ch = t14Channels
    Set uf = conditionForm
    uf.conditionSelect.Clear
    For i = 1 To maxConds
        If (i > cn) Or (conditionList(i) = 0) Then
            s(1, i) = "": s(2, i) = "": s(3, i) = ""
        Else
            s(1, i) = CStr(i): s(2, i) = conditionName(conditionList(i)): If i = 1 Then s(3, i) = "" Else s(3, i) = logicSwitch(conditionHw(conditionList(i)))
            uf.conditionSelect.AddItem conditionName(conditionList(i))
        End If  ' (i > cn) Or (conditionList(i) = 0)
    Next i
    uf.Label1 = s(1, 1): uf.cond1 = s(2, 1): uf.cond1Hw = s(3, 1): uf.Label2 = s(1, 2): uf.cond2 = s(2, 2): uf.cond2Hw = s(3, 2)
    uf.Label3 = s(1, 3): uf.cond3 = s(2, 3): uf.cond3Hw = s(3, 3): uf.Label4 = s(1, 4): uf.cond4 = s(2, 4): uf.cond4Hw = s(3, 4)
    uf.Label5 = s(1, 5): uf.cond5 = s(2, 5): uf.cond5Hw = s(3, 5): uf.Label6 = s(1, 6): uf.cond6 = s(2, 6): uf.cond6Hw = s(3, 6)
    uf.Label7 = s(1, 7): uf.cond7 = s(2, 7): uf.cond7Hw = s(3, 7): uf.Label8 = s(1, 8): uf.cond8 = s(2, 8): uf.cond8Hw = s(3, 8)
    For i = 1 To chMax
        If i > ch Then d(1, i) = "" Else d(1, i) = fa(functn(i))
        If (transType <> T18SZ) Or (functn(i) = 22) Or (functn(i) = 23) Then d(2, i) = "" Else d(2, i) = fnGrouping(i, 2)
    Next i
    uf.ch1Function = d(1, 1): uf.ch2Function = d(1, 2): uf.ch3Function = d(1, 3): uf.ch4Function = d(1, 4)
    uf.ch5Function = d(1, 5): uf.ch6Function = d(1, 6): uf.ch7Function = d(1, 7): uf.ch8Function = d(1, 8)
    uf.ch9Function = d(1, 9): uf.ch10Function = d(1, 10): uf.ch11Function = d(1, 11): uf.ch12Function = d(1, 12)
    uf.ch13Function = d(1, 13): uf.ch14Function = d(1, 14): uf.ch15Function = d(1, 15): uf.ch16Function = d(1, 16)
    uf.ch1dGrp = d(2, 1): uf.ch2dGrp = d(2, 2): uf.ch3dGrp = d(2, 3): uf.ch4dGrp = d(2, 4)
    uf.ch5dGrp = d(2, 5): uf.ch6dGrp = d(2, 6): uf.ch7dGrp = d(2, 7): uf.ch8dGrp = d(2, 8)
    uf.ch9dGrp = d(2, 9): uf.ch10dGrp = d(2, 10): uf.ch11dGrp = d(2, 11): uf.ch12dGrp = d(2, 12)
    uf.ch13dGrp = d(2, 13): uf.ch14dGrp = d(2, 14): uf.ch15dGrp = d(2, 15): uf.ch16dGrp = d(2, 16)
    If transType <> T18SZ Then uf.ch13Label = "": uf.ch14Label = "": uf.ch15Label = "": uf.ch16Label = ""
    uf.conditionSelect.ListIndex = 0
    uf.Show
End Sub ' conditionSelect

Function afrAvailable(ByVal f%) As Boolean
    afrAvailable = (f = 0) Or (f = 1) Or (f = 3) Or (f = 4) Or (f = 11) Or (f >= 24) Or (((f = 2) Or (f = 5) Or ((f >= 16) And (f <= 19))) And (modelType <> Heli)) Or _
        ((modelType = Plane) And ((f = 9) Or (f = 11) Or (f >= 22))) Or ((modelType = Heli) And (f = 7)) Or ((modelType = Glider) And (f >= 21)) Or _
        ((modelType = Multi) And ((f <= 11) Or ((f >= 16) And (f <= 21))))
End Function    ' afrAvailable

Public Sub getAFRPoint(uf As Object, ByVal cond%, ByVal afr%, ByVal i%)
    Dim j%, k%, ax&, aa&, ab&
    ax = myArr(addr18fnXC + afr): aa = addr18afrStart + t18CondLength * (conditionList(cond + 1) - 1) + t18afrLength * ax
    ab = addr18afrSec + addr18CondNameOffset * (conditionList(cond + 1) - 1) + t18afrSecLength * ax
    If normalRate(afr) Then k = 1 Else k = -1
    If i = 0 Then If Not pointActive(ab, currentPoint(afr)) Then i = 1
    For j = 1 To t18PointsMax
        currentPoint(afr) = currentPoint(afr) + i * k
        If currentPoint(afr) > t18PointsMax Then currentPoint(afr) = 1 Else If currentPoint(afr) < 1 Then currentPoint(afr) = t18PointsMax
        If pointActive(ab, currentPoint(afr)) Then Exit For
    Next j
    aa = aa + currentPoint(afr) - 1
    uf.rateA = rate2String(myArr(aa + t18PointsMax) + Int(myArr(aa + t18PointsMax * 2) / 16) * 256, 16 * k)
    uf.rateB = rate2String(myArr(aa) + (myArr(aa + t18PointsMax * 2) Mod 16) * 256, 10.5 * k)
End Sub ' getAFRPoint

Public Sub getAFR(uf As Object, ByVal cond%, ByVal afr%)
    Dim j%, aa&, ab&, ax&, a$, b$, f As Boolean
    ax = myArr(addr18fnXC + afr)
    If ((myArr(addr18afrGrouping + Int(ax / 8)) And Power(2, ax Mod 8))) Then uf.groupFunction = "Group" Else uf.groupFunction = "Single"
    aa = addr18afrStart + t18CondLength * (conditionList(cond + 1) - 1) + t18afrLength * ax
    ab = addr18afrSec + addr18CondNameOffset * (conditionList(cond + 1) - 1) + t18afrSecLength * ax
    If (myArr(aa + t18afrLength - 2) And 128) Then uf.speedMode = "Symmetry" Else uf.speedMode = "Linear"
    uf.speedIn = CStr(cServoSpeed(myArr(aa + t18afrLength - 2) Mod 128)): uf.speedOut = CStr(cServoSpeed(myArr(aa + t18afrLength - 1) Mod 128))
    If myArr(ab + 2) < 128 Then uf.afrGrouping = "Separ." Else uf.afrGrouping = "Comb."
    uf.afrOffset = rate2String((myArr(ab + 11) Mod 16) * 256 + myArr(ab + 10), 10)
    j = Int(myArr(ab + 2) / 8) Mod 4: uf.afrType = curveDesc(j)
    Select Case j
    Case 0
        uf.expoALabel = "EXP A": uf.expoBLabel = "EXP B"
        a = rate2String(Int(myArr(ab + 8) / 16) * 256 + myArr(ab + 6), 10): b = rate2String((myArr(ab + 8) Mod 16) * 256 + myArr(ab + 7), 10)
        If normalRate(afr) Then uf.expoA = a: uf.expoB = b Else uf.expoA = b: uf.expoB = a
    Case 1
        uf.expoALabel = "EXP 2": uf.expoBLabel = "": uf.expoA = rate2String(Int(myArr(ab + 11) / 16) * 256 + myArr(ab + 9), 10): uf.expoB = ""
    End Select  ' Case j
    f = (j > 1): uf.prevButton.Visible = f: uf.nextButton.Visible = f
    If f Then
        uf.rateALabel = "Position": uf.rateBLabel = "Rate": uf.expoALabel = "": uf.expoBLabel = "": uf.expoA = "": uf.expoB = ""
        currentPoint(afr) = 9: getAFRPoint uf, cond, afr, 0
    Else
        uf.rateALabel = "Rate A": uf.rateBLabel = "Rate B"
        a = rate2String(Int(myArr(ab + 5) / 16) * 256 + myArr(ab + 3), 10): b = rate2String((myArr(ab + 5) Mod 16) * 256 + myArr(ab + 4), 10)
        If normalRate(afr) Then uf.rateA = a: uf.rateB = b Else uf.rateA = b: uf.rateB = a
    End If  ' f
End Sub ' getAFR

Public Sub functionRate()
    Dim uf As Object, i%, j%
    If transType <> T18SZ Then Exit Sub
    Set uf = afrForm
    If afrFlag Then
        afrFlag = False: j = 0: uf.functionSelect.Clear
        For i = 1 To t18Channels
            If afrAvailable(functn(i)) Then uf.functionSelect.AddItem fa(functn(i)): afrSequence(j) = i - 1: j = j + 1
        Next i
        For i = j To t18Channels - 1
            afrSequence(j) = -1
        Next i
        uf.functionSelect.ListIndex = 0
    Else
        getAFR uf, uf.conditionSelect.ListIndex, functn(afrSequence(uf.functionSelect.ListIndex) + 1)
    End If  ' afrFlag
End Sub ' functionRate

Public Sub throttleDelayAFR()
    Dim uf As Object, i%, j%
    If transType = T18SZ Then
        afrFlag = True: j = Int(0.5 + t18PointsMax / 2)
        For i = 0 To functionNumber - 2
            currentPoint(i) = j
        Next i
        Set uf = afrForm
        uf.conditionSelect.Clear
        For i = 1 To t18Conditions
            If conditionList(i) Then uf.conditionSelect.AddItem conditionName(conditionList(i))
        Next i
        uf.conditionSelect.ListIndex = 0
    Else
        Set uf = tCutForm
        uf.Caption = "Throttle delay": uf.actLabel = "": uf.tCutAct = "": uf.posLabel = "": uf.tCutPos = ""
        uf.hwLabel = "Delay": uf.tCutHw = CStr(cServoSpeed(myArr(1362))): uf.altLabel = "": uf.tCutAlt = ""
    End If  ' transType = T18SZ
    uf.Show
End Sub

Public Sub getDRPoint(ByVal cond%, ByVal dr%, ByVal i%)
    Dim uf As Object, j%, k%, a&, p$, r$
    Set uf = dualRate18Form
    If normalRate(myArr((addr18drStart + t18CondLength * (conditionList(cond + 1) - 1) + t18drLength * (dr - 1))) Mod 32) Then k = 1 Else k = -1
    a = addr18drSec + addr18CondNameOffset * (conditionList(cond + 1) - 1) + t18drSecLength * (dr - 1): If i = 0 Then If Not pointActive(a, currentPoint(dr)) Then i = 1
    For j = 1 To t18PointsMax
        currentPoint(dr) = currentPoint(dr) + i * k
        If currentPoint(dr) > t18PointsMax Then currentPoint(dr) = 1 Else If currentPoint(dr) < 1 Then currentPoint(dr) = t18PointsMax
        If pointActive(a, currentPoint(dr)) Then Exit For
    Next j
    a = addr18drStart + t18CondLength * (conditionList(cond + 1) - 1) + t18drLength * (dr - 1) + currentPoint(dr) + 3
    r = rate2String(myArr(a) + (myArr(a + t18PointsMax * 2) Mod 16) * 256, 10.5 * k)
    p = rate2String(myArr(a + t18PointsMax) + Int(myArr(a + t18PointsMax * 2) / 16) * 256, 16 * k)
    Select Case dr
    Case 1
        uf.dr1rateA = p: uf.dr1rateB = r
    Case 2
        uf.dr2rateA = p: uf.dr2rateB = r
    Case 3
        uf.dr3rateA = p: uf.dr3rateB = r
    Case 4
        uf.dr4rateA = p: uf.dr4rateB = r
    Case 5
        uf.dr5rateA = p: uf.dr5rateB = r
    Case 6
        uf.dr6rateA = p: uf.dr6rateB = r
    End Select  ' Case dr
End Sub ' getDRPoint

Public Sub dualRate18(ByVal cond%)
    Const drItm = 17
    Dim uf As Object, i%, j%, s$(1 To drItm, 1 To t18dualRates), a$, b$, aa&, ab&, ac&, l&, drf(1 To t18dualRates) As Boolean, be(1 To t18dualRates) As Boolean
    Set uf = dualRate18Form
    aa = addr18drStart + t18CondLength * (conditionList(cond + 1) - 1)
    For i = 1 To t18dualRates
        ab = aa + t18drLength * (i - 1): s(2, i) = fa(myArr(ab) Mod 32): getHardware ab + 1
        If (myArr(ab) And 64) <> 0 Then s(3, i) = hwCtrl & "   " & hwPos & "   " & hwRev & "   " & hwSym & "   Alternate" _
                                   Else s(3, i) = hwCtrl & "    " & hwPos & "    " & hwRev & "    " & hwSym
        drf(i) = (myArr(ab) >= 128)
        If drf(i) Then
            s(1, i) = "ACT": If (myArr(ab + t18drLength - 2) And 128) Then s(4, i) = "Symmetry" Else s(4, i) = "Linear"
            s(5, i) = CStr(cServoSpeed(myArr(ab + t18drLength - 2) Mod 128)): s(6, i) = CStr(cServoSpeed(myArr(ab + t18drLength - 1) Mod 128))
            ac = addr18drSec + addr18CondNameOffset * (conditionList(cond + 1) - 1) + t18drSecLength * (i - 1)
            If myArr(ac + 2) < 128 Then s(8, i) = "Separ." Else s(8, i) = "Comb."
            s(9, i) = rate2String((myArr(ac + 11) Mod 16) * 256 + myArr(ac + 10), 10)
            j = Int(myArr(ac + 2) / 8) Mod 4: s(7, i) = curveDesc(j)
            Select Case j
            Case 0
                s(14, i) = "EXP A": s(15, i) = "EXP B"
                a = rate2String(Int(myArr(ac + 8) / 16) * 256 + myArr(ac + 6), 10): b = rate2String((myArr(ac + 8) Mod 16) * 256 + myArr(ac + 7), 10)
                If normalRate(myArr(ab) Mod 32) Then s(16, i) = a: s(17, i) = b Else s(16, i) = b: s(17, i) = a
            Case 1
                s(14, i) = "EXP 2": s(15, i) = "": s(16, i) = rate2String(Int(myArr(ac + 11) / 16) * 256 + myArr(ac + 9), 10): s(17, i) = ""
            End Select  ' Case j
            be(i) = (j > 1)
            If be(i) Then
                s(10, i) = "Position": s(11, i) = "Rate"
            Else
                s(10, i) = "Rate A": s(11, i) = "Rate B"
                a = rate2String(Int(myArr(ac + 5) / 16) * 256 + myArr(ac + 3), 10): b = rate2String((myArr(ac + 5) Mod 16) * 256 + myArr(ac + 4), 10)
                If normalRate(myArr(ab) Mod 32) Then s(12, i) = a: s(13, i) = b Else s(12, i) = b: s(13, i) = a
            End If  ' be(i)
        Else
            s(1, i) = "INH"
            For j = 4 To drItm
                s(j, i) = ""
            Next j
            be(i) = False
        End If  ' myArr(ab) < 128
    Next i
    uf.dr1Stat = s(1, 1): uf.dr1Func = s(2, 1): uf.dr1Hw = s(3, 1): uf.dr1Mode = s(4, 1): uf.dr1In = s(5, 1): uf.dr1Out = s(6, 1): uf.dr1Type = s(7, 1): uf.dr1Grouping = s(8, 1): uf.dr1Offset = s(9, 1): uf.dr1rateALabel = s(10, 1): uf.dr1rateBLabel = s(11, 1): uf.dr1rateA = s(12, 1): uf.dr1rateB = s(13, 1): uf.dr1expoALabel = s(14, 1): uf.dr1expoBLabel = s(15, 1): uf.dr1expoA = s(16, 1): uf.dr1expoB = s(17, 1)
    uf.dr2Stat = s(1, 2): uf.dr2Func = s(2, 2): uf.dr2Hw = s(3, 2): uf.dr2Mode = s(4, 2): uf.dr2In = s(5, 2): uf.dr2Out = s(6, 2): uf.dr2Type = s(7, 2): uf.dr2Grouping = s(8, 2): uf.dr2Offset = s(9, 2): uf.dr2rateALabel = s(10, 2): uf.dr2rateBLabel = s(11, 2): uf.dr2rateA = s(12, 2): uf.dr2rateB = s(13, 2): uf.dr2expoALabel = s(14, 2): uf.dr2expoBLabel = s(15, 2): uf.dr2expoA = s(16, 2): uf.dr2expoB = s(17, 2)
    uf.dr3Stat = s(1, 3): uf.dr3Func = s(2, 3): uf.dr3Hw = s(3, 3): uf.dr3Mode = s(4, 3): uf.dr3In = s(5, 3): uf.dr3Out = s(6, 3): uf.dr3Type = s(7, 3): uf.dr3Grouping = s(8, 3): uf.dr3Offset = s(9, 3): uf.dr3rateALabel = s(10, 3): uf.dr3rateBLabel = s(11, 3): uf.dr3rateA = s(12, 3): uf.dr3rateB = s(13, 3): uf.dr3expoALabel = s(14, 3): uf.dr3expoBLabel = s(15, 3): uf.dr3expoA = s(16, 3): uf.dr3expoB = s(17, 3)
    uf.dr4Stat = s(1, 4): uf.dr4Func = s(2, 4): uf.dr4Hw = s(3, 4): uf.dr4Mode = s(4, 4): uf.dr4In = s(5, 4): uf.dr4Out = s(6, 4): uf.dr4Type = s(7, 4): uf.dr4Grouping = s(8, 4): uf.dr4Offset = s(9, 4): uf.dr4rateALabel = s(10, 4): uf.dr4rateBLabel = s(11, 4): uf.dr4rateA = s(12, 4): uf.dr4rateB = s(13, 4): uf.dr4expoALabel = s(14, 4): uf.dr4expoBLabel = s(15, 4): uf.dr4expoA = s(16, 4): uf.dr4expoB = s(17, 4)
    uf.dr5Stat = s(1, 5): uf.dr5Func = s(2, 5): uf.dr5Hw = s(3, 5): uf.dr5Mode = s(4, 5): uf.dr5In = s(5, 5): uf.dr5Out = s(6, 5): uf.dr5Type = s(7, 5): uf.dr5Grouping = s(8, 5): uf.dr5Offset = s(9, 5): uf.dr5rateALabel = s(10, 5): uf.dr5rateBLabel = s(11, 5): uf.dr5rateA = s(12, 5): uf.dr5rateB = s(13, 5): uf.dr5expoALabel = s(14, 5): uf.dr5expoBLabel = s(15, 5): uf.dr5expoA = s(16, 5): uf.dr5expoB = s(17, 5)
    uf.dr6Stat = s(1, 6): uf.dr6Func = s(2, 6): uf.dr6Hw = s(3, 6): uf.dr6Mode = s(4, 6): uf.dr6In = s(5, 6): uf.dr6Out = s(6, 6): uf.dr6Type = s(7, 6): uf.dr6Grouping = s(8, 6): uf.dr6Offset = s(9, 6): uf.dr6rateALabel = s(10, 6): uf.dr6rateBLabel = s(11, 6): uf.dr6rateA = s(12, 6): uf.dr6rateB = s(13, 6): uf.dr6expoALabel = s(14, 6): uf.dr6expoBLabel = s(15, 6): uf.dr6expoA = s(16, 6): uf.dr6expoB = s(17, 6)
    uf.dr1Type.Visible = drf(1): uf.dr1Grouping.Visible = drf(1): uf.dr1speedLabel.Visible = drf(1): uf.dr1ModeLabel.Visible = drf(1): uf.dr1InLabel.Visible = drf(1)
    uf.dr1OutLabel.Visible = drf(1): uf.dr1offsetLabel.Visible = drf(1): uf.dr1Offset.Visible = drf(1): uf.dr1rateALabel.Visible = drf(1): uf.dr1rateBLabel.Visible = drf(1)
    uf.dr1rateA.Visible = drf(1): uf.dr1rateB.Visible = drf(1): uf.dr1expoALabel.Visible = drf(1) And Not be(1): uf.dr1expoBLabel.Visible = drf(1) And Not be(1)
    uf.dr1expoA.Visible = drf(1) And Not be(1): uf.dr1expoB.Visible = drf(1) And Not be(1): uf.dr1prevButton.Visible = be(1): uf.dr1nextButton.Visible = be(1)
    uf.dr2Type.Visible = drf(2): uf.dr2Grouping.Visible = drf(2): uf.dr2speedLabel.Visible = drf(2): uf.dr2ModeLabel.Visible = drf(2): uf.dr2InLabel.Visible = drf(2)
    uf.dr2OutLabel.Visible = drf(2): uf.dr2offsetLabel.Visible = drf(2): uf.dr2Offset.Visible = drf(2): uf.dr2rateALabel.Visible = drf(2): uf.dr2rateBLabel.Visible = drf(2)
    uf.dr2rateA.Visible = drf(2): uf.dr2rateB.Visible = drf(2): uf.dr2expoALabel.Visible = drf(2) And Not be(2): uf.dr2expoBLabel.Visible = drf(2) And Not be(2)
    uf.dr2expoA.Visible = drf(2) And Not be(2): uf.dr2expoB.Visible = drf(2) And Not be(2): uf.dr2prevButton.Visible = be(2): uf.dr2nextButton.Visible = be(2)
    uf.dr3Type.Visible = drf(3): uf.dr3Grouping.Visible = drf(3): uf.dr3speedLabel.Visible = drf(3): uf.dr3ModeLabel.Visible = drf(3): uf.dr3InLabel.Visible = drf(3)
    uf.dr3OutLabel.Visible = drf(3): uf.dr3offsetLabel.Visible = drf(3): uf.dr3Offset.Visible = drf(3): uf.dr3rateALabel.Visible = drf(3): uf.dr3rateBLabel.Visible = drf(3)
    uf.dr3rateA.Visible = drf(3): uf.dr3rateB.Visible = drf(3): uf.dr3expoALabel.Visible = drf(3) And Not be(3): uf.dr3expoBLabel.Visible = drf(3) And Not be(3)
    uf.dr3expoA.Visible = drf(3) And Not be(3): uf.dr3expoB.Visible = drf(3) And Not be(3): uf.dr3prevButton.Visible = be(3): uf.dr3nextButton.Visible = be(3)
    uf.dr4Type.Visible = drf(4): uf.dr4Grouping.Visible = drf(4): uf.dr4speedLabel.Visible = drf(4): uf.dr4ModeLabel.Visible = drf(4): uf.dr4InLabel.Visible = drf(4)
    uf.dr4OutLabel.Visible = drf(4): uf.dr4offsetLabel.Visible = drf(4): uf.dr4Offset.Visible = drf(4): uf.dr4rateALabel.Visible = drf(4): uf.dr4rateBLabel.Visible = drf(4)
    uf.dr4rateA.Visible = drf(4): uf.dr4rateB.Visible = drf(4): uf.dr4expoALabel.Visible = drf(4) And Not be(4): uf.dr4expoBLabel.Visible = drf(4) And Not be(4)
    uf.dr4expoA.Visible = drf(4) And Not be(4): uf.dr4expoB.Visible = drf(4) And Not be(4): uf.dr4prevButton.Visible = be(4): uf.dr4nextButton.Visible = be(4)
    uf.dr5Type.Visible = drf(5): uf.dr5Grouping.Visible = drf(5): uf.dr5speedLabel.Visible = drf(5): uf.dr5ModeLabel.Visible = drf(5): uf.dr5InLabel.Visible = drf(5)
    uf.dr5OutLabel.Visible = drf(5): uf.dr5offsetLabel.Visible = drf(5): uf.dr5Offset.Visible = drf(5): uf.dr5rateALabel.Visible = drf(5): uf.dr5rateBLabel.Visible = drf(5)
    uf.dr5rateA.Visible = drf(5): uf.dr5rateB.Visible = drf(5): uf.dr5expoALabel.Visible = drf(5) And Not be(5): uf.dr5expoBLabel.Visible = drf(5) And Not be(5)
    uf.dr5expoA.Visible = drf(5) And Not be(5): uf.dr5expoB.Visible = drf(5) And Not be(5): uf.dr5prevButton.Visible = be(5): uf.dr5nextButton.Visible = be(5)
    uf.dr6Type.Visible = drf(6): uf.dr6Grouping.Visible = drf(6): uf.dr6speedLabel.Visible = drf(6): uf.dr6ModeLabel.Visible = drf(6): uf.dr6InLabel.Visible = drf(6)
    uf.dr6OutLabel.Visible = drf(6): uf.dr6offsetLabel.Visible = drf(6): uf.dr6Offset.Visible = drf(6): uf.dr6rateALabel.Visible = drf(6): uf.dr6rateBLabel.Visible = drf(6)
    uf.dr6rateA.Visible = drf(6): uf.dr6rateB.Visible = drf(6): uf.dr6expoALabel.Visible = drf(6) And Not be(6): uf.dr6expoBLabel.Visible = drf(6) And Not be(6)
    uf.dr6expoA.Visible = drf(6) And Not be(6): uf.dr6expoB.Visible = drf(6) And Not be(6): uf.dr6prevButton.Visible = be(6): uf.dr6nextButton.Visible = be(6)
    For i = 1 To t18dualRates
        If be(i) Then currentPoint(i) = 9: getDRPoint cond, i, 0
    Next i
End Sub ' dualRate18

Public Sub dualRate14(ByVal func%)
    Const drCnt = 5: Const drItm = 10
    Const addr14drAilRt = 527: Const addr14drEleRt = addr14drAilRt + 7 * drCnt: Const addr14drRudRt = addr14drEleRt + 7 * drCnt
    Const addr14drAlt = 680: Const addr14drAilNt = addr14drAlt + 2: Const addr14drEleNt = addr14drAilNt + drCnt: Const addr14drRudNt = addr14drEleNt + drCnt
    Const addr14drThr = 1361: Const addr14drFlpRt = 1479: Const addr14drFlp3Rt = addr14drFlpRt + 2 * drCnt
    Const addr14drBflyRt = addr14drFlp3Rt + 2 * drCnt: Const addr14drCambRt = addr14drBflyRt + 2 * drCnt:
    Const addr14drFlpNt = addr14drCambRt + 2 * drCnt: Const addr14drFlp3Nt = addr14drFlpNt + drCnt
    Const addr14drBflyNt = addr14drFlpNt + drCnt: Const addr14drCambNt = addr14drBflyNt + drCnt
    Dim uf As Object, i%, j%, k%, drf%, dr$(1 To drCnt, 1 To drItm), ab&, ac&
    Set uf = dualRate14Form
    drf = t14drFuncSeq(func)
    For i = 1 To drCnt
        For j = 1 To drItm
            dr(i, j) = ""
        Next j
    Next i
    If drf = 3 Then
        dr(1, 4) = "Expo": k = myArr(addr14drThr): If k < 128 Then dr(1, 5) = CStr(-k) Else dr(1, 5) = "+" & CStr(256 - k)
        If dr(1, 5) = "0" Then dr(1, 5) = "+0"
    Else
        If (modelType = Plane) And (drf > 2) Then j = 1 Else j = drCnt
        For i = 1 To j
            dr(i, 1) = "Rate " & CStr(i): dr(i, 7) = "Neutral " & CStr(i)
            If drf < 3 Then
                dr(i, 4) = "Expo " & CStr(i)
                ab = addr14drAilRt + (drf * drCnt + i - 1) * 7: ac = addr14drAilNt + drf * drCnt + i - 1
                If drf = 1 Then
                    dr(i, 2) = CStr(myArr(ab + 4)): dr(i, 3) = CStr(myArr(ab + 3))
                    k = myArr(ab + 6): If k < 128 Then dr(i, 5) = "+" & CStr(k) Else dr(i, 5) = CStr(k - 256)
                    k = myArr(ab + 5): If k < 128 Then dr(i, 6) = "+" & CStr(k) Else dr(i, 6) = CStr(k - 256)
                    k = myArr(ac): If k < 128 Then dr(i, 8) = CStr(-k) Else dr(i, 8) = "+" & CStr(256 - k)
                    If dr(i, 8) = "0" Then dr(i, 8) = "+0"
                Else
                    dr(i, 2) = CStr(myArr(ab + 3)): dr(i, 3) = CStr(myArr(ab + 4))
                    k = myArr(ab + 5): If k < 128 Then dr(i, 5) = "+" & CStr(k) Else dr(i, 5) = CStr(k - 256)
                    k = myArr(ab + 6): If k < 128 Then dr(i, 6) = "+" & CStr(k) Else dr(i, 6) = CStr(k - 256)
                    k = myArr(ac): If k < 128 Then dr(i, 8) = "+" & CStr(k) Else dr(i, 8) = CStr(k - 256)
                End If  ' drf = 1
                If (myArr(ab) And 240) = 32 Then
                    dr(i, 9) = "Condition " & CStr(i): dr(i, 10) = conditionName(i)
                Else
                    If i > 1 Then
                        dr(i, 9) = "Switch " & CStr(i)
                        getHardware ab
                        ac = (drf Mod 2) * 4 + i - 2
                        If (myArr(addr14drAlt + Int(drf / 2)) And Power(2, (drf Mod 2) * 4 + i - 2)) _
                            Then dr(i, 10) = hwCtrl & "   " & hwPos & "   " & hwRev & "   " & hwSym & "   Alternate" _
                            Else dr(i, 10) = hwCtrl & "    " & hwPos & "    " & hwRev & "    " & hwSym
                    End If  ' i > 1
                End If  ' (myArr(ab) And 240) = 32
            Else
                If modelType = Glider Then dr(i, 9) = "Condition " & CStr(i): dr(i, 10) = conditionName(i)
                ab = addr14drFlpRt + ((drf - 4) * drCnt + i - 1) * 2: ac = addr14drFlpNt + (drf - 4) * drCnt + i - 1
                If (modelType = Glider) And (drf < 6) Then
                    k = myArr(ab + 0): If k < 128 Then dr(i, 2) = "+" & CStr(k) Else dr(i, 2) = CStr(k - 256)
                    k = myArr(ab + 1): If k < 128 Then dr(i, 3) = "+" & CStr(k) Else dr(i, 3) = CStr(k - 256)
                    k = myArr(ac): If k < 128 Then dr(i, 8) = "+" & CStr(k) Else dr(i, 8) = CStr(k - 256)
                Else
                    k = myArr(ab + 1): If k < 128 Then dr(i, 2) = "+" & CStr(k) Else dr(i, 2) = CStr(k - 256)
                    k = myArr(ab + 0): If k < 128 Then dr(i, 3) = "+" & CStr(k) Else dr(i, 3) = CStr(k - 256)
                    k = myArr(ac): If k < 128 Then dr(i, 8) = CStr(-k) Else dr(i, 8) = "+" & CStr(256 - k)
                    If dr(i, 8) = "0" Then dr(i, 8) = "+0"
                End If  ' (modelType = Glider) And (drf < 6)
            End If  ' drf < 3
        Next i
    End If ' drf = 3
    uf.dr1Label = dr(1, 1): uf.dr1L = dr(1, 2): uf.dr1R = dr(1, 3): uf.expo1Label = dr(1, 4): uf.expo1L = dr(1, 5): uf.expo1R = dr(1, 6)
    uf.dr2Label = dr(2, 1): uf.dr2L = dr(2, 2): uf.dr2R = dr(2, 3): uf.expo2Label = dr(2, 4): uf.expo2L = dr(2, 5): uf.expo2R = dr(2, 6)
    uf.dr3Label = dr(3, 1): uf.dr3L = dr(3, 2): uf.dr3R = dr(3, 3): uf.expo3Label = dr(3, 4): uf.expo3L = dr(3, 5): uf.expo3R = dr(3, 6)
    uf.dr4Label = dr(4, 1): uf.dr4L = dr(4, 2): uf.dr4R = dr(4, 3): uf.expo4Label = dr(4, 4): uf.expo4L = dr(4, 5): uf.expo4R = dr(4, 6)
    uf.dr5Label = dr(5, 1): uf.dr5L = dr(5, 2): uf.dr5R = dr(5, 3): uf.expo5Label = dr(5, 4): uf.expo5L = dr(5, 5): uf.expo5R = dr(5, 6)
    uf.nt1Label = dr(1, 7): uf.nt1 = dr(1, 8): uf.hw1Label = dr(1, 9): uf.hw1 = dr(1, 10)
    uf.nt2Label = dr(2, 7): uf.nt2 = dr(2, 8): uf.hw2Label = dr(2, 9): uf.hw2 = dr(2, 10)
    uf.nt3Label = dr(3, 7): uf.nt3 = dr(3, 8): uf.hw3Label = dr(3, 9): uf.hw3 = dr(3, 10)
    uf.nt4Label = dr(4, 7): uf.nt4 = dr(4, 8): uf.hw4Label = dr(4, 9): uf.hw4 = dr(4, 10)
    uf.nt5Label = dr(5, 7): uf.nt5 = dr(5, 8): uf.hw5Label = dr(5, 9): uf.hw5 = dr(5, 10)
End Sub ' dualRate14

Function dr14FunctionEnabled(ByVal func$) As Boolean
    dr14FunctionEnabled = functionAssigned(t14drFuncDesc(func)) And Not ((modelType = Heli) And (t14drFuncDesc(func) = "Throttle"))
End Function    ' dr14FunctionEnabled

Function dr14Enabled() As Boolean
    Dim i%
    dr14Enabled = False
    For i = 0 To 3
        If dr14FunctionEnabled(i) Then dr14Enabled = True: Exit Function
    Next i
End Function    ' dr14Enabled

Public Sub dualRate()
    Dim uf As Object, i%, j%
    If transType = T18SZ Then
        j = Int(0.5 + t18PointsMax / 2)
        For i = 1 To t18dualRates
            currentPoint(i) = j
        Next i
        Set uf = dualRate18Form
        uf.conditionSelect.Clear
        For i = 1 To t18Conditions
            If conditionList(i) Then uf.conditionSelect.AddItem conditionName(conditionList(i))
        Next i
        uf.conditionSelect.ListIndex = 0
    Else
        Set uf = dualRate14Form
        uf.functionSelect.Clear
        j = 0
        For i = 0 To t14drFuncCount - 1
            If dr14FunctionEnabled(i) Then t14drFuncSeq(j) = i: j = j + 1: uf.functionSelect.AddItem t14drFuncDesc(i)
        Next i
        For i = j To t14drFuncCount - 1
            t14drFuncSeq(i) = -1
        Next i
        If j > 0 Then uf.functionSelect.ListIndex = 0
    End If  ' transType = T18SZ
    uf.Show
End Sub ' dualRate

Public Sub getpMixPoint(ByVal cond%, ByVal pmix%, ByVal i%)
    Dim uf As Object, j%, k%, aa&, ab&
    Set uf = pMixForm
    aa = addr18pMixStart + t18CondLength * (conditionList(cond + 1) - 1) + t18pMixLength * pmix
    ab = addr18pMixSec + addr18CondNameOffset * (conditionList(cond + 1) - 1) + t18pMixSecLength * pmix
    If pmnR Then k = 1 Else k = -1
    If i = 0 Then If Not pointActive(ab, currentPoint(pmix)) Then i = 1
    For j = 1 To t18PointsMax
        currentPoint(pmix) = currentPoint(pmix) + i * k
        If currentPoint(pmix) > t18PointsMax Then currentPoint(pmix) = 1 Else If currentPoint(pmix) < 1 Then currentPoint(pmix) = t18PointsMax
        If pointActive(ab, currentPoint(pmix)) Then Exit For
    Next j
    aa = aa + currentPoint(pmix) + 5
    uf.rateA = rate2String(myArr(aa + t18PointsMax) + Int(myArr(aa + t18PointsMax * 2) / 16) * 256, posDiv * k)
    uf.rateB = rate2String(myArr(aa) + (myArr(aa + t18PointsMax * 2) Mod 16) * 256, 10.5 * k)
End Sub ' getpMixPoint

Public Sub getProgMix(ByVal cond%, ByVal pmix%, ByVal ofs%)
    Const oDelF = t18pMixOffset + t18pMixOffsetLength * t18pMixOffsets + t18pMixOffsetOffLength * t18pMixOffsets
    Const delM1 = 20# / 71: Const delM2 = 160# / 453
    Dim uf As Object, k%, aa&, ao&, r#, tt As Boolean, fn As Boolean, om As Boolean, a$, b$
    tt = (transType = T18SZ): Set uf = pMixForm
    If tt Then aa = addr18pMixStart + t18CondLength * (conditionList(cond + 1) - 1) + t18pMixLength * pmix Else aa = addr14pMixStart + t14pMixLength * pmix
    om = (myArr(aa) And 16): om = om And tt: If myArr(aa) < 128 Then uf.mixStatus = "INH" Else uf.mixStatus = "ACT"
    getHardware aa + 3
    If (myArr(aa) And 64) Then uf.mixHw = hwCtrl & "   " & hwPos & "   " & hwRev & "   " & hwSym & "   Alternate" _
                          Else uf.mixHw = hwCtrl & "    " & hwPos & "    " & hwRev & "    " & hwSym
    If Not om Then
        If (myArr(aa) And 2) Then uf.trim = "ON" Else uf.trim = "OFF"
        fn = (myArr(aa) And 32): k = myArr(aa + 1) And 31
        If fn Then uf.masterFunction = hwCtrlDesc(k): posDiv = 16: pmnR = normalCtrlRate(k) _
              Else uf.masterFunction = fa(k): posDiv = 10.5: pmnR = normalRate(k)
        fn = Not fn: uf.trimLabel.Visible = fn: uf.trim.Visible = fn: uf.masterLink.Visible = fn
        If (myArr(aa) And 4) = 0 Then uf.masterLink = "OFF" Else If (myArr(aa + 2) And 64) Then uf.masterLink = "-" Else uf.masterLink = "+"
        k = myArr(aa + 2) And 31: uf.slaveFunction = fa(k)
        If (myArr(aa) And 8) = 0 Then uf.slaveLink = "OFF" Else If (myArr(aa + 2) And 128) Then uf.slaveLink = "-" Else uf.slaveLink = "+"
    End If  ' Not om
    If tt Then
        uf.offsetSelectLabel.Visible = om: uf.offsetSelect.Visible = om: uf.offsetMainFrame.Visible = om
        uf.mixingMainFrame.Visible = Not om: uf.t18RatesFrame.Visible = Not om
        If ((myArr(addr18pMixGrouping + Int(pmix / 8)) And Power(2, pmix Mod 8))) Then uf.mixGroup = "Group" Else uf.mixGroup = "Single"
        If om Then
            uf.mixMode = "Offset": ao = aa + t18pMixOffset + t18pMixOffsetLength * ofs: uf.offsetSlave = fa(myArr(ao + 1) Mod 32)
            If myArr(ao) < 128 Then uf.offsetMode = "Normal" Else uf.offsetMode = "Timer"
            r = myArr(ao + 2) * 256# + myArr(ao + 3): If myArr(ao + 2) >= 128 Then r = r - 65536
            uf.offsetOn = Format(Round(r / 21) / 2, "+0.0;-0.0;0.0")
            uf.offsetSpeedIn = CStr(cServoSpeed(myArr(ao + 4) Mod 128)): uf.offsetSpeedOut = CStr(cServoSpeed(myArr(ao + 5) Mod 128))
            If myArr(ao + 8) = 255 Then uf.fineTuneHw = "--": uf.fineTuneMode = "" _
                Else uf.fineTuneHw = hwCtrlDesc(myArr(ao + 8) And 31): uf.fineTuneMode = fineTuneModeDesc(Int(myArr(ao + 8) / 64))
            uf.fineTuneValue = t14nRate(ao + 9)
            If (myArr(aa + oDelF) And Power(2, ofs)) Then r = Round(myArr(ao + 6) * delM1) / 2 Else r = Round(myArr(ao + 6) * delM2) / 10
            uf.delayStart = Format(r, "0.0") & " sec."
            If (myArr(aa + oDelF + 1) And Power(2, ofs)) Then r = Round(myArr(ao + 7) * delM1) / 2 Else r = Round(myArr(ao + 7) * delM2) / 10
            uf.delayStop = Format(r, "0.0") & " sec."
            ao = aa + t18pMixOffset + t18pMixOffsetLength * t18pMixOffsets + t18pMixOffsetOffLength * ofs
            r = myArr(ao) * 256# + myArr(ao + 1): If myArr(ao) >= 128 Then r = r - 65536
            uf.offsetOff = Format(Round(r / 21) / 2, "+0.0;-0.0;0.0")
        Else
            uf.mixMode = "Mixing": If (myArr(aa + t18pMixLength - 6) And 128) Then uf.speedMode = "Symmetry" Else uf.speedMode = "Linear"
            uf.speedIn = CStr(cServoSpeed(myArr(aa + t18pMixLength - 6) Mod 128)): uf.speedOut = CStr(cServoSpeed(myArr(aa + t18pMixLength - 5) Mod 128))
            If (myArr(aa + 2) And 32) Then uf.speedFunction = "Master" Else uf.speedFunction = "Slave"
            r = myArr(aa + t18pMixLength - 4) / 225 * 4: uf.delayStart = Format(r, "0.0") & " sec."
            r = myArr(aa + t18pMixLength - 3) / 225 * 4: uf.delayStop = Format(r, "0.0") & " sec."
            If myArr(aa + t18pMixLength - 2) = 255 Then uf.fineTuneHw = "--": uf.fineTuneMode = "" _
                Else uf.fineTuneHw = hwCtrlDesc(myArr(aa + t18pMixLength - 2) And 31): uf.fineTuneMode = fineTuneModeDesc(Int(myArr(aa + t18pMixLength - 2) / 64))
            uf.fineTuneValue = t14nRate(aa + t18pMixLength - 1)
            aa = addr18pMixSec + addr18CondNameOffset * (conditionList(cond + 1) - 1) + t18pMixSecLength * pmix
            If myArr(aa + 2) < 128 Then uf.mixGrouping = "Separ." Else uf.mixGrouping = "Comb."
            uf.mixOffset = rate2String((myArr(aa + 11) Mod 16) * 256 + myArr(aa + 10), 10)
            k = Int(myArr(aa + 2) / 8) Mod 4: uf.mixType = curveDesc(k)
            Select Case k
            Case 0
                uf.expoALabel = "EXP A": uf.expoBLabel = "EXP B"
                a = rate2String(Int(myArr(aa + 8) / 16) * 256 + myArr(aa + 6), 10): b = rate2String((myArr(aa + 8) Mod 16) * 256 + myArr(aa + 7), 10)
                If pmnR Then uf.expoA = a: uf.expoB = b Else uf.expoA = b: uf.expoB = a
            Case 1
                uf.expoALabel = "EXP 2": uf.expoBLabel = "": uf.expoA = rate2String(Int(myArr(aa + 11) / 16) * 256 + myArr(aa + 9), 10): uf.expoB = ""
            End Select  ' Case k
            fn = (k > 1): uf.prevButton.Visible = fn: uf.nextButton.Visible = fn
            If fn Then
                uf.rateALabel = "Position": uf.rateBLabel = "Rate": uf.expoALabel = "": uf.expoBLabel = "": uf.expoA = "": uf.expoB = ""
                currentPoint(pmix) = 9: getpMixPoint cond, pmix, 0
            Else
                uf.rateALabel = "Rate A": uf.rateBLabel = "Rate B"
                a = rate2String(Int(myArr(aa + 5) / 16) * 256 + myArr(aa + 3), 10): b = rate2String((myArr(aa + 5) Mod 16) * 256 + myArr(aa + 4), 10)
                If pmnR Then uf.rateA = a: uf.rateB = b Else uf.rateA = b: uf.rateB = a
            End If  ' fn
        End If  ' om
    Else
        fn = (myArr(aa) And 16): uf.point3Label.Visible = fn: uf.point3.Visible = fn
        If fn Then
            uf.t14RateMode = "Point": uf.point5Label = "Point 5": uf.point4Label = "Point 4"
            uf.point3Label = "Point 3": uf.point2Label = "Point 2": uf.point1Label = "Point 1"
            If pmnR Then uf.point1 = t14nRate(aa + 6): uf.point2 = t14nRate(aa + 7): uf.point3 = t14nRate(aa + 8): uf.point4 = t14nRate(aa + 9): uf.point5 = t14nRate(aa + 10) _
                    Else uf.point1 = t14rRate(aa + 10): uf.point2 = t14rRate(aa + 9): uf.point3 = t14rRate(aa + 8): uf.point4 = t14rRate(aa + 7): uf.point5 = t14rRate(aa + 6)
        Else
            uf.t14RateMode = "Linear": uf.point5Label = "Rate A": uf.point4Label = "Rate B": uf.point2Label = "Offset X": uf.point1Label = "Offset Y"
            If pmnR Then uf.point5 = t14nRate(aa + 6): uf.point4 = t14nRate(aa + 7): uf.point2 = t14nRate(aa + 10): uf.point1 = t14nRate(aa + 11) _
                    Else uf.point5 = t14nRate(aa + 7): uf.point4 = t14nRate(aa + 6): uf.point2 = t14rRate(aa + 10): uf.point1 = t14rRate(aa + 11)
        End If  ' fn
    End If  ' tt
End Sub ' getProgMix

Public Sub pMixes()
    Dim uf As Object, i%, j%, m%
    If transType = T18SZ Then m = t18pMixCount Else m = t14pMixCount
    Set uf = pMixForm
    If afrFlag Then
        uf.offsetSelect.Clear
        For i = 1 To t18pMixOffsets
            uf.offsetSelect.AddItem "Offset " & CStr(i)
        Next i
        uf.offsetSelect.ListIndex = 0: afrFlag = False: uf.mixSelect.Clear
        For i = 1 To m
            uf.mixSelect.AddItem "Program. mix " & CStr(i)
        Next i
        uf.mixSelect.ListIndex = 0
    Else
        getProgMix uf.conditionSelect.ListIndex, uf.mixSelect.ListIndex, uf.offsetSelect.ListIndex
    End If  ' afrFlag
End Sub ' pMixes

Public Sub progMixes()
    Dim uf As Object, i%, j%, tt As Boolean
    tt = (transType = T18SZ): afrFlag = True: Set uf = pMixForm
    uf.t18MainFrame.Visible = tt: uf.servoSpeedFrame.Visible = tt: uf.fineTuningFrame.Visible = tt: uf.delayFrame.Visible = tt
    uf.t18RatesFrame.Visible = tt: uf.t14RatesFrame.Visible = Not tt: uf.conditionLabel.Visible = tt: uf.conditionSelect.Visible = tt
    If tt Then
        j = Int(0.5 + t18PointsMax / 2)
        For i = 0 To t18pMixCount - 1
            currentPoint(i) = j
        Next i
        uf.conditionSelect.Clear
        For i = 1 To t18Conditions
            If conditionList(i) Then uf.conditionSelect.AddItem conditionName(conditionList(i))
        Next i
        uf.conditionSelect.ListIndex = 0
    Else
        pMixes
    End If  ' tt
    uf.Show
End Sub ' progMixes

Public Sub pCurve(ByVal cond%)
    Dim uf As Object, j%, aa&, tt As Boolean, f As Boolean
    tt = (transType = T18SZ): f = (Not tt) And (modelType = Plane): Set uf = pitchCurveForm
    If tt Then
        getAFR uf, cond, pitchF
        aa = addr18pCurGrp: If (myArr(aa) And 4) Then uf.hoverGrouping = "Group" Else uf.hoverGrouping = "Single"
        If (myArr(aa) And 2) Then uf.lowPitchGrouping = "Group" Else uf.lowPitchGrouping = "Single"
        If (myArr(aa) And 1) Then uf.highPitchGrouping = "Group" Else uf.highPitchGrouping = "Single"
        aa = 2624 + (conditionList(cond + 1) - 1) * t18CondLength: If myArr(aa) < 128 Then uf.highPitchStatus = "INH" Else uf.highPitchStatus = "ACT"
        uf.highPitchControl = hwCtrlDesc(myArr(aa + 1) And 31): uf.highPitchRate = t14nRate(aa + 2)
        aa = aa + 3: If myArr(aa) < 128 Then uf.lowPitchStatus = "INH" Else uf.lowPitchStatus = "ACT"
        uf.lowPitchControl = hwCtrlDesc(myArr(aa + 1) And 31): uf.lowPitchRate = t14nRate(aa + 2)
        aa = aa + 3: If myArr(aa) < 128 Then uf.hoverStatus = "INH" Else uf.hoverStatus = "ACT"
        uf.hoverControl = hwCtrlDesc(myArr(aa + 1) And 31): uf.hoverRate = t14nRate(aa + 2)
        uf.hoverRange = myArr(aa + 3): If (myArr(aa + 4) And 64) Then uf.hoverMode = "Normal" Else uf.hoverMode = "Center"
    Else
        If f Then
            If cond = 0 Then
                uf.switchLabel = "": uf.mixHw = ""
            Else
                uf.switchLabel = "Switch": getHardware 1133 + cond * 4
                uf.mixHw = hwCtrl & "    " & hwPos & "    " & hwRev & "    " & hwSym
            End If  ' cond = 0
            aa = 1174 + cond * 10
        Else
            aa = 762 + (conditionList(cond + 1) - 1) * 4: If myArr(aa) < 128 Then uf.highPitchStatus = "INH" Else uf.highPitchStatus = "ACT"
            uf.highPitchGrouping = "": uf.highPitchControl = hwCtrlDesc(myArr(aa + 1) And 31): uf.highPitchRate = t14rRate(aa + 2) & "%"
            aa = aa + t14Conditions * 4: If myArr(aa) < 128 Then uf.lowPitchStatus = "INH" Else uf.lowPitchStatus = "ACT"
            uf.lowPitchGrouping = "": uf.lowPitchControl = hwCtrlDesc(myArr(aa + 1) And 31): uf.lowPitchRate = t14nRate(aa + 2) & "%"
            aa = aa + t14Conditions * 4 + (conditionList(cond + 1) - 1) * 2: If myArr(aa) < 128 Then uf.hoverStatus = "INH" Else uf.hoverStatus = "ACT"
            uf.hoverGrouping = "": uf.hoverControl = hwCtrlDesc(myArr(aa + 1) And 31): uf.hoverRate = t14rRate(aa + 2) & "%"
            uf.hoverRange = myArr(aa + 3) & "%": If (myArr(aa + 4) And 64) Then uf.hoverMode = "NORM" Else uf.hoverMode = "CTRM"
            aa = 1034 + (conditionList(cond + 1) - 1) * 10
        End If  ' f
        uf.point5Rate = t14rRate(aa + 5): uf.point5Position = t14rPosition(aa + 0)
        uf.point4Rate = t14rRate(aa + 6): uf.point4Position = t14rPosition(aa + 1)
        uf.point3Rate = t14rRate(aa + 7): uf.point3Position = t14rPosition(aa + 2)
        uf.point2Rate = t14rRate(aa + 8): uf.point2Position = t14rPosition(aa + 3)
        uf.point1Rate = t14rRate(aa + 9): uf.point1Position = t14rPosition(aa + 4)
    End If  ' tt
End Sub ' pCurve

Public Sub pitchCurve()
    Dim uf As Object, i%, j%, tt As Boolean, f As Boolean
    tt = (transType = T18SZ): f = (Not tt) And (modelType = Plane): Set uf = pitchCurveForm
    uf.t18MainFrame.Visible = tt: uf.t18RatesFrame.Visible = tt: uf.switchLabel.Visible = f: uf.mixHw.Visible = f: uf.t14RatesFrame.Visible = Not tt
    uf.statusLabel.Visible = Not f: uf.controlLabel.Visible = Not f: uf.rateLabel.Visible = Not f: uf.modeLabel.Visible = Not f: uf.rangeLabel.Visible = Not f
    uf.hoverFrame.Visible = Not f: uf.lowPitchFrame.Visible = Not f: uf.highPitchFrame.Visible = Not f: uf.conditionSelect.Clear
    If f Then
        uf.Width = 252: uf.mFunction = "VPP": uf.conditionLabel = "#"
        For i = 1 To 3
            uf.conditionSelect.AddItem CStr(i)
        Next i
    Else
        uf.mFunction = "Pitch"
        If tt Then
            For i = 1 To t18Channels
                If fa(functn(i)) = "Pitch" Then pitchF = functn(i): Exit For
            Next i
            currentPoint(pitchF) = Int(0.5 + t18PointsMax / 2): j = t18Conditions
        Else
            j = t14Conditions
        End If  ' tt
        For i = 1 To j
            If conditionList(i) Then uf.conditionSelect.AddItem conditionName(conditionList(i))
        Next i
    End If  ' f
    uf.conditionSelect.ListIndex = 0
    uf.Show
End Sub ' pitchCurve

Public Sub tCurve(ByVal cond%)
    Dim uf As Object, aa&
    Set uf = throttleCurveForm
    If transType = T18SZ Then
        getAFR uf, cond, pitchF
        aa = addr18pCurGrp: If (myArr(aa) And 8) Then uf.hoverGrouping = "Group" Else uf.hoverGrouping = "Single"
        aa = 2635 + (conditionList(cond + 1) - 1) * t18CondLength: If myArr(aa) < 128 Then uf.hoverStatus = "INH" Else uf.hoverStatus = "ACT"
        uf.hoverControl = hwCtrlDesc(myArr(aa + 1) And 31): uf.hoverRate = t14nRate(aa + 2)
        uf.hoverRange = myArr(aa + 3): If (myArr(aa + 4) And 64) Then uf.hoverMode = "Normal" Else uf.hoverMode = "Center"
    Else
        If modelType <> Heli Then
            aa = 1428: If myArr(aa - 1) < 128 Then uf.mixStatus = "INH" Else uf.mixStatus = "ON"
        End If  '  modelType <> Heli
        If modelType = Plane Then
            If cond = 0 Then uf.switchLabel = "": uf.mixHw = "" _
                Else uf.switchLabel = "Switch": getHardware 1133 + cond * 4: uf.mixHw = hwCtrl & "    " & hwPos & "    " & hwRev & "    " & hwSym: aa = 1144 + cond * 10
        End If  ' modelType = Plane
        If modelType = Heli Then
            aa = 832 + afrSequence(cond) * 6: If myArr(aa) < 128 Then uf.hoverStatus = "INH" Else uf.hoverStatus = "ACT"
            uf.hoverGrouping = "": uf.hoverControl = hwCtrlDesc(myArr(aa + 1) And 31): uf.hoverRate = t14rRate(aa + 2) & "%"
            uf.hoverRange = myArr(aa + 3) & "%": If (myArr(aa + 4) And 64) Then uf.hoverMode = "NORM" Else uf.hoverMode = "CTRM"
            aa = 1240 + afrSequence(cond) * 8: If myArr(aa) < 128 Then uf.thrLimiterStatus = "INH" Else uf.thrLimiterStatus = "ACT"
            getHardware aa + 1: uf.thrLimiterHw = hwCtrl & "    " & hwPos & "    " & hwRev & "    " & hwSym
            uf.thrLimiterControl = hwCtrlDesc(myArr(aa + 4) And 31): uf.thrLimiterHigh = myArr(aa + 5): uf.thrLimiterLow = myArr(aa + 6)
            If (myArr(aa) And 1) Then uf.thrLimiterCenter = myArr(aa + 7) Else uf.thrLimiterCenter = "INH"
            aa = 1084 + afrSequence(cond) * 10
        End If  '  modelType = Heli
        uf.point5Rate = t14rPosition(aa + 5): uf.point5Position = t14rPosition(aa + 0)
        uf.point4Rate = t14rPosition(aa + 6): uf.point4Position = t14rPosition(aa + 1)
        uf.point3Rate = t14rPosition(aa + 7): uf.point3Position = t14rPosition(aa + 2)
        uf.point2Rate = t14rPosition(aa + 8): uf.point2Position = t14rPosition(aa + 3)
        uf.point1Rate = t14rPosition(aa + 9): uf.point1Position = t14rPosition(aa + 4)
    End If  ' transType = T18SZ
End Sub ' tCurve

Public Sub throttleCurve()
    Dim uf As Object, i%, j%, k%, tt As Boolean, f As Boolean, f2 As Boolean, f3 As Boolean
    tt = (transType = T18SZ): f = (Not tt) And (modelType = Plane): f2 = (Not tt) And ((modelType = Glider) Or (modelType = Multi)): f3 = f Or f2
    Set uf = throttleCurveForm: If (modelType = Multi) Or functionAssigned("Throttle") Then uf.mFunction = "Throttle" Else uf.mFunction = "Motor"
    uf.conditionLabel.Visible = Not f2: uf.conditionSelect.Visible = Not f2: uf.t18MainFrame.Visible = tt: uf.t18RatesFrame.Visible = tt
    uf.mixStatusLabel.Visible = f3: uf.mixStatus.Visible = f3: uf.switchLabel.Visible = f: uf.mixHw.Visible = f: uf.t14RatesFrame.Visible = Not tt
    uf.statusLabel.Visible = Not f3: uf.controlLabel.Visible = Not f3: uf.rateLabel.Visible = Not f3: uf.modeLabel.Visible = Not f3: uf.rangeLabel.Visible = Not f3
    uf.hoverFrame.Visible = Not f3: uf.thrLimiterFrame.Visible = (Not f3) And (Not tt): k = 0: uf.conditionSelect.Clear: If f3 Then uf.Width = 252
    If f2 Then
        tCurve 0
    Else
        If f Then
            uf.conditionLabel = "#"
            For i = 1 To 3
                uf.conditionSelect.AddItem CStr(i)
            Next i
        Else
            If tt Then
                For i = 1 To t18Channels
                    If fa(functn(i)) = "Throttle" Then pitchF = functn(i): Exit For
                Next i
                currentPoint(pitchF) = Int(0.5 + t18PointsMax / 2): j = t18Conditions: uf.Width = 378
            Else
                j = t14Conditions
            End If  ' tt
            For i = 1 To j
                If (conditionList(i) <> 0) And (tt Or (conditionList(i) < 5)) Then uf.conditionSelect.AddItem conditionName(conditionList(i)): afrSequence(k) = conditionList(i) - 1: k = k + 1
            Next i
        End If  ' f
        uf.conditionSelect.ListIndex = 0
    End If  ' f2
    uf.Show
End Sub ' throttleCurve

Public Sub getFTunePoint(uf As Object, ByVal i%)
    Dim j%, k%, aa&
    If t18fTuneRate Then k = 1 Else k = -1
    If i = 0 Then If Not pointActive(addr18fTuneSec, currentPoint(1)) Then i = 1
    For j = 1 To t18PointsMax
        currentPoint(1) = currentPoint(1) + i * k
        If currentPoint(1) > t18fTunePoints Then currentPoint(1) = 1 Else If currentPoint(1) < 1 Then currentPoint(1) = t18fTunePoints
        If pointActive(addr18fTuneSec, currentPoint(1)) Then Exit For
    Next j
    aa = addr18fTunePri + currentPoint(1) - 1
    uf.rateA = rate2String(myArr(aa + t18fTunePoints) + Int(myArr(aa + t18fTunePoints * 2) / 16) * 256, ftPosi * k)
    uf.rateB = rate2String(myArr(aa) + (myArr(aa + t18fTunePoints * 2) Mod 16) * 256, ftRate * k)
End Sub ' getFTunePoint

Public Sub getFTune(uf As Object)
    Dim j%, ab&, a$, b$, f As Boolean
    currentPoint(1) = Int(0.5 + t18fTunePoints / 2):
    ab = addr18fTuneSec: If myArr(ab + 2) < 128 Then uf.pointGrouping = "Separ." Else uf.pointGrouping = "Comb."
    uf.curveOffset = rate2String((myArr(ab + 11) Mod 16) * 256 + myArr(ab + 10), 10): j = Int(myArr(ab + 2) / 8) Mod 4: uf.curveType = curveDesc(j)
    Select Case j
    Case 0
        uf.expoALabel = "EXP A": uf.expoBLabel = "EXP B"
        a = rate2String(Int(myArr(ab + 8) / 16) * 256 + myArr(ab + 6), 10): b = rate2String((myArr(ab + 8) Mod 16) * 256 + myArr(ab + 7), 10)
        If t18fTuneRate Then uf.expoA = a: uf.expoB = b Else uf.expoA = b: uf.expoB = a
    Case 1
        uf.expoALabel = "EXP 2": uf.expoBLabel = "": uf.expoA = rate2String(Int(myArr(ab + 11) / 16) * 256 + myArr(ab + 9), 10): uf.expoB = ""
    End Select  ' Case j
    f = (j > 1): uf.prevButton.Visible = f: uf.nextButton.Visible = f
    If f Then
        uf.rateALabel = "Position": uf.rateBLabel = "Rate": uf.expoALabel = "": uf.expoBLabel = "": uf.expoA = "": uf.expoB = "": getFTunePoint uf, 0
    Else
        uf.rateALabel = "Rate A": uf.rateBLabel = "Rate B"
        a = rate2String(Int(myArr(ab + 5) / 16) * 256 + myArr(ab + 3), 10): b = rate2String((myArr(ab + 5) Mod 16) * 256 + myArr(ab + 4), 10)
        If t18fTuneRate Then uf.rateA = a: uf.rateB = b Else uf.rateA = b: uf.rateB = a
    End If  ' f
End Sub ' getFTune

Sub t14fTune(uf As Object, ByVal a&)
    uf.t14Hw = hwCtrlDesc(myArr(a) And 31): If (myArr(a) And 31) = 31 Then uf.t14Mode = "" Else uf.t14Mode = fineTuneModeDesc(Int(myArr(a) / 64))
    If myArr(a + 1) < 128 Then uf.t14Rate = "+" & myArr(a + 1) & "%" Else uf.t14Rate = (myArr(a + 1) - 256) & "%"
End Sub ' t14fTune

Public Sub diffAil(ByVal cond%)
    Dim uf As Object, aa&, s$, j%
    Set uf = aDifForm
    If transType = T18SZ Then
        s = "": aa = 2624 + t18CondLength * (conditionList(cond + 1) - 1): addr18fTunePri = aa + 9
        addr18fTuneSec = 28542 + addr18CondNameOffset * (conditionList(cond + 1) - 1)
        j = myArr(addr18fTunePri - 1) And 31: uf.hw = hwCtrlDesc(j): t18fTuneRate = normalCtrlRate(j): ftPosi = 16: ftRate = 16: getFTune uf
    Else
        s = "%": t14fTune uf, 1563 + 2 * (conditionList(cond + 1) - 1): aa = 1087 + 9 * (conditionList(cond + 1) - 1)
        If myArr(aa + 8) < 128 Then uf.bFlyAdjust = "+" & myArr(aa + 8) & s Else uf.bFlyAdjust = (myArr(aa + 8) - 256) & s
    End If  ' transType = T18SZ
    uf.ailLeft = myArr(aa + 0) & s: uf.ailRight = myArr(aa + 1) & s: uf.ail2Left = myArr(aa + 2) & s: uf.ail2Right = myArr(aa + 3) & s
    uf.ail3Left = myArr(aa + 4) & s: uf.ail3Right = myArr(aa + 5) & s: uf.ail4Left = myArr(aa + 6) & s: uf.ail4Right = myArr(aa + 7) & s
End Sub ' diffAil

Public Sub diffAileron()
    Dim uf As Object, i%, j%, tt As Boolean, f As Boolean, f2 As Boolean, f3 As Boolean
    tt = (transType = T18SZ): f = tt Or (modelType = Glider): f2 = ((wingType And 7) >= 5) And ((wingType And 7) <= 6): f3 = (Not tt) And (modelType = Glider)
    Set uf = aDifForm: uf.conditionLabel.Visible = f: uf.conditionSelect.Visible = f: uf.ail34Frame.Visible = f2: uf.bFlyAdjustFrame.Visible = f3
    uf.t18Frame.Visible = tt: uf.t14Frame.Visible = (transType = T14SG): uf.conditionSelect.Clear
    If f Then
        If tt Then
            j = t18Conditions: If (myArr(addr18pCurGrp) And 1) Then uf.groupFunction = "Group" Else uf.groupFunction = "Single"
        Else
            j = t14Conditions
        End If  ' tt
        For i = 1 To j
            If conditionList(i) Then uf.conditionSelect.AddItem conditionName(conditionList(i))
        Next i
        uf.conditionSelect.ListIndex = 0
    Else
        diffAil 0
    End If  ' f
    uf.Show
End Sub ' diffAileron

Function t14Offset$(ByVal a&)
    Dim b&
    b = myArr(a): b = b * 256 + myArr(a + 1): b = Int(b / 64)
    If myArr(a) < 128 Then t14Offset = "+" & b Else t14Offset = b - 1024
End Function    ' t14Offset

Public Sub flapSet(ByVal cond%)
    Dim uf As Object, aa&, s$
    Set uf = flapSetForm
    If transType = T18SZ Then s = "": aa = 2666 + t18CondLength * (conditionList(cond + 1) - 1) Else s = "%": aa = 728
    uf.flapUp = t14nRate(aa + 0) & s: uf.flapDown = t14nRate(aa + 1) & s: uf.flapOffset = t14Offset(aa + 4) & s
    uf.flap2Up = t14nRate(aa + 2) & s: uf.flap2Down = t14nRate(aa + 3) & s: uf.flap2Offset = t14Offset(aa + 6) & s
    uf.flap3Up = t14nRate(aa + 8) & s: uf.flap3Down = t14nRate(aa + 9) & s: uf.flap3Offset = t14Offset(aa + 12) & s
    uf.flap4Up = t14nRate(aa + 10) & s: uf.flap4Down = t14nRate(aa + 11) & s: uf.flap4Offset = t14Offset(aa + 14) & s
    uf.mixUp = t14nRate(aa + 20): uf.mixDown = t14nRate(aa + 21): uf.mixOffset = t14Offset(aa + 22)
    If (myArr(aa + 16) And 128) Then uf.mixStatus = "ACT" Else uf.mixStatus = "INH"
    getHardware aa + 17
    If (myArr(aa + 16) And 64) Then uf.mixHw = hwCtrl & "   " & hwPos & "   " & hwRev & "   " & hwSym & "   Alternate" _
                               Else uf.mixHw = hwCtrl & "    " & hwPos & "    " & hwRev & "    " & hwSym
End Sub ' flapSet

Public Sub flapSetting()
    Dim uf As Object, i%, j%, tt As Boolean, f2 As Boolean, f4 As Boolean
    tt = (transType = T18SZ): f2 = ((wingType And 7) >= 3) And Not tt: f4 = ((wingType And 7) = 4) Or ((wingType And 7) = 6)
    Set uf = flapSetForm: uf.conditionLabel.Visible = tt: uf.conditionSelect.Visible = tt
    uf.groupLabel.Visible = tt: uf.camberFlapGroup.Visible = tt: uf.brakeFlapGroup.Visible = tt: uf.mixGroup.Visible = tt
    uf.flap2Label.Visible = f2: uf.flap2Up.Visible = f2: uf.flap2Down.Visible = f2: uf.flap2Offset.Visible = f2
    uf.brakeFlapFrame.Visible = f4: uf.mixFrame.Visible = f4: If Not f4 Then uf.Width = 222
    If tt Then
        If (myArr(addr18pCurGrp) And 2) Then uf.camberFlapGroup = "Group" Else uf.camberFlapGroup = "Single"
        If (myArr(addr18pCurGrp) And 4) Then uf.brakeFlapGroup = "Group" Else uf.brakeFlapGroup = "Single"
        If (myArr(addr18pCurGrp) And 8) Then uf.mixGroup = "Group" Else uf.mixGroup = "Single"
        For i = 1 To t18Conditions
            If conditionList(i) Then uf.conditionSelect.AddItem conditionName(conditionList(i))
        Next i
        uf.conditionSelect.ListIndex = 0
    Else
        flapSet 0
    End If  ' tt
    uf.Show
End Sub ' flapSetting

Public Sub mA2F(ByVal cond%)
    Dim uf As Object, a18p&, a18s&, a14&, aa&, ab&, s$
    Set uf = ail2cFlapForm
    Select Case gMix
    Case 1
        a18p = 2690: a18s = 28554: a14 = 768
    Case 2
        a18p = 2731: a18s = 28566: a14 = 792
    End Select  ' Case gMix
    If transType = T18SZ Then
        s = "": aa = a18p + t18CondLength * (conditionList(cond + 1) - 1): addr18fTunePri = aa + 4: ab = aa + 37
        addr18fTuneSec = a18s + addr18CondNameOffset * (conditionList(cond + 1) - 1): t18fTuneRate = True: ftPosi = 10.5: ftRate = 10.5: getFTune uf
    Else
        s = "%": aa = a14: ab = aa + 4 + 4 * (conditionList(cond + 1) - 1)
    End If  ' transType = T18SZ
    If (myArr(aa) And 128) Then uf.status = "ACT" Else uf.status = "INH"
    If (myArr(aa) And 1) Then uf.link = "ON" Else uf.link = "OFF"
    getHardware aa + 1
    If (myArr(aa) And 64) Then uf.hw = hwCtrl & "   " & hwPos & "   " & hwRev & "   " & hwSym & "   Alternate" _
                          Else uf.hw = hwCtrl & "    " & hwPos & "    " & hwRev & "    " & hwSym
    uf.flapLeft = t14nRate(ab + 0) & s: uf.flapRight = t14nRate(ab + 1) & s: uf.flap2Left = t14nRate(ab + 2) & s: uf.flap2Right = t14nRate(ab + 3) & s
End Sub ' mA2F

Public Sub mAil2Flap(ByVal mix%)
    Dim uf As Object, i%, j%, tt As Boolean, cf As Boolean
    gMix = mix: tt = (transType = T18SZ): cf = tt Or (modelType = Glider): Set uf = ail2cFlapForm: If Not tt Then uf.Width = 222
    uf.conditionLabel.Visible = cf: uf.conditionSelect.Visible = cf: uf.groupLabel.Visible = tt: uf.group.Visible = tt: uf.afrFrame.Visible = tt
    Select Case mix
    Case 1
        uf.Caption = "AIL -› Camber flap": uf.flapLabel = "Flap": uf.flap2Label = "Flap2": i = 16
    Case 2
        uf.Caption = "AIL -› Brake flap": uf.flapLabel = "Flap3": uf.flap2Label = "Flap4": i = 32
    End Select  ' Case mix
    If tt Then
        j = t18Conditions: If (myArr(addr18pCurGrp) And i) Then uf.group = "Group" Else uf.group = "Single"
    Else
        j = t14Conditions
    End If  ' tt
    If cf Then
        For i = 1 To j
            If conditionList(i) Then uf.conditionSelect.AddItem conditionName(conditionList(i))
        Next i
        uf.conditionSelect.ListIndex = 0
    Else
        mA2F 0
    End If  ' cf
    uf.Show
End Sub ' mAil2Flap

Sub showMainForm()
    Dim uf As Object, wt%, tt%, b As Boolean
    Set uf = mainForm
    uf.transTypeBox = transDesc(transType)
    uf.modelNameBox = modelName
    uf.modelTypeBox = acDesc(modelType)
    uf.systemTypeBox = modulationDesc(sysModulation)
    Select Case modelType
    Case Plane, Glider
        uf.wingTypeLabel = "Wing type:": uf.wingTypeLabel.Enabled = True
        uf.tailTypeLabel = "Tail type:": uf.tailTypeLabel.Enabled = True
        wt = wingType: tt = tailType
        If wt >= 8 Then
            wt = wt - 2
            If tt = 3 Then tt = tt + 1
        End If  ' wt >= 8
        uf.wingTypeBox = wingDesc(wt): uf.wingTypeBox.Enabled = True
        uf.tailTypeBox = tailDesc(tt): uf.tailTypeBox.Enabled = True
        uf.swashRingButton.Enabled = False: uf.swashButton.Enabled = False
    Case Heli
        uf.wingTypeLabel = "Swash type:": uf.wingTypeLabel.Enabled = True
        uf.tailTypeLabel = "": uf.tailTypeLabel.Enabled = False
        uf.wingTypeBox = swashDesc(wingType): uf.wingTypeBox.Enabled = True
        uf.tailTypeBox = "": uf.tailTypeBox.Enabled = False
        uf.swashRingButton.Enabled = True: uf.swashButton.Enabled = (wingType > 0)
    Case Multi
        uf.wingTypeLabel = "": uf.wingTypeLabel.Enabled = False
        uf.tailTypeLabel = "": uf.tailTypeLabel.Enabled = False
        uf.wingTypeBox = "": uf.wingTypeBox.Enabled = False
        uf.tailTypeBox = "": uf.tailTypeBox.Enabled = False
        uf.swashRingButton.Enabled = False: uf.swashButton.Enabled = False
    End Select  ' modelType
    uf.throttleCutButton.Enabled = True
    b = (transType <> T8FG)
    uf.servoSpeedButton.Enabled = b: uf.stickAlarmButton.Enabled = b: uf.sensorButton.Enabled = b
    uf.telemetryButton.Enabled = b: uf.teleSettingButton.Enabled = b
    uf.trimMix1Button.Enabled = False: uf.trimMix2Button.Enabled = False
    uf.pitchCurveButton.Enabled = functionAssigned("Pitch") Or functionAssigned("VPP")
    uf.throttleCurveButton.Enabled = ((transType = T18SZ) And (modelType = Heli) And functionAssigned("Throttle")) Or _
                                     ((transType <> T18SZ) And (((modelType = Plane) And (functionAssigned("Throttle") Or functionAssigned("Motor"))) Or _
                                                                ((modelType = Heli) And functionAssigned("Throttle")) Or _
                                                                ((modelType = Glider) And functionAssigned("Motor")) Or (modelType = Multi)))
    wt = wingType And 7
    uf.ailDifferentialButton.Enabled = ((modelType = Plane) Or (modelType = Glider)) And (wt >= 1)
    uf.flapSettingButton.Enabled = ((modelType = Plane) Or (modelType = Glider)) And ((wt >= 3) Or ((wt = 2) And (transType <> T18SZ)))
    
    If transType = T18SZ Then
        uf.idleDownButton.Enabled = True: uf.functionNameButton.Enabled = True
        uf.saveButton.Enabled = False: uf.trimSettingButton.Caption = "T1-T6 setting": uf.condSelectButton.Enabled = True
        uf.afrButton.Caption = "AFR": uf.afrButton.Enabled = True: uf.pit2NeedleButton.Caption = "Pitch -› Needle"
        Select Case modelType
        Case Plane
            uf.abrk2ElevatorButton.Enabled = True: uf.airbrakeButton.Enabled = True
            uf.accelerationButton.Enabled = False: uf.pit2NeedleButton.Enabled = False
        Case Heli
            uf.abrk2ElevatorButton.Enabled = False: uf.accelerationButton.Enabled = True
        Case Glider
            uf.abrk2ElevatorButton.Enabled = True: uf.trimMix1Button.Enabled = True: uf.trimMix2Button.Enabled = True
            uf.accelerationButton.Enabled = True
            uf.butterflyButton.Enabled = (wt >= 1) And (wt <= 6)    ' 2AIL or more
        Case Multi
            uf.abrk2ElevatorButton.Enabled = False: uf.accelerationButton.Enabled = False
        End Select  ' Case modelType
    Else
        uf.functionNameButton.Enabled = False: uf.saveButton.Enabled = True: uf.dualRateButton.Enabled = dr14Enabled
        uf.trimSettingButton.Caption = "T1-T4 setting": uf.afrButton.Caption = "Throttle delay": uf.pit2NeedleButton.Caption = "Fuel mix"
        uf.abrk2ElevatorButton.Enabled = False: uf.accelerationButton.Enabled = False
        Select Case modelType
        Case Plane
            uf.idleDownButton.Enabled = True: uf.condSelectButton.Enabled = False: uf.afrButton.Enabled = True
            uf.pit2NeedleButton.Enabled = True:
            uf.airbrakeButton.Enabled = (wt >= 1) And (wt <= 6) ' 2AIL or more
        Case Heli
            uf.idleDownButton.Enabled = False: uf.condSelectButton.Enabled = True: uf.afrButton.Enabled = False
        Case Glider
            uf.throttleCutButton.Enabled = False: uf.idleDownButton.Enabled = False: uf.condSelectButton.Enabled = True
            uf.afrButton.Enabled = False
            uf.trimMix1Button.Enabled = (wt >= 1) And (wt <= 6) ' 2AIL or more
            uf.butterflyButton.Enabled = ((wingType >= 1) And (wingType <= 6)) Or ((wingType >= 10) And (wingType <= 14))
        Case Multi
            uf.idleDownButton.Enabled = False: uf.condSelectButton.Enabled = False: uf.afrButton.Enabled = False
        End Select  ' Case modelType
    End If  ' transType = T18SZ
    Select Case modelType
    Case Plane
        uf.ail2RudderButton.Enabled = True: uf.rud2AileronButton.Enabled = True: uf.rud2ElevatorButton.Enabled = True
        uf.butterflyButton.Enabled = False: uf.snapRollButton.Enabled = True: uf.throttleHoldButton.Enabled = False
        uf.swashMixButton.Enabled = False: uf.throttleMixButton.Enabled = False: uf.pit2RudderButton.Enabled = False
        uf.governorButton.Enabled = False: uf.motorButton.Enabled = True
        uf.ailevatorButton.Enabled = (tt = 2): uf.vtailButton.Enabled = (tt = 1): uf.wingletButton.Enabled = (tt = 4)
        b = (wt >= 1) And (wt <= 6) ' 2AIL or more
        uf.ele2CamberButton.Enabled = b: uf.camberMixingButton.Enabled = b
        uf.cambFlap2EleButton.Enabled = (wt >= 2) And (wt <= 6)     ' 1FLP or more
        uf.ail2CamberFlapButton.Enabled = (wt >= 3) And (wt <= 6)   ' 2FLP or more
        uf.ail2BrakeFlapButton.Enabled = (wt = 4) Or (wt = 6)       ' 4FLP
    Case Heli
        uf.ail2CamberFlapButton.Enabled = False: uf.ail2BrakeFlapButton.Enabled = False: uf.ail2RudderButton.Enabled = False
        uf.ele2CamberButton.Enabled = False: uf.camberMixingButton.Enabled = False: uf.cambFlap2EleButton.Enabled = False
        uf.rud2AileronButton.Enabled = False: uf.rud2ElevatorButton.Enabled = False: uf.butterflyButton.Enabled = False
        uf.snapRollButton.Enabled = False: uf.airbrakeButton.Enabled = False: uf.throttleHoldButton.Enabled = True
        uf.swashMixButton.Enabled = True: uf.throttleMixButton.Enabled = True: uf.pit2NeedleButton.Enabled = True
        uf.pit2RudderButton.Enabled = True: uf.governorButton.Enabled = True: uf.ailevatorButton.Enabled = False
        uf.vtailButton.Enabled = False: uf.wingletButton.Enabled = False: uf.motorButton.Enabled = False
    Case Glider
        uf.ail2RudderButton.Enabled = True
        uf.rud2AileronButton.Enabled = True: uf.rud2ElevatorButton.Enabled = False: uf.snapRollButton.Enabled = False
        uf.airbrakeButton.Enabled = False: uf.throttleHoldButton.Enabled = False: uf.swashMixButton.Enabled = False
        uf.throttleMixButton.Enabled = False: uf.pit2NeedleButton.Enabled = False: uf.pit2RudderButton.Enabled = False
        uf.governorButton.Enabled = False: uf.motorButton.Enabled = True
        b = (wt >= 1) And (wt <= 6) ' 2AIL or more
        uf.ele2CamberButton.Enabled = b: uf.camberMixingButton.Enabled = b
        uf.cambFlap2EleButton.Enabled = (wt >= 2) And (wt <= 6)     ' 1FLP or more
        uf.ail2CamberFlapButton.Enabled = (wt >= 3) And (wt <= 6)   ' 2FLP or more
        uf.ail2BrakeFlapButton.Enabled = (wt = 4) Or (wt = 6)       ' 4FLP
        uf.ailevatorButton.Enabled = (tt = 2): uf.vtailButton.Enabled = (tt = 1): uf.wingletButton.Enabled = (tt = 4)
    Case Multi
        uf.ail2CamberFlapButton.Enabled = False: uf.ail2BrakeFlapButton.Enabled = False: uf.ail2RudderButton.Enabled = False
        uf.ele2CamberButton.Enabled = False: uf.camberMixingButton.Enabled = False: uf.cambFlap2EleButton.Enabled = False
        uf.rud2AileronButton.Enabled = False: uf.rud2ElevatorButton.Enabled = False: uf.butterflyButton.Enabled = False
        uf.snapRollButton.Enabled = False: uf.airbrakeButton.Enabled = False: uf.throttleHoldButton.Enabled = False
        uf.swashMixButton.Enabled = False: uf.throttleMixButton.Enabled = False: uf.pit2NeedleButton.Enabled = False
        uf.pit2RudderButton.Enabled = False: uf.governorButton.Enabled = False: uf.ailevatorButton.Enabled = False
        uf.vtailButton.Enabled = False: uf.wingletButton.Enabled = False: uf.motorButton.Enabled = False
    End Select  ' Case modelType
    readFlag = False: saveFlag = False: exitFlag = True
    uf.Caption = "Futaba 18SZ Utility - " & fName
    uf.Show
End Sub ' showMainForm

Sub Main()
    transDesc = Split(transList, mySep): acDesc = Split(acList, mySep): wingDesc = Split(wingList, mySep)
    tailDesc = Split(tailList, mySep): swashDesc = Split(swashList, mySep): hwCtrlDesc = Split(hwCtrlList, mySep)
    modulationDesc = Split(modulationList, mySep): telemTypeDesc = Split(telemTypeList, mySep): tfhssVoltDesc = Split(tfhssVoltList, mySep)
    trainerChModeDesc = Split(trainerChModeList, mySep): trainerModeDesc = Split(trainerModeList, mySep): trainerChSwDesc = Split(trainerChSwList, mySep)
    curveDesc = Split(curveList, mySep): fineTuneModeDesc = Split(fineTuneModeList, mySep): t14drFuncDesc = Split(t14drFuncList, mySep)
    chMask(1) = 1: For i = 2 To chMax: chMask(i) = chMask(i - 1) * 2: Next i
    otFlag = True: readFlag = True: ChDrive ActiveWorkbook.Path: ChDir ActiveWorkbook.Path
    Do While True
        If readFlag Then
            readBinaryFile
            If newData Then
                transType = getTransType
                If transType > 127 Then MsgBox "Unrecognized file format!": exitProgram
                modelName = getModelName
                getModelType
                If modelType > 127 Then MsgBox "Invalid model type!": exitProgram
                otFlag = False: getModulation: getFunction: getConditions
            Else
                If otFlag Then exitProgram
            End If  ' newData
        End If  ' readFlag
        showMainForm
        If exitFlag Then If debugFlag Then Exit Sub Else exitProgram
        If saveFlag Then
            convertData
            saveBinaryFile
        End If  ' saveFlag
    Loop    ' While True
End Sub ' Main

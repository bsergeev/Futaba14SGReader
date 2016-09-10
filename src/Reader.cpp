#include <array>
#include <cstdint>
#include <cassert>
#include <iomanip>  // std::setw
#include <iostream>
#include <fstream>  // std::ifstream
#include <string>
#include <vector>

using namespace std::string_literals;

const size_t arrSizeMax = 32767;
const size_t chMax = 16;
const size_t t14Channels = 12,   t18Channels = 16;
const size_t t14Conditions = 5,  t18Conditions = 8;
const size_t t14ChannelsLow = 8, t18ChannelsLow = 12;

enum eTxType {
    INVALID_TX = 255, // <<< DEBUG
    T8FG  = 0,
    T14SG = 1,
    T18SZ = 2
};
enum eModelType {
    INVALID_MODEL = -1,
    Plane  = 0,
    Heli   = 1,
    Glider = 2,
    Multi  = 3
};
uint8_t m_wingType = 0;
uint8_t m_tailType = 0;

std::array<bool, chMax> reversed;
std::array<bool, 2>     reversedDG;
std::array<uint8_t, chMax> travelLo, travelHi, limitLo, limitHi;

std::array<uint8_t, chMax> functn; // value is the index of FunctionNames_t, i.e. < 33

typedef std::array<std::string, 33> FunctionNames_t;
FunctionNames_t& fa = FunctionNames_t{};
FunctionNames_t functionListAir = {
    "Aileron"s, "Elevator"s, "Throttle"s, "Rudder"s, "Gear"s, 
    "Flap"s, "Aileron2"s, "Aileron3"s, "Aileron4"s, "Elevator2"s, 
    "Flap2"s, "Air brake"s, "Fuel mix"s, "Gyro"s, "Gyro2"s, 
    "Gyro3"s, "Throttle2"s, "Throttle3"s, "Throttle4"s, "Flap3"s, 
    "Flap4"s, "Rudder2"s, "Butterfly"s, "Camber"s, "Motor"s, 
    "Auxiliary7"s, "Auxiliary6"s, "Auxiliary5"s, "Auxiliary4"s, "Auxiliary3"s, 
    "Auxiliary2"s, "Auxiliary1"s, "--" };
FunctionNames_t functionListHeli = {
    "Aileron"s, "Elevator"s, "Throttle"s, "Rudder"s, "Gear"s, 
    "Pitch"s, "Governor"s, "Governor2"s, "Aileron4"s, "Elevator2"s,
    "Flap2"s, "Needle"s, "Fuel mix"s, "Gyro"s, "Gyro2"s, 
    "Gyro3"s, "Throttle2"s, "Throttle3"s, "Throttle4"s, "Flap3"s, 
    "Flap4"s, "Rudder2"s, "Butterfly"s, "Camber"s, "Auxiliary8"s, 
    "Auxiliary7"s, "Auxiliary6"s, "Auxiliary5"s, "Auxiliary4"s, "Auxiliary3"s, 
    "Auxiliary2"s, "Auxiliary1"s, "--" };
FunctionNames_t functionListMulti = {
    "Aileron"s, "Elevator"s, "Throttle"s, "Rudder"s, "Gear"s, 
    "Flap"s, "Aileron2"s, "Aileron3"s, "Aileron4"s, "Elevator2"s, 
    "Flap2"s, "Air brake"s, "Fuel mix"s, "Gyro"s, "Gyro2"s, 
    "Gyro3"s, "Camera roll"s, "Camera tilt"s, "Camera pan"s, "Camera rec"s, 
    "Mode"s, "Rudder2"s, "Butterfly"s, "Camber"s, "Motor"s, 
    "Auxiliary7"s, "Auxiliary6"s, "Auxiliary5"s, "Auxiliary4"s, "Auxiliary3"s, 
    "Auxiliary2"s, "Auxiliary1"s, "--" };


// & -> Long
// % -> Integer
// # -> Double
// ! -> Single
// @ -> Decimal
// $ -> String

std::array<std::wstring, t18Conditions>  conditionName;
std::array<int,          t18Conditions> conditionState, conditionList;
std::array<size_t,       t18Conditions> conditionHw;


bool LoadFromFile(const std::string& fileName, std::vector<uint8_t>& data)
{
    bool ok = false;
    data.clear();
    if (!fileName.empty())
    {
        std::ifstream inStream(fileName, std::ifstream::in | std::ifstream::binary);
        if ((ok = (inStream.good() && inStream.is_open())) == true)
        {
            inStream.seekg(0, inStream.end); // get length of file
            const size_t length = static_cast<size_t>(inStream.tellg());
            assert(length <= arrSizeMax);
            inStream.seekg(0, inStream.beg);

            data.resize(length);
            inStream.read(reinterpret_cast<char*>(&data[0]), length);

            ok = !inStream.fail();
        }
    }
    return ok;
}

eTxType getTransType(const std::vector<uint8_t>& data)
{
    const size_t TX_ID_LENGTH = 8;
    std::array<char, TX_ID_LENGTH+1>  buffer;
    buffer.fill('\0');

    for (size_t i = 0; i < TX_ID_LENGTH; ++i) {
        buffer[i] = static_cast<const char>(data.at(i));
    }
    if (std::string{ buffer.data() } == "T18SZ   "s) {
        return T18SZ;
    }

    buffer.fill('\0');
    for (size_t i = 0; i < TX_ID_LENGTH; ++i) {
        buffer[i] = static_cast<const char>(data.at(i * 2 + 2));
    }
    const std::string txName{ buffer.data() };
    if (txName == "T8FG    "s) {
        return T8FG;
    } else if (txName == "T14SG   "s) {
        return T14SG;
    }

    return INVALID_TX;
}

std::wstring getModelName(const std::vector<uint8_t>& data, eTxType txType)
{
    const size_t t14mNameStart  = 17, t14mNameLength = 10;
    const size_t t18mNameStart  = 10, t18mNameLength = 15;

    std::array<wchar_t, t18mNameLength + 1> buffer;
    buffer.fill(0);

    size_t startPos=0, len=0;
    if (txType == T18SZ) {
        startPos = t18mNameStart; len = t18mNameLength;
    } else {
        startPos = t14mNameStart; len = t14mNameLength;
    }
    for (size_t k = 0; k < len; ++k) {
        const char hi = static_cast<const char>(data.at(startPos + k*2));
        const char lo = static_cast<const char>(data.at(startPos + k*2 + 1));
        buffer[k] = static_cast<wchar_t>((hi << 8) + lo);
        if (buffer[k] == 0) {
            break;
        }
    }
    return std::wstring{ buffer.data() };
}

eModelType getModelType(const std::vector<uint8_t>& data, eTxType txType)
{
    const size_t i = (txType == T18SZ)? 93 : 152;
    const uint8_t v = data.at(i + 1) / 16;
    const eModelType modelType = (v > Multi)? INVALID_MODEL : static_cast<eModelType>(v);
    m_wingType =  data.at(i) & 0x0F;
    m_tailType = (data.at(i) & 0x30) >> 4;
    return modelType;
}

int getModulation(const std::vector<uint8_t>& data, eTxType txType)
{
    int sysModulation = 0;

    const size_t addr14sysTyp = 154, div14sysTyp = 1;
    const size_t addr18sysTyp = 92,  div18sysTyp = 16;

    const size_t am = (txType == T18SZ)? addr18sysTyp : addr14sysTyp;
    if (txType == T8FG) {
        const size_t v = ((data.at(am) & 0x30) + (data.at(am + 1) & 0x80)) >> 4;
        switch (v) {
        case 1: sysModulation = 1;
                break;
        case 3: sysModulation = 0;
                break;
        case 9: sysModulation = 2;
                break;
        }
    } else {
        const size_t dm = (txType == T18SZ)? div18sysTyp : div14sysTyp;
        sysModulation = (data.at(am) / dm) & 0x0F;
    }

    return sysModulation;
}

void getFunction(const std::vector<uint8_t>& data, eTxType txType, eModelType modelType)
{
    const size_t addr14Func = 178, addr18Func = 102;

    size_t numChannels, addr;
    if (txType == T18SZ) {
        numChannels = t18Channels; addr = addr18Func;
    } else { 
        numChannels = t14Channels; addr = addr14Func;
    }
    for (size_t i = 0; i < numChannels; ++i) {
        functn[i] = data.at(addr + i);
    }
    switch (modelType) {
    case Plane:
    case Glider: fa = functionListAir;
                 break;
    case Heli:   fa = functionListHeli;
                 break;
    case Multi:  fa = functionListMulti;
                 break;
    }
    if (txType != T18SZ && modelType == Plane) {
        fa[25] = "VPP"s;
        return;
    }
/*
    const size_t addr18AuxName = 25300, len18AuxName = 10; const size_t offs18AuxName = (len18AuxName + 1) * 2;
    const size_t addr18AuxAbbr = 25476, len18AuxAbbr = 4;  const size_t offs18AuxAbbr = (len18AuxAbbr + 1) * 2;
    const size_t t14ChannelsLow = 8, t18ChannelsLow = 12, t18PointsMax = 17, t18fTunePoints = 11;
    std::array<uint8_t, len18AuxName - 1> bName;
    std::array<uint8_t, len18AuxAbbr - 1> bAbbr;
    for (size_t i = 0;  i < auxFunctionNumber;  ++i)
    {
        f = True
        for (size_t j = 0 To len18AuxName - 1
            if (f) {
                bName(j) = data(addr18AuxName + i * offs18AuxName + j * 2 + 1): if (bName(j) = 0) { bName(j) = 32: f = False
            Else
                bName(j) = 32
            End if ( // f
        }
        auxFunctionName(i) = RTrim(StrConv(bName, vbUnicode))
        if (auxFunctionName(i) = "") { auxFunctionName(i) = fa(functionNumber - 2 - i) Else fa(functionNumber - 2 - i) = auxFunctionName(i)
        f = True
        for (size_t j = 0 To len18AuxAbbr - 1
            if (f) {
                bAbbr(j) = data(addr18AuxAbbr + i * offs18AuxAbbr + j * 2 + 1): if (bAbbr(j) = 0) { bAbbr(j) = 32: f = False
            Else
                bAbbr(j) = 32
            End if ( // f
        }
        auxFunctionAbbr(i) = RTrim(StrConv(bAbbr, vbUnicode))
        if (auxFunctionAbbr(i) = "") {
            if ((i = 7) && (modelType <> Heli)) { auxFunctionAbbr(i) = "MOT" Else auxFunctionAbbr(i) = "AUX" & CStr(i + 1)
        End if ( // auxFunctionAbbr(i) = ""
    }
*/
}

void getConditions(const std::vector<uint8_t>& data, eTxType txType, eModelType modelType)
{
    const size_t addr14CondSelect = 464/*451*/, addr18CondSelect = 64;
    const size_t addr14CondName = 1700, addr18CondName = 28140;
    const size_t addr14CondNameOffset = 9, addr18CondNameOffset = 578;

    for (size_t i = 0; i < t18Conditions; ++i) {
        conditionName[i] = L""; conditionState[i] = 0; conditionHw[i] = -1 /*hwOff*/; conditionList[i] = 0;
    }
    conditionState[0] = 128 + 15; 
    conditionList [0] = 1;

    // Get names of the conditions
    const size_t numConditions = (txType == T18SZ)? t18Conditions : t14Conditions;
    if (txType == T8FG) {
        switch (modelType) {
        case Heli:   conditionName[0] = L"NORMAL";   conditionName[1] = L"IDLEUP1"; conditionName[2] = L"IDLEUP2";
            conditionName[3] = L"IDLEUP3";  conditionName[4] = L"HOLD";
            break;
        case Glider: conditionName[0] = L"NORMAL";   conditionName[1] = L"START"; conditionName[2] = L"SPEED";
            conditionName[3] = L"DISTANCE"; conditionName[4] = L"LANDING";
            break;
        }
    }
    else // T18SZ or T14SZ
    {
        if (txType == T18SZ || modelType == Heli || modelType == Glider) 
        {
            for (size_t condIdx = 0; condIdx < numConditions; ++condIdx) 
            {
                std::array<wchar_t, 8 + 1> buffer;
                buffer.fill(0);
                for (size_t charIdx = 0; charIdx < 8; ++charIdx) 
                {
                    if (txType == T18SZ) { // UTF16
                        const char hi = static_cast<const char>(data.at(addr18CondName + condIdx*addr18CondNameOffset + charIdx * 2));
                        const char lo = static_cast<const char>(data.at(addr18CondName + condIdx*addr18CondNameOffset + charIdx * 2 + 1));
                        buffer[charIdx] = static_cast<wchar_t>((hi << 8) + lo);
                    } else { // T14SZ, 1-byte
                        buffer[charIdx] = data.at(addr14CondName + condIdx*addr14CondNameOffset + charIdx);
                    }

                    if (buffer[charIdx] == 0) {
                        break;
                    }
                }
                conditionName[condIdx] = std::wstring{ buffer.data() };
            }
        }
    }
 
    // <<< DEBUG : The following code doesn't make sense...
    if (txType == T18SZ || modelType == Heli || modelType == Glider) 
    {
        std::array<int, t18Conditions> cp; 
        cp.fill(0);

        const size_t addr = (txType == T18SZ)? addr18CondSelect : addr14CondSelect;
        for (size_t i = 1; i < numConditions; ++i) {
            const uint8_t v = data.at(addr + (i - 1) * 4);
            const uint8_t m = v & 0x0F;
            if (v > 127) {
                conditionState[m] = 128 + i;
                conditionHw   [m] = addr + (i - 1)*4 + 1; // <<< DEBUG "+1"?
            }
        }
        conditionState[0] = 128 + 15;
        for (size_t i = 1; i < t18Conditions;  ++i) {
            if (conditionState[i] > 128) {
                cp[conditionState[i] - 128] = i;
            }
        }

        for (size_t j=1, i=t18Conditions-1; /* i >= 0 */; --i) {
            if (cp[i] > 0) {
                conditionList[j] = cp[i]; 
                j = j + 1;
            }
            if (i == 0) break;
        }
    }
}

void getServoRevers(const std::vector<uint8_t>& data, eTxType txType)
{
    reversed.fill(false);
    reversedDG.fill(false);

    const size_t addr14RevLo = 268, addr14RevHi = 165, addr14RevDg = 154;
    const size_t addr18RevLo = 252, addr18RevHi = 253, addr18RevDg = 518;

    const bool isT18SZ = (txType == T18SZ);
    const uint8_t  revLo = data.at((isT18SZ)? addr18RevLo : addr14RevLo);
    const uint16_t revHi = data.at((isT18SZ)? addr18RevHi : addr14RevHi);
    const uint8_t  revDg = data.at((isT18SZ)? addr18RevDg : addr14RevDg) & 0xC0;
    const uint16_t rev = (revHi << 8) | revLo;

    const size_t numChannels = (isT18SZ)? t18Channels : t14Channels;
    for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
        reversed[chIdx] = (rev & (0x0001 << chIdx)) != 0;
    }
    reversedDG[0] = (revDg & 0x40) != 0;
    reversedDG[1] = (revDg & 0x80) != 0;
}

void endEndPoints(const std::vector<uint8_t>& data, eTxType txType)
{
    travelLo.fill(0); travelHi.fill(0);
    limitLo. fill(0); limitHi. fill(0);

    const size_t addr14TrlLo = 290, addr14TrlHi = 706, addr18TrlLo = 254, addr18TrlHi = 562;
    const size_t addr14LimLo = 664, addr14LimHi = 714, addr18LimLo = 278, addr18LimHi = 570;

    size_t atl, ath, ln, all, alh;
    const bool isT18SZ = (txType == T18SZ);
    if (isT18SZ) {
        atl = addr18TrlLo; ath = addr18TrlHi; ln = t18ChannelsLow;
        all = addr18LimLo; alh = addr18LimHi; 
    } else {
        atl = addr14TrlLo; ath = addr14TrlHi; ln = t14ChannelsLow; 
        all = addr14LimLo; alh = addr14LimHi; 
    }
    const size_t numChannels = (isT18SZ)? t18Channels : t14Channels;
    for (size_t i = 0; i < numChannels; ++i) {
        size_t j = (i < ln)? atl + i*2 : ath + (i - ln)*2;
        travelLo[i] = data.at(j); travelHi[i] = data.at(j + 1);
        j = (i < ln)? all + i*2 : alh + (i - ln)*2;
        limitLo[i] = data.at(j); limitHi[i] = data.at(j + 1);
    }
/*
    uf.ch1Function = fa(functn(1)); uf.ch2Function = fa(functn(2)); uf.ch3Function = fa(functn(3)); uf.ch4Function = fa(functn(4))
    uf.ch5Function = fa(functn(5)); uf.ch6Function = fa(functn(6)); uf.ch7Function = fa(functn(7)); uf.ch8Function = fa(functn(8))
    uf.ch9Function = fa(functn(9)); uf.ch10Function = fa(functn(10)); uf.ch11Function = fa(functn(11)); uf.ch12Function = fa(functn(12))
    uf.ch1TrlL = CStr(travel(1, 1)); uf.ch1TrlR = CStr(travel(1, 2)); uf.ch2TrlL = CStr(travel(2, 1)); uf.ch2TrlR = CStr(travel(2, 2))
    uf.ch3TrlL = CStr(travel(3, 1)); uf.ch3TrlR = CStr(travel(3, 2)); uf.ch4TrlL = CStr(travel(4, 1)); uf.ch4TrlR = CStr(travel(4, 2))
    uf.ch5TrlL = CStr(travel(5, 1)); uf.ch5TrlR = CStr(travel(5, 2)); uf.ch6TrlL = CStr(travel(6, 1)); uf.ch6TrlR = CStr(travel(6, 2))
    uf.ch7TrlL = CStr(travel(7, 1)); uf.ch7TrlR = CStr(travel(7, 2)); uf.ch8TrlL = CStr(travel(8, 1)); uf.ch8TrlR = CStr(travel(8, 2))
    uf.ch9TrlL = CStr(travel(9, 1)); uf.ch9TrlR = CStr(travel(9, 2)); uf.ch10TrlL = CStr(travel(10, 1)); uf.ch10TrlR = CStr(travel(10, 2))
    uf.ch11TrlL = CStr(travel(11, 1)); uf.ch11TrlR = CStr(travel(11, 2)); uf.ch12TrlL = CStr(travel(12, 1)); uf.ch12TrlR = CStr(travel(12, 2))
    uf.ch1LimL = CStr(limit(1, 1)); uf.ch1LimR = CStr(limit(1, 2)); uf.ch2LimL = CStr(limit(2, 1)); uf.ch2LimR = CStr(limit(2, 2))
    uf.ch3LimL = CStr(limit(3, 1)); uf.ch3LimR = CStr(limit(3, 2)); uf.ch4LimL = CStr(limit(4, 1)); uf.ch4LimR = CStr(limit(4, 2))
    uf.ch5LimL = CStr(limit(5, 1)); uf.ch5LimR = CStr(limit(5, 2)); uf.ch6LimL = CStr(limit(6, 1)); uf.ch6LimR = CStr(limit(6, 2))
    uf.ch7LimL = CStr(limit(7, 1)); uf.ch7LimR = CStr(limit(7, 2)); uf.ch8LimL = CStr(limit(8, 1)); uf.ch8LimR = CStr(limit(8, 2))
    uf.ch9LimL = CStr(limit(9, 1)); uf.ch9LimR = CStr(limit(9, 2)); uf.ch10LimL = CStr(limit(10, 1)); uf.ch10LimR = CStr(limit(10, 2))
    uf.ch11LimL = CStr(limit(11, 1)); uf.ch11LimR = CStr(limit(11, 2)); uf.ch12LimL = CStr(limit(12, 1)); uf.ch12LimR = CStr(limit(12, 2))
    uf.ch13Label.Visible = isT18SZ; uf.ch14Label.Visible = isT18SZ; uf.ch15Label.Visible = isT18SZ; uf.ch16Label.Visible = isT18SZ
    uf.ch13Function.Visible = isT18SZ; uf.ch14Function.Visible = isT18SZ; uf.ch15Function.Visible = isT18SZ; uf.ch16Function.Visible = isT18SZ
    uf.ch13TrlL.Visible = isT18SZ; uf.ch13TrlR.Visible = isT18SZ; uf.ch14TrlL.Visible = isT18SZ; uf.ch14TrlR.Visible = isT18SZ
    uf.ch15TrlL.Visible = isT18SZ; uf.ch15TrlR.Visible = isT18SZ; uf.ch16TrlL.Visible = isT18SZ; uf.ch16TrlR.Visible = isT18SZ
    uf.ch13LimL.Visible = isT18SZ; uf.ch13LimR.Visible = isT18SZ; uf.ch14LimL.Visible = isT18SZ; uf.ch14LimR.Visible = isT18SZ
    uf.ch15LimL.Visible = isT18SZ; uf.ch15LimR.Visible = isT18SZ; uf.ch16LimL.Visible = isT18SZ; uf.ch16LimR.Visible = isT18SZ
    if (isT18SZ) {
        uf.ch13Function = fa(functn(13)); uf.ch14Function = fa(functn(14)); uf.ch15Function = fa(functn(15)); uf.ch16Function = fa(functn(16))
        uf.ch13TrlL = CStr(travel(13, 1)); uf.ch13TrlR = CStr(travel(13, 2)); uf.ch14TrlL = CStr(travel(14, 1)); uf.ch14TrlR = CStr(travel(14, 2))
        uf.ch15TrlL = CStr(travel(15, 1)); uf.ch15TrlR = CStr(travel(15, 2)); uf.ch16TrlL = CStr(travel(16, 1)); uf.ch16TrlR = CStr(travel(16, 2))
        uf.ch13LimL = CStr(limit(13, 1)); uf.ch13LimR = CStr(limit(13, 2)); uf.ch14LimL = CStr(limit(14, 1)); uf.ch14LimR = CStr(limit(14, 2))
        uf.ch15LimL = CStr(limit(15, 1)); uf.ch15LimR = CStr(limit(15, 2)); uf.ch16LimL = CStr(limit(16, 1)); uf.ch16LimR = CStr(limit(16, 2))
    }
*/
}


int main()
{
    std::vector<uint8_t> data;
    for (const char* fname : { "data\\KatanaMX",  "data\\3DHKatana",
                               "data\\ShurikBipe","data\\FASSTest-2" 
                               "data\\COND_SA", "data\\COND_SA2"
                             })
    {
        const bool loaded = LoadFromFile(fname, data);
        if (loaded && !data.empty())
        {
            const eTxType txType = getTransType(data);
            std::cout << "TX: " << ((txType == INVALID_TX)? "INVALID" : std::array<char*, 3>{"T8FG", "T14SG", "T18SZ"}[txType]) << std::endl;

            const std::wstring txName = getModelName(data, txType);
            std::wcout << L"Model name: \"" << txName << L"\"" << std::endl;

            const eModelType modelType = getModelType(data, txType);
            std::cout << "Model: " << ((modelType == INVALID_MODEL)? "INVALID" : std::array<char*, 4>{"Plane", "Heli", "Glider", "Multi"}[modelType]) << std::endl;

            const char* modulationList[16] = {
                "FASST 7CH",     "FASST MULTI", "FASST MLT2",    "--",
                "S-FHSS",        "--",          "--",            "--",
                "FASSTest 14CH", "--",          "FASSTest 12CH", "--",
                "T-FHSS",        "--",          "--",            "--" };
            int m = getModulation(data, txType);
            std::cout << "Modulation: " << modulationList[m] << std::endl;

            getFunction   (data, txType, modelType);
            getServoRevers(data, txType);
            endEndPoints  (data, txType);
            std::cout << "Reverse & End Point" << std::endl;
            const size_t numChannels = (txType == T18SZ)? t18Channels : t14Channels;
            for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
                std::cout << "\t" << std::setw(2) << chIdx + 1 << " " << std::setw(10) << std::left << fa[functn[chIdx]] << ": "
                    << ((reversed[chIdx])? "REVERS" : "      "/*"normal"*/) << "  "
                    << std::right << std::setw(3) << (int)limitLo[chIdx] << "  " << std::setw(3) << (int)travelLo[chIdx] << "   "
                    << std::setw(3) << (int)travelHi[chIdx] << "  " << std::setw(3) << (int)limitHi[chIdx] << std::endl;
            }

            getConditions(data, txType, modelType);
            if (txType == T18SZ || modelType == Heli || modelType == Glider) {
                std::cout << "Condition #" << std::endl;
                const size_t numConditions = (txType == T18SZ)? t18Conditions : t14Conditions;
                for (size_t condIdx = 0; condIdx < numConditions; ++condIdx) {
                    std::wcout << L"\t" << condIdx + 1 << L": " << conditionName[condIdx]
                        //<< L", state: " << conditionState[condIdx]
                        //<< L", list: " << conditionList[condIdx]
                        << std::endl;
                    ;
                }
            }
        } else {
            std::cout << "Failed to load \""<< fname <<"\"" << std::endl;
        }
        std::cout << "-------------------" << std::endl;
    }
    return 0;
}


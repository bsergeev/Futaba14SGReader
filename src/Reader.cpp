#include <array>
#include <cstdint>
#include <cassert>
#include <iomanip>  // std::setw
#include <iostream>
#include <fstream>  // std::ifstream
#include <limits>
#include <math.h>   // round
#include <string>
#include <vector>

using namespace std::string_literals;

constexpr size_t t14Channels = 12, t18Channels = 16;
constexpr size_t chMax = std::max(t14Channels, t18Channels);

constexpr size_t t14ChannelsLow = 8, t18ChannelsLow = 12;

const size_t t14Conditions = 5,  t18Conditions = 8;
constexpr size_t maxConds = std::max(t14Conditions, t18Conditions);

const size_t functionNumber = 33;

enum eTxType {
    INVALID_TX = 255, // <<< DEBUG
    T8FG  = 0,
    T14SG = 1,
    T18SZ = 2
};
enum class eAreaType {
    UNKNOWN,
    General,
    France
};
enum eModelType {
    INVALID_MODEL = -1,
    Plane  = 0,
    Heli   = 1,
    Glider = 2,
    Multi  = 3
};
enum class eTrimMode {
    INVALID = 0,
    Normal, ATLRev, ATLNorm, Center
};

typedef size_t hwControlIdx_t;
constexpr std::array<const char*, 32> hwCtrlDesc = { 
    "J1", "J2", "J4", "J3", "SC", "SD", "SG", "SH", "RD", "RS", 
    "OA", "0B", "SA", "SB", "SE", "SF", "LD", "11", "LS", "13",
    "T1", "T2", "T4", "T3", "T5", "T6", "T7", "1B", "1C", "1D", "1E", "--" };
constexpr hwControlIdx_t NO_CONTROL_IDX = hwCtrlDesc.size() - 1; // "--" is last

uint8_t  m_wingType = 0;
uint8_t  m_tailType = 0;
uint16_t m_FSMode   = 0;
uint16_t m_FSBattery= 0;
std::string m_releaseBfsHW;
bool m_sysTelemAct = false;
bool m_singleRX  = true;
eAreaType m_Area = eAreaType::UNKNOWN;
double m_telemDlInterval = 0.0;

std::array<bool, chMax> reversed;
std::array<bool, 2>     reversedDG;
std::array<uint8_t, chMax> travelLo, travelHi, limitLo, limitHi;
std::array<uint8_t, chMax> sSpeed; // [0, 27]
std::array<int16_t, chMax> sTrim;  // [-240, 240]
std::array<int16_t, chMax> fsPosition;
std::array<hwControlIdx_t, chMax> trim;
std::array<int16_t, chMax>   trimRate;
std::array<eTrimMode, chMax> trimMode;

struct RxInfo {
    uint32_t ID = 0; // invalid
    double   BatteryFsV = 0.0;
};
std::array<RxInfo, 2> RX;

size_t numConditions = 1; // 1 for condition-less models, or set in getConditions()
struct ConditionDependentParams {
    ConditionDependentParams() { control.fill(NO_CONTROL_IDX); }

    bool operator ==(const ConditionDependentParams& o) const {
        for (size_t i = 0; i < chMax; ++i) {
            if (!(control[i] == o.control[i])) {
                return false;
            }
        }
        return true;
    }

    std::array<hwControlIdx_t, chMax> control;
    std::string conditionControl; 
};
std::vector<ConditionDependentParams> m_conditionalData; // .size() == numConditions

std::array<uint8_t, chMax> functn; // value is the index of FunctionNames_t, i.e. < 33

typedef std::array<std::string, functionNumber> FunctionNames_t;
FunctionNames_t& fa = FunctionNames_t{};
FunctionNames_t functionListAir = {
    "Aileron"s, "Elevator"s, "Throttle"s, "Rudder"s, "Gear"s, 
    "Flap"s, "Aileron2"s, "Aileron3"s, "Aileron4"s, "Elevator2"s, 
    "Flap2"s, "Air brake"s, "Fuel mix"s, "Gyro"s, "Gyro2"s, 
    "Gyro3"s, "Throttle2"s, "Throttle3"s, "Throttle4"s, "Flap3"s, 
    "Flap4"s, "Rudder2"s, "Butterfly"s, "Camber"s, "Motor"s, 
    "Auxiliary7"s, "Auxiliary6"s, "Auxiliary5"s, "Auxiliary4"s, "Auxiliary3"s, 
    "Auxiliary2"s, "Auxiliary1"s, "--"s };
FunctionNames_t functionListHeli = {
    "Aileron"s, "Elevator"s, "Throttle"s, "Rudder"s, "Gear"s, 
    "Pitch"s, "Governor"s, "Governor2"s, "Aileron4"s, "Elevator2"s,
    "Flap2"s, "Needle"s, "Fuel mix"s, "Gyro"s, "Gyro2"s, 
    "Gyro3"s, "Throttle2"s, "Throttle3"s, "Throttle4"s, "Flap3"s, 
    "Flap4"s, "Rudder2"s, "Butterfly"s, "Camber"s, "Auxiliary8"s, 
    "Auxiliary7"s, "Auxiliary6"s, "Auxiliary5"s, "Auxiliary4"s, "Auxiliary3"s, 
    "Auxiliary2"s, "Auxiliary1"s, "--"s };
FunctionNames_t functionListMulti = {
    "Aileron"s, "Elevator"s, "Throttle"s, "Rudder"s, "Gear"s, 
    "Flap"s, "Aileron2"s, "Aileron3"s, "Aileron4"s, "Elevator2"s, 
    "Flap2"s, "Air brake"s, "Fuel mix"s, "Gyro"s, "Gyro2"s, 
    "Gyro3"s, "Camera roll"s, "Camera tilt"s, "Camera pan"s, "Camera rec"s, 
    "Mode"s, "Rudder2"s, "Butterfly"s, "Camber"s, "Motor"s, 
    "Auxiliary7"s, "Auxiliary6"s, "Auxiliary5"s, "Auxiliary4"s, "Auxiliary3"s, 
    "Auxiliary2"s, "Auxiliary1"s, "--"s };

static const std::array<uint8_t, 16> telemType = { 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 2, 0, 0, 0 };
static const std::array<double, 16> tfhssVoltList = { 3.8, 0.0, 4.0, 4.2, 4.4, 4.6, 4.8, 5.0,
                                                      5.3, 5.6, 5.9, 6.2, 6.5, 6.8, 7.1, 7.4 };

// & -> Long
// % -> Integer
// # -> Double
// ! -> Single
// @ -> Decimal
// $ -> String

std::array<std::wstring, t18Conditions> conditionName;
std::array<size_t,       t18Conditions> conditionState, conditionList;
std::array<size_t,       t18Conditions> conditionHw;
std::array<std::string, 2> digiCtrl;

struct HwNamnes {
    int8_t Type = -1;
    std::string Ctrl, Pos, Rev, Sym;
};

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
            inStream.seekg(0, inStream.beg);

            data.resize(length);
            inStream.read(reinterpret_cast<char*>(&data[0]), length);

            ok = !inStream.fail();
        }
    }
    return ok;
}

eTxType getTxType(const std::vector<uint8_t>& data)
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

size_t getModulation(const std::vector<uint8_t>& data, eTxType txType)
{
    size_t sysModulation = 0;

    const size_t addr14sysTyp = 154, div14sysTyp = 1;
    const size_t addr18sysTyp = 92,  div18sysTyp = 16;

    const size_t am = (txType == T18SZ)? addr18sysTyp : addr14sysTyp;
    if (txType == T8FG) {
        const size_t v = ((data.at(am) & 0x30) + (data.at(am + 1) & 0x80)) >> 4;
        switch (v) {
            case 1: sysModulation = 1; break;
            case 3: sysModulation = 0; break;
            case 9: sysModulation = 2; break;
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
      case Glider: fa = functionListAir;   break;
      case Heli:   fa = functionListHeli;  break;
      case Multi:  fa = functionListMulti; break;
    }
    if (txType != T18SZ && modelType == Plane) {
        fa[25] = "VPP"s;
        return;
    }
}

void getConditions(const std::vector<uint8_t>& data, eTxType txType, eModelType modelType)
{
    const size_t addr14CondSelect = /*464*/451, addr18CondSelect = 64;
    const size_t addr14CondName = 1700, addr18CondName = 28140;
    const size_t addr14CondNameOffset = 9, addr18CondNameOffset = 578;

    for (size_t i = 0; i < t18Conditions; ++i) {
        conditionName[i] = L""; conditionState[i] = 0; conditionHw[i] = -1 /*hwOff*/; conditionList[i] = 0;
    }
    numConditions = 1;
    conditionState[0] = 128 + 15; 
    conditionList [0] = 0;

    // Get names of the conditions
    const size_t numTxConditions = (txType == T18SZ)? t18Conditions : t14Conditions;
    if (txType == T8FG) {
        switch (modelType) {
        case Heli:   conditionName[0] = L"NORMAL";   conditionName[1] = L"IDLEUP1"; conditionName[2] = L"IDLEUP2";
            conditionName[3] = L"IDLEUP3";  conditionName[4] = L"HOLD";
            numConditions = 5;
            break;
        case Glider: conditionName[0] = L"NORMAL";   conditionName[1] = L"START"; conditionName[2] = L"SPEED";
            conditionName[3] = L"DISTANCE"; conditionName[4] = L"LANDING";
            numConditions = 5;
            break;
        }
    }
    else // T18SZ or T14SZ
    {
        if (txType == T18SZ || modelType == Heli || modelType == Glider) 
        {
            numConditions = numTxConditions;
            for (size_t condIdx = 0; condIdx < numTxConditions; ++condIdx)
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
 
    // <<< DEBUG : The following code doesn't make sense (although seems to work)...
    if (txType == T18SZ || modelType == Heli || modelType == Glider) 
    {
        std::array<size_t, t18Conditions> cp; 
        cp.fill(0);

        const size_t addr = (txType == T18SZ)? addr18CondSelect : addr14CondSelect;
        for (size_t i = 1; i < numTxConditions; ++i) {
            const uint8_t v = data.at(addr + (i - 1) * 4);
            const uint8_t m = v & 0x0F;
            if (v > 127) {
                conditionState[m] = 128 + i;
                conditionHw   [m] = addr + (i - 1)*4 + 1;
            }
        }
        conditionState[0] = 128 + 15;
        for (size_t i = 1; i < t18Conditions;  ++i) {
            if (conditionState[i] > 128) {
                cp[conditionState[i] - 128 -1] = i;
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
 
HwNamnes getHardware(const std::vector<uint8_t>& data, size_t a)
{
    HwNamnes hw;
    const uint8_t hC0 = data.at(a);
    const uint8_t i1 = data.at(a + 1);
    const uint8_t i2 = data.at(a + 2);
    if (hC0 == 0xFF) {
        hw.Type = -1; hw.Ctrl = "--";
        hw.Pos = (i1 != 0 || i2 != 0)? "OFF" : "ON";
        return hw;
    }
    const uint8_t hR = hC0 & 0x40;
    const uint8_t hC = hC0 & 0x3F;
    if (hC >= 32) { hw.Ctrl = "Logic"; return hw; }
    hw.Ctrl = hwCtrlDesc[hC];
    if ((hC & 0x34) == 4) {
        if ((hC & 0x37) == 7) {
            hw.Type = 2; // 2-position switch
            hw.Pos = (hR == 0)? "OFF/ON" : "ON/OFF";
            return hw;
        }
        hw.Type = 3; // 3 - position switch
        if (hR != 0)                          { hw.Pos = "ON/OFF/ON";  return hw; }
        if ((i1 & 0x80) == 0 && i2 >= 0x40)   { hw.Pos = "OFF/OFF/ON"; return hw; }
        if ((i1 & 0x80) != 0 && i2 >= 0x40)   { hw.Pos = "ON/OFF/OFF"; return hw; } // was "OFF/ON/ON"
        if ((i1 <= 0xC0) && (i2 & 0x80) == 0) { hw.Pos = "ON/ON/OFF";  return hw; }
        if ((i1 <= 0xC0) && (i2 & 0x80) != 0) { hw.Pos = "OFF/ON/ON";  return hw; } // was "ON/OFF/OFF"
        else                                  { hw.Pos = "OFF/ON/OFF"; return hw; }
    }
    hw.Type = 0; // Analog input
    hw.Rev = (hR == 0)? "Normal" : "Reverse";
    if (static_cast<uint16_t>(i1) + i2 == 0x0100) { hw.Sym = "Symmetry"; hw.Pos = std::to_string(round(static_cast<int8_t>(i2)*100.0 / 64)); return hw; }
    if (i1 - i2 == 1)                             { hw.Sym = "Linear";   hw.Pos = std::to_string(round(static_cast<int8_t>(i1)*100.0 / 64)); return hw; }
    hw.Pos = "Error!!!"; hw.Rev = ""; hw.Sym = "";
    return hw;
}


void conditionSelect(const std::vector<uint8_t>& data, eTxType txType)
{
    auto logicSwitch = [&data, txType](size_t a) -> std::string {
        const size_t addr14logSw = 328, addr18logSw = 456;
        const size_t aa = (txType == T18SZ)? addr18logSw : addr14logSw;
        if ((data.at(a) & 48) == 48) 
        {
            auto hw = getHardware(data, aa + (data.at(a) & 7) * 6);
            std::string alt = hw.Ctrl +" "+ hw.Pos +" "+ hw.Rev +" "+ hw.Sym;
            if (data.at(a + 1) & 128) {
                alt = alt + " Alternate";
            }
            std::string l;
            switch (data.at(a + 1) % 4) {
            case 0: l = "AND";     break;
            case 1: l = "OR";      break;
            case 2: l = "EX-OR";   break;
            case 3: l = "!UNDEF!"; break;
            }
            alt = alt + "  " + l + "  ";
            hw = getHardware(data, aa + (data.at(a) & 7) * 6 + 3);
            alt = alt + hw.Ctrl + " " + hw.Pos + " " + hw.Rev + " " + hw.Sym;
            if (data.at(a + 1) & 0x40) {
                alt = alt + " Alternate";
            }
            return alt;
        } else {
            auto hw = getHardware(data, a);
            return hw.Ctrl + " " + hw.Pos + " " + hw.Rev + " " + hw.Sym;
        }
    };

    for (size_t i = 1; i < numConditions; ++i) {
        if (conditionList[i] != 0) {
            m_conditionalData[i].conditionControl = logicSwitch(conditionHw[conditionList[i]]);
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

void getEndPoints(const std::vector<uint8_t>& data, eTxType txType)
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
}

uint8_t cServoSpeed(uint8_t y) {
    if (y < 67)  { return static_cast<uint8_t>(round( y / 10)); }
    if (y < 78)  { return static_cast<uint8_t>(round((y - 67)/4) + 7); }
    if (y == 78) { return 10; } // y - 68;
    if (y < 88)  { return static_cast<uint8_t>(round((y - 80)/3) + 11); }
    return  (y - 74);
}

void getServoSpeed(const std::vector<uint8_t>& data, eTxType txType)
{
    sSpeed.fill(0);

    const size_t addr14sSpLo = 1812, addr14sSpHi = 1828, addr18sSpLo = 438, addr18sSpHi = 594;
    const bool isT18SZ = (txType == T18SZ);
    size_t al, ah, ln, k;
    if (isT18SZ) {
        al = addr18sSpLo; ah = addr18sSpHi; ln = t18ChannelsLow; k = 1;
    } else {
        al = addr14sSpLo; ah = addr14sSpHi; ln = t14ChannelsLow; k = 2;
    }
    const size_t numChannels = (isT18SZ)? t18Channels : t14Channels;
    for (size_t i = 0; i < numChannels; ++i) {
        const size_t j = (i < ln)? al + i*k : ah + (i - ln)*k;
        sSpeed[i] = data.at(j);
    }
}

void getSubTrim(const std::vector<uint8_t>& data, eTxType txType)
{
    sTrim.fill(0);

    const size_t  addr14sTrLo = 306, addr14sTrHi = 166, addr18sTrLo = 414, addr18sTrHi = 586;
    const bool isT18SZ = (txType == T18SZ);
    size_t al, ah, ln;
    if (isT18SZ) {
        al = addr18sTrLo; ah = addr18sTrHi; ln = t18ChannelsLow; 
    } else { 
        al = addr14sTrLo; ah = addr14sTrHi; ln = t14ChannelsLow; 
    }
    const size_t numChannels = (isT18SZ)? t18Channels : t14Channels;
    for (size_t i = 0; i < numChannels; ++i) {
        const size_t j = (i < ln)? al + i*2 : ah + (i - ln)*2;
        const uint8_t hi = data.at(j);
        const uint8_t lo = data.at(j + 1);
        sTrim[i] = (hi << 8) | lo;
    }
}

void getControlAssignment(const std::vector<uint8_t>& data, eTxType txType, eModelType modelType)
{
    const size_t addr18CondStart = 640, t18CondLength = 3056, addr18fnXC = 118;
    const size_t addr14fnGrBfly = 1545, addr14fnGrCamb = 1453, addr14fnGrMot = 1539;
    const size_t addr14fnXC = 190, addr14fnCtrl = 222, addr14fnTrim = 234;
    const size_t addr18fnTRt = 182, addr18fnTSg = 519;
    const size_t addr14fnTRt = 246, addr14fnTSg = 258;
    const size_t addr18dgCtrl = 450, addr14dgCtrl = 322, addr14dgAlt = 681;
    const size_t addr14fnTCn = 260, addr14fnTMd = 260, addr14fnTMr = 262;
    const size_t addr18fnTCn = 214, addr18fnTMd = 218, addr18fnTMr = 222;

    assert(numConditions > 0);
    m_conditionalData.resize(numConditions);

    const bool isT18SZ = (txType == T18SZ);

    // Control assignments for each condition
    for (size_t condIdx = 0; condIdx < numConditions; ++condIdx)
    {
        auto& cd = m_conditionalData.at(condIdx);
        if (isT18SZ)
        {
            const size_t  ac = addr18CondStart, lc = t18CondLength, axc = addr18fnXC;
            for (size_t i = 0; i < t18Channels; ++i) {
                size_t a1 = axc + functn[i];
                size_t a2 = ac + lc * (conditionList[condIdx]) + data.at(a1);
                cd.control[i] = std::min<hwControlIdx_t>(NO_CONTROL_IDX, data.at(a2));
                a1 = axc + functn[i] + functionNumber - 1;
                a2 = ac + lc * (conditionList[condIdx]) + chMax + data.at(a1);
                trim[i] = std::min<hwControlIdx_t>(NO_CONTROL_IDX, data.at(a2)); // <<< DEBUG move out of condIdx loop!
            }
        } else {
            static const std::array<size_t, 3> ag = { addr14fnGrBfly, addr14fnGrCamb, addr14fnGrMot };
            for (size_t i = 0; i < t14Channels; ++i) {
                size_t a2 = addr14fnCtrl + data.at(addr14fnXC + functn[i]);
                if (functn[i] >= 22 && functn[i] <= 24 && modelType == Glider) {
                    const size_t a1 = ag[functn[i]-22];
                    if (data.at(a1) > 127) {
                        a2 = a1 + conditionList[condIdx] + 1;
                    }
                }
                cd.control[i] = std::min<hwControlIdx_t>(NO_CONTROL_IDX, data.at(a2));
                a2 = addr14fnTrim + data.at(addr14fnXC + functn[i]);
                trim[i] = std::min<hwControlIdx_t>(NO_CONTROL_IDX, data.at(a2)); // <<< DEBUG move out of condIdx loop!
            }
        }
    }

    // Controls for DGs (same for all conditions)
    const std::string ls = " ";
    for (size_t ch = 0; ch < 2; ++ch)
    {
        if (isT18SZ)
        {
            if ((data.at(addr18dgCtrl + ch*3) & 48) == 48) 
            {
                auto hw = getHardware(data, addr18dgCtrl + ((data.at(addr18dgCtrl + ch*3) & 7) + 1) * 6);
                std::string alt = hw.Ctrl +" "+ hw.Pos +" "+ hw.Rev +" "+ hw.Sym;
                if (data.at(addr18dgCtrl + ch*3 + 1) & 128) {
                    alt = alt + " Alternate";
                }
                std::string l;
                switch (data.at(addr18dgCtrl + ch * 3 + 1) % 4) {
                case 0: l = "AND";     break;
                case 1: l = "OR";      break;
                case 2: l = "EX-OR";   break;
                case 3: l = "!UNDEF!"; break;
                }
                alt = alt + "   " + l + "   ";
                hw = getHardware(data, addr18dgCtrl + ((data.at(addr18dgCtrl + ch * 3) & 7) + 1) * 6 + 3);
                alt = alt + hw.Ctrl + " " + hw.Pos + " " + hw.Rev + " " + hw.Sym;
                if (data.at(addr18dgCtrl + ch*3 + 1) & 0x40) { 
                    alt = alt + " Alternate";
                }
                digiCtrl[ch] = alt;
            } else {
                auto hw = getHardware(data, addr18dgCtrl + ch * 3);
                digiCtrl[ch] = ls + hw.Ctrl + "  " + hw.Pos + "  " + hw.Rev + "  " + hw.Sym;
            }
        } else { // <<< DEBUG TBD: handle 'Logic' for 14SG
            const uint8_t m = 1 << ch;
            std::string alt = (data.at(addr14dgAlt) & m)? "Alternate" : "";
            auto hw = getHardware(data, addr14dgCtrl + ch * 3);
            digiCtrl[ch] = ls + hw.Ctrl +"  "+ hw.Pos +"  "+ hw.Rev +"  "+ hw.Sym +"  "+ alt;
        }
    }

    // Trim rates
    const size_t numChannels = (isT18SZ)? t18Channels : t14Channels;
    for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
        size_t atr, ats, x;
        if (isT18SZ) {
            atr = addr18fnTRt; ats = addr18fnTSg; x = functn[chIdx];
        } else {
            atr = addr14fnTRt; ats = addr14fnTSg; x = data.at(addr14fnXC + functn[chIdx]);
        }
        const uint8_t m = 1 << (x % 8);
        trimRate[chIdx] = ((data.at(ats + x/8) & m)? -1 : 1)*static_cast<int8_t>(data.at(atr + x));
    }

    // Trim mode
    for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
        size_t atc, atm, atr, x;
        if (isT18SZ) {
            atc = addr18fnTCn; atm = addr18fnTMd; atr = addr18fnTMr; x = functn[chIdx];
        } else {
            atc = addr14fnTCn; atm = addr14fnTMd; atr = addr14fnTMr; x = data.at(addr14fnXC + functn[chIdx]);
        }
        const uint8_t m = 1 << (x % 8);
        if ((data.at(atc + x/8) & m) == 0 && (data.at(atm + x/8) & m) == 0) {
            trimMode[chIdx] = eTrimMode::Normal;
        } else {
            if (data.at(atm + x/8) & m) {
                trimMode[chIdx] = (data.at(atr + x/8) & m)? eTrimMode::ATLRev : eTrimMode::ATLNorm;
            } else {
                trimMode[chIdx] = eTrimMode::Center;
            }
        }
    }
}

void getFailSafe(const std::vector<uint8_t>& data, eTxType txType)
{
    const size_t addr14fsLo    = 269, addr14fsHi    = 697, addr18fsLo = 334, addr18fsHi = 335;
    const size_t addr14bfsLo   = 286, addr14bfsHi   = 164, addr18bfsLo = 360, addr18bfsHi = 361;
    const size_t addr14fsPosLo = 270, addr14fsPosHi = 698, addr18fsPosLo = 336, addr18fsPosHi = 578;
    const size_t addr14relBFS  = 287, addr18relBFS  = 362;

    const bool isT18SZ = (txType == T18SZ);
    m_FSMode    = (data.at((isT18SZ)? addr18fsHi  : addr14fsHi)  << 8) + data.at((isT18SZ)? addr18fsLo : addr14fsLo);
    m_FSBattery = (data.at((isT18SZ)? addr18bfsHi : addr14bfsHi) << 8) + data.at((isT18SZ)? addr18bfsLo : addr14bfsLo);

    size_t al, ah, ln;
    if (isT18SZ) { 
        al = addr18fsPosLo; ah = addr18fsPosHi; ln = t18ChannelsLow; 
    } else {
        al = addr14fsPosLo; ah = addr14fsPosHi; ln = t14ChannelsLow;
    }
    const size_t numChannels = (txType == T18SZ)? t18Channels : t14Channels;
    for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
        const size_t addr = (chIdx < ln)? al + chIdx*2
                                        : ah + (chIdx - ln)*2;
        const int16_t st = (data.at(addr) << 8) + data.at(addr + 1);
        fsPosition[chIdx] = static_cast<int16_t>(round((st - 1024)/6.73));
    }

    auto hw = getHardware(data, (isT18SZ)? addr18relBFS : addr14relBFS);
    m_releaseBfsHW = hw.Ctrl + "  " + hw.Pos + "  " + hw.Rev + "  " + hw.Sym;
}

void getSystemInfo(const std::vector<uint8_t>& data, eTxType txType, size_t sysModulation)
{
    auto getRxID = [&data](size_t a) { return (data.at(a) << 24) | (data.at(a + 1) << 16) | (data.at(a + 2) << 8) | data.at(a + 3); };

    auto getBFsVoltage = [&data](size_t sysModulation, size_t a) {
        const size_t tlmType = telemType[sysModulation % telemType.size()];
        const double v = (tlmType == 1)? data.at(a) / 10.0
                       : (tlmType == 2)? tfhssVoltList[data.at(a) % tfhssVoltList.size()]
                       : 0.0;
        return v;
    };

    constexpr size_t rxIDlen = 4;
    const size_t addr14IDRx1 = 2196, addr14IDRx2 = addr14IDRx1 + rxIDlen;
    const size_t addr18IDRx1 = 406,  addr18IDRx2 = addr18IDRx1 + rxIDlen;
    const size_t addr14sysAr = 156, addr14rxQty = 2204, mask14rxQty = 1;
    const size_t addr18sysAr = 0,   addr18rxQty = 516,  mask18rxQty = 2;
    const size_t addr14tAct = 2206, mask14tAct = 128;
    const size_t addr18tAct = 92,   mask18tAct = 2;
    const size_t addr14dlI = 2206, div14dlI = 1;
    const size_t addr18dlI = 516, div18dlI = 4;
    const size_t addr14bfsvRx1 = 2208, addr14bfsvRx2 = 2209;
    const size_t addr18bfsvRx1 = 401, addr18bfsvRx2 = 402;

    size_t aa, ar, ai1, ai2, ata, mta, adl, ddl, av1, av2, mr;
    if (txType == T18SZ) {
        aa = addr18sysAr; ar = addr18rxQty; ai1 = addr18IDRx1; ai2 = addr18IDRx2; ata = addr18tAct; mta = mask18tAct;
        adl = addr18dlI; ddl = div18dlI; av1 = addr18bfsvRx1; av2 = addr18bfsvRx2; mr = mask18rxQty;
    } else {
        aa = addr14sysAr; ar = addr14rxQty; ai1 = addr14IDRx1; ai2 = addr14IDRx2; ata = addr14tAct; mta = mask14tAct;
        adl = addr14dlI; ddl = div14dlI; av1 = addr14bfsvRx1; av2 = addr14bfsvRx2; mr = mask14rxQty;
    }
    m_sysTelemAct = (data.at(ata) & mta) != 0;
    if (txType != T18SZ) { // && (sysModulation & 0x03) != 0) { // i.e. "FASST MULTI" or "FASST MLT2" // <<< DEBUG correct?
        m_Area = ((data.at(aa) & 0x80) == 0)? eAreaType::General : eAreaType::France;
    }
    if (telemType[sysModulation] != 0) {
        m_singleRX = (data.at(ar) & mr) == 0 || sysModulation == 10; // "FASSTest 12CH" // <<< DEBUG correct?
        RX[0].ID = getRxID(ai1);
        RX[0].BatteryFsV = getBFsVoltage(sysModulation, av1);
        if (!m_singleRX) {
            RX[1].ID = getRxID(ai2);
            RX[1].BatteryFsV = getBFsVoltage(sysModulation, av2);
        }
    }
    if (telemType[sysModulation] == 1) {
        m_telemDlInterval = ((data.at(adl) / ddl) & 0x1F) / 10.0;
    }
}


int main()
{
    std::vector<uint8_t> data;
    for (const char* fname : { 
                               //"data\\KatanaMX",  "data\\3DHKatana",
                               //"data\\ShurikBipe",
                               //"data\\FASSTest-2", "data\\COND_SA"//,   
                               "data\\COND_SA2"
                             })
    {
        const bool loaded = LoadFromFile(fname, data);
        if (loaded && !data.empty())
        {
            const eTxType txType = getTxType(data);
            std::cout << "TX: " << ((txType == INVALID_TX)? "INVALID" : std::array<char*, 3>{"T8FG", "T14SG", "T18SZ"}[txType]) << std::endl;

            const std::wstring txName = getModelName(data, txType);
            std::wcout << L"Model name: \"" << txName << L"\"" << std::endl;

            const eModelType modelType = getModelType(data, txType);
            std::cout << "Model: " << ((modelType == INVALID_MODEL)? "INVALID" : std::array<char*, 4>{"Plane", "Heli", "Glider", "Multi"}[modelType]) << "\n\n";

            const size_t modulation = getModulation(data, txType);
            getSystemInfo(data, txType, modulation);
            const char* modulationList[16] = {"FASST 7CH",     "FASST MULTI", "FASST MLT2",    "--",
                                              "S-FHSS",        "--",          "--",            "--",
                                              "FASSTest 14CH", "--",          "FASSTest 12CH", "--",
                                              "T-FHSS",        "--",          "--",            "--" };
            std::cout << "SYSTEM" << std::endl;
            std::cout << "\t" << modulationList[modulation] << "  "
                << ((m_singleRX)? "SINGLE" : "DUAL") << " "
                << ((m_Area == eAreaType::UNKNOWN)? "" : std::array<const char*,2>{"G", "F"}[(int)m_Area-(int)eAreaType::UNKNOWN-1])
                << std::endl;
            std::cout << "\t" << RX[0].ID;
            if (!m_singleRX) { std::cout << "\t\t" << RX[1].ID; }
            std::cout << std::endl;
            std::cout << "\t" << std::setprecision(2) << RX[0].BatteryFsV <<"V";
            if (!m_singleRX) { std::cout << "\t\t" << std::setprecision(2) << RX[1].BatteryFsV << "V"; }
            std::cout << std::endl;
            std::cout << "\tTELEMETRY: " << ((m_sysTelemAct)? "ACT":"INH")
                << " DL " << std::setprecision(2) << m_telemDlInterval <<"s"<< std::endl;


            getFunction   (data, txType, modelType);
            getServoRevers(data, txType);
            getEndPoints  (data, txType);
            std::cout << "Reverse & End Point" << std::endl;
            const size_t numChannels = (txType == T18SZ)? t18Channels : t14Channels;
            for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
                std::cout << "\t" << std::setw(2) << chIdx + 1 << " " << std::setw(10) << std::left << fa[functn[chIdx]] << ": "
                    << ((reversed[chIdx])? "REVERSED" : "normal  ") << "  "
                    << std::right << std::setw(3) << (int)limitLo[chIdx] << "  " << std::setw(3) << (int)travelLo[chIdx] << "   "
                    << std::setw(3) << (int)travelHi[chIdx] << "  " << std::setw(3) << (int)limitHi[chIdx] << std::endl;
            }
            for (size_t chIdx = 0; chIdx < 2; ++chIdx) {
                std::cout << "\t" << std::setw(2) << chIdx + 13 << " DG"<< chIdx <<"       : "<< ((reversedDG[chIdx])? "REVERSED" : "normal")<< std::endl;
            }
            
            getServoSpeed(data, txType);
            getSubTrim   (data, txType);
            std::cout << "Servo Speed & SubTrim" << std::endl;
            for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
                std::cout << "\t" << std::setw(2) << chIdx + 1 <<" "<< std::setw(10) << std::left << fa[functn[chIdx]] << ": "
                    << std::right << std::setw(3) << (int)cServoSpeed(sSpeed[chIdx]) <<"  "<< std::setw(3) << (int)sTrim[chIdx] << std::endl;
            }

            getFailSafe(data, txType);
            auto cFailSafe  = [](size_t chIdx) { return ((m_FSMode    & (1 << chIdx)) == 0)? "HOLD" : "F/S"; };
            auto cBatteryFS = [](size_t chIdx) { return ((m_FSBattery & (1 << chIdx)) == 0)? "OFF"  : "ON"; };
            std::cout << "Fail Safe" << std::endl;
            std::cout << "\t\t\tF/S\tB.F/S\tPOS" << std::endl;
            for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
                std::cout << "\t" << std::setw(2) << chIdx + 1 << " " << std::setw(10) << std::left << fa[functn[chIdx]] << ": "
                    << '\t' << cFailSafe(chIdx) << "\t" << cBatteryFS(chIdx);
                if ((m_FSMode & (1 << chIdx)) != 0 || (m_FSBattery & (1 << chIdx)) != 0) {
                    std::cout << '\t' << std::right << std::setw(4) << fsPosition[chIdx] << "%";
                }
                std::cout << std::endl;
            }
            std::cout << "\tRelease battery F/S: " << m_releaseBfsHW << std::endl;

            getConditions(data, txType, modelType);
            getControlAssignment(data, txType, modelType);
            conditionSelect(data, txType);
            if (txType == T18SZ || modelType == Heli || modelType == Glider) {
                std::cout << "Condition #" << std::endl;
                for (size_t condIdx = 0; condIdx < numConditions; ++condIdx) {
                    std::wcout << L"\t" << condIdx + 1 << L": " << conditionName[condIdx];
                    std::cout  << "  " << m_conditionalData[condIdx].conditionControl << std::endl;
                }
            }
            
            std::cout << "Function" << std::endl;
            for (size_t condIdx = 0; condIdx < numConditions; ++condIdx)
            {
                if (numConditions > 1) {
                    std::wcout << L"    Condition #"<< condIdx+1 <<L": "<< conditionName[condIdx] << std::endl;
                }
                
                const auto& cd = m_conditionalData[condIdx];
                
                const size_t INVALID_INDEX = std::numeric_limits<size_t>::max();
                size_t sameAsCondition = INVALID_INDEX;
                if (condIdx > 0) {
                    for (size_t prevCondIdx = 0; prevCondIdx < condIdx; ++prevCondIdx) {
                        if (m_conditionalData[prevCondIdx] == cd) {
                            sameAsCondition = prevCondIdx;
                            break;
                        }
                    }
                }
                if (sameAsCondition != INVALID_INDEX) {
                    std::wcout << L"\tSame as condition #"<< sameAsCondition+1 <<L" "<< conditionName[sameAsCondition]  << std::endl;
                } else {
                    std::cout << "\t # Function  Ctrl Trim Rate  Mode" << std::endl;
                    for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
                        std::cout << "\t" << std::setw(2) << chIdx + 1 << " " << std::setw(10) << std::left << fa[functn[chIdx]] << ": "
                            << std::right << hwCtrlDesc[cd.control[chIdx]] << "  " << hwCtrlDesc[trim[chIdx]] 
                            << "  "<< std::showpos << trimRate[chIdx] <<"%  "
                            << std::array<std::string, 5>{{"?", "Normal", "ATL Revers", "ATL Norm", "Center"}}[static_cast<std::underlying_type<eTrimMode>::type>(trimMode[chIdx]) % 5] 
                            << std::endl;
                    }
                    for (size_t chIdx = 0; chIdx < 2; ++chIdx) {
                        std::cout << "\t" << std::setw(2) << chIdx+13 << " DG"<< chIdx <<"       :"<< digiCtrl[chIdx] << std::endl;
                    }
                }
            }

        } else {
            std::cout << "Failed to load \""<< fname <<"\"" << std::endl;
        }
        std::cout << "-------------------" << std::endl;
    }
    return 0;
}


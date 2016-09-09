#include <array>
#include <cstdint>
#include <cassert>
#include <iostream>
#include <fstream>  // std::ifstream
#include <string>
#include <vector>

using namespace std::string_literals;

const size_t arrSizeMax = 32767;
const size_t chMax = 16;
const size_t t14Channels = 12, t18Channels = 16;
const size_t t14Conditions = 5, t18Conditions = 8;

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
    if (!fileName.empty())
    {
        std::ifstream inStream(fileName, std::ifstream::in | std::ifstream::binary);
        if ((ok = (inStream.good() && inStream.is_open())) == true)
        {
            inStream.seekg(0, inStream.end); // get length of file
            const size_t length = static_cast<size_t>(inStream.tellg());
            assert(length <= arrSizeMax);
            inStream.seekg(0, inStream.beg);

            data.clear();
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
            if ((i = 7) And (modelType <> Heli)) { auxFunctionAbbr(i) = "MOT" Else auxFunctionAbbr(i) = "AUX" & CStr(i + 1)
        End if ( // auxFunctionAbbr(i) = ""
    }
*/
}

void getConditions(const std::vector<uint8_t>& data, eTxType txType, eModelType modelType)
{
    const size_t addr14CondSelect = 451, addr18CondSelect = 64;
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
 
    if (txType == T18SZ || modelType == Heli || modelType == Glider) 
    {
        std::array<int, t18Conditions> cp; 
        cp.fill(0);

        const size_t addr = (txType == T18SZ)? addr18CondSelect : addr14CondSelect;
        for (size_t i = 1; i < numConditions; ++i) {
            const uint8_t v = data.at(addr + (i - 1) * 4);
            const uint8_t m = v && 0x0F;
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
            if (i == 0) {
                break;
            }
        }

    }
}

int main()
{
    std::vector<uint8_t> data;
    for (const char* fname : { "data\\KatanaMX",  "data\\3DHKatana",
                               "data\\ShurikBipe","data\\FASSTest-2" })
    {
        LoadFromFile(fname, data);

        const eTxType      txType = getTransType(data);
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

        getFunction(data, txType, modelType);
        std::cout << "Channel #"<< std::endl;
        const size_t numChannels = (txType == T18SZ)? t18Channels : t14Channels;
        for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
            std::cout << "\t" << chIdx + 1 << ": "<< fa[functn[chIdx]] << std::endl;
        }

        getConditions(data, txType, modelType);
        if (txType == T18SZ || modelType == Heli || modelType == Glider) {
            std::cout << "Condition #" << std::endl;
            const size_t numConditions = (txType == T18SZ)? t18Conditions : t14Conditions;
            for (size_t condIdx = 0; condIdx < numConditions; ++condIdx) {
                std::wcout << L"\t" << condIdx + 1 << L": " << conditionName[condIdx]
                    << L", state: "<< conditionState[condIdx]
                    << L", list: " << conditionList [condIdx]
                    << std::endl;
                ;
            }
        }

        std::cout << "-------------------" << std::endl;
    }
    return 0;
}


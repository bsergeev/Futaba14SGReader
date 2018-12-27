// Builds warning free with:
//  g++ -std=c++17 -Wall -Wextra -Wshadow -Wnon-virtual-dtor -Wold-style-cast -Wcast-align -Wunused -Woverloaded-virtual -Wpedantic -Wsign-conversion -Wmisleading-indentation -Wduplicated-cond -Wduplicated-branches -Wlogical-op -Wnull-dereference -Wuseless-cast -Wdouble-promotion -Wformat=2
#include <array>
#include <cmath>
#include <cstdint>
#include <cassert>
#include <iomanip>
#include <iostream>
#include <filesystem>
#include <fstream>
#include <limits>
#include <string>
#include <string_view>
#include <vector>

using namespace std::string_literals;

namespace {

template <typename ENUM>
constexpr auto to_ut(ENUM e) -> typename std::underlying_type<ENUM>::type {
   return static_cast<typename std::underlying_type<ENUM>::type>(e);
} 

} // anonymous namespace
//------------------------------------------------------------------------------
class Model {
public: // <<< DEBUG
  static const size_t t14Channels = 12, t18Channels = 16;
  static const size_t MAX_CH = std::max(t14Channels, t18Channels);

  static const size_t t14ChannelsLow = 8, t18ChannelsLow = 12;

  static const size_t t14Conditions = 5, t18Conditions = 8;
  static const size_t maxConds = std::max(t14Conditions, t18Conditions);

  static const size_t NUMBER_OF_FUNCTIONS = 33;

  enum eTxType {
    INVALID_TX = 255, // <<< DEBUG
    T8FG = 0,
    T14SG = 1,
    T18SZ = 2
  };
  enum class eAreaType : uint8_t {
    UNKNOWN,
    General,
    France
  };
  enum eModelType {
    INVALID_MODEL = -1,
    Plane = 0,
    Heli = 1,
    Glider = 2,
    Multi = 3
  };
  enum class eTrimMode : uint8_t {
    INVALID = 0,
    Normal, ATLRev, ATLNorm, Center
  };

  typedef size_t hwControlIdx_t;
  static const std::array<const char*, 32> hwCtrlDesc;
  static const hwControlIdx_t NO_CONTROL_IDX = hwCtrlDesc.size() - 1; // "--" is last

  uint8_t     m_wingType = 0;
  uint8_t     m_tailType = 0;
  uint16_t    m_FSMode = 0;
  uint16_t    m_FSBattery = 0;
  std::string m_releaseBfsHW;
  bool        m_sysTelemAct = false;
  bool        m_singleRX = true;
  eAreaType   m_Area = eAreaType::UNKNOWN;
  double      m_telemDlInterval = 0.0;

  std::array<bool, MAX_CH>    m_reversed;
  std::array<bool, 2>         m_reversedDG;
  std::array<uint8_t, MAX_CH> m_travelLo, m_travelHi, m_limitLo, m_limitHi;
  std::array<uint8_t, MAX_CH> m_sSpeed; // [0, 27]
  std::array<int16_t, MAX_CH> m_sTrim;  // [-240, 240]
  std::array<int16_t, MAX_CH> m_fsPosition;
  std::array<hwControlIdx_t, MAX_CH> m_trim;
  std::array<int16_t, MAX_CH>   m_trimRate;
  std::array<eTrimMode, MAX_CH> m_trimMode;

  struct RxInfo {
    uint32_t ID = 0; // invalid
    double   BatteryFsV = 0.0;
  };
  std::array<RxInfo, 2> m_RX;

  size_t numConditions = 1; // 1 for condition-less models, or set in getConditions()
  struct ConditionDependentParams {
    ConditionDependentParams() { control.fill(NO_CONTROL_IDX); }
    bool operator ==(const ConditionDependentParams& o) const {
      for (size_t i = 0; i < MAX_CH; ++i) {
        if (!(control[i] == o.control[i])) {
          return false;
        }
      }
      return true;
    }
    std::array<hwControlIdx_t, MAX_CH> control;
    std::string conditionControl;
  };
  std::vector<ConditionDependentParams> m_conditionalData; // .size() == numConditions

  std::array<uint8_t, MAX_CH> m_functn; // value is the index of std::array<std::string, NUMBER_OF_FUNCTIONS>, i.e. < 33

  std::array<std::string, NUMBER_OF_FUNCTIONS> m_fa;
  static const std::array<std::string, NUMBER_OF_FUNCTIONS> FUNCTIONS_AIR;
  static const std::array<std::string, NUMBER_OF_FUNCTIONS> FUNCTIONS_HELI;
  static const std::array<std::string, NUMBER_OF_FUNCTIONS> FUNCTIONS_MULTI;
 
  static const std::array<uint8_t, 16> TELEMETRY_TYPE;
  static const std::array<double,  16> TFHSS_VOLT_LIST;

  // VB:
  // & -> Long
  // % -> Integer
  // # -> Double
  // ! -> Single
  // @ -> Decimal
  // $ -> String

  std::array<std::wstring, t18Conditions> m_conditionName;
  std::array<size_t, t18Conditions> m_conditionState, m_conditionList;
  std::array<size_t, t18Conditions> m_conditionHw;
  std::array<std::string, 2> m_digiCtrl;

  struct HwNamnes {
    int8_t Type = -1;
    std::string Ctrl, Pos, Rev, Sym;
  };

  std::vector<uint8_t> m_data;
  eTxType    m_txType    = eTxType::INVALID_TX;
  eModelType m_modelType = eModelType::INVALID_MODEL;

public: // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Model(const std::filesystem::path& filePath) {
    if (std::ifstream ifs{ filePath, std::ios::binary }; ifs.good()) {
      const auto file_size = ifs.seekg(0, std::ios_base::end).tellg();
      ifs.seekg(0);
      std::vector<char> data_;
      data_.resize(static_cast<std::size_t>(file_size));
      ifs.read(data_.data(), file_size);
      m_data = std::vector<uint8_t>{ begin(data_), end(data_) };

      // Get TX type
      const char* p = reinterpret_cast<const char*>(&m_data.front());
      constexpr size_t TX_ID_LENGTH = 8;
      if (std::string_view{ p, TX_ID_LENGTH } == "T18SZ   "s) {
        m_txType = T18SZ;
      } else {
        std::array<char, TX_ID_LENGTH>  buffer;
        for (size_t i = 0, j = 2; i < TX_ID_LENGTH; ++i, j += 2) {
          buffer[i] = p[j];
        }
        std::string_view txTypeName{ buffer.data(), TX_ID_LENGTH };
        if (txTypeName == "T8FG    "s) {
          m_txType = T8FG;
        } else if (txTypeName == "T14SG   "s) {
          m_txType = T14SG;
        } else {
          assert(!"Unknown TX type");
        }
      }

      // Get model type
      const size_t i = (m_txType == T18SZ) ? 93 : 152;
      const uint8_t v = m_data.at(i + 1) / 16;
      m_modelType = (v > Multi) ? INVALID_MODEL : static_cast<eModelType>(v);
      m_wingType = m_data.at(i) & 0x0F;
      m_tailType = (m_data.at(i) & 0x30) >> 4;
    }
  }

  [[nodiscard]] bool empty() const noexcept {
    return m_data.empty();
  }

  std::wstring getModelName() const {
    size_t startPos = 0, len = 0;
    if (m_txType == T18SZ) {
      startPos = 10; len = 15;
    }
    else {
      startPos = 17; len = 10;
    }

    std::array<wchar_t, 15> buffer; // longer len
    size_t i = 0;
    for (size_t j = startPos; i < len; j += 2) {
      buffer[i] = static_cast<wchar_t>((m_data.at(j) << 8) + m_data.at(j + 1));
      if (buffer[i++] == 0) {
        break;
      }
    }
    return std::wstring{ buffer.data(), i };
  }

  size_t getModulation() const
  {
    size_t sysModulation = 0;

    const size_t am = (m_txType == T18SZ) ? 92 : 154;
    if (m_txType == T8FG) {
      auto v = ((m_data.at(am) & 0x30) + (m_data.at(am + 1) & 0x80)) >> 4;
      switch (v) {
      case 1: sysModulation = 1; break;
      case 3: sysModulation = 0; break;
      case 9: sysModulation = 2; break;
      }
    }
    else {
      const size_t dm = (m_txType == T18SZ) ? 16 : 1;
      sysModulation = (m_data.at(am) / dm) & 0x0F;
    }

    return sysModulation;
  }

  void getFunction()
  {
    const size_t addr14Func = 178, addr18Func = 102;

    size_t numChannels, addr;
    if (m_txType == T18SZ) {
      numChannels = t18Channels; addr = addr18Func;
    }
    else {
      numChannels = t14Channels; addr = addr14Func;
    }
    for (size_t i = 0; i < numChannels; ++i) {
      m_functn[i] = m_data.at(addr + i);
    }
    switch (m_modelType) {
    case Plane:
    case Glider: m_fa = FUNCTIONS_AIR;   break;
    case Heli:   m_fa = FUNCTIONS_HELI;  break;
    case Multi:  m_fa = FUNCTIONS_MULTI; break;
    case INVALID_MODEL: assert(!"Invalid model type"); break;
    }
    if (m_txType != T18SZ && m_modelType == Plane) {
      m_fa[25] = "VPP"s;
      return;
    }
  }

  void getConditions()
  {
    const size_t addr14CondSelect = /*464*/451, addr18CondSelect = 64;
    const size_t addr14CondName = 1700, addr18CondName = 28140;
    const size_t addr14CondNameOffset = 9, addr18CondNameOffset = 578;

    for (size_t i = 0; i < t18Conditions; ++i) {
      m_conditionName[i] = L""; m_conditionState[i] = 0; m_conditionHw[i] = static_cast<size_t>(-1) /*hwOff*/; m_conditionList[i] = 0;
    }
    numConditions = 1;
    m_conditionState[0] = 128 + 15;
    m_conditionList[0] = 0;

    // Get names of the conditions
    const size_t numTxConditions = (m_txType == T18SZ) ? t18Conditions : t14Conditions;
    if (m_txType == T8FG) {
      switch (m_modelType) {
      case Heli:   m_conditionName[0] = L"NORMAL";   m_conditionName[1] = L"IDLEUP1"; m_conditionName[2] = L"IDLEUP2";
        m_conditionName[3] = L"IDLEUP3";  m_conditionName[4] = L"HOLD";
        numConditions = 5;
        break;
      case Glider: m_conditionName[0] = L"NORMAL";   m_conditionName[1] = L"START"; m_conditionName[2] = L"SPEED";
        m_conditionName[3] = L"DISTANCE"; m_conditionName[4] = L"LANDING";
        numConditions = 5;
        break;
      case INVALID_MODEL: assert(!"Invalid model type"); break;
      default: break; // do nothing
      }
    }
    else // T18SZ or T14SZ
    {
      if (m_txType == T18SZ || m_modelType == Heli || m_modelType == Glider)
      {
        numConditions = numTxConditions;
        for (size_t condIdx = 0; condIdx < numTxConditions; ++condIdx)
        {
          std::array<wchar_t, 8 + 1> buffer;
          buffer.fill(0);
          for (size_t charIdx = 0; charIdx < 8; ++charIdx)
          {
            if (m_txType == T18SZ) { // UTF16
              const char hi = static_cast<const char>(m_data.at(addr18CondName + condIdx * addr18CondNameOffset + charIdx * 2));
              const char lo = static_cast<const char>(m_data.at(addr18CondName + condIdx * addr18CondNameOffset + charIdx * 2 + 1));
              buffer[charIdx] = static_cast<wchar_t>((hi << 8) + lo);
            }
            else { // T14SZ, 1-byte
              buffer[charIdx] = m_data.at(addr14CondName + condIdx * addr14CondNameOffset + charIdx);
            }

            if (buffer[charIdx] == 0) {
              break;
            }
          }
          m_conditionName[condIdx] = std::wstring{ buffer.data() };
        }
      }
    }

    // <<< DEBUG : The following code doesn't make sense (although seems to work)...
    if (m_txType == T18SZ || m_modelType == Heli || m_modelType == Glider)
    {
      std::array<size_t, t18Conditions> cp;
      cp.fill(0);

      const size_t addr = (m_txType == T18SZ) ? addr18CondSelect : addr14CondSelect;
      for (size_t i = 1; i < numTxConditions; ++i) {
        const uint8_t v = m_data.at(addr + (i - 1) * 4);
        const uint8_t m = v & 0x0F;
        if (v > 127) {
          m_conditionState[m] = 128 + i;
          m_conditionHw[m] = addr + (i - 1) * 4 + 1;
        }
      }
      m_conditionState[0] = 128 + 15;
      for (size_t i = 1; i < t18Conditions; ++i) {
        if (m_conditionState[i] > 128) {
          cp[m_conditionState[i] - 128 - 1] = i;
        }
      }

      for (size_t j = 1, i = t18Conditions - 1; /* i >= 0 */; --i) {
        if (cp[i] > 0) {
          m_conditionList[j] = cp[i];
          j = j + 1;
        }
        if (i == 0) break;
      }
    }
  }

  HwNamnes getHardware(size_t a) const 
  {
    HwNamnes hw;
    const uint8_t hC0 = m_data.at(a);
    const uint8_t i1 = m_data.at(a + 1);
    const uint8_t i2 = m_data.at(a + 2);
    if (hC0 == 0xFF) {
      hw.Type = -1; hw.Ctrl = "--";
      hw.Pos = (i1 != 0 || i2 != 0) ? "OFF" : "ON";
      return hw;
    }
    const uint8_t hR = hC0 & 0x40;
    const uint8_t hC = hC0 & 0x3F;
    if (hC >= 32) { hw.Ctrl = "Logic"; return hw; }
    hw.Ctrl = hwCtrlDesc[hC];
    if ((hC & 0x34) == 4) {
      if ((hC & 0x37) == 7) {
        hw.Type = 2; // 2-position switch
        hw.Pos = (hR == 0) ? "OFF/ON" : "ON/OFF";
        return hw;
      }
      hw.Type = 3; // 3 - position switch
      if (hR != 0) { hw.Pos = "ON/OFF/ON";  return hw; }
      if ((i1 & 0x80) == 0 && i2 >= 0x40) { hw.Pos = "OFF/OFF/ON"; return hw; }
      if ((i1 & 0x80) != 0 && i2 >= 0x40) { hw.Pos = "ON/OFF/OFF"; return hw; } // was "OFF/ON/ON"
      if ((i1 <= 0xC0) && (i2 & 0x80) == 0) { hw.Pos = "ON/ON/OFF";  return hw; }
      if ((i1 <= 0xC0) && (i2 & 0x80) != 0) { hw.Pos = "OFF/ON/ON";  return hw; } // was "ON/OFF/OFF"
      else { hw.Pos = "OFF/ON/OFF"; return hw; }
    }
    hw.Type = 0; // Analog input
    hw.Rev = (hR == 0) ? "Normal" : "Reverse";
    if (static_cast<uint16_t>(i1) + i2 == 0x0100) { hw.Sym = "Symmetry"; hw.Pos = std::to_string(round(static_cast<int8_t>(i2)*100.0 / 64)); return hw; }
    if (i1 - i2 == 1) { hw.Sym = "Linear";   hw.Pos = std::to_string(round(static_cast<int8_t>(i1)*100.0 / 64)); return hw; }
    hw.Pos = "Error!!!"; hw.Rev = ""; hw.Sym = "";
    return hw;
  }


  void conditionSelect()
  {
    auto logicSwitch = [this](size_t a) -> std::string {
      const size_t addr14logSw = 328, addr18logSw = 456;
      const size_t aa = (m_txType == T18SZ) ? addr18logSw : addr14logSw;
      if ((m_data.at(a) & 48) == 48)
      {
        auto hw = getHardware(aa + (m_data.at(a) & 0x07U) * 6U);
        std::string alt = hw.Ctrl + " " + hw.Pos + " " + hw.Rev + " " + hw.Sym;
        if (m_data.at(a + 1) & 128) {
          alt = alt + " Alternate";
        }
        std::string l;
        switch (m_data.at(a + 1) % 4) {
        case 0: l = "AND";     break;
        case 1: l = "OR";      break;
        case 2: l = "EX-OR";   break;
        case 3: l = "!UNDEF!"; break;
        }
        alt = alt + "  " + l + "  ";
        hw = getHardware(aa + (m_data.at(a) & 0x7U) * 6U + 3U);
        alt = alt + hw.Ctrl + " " + hw.Pos + " " + hw.Rev + " " + hw.Sym;
        if (m_data.at(a + 1) & 0x40) {
          alt = alt + " Alternate";
        }
        return alt;
      }
      else {
        auto hw = getHardware(a);
        return hw.Ctrl + " " + hw.Pos + " " + hw.Rev + " " + hw.Sym;
      }
    };

    for (size_t i = 1; i < numConditions; ++i) {
      if (m_conditionList[i] != 0) {
        m_conditionalData[i].conditionControl = logicSwitch(m_conditionHw[m_conditionList[i]]);
      }
    }
  }

  void getServoRevers()
  {
    m_reversed.fill(false);
    m_reversedDG.fill(false);

    const size_t addr14RevLo = 268, addr14RevHi = 165, addr14RevDg = 154;
    const size_t addr18RevLo = 252, addr18RevHi = 253, addr18RevDg = 518;

    const bool isT18SZ = (m_txType == T18SZ);
    const uint8_t  revLo = m_data.at((isT18SZ) ? addr18RevLo : addr14RevLo);
    const uint16_t revHi = m_data.at((isT18SZ) ? addr18RevHi : addr14RevHi);
    const uint8_t  revDg = m_data.at((isT18SZ) ? addr18RevDg : addr14RevDg) & 0xC0;
    const uint16_t rev = (revHi << 8) | revLo;

    const size_t numChannels = (isT18SZ) ? t18Channels : t14Channels;
    for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
      m_reversed[chIdx] = (rev & (0x0001 << chIdx)) != 0;
    }
    m_reversedDG[0] = (revDg & 0x40) != 0;
    m_reversedDG[1] = (revDg & 0x80) != 0;
  }

  void getEndPoints()
  {
    m_travelLo.fill(0); m_travelHi.fill(0);
    m_limitLo.fill(0); m_limitHi.fill(0);

    const size_t addr14TrlLo = 290, addr14TrlHi = 706, addr18TrlLo = 254, addr18TrlHi = 562;
    const size_t addr14LimLo = 664, addr14LimHi = 714, addr18LimLo = 278, addr18LimHi = 570;

    size_t atl, ath, ln, all, alh;
    const bool isT18SZ = (m_txType == T18SZ);
    if (isT18SZ) {
      atl = addr18TrlLo; ath = addr18TrlHi; ln = t18ChannelsLow;
      all = addr18LimLo; alh = addr18LimHi;
    }
    else {
      atl = addr14TrlLo; ath = addr14TrlHi; ln = t14ChannelsLow;
      all = addr14LimLo; alh = addr14LimHi;
    }
    const size_t numChannels = (isT18SZ) ? t18Channels : t14Channels;
    for (size_t i = 0; i < numChannels; ++i) {
      size_t j = (i < ln) ? atl + i * 2 : ath + (i - ln) * 2;
      m_travelLo[i] = m_data.at(j); m_travelHi[i] = m_data.at(j + 1);
      j = (i < ln) ? all + i * 2 : alh + (i - ln) * 2;
      m_limitLo[i] = m_data.at(j); m_limitHi[i] = m_data.at(j + 1);
    }
  }

  static uint8_t cServoSpeed(uint8_t y) {
    if (y < 67) { return static_cast<uint8_t>(round(y / 10)); }
    if (y < 78) { return static_cast<uint8_t>(round((y - 67) / 4) + 7); }
    if (y == 78) { return 10; } // y - 68;
    if (y < 88) { return static_cast<uint8_t>(round((y - 80) / 3) + 11); }
    return  static_cast<uint8_t>((y - 74));
  }

  void getServoSpeed()
  {
    m_sSpeed.fill(0);

    const size_t addr14sSpLo = 1812, addr14sSpHi = 1828, addr18sSpLo = 438, addr18sSpHi = 594;
    const bool isT18SZ = (m_txType == T18SZ);
    size_t al, ah, ln, k;
    if (isT18SZ) {
      al = addr18sSpLo; ah = addr18sSpHi; ln = t18ChannelsLow; k = 1;
    }
    else {
      al = addr14sSpLo; ah = addr14sSpHi; ln = t14ChannelsLow; k = 2;
    }
    const size_t numChannels = (isT18SZ) ? t18Channels : t14Channels;
    for (size_t i = 0; i < numChannels; ++i) {
      const size_t j = (i < ln) ? al + i * k : ah + (i - ln)*k;
      m_sSpeed[i] = m_data.at(j);
    }
  }

  void getSubTrim()
  {
    m_sTrim.fill(0);

    const size_t  addr14sTrLo = 306, addr14sTrHi = 166, addr18sTrLo = 414, addr18sTrHi = 586;
    const bool isT18SZ = (m_txType == T18SZ);
    size_t al, ah, ln;
    if (isT18SZ) {
      al = addr18sTrLo; ah = addr18sTrHi; ln = t18ChannelsLow;
    }
    else {
      al = addr14sTrLo; ah = addr14sTrHi; ln = t14ChannelsLow;
    }
    const size_t numChannels = (isT18SZ) ? t18Channels : t14Channels;
    for (size_t i = 0; i < numChannels; ++i) {
      const size_t j = (i < ln) ? al + i * 2 : ah + (i - ln) * 2;
      const uint8_t hi = m_data.at(j);
      const uint8_t lo = m_data.at(j + 1);
      m_sTrim[i] = (hi << 8) | lo;
    }
  }

  void getControlAssignment()
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

    const bool isT18SZ = (m_txType == T18SZ);

    // Control assignments for each condition
    for (size_t condIdx = 0; condIdx < numConditions; ++condIdx)
    {
      auto& cd = m_conditionalData.at(condIdx);
      if (isT18SZ)
      {
        const size_t  ac = addr18CondStart, lc = t18CondLength, axc = addr18fnXC;
        for (size_t i = 0; i < t18Channels; ++i) {
          size_t a1 = axc + m_functn[i];
          size_t a2 = ac + lc * (m_conditionList[condIdx]) + m_data.at(a1);
          cd.control[i] = std::min<hwControlIdx_t>(NO_CONTROL_IDX, m_data.at(a2));
          a1 = axc + m_functn[i] + NUMBER_OF_FUNCTIONS - 1;
          a2 = ac + lc * (m_conditionList[condIdx]) + MAX_CH + m_data.at(a1);
          m_trim[i] = std::min<hwControlIdx_t>(NO_CONTROL_IDX, m_data.at(a2)); // <<< DEBUG move out of condIdx loop!
        }
      }
      else {
        static const std::array<size_t, 3> ag = { addr14fnGrBfly, addr14fnGrCamb, addr14fnGrMot };
        for (size_t i = 0; i < t14Channels; ++i) {
          size_t a2 = addr14fnCtrl + m_data.at(addr14fnXC + m_functn[i]);
          if (m_functn[i] >= 22 && m_functn[i] <= 24 && m_modelType == Glider) {
            const size_t a1 = ag[m_functn[i] - 22U];
            if (m_data.at(a1) > 127) {
              a2 = a1 + m_conditionList[condIdx] + 1;
            }
          }
          cd.control[i] = std::min<hwControlIdx_t>(NO_CONTROL_IDX, m_data.at(a2));
          a2 = addr14fnTrim + m_data.at(addr14fnXC + m_functn[i]);
          m_trim[i] = std::min<hwControlIdx_t>(NO_CONTROL_IDX, m_data.at(a2)); // <<< DEBUG move out of condIdx loop!
        }
      }
    }

    // Controls for DGs (same for all conditions)
    const std::string ls = " ";
    for (size_t ch = 0; ch < 2; ++ch)
    {
      if (isT18SZ)
      {
        if ((m_data.at(addr18dgCtrl + ch * 3) & 48) == 48)
        {
          auto hw = getHardware(addr18dgCtrl + ((m_data.at(addr18dgCtrl + ch * 3) & 0x07U) + 1U) * 6U);
          std::string alt = hw.Ctrl + " " + hw.Pos + " " + hw.Rev + " " + hw.Sym;
          if (m_data.at(addr18dgCtrl + ch * 3 + 1) & 128) {
            alt = alt + " Alternate";
          }
          std::string l;
          switch (m_data.at(addr18dgCtrl + ch * 3 + 1) % 4) {
          case 0: l = "AND";     break;
          case 1: l = "OR";      break;
          case 2: l = "EX-OR";   break;
          case 3: l = "!UNDEF!"; break;
          }
          alt = alt + "   " + l + "   ";
          hw = getHardware(addr18dgCtrl + ((m_data.at(addr18dgCtrl + ch * 3U) & 0x07U) + 1U) * 6U + 3U);
          alt = alt + hw.Ctrl + " " + hw.Pos + " " + hw.Rev + " " + hw.Sym;
          if (m_data.at(addr18dgCtrl + ch * 3 + 1) & 0x40) {
            alt = alt + " Alternate";
          }
          m_digiCtrl[ch] = alt;
        }
        else {
          auto hw = getHardware(addr18dgCtrl + ch * 3);
          m_digiCtrl[ch] = ls + hw.Ctrl + "  " + hw.Pos + "  " + hw.Rev + "  " + hw.Sym;
        }
      }
      else { // <<< DEBUG TBD: handle 'Logic' for 14SG
        const uint8_t m = static_cast<uint8_t>(1U << ch);
        std::string alt = (m_data.at(addr14dgAlt) & m) ? "Alternate" : "";
        auto hw = getHardware(addr14dgCtrl + ch * 3);
        m_digiCtrl[ch] = ls + hw.Ctrl + "  " + hw.Pos + "  " + hw.Rev + "  " + hw.Sym + "  " + alt;
      }
    }

    // Trim rates
    const size_t numChannels = (isT18SZ) ? t18Channels : t14Channels;
    for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
      size_t atr, ats, x;
      if (isT18SZ) {
        atr = addr18fnTRt; ats = addr18fnTSg; x = m_functn[chIdx];
      }
      else {
        atr = addr14fnTRt; ats = addr14fnTSg; x = m_data.at(addr14fnXC + m_functn[chIdx]);
      }
      const uint8_t m = static_cast<uint8_t>(1U << (x % 8));
      auto v = static_cast<int16_t>(m_data.at(atr + x));
      m_trimRate[chIdx] = ((m_data.at(ats + x / 8) & m) != 0) ? -v : v;
    }

    // Trim mode
    for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
      size_t atc, atm, atr, x;
      if (isT18SZ) {
        atc = addr18fnTCn; atm = addr18fnTMd; atr = addr18fnTMr; x = m_functn[chIdx];
      }
      else {
        atc = addr14fnTCn; atm = addr14fnTMd; atr = addr14fnTMr; x = m_data.at(addr14fnXC + m_functn[chIdx]);
      }
      const uint8_t m = static_cast<uint8_t>(1 << (x % 8U));
      if ((m_data.at(atc + x / 8) & m) == 0 && (m_data.at(atm + x / 8) & m) == 0) {
        m_trimMode[chIdx] = eTrimMode::Normal;
      }
      else {
        if (m_data.at(atm + x / 8) & m) {
          m_trimMode[chIdx] = (m_data.at(atr + x / 8) & m) ? eTrimMode::ATLRev : eTrimMode::ATLNorm;
        }
        else {
          m_trimMode[chIdx] = eTrimMode::Center;
        }
      }
    }
  }

  void getFailSafe()
  {
    const size_t addr14fsLo = 269, addr14fsHi = 697, addr18fsLo = 334, addr18fsHi = 335;
    const size_t addr14bfsLo = 286, addr14bfsHi = 164, addr18bfsLo = 360, addr18bfsHi = 361;
    const size_t addr14fsPosLo = 270, addr14fsPosHi = 698, addr18fsPosLo = 336, addr18fsPosHi = 578;
    const size_t addr14relBFS = 287, addr18relBFS = 362;

    const bool isT18SZ = (m_txType == T18SZ);
    m_FSMode = (m_data.at((isT18SZ) ? addr18fsHi : addr14fsHi) << 8) + m_data.at((isT18SZ) ? addr18fsLo : addr14fsLo);
    m_FSBattery = (m_data.at((isT18SZ) ? addr18bfsHi : addr14bfsHi) << 8) + m_data.at((isT18SZ) ? addr18bfsLo : addr14bfsLo);

    size_t al, ah, ln;
    if (isT18SZ) {
      al = addr18fsPosLo; ah = addr18fsPosHi; ln = t18ChannelsLow;
    }
    else {
      al = addr14fsPosLo; ah = addr14fsPosHi; ln = t14ChannelsLow;
    }
    const size_t numChannels = (m_txType == T18SZ) ? t18Channels : t14Channels;
    for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
      const size_t addr = (chIdx < ln) ? al + chIdx * 2
        : ah + (chIdx - ln) * 2;
      const int16_t st = (m_data.at(addr) << 8) + m_data.at(addr + 1);
      m_fsPosition[chIdx] = static_cast<int16_t>(round((st - 1024) / 6.73));
    }

    auto hw = getHardware((isT18SZ) ? addr18relBFS : addr14relBFS);
    m_releaseBfsHW = hw.Ctrl + "  " + hw.Pos + "  " + hw.Rev + "  " + hw.Sym;
  }

  void getSystemInfo(size_t sysModulation)
  {
    auto getRxID = [this](size_t a) -> uint32_t { return static_cast<uint32_t>((m_data.at(a) << 24) | (m_data.at(a + 1) << 16) | (m_data.at(a + 2) << 8) | m_data.at(a + 3)); };

    auto getBFsVoltage = [this](size_t sysModultn, size_t a) {
      const size_t tlmType = TELEMETRY_TYPE[sysModultn % TELEMETRY_TYPE.size()];
      const double v = (tlmType == 1) ? m_data.at(a) / 10.0
        : (tlmType == 2) ? TFHSS_VOLT_LIST[m_data.at(a) % TFHSS_VOLT_LIST.size()]
        : 0.0;
      return v;
    };

    constexpr size_t rxIDlen = 4;
    const size_t addr14IDRx1 = 2196, addr14IDRx2 = addr14IDRx1 + rxIDlen;
    const size_t addr18IDRx1 = 406, addr18IDRx2 = addr18IDRx1 + rxIDlen;
    const size_t addr14sysAr = 156, addr14rxQty = 2204, mask14rxQty = 1;
    const size_t addr18sysAr = 0, addr18rxQty = 516, mask18rxQty = 2;
    const size_t addr14tAct = 2206, mask14tAct = 128;
    const size_t addr18tAct = 92, mask18tAct = 2;
    const size_t addr14dlI = 2206, div14dlI = 1;
    const size_t addr18dlI = 516, div18dlI = 4;
    const size_t addr14bfsvRx1 = 2208, addr14bfsvRx2 = 2209;
    const size_t addr18bfsvRx1 = 401, addr18bfsvRx2 = 402;

    size_t aa, ar, ai1, ai2, ata, mta, adl, ddl, av1, av2, mr;
    if (m_txType == T18SZ) {
      aa = addr18sysAr; ar = addr18rxQty; ai1 = addr18IDRx1; ai2 = addr18IDRx2; ata = addr18tAct; mta = mask18tAct;
      adl = addr18dlI; ddl = div18dlI; av1 = addr18bfsvRx1; av2 = addr18bfsvRx2; mr = mask18rxQty;
    }
    else {
      aa = addr14sysAr; ar = addr14rxQty; ai1 = addr14IDRx1; ai2 = addr14IDRx2; ata = addr14tAct; mta = mask14tAct;
      adl = addr14dlI; ddl = div14dlI; av1 = addr14bfsvRx1; av2 = addr14bfsvRx2; mr = mask14rxQty;
    }
    m_sysTelemAct = (m_data.at(ata) & mta) != 0;
    if (m_txType != T18SZ) { // && (sysModulation & 0x03) != 0) { // i.e. "FASST MULTI" or "FASST MLT2" // <<< DEBUG correct?
      m_Area = ((m_data.at(aa) & 0x80) == 0) ? eAreaType::General : eAreaType::France;
    }
    if (TELEMETRY_TYPE[sysModulation] != 0) {
      m_singleRX = (m_data.at(ar) & mr) == 0 || sysModulation == 10; // "FASSTest 12CH" // <<< DEBUG correct?
      m_RX[0].ID = getRxID(ai1);
      m_RX[0].BatteryFsV = getBFsVoltage(sysModulation, av1);
      if (!m_singleRX) {
        m_RX[1].ID = getRxID(ai2);
        m_RX[1].BatteryFsV = getBFsVoltage(sysModulation, av2);
      }
    }
    if (TELEMETRY_TYPE[sysModulation] == 1) {
      m_telemDlInterval = ((m_data.at(adl) / ddl) & 0x1F) / 10.0;
    }
  }
}; // class Model

//static 
const std::array<const char*, 32> Model::hwCtrlDesc = {
    "J1", "J2", "J4", "J3", "SC", "SD", "SG", "SH", "RD", "RS",
    "OA", "0B", "SA", "SB", "SE", "SF", "LD", "11", "LS", "13",
    "T1", "T2", "T4", "T3", "T5", "T6", "T7", "1B", "1C", "1D", "1E", "--" };
const std::array<std::string, Model::NUMBER_OF_FUNCTIONS> Model::FUNCTIONS_AIR = {
    "Aileron"s, "Elevator"s, "Throttle"s, "Rudder"s, "Gear"s,
    "Flap"s, "Aileron2"s, "Aileron3"s, "Aileron4"s, "Elevator2"s,
    "Flap2"s, "Air brake"s, "Fuel mix"s, "Gyro"s, "Gyro2"s,
    "Gyro3"s, "Throttle2"s, "Throttle3"s, "Throttle4"s, "Flap3"s,
    "Flap4"s, "Rudder2"s, "Butterfly"s, "Camber"s, "Motor"s,
    "Auxiliary7"s, "Auxiliary6"s, "Auxiliary5"s, "Auxiliary4"s, "Auxiliary3"s,
    "Auxiliary2"s, "Auxiliary1"s, "--"s };
const std::array<std::string, Model::NUMBER_OF_FUNCTIONS> Model::FUNCTIONS_HELI = {
    "Aileron"s, "Elevator"s, "Throttle"s, "Rudder"s, "Gear"s,
    "Pitch"s, "Governor"s, "Governor2"s, "Aileron4"s, "Elevator2"s,
    "Flap2"s, "Needle"s, "Fuel mix"s, "Gyro"s, "Gyro2"s,
    "Gyro3"s, "Throttle2"s, "Throttle3"s, "Throttle4"s, "Flap3"s,
    "Flap4"s, "Rudder2"s, "Butterfly"s, "Camber"s, "Auxiliary8"s,
    "Auxiliary7"s, "Auxiliary6"s, "Auxiliary5"s, "Auxiliary4"s, "Auxiliary3"s,
    "Auxiliary2"s, "Auxiliary1"s, "--"s };
const std::array<std::string, Model::NUMBER_OF_FUNCTIONS> Model::FUNCTIONS_MULTI = {
    "Aileron"s, "Elevator"s, "Throttle"s, "Rudder"s, "Gear"s,
    "Flap"s, "Aileron2"s, "Aileron3"s, "Aileron4"s, "Elevator2"s,
    "Flap2"s, "Air brake"s, "Fuel mix"s, "Gyro"s, "Gyro2"s,
    "Gyro3"s, "Camera roll"s, "Camera tilt"s, "Camera pan"s, "Camera rec"s,
    "Mode"s, "Rudder2"s, "Butterfly"s, "Camber"s, "Motor"s,
    "Auxiliary7"s, "Auxiliary6"s, "Auxiliary5"s, "Auxiliary4"s, "Auxiliary3"s,
    "Auxiliary2"s, "Auxiliary1"s, "--"s };
const std::array<uint8_t, 16> Model::TELEMETRY_TYPE = { 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 2, 0, 0, 0 };
const std::array<double,  16> Model::TFHSS_VOLT_LIST = { 3.8, 0.0, 4.0, 4.2, 4.4, 4.6, 4.8, 5.0,
                                                       5.3, 5.6, 5.9, 6.2, 6.5, 6.8, 7.1, 7.4 };

//------------------------------------------------------------------------------
int main(int argc, char* argv[])
{
  std::vector<const char*> names{ { "data/KatanaMX" } };
  if (argc > 1) {
    names.clear();
    for (int i = 1; i < argc; ++i) {
      names.push_back(argv[i]);
    }
  }

  for (const char* fname : names)
  {
    Model m{ std::filesystem::path(fname) };
    if (!m.empty())
    {
      std::cout << "TX: " << ((m.m_txType == Model::eTxType::INVALID_TX) ? "INVALID"
        : std::array<const char*, 3>{"T8FG", "T14SG", "T18SZ"}[m.m_txType]) << std::endl;

      const std::wstring modelName = m.getModelName();
      std::wcout << L"Model name: \"" << modelName << L"\"" << std::endl;

      std::cout << "Model: " << ((m.m_modelType == Model::eModelType::INVALID_MODEL) ? "INVALID"
        : std::array<const char*, 4>{"Plane", "Heli", "Glider", "Multi"}[m.m_modelType]) << "\n\n";

      const size_t modulation = m.getModulation();
      m.getSystemInfo(modulation);
      const char* modulationList[16] = { "FASST 7CH",     "FASST MULTI", "FASST MLT2",    "--",
                                         "S-FHSS",        "--",          "--",            "--",
                                         "FASSTest 14CH", "--",          "FASSTest 12CH", "--",
                                         "T-FHSS",        "--",          "--",            "--" };
      std::cout << "SYSTEM" << std::endl;
      std::cout << "\t" << modulationList[modulation] << "  " << ((m.m_singleRX) ? "SINGLE" : "DUAL") << " "
        << ((m.m_Area == Model::eAreaType::UNKNOWN) ? "" : (m.m_Area == Model::eAreaType::General ? "G" : "F")) << std::endl;
      std::cout << "\t" << m.m_RX[0].ID;
      if (!m.m_singleRX) { std::cout << "\t\t" << m.m_RX[1].ID; }
      std::cout << std::endl;
      std::cout << "\t" << std::setprecision(2) << m.m_RX[0].BatteryFsV << "V";
      if (!m.m_singleRX) { std::cout << "\t\t" << std::setprecision(2) << m.m_RX[1].BatteryFsV << "V"; }
      std::cout << std::endl;
      std::cout << "\tTELEMETRY: " << ((m.m_sysTelemAct) ? "ACT" : "INH")
        << " DL " << std::setprecision(2) << m.m_telemDlInterval << "s" << std::endl;


      m.getFunction();
      m.getServoRevers();
      m.getEndPoints();
      std::cout << "Reverse & End Point" << std::endl;
      const size_t numChannels = (m.m_txType == Model::T18SZ) ? Model::t18Channels : Model::t14Channels;
      for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
        std::cout << "\t" << std::setw(2) << chIdx + 1 << " " << std::setw(10) << std::left << m.m_fa[m.m_functn[chIdx]] << ": "
          << ((m.m_reversed[chIdx]) ? "REVERSED" : "normal  ") << "  "
          << std::right << std::setw(3) << static_cast<int>(m.m_limitLo[chIdx]) << "  "
          << std::setw(3) << static_cast<int>(m.m_travelLo[chIdx]) << "   "
          << std::setw(3) << static_cast<int>(m.m_travelHi[chIdx]) << "  "
          << std::setw(3) << static_cast<int>(m.m_limitHi[chIdx]) << std::endl;
      }
      for (size_t chIdx = 0; chIdx < 2; ++chIdx) {
        std::cout << "\t" << std::setw(2) << chIdx + 13 << " DG" << chIdx << "       : " 
          << ((m.m_reversedDG[chIdx]) ? "REVERSED" : "normal") << std::endl;
      }

      m.getServoSpeed();
      m.getSubTrim();
      std::cout << "Servo Speed & SubTrim" << std::endl;
      for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
        std::cout << "\t" << std::setw(2) << chIdx + 1 << " " << std::setw(10) << std::left << m.m_fa[m.m_functn[chIdx]] << ": "
          << std::right << std::setw(3) << static_cast<int>(m.cServoSpeed(m.m_sSpeed[chIdx])) << "  " << std::setw(3)
          << static_cast<int>(m.m_sTrim[chIdx]) << std::endl;
      }

      m.getFailSafe();
      auto cFailSafe = [m](size_t chIdx) { return ((m.m_FSMode    & (1 << chIdx)) == 0) ? "HOLD" : "F/S"; };
      auto cBatteryFS = [m](size_t chIdx) { return ((m.m_FSBattery & (1 << chIdx)) == 0) ? " OFF" : "ON"; };
      std::cout << "Fail Safe" << std::endl;
      std::cout << "\t\t\tF/S\tB.F/S\tPOS" << std::endl;
      for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
        std::cout << "\t" << std::setw(2) << chIdx + 1 << " " << std::setw(10) << std::left << m.m_fa[m.m_functn[chIdx]] << ": "
          << '\t' << cFailSafe(chIdx) << "\t" << cBatteryFS(chIdx);
        if ((m.m_FSMode & (1 << chIdx)) != 0 || (m.m_FSBattery & (1 << chIdx)) != 0) {
          std::cout << '\t' << std::right << std::setw(4) << m.m_fsPosition[chIdx] << "%";
        }
        std::cout << std::right << std::endl;
      }
      std::cout << "\tRelease battery F/S: " << m.m_releaseBfsHW << std::endl;

      m.getConditions();
      m.getControlAssignment();
      m.conditionSelect();
      if (m.m_txType == Model::T18SZ || m.m_modelType == Model::Heli || m.m_modelType == Model::Glider) {
        std::cout << "Condition #" << std::endl;
        for (size_t condIdx = 0; condIdx < m.numConditions; ++condIdx) {
          std::wcout << L"\t" << condIdx + 1 << L": " << m.m_conditionName[condIdx];
          std::cout << "  " << m.m_conditionalData[condIdx].conditionControl << std::endl;
        }
      }

      std::cout << "Function" << std::endl;
      for (size_t condIdx = 0; condIdx < m.numConditions; ++condIdx)
      {
        if (m.numConditions > 1) {
          std::wcout << L"    Condition #" << condIdx + 1 << L": " << m.m_conditionName[condIdx] << std::endl;
        }

        const auto& cd = m.m_conditionalData[condIdx];

        const size_t INVALID_INDEX = std::numeric_limits<size_t>::max();
        size_t sameAsCondition = INVALID_INDEX;
        if (condIdx > 0) {
          for (size_t prevCondIdx = 0; prevCondIdx < condIdx; ++prevCondIdx) {
            if (m.m_conditionalData[prevCondIdx] == cd) {
              sameAsCondition = prevCondIdx;
              break;
            }
          }
        }
        if (sameAsCondition != INVALID_INDEX) {
          std::wcout << L"\tSame as condition #" << sameAsCondition + 1 << L" " << m.m_conditionName[sameAsCondition] << std::endl;
        }
        else {
          std::cout << "\t # Function  Ctrl Trim Rate  Mode" << std::endl;
          for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
            std::cout << "\t" << std::setw(2) << chIdx + 1 << " " << std::setw(10) << std::left << m.m_fa[m.m_functn[chIdx]] << ": "
              << std::right << m.hwCtrlDesc[cd.control[chIdx]] << "  " << m.hwCtrlDesc[m.m_trim[chIdx]]
              << "  " << std::showpos << m.m_trimRate[chIdx] << "%  "
              << std::array<std::string, 5>{ {"?", "Normal", "ATL Revers", "ATL Norm", "Center"}}
            [static_cast<size_t>(to_ut(m.m_trimMode[chIdx]) % 5U)]
            << std::endl;
          }
          for (size_t chIdx = 0; chIdx < 2; ++chIdx) {
            std::cout << "\t" << std::setw(2) << chIdx + 13 << " DG" << chIdx << "       :" << m.m_digiCtrl[chIdx] << std::endl;
          }
        }
      }

    }
    else {
      std::cout << "Failed to load \"" << fname << "\"" << std::endl;
    }
    std::cout << "-------------------" << std::endl;
  }
}

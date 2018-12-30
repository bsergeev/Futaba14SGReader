// Builds warning free with:
//  g++ -std=c++17 -Wall -Wextra -Wshadow -Wnon-virtual-dtor -Wold-style-cast -Wcast-align -Wunused -Woverloaded-virtual -Wpedantic -Wsign-conversion -Wmisleading-indentation -Wduplicated-cond -Wduplicated-branches -Wlogical-op -Wnull-dereference -Wuseless-cast -Wdouble-promotion -Wformat=2
// In VB:
// & -> Long
// % -> Integer
// # -> Double
// ! -> Single
// @ -> Decimal
// $ -> String

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
public:
  enum class TxType : uint8_t {
    INVALID = 0xFF,
    T8FG  = 0,
    T14SG = 1,
    T18SZ = 2
  };
  enum class Geo : uint8_t {
    UNKNOWN,
    General,
    France
  };
  enum class ModelType : uint8_t {
    INVALID = 0xFF,
    Plane  = 0,
    Heli   = 1,
    Glider = 2,
    Multi  = 3
  };
  enum class TrimMode : uint8_t {
    INVALID = 0,
    Normal, ATLRev, ATLNorm, Center
  };
  enum class Modulation : uint8_t { 
    INVALID    = 0xFF,
    FASST_7CH     = 0,
    FASST_MULTI   = 1, 
    FASST_MLT2    = 2,
    S_FHSS        = 4,  
    FASSTest_14CH = 8, 
    FASSTest_12CH = 10, 
    T_FHSS        = 12 
  };

  struct RxInfo {
    uint32_t ID = 0; // invalid
    double   BatteryFsV = 0.0;
  };

  inline static const size_t NO_CONTROL_IDX = 31; // GCC 8 requires "inline", WTF? 

  static const size_t MAX_CH = 16; // std::max(t14Channels, t18Channels);
  typedef size_t hwCtrlIdx;
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
    std::array<hwCtrlIdx, MAX_CH> control;
    std::string conditionControl;
  };

private:
  static const size_t t14Channels   = 12, t18Channels    = 16;
  static const size_t t14ChannelsLow = 8, t18ChannelsLow = 12;
  static const size_t t14Conditions  = 5, t18Conditions = 8;
  static const size_t MAX_CONDITNS = std::max(t14Conditions, t18Conditions);

  static const size_t                  NUMBER_OF_FUNCTIONS = 33;
  static const std::array<std::string, NUMBER_OF_FUNCTIONS> FUNCTIONS_AIR;
  static const std::array<std::string, NUMBER_OF_FUNCTIONS> FUNCTIONS_HELI;
  static const std::array<std::string, NUMBER_OF_FUNCTIONS> FUNCTIONS_MULTI;
  static const std::array<std::string, NO_CONTROL_IDX + 1>  SWITCH_NAME;
  static const std::array<uint8_t,     16>                  TELEMETRY_TYPE;
  static const std::array<double,      16>                  TFHSS_VOLT_LIST;


  uint8_t     m_wingType  = 0;
  uint8_t     m_tailType  = 0;
  uint16_t    m_FSMode    = 0;
  uint16_t    m_FSBattery = 0;
  std::string m_releaseBfsHW;
  bool        m_sysTelemAct = false;
  bool        m_singleRX = true;
  Geo         m_Area = Geo::UNKNOWN;
  double      m_telemDlInterval = 0.0;

  std::array<bool,      MAX_CH> m_reversed;
  std::array<bool,      2>      m_reversedDG;
  std::array<uint8_t,   MAX_CH> m_travelLo, m_travelHi, m_limitLo, m_limitHi;
  std::array<uint8_t,   MAX_CH> m_sSpeed; // [0, 27]
  std::array<int16_t,   MAX_CH> m_sTrim;  // [-240, 240]
  std::array<int16_t,   MAX_CH> m_fsPosition;
  std::array<hwCtrlIdx, MAX_CH> m_trim;
  std::array<int16_t,   MAX_CH> m_trimRate;
  std::array<TrimMode,  MAX_CH> m_trimMode;
  std::array<RxInfo,    2>      m_RX;

  size_t m_numConditions = 1; // 1 for condition-less models, or set in getConditions(): up to 5 for 14SG or 8 for 18SZ
  std::vector<ConditionDependentParams> m_conditionalData; // .size() == m_numConditions

  std::array<uint8_t, MAX_CH> m_functn; // value is the index of std::array<std::string, NUMBER_OF_FUNCTIONS>, i.e. < 33
  std::array<std::string, NUMBER_OF_FUNCTIONS> m_funcName;

  std::array<std::wstring, MAX_CONDITNS> m_conditionName;
  std::array<size_t,       MAX_CONDITNS> m_conditionState, m_conditionList;
  std::array<size_t,       MAX_CONDITNS> m_conditionHw;
  std::array<std::string,  2> m_digiCtrl;

  struct HwNamnes {
    int8_t Type = -1;
    std::string Ctrl, Pos, Rev, Sym;
  };

  std::vector<uint8_t> m_data;
  TxType       m_txType     = TxType::INVALID;
  Modulation   m_modulation = Modulation::INVALID;
  ModelType    m_modelType  = ModelType::INVALID;
  std::wstring m_modelName  = L"UNKNOWN";

public: // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  explicit Model(const std::filesystem::path& filePath) {
    if (std::ifstream ifs{ filePath, std::ios::binary }; ifs.good()) {
      const auto file_size = ifs.seekg(0, std::ios_base::end).tellg();
      ifs.seekg(0);
      
      std::vector<char> data_;
      data_.resize(static_cast<std::size_t>(file_size));
      ifs.read(data_.data(), file_size);
      
      m_data = std::vector<uint8_t>{ begin(data_), end(data_) };

      process();
    }
  }

  [[nodiscard]] bool empty() const noexcept {
    return m_data.empty();
  }

  [[nodiscard]] std::wstring getModelName() const noexcept {
    return m_modelName;
  }

  [[nodiscard]] Modulation getModulation() const noexcept {
    return m_modulation;
  }

  void dump(std::ostream& out = std::cout, std::wostream& wout = std::wcout) {
    if (!empty()) {
      out << "TX: " << ((m_txType == Model::TxType::INVALID) ? "INVALID"
        : std::array<const char*, 3>{"T8FG", "T14SG", "T18SZ"}[to_ut(m_txType)]) << std::endl;

      wout << L"Model name: \"" << getModelName() << L"\"" << std::endl;

      out << "Model: " << ((m_modelType == Model::ModelType::INVALID) ? "INVALID"
        : std::array<const char*, 4>{"Plane", "Heli", "Glider", "Multi"}[to_ut(m_modelType)]) << "\n\n";

      const auto modulation = to_ut(getModulation());
      const char* modulationList[16] = { "FASST 7CH",     "FASST MULTI", "FASST MLT2",    "--",
                                         "S-FHSS",        "--",          "--",            "--",
                                         "FASSTest 14CH", "--",          "FASSTest 12CH", "--",
                                         "T-FHSS",        "--",          "--",            "--" };
      out << "SYSTEM" << std::endl;
      out << "\t" << modulationList[modulation] << "  " << ((m_singleRX) ? "SINGLE" : "DUAL") << " "
        << ((m_Area == Model::Geo::UNKNOWN) ? "" : (m_Area == Model::Geo::General ? "G" : "F")) << std::endl;
      out << "\t" << m_RX[0].ID;
      if (!m_singleRX) { out << "\t\t" << m_RX[1].ID; }
      out << std::endl;
      out << "\t" << std::setprecision(2) << m_RX[0].BatteryFsV << "V";
      if (!m_singleRX) { out << "\t\t" << std::setprecision(2) << m_RX[1].BatteryFsV << "V"; }
      out << std::endl;
      out << "\tTELEMETRY: " << ((m_sysTelemAct) ? "ACT" : "INH")
        << " DL " << std::setprecision(2) << m_telemDlInterval << "s" << std::endl;


      out << "Reverse & End Point" << std::endl;
      const size_t numChannels = (m_txType == Model::TxType::T18SZ) ? Model::t18Channels : Model::t14Channels;
      for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
        out << "\t" << std::setw(2) << chIdx + 1 << " " << std::setw(10) << std::left << m_funcName[m_functn[chIdx]] << ": "
          << ((m_reversed[chIdx]) ? "REVERSED" : "normal  ") << "  "
          << std::right << std::setw(3) << static_cast<int>(m_limitLo[chIdx]) << "  "
          << std::setw(3) << static_cast<int>(m_travelLo[chIdx]) << "   "
          << std::setw(3) << static_cast<int>(m_travelHi[chIdx]) << "  "
          << std::setw(3) << static_cast<int>(m_limitHi[chIdx]) << std::endl;
      }
      for (size_t chIdx = 0; chIdx < 2; ++chIdx) {
        out << "\t" << std::setw(2) << chIdx + 13 << " DG" << chIdx << "       : "
          << ((m_reversedDG[chIdx]) ? "REVERSED" : "normal") << std::endl;
      }

      out << "Servo Speed & SubTrim" << std::endl;
      for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
        out << "\t" << std::setw(2) << chIdx + 1 << " " << std::setw(10) << std::left << m_funcName[m_functn[chIdx]] << ": "
          << std::right << std::setw(3) << static_cast<int>(cServoSpeed(m_sSpeed[chIdx])) << "  " << std::setw(3)
          << static_cast<int>(m_sTrim[chIdx]) << std::endl;
      }

      auto cFailSafe = [this](size_t chIdx) { return ((m_FSMode    & (1 << chIdx)) == 0) ? "HOLD" : "F/S"; };
      auto cBatteryFS = [this](size_t chIdx) { return ((m_FSBattery & (1 << chIdx)) == 0) ? " OFF" : "ON"; };
      out << "Fail Safe" << std::endl;
      out << "\t\t\tF/S\tB.F/S\tPOS" << std::endl;
      for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
        out << "\t" << std::setw(2) << chIdx + 1 << " " << std::setw(10) << std::left << m_funcName[m_functn[chIdx]] << ": "
          << '\t' << cFailSafe(chIdx) << "\t" << cBatteryFS(chIdx);
        if ((m_FSMode & (1 << chIdx)) != 0 || (m_FSBattery & (1 << chIdx)) != 0) {
          out << '\t' << std::right << std::setw(4) << m_fsPosition[chIdx] << "%";
        }
        out << std::right << std::endl;
      }
      out << "\tRelease battery F/S: " << m_releaseBfsHW << std::endl;

      if (m_txType == Model::TxType::T18SZ || m_modelType == Model::ModelType::Heli || m_modelType == Model::ModelType::Glider) {
        out << "Condition #" << std::endl;
        for (size_t condIdx = 0; condIdx < m_numConditions; ++condIdx) {
          wout << L"\t" << condIdx + 1 << L": " << m_conditionName[condIdx];
          out << "  " << m_conditionalData[condIdx].conditionControl << std::endl;
        }
      }

      out << "Function" << std::endl;
      for (size_t condIdx = 0; condIdx < m_numConditions; ++condIdx)
      {
        if (m_numConditions > 1) {
          wout << L"    Condition #" << condIdx + 1 << L": " << m_conditionName[condIdx] << std::endl;
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
          wout << L"\tSame as condition #" << sameAsCondition + 1 << L" " << m_conditionName[sameAsCondition] << std::endl;
        } else {
          out << "\t # Function  Ctrl Trim Rate  Mode" << std::endl;
          for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
            out << "\t" << std::setw(2) << chIdx + 1 << " " << std::setw(10) << std::left << m_funcName[m_functn[chIdx]] << ": "
              << std::right << SWITCH_NAME[cd.control[chIdx]] << "  " << SWITCH_NAME[m_trim[chIdx]]
              << "  " << std::showpos << m_trimRate[chIdx] << "%  "
              << std::array<std::string, 5>{ {"?", "Normal", "ATL Revers", "ATL Norm", "Center"}}[static_cast<size_t>(to_ut(m_trimMode[chIdx]) % 5U)]
              << std::endl;
          }
          for (size_t chIdx = 0; chIdx < 2; ++chIdx) {
            out << "\t" << std::setw(2) << chIdx + 13 << " DG" << chIdx << "       :" << m_digiCtrl[chIdx] << std::endl;
          }
        }
      }
    } else {
      out << "Model empty" << std::endl;
    }
  }

private:
  [[nodiscard]] static constexpr uint8_t cServoSpeed(uint8_t y) noexcept {
    if (y < 67) { return static_cast<uint8_t>(y / 10.0 + 0.5); }
    if (y < 78) { return static_cast<uint8_t>((y - 67) / 4.0 + 0.5) + 0x07U; }
    if (y ==78) { return 10; } // y - 68;
    if (y < 88) { return static_cast<uint8_t>((y - 80) / 3.0 + 0.5) + 0x0BU; }
    return  static_cast<uint8_t>(y - 74);
  }

  [[nodiscard]] inline bool isT18SZ() const noexcept { // TODO: replace everywhere
    return (m_txType == TxType::T18SZ);
  }

  // Read all the parameters from m_data
  void process() {
    m_modelName = readModelName();

    // Read TX type
    const char* p = reinterpret_cast<const char*>(&m_data.front());
    constexpr size_t TX_ID_LENGTH = 8;
    if (std::string_view{ p, TX_ID_LENGTH } == "T18SZ   "s) {
      m_txType = TxType::T18SZ;
    } else {
      std::array<char, TX_ID_LENGTH>  buffer;
      for (size_t i = 0, j = 2; i < TX_ID_LENGTH; ++i, j += 2) {
        buffer[i] = p[j];
      }
      std::string_view txTypeName{ buffer.data(), TX_ID_LENGTH };
      if (txTypeName == "T8FG    "s) {
        m_txType = TxType::T8FG;
      } else if (txTypeName == "T14SG   "s) {
        m_txType = TxType::T14SG;
      } else {
        assert(!"Unknown TX type");
      }
    }

    // Read model type
    const size_t i = (m_txType == TxType::T18SZ) ? 93 : 152;
    const uint8_t v = m_data.at(i + 1) / 16;
    m_modelType = (v > to_ut(ModelType::Multi))? ModelType::INVALID : static_cast<ModelType>(v);
    m_wingType =  m_data.at(i) & 0x0F;
    m_tailType = (m_data.at(i) & 0x30) >> 4;

    // Read modulation
    const size_t am = isT18SZ()? 92 : 154;
    if (m_txType == TxType::T8FG) {
      const auto w = ((m_data.at(am) & 0x30) + (m_data.at(am + 1) & 0x80)) >> 4;
      switch (w) {
      case 1: m_modulation = Modulation::FASST_MULTI; break;
      case 3: m_modulation = Modulation::FASST_7CH;   break;
      case 9: m_modulation = Modulation::FASST_MLT2;  break;
      }
    } else {
      const size_t dm = (m_txType == TxType::T18SZ) ? 16 : 1;
      m_modulation = static_cast<Modulation>((m_data.at(am) / dm) & 0x0F);
    }

    // Read everything else
    readSystemInfo();
    readFunction();
    readServoRevers();
    readEndPoints();
    readServoSpeed();
    readSubTrim();
    readFailSafe();
    readConditions();
    readControlAssignment();
    readConditionSelect();
  } // process()

  std::wstring readModelName() const noexcept {
    size_t startPos = 0, len = 0;
    if (isT18SZ()) {
      startPos = 10; len = 15;
    } else {
      startPos = 17; len = 10;
    }

    std::array<wchar_t, 15> buffer; // longer of the two len
    size_t i = 0;
    for (size_t j = startPos; i < len; j += 2) {
      buffer[i] = static_cast<wchar_t>((m_data.at(j) << 8) + m_data.at(j + 1));
      if (buffer[i++] == 0) {
        break;
      }
    }
    return std::wstring{ buffer.data(), i };
  }

  void readFunction() {
    size_t numChannels, addr;
    if (isT18SZ()) {
      numChannels = t18Channels; addr = 102;
    } else {
      numChannels = t14Channels; addr = 178;
    }
    for (size_t i = 0; i < numChannels; ++i) {
      m_functn[i] = m_data.at(addr + i);
    }
    switch (m_modelType) {
    case ModelType::Plane:
    case ModelType::Glider: m_funcName = FUNCTIONS_AIR;   break;
    case ModelType::Heli:   m_funcName = FUNCTIONS_HELI;  break;
    case ModelType::Multi:  m_funcName = FUNCTIONS_MULTI; break;
    case ModelType::INVALID: assert(!"Invalid model type"); break;
    }
    if (!isT18SZ() && m_modelType == ModelType::Plane) {
      m_funcName[25] = "VPP"s;
    }
  }

  void readConditions() {
    for (size_t i = 0; i < t18Conditions; ++i) {
      m_conditionName[i] = L""; m_conditionState[i] = 0; m_conditionHw[i] = static_cast<size_t>(-1) /*hwOff*/; m_conditionList[i] = 0;
    }
    m_numConditions = 1;
    m_conditionState[0] = 128 + 15;
    m_conditionList [0] = 0;

    // Get names of the conditions
    const size_t numTxConditions = (isT18SZ())? t18Conditions : t14Conditions;
    if (m_txType == TxType::T8FG) {
      switch (m_modelType) {
      case ModelType::Heli:   m_conditionName[0] = L"NORMAL";   m_conditionName[1] = L"IDLEUP1"; m_conditionName[2] = L"IDLEUP2";
        m_conditionName[3] = L"IDLEUP3";  m_conditionName[4] = L"HOLD";
        m_numConditions = 5;
        break;
      case ModelType::Glider: m_conditionName[0] = L"NORMAL";   m_conditionName[1] = L"START"; m_conditionName[2] = L"SPEED";
        m_conditionName[3] = L"DISTANCE"; m_conditionName[4] = L"LANDING";
        m_numConditions = 5;
        break;
      case ModelType::INVALID: assert(!"Invalid model type"); break;
      default: break; // do nothing
      }
    } else { // T18SZ or T14SZ
      if (m_txType == TxType::T18SZ || m_modelType == ModelType::Heli || m_modelType == ModelType::Glider) {
        m_numConditions = numTxConditions;
        for (size_t condIdx = 0; condIdx < numTxConditions; ++condIdx) {
          std::array<wchar_t, 8 + 1> buffer;
          buffer.fill(0);
          for (size_t charIdx = 0; charIdx < 8; ++charIdx) {
            if (m_txType == TxType::T18SZ) { // UTF16
              const char hi = static_cast<char>(m_data.at(28140 + condIdx * 578 + charIdx * 2));
              const char lo = static_cast<char>(m_data.at(28140 + condIdx * 578 + charIdx * 2 + 1));
              buffer[charIdx] = static_cast<wchar_t>((hi << 8) + lo);
            } else { // T14SZ, 1-byte
              buffer[charIdx] = m_data.at(1700 + condIdx * 9 + charIdx);
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
    if (m_txType == TxType::T18SZ || m_modelType == ModelType::Heli || m_modelType == ModelType::Glider)
    {
      std::array<size_t, t18Conditions> cp;
      cp.fill(0);

      const size_t addr = (isT18SZ())? 64 : /*464*/451;
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

  HwNamnes getHardware(size_t a) const {
    HwNamnes hw;
    const uint8_t hC0 = m_data.at(a);
    const uint8_t i1  = m_data.at(a + 1);
    const uint8_t i2  = m_data.at(a + 2);
    if (hC0 == 0xFF) {
      hw.Type = -1; hw.Ctrl = "--";
      hw.Pos = (i1 != 0 || i2 != 0) ? "OFF" : "ON";
      return hw;
    }
    const uint8_t hR = hC0 & 0x40;
    const uint8_t hC = hC0 & 0x3F;
    if (hC >= 32) { 
      hw.Ctrl = "Logic"; 
      return hw; 
    }
    hw.Ctrl = SWITCH_NAME[hC];
    if ((hC & 0x34) == 4) {
      if ((hC & 0x37) == 7) {
        hw.Type = 2; // 2-position switch
        hw.Pos = (hR == 0)? "OFF/ON" : "ON/OFF";
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
    if (static_cast<uint16_t>(i1) + i2 == 0x0100) { 
      hw.Sym = "Symmetry"; 
      hw.Pos = std::to_string(round(static_cast<int8_t>(i2)*100.0 / 64)); 
      return hw; 
    }
    if (i1 - i2 == 1) { 
      hw.Sym = "Linear";   
      hw.Pos = std::to_string(round(static_cast<int8_t>(i1)*100.0 / 64)); 
      return hw; 
    }
    hw.Pos = "Error!!!"; hw.Rev = ""; hw.Sym = "";
    return hw;
  }

  void readConditionSelect() {
    auto logicSwitch = [this](size_t a) -> std::string {
      const size_t aa = isT18SZ()? 456 : 328;
      if ((m_data.at(a) & 48) == 48) {
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
      } else {
        auto hw = getHardware(a);
        return hw.Ctrl + " " + hw.Pos + " " + hw.Rev + " " + hw.Sym;
      }
    };

    for (size_t i = 1; i < m_numConditions; ++i) {
      if (m_conditionList[i] != 0) {
        m_conditionalData[i].conditionControl = logicSwitch(m_conditionHw[m_conditionList[i]]);
      }
    }
  }

  void readServoRevers() {
    m_reversed.fill(false);
    m_reversedDG.fill(false);

    const bool isT18 = isT18SZ();
    const uint8_t  revLo = m_data.at((isT18)? 252 : 268);
    const uint16_t revHi = m_data.at((isT18)? 253 : 165);
    const uint8_t  revDg = m_data.at((isT18)? 518 : 154) & 0xC0;
    const uint16_t rev = (revHi << 8) | revLo;

    const size_t numChannels = (isT18)? t18Channels : t14Channels;
    for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
      m_reversed[chIdx] = (rev & (0x0001 << chIdx)) != 0;
    }
    m_reversedDG[0] = (revDg & 0x40) != 0;
    m_reversedDG[1] = (revDg & 0x80) != 0;
  }

  void readEndPoints() {
    m_travelLo.fill(0); m_travelHi.fill(0);
    m_limitLo.fill(0);  m_limitHi.fill(0);
    size_t atl, ath, ln, all, alh;
    if (isT18SZ()) {
      atl = 254; ath = 562; ln = t18ChannelsLow;
      all = 278; alh = 570;
    } else {
      atl = 290; ath = 706; ln = t14ChannelsLow;
      all = 664; alh = 714;
    }
    const size_t numChannels = (isT18SZ())? t18Channels : t14Channels;
    for (size_t i = 0; i < numChannels; ++i) {
      size_t j = (i < ln) ? atl + i*2 : ath + (i - ln)*2;
      m_travelLo[i] = m_data.at(j);  m_travelHi[i] = m_data.at(j + 1);
      j = (i < ln) ? all + i*2 : alh + (i - ln)*2;
      m_limitLo[i] = m_data.at(j);   m_limitHi[i] = m_data.at(j + 1);
    }
  }

  void readServoSpeed() {
    m_sSpeed.fill(0);
    size_t al, ah, ln, k;
    if (isT18SZ()) {
      al = 438;  ah = 594; ln = t18ChannelsLow;  k = 1;
    } else {
      al = 1812; ah = 1828; ln = t14ChannelsLow; k = 2;
    }
    const size_t numChannels = (isT18SZ()) ? t18Channels : t14Channels;
    for (size_t i = 0; i < numChannels; ++i) {
      const size_t j = (i < ln) ? al + i * k : ah + (i - ln)*k;
      m_sSpeed[i] = m_data.at(j);
    }
  }

  void readSubTrim() {
    m_sTrim.fill(0);
    size_t al, ah, ln;
    if (isT18SZ()) {
      al = 414; ah = 586; ln = t18ChannelsLow;
    } else {
      al = 306; ah = 166; ln = t14ChannelsLow;
    }
    const size_t numChannels = (isT18SZ())? t18Channels : t14Channels;
    for (size_t i = 0; i < numChannels; ++i) {
      const size_t j = (i < ln)? al + i*2 : ah + (i - ln)*2;
      m_sTrim[i] = (m_data.at(j) << 8) | m_data.at(j + 1);
    }
  }

  void readControlAssignment() {
    assert(m_numConditions > 0);
    m_conditionalData.resize(m_numConditions);

    const bool isT18 = isT18SZ();

    // Control assignments for each condition
    for (size_t condIdx = 0; condIdx < m_numConditions; ++condIdx) {
      auto& cd = m_conditionalData.at(condIdx);
      if (isT18) {
        const size_t  ac = 640, lc = 3056, axc = 118;
        for (size_t i = 0; i < t18Channels; ++i) {
          size_t a1 = axc + m_functn[i];
          size_t a2 = ac + lc * (m_conditionList[condIdx]) + m_data.at(a1);
          cd.control[i] = std::min<hwCtrlIdx>(NO_CONTROL_IDX, m_data.at(a2));
          a1 = axc + m_functn[i] + NUMBER_OF_FUNCTIONS - 1;
          a2 = ac + lc * (m_conditionList[condIdx]) + MAX_CH + m_data.at(a1);
          m_trim[i] = std::min<hwCtrlIdx>(NO_CONTROL_IDX, m_data.at(a2)); // <<< DEBUG move out of condIdx loop!
        }
      } else {
        static const std::array<size_t, 3> ag = { 1545, 1453, 1539 };
        for (size_t i = 0; i < t14Channels; ++i) {
          size_t a2 = 222 + m_data.at(190 + m_functn[i]);
          if (m_functn[i] >= 22 && m_functn[i] <= 24 && m_modelType == ModelType::Glider) {
            const size_t a1 = ag[m_functn[i] - 22U];
            if (m_data.at(a1) > 127) {
              a2 = a1 + m_conditionList[condIdx] + 1;
            }
          }
          cd.control[i] = std::min<hwCtrlIdx>(NO_CONTROL_IDX, m_data.at(a2));
          a2 = 234 + m_data.at(190 + m_functn[i]);
          m_trim[i] = std::min<hwCtrlIdx>(NO_CONTROL_IDX, m_data.at(a2)); // <<< DEBUG move out of condIdx loop!
        }
      }
    }

    // Controls for DGs (same for all conditions)
    const std::string ls = " ";
    for (size_t ch = 0; ch < 2; ++ch) {
      if (isT18) {
        const size_t addr18dgCtrl = 450;
        if ((m_data.at(addr18dgCtrl + ch * 3) & 48) == 48) {
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
        } else {
          auto hw = getHardware(addr18dgCtrl + ch * 3);
          m_digiCtrl[ch] = ls + hw.Ctrl + "  " + hw.Pos + "  " + hw.Rev + "  " + hw.Sym;
        }
      } else { // <<< DEBUG TBD: handle 'Logic' for 14SG
        const uint8_t m = static_cast<uint8_t>(1U << ch);
        std::string alt = (m_data.at(681) & m) ? "Alternate" : "";
        auto hw = getHardware(322 + ch * 3);
        m_digiCtrl[ch] = ls + hw.Ctrl + "  " + hw.Pos + "  " + hw.Rev + "  " + hw.Sym + "  " + alt;
      }
    }

    // Trim rates
    const size_t numChannels = (isT18) ? t18Channels : t14Channels;
    for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
      size_t atr, ats, x;
      if (isT18) {
        atr = 182; ats = 519; x = m_functn[chIdx];
      } else {
        atr = 246; ats = 258; x = m_data.at(190 + m_functn[chIdx]);
      }
      const uint8_t m = static_cast<uint8_t>(1U << (x % 8));
      auto v = static_cast<int16_t>(m_data.at(atr + x));
      m_trimRate[chIdx] = ((m_data.at(ats + x / 8) & m) != 0) ? -v : v;
    }

    // Trim mode
    for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
      size_t atc, atm, atr, x;
      if (isT18) {
        atc = 214; atm = 218; atr = 222; x = m_functn[chIdx];
      } else {
        atc = 260; atm = 260; atr = 262; x = m_data.at(190 + m_functn[chIdx]);
      }
      const uint8_t m = static_cast<uint8_t>(1 << (x % 8U));
      if ((m_data.at(atc + x / 8) & m) == 0 && (m_data.at(atm + x / 8) & m) == 0) {
        m_trimMode[chIdx] = TrimMode::Normal;
      } else {
        if (m_data.at(atm + x / 8) & m) {
          m_trimMode[chIdx] = (m_data.at(atr + x / 8) & m) ? TrimMode::ATLRev : TrimMode::ATLNorm;
        } else {
          m_trimMode[chIdx] = TrimMode::Center;
        }
      }
    }
  }

  void readFailSafe() {
    const bool isT18 = isT18SZ();
    m_FSMode = (m_data.at((isT18) ? 335 : 697) << 8) + m_data.at((isT18) ? 334 : 269);
    m_FSBattery = (m_data.at((isT18) ? 361 : 164) << 8) + m_data.at((isT18) ? 360 : 286);

    size_t al, ah, ln;
    if (isT18) {
      al = 336; ah = 578; ln = t18ChannelsLow;
    } else {
      al = 270; ah = 698; ln = t14ChannelsLow;
    }
    const size_t numChannels = (isT18)? t18Channels : t14Channels;
    for (size_t chIdx = 0; chIdx < numChannels; ++chIdx) {
      const size_t addr = (chIdx < ln)? al + chIdx * 2
                                      : ah + (chIdx - ln) * 2;
      const int16_t st = (m_data.at(addr) << 8) + m_data.at(addr + 1);
      m_fsPosition[chIdx] = static_cast<int16_t>(round((st - 1024) / 6.73));
    }

    auto hw = getHardware((isT18) ? 362 : 287);
    m_releaseBfsHW = hw.Ctrl + "  " + hw.Pos + "  " + hw.Rev + "  " + hw.Sym;
  }

  void readSystemInfo() {
    auto getRxID = [this](size_t a) -> uint32_t { 
      return static_cast<uint32_t>((m_data.at(a) << 24) | (m_data.at(a + 1) << 16) | (m_data.at(a + 2) << 8) | m_data.at(a + 3)); 
    };
    auto getBFsVoltage = [this](size_t sysModultn, size_t a) -> double {
      const size_t tlmType = TELEMETRY_TYPE[sysModultn % TELEMETRY_TYPE.size()];
      return (tlmType == 1) ? m_data.at(a) / 10.0
                            : (tlmType == 2) ? TFHSS_VOLT_LIST[m_data.at(a) % TFHSS_VOLT_LIST.size()]
                                             : 0.0;
    };

    size_t aa, ar, ai1, ai2, ata, mta, adl, ddl, av1, av2, mr;
    if (isT18SZ()) {
      aa = 0; ar = 516; ai1 = 406; ai2 = 410; ata = 92; mta = 2;
      adl = 516; ddl = 4; av1 = 401; av2 = 402; mr = 2;
    } else {
      aa = 156; ar = 2204; ai1 = 2196; ai2 = 2200; ata = 2206; mta = 128;
      adl = 2206; ddl = 1; av1 = 2208; av2 = 2209; mr = 1;
    }
    m_sysTelemAct = (m_data.at(ata) & mta) != 0;
    const size_t sysModulation = to_ut(m_modulation);
    if (isT18SZ()) { // && (sysModulation & 0x03) != 0) { // i.e. "FASST MULTI" or "FASST MLT2" // <<< DEBUG correct?
      m_Area = ((m_data.at(aa) & 0x80) == 0) ? Geo::General : Geo::France;
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
  } // getSystemInfo()
}; // class Model

//static 
const std::array<std::string, 32> Model::SWITCH_NAME = {
    "J1"s, "J2"s, "J4"s, "J3"s, "SC"s, "SD"s, "SG"s, "SH"s, "RD"s, "RS"s,
    "OA"s, "0B"s, "SA"s, "SB"s, "SE"s, "SF"s, "LD"s, "11"s, "LS"s, "13"s,
    "T1"s, "T2"s, "T4"s, "T3"s, "T5"s, "T6"s, "T7"s, "1B"s, "1C"s, "1D"s, "1E"s, "--"s };
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

  for (const char* fname : names) {
    Model m{ std::filesystem::path(fname) };
    m.dump();
    std::cout << "-------------------" << std::endl;
  }
}

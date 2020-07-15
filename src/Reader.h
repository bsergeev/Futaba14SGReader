// Copyright 2012...2020 MileHighWings, All Rights Reserved

#pragma once

#include <cstdint>
#include <type_traits>

// Cast a scoped enum value to its underlying type
template <typename E,
          typename std::enable_if_t<std::is_enum_v<E>>* = nullptr>
  [[nodiscard]] constexpr typename std::underlying_type<E>::type ut_cast(E e) noexcept {
  return static_cast<typename std::underlying_type<E>::type>(e);
}

#include <algorithm>
#include <array>
#include <iostream>
#include <filesystem>
#include <fstream>
#include <limits>
#include <string>
#include <vector>

#define val const auto

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

  inline static constexpr size_t NO_CONTROL_IDX = 31; // GCC 8 requires "inline", WTF? 

private:
  static constexpr size_t t14Channels = 12, t18Channels = 16;
  static constexpr size_t MAX_CH = std::max(t14Channels, t18Channels);

  using hwCtrlIdx = size_t;
  struct ConditionDependentParams {
    ConditionDependentParams() { control.fill(NO_CONTROL_IDX); }

    bool operator ==(const ConditionDependentParams& o) const {
      return std::equal(std::begin(control), std::end(control), std::begin(o.control));
    }
  //data:
    std::array<hwCtrlIdx, MAX_CH> control;
    std::string conditionControl;
  };

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
  explicit Model(const std::filesystem::path& filePath);

  [[nodiscard]] bool empty() const noexcept { return m_data.empty(); }
  [[nodiscard]] size_t getNumChannels() const noexcept { return isT18SZ()? t18Channels : t14Channels; }

  [[nodiscard]] std::wstring getModelName() const noexcept { return m_modelName; }
  [[nodiscard]] Modulation  getModulation() const noexcept { return m_modulation; }

  void dump(std::ostream& out = std::cout, std::wostream& wout = std::wcout) const;
  
private:
  [[nodiscard]] static constexpr uint8_t cServoSpeed(uint8_t y) noexcept;
  [[nodiscard]] inline bool isT18SZ() const noexcept { return (m_txType == TxType::T18SZ); }

  void processData(); // Read all the parameters from m_data
  
  [[nodiscard]] std::wstring readModelName() const noexcept;
  [[nodiscard]] HwNamnes     getHardware(size_t a) const;

  void readFunction();
  void readConditions();
  void readConditionSelect();
  void readServoRevers();
  void readEndPoints();
  void readServoSpeed();
  void readSubTrim();
  void readControlAssignment();
  void readFailSafe();
  void readSystemInfo();
}; // class Model

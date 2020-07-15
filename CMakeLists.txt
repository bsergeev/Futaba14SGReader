cmake_minimum_required (VERSION 3.5)
set(REQUIRED_CPP_VERSION 17)
set(CMAKE_CXX_STANDARD ${REQUIRED_CPP_VERSION})

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug)
endif()
message(STATUS "CMAKE_BUILD_TYPE = ${CMAKE_BUILD_TYPE}")

set(TARGET_NAME Reader)
project(${TARGET_NAME} CXX)

set(headers
  "${PROJECT_SOURCE_DIR}/src/Reader.h"
)
set(sources 
  "${PROJECT_SOURCE_DIR}/src/Reader.cpp"
)

add_executable(${TARGET_NAME} ${sources} ${headers})
include_directories(src)

set_property(TARGET ${TARGET_NAME} PROPERTY CXX_STANDARD ${REQUIRED_CPP_VERSION})
set_property(TARGET ${TARGET_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)
if (WIN32 OR WIN64)
  set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT ${TARGET_NAME})
  #target_link_libraries(${TARGET_NAME} ${Boost_LIBRARIES})
elseif(UNIX)
  # if (${CMAKE_BUILD_TYPE} STREQUAL "Debug")
  #   set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=thread")
  #   set_property(TARGET ${TARGET_NAME} APPEND_STRING PROPERTY LINK_FLAGS " -fsanitize=thread")
  # endif()      
  target_link_libraries(${TARGET_NAME} stdc++fs)
endif()

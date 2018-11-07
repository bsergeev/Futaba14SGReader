cmake_minimum_required (VERSION 3.5)
set(REQUIRED_CPP_VERSION 14)
set(CMAKE_CXX_STANDARD ${REQUIRED_CPP_VERSION})

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug)
endif()
message(STATUS "CMAKE_BUILD_TYPE = ${CMAKE_BUILD_TYPE}")

set(TARGET_NAME TestQueue)
project(${TARGET_NAME} CXX)

option(Boost_USE_STATIC_LIBS "Use Boost static version" ON)
set(BOOST_VER "1.66.0" CACHE STRING "boost version" )
set(Boost_USE_MULTITHREADED ON)
set(Boost_USE_STATIC_RUNTIME OFF)
find_package(Boost ${BOOST_VER} REQUIRED COMPONENTS thread regex REQUIRED)
if (NOT Boost_FOUND)
  message(FATAL_ERROR "Boost not found.")
endif()


# gather the files
set(headers
  "${PROJECT_SOURCE_DIR}/src/utils.h"
  "${PROJECT_SOURCE_DIR}/src/EventQueue.h"
)
set(sources 
  "${PROJECT_SOURCE_DIR}/src/main.cpp"
)
include_directories("${Boost_INCLUDE_DIRS}")

add_executable(${TARGET_NAME} ${sources} ${headers})
include_directories(include src)

set_property(TARGET ${TARGET_NAME} PROPERTY CXX_STANDARD ${REQUIRED_CPP_VERSION})
if (WIN32 OR WIN64)
  set_property(TARGET ${TARGET_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)
  set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT ${TARGET_NAME})
  target_link_libraries(${TARGET_NAME} ${Boost_LIBRARIES})
elseif(UNIX)
  # if (${CMAKE_BUILD_TYPE} STREQUAL "Debug")
  #   set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=thread")
  #   set_property(TARGET ${TARGET_NAME} APPEND_STRING PROPERTY LINK_FLAGS " -fsanitize=thread")
  # endif()      
  # target_link_libraries(${TARGET_NAME} ${Boost_LIBRARIES} c++ c++abi pthread)
  target_link_libraries(${TARGET_NAME} ${Boost_LIBRARIES} pthread)
endif()

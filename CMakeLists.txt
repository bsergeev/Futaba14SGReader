cmake_minimum_required (VERSION 3.5)
set(REQUIRED_CPP_VERSION 17)
set(CMAKE_CXX_STANDARD ${REQUIRED_CPP_VERSION})

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug)
endif()
message(STATUS "CMAKE_BUILD_TYPE = ${CMAKE_BUILD_TYPE}")

if(NOT TARGET_NAME)
  set(TARGET_NAME Reader)
endif()
project(${TARGET_NAME} VERSION 0.0.1 LANGUAGES CXX)

# Define project_options "library" with all the build options and warnings
include(cmake/ProjectOptions.cmake)
include(cmake/BuildType.cmake)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

# gather the files
set(include_dirs  "${PROJECT_SOURCE_DIR}/src")
set(headers
  "${PROJECT_SOURCE_DIR}/src/Reader.h"
)
set(sources 
  "${PROJECT_SOURCE_DIR}/src/Reader.cpp"
)

find_package(Qt5 COMPONENTS Core Widgets OpenGL REQUIRED)
if (NOT Qt5_FOUND)
  message(FATAL_ERROR "Qt5 not found.")
endif()
message(STATUS "-> Qt: ${Qt5_DIR}")

add_executable(${TARGET_NAME} ${sources} ${headers})
target_include_directories(${TARGET_NAME} PRIVATE "${include_dirs}")

if (MSVC)
  add_definitions(-D_WIN32_WINNT=0x0601) # Windows 7
  set_property(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY VS_STARTUP_PROJECT ${TARGET_NAME})
endif()

set_property(TARGET ${TARGET_NAME} PROPERTY CXX_STANDARD ${REQUIRED_CPP_VERSION})
set_property(TARGET ${TARGET_NAME} PROPERTY CXX_STANDARD_REQUIRED ON)

if (WIN32 OR WIN64)
  target_link_libraries(${TARGET_NAME} project_options
    Qt5::Widgets
  )

  get_target_property(_qmake_executable Qt5::qmake IMPORTED_LOCATION)
  get_filename_component(_qt_bin_dir "${_qmake_executable}" DIRECTORY)
  find_program(WINDEPLOYQT_EXECUTABLE windeployqt HINTS "${_qt_bin_dir}")
  add_custom_command(TARGET ${TARGET_NAME} POST_BUILD
    COMMAND "${CMAKE_COMMAND}" -E
        env PATH="${_qt_bin_dir}" "${WINDEPLOYQT_EXECUTABLE}"
            --verbose 0
            --no-compiler-runtime
            \"$<TARGET_FILE:${TARGET_NAME}>\"
    COMMENT "Deploying Qt...")
elseif(UNIX)
  # if (${CMAKE_BUILD_TYPE} STREQUAL "Debug")
  #   set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=thread")
  #   set_property(TARGET ${TARGET_NAME} APPEND_STRING PROPERTY LINK_FLAGS " -fsanitize=thread")
  # endif()      
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17") #  -stdlib=libc++
  target_link_libraries(${TARGET_NAME} project_options 
    Qt5::Widgets
    stdc++fs
  )
endif()

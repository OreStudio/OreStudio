# Builds the datum catalogue harness against a local ORE engine build. It is not
# part of the ORE Studio build, which does not link ORE.
#
#   cmake -S . -B build -DORE_SOURCE_DIR=<engine source> -DORE_BUILD_DIR=<engine build>
#   cmake --build build
cmake_minimum_required(VERSION 3.25)
project(ore_datum_catalogue CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if(NOT ORE_SOURCE_DIR OR NOT ORE_BUILD_DIR)
    message(FATAL_ERROR "Set ORE_SOURCE_DIR and ORE_BUILD_DIR to a built ORE engine checkout.")
endif()

find_package(Boost REQUIRED)

add_executable(datum_catalogue datum_catalogue.cpp)
target_include_directories(datum_catalogue PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${ORE_SOURCE_DIR}/OREData
    ${ORE_SOURCE_DIR}/QuantExt
    ${ORE_SOURCE_DIR}/QuantLib
    ${ORE_BUILD_DIR}/QuantLib)
target_link_libraries(datum_catalogue PRIVATE
    ${ORE_BUILD_DIR}/OREData/ored/libOREData.so
    ${ORE_BUILD_DIR}/QuantExt/qle/libQuantExt.so
    ${ORE_BUILD_DIR}/QuantLib/ql/libQuantLib.so
    Boost::headers)
set_target_properties(datum_catalogue PROPERTIES
    BUILD_RPATH "${ORE_BUILD_DIR}/OREData/ored;${ORE_BUILD_DIR}/QuantExt/qle;${ORE_BUILD_DIR}/QuantLib/ql")

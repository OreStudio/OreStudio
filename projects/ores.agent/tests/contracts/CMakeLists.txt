cmake_minimum_required(VERSION 3.25)
project(agent_contract_probe LANGUAGES CXX)

# Reuse prepared dependencies. This target never builds or starts server services.
get_filename_component(REPO_ROOT "${CMAKE_CURRENT_LIST_DIR}/../../../.." ABSOLUTE)
set(DEPENDENCY_ROOT "" CACHE PATH "Existing dependency prefix (include and lib)")
set(API_LIB_DIR "" CACHE PATH "Existing utility and platform libraries")
file(GLOB API_INCLUDES "${REPO_ROOT}/projects/ores.*/api/include")
add_executable(agent-contract-probe ../codec-probe.cpp)
target_compile_features(agent-contract-probe PRIVATE cxx_std_23)
if(MSVC)
    target_compile_options(agent-contract-probe PRIVATE /utf-8 /bigobj /EHsc /Od /Ob0)
    target_compile_definitions(agent-contract-probe PRIVATE _WIN32_WINNT=0x0A00)
endif()
target_include_directories(agent-contract-probe PRIVATE
    "${CMAKE_CURRENT_BINARY_DIR}" "${DEPENDENCY_ROOT}/include" ${API_INCLUDES}
    "${REPO_ROOT}/projects/ores.utility/include"
    "${REPO_ROOT}/projects/ores.platform/include")
foreach(LIBRARY IN ITEMS msgpack-c yyjson ores.utility ores.platform)
    string(REPLACE "." "_" LIB_ID "${LIBRARY}")
    find_library(${LIB_ID}_LIB NAMES ${LIBRARY}
        HINTS "${DEPENDENCY_ROOT}/lib" "${API_LIB_DIR}" REQUIRED)
    target_link_libraries(agent-contract-probe PRIVATE "${${LIB_ID}_LIB}")
endforeach()

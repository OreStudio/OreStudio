# -*- mode: cmake; cmake-tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
set(name "ores.ore.core")
set(lib_target_name ${name}.lib)
set(tests_binary_name ${name}.tests)
set(tests_target_name ${name}.tests)

set(files
    "domain_currency_mapper_tests.cpp"
    "domain_trade_mapper_tests.cpp"
    "hierarchy_hierarchy_builder_tests.cpp"
    "log_ore_log_parser_tests.cpp"
    "main.cpp"
    "market_fx_quote_convention_checker_tests.cpp"
    "market_market_data_parser_tests.cpp"
    "market_market_data_roundtrip_tests.cpp"
    "market_series_key_registry_tests.cpp"
    "planner_import_planner_tests.cpp"
    "scanner_directory_scanner_tests.cpp"
    "xml_bond_golden_roundtrip_tests.cpp"
    "xml_bond_mapper_roundtrip_tests.cpp"
    "xml_bond_option_golden_roundtrip_tests.cpp"
    "xml_bond_option_mapper_roundtrip_tests.cpp"
    "xml_calendaradjustment_roundtrip_tests.cpp"
    "xml_collateralbalances_roundtrip_tests.cpp"
    "xml_commodity_golden_roundtrip_tests.cpp"
    "xml_commodity_mapper_roundtrip_tests.cpp"
    "xml_composite_scripted_mapper_roundtrip_tests.cpp"
    "xml_conventions_roundtrip_tests.cpp"
    "xml_counterpartyinformation_roundtrip_tests.cpp"
    "xml_credit_golden_roundtrip_tests.cpp"
    "xml_credit_mapper_roundtrip_tests.cpp"
    "xml_creditsimulation_roundtrip_tests.cpp"
    "xml_crossassetmodel_roundtrip_tests.cpp"
    "xml_currency_config_tests.cpp"
    "xml_curveconfig_roundtrip_tests.cpp"
    "xml_equity_golden_roundtrip_tests.cpp"
    "xml_equity_mapper_roundtrip_tests.cpp"
    "xml_exotic_golden_roundtrip_tests.cpp"
    "xml_exporter_tests.cpp"
    "xml_file_io_tests.cpp"
    "xml_fx_convention_mapper_roundtrip_tests.cpp"
    "xml_fx_exotic_mapper_roundtrip_tests.cpp"
    "xml_fx_golden_roundtrip_tests.cpp"
    "xml_fx_mapper_roundtrip_tests.cpp"
    "xml_hybrid_golden_roundtrip_tests.cpp"
    "xml_importer_tests.cpp"
    "xml_inflation_golden_roundtrip_tests.cpp"
    "xml_ir_batch2_golden_roundtrip_tests.cpp"
    "xml_ir_golden_roundtrip_tests.cpp"
    "xml_ir_mapper_roundtrip_tests.cpp"
    "xml_nettingsetdefinitions_roundtrip_tests.cpp"
    "xml_ore_roundtrip_tests.cpp"
    "xml_portfolio_roundtrip_tests.cpp"
    "xml_pricingengines_roundtrip_tests.cpp"
    "xml_remaining_phases_mapper_roundtrip_tests.cpp"
    "xml_roundtrip_tests.cpp"
    "xml_scripted_golden_roundtrip_tests.cpp"
    "xml_sensitivityanalysis_roundtrip_tests.cpp"
    "xml_simulation_roundtrip_tests.cpp"
    "xml_stresstesting_roundtrip_tests.cpp"
    "xml_swaption_mapper_roundtrip_tests.cpp"
    "xml_todaysmarket_roundtrip_tests.cpp"
    "xml_trade_import_tests.cpp"
)

add_executable(${tests_target_name} ${files})
set_target_properties(${tests_target_name}
    PROPERTIES OUTPUT_NAME ${tests_binary_name})

# The xsdcpp XML parser in domain.cpp uses deep recursion (parseElement calls
# itself for each nested child element). In release mode, Clang inlines
# aggressively, increasing per-frame stack usage. Windows default stack is 1MB;
# bump to 8MB to prevent stack overflow on complex ORE XML documents.
if(WIN32)
    if(MSVC)
        # MSVC and clang-cl: /STACK is a native linker flag
        target_link_options(${tests_target_name} PRIVATE /STACK:8388608)
    else()
        # Plain clang++ on Windows uses lld; pass via -Wl,
        target_link_options(${tests_target_name} PRIVATE -Wl,/STACK:8388608)
    endif()
endif()

target_link_libraries(${tests_target_name}
    PRIVATE
        ${lib_target_name}
        ores.utility.lib
        ores.platform.lib
        ores.testing.lib
        Catch2::Catch2
        faker-cxx::faker-cxx
        ${CMAKE_THREAD_LIBS_INIT})

add_custom_target(test_${tests_target_name}
    COMMENT "Testing ${tests_target_name}" VERBATIM
    COMMAND ${ORES_TEST_ENV_CMD} $<TARGET_FILE:${tests_binary_name}> ${ORES_CATCH2_ARGS}
        -r xml::out=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-results-${tests_target_name}.xml
    WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
    DEPENDS ${tests_target_name})

add_dependencies(run_all_tests test_${tests_target_name})

add_test(NAME ${tests_target_name}
    COMMAND ${tests_binary_name} ${ORES_CATCH2_ARGS} -r xml::out=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-results-${tests_target_name}.xml
    WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})

set_tests_properties(${tests_target_name} PROPERTIES
    PROCESSORS 1
    ENVIRONMENT "${ORES_TEST_ENV}"
    ATTACHED_FILES_ON_FAIL "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-results-${tests_target_name}.xml"
)

set_property(TEST ${tests_target_name} PROPERTY USES_XML_OUTPUT ON)

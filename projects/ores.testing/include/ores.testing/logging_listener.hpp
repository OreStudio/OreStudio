/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_TESTING_LOGGING_LISTENER_HPP
#define ORES_TESTING_LOGGING_LISTENER_HPP

#include <catch2/catch_test_case_info.hpp>
#include <catch2/reporters/catch_reporter_event_listener.hpp>
#include "ores.telemetry/log/lifecycle_manager.hpp"

namespace ores::testing {

/**
 * @brief Catch2 event listener that sets up logging for each test case and
 * logs all test events to Boost.Log files.
 *
 * This listener automatically initializes the logging infrastructure before
 * each test case starts and cleans it up when the test completes. It also logs
 * all Catch2 test events (test start/end, sections, assertions) to the
 * Boost.Log file.
 *
 * The listener must be registered in each test's main.cpp file using:
 * CATCH_REGISTER_LISTENER(ores::utility::test::logging_listener)
 *
 * In your tests, use logger() to access the current test's logger:
 *
 * TEST_CASE("my_test", "[my_suite]") {
 *     BOOST_LOG_SEV(logger(), info) << "Test message";
 * }
 *
 */
class logging_listener : public Catch::EventListenerBase {
public:
    /**
     * @brief Sets the test module name from the main function
     */
    static void set_test_module_name(const std::string& module_name);

    /**
     * @brief Extracts the first tag from a Catch2 test case as the suite name.
     *
     * If no tags are present, returns "default_suite".
     */
    std::string extract_suite_name(const Catch::TestCaseInfo& testInfo);

    /**
     * @brief Extracts the module name from the test binary name.
     *
     * Assumes the binary follows the pattern "ores.*.tests"
     */
    std::string extract_module_name();

public:
    using Catch::EventListenerBase::EventListenerBase;

    void testRunStarting(Catch::TestRunInfo const& testRunInfo) override;
    void testRunEnded(Catch::TestRunStats const& testRunStats) override;
    void testCaseStarting(Catch::TestCaseInfo const& testInfo) override;
    void testCaseEnded(Catch::TestCaseStats const& testCaseStats) override;
    void assertionEnded(Catch::AssertionStats const& assertionStats) override;
    void sectionStarting(Catch::SectionInfo const& sectionInfo) override;
    void sectionEnded(Catch::SectionStats const& sectionStats) override;

private:
    std::shared_ptr<utility::log::lifecycle_manager> lifecycle_manager_;
};

}

#endif

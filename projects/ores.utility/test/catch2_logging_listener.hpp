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
#ifndef ORES_UTILITY_TEST_CATCH2_LOGGING_LISTENER_HPP
#define ORES_UTILITY_TEST_CATCH2_LOGGING_LISTENER_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <memory>
#include <string>
#include <sstream>
#include <optional>
#include <catch2/catch_test_case_info.hpp>
#include <catch2/reporters/catch_reporter_event_listener.hpp>
#include <catch2/reporters/catch_reporter_registrars.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/log/scoped_lifecycle_manager.hpp"
#include "ores.utility/log/logging_options.hpp"
#include "ores.utility/test/logging.hpp"

namespace ores::utility::test {

namespace detail {

/**
 * @brief Thread-local storage for the current test logger and lifecycle manager.
 */
struct test_logging_context {
    std::unique_ptr<ores::utility::log::scoped_lifecycle_manager> lifecycle_manager;
    std::optional<boost::log::sources::severity_channel_logger_mt<
        ores::utility::log::severity_level, std::string>> logger;
};

inline thread_local test_logging_context current_test_context;

// Global variable to store the test module name, set from main function
inline std::string test_module_name = "ores.tests"; // default fallback

/**
 * @brief Sets the test module name from the main function
 */
inline void set_test_module_name(const std::string& module_name) {
    test_module_name = module_name;
}

/**
 * @brief Extracts the first tag from a Catch2 test case as the suite name.
 *
 * If no tags are present, returns "default_suite".
 */
inline std::string extract_suite_name(const Catch::TestCaseInfo& testInfo) {
    const auto& tags = testInfo.tags;
    if (!tags.empty()) {
        // Tags in Catch2 are stored with brackets, e.g., "[suite_name]"
        std::string first_tag = static_cast<std::string>(tags[0].original);
        // Remove brackets
        if (first_tag.size() >= 2 && first_tag.front() == '[' && first_tag.back() == ']') {
            return first_tag.substr(1, first_tag.size() - 2);
        }
        return first_tag;
    }
    return "default_suite";
}

/**
 * @brief Extracts the module name from the test binary name.
 *
 * Assumes the binary follows the pattern "ores.*.tests"
 */
inline std::string extract_module_name() {
    return detail::test_module_name;
}

} // namespace detail

/**
 * @brief Catch2 event listener that sets up logging for each test case.
 *
 * This listener automatically initializes the logging infrastructure before
 * each test case starts and cleans it up when the test completes. It integrates
 * with the existing ores::utility::log infrastructure.
 *
 * Usage:
 *   Simply include this header in your test file. The listener is automatically
 *   registered via CATCH_REGISTER_LISTENER at the end of this file.
 *
 *   In your tests, use logger() to access the current test's logger:
 *
 *   TEST_CASE("my_test", "[my_suite]") {
 *       BOOST_LOG_SEV(logger(), info) << "Test message";
 *   }
 */
class catch2_logging_listener : public Catch::EventListenerBase {
public:
    using Catch::EventListenerBase::EventListenerBase;

    void testCaseStarting(Catch::TestCaseInfo const& testInfo) override {
        const std::string module = detail::extract_module_name();
        const std::string suite = detail::extract_suite_name(testInfo);
        const std::string test_name = testInfo.name;

        // Build logging options
        std::ostringstream s;
        s << "../log/" << module << "/" << suite << "/" << test_name;
        ores::utility::log::logging_options cfg;
        cfg.filename = s.str();
        cfg.severity = "trace";
        cfg.output_to_console = false;

        // Initialize logging lifecycle manager with options
        detail::current_test_context.lifecycle_manager =
            std::make_unique<ores::utility::log::scoped_lifecycle_manager>(
                std::optional<ores::utility::log::logging_options>{cfg});

        // Create logger for this test
        detail::current_test_context.logger.emplace(
            ores::utility::log::make_logger(suite));
    }

    void testCaseEnded(Catch::TestCaseStats const&) override {
        // Clean up logging context
        detail::current_test_context.logger.reset();
        detail::current_test_context.lifecycle_manager.reset();
    }
};

/**
 * @brief Returns the logger for the current test case.
 *
 * This function provides access to the thread-local logger set up by the
 * catch2_logging_listener. It should be called from within a test case.
 *
 * @return Reference to the current test's logger
 * @throws std::runtime_error if called outside of a test case context
 */
inline auto& logger() {
    if (!detail::current_test_context.logger.has_value()) {
        throw std::runtime_error(
            "logger() called outside of test case context. "
            "Ensure catch2_logging_listener.hpp is included in your test file.");
    }
    return detail::current_test_context.logger.value();
}

} // namespace ores::utility::test

// Automatically register the listener
CATCH_REGISTER_LISTENER(ores::utility::test::catch2_logging_listener)

#endif

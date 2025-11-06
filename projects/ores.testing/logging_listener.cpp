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
#include "ores.testing/logging_listener.hpp"

#include <memory>
#include <string>
#include <sstream>
#include <optional>
#include <filesystem>
#include <catch2/catch_test_case_info.hpp>
#include <catch2/reporters/catch_reporter_event_listener.hpp>
#include <catch2/reporters/catch_reporter_registrars.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/log/lifecycle_manager.hpp"
#include "ores.utility/log/logging_options.hpp"

namespace {

/**
 * @brief Thread-local storage for the current test logger and lifecycle manager.
 */
struct test_logging_context {
    std::unique_ptr<ores::utility::log::lifecycle_manager> lifecycle_manager;
    std::optional<boost::log::sources::severity_channel_logger_mt<
        ores::utility::log::severity_level, std::string>> logger;
};

thread_local test_logging_context current_test_context;

// Global variable to store the test module name, set from main function
std::string test_module_name = "ores.tests"; // default fallback

/**
 * @brief Returns the logger for the current test case.
 *
 * This function provides access to the thread-local logger set up by the
 * logging_listener. It should be called from within a test case.
 *
 * @return Reference to the current test's logger
 * @throws std::runtime_error if called outside of a test case context
 */
inline auto& logger() {
    if (!current_test_context.logger.has_value()) {
        throw std::runtime_error(
            "logger() called outside of test case context. "
            "Ensure logging_listener.hpp is included in your test file.");
    }
    return current_test_context.logger.value();
}

}

namespace ores::testing {

std::string logging_listener::
extract_suite_name(const Catch::TestCaseInfo& testInfo) {
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

std::string logging_listener::extract_module_name() {
    return test_module_name;
}

void logging_listener::set_test_module_name(const std::string& module_name) {
    test_module_name = module_name;
}

void logging_listener::testRunStarting(Catch::TestRunInfo const& /*testRunInfo*/) {
    const std::string module = extract_module_name();

    // Build logging options for test suite level logging
    std::filesystem::path log_dir = std::filesystem::path("..") / "log" / module;

    ores::utility::log::logging_options cfg;
    cfg.output_directory = log_dir;
    cfg.filename = module;
    cfg.severity = "trace";
    cfg.output_to_console = false;
    cfg.tag = "TestSuite";

    // Initialize logging lifecycle manager with options
    lifecycle_manager_ = std::make_shared<utility::log::lifecycle_manager>(
        std::optional<ores::utility::log::logging_options>{cfg});
}

void logging_listener::testRunEnded(Catch::TestRunStats const& testRunStats) {
    // This is called once at the end of the test run, but logger is already
    // cleaned up, so we'll skip logging here
}

void logging_listener::testCaseStarting(Catch::TestCaseInfo const& testInfo) {
    const std::string module = extract_module_name();
    const std::string suite = extract_suite_name(testInfo);
    const std::string test_name = testInfo.name;

    // Build logging options
    std::filesystem::path log_dir = std::filesystem::path("..") / "log" / module / suite;
    ores::utility::log::logging_options cfg;
    cfg.output_directory = log_dir;
    cfg.filename = test_name;
    cfg.severity = "trace";
    cfg.output_to_console = false;

    // Initialize logging lifecycle manager with options
    current_test_context.lifecycle_manager =
        std::make_unique<ores::utility::log::lifecycle_manager>(
            std::optional<ores::utility::log::logging_options>{cfg});

    // Create logger for this test
    current_test_context.logger.emplace(
        ores::utility::log::make_logger(suite));

    // Log test case start
    using ores::utility::log::severity_level;
    BOOST_LOG_SEV(current_test_context.logger.value(), severity_level::info)
        << "Test case starting: " << testInfo.name;
    if (!testInfo.tags.empty()) {
        std::ostringstream tags;
        tags << "  Tags: ";
        for (const auto& tag : testInfo.tags) {
            tags << tag.original << " ";
        }
        BOOST_LOG_SEV(current_test_context.logger.value(), severity_level::debug)
            << tags.str();
    }
}

void logging_listener::testCaseEnded(Catch::TestCaseStats const& testCaseStats) {
    if (!current_test_context.logger.has_value()) return;

    using ores::utility::log::severity_level;
    const auto& totals = testCaseStats.totals;

    auto log_level = totals.assertions.allOk() ? severity_level::info : severity_level::error;
    BOOST_LOG_SEV(current_test_context.logger.value(), log_level)
        << "Test case ended: " << testCaseStats.testInfo->name
        << " - " << (totals.assertions.allOk() ? "PASSED" : "FAILED");

    BOOST_LOG_SEV(current_test_context.logger.value(), severity_level::info)
        << "  Assertions: " << totals.assertions.passed << " passed, "
        << totals.assertions.failed << " failed, "
        << totals.assertions.total() << " total";

    if (testCaseStats.stdOut.size() > 0) {
        BOOST_LOG_SEV(current_test_context.logger.value(), severity_level::debug)
            << "  Standard output:\n" << testCaseStats.stdOut;
    }
    if (testCaseStats.stdErr.size() > 0) {
        BOOST_LOG_SEV(current_test_context.logger.value(), severity_level::debug)
            << "  Standard error:\n" << testCaseStats.stdErr;
    }

    // Clean up logging context
    current_test_context.logger.reset();
    current_test_context.lifecycle_manager.reset();
}

void logging_listener::assertionEnded(Catch::AssertionStats const& assertionStats) {
    if (!current_test_context.logger.has_value()) return;

    using ores::utility::log::severity_level;
    const auto& result = assertionStats.assertionResult;

    if (result.isOk()) {
        BOOST_LOG_SEV(current_test_context.logger.value(), severity_level::trace)
            << "Assertion passed: " << result.getExpression();
    } else {
        BOOST_LOG_SEV(current_test_context.logger.value(), severity_level::error)
            << "Assertion failed: " << result.getExpression()
            << " at " << result.getSourceInfo().file << ":" << result.getSourceInfo().line;
        if (result.hasMessage()) {
            BOOST_LOG_SEV(current_test_context.logger.value(), severity_level::error)
                << "  Message: " << result.getMessage();
        }
        if (result.hasExpandedExpression()) {
            BOOST_LOG_SEV(current_test_context.logger.value(), severity_level::error)
                << "  Expanded: " << result.getExpandedExpression();
        }
    }
}

void logging_listener::sectionStarting(Catch::SectionInfo const& sectionInfo) {
    if (!current_test_context.logger.has_value()) return;

    using ores::utility::log::severity_level;
    BOOST_LOG_SEV(current_test_context.logger.value(), severity_level::debug)
        << "Section starting: " << sectionInfo.name;
}

void logging_listener::sectionEnded(Catch::SectionStats const& sectionStats) {
    if (!current_test_context.logger.has_value()) return;

    using ores::utility::log::severity_level;
    BOOST_LOG_SEV(current_test_context.logger.value(), severity_level::debug)
        << "Section ended: " << sectionStats.sectionInfo.name
        << " (assertions: " << sectionStats.assertions.total() << ")";
}


}

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
#include "ores.database/service/tenant_context.hpp"
#include "ores.logging/logging_options.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.platform/environment/environment.hpp"
#include "ores.telemetry.core/domain/resource.hpp"
#include "ores.telemetry.core/log/lifecycle_manager.hpp"
#include "ores.telemetry.database/repository/telemetry_repository.hpp"
#include "ores.testing/test_database_manager.hpp"
#include <catch2/catch_test_case_info.hpp>
#include <catch2/reporters/catch_reporter_event_listener.hpp>
#include <catch2/reporters/catch_reporter_registrars.hpp>
#include <filesystem>
#include <iostream>
#include <memory>
#include <optional>
#include <sstream>
#include <string>

using namespace ores::logging;
using telemetry_lifecycle_manager = ores::telemetry::log::lifecycle_manager;
using env = ores::platform::environment::environment;

namespace {

/**
 * @brief Thread-local storage for the current test logger and lifecycle manager.
 */
struct test_logging_context {
    std::unique_ptr<telemetry_lifecycle_manager> lifecycle_mgr;
    std::optional<logger_t> logger;
};

thread_local test_logging_context current_test_context;
inline std::string_view component_name = "catch2";

std::string test_module_name = "ores.tests";

/**
 * @brief Returns true if test logging is enabled via environment variable.
 *
 * Controlled by ORES_TEST_LOG_ENABLED. Defaults to false (disabled).
 */
bool is_logging_enabled() {
    static const bool enabled =
        env::get_value_or_default("ORES_TEST_LOG_ENABLED", "false") == "true";
    return enabled;
}

/**
 * @brief Returns the log severity level from environment variable.
 *
 * Controlled by ORES_TEST_LOG_LEVEL. Defaults to "trace".
 */
std::string get_log_level() {
    static const std::string level = env::get_value_or_default("ORES_TEST_LOG_LEVEL", "trace");
    return level;
}

/**
 * @brief Returns true if console output is enabled via environment variable.
 *
 * Controlled by ORES_TEST_LOG_CONSOLE. Defaults to false.
 */
bool is_console_output_enabled() {
    static const bool enabled =
        env::get_value_or_default("ORES_TEST_LOG_CONSOLE", "false") == "true";
    return enabled;
}

/**
 * @brief Returns true if database output is enabled via environment variable.
 *
 * Controlled by ORES_TEST_LOG_DATABASE. Defaults to false.
 */
bool is_database_output_enabled() {
    static const bool enabled =
        env::get_value_or_default("ORES_TEST_LOG_DATABASE", "false") == "true";
    return enabled;
}

}

namespace ores::testing {

std::string logging_listener::extract_suite_name(const Catch::TestCaseInfo& testInfo) {
    const auto& tags = testInfo.tags;
    if (tags.empty())
        return "default_suite";
    // Catch2 strips the square brackets before it stores a tag.
    return static_cast<std::string>(tags[0].original);
}

std::string logging_listener::extract_module_name() {
    return test_module_name;
}

void logging_listener::set_test_module_name(const std::string& module_name) {
    test_module_name = module_name;
}

void logging_listener::testRunStarting(Catch::TestRunInfo const& /*testRunInfo*/) {
    // When logging is disabled, create lifecycle_manager with nullopt to
    // globally disable all Boost.Log output via core.set_logging_enabled(false)
    if (!is_logging_enabled()) {
        lifecycle_manager_ = std::make_shared<telemetry_lifecycle_manager>(std::nullopt);
        return;
    }

    const std::string module = extract_module_name();

    std::filesystem::path log_dir = std::filesystem::path("..") / "log" / module;

    logging_options cfg;
    cfg.output_directory = log_dir;
    cfg.filename = module;
    cfg.severity = get_log_level();
    cfg.output_to_console = is_console_output_enabled();
    cfg.tag = "TestSuite";

    lifecycle_manager_ =
        std::make_shared<telemetry_lifecycle_manager>(std::optional<logging_options>{cfg});
}

void logging_listener::testRunEnded(Catch::TestRunStats const& /*testRunStats*/) {
    // This is called once at the end of the test run, but logger is already
    // cleaned up, so we'll skip logging here
}

void logging_listener::testCaseStarting(Catch::TestCaseInfo const& testInfo) {
    if (!is_logging_enabled())
        return;

    const std::string module = extract_module_name();
    const std::string suite = extract_suite_name(testInfo);
    const std::string test_name = testInfo.name;

    std::filesystem::path log_dir = std::filesystem::path("..") / "log" / module / suite;
    logging_options cfg;
    cfg.output_directory = log_dir;
    cfg.filename = test_name;
    cfg.severity = get_log_level();
    cfg.output_to_console = is_console_output_enabled();

    current_test_context.lifecycle_mgr =
        std::make_unique<telemetry_lifecycle_manager>(std::optional<logging_options>{cfg});

    if (is_database_output_enabled()) {
        const auto tenant_id = test_database_manager::get_test_tenant_id_env();
        if (!tenant_id.empty()) {
            try {
                auto ctx = std::make_shared<ores::database::context>(
                    ores::database::service::tenant_context::with_tenant(
                        test_database_manager::make_context(), tenant_id));

                auto repo =
                    std::make_shared<ores::telemetry::database::repository::telemetry_repository>();

                auto resource = std::make_shared<ores::telemetry::domain::resource>(
                    ores::telemetry::domain::resource::from_environment(module, "test"));

                // The handler must not log, to avoid recursion.
                current_test_context.lifecycle_mgr->add_database_sink(
                    resource,
                    [repo, ctx](const ores::telemetry::domain::telemetry_log_entry& entry) {
                        try {
                            repo->create(*ctx, entry);
                        } catch (const std::exception& ex) {
                            std::cerr << "[Database Sink] Failed to write log: " << ex.what()
                                      << std::endl;
                        }
                    },
                    "test",
                    module + "/" + suite + "/" + test_name);
            } catch (const std::exception& e) {
                // A failed sink must not fail the test.
                std::cerr << "[Logging] Failed to set up database sink: " << e.what() << std::endl;
            }
        }
    }

    current_test_context.logger.emplace(make_logger(component_name));

    auto& lg = current_test_context.logger.value();
    BOOST_LOG_SEV(lg, info) << "Test case starting: " << testInfo.name;
    if (!testInfo.tags.empty()) {
        std::ostringstream tags;
        tags << "  Tags: ";
        for (const auto& tag : testInfo.tags)
            tags << tag.original << " ";

        BOOST_LOG_SEV(lg, debug) << tags.str();
    }
}

void logging_listener::testCaseEnded(Catch::TestCaseStats const& testCaseStats) {
    if (!current_test_context.logger.has_value())
        return;

    const auto& totals = testCaseStats.totals;

    auto log_level = totals.assertions.allOk() ? info : error;
    auto& lg = current_test_context.logger.value();
    BOOST_LOG_SEV(lg, log_level) << "Test case ended: " << testCaseStats.testInfo->name << " - "
                                 << (totals.assertions.allOk() ? "PASSED" : "FAILED");

    BOOST_LOG_SEV(lg, info) << "  Assertions: " << totals.assertions.passed << " passed, "
                            << totals.assertions.failed << " failed, " << totals.assertions.total()
                            << " total";

    if (testCaseStats.stdOut.size() > 0)
        BOOST_LOG_SEV(lg, debug) << "  Standard output:\n" << testCaseStats.stdOut;

    if (testCaseStats.stdErr.size() > 0)
        BOOST_LOG_SEV(lg, debug) << "  Standard error:\n" << testCaseStats.stdErr;

    current_test_context.logger.reset();
    current_test_context.lifecycle_mgr.reset();
}

void logging_listener::assertionEnded(Catch::AssertionStats const& assertionStats) {
    if (!current_test_context.logger.has_value())
        return;

    const auto& result = assertionStats.assertionResult;
    auto& lg = current_test_context.logger.value();

    if (result.isOk())
        BOOST_LOG_SEV(lg, trace) << "Assertion passed: " << result.getExpression();
    else {
        BOOST_LOG_SEV(lg, error) << "Assertion failed: " << result.getExpression() << " at "
                                 << result.getSourceInfo().file << ":"
                                 << result.getSourceInfo().line;
        if (result.hasMessage())
            BOOST_LOG_SEV(lg, error) << "  Message: " << result.getMessage();

        if (result.hasExpandedExpression())
            BOOST_LOG_SEV(lg, error) << "  Expanded: " << result.getExpandedExpression();
    }
}

void logging_listener::sectionStarting(Catch::SectionInfo const& sectionInfo) {
    if (!current_test_context.logger.has_value())
        return;

    auto& lg = current_test_context.logger.value();
    BOOST_LOG_SEV(lg, debug) << "Section starting: " << sectionInfo.name;
}

void logging_listener::sectionEnded(Catch::SectionStats const& sectionStats) {
    if (!current_test_context.logger.has_value())
        return;

    auto& lg = current_test_context.logger.value();
    BOOST_LOG_SEV(lg, debug) << "Section ended: " << sectionStats.sectionInfo.name
                             << " (assertions: " << sectionStats.assertions.total() << ")";
}

}

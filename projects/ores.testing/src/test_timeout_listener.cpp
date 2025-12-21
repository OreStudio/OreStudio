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
#include "ores.testing/test_timeout_listener.hpp"

#include <cstdlib>
#include <iostream>
#include "ores.platform/environment/environment.hpp"

namespace ores::testing {

using namespace ores::utility::log;

void test_timeout_listener::testCaseStarting(Catch::TestCaseInfo const& testInfo) {
    // Check for environment variable override
    using ores::platform::environment::environment;
    timeout_ = std::chrono::seconds(
        environment::get_int_value_or_default("ORES_TEST_TIMEOUT_SECONDS", 60));

    current_test_name_ = testInfo.name;
    test_start_time_ = std::chrono::steady_clock::now();
    test_running_ = true;

    BOOST_LOG_SEV(lg(), debug) << "Starting test: " << current_test_name_
                               << " (timeout: " << timeout_.count() << "s)";

    // Start watchdog thread
    watchdog_thread_ = std::thread([this]() { watchdog_thread_func(); });
}

void test_timeout_listener::testCaseEnded(Catch::TestCaseStats const& testCaseStats) {
    test_running_ = false;

    // Wait for watchdog thread to finish
    if (watchdog_thread_.joinable()) {
        watchdog_thread_.join();
    }

    auto elapsed = std::chrono::steady_clock::now() - test_start_time_;
    auto elapsed_seconds = std::chrono::duration_cast<std::chrono::seconds>(elapsed);

    BOOST_LOG_SEV(lg(), debug) << "Test completed: " << current_test_name_
                               << " (duration: " << elapsed_seconds.count() << "s)";

    // Warn if test took more than half the timeout
    if (elapsed > timeout_ / 2) {
        BOOST_LOG_SEV(lg(), warn) << "Slow test: " << current_test_name_
                                     << " took " << elapsed_seconds.count()
                                     << "s (timeout: " << timeout_.count() << "s)";
    }
}

void test_timeout_listener::watchdog_thread_func() {
    const auto check_interval = std::chrono::seconds(1);

    while (test_running_) {
        std::this_thread::sleep_for(check_interval);

        if (!test_running_) {
            return;
        }

        auto elapsed = std::chrono::steady_clock::now() - test_start_time_;
        if (elapsed > timeout_) {
            // Log to both Boost.Log and stderr to ensure visibility
            BOOST_LOG_SEV(lg(), error) << "TEST TIMEOUT: " << current_test_name_
                                       << " exceeded " << timeout_.count()
                                       << " seconds limit!";

            std::cerr << "\n\n"
                      << "========================================\n"
                      << "TEST TIMEOUT FAILURE\n"
                      << "========================================\n"
                      << "Test: " << current_test_name_ << "\n"
                      << "Timeout: " << timeout_.count() << " seconds\n"
                      << "========================================\n\n";

            // Force exit with failure status
            // This ensures the test fails immediately with a clear message
            std::_Exit(EXIT_FAILURE);
        }
    }
}

}

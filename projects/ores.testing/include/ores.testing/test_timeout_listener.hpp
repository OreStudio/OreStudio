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
#ifndef ORES_TESTING_TEST_TIMEOUT_LISTENER_HPP
#define ORES_TESTING_TEST_TIMEOUT_LISTENER_HPP

#include <atomic>
#include <chrono>
#include <thread>
#include <string>
#include <catch2/catch_test_case_info.hpp>
#include <catch2/reporters/catch_reporter_event_listener.hpp>
#include "ores.telemetry/log/make_logger.hpp"

namespace ores::testing {

/**
 * @brief Catch2 listener that enforces per-test timeouts.
 *
 * This listener tracks the duration of each test case and will terminate the
 * process if a test exceeds the configured timeout. This provides visibility
 * into which specific test is timing out, rather than just seeing the overall
 * test binary timeout from CTest.
 *
 * Default timeout is 60 seconds per test. Set the ORES_TEST_TIMEOUT_SECONDS
 * environment variable to override.
 */
class test_timeout_listener : public Catch::EventListenerBase {
private:
    inline static std::string_view logger_name =
        "ores.testing.test_timeout_listener";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using Catch::EventListenerBase::EventListenerBase;

    void testCaseStarting(Catch::TestCaseInfo const& testInfo) override;
    void testCaseEnded(Catch::TestCaseStats const& testCaseStats) override;

private:
    void watchdog_thread_func();

    std::chrono::steady_clock::time_point test_start_time_;
    std::string current_test_name_;
    std::atomic<bool> test_running_{false};
    std::thread watchdog_thread_;
    std::chrono::seconds timeout_{60};
};

}

#endif

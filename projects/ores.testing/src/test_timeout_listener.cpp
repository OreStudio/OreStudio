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
#include "ores.platform/environment/environment.hpp"
#include <cstdio>
#include <cstdlib>
#include <string>
#include <string_view>

#if defined(__unix__) || defined(__APPLE__)
#    include <csignal>
#    include <execinfo.h>
#    define ORES_TESTING_HAS_CRASH_TRACE 1
#endif

namespace ores::testing {

using namespace ores::logging;

#ifdef ORES_TESTING_HAS_CRASH_TRACE
namespace {

constexpr int crash_trace_frames = 64;

/// The signal's name, as a writer reports it.
constexpr std::string_view crash_signal_name(int signal) {
    switch (signal) {
        case SIGABRT:
            return "SIGABRT";
        case SIGSEGV:
            return "SIGSEGV";
        case SIGBUS:
            return "SIGBUS";
        case SIGFPE:
            return "SIGFPE";
        case SIGILL:
            return "SIGILL";
        default:
            return "signal";
    }
}

/// Writes the stack of the thread that took the signal, and dies as it would
/// have died. glibc reports a corrupted heap in one line and no stack, so a
/// suite that frees a block twice says where it happened once only. The
/// frames carry mangled names: pipe them through c++filt to read them.
///
/// Only the calls the dying process can still make are here: write and the
/// unwinder. Allocating, in a process that has just failed inside free,
/// would turn the report into a deadlock.
extern "C" void crash_trace_handler(int signal) {
    void* frames[crash_trace_frames];
    const auto count = ::backtrace(frames, crash_trace_frames);

    constexpr std::string_view note = "\n\nTEST FATAL SIGNAL: ";
    const auto name = crash_signal_name(signal);
    std::fwrite(note.data(), 1, note.size(), stderr);
    std::fwrite(name.data(), 1, name.size(), stderr);
    std::fputc('\n', stderr);
    std::fflush(stderr);
    ::backtrace_symbols_fd(frames, count, ::fileno(stderr));

    ::signal(signal, SIG_DFL);
    ::raise(signal);
}

/// Installs the handler once per process, on the first case that starts.
void install_crash_trace() {
    static const auto installed = [] {
        for (const auto signal : {SIGABRT, SIGSEGV, SIGBUS, SIGFPE, SIGILL})
            ::signal(signal, crash_trace_handler);
        return true;
    }();
    static_cast<void>(installed);
}

}
#endif

void test_timeout_listener::testCaseStarting(Catch::TestCaseInfo const& testInfo) {
#ifdef ORES_TESTING_HAS_CRASH_TRACE
    install_crash_trace();
#endif

    using ores::platform::environment::environment;
    timeout_ = std::chrono::seconds(environment::get_int_value_or_default(
        "ORES_TEST_TIMEOUT_SECONDS", static_cast<int>(timeout_.count())));

    // A case tagged slow reads a whole page or walks a whole corpus, so an
    // instrumented run multiplies a time a native run already spends. The
    // limit still exists, ten times further away, so a hang there is still
    // named and no other case waits longer.
    if (testInfo.tagsAsString().find("[slow]") != std::string::npos)
        timeout_ *= 10;

    current_test_name_ = testInfo.name;
    test_start_time_ = std::chrono::steady_clock::now();
    {
        const std::lock_guard<std::mutex> lock(mutex_);
        test_running_ = true;
    }

    ORES_LOG_SEV(lg(), debug) << "Starting test: " << current_test_name_
                              << " (timeout: " << timeout_.count() << "s)";

    watchdog_thread_ = std::thread([this]() { watchdog_thread_func(); });
}

void test_timeout_listener::testCaseEnded(Catch::TestCaseStats const& /*testCaseStats*/) {
    // Wake the watchdog instead of leaving it to finish its tick: joining a
    // sleeping thread made every case that ended inside a tick pay the rest of
    // it, which is a second per test on a suite of quick cases.
    {
        const std::lock_guard<std::mutex> lock(mutex_);
        test_running_ = false;
    }
    finished_.notify_all();

    if (watchdog_thread_.joinable()) {
        watchdog_thread_.join();
    }

    auto elapsed = std::chrono::steady_clock::now() - test_start_time_;
    auto elapsed_seconds = std::chrono::duration_cast<std::chrono::seconds>(elapsed);

    ORES_LOG_SEV(lg(), debug) << "Test completed: " << current_test_name_
                              << " (duration: " << elapsed_seconds.count() << "s)";

    if (elapsed > timeout_ / 2) {
        ORES_LOG_SEV(lg(), warn) << "Slow test: " << current_test_name_ << " took "
                                 << elapsed_seconds.count() << "s (timeout: " << timeout_.count()
                                 << "s)";
    }
}

void test_timeout_listener::watchdog_thread_func() {
    const auto check_interval = std::chrono::seconds(1);

    std::unique_lock<std::mutex> lock(mutex_);
    while (test_running_) {
        // The wait doubles as the sleep: it returns early when the test ends,
        // and otherwise wakes every check_interval to look at the clock.
        if (finished_.wait_for(lock, check_interval, [this]() { return !test_running_; })) {
            return;
        }

        auto elapsed = std::chrono::steady_clock::now() - test_start_time_;
        if (elapsed > timeout_) {
            ORES_LOG_SEV(lg(), error) << "TEST TIMEOUT: " << current_test_name_ << " exceeded "
                                      << timeout_.count() << " seconds limit!";

            // A memcheck run reads the streams through a pipe, and a failed
            // suite's output is not always what the run shows. C stdio puts
            // the test name in the output whatever buffering the C++ streams
            // chose.
            const auto banner = "\n\n========================================\n"
                                "TEST TIMEOUT FAILURE\n"
                                "========================================\n"
                                "Test: " +
                                current_test_name_ +
                                "\nTime limit: " + std::to_string(timeout_.count()) +
                                " seconds\n"
                                "========================================\n\n";
            std::fwrite(banner.data(), 1, banner.size(), stderr);
            std::fflush(stderr);

            // _Exit, not exit: the watchdog must not unwind a test that is
            // still running. Holding the lock is harmless, since nothing
            // unwinds.
            std::_Exit(EXIT_FAILURE);
        }
    }
}

}

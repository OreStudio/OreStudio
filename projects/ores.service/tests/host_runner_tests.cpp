/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.service/service/host_runner.hpp"
#include "ores.logging/logging_options.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_future.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cstdlib>
#include <optional>
#include <ostream>
#include <sstream>
#include <stdexcept>
#include <string>

namespace {

const std::string tags("[host_runner]");
const std::string test_suite("ores.service.tests");

// The runners only read cfg.logging, and a null optional turns logging off for
// the run, so the tests never need a configured logging_options.
struct fake_config {
    std::optional<ores::logging::logging_options> logging;
};

std::ostream& operator<<(std::ostream& os, const fake_config&) {
    return os;
}

// A parser that reports "no configuration": the run stops before the logging
// lifecycle starts.
struct deferring_parser {
    std::optional<fake_config>
    parse(const std::vector<std::string>&, std::ostream&, std::ostream&) const {
        return std::nullopt;
    }
};

struct accepting_parser {
    std::optional<fake_config>
    parse(const std::vector<std::string>&, std::ostream&, std::ostream&) const {
        return fake_config{};
    }
};

struct counting_application {
    inline static int runs = 0;

    boost::asio::awaitable<void> run(boost::asio::io_context&, const fake_config&) {
        ++runs;
        co_return;
    }
};

struct throwing_application {
    boost::asio::awaitable<void> run(boost::asio::io_context&, const fake_config&) {
        co_await boost::asio::this_coro::executor;
        throw std::runtime_error("application failed");
    }
};

}

using ores::service::service::host_runner_options;
using ores::service::service::run_host_async;
using ores::service::service::run_host_sync;

TEST_CASE("sync host run stops at the parse when the parser defers", tags) {
    std::ostringstream output;
    auto lg(ores::logging::make_logger(test_suite));
    bool ran = false;
    int before_logs = 0;
    host_runner_options opts;
    opts.on_before_log = [&before_logs] {
        ++before_logs;
    };

    const auto code = run_host_sync<deferring_parser>(
        {},
        output,
        output,
        lg,
        [](const fake_config&) -> std::optional<int> {
            return std::nullopt;
        },
        [&ran](const fake_config&) {
            ran = true;
        },
        opts);

    REQUIRE(code == EXIT_SUCCESS);
    REQUIRE_FALSE(ran);
    // Nothing ran, so the logging lifecycle was never started either.
    REQUIRE(before_logs == 0);
}

TEST_CASE("sync host run returns the early exit code without running the application", tags) {
    std::ostringstream output;
    auto lg(ores::logging::make_logger(test_suite));
    bool ran = false;

    const auto code = run_host_sync<accepting_parser>(
        {},
        output,
        output,
        lg,
        [](const fake_config&) -> std::optional<int> {
            return 7;
        },
        [&ran](const fake_config&) {
            ran = true;
        });

    REQUIRE(code == 7);
    REQUIRE_FALSE(ran);
}

TEST_CASE("sync host run runs the application once and reports success", tags) {
    std::ostringstream output;
    auto lg(ores::logging::make_logger(test_suite));
    int runs = 0;
    int successes = 0;
    int before_logs = 0;
    host_runner_options opts;
    opts.on_before_log = [&before_logs] {
        ++before_logs;
    };
    opts.on_success = [&successes] {
        ++successes;
    };

    const auto code = run_host_sync<accepting_parser>(
        {},
        output,
        output,
        lg,
        [](const fake_config&) -> std::optional<int> {
            return std::nullopt;
        },
        [&runs](const fake_config&) {
            ++runs;
        },
        opts);

    REQUIRE(code == EXIT_SUCCESS);
    REQUIRE(before_logs == 1);
    REQUIRE(runs == 1);
    REQUIRE(successes == 1);
}

TEST_CASE("sync host run rethrows what the application threw", tags) {
    std::ostringstream output;
    auto lg(ores::logging::make_logger(test_suite));

    REQUIRE_THROWS_AS(run_host_sync<accepting_parser>(
                          {},
                          output,
                          output,
                          lg,
                          [](const fake_config&) -> std::optional<int> {
                              return std::nullopt;
                          },
                          [](const fake_config&) {
                              throw std::runtime_error("run failed");
                          }),
                      std::runtime_error);
}

TEST_CASE("async host run reports success after the application runs once", tags) {
    std::ostringstream output;
    auto lg(ores::logging::make_logger(test_suite));
    boost::asio::io_context io;
    counting_application::runs = 0;
    int successes = 0;
    host_runner_options opts;
    opts.on_success = [&successes] {
        ++successes;
    };

    auto result = boost::asio::co_spawn(
        io,
        run_host_async<accepting_parser, counting_application>({}, output, output, io, lg, opts),
        boost::asio::use_future);
    io.run();

    REQUIRE(result.get() == EXIT_SUCCESS);
    REQUIRE(counting_application::runs == 1);
    REQUIRE(successes == 1);
}

TEST_CASE("async host run rethrows what the application threw", tags) {
    std::ostringstream output;
    auto lg(ores::logging::make_logger(test_suite));
    boost::asio::io_context io;

    auto result = boost::asio::co_spawn(
        io,
        run_host_async<accepting_parser, throwing_application>({}, output, output, io, lg),
        boost::asio::use_future);
    io.run();

    REQUIRE_THROWS_AS(result.get(), std::runtime_error);
}

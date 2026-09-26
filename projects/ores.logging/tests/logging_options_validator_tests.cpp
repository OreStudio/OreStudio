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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.logging/logging_exception.hpp"
#include "ores.logging/logging_options.hpp"
#include "ores.logging/logging_options_validator.hpp"
#include "ores.logging/make_logger.hpp"
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <stdexcept>
#include <string>
#include <string_view>

namespace {

const std::string_view test_suite("ores.logging.tests");
const std::string tags("[logging]");

ores::logging::logging_options make_console_only_options() {
    ores::logging::logging_options r;
    r.severity = "info";
    r.output_to_console = true;
    return r;
}

}

using ores::logging::logging_exception;
using ores::logging::logging_options;
using ores::logging::logging_options_validator;

TEST_CASE("validate_accepts_console_only_options", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const logging_options cfg(make_console_only_options());
    BOOST_LOG_SEV(lg, ores::logging::info) << cfg;

    CHECK_NOTHROW(logging_options_validator::validate(cfg));
}

TEST_CASE("validate_accepts_file_only_options", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    logging_options cfg;
    cfg.severity = "info";
    cfg.output_to_console = false;
    cfg.filename = "app.log";
    BOOST_LOG_SEV(lg, ores::logging::info) << cfg;

    CHECK_NOTHROW(logging_options_validator::validate(cfg));
}

TEST_CASE("validate_rejects_options_with_no_output", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    logging_options cfg;
    cfg.severity = "info";
    cfg.output_to_console = false;
    BOOST_LOG_SEV(lg, ores::logging::info) << cfg;

    CHECK_THROWS_AS(logging_options_validator::validate(cfg), logging_exception);
    CHECK_THROWS_WITH(logging_options_validator::validate(cfg),
                      Catch::Matchers::Equals("Must log to file and/or console"));
}

TEST_CASE("validate_rejects_output_directory_without_filename", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    logging_options cfg(make_console_only_options());
    cfg.output_directory = "log";
    BOOST_LOG_SEV(lg, ores::logging::info) << cfg;

    CHECK_THROWS_AS(logging_options_validator::validate(cfg), logging_exception);
    CHECK_THROWS_WITH(logging_options_validator::validate(cfg),
                      Catch::Matchers::Equals("Output directory supplied without a file name."));
}

TEST_CASE("validate_rejects_unknown_severity_with_invalid_argument", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    logging_options cfg(make_console_only_options());
    cfg.severity = "bogus";
    BOOST_LOG_SEV(lg, ores::logging::info) << cfg;

    CHECK_THROWS_AS(logging_options_validator::validate(cfg), std::invalid_argument);
}

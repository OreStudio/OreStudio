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
#include "ores.logging/logging_exception.hpp"
#include "ores.logging/logging_options_validator.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[logging_options_validator]");

using namespace ores::logging;

}

/*
 * The accepted side of the contract cannot be asserted here.
 * logging_options_validator::validate() returns void and reports nothing, so
 * REQUIRE_NOTHROW would pass for an empty body. Each rejection case below
 * therefore starts from a configuration the validator accepts and changes
 * exactly one field to the value the contract must refuse; the refusal is the
 * literal expectation. File-only logging, with and without a directory, has no
 * field change that the contract refuses, so it has no rejection case.
 */

TEST_CASE("validate_throws_when_no_logging_destination", tags) {
    // Console-only logging at severity "debug" is accepted; disabling the
    // console leaves no destination at all.
    logging_options opts;
    opts.severity = "debug";
    opts.output_to_console = false;

    REQUIRE_THROWS_AS(logging_options_validator::validate(opts), logging_exception);
}

TEST_CASE("validate_throws_when_directory_without_filename", tags) {
    // Console-and-file logging ("warn", "app.log" under "/tmp") is accepted;
    // removing the file name leaves a directory with no file.
    logging_options opts;
    opts.severity = "warn";
    opts.output_to_console = true;
    opts.output_directory = "/tmp";

    REQUIRE_THROWS_AS(logging_options_validator::validate(opts), logging_exception);
}

TEST_CASE("validate_throws_on_invalid_severity", tags) {
    // Console-only logging accepts trace, debug, info, warn and error; any
    // other severity string is refused.
    logging_options opts;
    opts.severity = "invalid_level";
    opts.output_to_console = true;

    REQUIRE_THROWS(logging_options_validator::validate(opts));
}

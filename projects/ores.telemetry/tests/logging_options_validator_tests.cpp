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
#include "ores.logging/logging_options_validator.hpp"
#include "ores.logging/logging_exception.hpp"

#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[logging_options_validator]");

using namespace ores::logging;

}

TEST_CASE("validate_accepts_console_only_logging", tags) {
    logging_options opts;
    opts.severity = "debug";
    opts.output_to_console = true;

    REQUIRE_NOTHROW(logging_options_validator::validate(opts));
}

TEST_CASE("validate_accepts_file_only_logging", tags) {
    logging_options opts;
    opts.severity = "info";
    opts.output_to_console = false;
    opts.filename = "app.log";
    opts.output_directory = "/tmp";

    REQUIRE_NOTHROW(logging_options_validator::validate(opts));
}

TEST_CASE("validate_accepts_console_and_file_logging", tags) {
    logging_options opts;
    opts.severity = "warn";
    opts.output_to_console = true;
    opts.filename = "app.log";
    opts.output_directory = "/tmp";

    REQUIRE_NOTHROW(logging_options_validator::validate(opts));
}

TEST_CASE("validate_throws_when_no_logging_destination", tags) {
    logging_options opts;
    opts.severity = "debug";
    opts.output_to_console = false;

    REQUIRE_THROWS_AS(logging_options_validator::validate(opts),
        logging_exception);
}

TEST_CASE("validate_throws_when_directory_without_filename", tags) {
    logging_options opts;
    opts.severity = "debug";
    opts.output_to_console = true;
    opts.output_directory = "/tmp";

    REQUIRE_THROWS_AS(logging_options_validator::validate(opts),
        logging_exception);
}

TEST_CASE("validate_throws_on_invalid_severity", tags) {
    logging_options opts;
    opts.severity = "invalid_level";
    opts.output_to_console = true;

    REQUIRE_THROWS(logging_options_validator::validate(opts));
}

TEST_CASE("validate_accepts_all_valid_severity_levels", tags) {
    const std::vector<std::string> levels = {
        "trace", "debug", "info", "warn", "error"
    };

    for (const auto& level : levels) {
        logging_options opts;
        opts.severity = level;
        opts.output_to_console = true;
        REQUIRE_NOTHROW(logging_options_validator::validate(opts));
    }
}

TEST_CASE("validate_accepts_file_without_directory", tags) {
    logging_options opts;
    opts.severity = "debug";
    opts.output_to_console = false;
    opts.filename = "app.log";

    REQUIRE_NOTHROW(logging_options_validator::validate(opts));
}

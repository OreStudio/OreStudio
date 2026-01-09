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
#include "ores.comms.analyser/config/options.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string test_suite("ores.comms.analyser.tests");
const std::string tags("[options]");

}

using namespace ores::comms::analyser::config;
using namespace ores::logging;

TEST_CASE("options_default_construction", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing options default construction";

    options sut;

    CHECK(sut.cmd == command::read);
    CHECK(sut.input_file.empty());
    CHECK_FALSE(sut.verbose);
}

TEST_CASE("options_command_read", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing options with read command";

    options sut;
    sut.cmd = command::read;
    sut.input_file = "/path/to/session.ores";
    sut.verbose = true;

    CHECK(sut.cmd == command::read);
    CHECK(sut.input_file == "/path/to/session.ores");
    CHECK(sut.verbose);
}

TEST_CASE("options_command_info", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing options with info command";

    options sut;
    sut.cmd = command::info;
    sut.input_file = "/data/recording.ores";
    sut.verbose = false;

    CHECK(sut.cmd == command::info);
    CHECK(sut.input_file == "/data/recording.ores");
    CHECK_FALSE(sut.verbose);
}

TEST_CASE("options_input_file_path", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing options with various file paths";

    options sut;

    sut.input_file = "relative/path/session.ores";
    CHECK(sut.input_file == "relative/path/session.ores");

    sut.input_file = "/absolute/path/session.ores";
    CHECK(sut.input_file == "/absolute/path/session.ores");

    sut.input_file = "./current/dir/session.ores";
    CHECK(sut.input_file == "./current/dir/session.ores");
}

TEST_CASE("options_verbose_flag", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing options verbose flag";

    options verbose_opts;
    verbose_opts.verbose = true;
    CHECK(verbose_opts.verbose);

    options quiet_opts;
    quiet_opts.verbose = false;
    CHECK_FALSE(quiet_opts.verbose);
}

TEST_CASE("command_enum_values", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing command enum values are distinct";

    CHECK(command::read != command::info);
}

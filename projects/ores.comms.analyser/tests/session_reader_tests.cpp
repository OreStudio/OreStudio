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
#include "ores.comms.analyser/domain/session_reader.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.utility/log/make_logger.hpp"

namespace {

const std::string test_suite("ores.comms.analyser.tests");
const std::string tags("[domain]");

}

using namespace ores::comms::analyser::domain;
using namespace ores::utility::log;

TEST_CASE("session_reader_nonexistent_file_returns_error", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing session_reader with nonexistent file";

    auto result = session_reader::read("/nonexistent/path/session.ores");

    REQUIRE(!result.has_value());
    REQUIRE(result.error() == ores::comms::recording::session_file_error::file_open_failed);
}

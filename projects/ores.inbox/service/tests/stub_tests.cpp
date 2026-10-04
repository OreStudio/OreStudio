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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service_stub_test.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.inbox.service/config/parser.hpp"
#include "ores.inbox.service/config/parser_exception.hpp"
#include <catch2/catch_test_macros.hpp>
#include <sstream>
#include <string>
#include <vector>

namespace {

const std::string tags("[config]");

}

using namespace ores::inbox::service::config;

TEST_CASE("parse_help_does_not_throw", tags) {
    std::ostringstream out, err;
    parser p;
    const auto r(p.parse({"--help"}, out, err));
    REQUIRE(!r);
}

TEST_CASE("parse_version_does_not_throw", tags) {
    std::ostringstream out, err;
    parser p;
    const auto r(p.parse({"--version"}, out, err));
    REQUIRE(!r);
}

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
#include "ores.marketdata.official/config/options.hpp"
#include <catch2/catch_test_macros.hpp>

using ores::marketdata::official::config::parse_source;

TEST_CASE("a_source_names_its_adapter_and_its_key", "[config][options]") {
    const auto source = parse_source("ecb-frankfurter:ecb:frankfurter:ECB");

    REQUIRE(source.has_value());
    CHECK(source->source_name == "ecb-frankfurter");
    CHECK(source->provider_code == "ecb");
    CHECK(source->adapter_code == "frankfurter");
    CHECK(source->key == "ECB");
}

TEST_CASE("a_source_without_four_parts_is_refused", "[config][options]") {
    CHECK_FALSE(parse_source("ecb-frankfurter:ecb:ECB").has_value());
    CHECK_FALSE(parse_source("ecb-frankfurter:ecb:frankfurter:ECB:extra").has_value());
    CHECK_FALSE(parse_source("ecb-frankfurter:ecb::ECB").has_value());
    CHECK_FALSE(parse_source("").has_value());
}

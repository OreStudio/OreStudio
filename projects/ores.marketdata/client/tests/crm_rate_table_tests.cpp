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
#include "ores.marketdata.client/presentation/crm_rate_table.hpp"
#include <catch2/catch_test_macros.hpp>

using ores::marketdata::client::presentation::convert_to_table;
using ores::marketdata::client::presentation::crm_rate_display_service;

namespace {

crm_rate_display_service::row make_row() {
    crm_rate_display_service::row r;
    r.crm_name = "majors";
    r.base_currency_code = "EUR";
    r.quote_currency_code = "USD";
    r.status = "fresh";
    r.as_of = "2026-07-16T10:00:00Z";
    r.display.rate_text = "1.103";
    r.display.change_text = "+0.123%";
    return r;
}

}

TEST_CASE("crm_rate_table renders the header and every row's formatted fields",
          "[crm_rate_table]") {
    const auto table = convert_to_table({make_row()});
    REQUIRE(table.find("CRM") != std::string::npos);
    REQUIRE(table.find("Pair") != std::string::npos);
    REQUIRE(table.find("majors") != std::string::npos);
    REQUIRE(table.find("EUR/USD") != std::string::npos);
    REQUIRE(table.find("1.103") != std::string::npos);
    REQUIRE(table.find("fresh") != std::string::npos);
}

TEST_CASE("crm_rate_table renders one row per input", "[crm_rate_table]") {
    auto gbpusd = make_row();
    gbpusd.base_currency_code = "GBP";
    gbpusd.display.rate_text = "1.27134";

    const auto table = convert_to_table({make_row(), gbpusd});
    REQUIRE(table.find("EUR/USD") != std::string::npos);
    REQUIRE(table.find("GBP/USD") != std::string::npos);
}

TEST_CASE("crm_rate_table renders an empty table for no rows", "[crm_rate_table]") {
    const auto table = convert_to_table({});
    REQUIRE(table.find("CRM") != std::string::npos);
}

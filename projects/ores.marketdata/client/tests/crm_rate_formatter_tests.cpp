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
#include "ores.marketdata.client/presentation/crm_rate_formatter.hpp"
#include <catch2/catch_test_macros.hpp>

using ores::marketdata::client::presentation::crm_rate_formatter;
namespace marketdata_msg = ores::marketdata::messaging;

namespace {

marketdata_msg::crm_rate_item make_item() {
    marketdata_msg::crm_rate_item item;
    item.crm_name = "majors";
    item.base_currency_code = "EUR";
    item.quote_currency_code = "USD";
    item.rate = 1.10285123;
    item.status = "fresh";
    item.as_of = "2026-07-16T10:00:00Z";
    item.inverted = false;
    return item;
}

}

TEST_CASE("crm_rate_formatter rounds rate to the convention's decimal_places",
          "[crm_rate_formatter]") {
    auto item = make_item();
    const auto display = crm_rate_formatter::format(item, 3);
    REQUIRE(display.rate_text == "1.103");
}

TEST_CASE("crm_rate_formatter falls back to a fixed default precision "
          "when no convention is available",
          "[crm_rate_formatter]") {
    auto item = make_item();
    const auto display = crm_rate_formatter::format(item, std::nullopt);
    REQUIRE(display.rate_text == "1.10285");
}

TEST_CASE("crm_rate_formatter reports an inverted-pair tooltip",
          "[crm_rate_formatter]") {
    auto item = make_item();
    item.inverted = true;
    const auto display = crm_rate_formatter::format(item, 5);
    REQUIRE(display.tooltip_text ==
        "Computed inverse (1/rate); fresh as of 2026-07-16T10:00:00Z");
}

TEST_CASE("crm_rate_formatter reports a fresh, non-inverted tooltip",
          "[crm_rate_formatter]") {
    auto item = make_item();
    const auto display = crm_rate_formatter::format(item, 5);
    REQUIRE(display.tooltip_text == "Fresh as of 2026-07-16T10:00:00Z");
}

TEST_CASE("crm_rate_formatter reports a stale tooltip and does not print a change",
          "[crm_rate_formatter]") {
    auto item = make_item();
    item.status = "stale";
    item.delta_pct = 1.5;
    const auto display = crm_rate_formatter::format(item, 5);
    REQUIRE(display.tooltip_text == "Stale as of 2026-07-16T10:00:00Z");
}

TEST_CASE("crm_rate_formatter reports an unavailable tooltip",
          "[crm_rate_formatter]") {
    auto item = make_item();
    item.status = "unavailable";
    item.as_of = "";
    const auto display = crm_rate_formatter::format(item, 5);
    REQUIRE(display.tooltip_text == "Unavailable");
}

TEST_CASE("crm_rate_formatter renders an up delta with the up glyph",
          "[crm_rate_formatter]") {
    auto item = make_item();
    item.delta_pct = 0.123;
    const auto display = crm_rate_formatter::format(item, 5);
    REQUIRE(display.change_text == "▲ +0.123%");
}

TEST_CASE("crm_rate_formatter renders a down delta with the down glyph",
          "[crm_rate_formatter]") {
    auto item = make_item();
    item.delta_pct = -0.045;
    const auto display = crm_rate_formatter::format(item, 5);
    REQUIRE(display.change_text == "▼ -0.045%");
}

TEST_CASE("crm_rate_formatter renders a placeholder when there is no delta",
          "[crm_rate_formatter]") {
    auto item = make_item();
    const auto display = crm_rate_formatter::format(item, 5);
    REQUIRE(display.change_text == "—");
}

TEST_CASE("crm_rate_formatter renders a placeholder for a negligible delta",
          "[crm_rate_formatter]") {
    auto item = make_item();
    item.delta_pct = 1e-10;
    const auto display = crm_rate_formatter::format(item, 5);
    REQUIRE(display.change_text == "—");
}

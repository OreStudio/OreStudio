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

using ores::marketdata::client::presentation::crm_rate_display;
using ores::marketdata::client::presentation::crm_rate_format_request;
using ores::marketdata::client::presentation::crm_rate_formatter;
namespace marketdata_msg = ores::marketdata::messaging;
namespace refdata_domain = ores::refdata::domain;

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

refdata_domain::currency_pair_convention make_convention(
    double pip_factor, double tick_size, int decimal_places) {
    refdata_domain::currency_pair_convention convention;
    convention.pair_code = "EUR/USD";
    convention.pip_factor = pip_factor;
    convention.tick_size = tick_size;
    convention.decimal_places = decimal_places;
    return convention;
}

std::vector<crm_rate_display> format_one(
    const marketdata_msg::crm_rate_item& item,
    std::optional<refdata_domain::currency_pair_convention> convention) {
    return crm_rate_formatter::format({crm_rate_format_request{&item, std::move(convention)}});
}

}

TEST_CASE("crm_rate_formatter rounds rate to the nearest tick and decimal_places",
          "[crm_rate_formatter]") {
    auto item = make_item();
    // pip_factor 0.0001, tick_size 1 pip -> absolute tick 0.0001;
    // 1.10285123 snaps to 1.1029 at the nearest 0.0001, shown to 3dp.
    const auto convention = make_convention(0.0001, 1.0, 3);
    const auto displays = format_one(item, convention);
    REQUIRE(displays[0].rate_text == "1.103");
}

TEST_CASE("crm_rate_formatter falls back to a fixed default precision "
          "when no convention is available",
          "[crm_rate_formatter]") {
    auto item = make_item();
    const auto displays = format_one(item, std::nullopt);
    REQUIRE(displays[0].rate_text == "1.10285");
}

TEST_CASE("crm_rate_formatter snaps to a coarser half-pip tick",
          "[crm_rate_formatter]") {
    auto item = make_item();
    item.rate = 1.10287;
    // absolute tick = 0.5 * 0.0001 = 0.00005; nearest multiple of 0.00005
    // to 1.10287 is 1.10285.
    const auto convention = make_convention(0.0001, 0.5, 5);
    const auto displays = format_one(item, convention);
    REQUIRE(displays[0].rate_text == "1.10285");
}

TEST_CASE("crm_rate_formatter derives reciprocal-preserving precision for a "
          "reversed convention",
          "[crm_rate_formatter]") {
    auto item = make_item();
    item.base_currency_code = "JPY";
    item.quote_currency_code = "AUD";
    item.rate = 0.0120481928;
    // Convention is AUD/JPY's own (decimal_places 2, ~83 magnitude); this
    // request renders the reciprocal JPY/AUD cell.
    auto convention = make_convention(0.01, 1.0, 2);
    convention.pair_code = "AUD/JPY";
    const auto displays = crm_rate_formatter::format(
        {crm_rate_format_request{&item, convention, true}});
    REQUIRE(displays[0].rate_text == "0.01205");
}

TEST_CASE("crm_rate_formatter derives reciprocal precision correctly when "
          "the reciprocal magnitude lands exactly on a power-of-ten boundary",
          "[crm_rate_formatter]") {
    auto item = make_item();
    item.base_currency_code = "XXX";
    item.quote_currency_code = "YYY";
    item.rate = 0.01;
    // direct magnitude 1/0.01 = 100 (order 2); decimal_places 2 ->
    // 5 significant figures; inverted order -2 -> 5-1-(-2) = 6 dp.
    auto convention = make_convention(0.01, 1.0, 2);
    const auto displays = crm_rate_formatter::format(
        {crm_rate_format_request{&item, convention, true}});
    REQUIRE(displays[0].rate_text == "0.010000");
}

TEST_CASE("crm_rate_formatter does not tick-snap a reversed-convention rate",
          "[crm_rate_formatter]") {
    auto item = make_item();
    item.base_currency_code = "USD";
    item.quote_currency_code = "EUR";
    item.rate = 0.90675;
    auto convention = make_convention(0.0001, 1.0, 3);
    const auto displays = crm_rate_formatter::format(
        {crm_rate_format_request{&item, convention, true}});
    REQUIRE(displays[0].rate_text == "0.9067");
}

TEST_CASE("crm_rate_formatter reports an inverted-pair tooltip",
          "[crm_rate_formatter]") {
    auto item = make_item();
    item.inverted = true;
    const auto displays = format_one(item, make_convention(0.0001, 1.0, 5));
    REQUIRE(displays[0].tooltip_text ==
        "Computed inverse (1/rate); fresh as of 2026-07-16T10:00:00Z");
}

TEST_CASE("crm_rate_formatter reports a fresh, non-inverted tooltip",
          "[crm_rate_formatter]") {
    auto item = make_item();
    const auto displays = format_one(item, make_convention(0.0001, 1.0, 5));
    REQUIRE(displays[0].tooltip_text == "Fresh as of 2026-07-16T10:00:00Z");
}

TEST_CASE("crm_rate_formatter reports a stale tooltip and does not print a change",
          "[crm_rate_formatter]") {
    auto item = make_item();
    item.status = "stale";
    item.delta_pct = 1.5;
    const auto displays = format_one(item, make_convention(0.0001, 1.0, 5));
    REQUIRE(displays[0].tooltip_text == "Stale as of 2026-07-16T10:00:00Z");
}

TEST_CASE("crm_rate_formatter reports an unavailable tooltip",
          "[crm_rate_formatter]") {
    auto item = make_item();
    item.status = "unavailable";
    item.as_of = "";
    const auto displays = format_one(item, std::nullopt);
    REQUIRE(displays[0].tooltip_text == "Unavailable");
}

TEST_CASE("crm_rate_formatter renders a positive delta with a plus sign",
          "[crm_rate_formatter]") {
    auto item = make_item();
    item.delta_pct = 0.123;
    const auto displays = format_one(item, make_convention(0.0001, 1.0, 5));
    REQUIRE(displays[0].change_text == "+0.123%");
}

TEST_CASE("crm_rate_formatter renders a negative delta with a minus sign",
          "[crm_rate_formatter]") {
    auto item = make_item();
    item.delta_pct = -0.045;
    const auto displays = format_one(item, make_convention(0.0001, 1.0, 5));
    REQUIRE(displays[0].change_text == "-0.045%");
}

TEST_CASE("crm_rate_formatter renders a placeholder when there is no delta",
          "[crm_rate_formatter]") {
    auto item = make_item();
    const auto displays = format_one(item, make_convention(0.0001, 1.0, 5));
    REQUIRE(displays[0].change_text == "-");
}

TEST_CASE("crm_rate_formatter renders a placeholder for a negligible delta",
          "[crm_rate_formatter]") {
    auto item = make_item();
    item.delta_pct = 1e-10;
    const auto displays = format_one(item, make_convention(0.0001, 1.0, 5));
    REQUIRE(displays[0].change_text == "-");
}

TEST_CASE("crm_rate_formatter formats a batch of requests in one call, "
          "preserving order",
          "[crm_rate_formatter]") {
    auto eurusd = make_item();
    auto gbpusd = make_item();
    gbpusd.base_currency_code = "GBP";
    gbpusd.rate = 1.27134;

    const auto displays = crm_rate_formatter::format({
        crm_rate_format_request{&eurusd, make_convention(0.0001, 1.0, 3)},
        crm_rate_format_request{&gbpusd, std::nullopt},
    });

    REQUIRE(displays.size() == 2);
    REQUIRE(displays[0].rate_text == "1.103");
    REQUIRE(displays[1].rate_text == "1.27134");
}

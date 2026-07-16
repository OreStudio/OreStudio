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
#include "ores.analytics.quant/domain/crm_rate_view.hpp"
#include "ores.analytics.quant/domain/rate_status.hpp"
#include "ores.analytics.quant/service/rate_delta_tracker.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <vector>

using ores::analytics::quant::domain::crm_rate_view;
using ores::analytics::quant::domain::rate_status;
using ores::analytics::quant::service::rate_delta_tracker;

namespace {

crm_rate_view make_view(std::string base,
                        std::string quote,
                        double rate,
                        rate_status status = rate_status::fresh) {
    return crm_rate_view{.base_code = std::move(base),
                         .quote_code = std::move(quote),
                         .rate = rate,
                         .status = status,
                         .as_of = {},
                         .inverted = false,
                         .delta_pct = std::nullopt};
}

} // namespace

TEST_CASE("the first observation for a pair has no delta", "[rate_delta_tracker]") {
    rate_delta_tracker tracker;
    std::vector<crm_rate_view> views = {make_view("EUR", "USD", 1.10)};

    tracker.apply(views);

    CHECK_FALSE(views[0].delta_pct.has_value());
}

TEST_CASE("a second observation computes %-change vs. the first", "[rate_delta_tracker]") {
    rate_delta_tracker tracker;

    std::vector<crm_rate_view> first = {make_view("EUR", "USD", 1.00)};
    tracker.apply(first);

    std::vector<crm_rate_view> second = {make_view("EUR", "USD", 1.05)};
    tracker.apply(second);

    REQUIRE(second[0].delta_pct.has_value());
    CHECK(*second[0].delta_pct == Catch::Approx(5.0));
}

TEST_CASE("a negative move produces a negative delta", "[rate_delta_tracker]") {
    rate_delta_tracker tracker;

    std::vector<crm_rate_view> first = {make_view("EUR", "USD", 2.00)};
    tracker.apply(first);

    std::vector<crm_rate_view> second = {make_view("EUR", "USD", 1.80)};
    tracker.apply(second);

    REQUIRE(second[0].delta_pct.has_value());
    CHECK(*second[0].delta_pct == Catch::Approx(-10.0));
}

TEST_CASE("distinct pairs are tracked independently", "[rate_delta_tracker]") {
    rate_delta_tracker tracker;

    std::vector<crm_rate_view> first = {
        make_view("EUR", "USD", 1.00),
        make_view("GBP", "USD", 2.00),
    };
    tracker.apply(first);

    std::vector<crm_rate_view> second = {
        make_view("EUR", "USD", 1.10),
        make_view("GBP", "USD", 1.90),
    };
    tracker.apply(second);

    REQUIRE(second[0].delta_pct.has_value());
    CHECK(*second[0].delta_pct == Catch::Approx(10.0));
    REQUIRE(second[1].delta_pct.has_value());
    CHECK(*second[1].delta_pct == Catch::Approx(-5.0));
}

TEST_CASE("an unavailable observation gets no delta and is not remembered",
          "[rate_delta_tracker]") {
    rate_delta_tracker tracker;

    std::vector<crm_rate_view> first = {make_view("EUR", "USD", 1.00)};
    tracker.apply(first);

    std::vector<crm_rate_view> outage = {make_view("EUR", "USD", 0.0, rate_status::unavailable)};
    tracker.apply(outage);
    CHECK_FALSE(outage[0].delta_pct.has_value());

    // The outage must not have overwritten "previous" -- delta on
    // recovery is still relative to the last *valid* reading (1.00), not
    // the unavailable 0.0 reading in between.
    std::vector<crm_rate_view> recovered = {make_view("EUR", "USD", 1.02)};
    tracker.apply(recovered);
    REQUIRE(recovered[0].delta_pct.has_value());
    CHECK(*recovered[0].delta_pct == Catch::Approx(2.0));
}

TEST_CASE("the very first observation being unavailable leaves nothing to diff against later",
          "[rate_delta_tracker]") {
    rate_delta_tracker tracker;

    std::vector<crm_rate_view> first = {make_view("EUR", "USD", 0.0, rate_status::unavailable)};
    tracker.apply(first);
    CHECK_FALSE(first[0].delta_pct.has_value());

    std::vector<crm_rate_view> second = {make_view("EUR", "USD", 1.00)};
    tracker.apply(second);
    CHECK_FALSE(second[0].delta_pct.has_value());
}

TEST_CASE("an inverted pair is tracked by its own (base, quote) display key",
          "[rate_delta_tracker]") {
    rate_delta_tracker tracker;

    crm_rate_view v1 = make_view("CAD", "EUR", 1.0 / 1.50);
    v1.inverted = true;
    std::vector<crm_rate_view> first = {v1};
    tracker.apply(first);

    crm_rate_view v2 = make_view("CAD", "EUR", 1.0 / 1.45);
    v2.inverted = true;
    std::vector<crm_rate_view> second = {v2};
    tracker.apply(second);

    REQUIRE(second[0].delta_pct.has_value());
    const double expected = ((1.0 / 1.45) - (1.0 / 1.50)) / (1.0 / 1.50) * 100.0;
    CHECK(*second[0].delta_pct == Catch::Approx(expected));
}

TEST_CASE("repeated identical rates produce a zero delta, not a missing one",
          "[rate_delta_tracker]") {
    rate_delta_tracker tracker;

    std::vector<crm_rate_view> first = {make_view("EUR", "USD", 1.10)};
    tracker.apply(first);

    std::vector<crm_rate_view> second = {make_view("EUR", "USD", 1.10)};
    tracker.apply(second);

    REQUIRE(second[0].delta_pct.has_value());
    CHECK(*second[0].delta_pct == Catch::Approx(0.0));
}

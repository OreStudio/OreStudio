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
#include "ores.marketdata.client/presentation/crm_rate_display_service.hpp"
#include <catch2/catch_test_macros.hpp>
#include <unordered_map>

using ores::marketdata::client::crm_client;
using ores::marketdata::client::presentation::crm_rate_display_service;
namespace marketdata_msg = ores::marketdata::messaging;
namespace refdata_domain = ores::refdata::domain;

namespace {

marketdata_msg::crm_rate_item make_item(std::string base, std::string quote, double rate) {
    marketdata_msg::crm_rate_item item;
    item.crm_name = "majors";
    item.base_currency_code = std::move(base);
    item.quote_currency_code = std::move(quote);
    item.rate = rate;
    item.status = "fresh";
    item.as_of = "2026-07-16T10:00:00Z";
    item.reciprocal = false;
    return item;
}

refdata_domain::currency_pair_convention make_convention(std::string pair_code,
                                                         int decimal_places) {
    refdata_domain::currency_pair_convention convention;
    convention.pair_code = std::move(pair_code);
    convention.pip_factor = 0.0001;
    convention.tick_size = 1.0;
    convention.decimal_places = decimal_places;
    return convention;
}

/// Builds a service backed by canned in-memory data -- no NATS involved.
crm_rate_display_service make_service(
    std::vector<marketdata_msg::crm_rate_item> rates,
    std::unordered_map<std::string, refdata_domain::currency_pair_convention> conventions) {
    auto rates_fn = [rates = std::move(rates)](
                        const std::string&, const std::string&, bool) -> crm_client::rates_result {
        return {.success = true, .error = {}, .rates = rates};
    };
    auto lookup_fn =
        [conventions = std::move(conventions)](
            const std::string&,
            const std::string& key) -> std::optional<refdata_domain::currency_pair_convention> {
        const auto it = conventions.find(key);
        if (it == conventions.end())
            return std::nullopt;
        return it->second;
    };
    return crm_rate_display_service(std::move(rates_fn), std::move(lookup_fn));
}

}

TEST_CASE("crm_rate_display_service formats every fetched rate into a row",
          "[crm_rate_display_service]") {
    auto service =
        make_service({make_item("EUR", "USD", 1.10285), make_item("GBP", "USD", 1.27134)},
                     {{"EUR/USD", make_convention("EUR/USD", 3)}});

    const auto result = service.rates("tenant-1", "party-1", "majors", false);

    REQUIRE(result.success);
    REQUIRE(result.rows.size() == 2);
    REQUIRE(result.rows[0].base_currency_code == "EUR");
    REQUIRE(result.rows[0].display.rate_text == "1.103");
    // No convention for GBP/USD -- formatter's own default precision.
    REQUIRE(result.rows[1].display.rate_text == "1.27134");
}

TEST_CASE("crm_rate_display_service resolves a convention stored in the "
          "reverse pair-code direction, deriving reciprocal precision "
          "rather than reusing decimal_places verbatim",
          "[crm_rate_display_service]") {
    // Cell shows USD/EUR but the convention is only stored as EUR/USD
    // (decimal_places 3, calibrated for EUR/USD's own ~1.1 magnitude).
    auto service = make_service({make_item("USD", "EUR", 0.90675)},
                                {{"EUR/USD", make_convention("EUR/USD", 3)}});

    const auto result = service.rates("tenant-1", "party-1", "majors", false);

    REQUIRE(result.success);
    REQUIRE(result.rows.size() == 1);
    REQUIRE(result.rows[0].display.rate_text == "0.9067");
}

TEST_CASE("crm_rate_display_service preserves significant figures across a "
          "reversed convention for a JPY-magnitude pair",
          "[crm_rate_display_service]") {
    // Cell shows JPY/AUD but the convention is only stored as AUD/JPY
    // (decimal_places 2, calibrated for AUD/JPY's own ~83 magnitude).
    // Reusing decimal_places 2 verbatim would collapse every such cell to
    // "0.01"; the service must derive precision from the reciprocal
    // magnitude instead.
    auto service = make_service({make_item("JPY", "AUD", 0.0120481928)},
                                {{"AUD/JPY", make_convention("AUD/JPY", 2)}});

    const auto result = service.rates("tenant-1", "party-1", "majors", false);

    REQUIRE(result.success);
    REQUIRE(result.rows.size() == 1);
    REQUIRE(result.rows[0].display.rate_text == "0.01205");
}

TEST_CASE("crm_rate_display_service propagates a rates-source failure without formatting",
          "[crm_rate_display_service]") {
    auto rates_fn = [](const std::string&, const std::string&, bool) -> crm_client::rates_result {
        return {.success = false, .error = "unauthorized", .rates = {}};
    };
    auto lookup_fn =
        [](const std::string&,
           const std::string&) -> std::optional<refdata_domain::currency_pair_convention> {
        return std::nullopt;
    };
    crm_rate_display_service service(std::move(rates_fn), std::move(lookup_fn));

    const auto result = service.rates("tenant-1", "party-1", "majors", false);

    REQUIRE_FALSE(result.success);
    REQUIRE(result.error == "unauthorized");
    REQUIRE(result.rows.empty());
}

TEST_CASE("crm_rate_display_service passes through status/reciprocal/delta metadata",
          "[crm_rate_display_service]") {
    auto item = make_item("EUR", "USD", 1.10285);
    item.status = "stale";
    item.reciprocal = true;
    item.delta_pct = -0.5;
    auto service = make_service({item}, {});

    const auto result = service.rates("tenant-1", "party-1", "majors", false);

    REQUIRE(result.success);
    REQUIRE(result.rows[0].status == "stale");
    REQUIRE(result.rows[0].reciprocal);
    REQUIRE(result.rows[0].delta_pct.has_value());
    REQUIRE(result.rows[0].display.tooltip_text == "Stale - 2026-07-16T10:00:00Z");
}

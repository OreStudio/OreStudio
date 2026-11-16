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
#include "ores.marketdata.service/app/pair_levels.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <map>
#include <string>
#include <utility>
#include <vector>

namespace {

using namespace std::chrono;
using ores::marketdata::messaging::get_fixing_window_response;
using ores::marketdata::messaging::get_pair_levels_request;
using ores::marketdata::service::app::compose_pair_levels;
using ores::marketdata::service::app::party_fixing_sources;
using ores::utility::domain::outcome;

const std::string tag("[pair_levels]");

year_month_day day_of(int d) {
    return year_month_day{year{2026}, month{3}, day{static_cast<unsigned>(d)}};
}

/// Fixings by item: each a list of (day, value).
using fixings = std::map<std::string, std::vector<std::pair<int, std::string>>>;

auto reader_of(const fixings& held) {
    return [held](const std::string&, const std::string& item) {
        get_fixing_window_response out;
        const auto found = held.find(item);
        if (found == held.end())
            return out;
        for (const auto& [d, value] : found->second) {
            out.dates.push_back(day_of(d));
            out.values.push_back(value);
        }
        return out;
    };
}

/// One publisher quoting every currency against the first one, under the names given.
party_fixing_sources one_source(const std::string& provider,
                                const std::string& base,
                                const std::vector<std::string>& currencies,
                                int precedence = 20) {
    party_fixing_sources out;
    out.sources.push_back({.provider_code = provider,
                           .base_code = base,
                           .precedence = precedence,
                           .currencies = currencies});
    for (const auto& c : currencies)
        out.items.emplace(std::pair{provider, c}, provider + ":" + c);
    return out;
}

get_pair_levels_request pair(const std::string& base, const std::string& quote) {
    return {.party_id = "p",
            .base_currency_code = base,
            .quote_currency_code = quote,
            .to = day_of(10),
            .days = 10};
}

}

TEST_CASE("a_cross_is_composed_each_day_through_the_base_its_source_quotes_against", tag) {
    const auto party = one_source("ecb", "EUR", {"USD", "JPY"});
    const auto read =
        reader_of({{"ecb:USD", {{2, "1.10"}, {3, "1.20"}}}, {"ecb:JPY", {{2, "165"}, {3, "168"}}}});

    const auto r = compose_pair_levels(party, pair("USD", "JPY"), read);

    REQUIRE(r.result.outcome == outcome::ok);
    CHECK(r.provider_code == "ecb");
    CHECK(r.pivot_currency_code == "EUR");
    REQUIRE(r.dates == std::vector{day_of(2), day_of(3)});
    REQUIRE(r.levels.size() == 2);
    CHECK(r.levels[0] == Catch::Approx(165.0 / 1.10).epsilon(1e-12));
    CHECK(r.levels[1] == Catch::Approx(168.0 / 1.20).epsilon(1e-12));
}

TEST_CASE("a_pair_against_the_base_reads_its_one_fixing_either_way_round", tag) {
    const auto party = one_source("ecb", "EUR", {"USD"});
    const auto read = reader_of({{"ecb:USD", {{2, "1.25"}}}});

    const auto direct = compose_pair_levels(party, pair("EUR", "USD"), read);
    const auto inverse = compose_pair_levels(party, pair("USD", "EUR"), read);

    REQUIRE(direct.result.outcome == outcome::ok);
    REQUIRE(inverse.result.outcome == outcome::ok);
    REQUIRE(direct.levels.size() == 1);
    REQUIRE(inverse.levels.size() == 1);
    CHECK(direct.levels[0] == Catch::Approx(1.25).epsilon(1e-12));
    CHECK(inverse.levels[0] == Catch::Approx(0.8).epsilon(1e-12));
}

TEST_CASE("a_day_only_one_side_was_fixed_on_has_no_level", tag) {
    const auto party = one_source("ecb", "EUR", {"USD", "JPY"});
    const auto read = reader_of({{"ecb:USD", {{2, "1.10"}, {3, "1.20"}, {4, "1.30"}}},
                                 {"ecb:JPY", {{2, "165"}, {4, "170"}}}});

    const auto r = compose_pair_levels(party, pair("USD", "JPY"), read);

    REQUIRE(r.result.outcome == outcome::ok);
    CHECK(r.dates == std::vector{day_of(2), day_of(4)});
    REQUIRE(r.levels.size() == 2);
    CHECK(r.levels[1] == Catch::Approx(170.0 / 1.30).epsilon(1e-12));
}

TEST_CASE("a_pair_only_a_source_read_later_quotes_is_read_from_that_source", tag) {
    auto party = one_source("first", "AAA", {"BBB", "CCC"}, 20);
    const auto second = one_source("second", "BBB", {"DDD"}, 21);
    party.sources.push_back(second.sources.front());
    party.items.insert(second.items.begin(), second.items.end());
    const auto read = reader_of(
        {{"first:BBB", {{2, "2.0"}}}, {"first:CCC", {{2, "3.0"}}}, {"second:DDD", {{2, "900"}}}});

    const auto r = compose_pair_levels(party, pair("BBB", "DDD"), read);

    REQUIRE(r.result.outcome == outcome::ok);
    CHECK(r.provider_code == "second");
    CHECK(r.pivot_currency_code == "BBB");
    REQUIRE(r.levels.size() == 1);
    CHECK(r.levels[0] == Catch::Approx(900.0).epsilon(1e-12));
}

TEST_CASE("a_pair_no_source_can_express_is_refused_by_name", tag) {
    const auto party = one_source("ecb", "EUR", {"USD"});

    const auto r = compose_pair_levels(party, pair("USD", "XYZ"), reader_of({}));

    CHECK(r.result.outcome == outcome::missing);
    CHECK(r.result.code == "pair_unserved");
    CHECK(r.result.message.find("USD/XYZ") != std::string::npos);
    CHECK(r.levels.empty());
}

TEST_CASE("a_refusal_of_the_fixings_read_is_the_refusal_of_the_pair", tag) {
    const auto party = one_source("ecb", "EUR", {"USD"});
    const auto refused = [](const std::string&, const std::string&) {
        get_fixing_window_response out;
        out.result = {
            .outcome = outcome::denied, .code = "party_unseen", .message = "no", .fields = {}};
        return out;
    };

    const auto r = compose_pair_levels(party, pair("EUR", "USD"), refused);

    CHECK(r.result.outcome == outcome::denied);
    CHECK(r.result.code == "party_unseen");
}

TEST_CASE("a_pair_of_one_currency_is_refused", tag) {
    const auto party = one_source("ecb", "EUR", {"USD"});

    const auto r = compose_pair_levels(party, pair("USD", "USD"), reader_of({}));

    CHECK(r.result.outcome == outcome::invalid);
}

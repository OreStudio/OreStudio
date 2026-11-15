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
#include "ores.analytics.quant/service/topology_builder.hpp"
#include "ores.analytics.quant/service/topology_from_sources.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

using ores::analytics::quant::domain::ccy_pair_input;
using ores::analytics::quant::service::fixing_source;
using ores::analytics::quant::service::topology_builder;
using ores::analytics::quant::service::topology_from_sources;

namespace {

bool holds(const std::vector<ccy_pair_input>& pairs,
           const std::string& base,
           const std::string& quote) {
    return std::ranges::any_of(pairs, [&](const ccy_pair_input& p) {
        return p.base_code == base && p.quote_code == quote;
    });
}

}

TEST_CASE("the_source_resolution_reads_first_anchors_the_topology", "[topology_from_sources]") {
    // Two publishers that both quote the same currencies against their own base,
    // written so that the alphabetically later code is the one ranked first: the
    // choice must follow the rank, not the name.
    const fixing_source later{"zz-later", "EUR", 20, {"USD", "JPY", "GBP"}};
    const fixing_source earlier{"aa-earlier", "USD", 10, {"EUR", "JPY", "GBP"}};

    const auto chosen = topology_from_sources({later, earlier});
    REQUIRE(chosen);
    CHECK(chosen->provider_code == "aa-earlier");
    CHECK(chosen->pivot_code == "USD");
    REQUIRE(chosen->pairs.size() == 3);
    CHECK(holds(chosen->pairs, "USD", "EUR"));
    CHECK(holds(chosen->pairs, "USD", "JPY"));
    CHECK(holds(chosen->pairs, "USD", "GBP"));
    CHECK(std::ranges::all_of(chosen->pairs, [](const ccy_pair_input& p) { return p.is_driver; }));
}

TEST_CASE("an_equal_rank_is_broken_by_the_provider_code", "[topology_from_sources]") {
    const fixing_source beta{"beta", "USD", 15, {"EUR"}};
    const fixing_source alpha{"alpha", "EUR", 15, {"USD"}};

    for (const auto& sources : {std::vector{beta, alpha}, std::vector{alpha, beta}}) {
        const auto chosen = topology_from_sources(sources);
        REQUIRE(chosen);
        CHECK(chosen->provider_code == "alpha");
        CHECK(chosen->pivot_code == "EUR");
    }
}

TEST_CASE("a_source_that_quotes_only_its_own_base_is_passed_over", "[topology_from_sources]") {
    const fixing_source empty{"orphan", "EUR", 1, {"EUR"}};
    const fixing_source usable{"usable", "USD", 30, {"EUR"}};

    const auto chosen = topology_from_sources({empty, usable});
    REQUIRE(chosen);
    CHECK(chosen->provider_code == "usable");
    REQUIRE(chosen->pairs.size() == 1);
    CHECK(holds(chosen->pairs, "USD", "EUR"));
}

TEST_CASE("a_currency_named_twice_appears_once", "[topology_from_sources]") {
    const fixing_source source{"ecb", "EUR", 20, {"USD", "USD", "JPY", "USD"}};

    const auto chosen = topology_from_sources({source});
    REQUIRE(chosen);
    REQUIRE(chosen->pairs.size() == 2);
    CHECK(holds(chosen->pairs, "EUR", "USD"));
    CHECK(holds(chosen->pairs, "EUR", "JPY"));
}

TEST_CASE("the_identity_a_source_states_is_all_the_topology_knows", "[topology_from_sources]") {
    // The same shape under names the code has never met. If any currency or
    // publisher were written into the source rather than supplied, this would
    // differ from the case above.
    const fixing_source source{"test", "XYZ", 20, {"AAA", "BBB"}};

    const auto chosen = topology_from_sources({source});
    REQUIRE(chosen);
    CHECK(chosen->provider_code == "test");
    CHECK(chosen->pivot_code == "XYZ");
    REQUIRE(chosen->pairs.size() == 2);
    CHECK(holds(chosen->pairs, "XYZ", "AAA"));
    CHECK(holds(chosen->pairs, "XYZ", "BBB"));
}

TEST_CASE("no_source_at_all_is_refused", "[topology_from_sources]") {
    const auto chosen = topology_from_sources({});
    REQUIRE_FALSE(chosen);
    CHECK(chosen.error().find("No fixing source") != std::string::npos);
}

TEST_CASE("a_source_with_no_base_is_refused_and_named", "[topology_from_sources]") {
    const fixing_source source{"imf", "", 21, {"CLP"}};

    const auto chosen = topology_from_sources({source});
    REQUIRE_FALSE(chosen);
    CHECK(chosen.error().find("imf") != std::string::npos);
}

TEST_CASE("a_source_that_quotes_nothing_is_refused_and_named", "[topology_from_sources]") {
    const fixing_source empty{"imf", "USD", 21, {}};

    const auto chosen = topology_from_sources({empty});
    REQUIRE_FALSE(chosen);
    CHECK(chosen.error().find("imf") != std::string::npos);
    CHECK(chosen.error().find("USD") != std::string::npos);
}

TEST_CASE("the_star_a_source_states_builds_and_reaches_every_currency", "[topology_from_sources]") {
    const fixing_source source{"ecb", "EUR", 20, {"USD", "JPY", "GBP", "CHF"}};
    const auto chosen = topology_from_sources({source});
    REQUIRE(chosen);

    const auto built = topology_builder::build(chosen->pairs, chosen->pivot_code, {});
    CHECK(built.pivot() == *built.currency_id_for("EUR"));
    CHECK(built.vertex_count() == 5);
    for (const auto* code : {"USD", "JPY", "GBP", "CHF"}) {
        const auto id = built.currency_id_for(code);
        REQUIRE(id);
        CHECK(built.path_to_pivot(*id).size() == 1);
    }
}

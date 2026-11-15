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
#include "ores.iam.core/messaging/party_surface_blueprints.hpp"
#include "ores.testing/fx_vol_surface_fixture.hpp"
#include "ores.testing/uuid_of.hpp"
#include "party_blueprint_fixture.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

using ores::iam::messaging::party_surface_holdings;
using ores::iam::messaging::plan_party_surfaces;
using ores::iam::messaging::system_surface_blueprints;
using ores::marketdata::domain::market_series;

using ores::iam::tests::market_series_row;
using ores::iam::tests::system_party;
using ores::testing::uuid_of;

const auto acme_uk = uuid_of("00000000-0000-4000-8000-0000000000a2");
const auto blueprint_id = uuid_of("00000000-0000-4000-8000-0000000000c1");
const auto output_id = uuid_of("00000000-0000-4000-8000-0000000000d1");
const auto quotes_id = uuid_of("00000000-0000-4000-8000-0000000000e1");
const std::string quotes_uri =
    "oresmd://fx/EUR?type=series&instrument=fx_option&quote=rate_lnvol&ccy=USD";

system_surface_blueprints eurusd() {
    const auto rows =
        ores::testing::eur_usd_ssvi_surface(system_party, quotes_id, {"1M"}, {"ATM", "25RR"});
    system_surface_blueprints system;
    system.blueprints = {rows.blueprint};
    system.pillars = rows.pillars;
    system.sources = rows.sources;
    system.series.push_back(
        market_series_row(output_id,
                          system_party,
                          "oresmd://fx/EUR?type=series&instrument=fx_vol_surface"
                          "&quote=rate_lnvol&ccy=USD&surface_id=EURUSD_SSVI",
                          "derived"));
    system.series.push_back(market_series_row(quotes_id, system_party, quotes_uri, "derived"));
    return system;
}

}

TEST_CASE("a party's surface blueprint reads its own delivered volatility quotes",
          "[party_surfaces]") {
    const auto own_quotes = uuid_of("00000000-0000-4000-8000-0000000000a9");
    party_surface_holdings acme;
    acme.party_id = acme_uk;
    acme.series = {market_series_row(own_quotes, acme_uk, quotes_uri, "derived")};

    const auto plan = plan_party_surfaces(eurusd(), acme);

    REQUIRE(plan.waiting.empty());
    REQUIRE(plan.blueprints_to_write.size() == 1);
    const auto& b = plan.blueprints_to_write.front();
    CHECK(b.party_id == acme_uk);
    CHECK(b.name == "EURUSD_SSVI");
    CHECK(b.id != blueprint_id);
    REQUIRE(plan.pillars_to_write.size() == 2);
    for (const auto& p : plan.pillars_to_write)
        CHECK(p.blueprint_id == b.id);
    REQUIRE(plan.sources_to_write.size() == 2);
    for (const auto& s : plan.sources_to_write) {
        CHECK(s.party_id == acme_uk);
        CHECK(s.market_series_id == own_quotes);
    }
    REQUIRE(plan.series_to_write.size() == 1);
    CHECK(plan.series_to_write.front().id == b.output_series_id);
    CHECK(plan.series_to_write.front().party_id == acme_uk);
    CHECK(plan.series_to_write.front().derivation_kind == "OBSERVED");
}

TEST_CASE("a surface whose quotes have not reached the party waits and writes nothing",
          "[party_surfaces]") {
    party_surface_holdings acme;
    acme.party_id = acme_uk;

    const auto plan = plan_party_surfaces(eurusd(), acme);

    CHECK(plan.blueprints_to_write.empty());
    CHECK(plan.pillars_to_write.empty());
    CHECK(plan.sources_to_write.empty());
    CHECK(plan.series_to_write.empty());
    REQUIRE(plan.waiting.size() == 1);
    CHECK(plan.waiting.front() == "EURUSD_SSVI: no series of derived " + quotes_uri);
}

TEST_CASE("a party that already holds its surface blueprint is planned no row twice",
          "[party_surfaces]") {
    party_surface_holdings acme;
    acme.party_id = acme_uk;
    acme.series = {market_series_row(
        uuid_of("00000000-0000-4000-8000-0000000000a9"), acme_uk, quotes_uri, "derived")};
    const auto first = plan_party_surfaces(eurusd(), acme);
    REQUIRE(first.blueprints_to_write.size() == 1);

    acme.blueprint_ids.insert(first.blueprints_to_write.front().id);
    const auto second = plan_party_surfaces(eurusd(), acme);

    CHECK(second.blueprints_to_write.empty());
    CHECK(second.pillars_to_write.empty());
    CHECK(second.sources_to_write.empty());
    CHECK(second.ready == std::vector<std::string>{"EURUSD_SSVI"});
}

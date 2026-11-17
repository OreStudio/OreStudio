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
#include "ores.marketdata.core/messaging/party_curve_blueprints.hpp"
#include "ores.testing/uuid_of.hpp"
#include "party_blueprint_fixture.hpp"
#include "test_suite.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

using ores::marketdata::messaging::party_copy_id;
using ores::marketdata::messaging::party_curve_holdings;
using ores::marketdata::messaging::plan_party_curves;
using ores::marketdata::messaging::curve_blueprints;
using ores::marketdata::domain::market_series;
using ores::refdata::domain::ir_curve_bootstrap_config;
using ores::refdata::domain::ir_curve_bootstrap_pillar;

using ores::marketdata::tests::market_series_row;
using ores::marketdata::tests::system_party;
using ores::testing::uuid_of;

const auto acme_us = uuid_of("00000000-0000-4000-8000-0000000000a1");
const auto eur_config = uuid_of("00000000-0000-4000-8000-0000000000c1");
const auto eur_output = uuid_of("00000000-0000-4000-8000-0000000000d1");
const auto eur_quotes = uuid_of("00000000-0000-4000-8000-0000000000e1");
const std::string eur_quotes_uri =
    "oresmd://ir/EUR?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D";

curve_blueprints eur_estr() {
    curve_blueprints system;
    ir_curve_bootstrap_config config;
    config.id = eur_config;
    config.party_id = system_party;
    config.name = "EUR-ESTR";
    config.currency_code = "EUR";
    config.output_series_id = eur_output;
    system.configs.push_back(config);
    for (int i = 0; i < 2; ++i) {
        ir_curve_bootstrap_pillar p;
        p.id = uuid_of("00000000-0000-4000-8000-0000000000f" + std::to_string(i));
        p.party_id = system_party;
        p.bootstrap_config_id = eur_config;
        p.sequence_index = i;
        p.end_tenor_code = i == 0 ? "3M" : "1Y";
        p.market_series_id = eur_quotes;
        system.pillars.push_back(p);
    }
    auto output = market_series_row(
        eur_output,
        system_party,
        "oresmd://ir/EUR?type=series&instrument=discount&quote=rate&curve_id=EUR-ESTR",
        "ore",
        "yield");
    output.derivation_kind = "IR_CURVE_BOOTSTRAP";
    output.derivation_config_id = eur_config;
    output.derivation_config_version = 1;
    system.series.push_back(output);
    system.series.push_back(
        market_series_row(eur_quotes, system_party, eur_quotes_uri, "bloomberg", "yield"));
    return system;
}

}

TEST_CASE("a party's copy of a curve reads its own series of the same item and provider",
          "[party_curves]") {
    auto lg(make_logger(test_suite));

    const auto own_quotes = uuid_of("00000000-0000-4000-8000-0000000000b1");
    party_curve_holdings acme;
    acme.party_id = acme_us;
    acme.series = {market_series_row(own_quotes, acme_us, eur_quotes_uri, "lseg", "yield"),
                   market_series_row(uuid_of("00000000-0000-4000-8000-0000000000b2"),
                                     acme_us,
                                     eur_quotes_uri,
                                     "bloomberg",
                                     "yield")};

    const auto plan = plan_party_curves(eur_estr(), acme);

    REQUIRE(plan.waiting.empty());
    REQUIRE(plan.configs_to_write.size() == 1);
    const auto& config = plan.configs_to_write.front();
    CHECK(config.party_id == acme_us);
    CHECK(config.name == "EUR-ESTR");
    CHECK(config.id != eur_config);
    REQUIRE(plan.pillars_to_write.size() == 2);
    for (const auto& p : plan.pillars_to_write) {
        CHECK(p.party_id == acme_us);
        CHECK(p.bootstrap_config_id == config.id);
        CHECK(p.market_series_id == uuid_of("00000000-0000-4000-8000-0000000000b2"));
    }
    REQUIRE(plan.series_to_write.size() == 1);
    const auto& output = plan.series_to_write.front();
    CHECK(output.id == config.output_series_id);
    CHECK(output.party_id == acme_us);
    CHECK(output.provider_code == "ore");
    // The copy is the party's own bootstrap of that curve, so it carries the system's
    // kind and the party's config, never the observed label.
    CHECK(output.derivation_kind == "IR_CURVE_BOOTSTRAP");
    CHECK(output.derivation_config_id == config.id);
    // The series table refuses a derived series that names version 0, and the party's
    // config is new, so it is stored as version 1.
    CHECK(output.derivation_config_version == 1);
    REQUIRE(plan.ready.size() == 1);
    CHECK(plan.ready.front().config_id == config.id);
}

TEST_CASE("a curve whose quotes have not reached the party waits and writes nothing",
          "[party_curves]") {
    auto lg(make_logger(test_suite));

    party_curve_holdings acme;
    acme.party_id = acme_us;
    acme.series = {market_series_row(uuid_of("00000000-0000-4000-8000-0000000000b1"),
                                     acme_us,
                                     eur_quotes_uri,
                                     "lseg",
                                     "yield"),
                   market_series_row(uuid_of("00000000-0000-4000-8000-0000000000b3"),
                                     system_party,
                                     eur_quotes_uri,
                                     "bloomberg",
                                     "yield")};

    const auto plan = plan_party_curves(eur_estr(), acme);

    CHECK(plan.configs_to_write.empty());
    CHECK(plan.pillars_to_write.empty());
    CHECK(plan.series_to_write.empty());
    CHECK(plan.ready.empty());
    REQUIRE(plan.waiting.size() == 1);
    CHECK(plan.waiting.front() == "EUR-ESTR: no series of bloomberg " + eur_quotes_uri);
}

TEST_CASE("a party that already holds its copy is planned no row twice", "[party_curves]") {
    auto lg(make_logger(test_suite));

    party_curve_holdings acme;
    acme.party_id = acme_us;
    acme.series = {market_series_row(uuid_of("00000000-0000-4000-8000-0000000000b2"),
                                     acme_us,
                                     eur_quotes_uri,
                                     "bloomberg",
                                     "yield")};
    const auto first = plan_party_curves(eur_estr(), acme);
    REQUIRE(first.configs_to_write.size() == 1);

    acme.config_ids.insert(first.configs_to_write.front().id);
    acme.series.push_back(first.series_to_write.front());
    const auto second = plan_party_curves(eur_estr(), acme);

    CHECK(second.configs_to_write.empty());
    CHECK(second.pillars_to_write.empty());
    CHECK(second.series_to_write.empty());
    REQUIRE(second.ready.size() == 1);
    CHECK(second.ready.front().config_id == first.configs_to_write.front().id);
}

TEST_CASE("a curve whose discount curve waits waits too, and writes nothing", "[party_curves]") {
    auto lg(make_logger(test_suite));

    auto system = eur_estr();
    const auto projection_config = uuid_of("00000000-0000-4000-8000-0000000000c2");
    const auto projection_output = uuid_of("00000000-0000-4000-8000-0000000000d2");
    const auto projection_quotes = uuid_of("00000000-0000-4000-8000-0000000000e2");
    const std::string projection_uri =
        "oresmd://ir/EUR?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=6M";
    ir_curve_bootstrap_config projection;
    projection.id = projection_config;
    projection.party_id = system_party;
    projection.name = "EUR-EURIBOR-6M";
    projection.output_series_id = projection_output;
    projection.curve_family_role = "PROJECTION";
    projection.discount_curve_config_id = eur_config;
    system.configs.push_back(projection);
    ir_curve_bootstrap_pillar pillar;
    pillar.id = uuid_of("00000000-0000-4000-8000-0000000000f9");
    pillar.party_id = system_party;
    pillar.bootstrap_config_id = projection_config;
    pillar.market_series_id = projection_quotes;
    system.pillars.push_back(pillar);
    system.series.push_back(
        market_series_row(projection_output,
                          system_party,
                          "oresmd://ir/EUR?type=series&instrument=discount&quote=rate"
                          "&curve_id=EUR-EURIBOR-6M",
                          "ore",
                          "yield"));
    system.series.push_back(
        market_series_row(projection_quotes, system_party, projection_uri, "bloomberg", "yield"));

    party_curve_holdings acme;
    acme.party_id = acme_us;
    acme.series = {market_series_row(uuid_of("00000000-0000-4000-8000-0000000000b4"),
                                     acme_us,
                                     projection_uri,
                                     "bloomberg",
                                     "yield")};

    const auto plan = plan_party_curves(system, acme);

    CHECK(plan.configs_to_write.empty());
    CHECK(plan.pillars_to_write.empty());
    CHECK(plan.series_to_write.empty());
    CHECK(plan.ready.empty());
    REQUIRE(plan.waiting.size() == 2);
    CHECK(plan.waiting.back() == "EUR-EURIBOR-6M: its discount curve is not written yet");
}

TEST_CASE("two parties get different copies of one system row", "[party_curves]") {
    auto lg(make_logger(test_suite));

    CHECK(party_copy_id(acme_us, "ir_curve_bootstrap_config", eur_config) ==
          party_copy_id(acme_us, "ir_curve_bootstrap_config", eur_config));
    CHECK(party_copy_id(acme_us, "ir_curve_bootstrap_config", eur_config) !=
          party_copy_id(system_party, "ir_curve_bootstrap_config", eur_config));
    CHECK(party_copy_id(acme_us, "ir_curve_bootstrap_config", eur_config) !=
          party_copy_id(acme_us, "market_series", eur_config));
}

TEST_CASE("a_party_writes_a_projection_after_the_funding_curve_it_discounts_off",
          "[party_curves]") {
    auto lg(make_logger(test_suite));

    // The system states its configs in name order, so a projection can sort before the funding
    // curve it names -- USD-LIBOR-1M before USD-SOFR-FOMC. The store checks the discount curve
    // as each row lands, so the batch must name the funding curve first however the system
    // lists them.
    auto system = eur_estr();
    const auto projection_config = uuid_of("00000000-0000-4000-8000-0000000000c3");
    const auto projection_output = uuid_of("00000000-0000-4000-8000-0000000000d3");
    const auto projection_quotes = uuid_of("00000000-0000-4000-8000-0000000000e3");
    const std::string projection_uri =
        "oresmd://ir/EUR?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=6M";
    ir_curve_bootstrap_config projection;
    projection.id = projection_config;
    projection.party_id = system_party;
    projection.name = "AAA-EURIBOR-6M";
    projection.output_series_id = projection_output;
    projection.curve_family_role = "PROJECTION";
    projection.discount_curve_config_id = eur_config;
    system.configs.insert(system.configs.begin(), projection);
    ir_curve_bootstrap_pillar pillar;
    pillar.id = uuid_of("00000000-0000-4000-8000-0000000000fa");
    pillar.party_id = system_party;
    pillar.bootstrap_config_id = projection_config;
    pillar.market_series_id = projection_quotes;
    system.pillars.push_back(pillar);
    // The seed writes a curve's output series as observed, and only its first republish
    // stamps it a bootstrap, so a party can be provisioned before the system's label is
    // right.
    system.series.push_back(
        market_series_row(projection_output,
                          system_party,
                          "oresmd://ir/EUR?type=series&instrument=discount&quote=rate"
                          "&curve_id=AAA-EURIBOR-6M",
                          "ore",
                          "yield"));
    system.series.push_back(
        market_series_row(projection_quotes, system_party, projection_uri, "bloomberg", "yield"));

    party_curve_holdings acme;
    acme.party_id = acme_us;
    acme.series = {market_series_row(uuid_of("00000000-0000-4000-8000-0000000000b5"),
                                     acme_us,
                                     eur_quotes_uri,
                                     "bloomberg",
                                     "yield"),
                   market_series_row(uuid_of("00000000-0000-4000-8000-0000000000b6"),
                                     acme_us,
                                     projection_uri,
                                     "bloomberg",
                                     "yield")};

    const auto plan = plan_party_curves(system, acme);

    REQUIRE(plan.configs_to_write.size() == 2);
    CHECK(plan.configs_to_write[0].name == "EUR-ESTR");
    CHECK(plan.configs_to_write[1].name == "AAA-EURIBOR-6M");
    CHECK(plan.configs_to_write[1].discount_curve_config_id == plan.configs_to_write[0].id);

    // The party's output series is its own bootstrap of the same curve, whatever the
    // system's row says yet, and it names the party's own copy of the config.
    const auto output = std::ranges::find_if(plan.series_to_write, [&](const auto& s) {
        return s.id == party_copy_id(acme_us, "market_series", projection_output);
    });
    REQUIRE(output != plan.series_to_write.end());
    CHECK(output->derivation_kind == "IR_CURVE_BOOTSTRAP");
    CHECK(output->derivation_config_id ==
          party_copy_id(acme_us, "ir_curve_bootstrap_config", projection_config));
    CHECK(output->derivation_config_version == 1);
}

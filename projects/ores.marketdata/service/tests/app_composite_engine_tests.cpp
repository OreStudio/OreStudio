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
#include "ores.marketdata.service/app/composite_engine.hpp"
#include "ores.marketdata.service/app/demand_calculator.hpp"
#include "ores.marketdata.service/app/usage_planner.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cmath>
#include <string>
#include <tuple>

namespace {

const std::string tags("[app][composite_engine]");

using namespace std::chrono_literals;
using namespace ores::marketdata::service::app;

const std::string blend = "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=CHF";
const std::string bbg_eur = "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD";
const std::string lseg_eur = "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=GBP";

struct world {
    boost::uuids::random_generator gen;
    ores::marketdata::domain::composite composite;
    std::vector<ores::marketdata::domain::composite_input> inputs;

    world() {
        composite.id = gen();
        composite.name = "eurusd-blend";
        composite.provider_code = "composite";
        composite.oresmd_uri = blend;
        composite.rule_code = "average";
        add_input("bloomberg", bbg_eur, 0);
        add_input("lseg", lseg_eur, 1);
    }

    void add_input(const std::string& provider, const std::string& uri, int position) {
        ores::marketdata::domain::composite_input i;
        i.id = gen();
        i.composite_id = composite.id;
        i.provider_code = provider;
        i.oresmd_uri = uri;
        i.position = position;
        inputs.push_back(i);
    }

    static ores::marketdata::domain::subscription subscription(const std::string& provider,
                                                               bool derived) {
        ores::marketdata::domain::subscription s;
        s.provider_code = provider;
        s.display = true;
        s.derived = derived;
        return s;
    }
};

}

TEST_CASE("a_pass_through_rule_takes_the_first_input_that_has_a_value", tags) {
    const auto rule = make_composite_rule("pass_through");
    REQUIRE(rule);
    CHECK(rule->evaluate({1.5, 2.5}) == 1.5);
    CHECK_FALSE(rule->evaluate({}).has_value());
}

TEST_CASE("the_average_and_median_rules_derive_a_value", tags) {
    CHECK(make_composite_rule("average")->evaluate({1.0, 2.0, 6.0}) == 3.0);
    CHECK(make_composite_rule("median")->evaluate({9.0, 1.0, 5.0}) == 5.0);
    CHECK(make_composite_rule("median")->evaluate({1.0, 2.0, 3.0, 10.0}) == 2.5);
    CHECK_FALSE(make_composite_rule("average")->evaluate({}).has_value());
}

TEST_CASE("a_code_that_names_no_rule_makes_no_rule", tags) {
    CHECK(make_composite_rule("waterfall") == nullptr);
    CHECK(make_composite_rule("") == nullptr);
}

TEST_CASE("a_composite_takes_its_value_from_its_inputs_when_the_party_holds_the_rights", tags) {
    world w;
    const std::vector subscriptions{world::subscription("bloomberg", true),
                                    world::subscription("lseg", true)};
    const input_values latest{{{"bloomberg", bbg_eur}, 1.08}, {{"lseg", lseg_eur}, 1.10}};

    const auto out = evaluate_composite(w.composite, w.inputs, subscriptions, latest);

    REQUIRE(out.value.has_value());
    CHECK(std::fabs(*out.value - 1.09) < 1e-12);
    CHECK(out.reason.empty());
}

TEST_CASE("a_composite_with_a_missing_right_gets_no_value_and_shows_the_reason", tags) {
    world w;
    const std::vector subscriptions{world::subscription("bloomberg", true),
                                    world::subscription("lseg", false)};
    const input_values latest{{{"bloomberg", bbg_eur}, 1.08}, {{"lseg", lseg_eur}, 1.10}};

    const auto out = evaluate_composite(w.composite, w.inputs, subscriptions, latest);

    CHECK_FALSE(out.value.has_value());
    CHECK(out.reason.find("lseg") != std::string::npos);
    CHECK(out.reason.find("bloomberg") == std::string::npos);
}

TEST_CASE("a_party_with_no_subscription_lacks_the_right_on_every_provider", tags) {
    world w;

    const auto missing = missing_derived_rights(w.inputs, {});

    REQUIRE(missing.size() == 2);
    CHECK(missing[0] == "bloomberg");
    CHECK(missing[1] == "lseg");
}

TEST_CASE("the_display_right_alone_does_not_license_a_composite", tags) {
    world w;
    const std::vector subscriptions{world::subscription("bloomberg", false),
                                    world::subscription("lseg", false)};

    CHECK(missing_derived_rights(w.inputs, subscriptions).size() == 2);
}

TEST_CASE("a_composite_with_no_input_value_shows_that_reason", tags) {
    world w;
    const std::vector subscriptions{world::subscription("bloomberg", true),
                                    world::subscription("lseg", true)};

    const auto out = evaluate_composite(w.composite, w.inputs, subscriptions, {});

    CHECK_FALSE(out.value.has_value());
    CHECK(out.reason == "No input has a value.");
}

TEST_CASE("a_composite_uses_the_inputs_that_have_a_value_in_the_order_of_their_positions", tags) {
    world w;
    w.composite.rule_code = "pass_through";
    const std::vector subscriptions{world::subscription("bloomberg", true),
                                    world::subscription("lseg", true)};

    const auto only_second =
        evaluate_composite(w.composite, w.inputs, subscriptions, {{{"lseg", lseg_eur}, 1.10}});
    REQUIRE(only_second.value.has_value());
    CHECK(*only_second.value == 1.10);

    const auto both =
        evaluate_composite(w.composite,
                           w.inputs,
                           subscriptions,
                           {{{"bloomberg", bbg_eur}, 1.08}, {{"lseg", lseg_eur}, 1.10}});
    REQUIRE(both.value.has_value());
    CHECK(*both.value == 1.08);
}

TEST_CASE("a_composite_with_an_unknown_rule_shows_that_reason", tags) {
    world w;
    w.composite.rule_code = "waterfall";

    const auto out = evaluate_composite(w.composite, w.inputs, {}, {});

    CHECK_FALSE(out.value.has_value());
    CHECK(out.reason.find("waterfall") != std::string::npos);
}

TEST_CASE("the_resolver_gives_the_inputs_of_a_composite_in_order", tags) {
    world w;
    const auto resolve = composite_input_resolver({w.composite}, w.inputs);

    const auto items = resolve(blend);

    REQUIRE(items.size() == 2);
    CHECK(items[0] == bbg_eur);
    CHECK(items[1] == lseg_eur);
    CHECK(resolve(bbg_eur).empty());
}

TEST_CASE("the_demand_and_the_metering_of_a_composite_include_its_inputs", tags) {
    world w;
    boost::uuids::random_generator gen;
    const auto now = std::chrono::system_clock::now();

    demand_catalogue c;
    for (const auto& [provider, uri, id_scheme, external] :
         {std::tuple{std::string("bloomberg"),
                     bbg_eur,
                     std::string("bbg_ticker"),
                     std::string("EURUSD Curncy")},
          std::tuple{std::string("lseg"), lseg_eur, std::string("ric"), std::string("EURGBP=")}}) {
        ores::marketdata::domain::market_data_source source;
        source.id = gen();
        source.name = provider + "-sim";
        source.provider_code = provider;
        c.sources.push_back(source);

        ores::marketdata::domain::market_data_vendor_instrument v;
        v.id = gen();
        v.provider_code = provider;
        v.id_scheme = id_scheme;
        v.external_id = external;
        v.field = "PX_LAST";
        c.instruments.push_back(v);

        ores::marketdata::domain::market_data_mapping m;
        m.id = gen();
        m.vendor_instrument_id = v.id;
        m.oresmd_uri = uri;
        m.effective_from = now - 24h;
        c.mappings.push_back(m);

        const auto party = w.composite.id;
        ores::marketdata::domain::route r;
        r.id = gen();
        r.party_id = party;
        r.provider_code = provider;
        r.source_id = source.id;
        c.routes.push_back(r);

        ores::marketdata::domain::subscription s;
        s.id = gen();
        s.party_id = party;
        s.provider_code = provider;
        s.derived = true;
        c.subscriptions.push_back(s);
    }

    demand_policy policy;
    policy.requirements = [&](const auto&, const auto&) {
        return std::optional<std::set<std::string>>(std::set<std::string>{blend});
    };
    policy.composite_inputs = composite_input_resolver({w.composite}, w.inputs);

    const auto result = compute_demand(c, now, policy);
    REQUIRE(result.by_adapter.contains("bloomberg-sim"));
    REQUIRE(result.by_adapter.contains("lseg-sim"));
    CHECK(result.by_adapter.at("bloomberg-sim").size() == 1);
    CHECK(result.by_adapter.at("lseg-sim").size() == 1);

    const auto states = demand_states(compute_party_demand(c, now, policy));
    CHECK(states.size() == 2);
}

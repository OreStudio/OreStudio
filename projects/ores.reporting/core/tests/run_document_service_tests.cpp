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
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.reporting.api/generators/report_definition_generator.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.reporting.core/service/run_document_service.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>

namespace {

const std::string tags("[reporting][run_document][database]");

// A document belongs to a party, so each case writes one and acts for it.
ores::database::context party_context(ores::testing::scoped_database_helper& h) {
    auto gen = ores::testing::make_generation_context(h);
    ores::refdata::repository::party_repository repo;
    auto party = ores::refdata::generators::generate_synthetic_party(gen);
    party.change_reason_code = "system.test";
    for (const auto& e : repo.read_latest(h.context()))
        if (e.tenant_id == party.tenant_id) {
            party.parent_party_id = e.id;
            break;
        }
    repo.write(h.context(), party);
    return h.context().with_party(h.tenant_id(), party.id, {party.id}, h.db_user());
}

boost::uuids::uuid make_definition(ores::testing::scoped_database_helper& h,
                                   const ores::database::context& ctx) {
    auto gen = ores::testing::make_generation_context(h);
    auto d = ores::reporting::generators::generate_synthetic_report_definition(gen);
    d.change_reason_code = "system.test";
    d.report_type = "risk";
    d.party_id = *ctx.party_id();
    ores::reporting::repository::report_definition_repository().write(ctx, d);
    return d.id;
}

// An npv analytic with one parameter, both of which the seeded vocabulary holds.
ores::reporting::messaging::run_document npv_run() {
    ores::reporting::messaging::run_document doc;
    doc.setup.asof_date = "2016-02-05";
    doc.setup.curve_config_file = "curveconfig.xml";
    ores::reporting::messaging::run_analytic npv;
    npv.analytic.analytic_type_code = "npv";
    npv.analytic.display_order = 1;
    npv.analytic.active = "Y";
    npv.parameters.push_back({"baseCurrency", "EUR", 1});
    doc.analytics.push_back(npv);
    ores::reporting::domain::report_market_binding pricing;
    pricing.role = "pricing";
    pricing.configuration_name = "default";
    pricing.position = 1;
    doc.market_bindings.push_back(pricing);
    return doc;
}

}

using ores::reporting::service::run_document_service;

TEST_CASE("a run document reads back as it was saved", tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    const auto definition = make_definition(h, ctx);
    run_document_service runs(ctx);
    runs.save(definition, npv_run());

    const auto back = runs.get(definition);
    REQUIRE(back.has_value());
    CHECK(back->setup.curve_config_file == std::optional<std::string>("curveconfig.xml"));
    REQUIRE(back->analytics.size() == 1);
    CHECK(back->analytics[0].analytic.analytic_type_code == "npv");
    REQUIRE(back->analytics[0].parameters.size() == 1);
    CHECK(back->analytics[0].parameters[0].name == "baseCurrency");
    CHECK(back->analytics[0].parameters[0].value == "EUR");
    REQUIRE(back->market_bindings.size() == 1);
    CHECK(back->market_bindings[0].configuration_name == "default");
}

TEST_CASE("a definition holds one run document", tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    const auto definition = make_definition(h, ctx);
    run_document_service runs(ctx);
    runs.save(definition, npv_run());
    CHECK_THROWS_AS(runs.save(definition, npv_run()), std::invalid_argument);
}

TEST_CASE("a parameter no definition describes is refused before anything is written", tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    const auto definition = make_definition(h, ctx);
    auto doc = npv_run();
    doc.analytics[0].parameters.push_back({"noSuchParameter", "x", 2});
    run_document_service runs(ctx);
    CHECK_THROWS_AS(runs.save(definition, doc), std::invalid_argument);
    CHECK_FALSE(runs.get(definition).has_value());
}

TEST_CASE("a binding takes its owning component from the configuration type", tags) {
    ores::testing::scoped_database_helper h;
    const auto ctx = party_context(h);
    const auto definition = make_definition(h, ctx);
    run_document_service runs(ctx);
    const auto c = runs.bind(definition, "curve_configuration", "test/curveconfig.xml");
    CHECK(c.owning_component == "ores.refdata");

    const auto bindings = runs.bindings(definition);
    REQUIRE(bindings.size() == 1);
    CHECK(bindings[0].configuration_id == c.id);
    CHECK(bindings[0].configuration_type_code == "curve_configuration");
}

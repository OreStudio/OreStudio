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
#include "ores.reporting.api/messaging/run_document_protocol.hpp"
#include "ores.reporting.core/messaging/run_document_handler.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.testing/handler_test_support.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

const std::string tags("[reporting][run_document][handler]");

// Every row is party-scoped and the token carries the test tenant's system
// party, so the handler's reads see what the case wrote.
boost::uuids::uuid system_party(ores::testing::scoped_database_helper& h) {
    for (const auto& p : ores::refdata::repository::party_repository().read_latest(h.context()))
        if (p.tenant_id == h.tenant_id() && p.party_category == "System")
            return p.id;
    throw std::runtime_error("the test tenant has no system party");
}

std::string subject(const std::string& op) {
    return "ores.test.reporting.run_document_handler." + op;
}

struct fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    ores::testing::handler_test_keys keys;
    std::vector<ores::nats::service::subscription> subs;
    boost::uuids::uuid party;

    ores::reporting::messaging::run_document_handler handler() {
        return ores::reporting::messaging::run_document_handler(
            nats, db.context(), std::optional(keys.verifier()));
    }

    void subscribe(const std::string& op, std::function<void(ores::nats::message)> f) {
        subs.push_back(nats.queue_subscribe(subject(op), "ores.test", std::move(f)));
    }

    std::string token(const std::vector<std::string>& permissions) {
        return keys.token(db.tenant_id().to_string(), party, permissions);
    }

    fixture()
        : nats(ores::testing::make_nats_options()) {
        nats.connect();
        REQUIRE(nats.is_connected());
        party = system_party(db);
        subscribe("save_run_document",
                  [this](ores::nats::message m) { handler().save_run_document(std::move(m)); });
        subscribe("bind_configuration",
                  [this](ores::nats::message m) { handler().bind_configuration(std::move(m)); });
        subscribe("get_run_document",
                  [this](ores::nats::message m) { handler().get_run_document(std::move(m)); });
        subscribe("delete_run_document",
                  [this](ores::nats::message m) { handler().delete_run_document(std::move(m)); });
    }
};

std::string make_definition(fixture& f, const boost::uuids::uuid& party) {
    auto gen = ores::testing::make_generation_context(f.db);
    auto d = ores::reporting::generators::generate_synthetic_report_definition(gen);
    d.change_reason_code = "system.test";
    d.report_type = "risk";
    d.party_id = party;
    ores::reporting::repository::report_definition_repository().write(
        f.db.context().with_party(f.db.tenant_id(), party, {party}, f.db.db_user()), d);
    return boost::uuids::to_string(d.id);
}

ores::reporting::domain::run_document npv_run() {
    ores::reporting::domain::run_document doc;
    doc.setup.curve_config_file = "curveconfig.xml";
    ores::reporting::domain::run_analytic npv;
    npv.analytic.analytic_type_code = "npv";
    npv.analytic.display_order = 1;
    npv.analytic.active = "Y";
    npv.parameters.push_back({"baseCurrency", "EUR", 1});
    doc.analytics.push_back(npv);
    return doc;
}

const std::vector<std::string> all_permissions{"reporting::report_run_setups:read",
                                               "reporting::report_run_setups:write",
                                               "reporting::report_run_setups:delete",
                                               "reporting::configurations:write"};

}

using namespace ores::reporting::messaging;
using ores::testing::decode_reply;
using ores::testing::request;

TEST_CASE("a run document saves, binds, reads back and deletes through the operations", tags) {
    fixture f;
    const auto token = f.token(all_permissions);
    const auto definition = make_definition(f, f.party);

    const auto saved = decode_reply<save_run_document_response>(request(
        f.nats,
        subject("save_run_document"),
        token,
        save_run_document_request{.report_definition_id = definition, .document = npv_run()}));
    REQUIRE(saved);
    INFO(saved->message);
    REQUIRE(saved->success);

    const auto bound = decode_reply<bind_configuration_response>(
        request(f.nats,
                subject("bind_configuration"),
                token,
                bind_configuration_request{.report_definition_id = definition,
                                           .configuration_type_code = "curve_configuration",
                                           .name = "test/curveconfig.xml"}));
    REQUIRE(bound);
    INFO(bound->message);
    REQUIRE(bound->success);

    const auto got = decode_reply<get_run_document_response>(
        request(f.nats,
                subject("get_run_document"),
                token,
                get_run_document_request{.report_definition_id = definition}));
    REQUIRE(got);
    REQUIRE(got->success);
    REQUIRE(got->document.analytics.size() == 1);
    CHECK(got->document.analytics[0].parameters[0].name == "baseCurrency");
    REQUIRE(got->bindings.size() == 1);
    CHECK(boost::uuids::to_string(got->bindings[0].configuration_id) == bound->configuration_id);
    CHECK(got->party_id == boost::uuids::to_string(f.party));

    const auto deleted = decode_reply<delete_run_document_response>(
        request(f.nats,
                subject("delete_run_document"),
                token,
                delete_run_document_request{.report_definition_id = definition}));
    REQUIRE(deleted);
    CHECK(deleted->success);
    const auto gone = decode_reply<get_run_document_response>(
        request(f.nats,
                subject("get_run_document"),
                token,
                get_run_document_request{.report_definition_id = definition}));
    REQUIRE(gone);
    CHECK_FALSE(gone->success);
}

TEST_CASE("a party cannot store a run on another party's definition", tags) {
    fixture f;
    auto gen = ores::testing::make_generation_context(f.db);
    auto other = ores::refdata::generators::generate_synthetic_party(gen);
    other.change_reason_code = "system.test";
    other.parent_party_id = f.party;
    ores::refdata::repository::party_repository().write(f.db.context(), other);
    const auto definition = make_definition(f, other.id);

    auto claims_token = f.keys.token(f.db.tenant_id().to_string(), f.party, all_permissions);
    const auto saved = decode_reply<save_run_document_response>(request(
        f.nats,
        subject("save_run_document"),
        claims_token,
        save_run_document_request{.report_definition_id = definition, .document = npv_run()}));
    REQUIRE(saved);
    CHECK_FALSE(saved->success);
}

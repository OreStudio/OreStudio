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
#include "ores.database/domain/context.hpp"
#include "ores.iam.api/messaging/run_grant_operations_protocol.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/config/nats_options.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.reporting.api/generators/report_definition_generator.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.api/messaging/run_document_protocol.hpp"
#include "ores.reporting.core/messaging/report_instance_trigger_handler.hpp"
#include "ores.reporting.core/messaging/run_document_handler.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.reporting.core/repository/report_instance_repository.hpp"
#include "ores.reporting.core/service/report_run_grant_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.testing/handler_test_support.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.core/service/fsm_state_map.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <memory>
#include <optional>
#include <string>
#include <vector>

using ores::service::messaging::reply;

// The decision these pin: a report's consent to run is created when it does
// not exist, from the person who asked, and is recorded on the definition so
// every later trigger reuses it. The tests read the definition and the instance
// back through the repositories, so what is asserted is what the store holds.
//
// The reporting service reaches IAM on the person's token, not its own. The
// fixture asserts that by standing in for IAM on the real subject and checking
// the delegated bearer the call arrives with: a service-account call would
// arrive with no delegated token and this stub would refuse it.

namespace {

const std::string_view test_suite("ores.reporting.tests.run_grant");
const std::string tags("[run_grant][trigger][handler]");

const std::vector<std::string> person_permissions{"reporting::report_definitions:read",
                                                  "reporting::report_definitions:write",
                                                  "reporting::configurations:write",
                                                  "reporting::run_documents:write"};

// The configuration types the risk report type requires that a person binds
// on their own. Its risk report configuration is not among them: the run
// document makes that one, because an ORE run needs an analytic switched on
// and only the document states which.
const std::vector<std::string> required_configuration_types{
    "pricing_engines", "todays_market", "curve_configuration", "conventions"};

// One subject namespace for the whole fixture, so a request this test makes is
// answered by this test and never by the running services on the same server.
constexpr std::string_view test_subject_prefix = "ores.test.bright_faraday.run_grant";

std::string trigger_subject() {
    return "trigger_report_instance";
}

std::string bind_subject() {
    return "bind_configuration";
}

std::string save_document_subject() {
    return "save_run_document";
}

ores::nats::config::nats_options test_nats_options() {
    auto opts = ores::testing::make_nats_options();
    opts.subject_prefix = std::string(test_subject_prefix);
    return opts;
}

using ores::reporting::repository::report_definition_repository;
using ores::reporting::repository::report_instance_repository;
using ores::reporting::service::report_run_grant_service;
using ores::testing::decode_reply;
using ores::testing::request;

/**
 * @brief One trigger path with a stand-in for IAM's run grant operation.
 *
 * The stub is what proves the person's authority travelled: it records the
 * delegated bearer the call carried, and answers the grant IAM would answer.
 */
struct fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    ores::testing::handler_test_keys keys;
    ores::nats::service::nats_client svc;
    std::vector<ores::nats::service::subscription> subs;
    boost::uuids::uuid party;
    std::string person_token;

    // What the stand-in IAM saw, so a test can say the call arrived on the
    // person's token rather than the service's.
    std::atomic<int> grant_calls{0};
    std::string resource_granted;
    std::string delegated_token_seen;
    std::string grant_id = boost::uuids::to_string(boost::uuids::random_generator()());

    ores::database::context party_ctx() {
        return db.context().with_party(db.tenant_id(), party, {party}, db.db_user());
    }

    std::string token(const std::vector<std::string>& permissions) {
        return keys.token(db.tenant_id().to_string(), party, permissions);
    }

    void answer_run_grants(ores::nats::message msg) {
        const auto codec = ores::nats::default_wire_codec();
        const auto req = codec.decode<ores::iam::messaging::create_run_grant_request>(msg.data);
        REQUIRE(req.has_value());
        ++grant_calls;
        resource_granted = req->resource;
        const auto delegated =
            msg.headers.find(std::string(ores::nats::headers::delegated_authorization));
        delegated_token_seen = delegated == msg.headers.end() ? std::string() : delegated->second;
        ores::iam::messaging::create_run_grant_response response;
        response.success = true;
        response.grant_id = grant_id;
        response.message = "The grant is already active with this consent.";
        reply(nats, msg, response);
    }

    // The trigger refuses a run that does not bind every configuration type
    // its report type requires, so the bindings are made the way a person
    // makes them. An ORE run needs an analytic switched on, so the run
    // document states one before the risk configuration is bound.
    void bind_configurations(const boost::uuids::uuid& definition_id) {
        ores::reporting::messaging::run_document_handler binds(
            nats, db.context(), std::optional(keys.verifier()));
        subs.push_back(
            nats.queue_subscribe(bind_subject(), "ores.test", [&binds](ores::nats::message m) {
                binds.bind_configuration(std::move(m));
            }));
        subs.push_back(nats.queue_subscribe(
            save_document_subject(), "ores.test", [&binds](ores::nats::message m) {
                binds.save_run_document(std::move(m));
            }));

        ores::reporting::messaging::run_document document;
        document.settings.base_currency = "EUR";
        ores::reporting::messaging::run_analytic npv;
        npv.analytic.analytic_type_code = "npv";
        npv.analytic.display_order = 1;
        npv.analytic.active = "Y";
        npv.parameters.push_back({"baseCurrency", "EUR", 1});
        document.analytics.push_back(npv);
        const auto saved = decode_reply<ores::reporting::messaging::save_run_document_response>(
            request(nats,
                    save_document_subject(),
                    person_token,
                    ores::reporting::messaging::save_run_document_request{
                        .report_definition_id = boost::uuids::to_string(definition_id),
                        .document = document}));
        REQUIRE(saved);
        INFO(saved->message);
        REQUIRE(saved->success);

        for (const auto& type : required_configuration_types) {
            const auto bound =
                decode_reply<ores::reporting::messaging::bind_configuration_response>(
                    request(nats,
                            bind_subject(),
                            person_token,
                            ores::reporting::messaging::bind_configuration_request{
                                .report_definition_id = boost::uuids::to_string(definition_id),
                                .configuration_type_code = type,
                                .name = "test/" + type + ".xml"}));
            REQUIRE(bound);
            INFO(bound->message);
            REQUIRE(bound->success);
        }
    }

    ores::reporting::domain::report_definition make_definition() {
        auto gen = ores::testing::make_generation_context(db);
        auto def = ores::reporting::generators::generate_synthetic_report_definition(gen);
        def.change_reason_code = "system.test";
        // The generator draws its own tenant, and the definition is written in
        // the test tenant the party lives in.
        def.tenant_id = db.tenant_id();
        def.party_id = party;
        def.report_type = "risk";
        def.scheduler_job_id = std::nullopt;
        def.run_grant_id = std::nullopt;
        report_definition_repository().write(party_ctx(), def);
        return def;
    }

    std::optional<ores::reporting::domain::report_definition>
    definition_of(const boost::uuids::uuid& id) {
        const auto rows =
            report_definition_repository().read_latest(party_ctx(), boost::uuids::to_string(id));
        if (rows.empty())
            return std::nullopt;
        return rows.front();
    }

    fixture()
        : nats(test_nats_options()) {
        nats.connect();
        REQUIRE(nats.is_connected());
        auto gen = ores::testing::make_generation_context(db);
        auto p = ores::refdata::generators::generate_synthetic_party(gen);
        p.change_reason_code = "system.test";
        // A tenant's party tree holds one root, so this party hangs off one
        // that is already there. The test tenant has none, and then this party
        // is the root.
        const auto existing_party =
            ores::refdata::repository::party_repository().read_latest(db.context());
        if (!existing_party.empty())
            p.parent_party_id = existing_party.front().id;
        ores::refdata::repository::party_repository().write(db.context(), p);
        party = p.id;
        person_token = token(person_permissions);
        // The service's own client needs a credential for the Authorization
        // header. What this fixture asserts is the delegated header, so the
        // service token only has to be a token.
        svc = ores::nats::service::nats_client(nats, [this](bool) { return person_token; });
    }

    ~fixture() {
        svc = ores::nats::service::nats_client();
        subs.clear();
    }
};

}

TEST_CASE("a definition with no consent is given one, from the person asking", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    fixture f;
    const auto def = f.make_definition();
    REQUIRE_FALSE(def.run_grant_id.has_value());

    // IAM answers on the real subject, so the call the reporting service makes
    // is the one a deployment makes.
    f.subs.push_back(f.nats.queue_subscribe(
        std::string(ores::iam::messaging::create_run_grant_request::nats_subject),
        "ores.test",
        [&f](ores::nats::message m) { f.answer_run_grants(std::move(m)); }));

    report_run_grant_service grants(f.party_ctx(), f.svc.with_delegation(f.person_token));
    const auto granted = grants.ensure(def);
    INFO(granted.error_or(std::string()));
    REQUIRE(granted);
    CHECK(boost::uuids::to_string(*granted) == f.grant_id);

    // The consent is the definition's, so the next reader of the row finds it.
    const auto stored = f.definition_of(def.id);
    REQUIRE(stored);
    REQUIRE(stored->run_grant_id.has_value());
    CHECK(boost::uuids::to_string(*stored->run_grant_id) == f.grant_id);

    // It was asked for on the person's token, against the definition's own
    // resource.
    CHECK(f.grant_calls == 1);
    CHECK(f.resource_granted == "reporting.report_definition/" + boost::uuids::to_string(def.id));
    CHECK(f.delegated_token_seen ==
          std::string(ores::nats::headers::bearer_prefix) + f.person_token);

    // Every later trigger reuses it: a definition that holds a consent does not
    // ask IAM again.
    const auto again = grants.ensure(*stored);
    REQUIRE(again);
    CHECK(*again == *granted);
    CHECK(f.grant_calls == 1);
}

TEST_CASE("an on-demand run of a never-scheduled definition holds its consent", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    fixture f;
    // A package is what a person asks for by hand, and it is what building a
    // report's ORE packages asks for. A package instance neither waits for a
    // production run nor holds one up, so the run proceeds to admission.
    const auto def = f.make_definition();
    f.bind_configurations(def.id);

    const auto states = ores::workflow::service::load_fsm_states(
        f.party_ctx().with_tenant(ores::utility::uuid::tenant_id::system(), f.db.db_user()),
        "report_instance_lifecycle");

    ores::reporting::messaging::report_instance_trigger_handler handler(
        f.nats, f.db.context(), std::optional(f.keys.verifier()), states, f.svc);
    f.subs.push_back(
        f.nats.queue_subscribe(trigger_subject(), "ores.test", [&handler](ores::nats::message m) {
            handler.trigger(std::move(m));
        }));
    f.subs.push_back(f.nats.queue_subscribe(
        std::string(ores::iam::messaging::create_run_grant_request::nats_subject),
        "ores.test",
        [&f](ores::nats::message m) { f.answer_run_grants(std::move(m)); }));

    const ores::reporting::messaging::trigger_report_instance_request req{
        .report_definition_id = def.id,
        .tenant_id = f.db.tenant_id().to_uuid(),
        .job_instance_id = 0,
        .kind = "package"};

    const auto triggered =
        decode_reply<ores::reporting::messaging::trigger_report_instance_response>(
            request(f.nats, trigger_subject(), f.person_token, req));
    REQUIRE(triggered);
    INFO(triggered->result.message);
    // The instance is admitted whether or not the workflow that follows is
    // reachable from here: this fixture publishes under a subject prefix no
    // stream covers, so the dispatch that would start the run is refused while
    // the run the person asked for is already recorded.
    REQUIRE_FALSE(triggered->report_instance_id.empty());

    // The definition now holds the consent, and the instance that ran under it
    // records the same grant.
    const auto stored = f.definition_of(def.id);
    REQUIRE(stored);
    REQUIRE(stored->run_grant_id.has_value());
    CHECK(boost::uuids::to_string(*stored->run_grant_id) == f.grant_id);
    CHECK_FALSE(stored->scheduler_job_id.has_value());

    const auto instances =
        report_instance_repository().read_latest(f.party_ctx(), triggered->report_instance_id);
    REQUIRE(instances.size() == 1);
    REQUIRE(instances.front().run_grant_id.has_value());
    CHECK(boost::uuids::to_string(*instances.front().run_grant_id) == f.grant_id);
    CHECK(instances.front().definition_id == def.id);

    CHECK(f.grant_calls == 1);
    CHECK(f.delegated_token_seen ==
          std::string(ores::nats::headers::bearer_prefix) + f.person_token);
}

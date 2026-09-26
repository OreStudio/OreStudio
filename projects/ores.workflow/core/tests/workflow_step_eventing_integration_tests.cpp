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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_nats_integration_test.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.database/domain/context.hpp"
#include "ores.eventing.api/domain/entity_event.hpp"
#include "ores.eventing.api/domain/entity_event_traits.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/entity_event_publisher.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.workflow.api/domain/workflow_step.hpp"
#include "ores.workflow.api/domain/workflow_step_json_io.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/eventing/workflow_step_event.hpp"
#include "ores.workflow.api/generators/workflow_step_generator.hpp"
#include "ores.workflow.api/messaging/workflow_step_protocol.hpp"
#include "ores.workflow.core/repository/workflow_step_repository.hpp"
#include "ores.workflow.core/service/workflow_step_service.hpp"
// Soft-FK parent seeding (ores_workflow_workflow_instances_tbl): the parent may live in another
// component, so its own component names the headers.
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/generators/workflow_instance_generator.hpp"
#include "ores.workflow.core/repository/workflow_instance_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <thread>

// Proves the "write an entity, observe its NATS entity-changed
// notification" pattern end to end for workflow_step -- the
// production DB-write -> pg_notify -> postgres_event_source ->
// event_bus -> NATS publish chain, assembled directly here the same
// way the production event-registrar wires it.

namespace {

const std::string_view test_suite("workflow.tests");
const std::string tags("[eventing][integration]");


}

using namespace ores::workflow::generators;
using ores::workflow::domain::workflow_step;
using ores::workflow::repository::workflow_step_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_workflow_step_publishes_an_event", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto& party_ctx = h.context();

    // 1. Wire the same DB-notify -> event_bus -> NATS-publish chain the
    // production event-registrar wires in the live service, assembled
    // directly in the test instead of via a running process.
    namespace ev = ores::eventing;
    ev::service::event_bus bus;
    ev::service::postgres_event_source event_source(party_ctx, bus);

    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats.connect();
    REQUIRE(nats.is_connected());

    using event_type = ores::workflow::messaging::workflow_step_event;
    auto sub = bus.subscribe<event_type>([&nats](const event_type& e) {
        // One payload is addressed by three subjects, so the subject is the
        // collection's prefix and the action the event reports.
        ev::service::publish_entity_event(nats, ev::domain::event_subject<event_type>(e.action), e);
    });

    event_source.register_entity_event_mapping<event_type>("ores_workflow_workflow_steps");

    // 2. Subscribe as an external observer would, on the relative subject --
    // client::subscribe() prepends the subject_prefix itself. The wildcard
    // takes every action: the first write creates the row and a re-drive
    // updates it, and the chain is what is under test rather than which of
    // the three subjects carried it.
    auto observer = nats.subscribe_buffered(
        std::string(ev::domain::entity_event_traits<event_type>::subject_prefix) + ".>", 10);

    // The listener thread issues LISTEN asynchronously on its own
    // dedicated connection. Block until it has actually done so before
    // writing -- Postgres does not queue NOTIFYs sent before a matching
    // LISTEN is registered.
    event_source.start();
    REQUIRE(event_source.wait_until_ready());

    // 3. Write -- triggers the entity's notify trigger -> pg_notify ->
    // the chain wired above -> NATS.
    auto v = generate_synthetic_workflow_step(ctx);
    v.change_reason_code = "system.test";
    // Seed the active workflow_instance row ores_workflow_workflow_instances_tbl references:
    // the insert trigger's existence check rejects a synthetic key that
    // matches no active row, so the parent must be written first.
    auto workflow_id_parent = ores::workflow::generators::generate_synthetic_workflow_instance(ctx);
    workflow_id_parent.change_reason_code = "system.test";
    ores::workflow::repository::workflow_instance_repository workflow_id_repo;
    workflow_id_repo.write(party_ctx, workflow_id_parent);
    v.workflow_id = workflow_id_parent.id;
    const auto id_str = boost::uuids::to_string(v.id);
    BOOST_LOG_SEV(lg, debug) << "Workflow Step: " << v;

    workflow_step_repository repo;
    repo.write(party_ctx, v);

    // 4. Poll the observer's buffer for the notification. The chain --
    // trigger -> pg_notify -> 100ms listener poll -> event_bus -> NATS
    // round trip -- is real, no mocks. Under CI load the listener or
    // NATS connection can hiccup once (reconnect backoff 1-5s) and the
    // notification in flight is lost forever; a lost notification never
    // arrives, so re-drive the write -- a new version row re-fires the
    // notify trigger. Bounded: 4 attempts, each polling ~2.5s.
    constexpr int max_attempts = 4;
    constexpr int polls_per_attempt = 25;
    std::vector<ores::nats::message> received;
    for (int attempt = 1; attempt <= max_attempts && received.empty(); ++attempt) {
        if (attempt > 1) {
            BOOST_LOG_SEV(lg, warn) << "No matching notification yet; re-driving write"
                                    << " (attempt " << attempt << " of " << max_attempts << ")";
            repo.write(party_ctx, v);
        }
        for (int i = 0; i < polls_per_attempt && received.empty(); ++i) {
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
            auto snap = observer.snapshot();
            for (const auto& msg : snap) {
                auto decoded = ores::nats::default_wire_codec().decode<event_type>(msg.data);
                // The event carries the row's own key record, so the row under
                // test is recognised by comparing it with the row written.
                if (decoded && decoded->key.id == v.id)
                    received.push_back(msg);
            }
        }
    }

    event_source.stop();

    if (received.empty()) {
        // Exhausted the budget: report what the observer did see so a
        // genuinely broken chain is diagnosable, not a bare empty check.
        const auto final_snapshot = observer.snapshot();
        BOOST_LOG_SEV(lg, error) << "No notification for workflow_step " << id_str << " after "
                                 << max_attempts << " writes; observer received "
                                 << final_snapshot.size() << " message(s) in total";
        for (const auto& msg : final_snapshot)
            BOOST_LOG_SEV(lg, error) << "  unexpected message on subject '" << msg.subject << "', "
                                     << msg.data.size() << " bytes";
    }
    REQUIRE_FALSE(received.empty());
    BOOST_LOG_SEV(lg, info) << "Received " << received.size()
                            << " matching NATS notification(s) for workflow_step " << id_str;

    // 5. CRUD round trip on the same row: update through the
    // repository, read the version history through the service, and
    // delete. Reads and writes go through party_ctx: for party-scoped
    // entities it already carries the visible-party GUC the RLS
    // policies filter every service read by; otherwise it is the
    // plain test context. The version history grows by one per write
    // (the notify re-drive above may have written more than once), so
    // only growth is asserted, not an exact count.
    {
        const auto& crud_ctx = party_ctx;
        ores::workflow::service::workflow_step_service svc(crud_ctx);
        v.change_commentary = "updated-by-crud-round-trip";
        repo.write(crud_ctx, v);

        auto versions = svc.get_step_history(id_str);
        REQUIRE(versions.size() >= 2);
        REQUIRE(versions.front().change_commentary == "updated-by-crud-round-trip");

        svc.delete_step(v.id);
        // Delete soft-closes the active row (the instead-of delete
        // rule sets valid_to): the row disappears from latest reads,
        // and the version history keeps every version.
        REQUIRE_FALSE(svc.get_step(v.id).has_value());
        REQUIRE(svc.get_step_history(id_str).size() == versions.size());
    }
}

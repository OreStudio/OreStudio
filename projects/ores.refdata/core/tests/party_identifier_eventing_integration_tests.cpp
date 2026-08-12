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
#include "ores.eventing.api/domain/entity_change_event.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/entity_event_publisher.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/domain/party_identifier.hpp"
#include "ores.refdata.api/domain/party_identifier_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/eventing/party_identifier_changed_event.hpp"
#include "ores.refdata.api/generators/party_identifier_generator.hpp"
#include "ores.refdata.core/repository/party_identifier_repository.hpp"
// Party seeds (mandatory party_id soft FKs, direct or via a parent's own
// mandatory party_id FK): the party generator and repository are used
// regardless of the child's generator facet, hence the fully-qualified
// refdata paths.
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <thread>

// Proves the "write an entity, observe its NATS entity-changed
// notification" pattern end to end for party_identifier -- the
// production DB-write -> pg_notify -> postgres_event_source ->
// event_bus -> NATS publish chain, assembled directly here the same
// way the production event-registrar wires it.

namespace {

const std::string_view test_suite("refdata.tests");
const std::string tags("[eventing][integration]");


}

using namespace ores::refdata::generators;
using ores::refdata::domain::party_identifier;
using ores::refdata::repository::party_identifier_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_party_identifier_publishes_nats_changed_event", tags) {
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

    auto sub = bus.subscribe<ores::refdata::eventing::party_identifier_changed_event>(
        [&nats](const ores::refdata::eventing::party_identifier_changed_event& e) {
            ev::service::publish_entity_event(
                nats,
                std::string(ev::domain::event_traits<
                            ores::refdata::eventing::party_identifier_changed_event>::name),
                ev::domain::entity_change_event{.entity = "ores.refdata.party_identifier",
                                                .timestamp = e.timestamp,
                                                .entity_ids = e.party_identifier_ids,
                                                .tenant_id = e.tenant_id});
        });

    event_source.register_mapping<ores::refdata::eventing::party_identifier_changed_event>(
        "ores.refdata.party_identifier", "ores_refdata_party_identifiers");

    // 2. Subscribe as an external observer would, on the relative subject --
    // client::subscribe() prepends the subject_prefix itself.
    auto observer = nats.subscribe_buffered(
        std::string(ev::domain::event_traits<
                    ores::refdata::eventing::party_identifier_changed_event>::name),
        10);

    // The listener thread issues LISTEN asynchronously on its own
    // dedicated connection. Block until it has actually done so before
    // writing -- Postgres does not queue NOTIFYs sent before a matching
    // LISTEN is registered.
    event_source.start();
    REQUIRE(event_source.wait_until_ready());

    // 3. Write -- triggers the entity's notify trigger -> pg_notify ->
    // the chain wired above -> NATS.
    auto v = generate_synthetic_party_identifier(ctx);
    v.change_reason_code = "system.test";
    // Seed the active party row ores_refdata_parties_tbl references:
    // the insert trigger's existence check rejects a synthetic key that
    // matches no active row, so the parent must be written first.
    auto party_id_parent = ores::refdata::generators::generate_synthetic_party(ctx);
    party_id_parent.change_reason_code = "system.test";
    // Only one root party (parent_party_id null) is allowed per tenant:
    // attach to the existing root party instead of creating a second one.
    auto party_id_existing = ores::refdata::repository::party_repository().read_latest(party_ctx);
    for (const auto& e : party_id_existing) {
        if (e.tenant_id == party_id_parent.tenant_id) {
            party_id_parent.parent_party_id = e.id;
            break;
        }
    }
    ores::refdata::repository::party_repository party_id_repo;
    party_id_repo.write(party_ctx, party_id_parent);
    v.party_id = party_id_parent.id;
    const auto id_str = boost::uuids::to_string(v.id);
    BOOST_LOG_SEV(lg, debug) << "Party Identifier: " << v;

    party_identifier_repository repo;
    repo.write(party_ctx, v);

    // 4. Poll the observer's buffer for the notification. Generous
    // timeout: trigger -> pg_notify -> 100ms listener poll -> event_bus
    // -> NATS round trip, all real, no mocks.
    std::vector<ores::nats::message> received;
    for (int i = 0; i < 50 && received.empty(); ++i) {
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
        auto snap = observer.snapshot();
        for (const auto& msg : snap) {
            auto decoded =
                ores::nats::default_wire_codec().decode<ev::domain::entity_change_event>(msg.data);
            if (decoded && decoded->entity == "ores.refdata.party_identifier") {
                for (const auto& changed_id : decoded->entity_ids) {
                    if (changed_id == id_str)
                        received.push_back(msg);
                }
            }
        }
    }

    event_source.stop();

    REQUIRE_FALSE(received.empty());
    BOOST_LOG_SEV(lg, info) << "Received " << received.size()
                            << " matching NATS notification(s) for party_identifier " << id_str;
}

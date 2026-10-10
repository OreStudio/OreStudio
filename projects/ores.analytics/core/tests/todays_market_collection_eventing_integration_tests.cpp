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
// A seeded parent is system-tenant reference data (its soft FK carries
// :use_system_tenant:), so its row is forced to the system tenant and
// written under a system-scoped context, and the tenant_id helpers are
// needed.
#include "ores.analytics.api/domain/todays_market_collection.hpp"
#include "ores.analytics.api/domain/todays_market_collection_json_io.hpp" // IWYU pragma: keep.
#include "ores.analytics.api/eventing/todays_market_collection_event.hpp"
#include "ores.analytics.api/generators/todays_market_collection_generator.hpp"
#include "ores.analytics.api/messaging/todays_market_collection_protocol.hpp"
#include "ores.analytics.core/repository/todays_market_collection_repository.hpp"
#include "ores.analytics.core/service/todays_market_collection_service.hpp"
#include "ores.eventing.api/domain/entity_event_traits.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/entity_event_publisher.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
// Soft-FK parent seeding (ores_analytics_todays_market_collection_kinds_tbl): the parent may live
// in another component, so its own component names the headers. A system-tenant parent is read
// rather than generated, so it needs no generator.
#include "ores.analytics.core/repository/todays_market_collection_kind_repository.hpp"
// Soft-FK parent seeding (ores_analytics_todays_market_configs_tbl): the parent may live in another
// component, so its own component names the headers. A system-tenant parent
// is read rather than generated, so it needs no generator.
#include "ores.analytics.api/generators/todays_market_config_generator.hpp"
#include "ores.analytics.core/repository/todays_market_config_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <string_view>
#include <thread>
#include <vector>

// Proves the "write an entity, observe its NATS entity-changed
// notification" pattern end to end for todays_market_collection -- the
// production DB-write -> pg_notify -> postgres_event_source ->
// event_bus -> NATS publish chain, assembled directly here the same
// way the production event-registrar wires it.

namespace {

const std::string_view test_suite("analytics.tests");
const std::string tags("[eventing][integration]");

// Todays Market Collection writes are party-scoped: the session-level
// app.current_party_id GUC must be set before writing.
ores::database::context
write_test_party_and_scope_context(ores::testing::scoped_database_helper& h,
                                   ores::utility::generation::generation_context& ctx) {
    using ores::refdata::repository::party_repository;
    party_repository party_repo;
    auto party = ores::refdata::generators::generate_synthetic_party(ctx);
    party.change_reason_code = "system.test";
    auto existing = party_repo.read_latest(h.context());
    for (const auto& e : existing) {
        if (e.tenant_id == party.tenant_id) {
            party.parent_party_id = e.id;
            break;
        }
    }
    party_repo.write(h.context(), party);
    return h.context().with_party(h.tenant_id(), party.id, {party.id}, h.db_user());
}

}

using namespace ores::analytics::generators;
using ores::analytics::domain::todays_market_collection;
using ores::analytics::repository::todays_market_collection_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_todays_market_collection_publishes_an_event", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto party_ctx = write_test_party_and_scope_context(h, ctx);

    // 1. Wire the same DB-notify -> event_bus -> NATS-publish chain the
    // production event-registrar wires in the live service, assembled
    // directly in the test instead of via a running process.
    namespace ev = ores::eventing;
    ev::service::event_bus bus;
    ev::service::postgres_event_source event_source(party_ctx, bus);

    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats.connect();
    REQUIRE(nats.is_connected());

    using event_type = ores::analytics::messaging::todays_market_collection_event;
    using published_type = ev::domain::published_entity_event<event_type>;
    auto sub = bus.subscribe<published_type>([&nats](const published_type& p) {
        // One payload is addressed by three subjects, so the subject is the
        // collection's prefix and the action the event reports. The tenant
        // travels in the headers.
        ev::service::publish_entity_event(
            nats, ev::domain::event_subject<event_type>(p.event.action), p);
    });

    event_source.register_entity_event_mapping<event_type>(
        "ores_analytics_todays_market_collections");

    // 2. Subscribe as an external observer would, on the relative subject --
    // client::subscribe() prepends the subject_prefix itself. The wildcard
    // takes every action: the first write creates the row and a re-drive
    // updates it, and the chain is what is under test rather than which of
    // the three subjects carried it.
    auto observer = nats.subscribe_buffered(ev::domain::event_subject_wildcard<event_type>(), 10);

    // The listener thread issues LISTEN asynchronously on its own
    // dedicated connection. Block until it has actually done so before
    // writing -- Postgres does not queue NOTIFYs sent before a matching
    // LISTEN is registered.
    event_source.start();
    REQUIRE(event_source.wait_until_ready());

    // 3. Write -- triggers the entity's notify trigger -> pg_notify ->
    // the chain wired above -> NATS.
    auto v = generate_synthetic_todays_market_collection(ctx);
    v.change_reason_code = "system.test";
    v.party_id = *party_ctx.party_id();
    // todays_market_collection_kind is system-tenant reference data: reference a
    // seeded catalogue row instead of creating one, so the shared system
    // catalogue keeps exactly the rows the populate scripts put there. The
    // referencing row's insert trigger resolves the parent under the system
    // tenant.
    {
        ores::analytics::repository::todays_market_collection_kind_repository
            collection_catalogue_repo;
        const auto collection_catalogue = collection_catalogue_repo.read_latest(
            party_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user()));
        REQUIRE_FALSE(collection_catalogue.empty());
        v.collection = collection_catalogue.front().code;
    }
    // Seed the active todays_market_config row ores_analytics_todays_market_configs_tbl references:
    // the insert trigger's existence check rejects a synthetic key that
    // matches no active row, so the parent must be written first.
    auto todays_market_config_id_parent =
        ores::analytics::generators::generate_synthetic_todays_market_config(ctx);
    todays_market_config_id_parent.change_reason_code = "system.test";
    // The todays_market_config is party-isolated, so it carries the
    // session party: under any other party the child's insert trigger could
    // not see it, and the existence check would reject the child.
    todays_market_config_id_parent.party_id = *party_ctx.party_id();
    ores::analytics::repository::todays_market_config_repository todays_market_config_id_repo;
    todays_market_config_id_repo.write(party_ctx, todays_market_config_id_parent);
    v.todays_market_config_id = todays_market_config_id_parent.id;
    const auto id_str = boost::uuids::to_string(v.id);
    BOOST_LOG_SEV(lg, debug) << "Todays Market Collection: " << v;

    todays_market_collection_repository repo;
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
                if (decoded && decoded->key.collection == v.collection)
                    received.push_back(msg);
            }
        }
    }

    event_source.stop();

    if (received.empty()) {
        // Exhausted the budget: report what the observer did see so a
        // genuinely broken chain is diagnosable, not a bare empty check.
        const auto final_snapshot = observer.snapshot();
        BOOST_LOG_SEV(lg, error) << "No notification for todays_market_collection " << id_str
                                 << " after " << max_attempts << " writes; observer received "
                                 << final_snapshot.size() << " message(s) in total";
        for (const auto& msg : final_snapshot)
            BOOST_LOG_SEV(lg, error) << "  unexpected message on subject '" << msg.subject << "', "
                                     << msg.data.size() << " bytes";
    }
    REQUIRE_FALSE(received.empty());
    // The envelope names the tenant that owns the row, so a relay can pass the
    // event on only to a reader of that tenant.
    REQUIRE(received.front().headers.contains(std::string(ores::nats::headers::x_tenant_id)));
    BOOST_LOG_SEV(lg, info) << "Received " << received.size()
                            << " matching NATS notification(s) for todays_market_collection "
                            << id_str;

    // 5. CRUD round trip on the same row: update through the
    // repository, read the version history through the service, and
    // delete. Reads and writes go through party_ctx: for party-scoped
    // entities it already carries the visible-party GUC the RLS
    // policies filter every service read by; otherwise it is the
    // plain test context. The version history grows by one per write
    // (the notify re-drive above may have written more than once), so
    // only growth is asserted, not an exact count.
    {
        // party_ctx already carries the visible-party set: v's own
        // party is the session party the RLS policies filter by.
        const auto& crud_ctx = party_ctx;
        ores::analytics::service::todays_market_collection_service svc(crud_ctx);
        v.change_commentary = "updated-by-crud-round-trip";
        repo.write(crud_ctx, v);

        auto versions = svc.get_collection_history(v.collection);
        REQUIRE(versions.size() >= 2);
        REQUIRE(versions.front().change_commentary == "updated-by-crud-round-trip");

        svc.delete_collection(v.id);
        // Delete soft-closes the active row (the instead-of delete
        // rule sets valid_to): the row disappears from latest reads,
        // and the version history keeps every version.
        REQUIRE_FALSE(svc.get_collection(v.id).has_value());
        REQUIRE(svc.get_collection_history(v.collection).size() == versions.size());
    }
}

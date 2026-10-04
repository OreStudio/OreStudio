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
#include "ores.eventing.api/domain/entity_event.hpp"
#include "ores.eventing.api/domain/entity_event_traits.hpp"
#include "ores.eventing.api/domain/event_traits.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.eventing.core/service/entity_event_publisher.hpp"
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.inbox.api/domain/notification_preference.hpp"
#include "ores.inbox.api/domain/notification_preference_json_io.hpp" // IWYU pragma: keep.
#include "ores.inbox.api/eventing/notification_preference_event.hpp"
#include "ores.inbox.api/generators/notification_preference_generator.hpp"
#include "ores.inbox.api/messaging/notification_preference_protocol.hpp"
#include "ores.inbox.core/repository/notification_preference_repository.hpp"
#include "ores.inbox.core/service/notification_preference_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
// Soft-FK parent seeding (ores_iam_accounts_tbl): the parent may live in another
// component, so its own component names the headers.
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
// Soft-FK parent seeding (ores_inbox_notification_kinds_tbl): the parent may live in another
// component, so its own component names the headers.
#include "ores.inbox.api/generators/notification_kind_generator.hpp"
#include "ores.inbox.core/repository/notification_kind_repository.hpp"
// Soft-FK parent seeding (ores_inbox_notification_channels_tbl): the parent may live in another
// component, so its own component names the headers.
#include "ores.inbox.api/generators/notification_channel_generator.hpp"
#include "ores.inbox.core/repository/notification_channel_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <thread>

// Proves the "write an entity, observe its NATS entity-changed
// notification" pattern end to end for notification_preference -- the
// production DB-write -> pg_notify -> postgres_event_source ->
// event_bus -> NATS publish chain, assembled directly here the same
// way the production event-registrar wires it.

namespace {

const std::string_view test_suite("inbox.tests");
const std::string tags("[eventing][integration]");


}

using namespace ores::inbox::generators;
using ores::inbox::domain::notification_preference;
using ores::inbox::repository::notification_preference_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_notification_preference_publishes_an_event", tags) {
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

    using event_type = ores::inbox::messaging::notification_preference_event;
    auto sub = bus.subscribe<event_type>([&nats](const event_type& e) {
        // One payload is addressed by three subjects, so the subject is the
        // collection's prefix and the action the event reports.
        ev::service::publish_entity_event(nats, ev::domain::event_subject<event_type>(e.action), e);
    });

    event_source.register_entity_event_mapping<event_type>("ores_inbox_notification_preferences");

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
    auto v = generate_synthetic_notification_preference(ctx);
    v.change_reason_code = "system.test";
    // Seed the active account row ores_iam_accounts_tbl references:
    // the insert trigger's existence check rejects a synthetic key that
    // matches no active row, so the parent must be written first.
    auto account_id_parent = ores::iam::generators::generate_synthetic_account(ctx);
    account_id_parent.change_reason_code = "system.test";
    ores::iam::repository::account_repository account_id_repo;
    account_id_repo.write(party_ctx, account_id_parent);
    v.account_id = account_id_parent.id;
    // notification_kind is system-tenant reference data: reference a
    // seeded catalogue row instead of creating one, so the shared system
    // catalogue keeps exactly the rows the populate scripts put there. The
    // referencing row's insert trigger resolves the parent under the system
    // tenant.
    {
        ores::inbox::repository::notification_kind_repository kind_code_catalogue_repo;
        const auto kind_code_catalogue = kind_code_catalogue_repo.read_latest(
            party_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user()));
        REQUIRE_FALSE(kind_code_catalogue.empty());
        v.kind_code = kind_code_catalogue.front().code;
    }
    // notification_channel is system-tenant reference data: reference a
    // seeded catalogue row instead of creating one, so the shared system
    // catalogue keeps exactly the rows the populate scripts put there. The
    // referencing row's insert trigger resolves the parent under the system
    // tenant.
    {
        ores::inbox::repository::notification_channel_repository channel_code_catalogue_repo;
        const auto channel_code_catalogue = channel_code_catalogue_repo.read_latest(
            party_ctx.with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user()));
        REQUIRE_FALSE(channel_code_catalogue.empty());
        v.channel_code = channel_code_catalogue.front().code;
    }
    const auto id_str = boost::uuids::to_string(v.account_id);
    // The notify trigger emits one entity_id per key column, so a
    // notification belongs to this row only when every key part is in it.
    const std::vector<std::string> key_parts = {id_str, v.kind_code, v.channel_code};
    BOOST_LOG_SEV(lg, debug) << "Notification Preference: " << v;

    notification_preference_repository repo;
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
                if (decoded && decoded->key.account_id == v.account_id &&
                    decoded->key.kind_code == v.kind_code &&
                    decoded->key.channel_code == v.channel_code)
                    received.push_back(msg);
            }
        }
    }

    event_source.stop();

    if (received.empty()) {
        // Exhausted the budget: report what the observer did see so a
        // genuinely broken chain is diagnosable, not a bare empty check.
        const auto final_snapshot = observer.snapshot();
        BOOST_LOG_SEV(lg, error) << "No notification for notification_preference " << id_str << "/"
                                 << v.kind_code << "/" << v.channel_code << " after "
                                 << max_attempts << " writes; observer received "
                                 << final_snapshot.size() << " message(s) in total";
        for (const auto& msg : final_snapshot)
            BOOST_LOG_SEV(lg, error) << "  unexpected message on subject '" << msg.subject << "', "
                                     << msg.data.size() << " bytes";
    }
    REQUIRE_FALSE(received.empty());
    BOOST_LOG_SEV(lg, info) << "Received " << received.size()
                            << " matching NATS notification(s) for notification_preference "
                            << id_str << "/" << v.kind_code << "/" << v.channel_code;

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
        ores::inbox::service::notification_preference_service svc(crud_ctx);
        v.change_commentary = "updated-by-crud-round-trip";
        repo.write(crud_ctx, v);

        auto versions = svc.get_preference_history(id_str, v.kind_code, v.channel_code);
        REQUIRE(versions.size() >= 2);
        REQUIRE(versions.front().change_commentary == "updated-by-crud-round-trip");

        svc.delete_preference(id_str, v.kind_code, v.channel_code);
        // Delete soft-closes the active row (the instead-of delete
        // rule sets valid_to): the row disappears from latest reads,
        // and the version history keeps every version.
        REQUIRE_FALSE(svc.get_preference(id_str, v.kind_code, v.channel_code).has_value());
        REQUIRE(svc.get_preference_history(id_str, v.kind_code, v.channel_code).size() ==
                versions.size());
    }
}

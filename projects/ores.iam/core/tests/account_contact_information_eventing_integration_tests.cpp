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
#include "ores.iam.api/domain/account_contact_information.hpp"
#include "ores.iam.api/domain/account_contact_information_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.api/eventing/account_contact_information_changed_event.hpp"
#include "ores.iam.api/generators/account_contact_information_generator.hpp"
#include "ores.iam.core/repository/account_contact_information_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <thread>

// Proves the "write an entity, observe its NATS entity-changed
// notification" pattern end to end for account_contact_information -- the
// production DB-write -> pg_notify -> postgres_event_source ->
// event_bus -> NATS publish chain, assembled directly here the same
// way the production event-registrar wires it.

namespace {

const std::string_view test_suite("iam.tests");
const std::string tags("[eventing][integration]");


}

using namespace ores::iam::generators;
using ores::iam::domain::account_contact_information;
using ores::iam::repository::account_contact_information_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_account_contact_information_publishes_nats_changed_event", tags) {
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

    auto sub = bus.subscribe<ores::iam::eventing::account_contact_information_changed_event>(
        [&nats](const ores::iam::eventing::account_contact_information_changed_event& e) {
            ev::service::publish_entity_event(
                nats,
                std::string(ev::domain::event_traits<
                            ores::iam::eventing::account_contact_information_changed_event>::name),
                ev::domain::entity_change_event{.entity = "ores.iam.account_contact_information",
                                                .timestamp = e.timestamp,
                                                .entity_ids = e.account_contact_information_ids,
                                                .tenant_id = e.tenant_id});
        });

    event_source.register_mapping<ores::iam::eventing::account_contact_information_changed_event>(
        "ores.iam.account_contact_information", "ores_iam_account_contact_informations");

    // 2. Subscribe as an external observer would, on the relative subject --
    // client::subscribe() prepends the subject_prefix itself.
    auto observer = nats.subscribe_buffered(
        std::string(ev::domain::event_traits<
                    ores::iam::eventing::account_contact_information_changed_event>::name),
        10);

    // The listener thread issues LISTEN asynchronously on its own
    // dedicated connection. Block until it has actually done so before
    // writing -- Postgres does not queue NOTIFYs sent before a matching
    // LISTEN is registered.
    event_source.start();
    REQUIRE(event_source.wait_until_ready());

    // 3. Write -- triggers the entity's notify trigger -> pg_notify ->
    // the chain wired above -> NATS.
    auto v = generate_synthetic_account_contact_information(ctx);
    v.change_reason_code = "system.test";
    const auto id_str = boost::uuids::to_string(v.id);
    BOOST_LOG_SEV(lg, debug) << "Account Contact Information: " << v;

    account_contact_information_repository repo;
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
                auto decoded =
                    ores::nats::default_wire_codec().decode<ev::domain::entity_change_event>(
                        msg.data);
                if (decoded && decoded->entity == "ores.iam.account_contact_information") {
                    for (const auto& changed_id : decoded->entity_ids) {
                        if (changed_id == id_str)
                            received.push_back(msg);
                    }
                }
            }
        }
    }

    event_source.stop();

    if (received.empty()) {
        // Exhausted the budget: report what the observer did see so a
        // genuinely broken chain is diagnosable, not a bare empty check.
        const auto final_snapshot = observer.snapshot();
        BOOST_LOG_SEV(lg, error) << "No notification for account_contact_information " << id_str
                                 << " after " << max_attempts << " writes; observer received "
                                 << final_snapshot.size() << " message(s) in total";
        for (const auto& msg : final_snapshot)
            BOOST_LOG_SEV(lg, error) << "  unexpected message on subject '" << msg.subject << "', "
                                     << msg.data.size() << " bytes";
    }
    REQUIRE_FALSE(received.empty());
    BOOST_LOG_SEV(lg, info) << "Received " << received.size()
                            << " matching NATS notification(s) for account_contact_information "
                            << id_str;
}

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
#include "ores.refdata.api/domain/ois_convention.hpp"
#include "ores.refdata.api/domain/ois_convention_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/eventing/ois_convention_changed_event.hpp"
#include "ores.refdata.api/generators/ois_convention_generator.hpp"
#include "ores.refdata.core/repository/ois_convention_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cstdlib>
#include <thread>

// Proves the "write an entity, observe its NATS entity-changed
// notification" pattern end to end for ois_convention -- the
// production DB-write -> pg_notify -> postgres_event_source ->
// event_bus -> NATS publish chain, assembled directly here the same
// way the production event-registrar wires it.

namespace {

const std::string_view test_suite("refdata.tests");
const std::string tags("[eventing][integration]");


// Reads NATS connection settings the same way every service resolves
// them at startup -- CMake bakes every .env variable into the ctest
// process environment, so these are populated identically for both
// `compass build` local runs and CI.
ores::nats::config::nats_options test_nats_options() {
    auto env = [](const char* name) -> std::string {
        const char* v = std::getenv(name);
        return v ? std::string(v) : std::string();
    };

    ores::nats::config::nats_options opts;
    opts.url = env("ORES_NATS_URL");
    if (opts.url.empty())
        opts.url = "nats://localhost:4222";
    opts.subject_prefix = env("ORES_NATS_SUBJECT_PREFIX");
    opts.tls_ca_cert = env("ORES_NATS_TLS_CA");
    opts.tls_client_cert = env("ORES_NATS_TLS_CERT");
    opts.tls_client_key = env("ORES_NATS_TLS_KEY");
    return opts;
}

}

using namespace ores::refdata::generators;
using ores::refdata::domain::ois_convention;
using ores::refdata::repository::ois_convention_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_ois_convention_publishes_nats_changed_event", tags) {
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

    ores::nats::service::client nats(test_nats_options());
    nats.connect();
    REQUIRE(nats.is_connected());

    auto sub = bus.subscribe<ores::refdata::eventing::ois_convention_changed_event>(
        [&nats](const ores::refdata::eventing::ois_convention_changed_event& e) {
            ev::service::publish_entity_event(
                nats,
                std::string(ev::domain::event_traits<
                            ores::refdata::eventing::ois_convention_changed_event>::name),
                ev::domain::entity_change_event{.entity = "ores.refdata.ois_convention",
                                                .timestamp = e.timestamp,
                                                .entity_ids = e.ids,
                                                .tenant_id = e.tenant_id});
        });

    event_source.register_mapping<ores::refdata::eventing::ois_convention_changed_event>(
        "ores.refdata.ois_convention", "ores_refdata_ois_conventions");

    // 2. Subscribe as an external observer would, on the relative subject --
    // client::subscribe() prepends the subject_prefix itself.
    auto observer = nats.subscribe_buffered(
        std::string(
            ev::domain::event_traits<ores::refdata::eventing::ois_convention_changed_event>::name),
        10);

    // The listener thread issues LISTEN asynchronously on its own
    // dedicated connection. Block until it has actually done so before
    // writing -- Postgres does not queue NOTIFYs sent before a matching
    // LISTEN is registered.
    event_source.start();
    REQUIRE(event_source.wait_until_ready());

    // 3. Write -- triggers the entity's notify trigger -> pg_notify ->
    // the chain wired above -> NATS.
    auto v = generate_synthetic_ois_convention(ctx);
    v.change_reason_code = "system.test";
    const auto id_str = v.id;
    BOOST_LOG_SEV(lg, debug) << "OIS Convention: " << v;

    ois_convention_repository repo;
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
            if (decoded && decoded->entity == "ores.refdata.ois_convention") {
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
                            << " matching NATS notification(s) for ois_convention " << id_str;
}

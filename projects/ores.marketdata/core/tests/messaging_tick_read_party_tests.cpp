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
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/messaging/curve_snapshot_handler.hpp"
#include "ores.marketdata.core/messaging/market_series_handler.hpp"
#include "ores.marketdata.core/messaging/ore_export_handler.hpp"
#include "ores.marketdata.core/messaging/party_required.hpp"
#include "ores.marketdata.core/messaging/series_evolution_handler.hpp"
#include "ores.marketdata.core/messaging/series_slice_handler.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include "tick_handler_support.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

// Proves that every read of ticks refuses a session that names no party, over real
// NATS: the snapshot, the slice, the evolution, the export and the export to storage. The party
// policy of the tick table shows such a session no tick, so any of these reads would answer it
// with a result that looks complete and lacks its live values. Each handler runs on a
// test-scoped subject, because the running service subscribes to the real subjects with its own
// verifier, which would refuse the test token.

namespace {

using namespace ores::marketdata::messaging;
using ores::marketdata::tests::mint_token;
using ores::marketdata::tests::system_party_of_the_test_tenant;
using ores::marketdata::tests::test_audience;
using ores::marketdata::tests::test_issuer;
using ores::marketdata::tests::test_secret;

const std::string snapshot_subject("ores.test.tick_party.snapshot");
const std::string slice_subject("ores.test.tick_party.slice");
const std::string evolution_subject("ores.test.tick_party.evolution");
const std::string export_subject("ores.test.tick_party.export");
const std::string storage_subject("ores.test.tick_party.export_to_storage");

/// The permissions of the five reads, so that the session check is the only one that can refuse.
/// A token carries its permissions in the claim named roles, which is why the fixture hands
/// this list to mint_token as the roles.
const std::vector<std::string> read_roles{"marketdata::curve_snapshots:read",
                                          "marketdata::observations:read",
                                          "marketdata::market_observations:read"};

/// Each handler runs on the NATS worker thread, as the registrar runs it in the service: one
/// instance for each inbound message, with the real database context.
struct handler_fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    std::optional<ores::security::jwt::jwt_authenticator> verifier;
    std::vector<ores::nats::service::subscription> subs;
    boost::uuids::uuid party;

    handler_fixture()
        : nats(ores::testing::make_nats_options())
        , party(system_party_of_the_test_tenant(db)) {
        nats.connect();
        REQUIRE(nats.is_connected());
        verifier = ores::security::jwt::jwt_authenticator::create_hs256(
            test_secret, test_issuer, test_audience);
        const std::string group("ores.marketdata.service");
        subs.push_back(nats.queue_subscribe(snapshot_subject, group, [this](auto msg) {
            curve_snapshot_handler(nats, db.context(), verifier).get_snapshot(std::move(msg));
        }));
        subs.push_back(nats.queue_subscribe(slice_subject, group, [this](auto msg) {
            series_slice_handler(nats, db.context(), verifier).get_slice(std::move(msg));
        }));
        subs.push_back(nats.queue_subscribe(evolution_subject, group, [this](auto msg) {
            series_evolution_handler(nats, db.context(), verifier).get_evolution(std::move(msg));
        }));
        subs.push_back(nats.queue_subscribe(export_subject, group, [this](auto msg) {
            ore_export_handler(nats, db.context(), verifier).write_all(std::move(msg));
        }));
        subs.push_back(nats.queue_subscribe(storage_subject, group, [this](auto msg) {
            market_series_handler(nats, db.context(), verifier)
                .export_to_storage(std::move(msg), "http://localhost:1");
        }));
    }

    std::string token(const std::vector<std::string>& visible) {
        return mint_token(db.tenant_id().to_string(), party, visible, read_roles);
    }

    /// The reply of the handler on @p subject to @p req, decoded as @p Resp.
    template <typename Resp, typename Req>
    Resp ask(const std::string& subject, const Req& req, const std::string& bearer) {
        const std::unordered_map<std::string, std::string> headers{
            {std::string(ores::nats::headers::authorization),
             std::string(ores::nats::headers::bearer_prefix) + bearer}};
        const auto reply =
            nats.request_sync(subject, ores::nats::default_wire_codec().encode(req), headers);
        REQUIRE_FALSE(reply.headers.contains(std::string(ores::nats::headers::x_error)));
        const auto decoded = ores::nats::default_wire_codec().decode<Resp>(reply.data);
        REQUIRE(decoded);
        return *decoded;
    }
};

}

TEST_CASE("every_read_of_ticks_refuses_a_session_that_names_no_party",
          "[tick_read_party][integration]") {
    handler_fixture f;
    const auto bearer = f.token({});
    const auto sentence = party_required_message();

    const auto snapshot =
        f.ask<get_curve_snapshot_response>(snapshot_subject, get_curve_snapshot_request{}, bearer);
    CHECK_FALSE(snapshot.success);
    CHECK(snapshot.message == sentence);

    const auto slice =
        f.ask<get_series_slice_response>(slice_subject, get_series_slice_request{}, bearer);
    CHECK_FALSE(slice.success);
    CHECK(slice.message == sentence);

    const auto evolution = f.ask<get_series_evolution_response>(
        evolution_subject, get_series_evolution_request{}, bearer);
    CHECK_FALSE(evolution.success);
    CHECK(evolution.message == sentence);

    const auto exported =
        f.ask<export_market_data_response>(export_subject, export_market_data_request{}, bearer);
    CHECK_FALSE(exported.success);
    CHECK(exported.message == sentence);

    const auto stored = f.ask<export_market_data_to_storage_response>(
        storage_subject, export_market_data_to_storage_request{}, bearer);
    CHECK_FALSE(stored.success);
    CHECK(stored.message == sentence);
}

// The reads have no series identity to resolve here, so each one fails for some other reason
// and the case states only what the party check owes: a session that names a party is not
// refused with the sentence of party_required. That a party reads its own ticks and no
// others is shown for the history call over NATS, and for the as-of reads at the repository.
TEST_CASE("a_session_that_names_a_party_is_not_refused_for_want_of_one",
          "[tick_read_party][integration]") {
    handler_fixture f;
    const auto bearer = f.token({boost::uuids::to_string(f.party)});
    const auto sentence = party_required_message();

    CHECK(f.ask<get_curve_snapshot_response>(snapshot_subject, get_curve_snapshot_request{}, bearer)
              .message != sentence);
    CHECK(f.ask<get_series_slice_response>(slice_subject, get_series_slice_request{}, bearer)
              .message != sentence);
    CHECK(f.ask<get_series_evolution_response>(
               evolution_subject, get_series_evolution_request{}, bearer)
              .message != sentence);
    CHECK(f.ask<export_market_data_response>(export_subject, export_market_data_request{}, bearer)
              .message != sentence);
    CHECK(f.ask<export_market_data_to_storage_response>(
               storage_subject, export_market_data_to_storage_request{}, bearer)
              .message != sentence);
}

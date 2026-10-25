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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/generators/market_series_generator.hpp"
#include "ores.marketdata.api/generators/subscription_generator.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/messaging/series_ticks_handler.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/subscription_repository.hpp"
#include "ores.marketdata.core/repository/tick_coords_repository.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"
#include "tick_handler_support.hpp"
#include "tick_test_support.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

// Proves the history call end to end over real NATS: a request with a bearer token goes to the
// handler on its own subject, the handler checks the session and the permission, the reader
// answers from the tick table, and the reply decodes on the wire. The handler resolves everything
// from the tenant and the party of the token, so the test seeds ticks under the party that the
// token names. The test subject is test-scoped: the running service subscribes to the real
// subject on the same queue group, and its own verifier would refuse the test token.

namespace {

const std::string tags("[series_ticks_handler][integration]");

const std::string test_subject("ores.test.series_ticks.get");
const std::string read_permission("marketdata::market_observations:read");

using namespace std::chrono_literals;
using ores::marketdata::messaging::get_series_ticks_request;
using ores::marketdata::messaging::get_series_ticks_response;
using ores::marketdata::tests::mint_token;
using ores::marketdata::tests::system_party_of_the_test_tenant;
using ores::marketdata::tests::test_audience;
using ores::marketdata::tests::test_issuer;
using ores::marketdata::tests::test_secret;
using ores::utility::domain::outcome;

std::chrono::system_clock::time_point now_second() {
    return std::chrono::floor<std::chrono::seconds>(std::chrono::system_clock::now());
}

/// The handler runs on the NATS worker thread, as the registrar runs it in the service: one
/// instance for each inbound message, with the real database context.
struct handler_fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    std::optional<ores::security::jwt::jwt_authenticator> verifier;
    std::optional<ores::nats::service::subscription> sub;
    boost::uuids::uuid party;
    ores::database::context owner;
    boost::uuids::uuid series;
    std::string provider;
    boost::uuids::uuid stream;
    std::string curve = ores::marketdata::tests::fresh_curve_id();
    int number;

    handler_fixture()
        : nats(ores::testing::make_nats_options())
        , party(system_party_of_the_test_tenant(db))
        , owner(db.context().with_party(db.tenant_id(), party, {party}, db.db_user()))
        , series(boost::uuids::random_generator{}()) {
        auto generation = ores::testing::make_generation_context(db);
        auto row = ores::marketdata::generators::generate_synthetic_market_series(generation);
        row.id = series;
        row.party_id = party;
        row.oresmd_uri = ores::marketdata::tests::zero_uri(curve);
        ores::marketdata::repository::market_series_repository{}.write(db.context(), row);
        provider = row.provider_code;
        stream = ores::marketdata::tests::stream_of(provider, row.oresmd_uri);
        number = ores::marketdata::repository::tick_coords_repository{}.resolve(
            owner, stream, "oresmd://test/series_ticks_handler");

        // The history read returns a tick only to a party that may display the provider's feeds.
        // The suite reuses its tenant and the system party, and a party holds one subscription
        // to a provider, so a later case finds the first case's row.
        const auto existing = ores::marketdata::repository::subscription_repository{}
                                  .read_latest_by_provider_code(owner, provider);
        if (existing.empty()) {
            auto entitlement =
                ores::marketdata::generators::generate_synthetic_subscription(generation);
            entitlement.tenant_id = db.tenant_id();
            entitlement.party_id = party;
            entitlement.provider_code = provider;
            entitlement.display = true;
            entitlement.non_display = false;
            entitlement.derived = false;
            entitlement.modified_by = db.db_user();
            entitlement.performed_by = db.db_user();
            entitlement.change_reason_code = "system.test";
            entitlement.change_commentary = "Test subscription";
            ores::marketdata::repository::subscription_repository{}.write(db.context(),
                                                                          entitlement);
        }

        nats.connect();
        REQUIRE(nats.is_connected());
        verifier = ores::security::jwt::jwt_authenticator::create_hs256(
            test_secret, test_issuer, test_audience);
        sub = nats.queue_subscribe(
            test_subject, "ores.marketdata.service", [this](ores::nats::message msg) {
                ores::marketdata::messaging::series_ticks_handler h(nats, db.context(), verifier);
                h.get_ticks(std::move(msg));
            });
    }

    std::string token(const std::vector<std::string>& visible,
                      const std::vector<std::string>& roles) {
        return mint_token(db.tenant_id().to_string(), party, visible, roles);
    }

    get_series_ticks_request span(std::chrono::system_clock::time_point from,
                                  std::chrono::system_clock::time_point to) const {
        get_series_ticks_request request;
        request.identity = ores::marketdata::tests::zero_identity(curve);
        request.from_instant = from;
        request.to_instant = to;
        return request;
    }

    /// The reply of the handler to a request that carries @p bearer, or none.
    ores::nats::message request(const get_series_ticks_request& req,
                                const std::optional<std::string>& bearer) {
        std::unordered_map<std::string, std::string> headers;
        if (bearer)
            headers[std::string(ores::nats::headers::authorization)] =
                std::string(ores::nats::headers::bearer_prefix) + *bearer;
        return nats.request_sync(
            test_subject, ores::nats::default_wire_codec().encode(req), headers);
    }
};

get_series_ticks_response decode_reply(const ores::nats::message& reply) {
    REQUIRE_FALSE(reply.headers.contains(std::string(ores::nats::headers::x_error)));
    const auto decoded =
        ores::nats::default_wire_codec().decode<get_series_ticks_response>(reply.data);
    REQUIRE(decoded);
    return *decoded;
}

}

TEST_CASE("a_token_with_the_read_permission_gets_the_ticks_over_nats", tags) {
    handler_fixture f;
    const auto t0 = now_second();
    ores::marketdata::tests::insert_tick(f.owner, f.stream, f.number, t0, 1.25);
    ores::marketdata::tests::insert_tick(f.owner, f.stream, f.number, t0 + 3s, 2.5);

    const auto reply = f.request(f.span(t0 - 1s, t0 + 1min),
                                 f.token({boost::uuids::to_string(f.party)}, {read_permission}));
    const auto response = decode_reply(reply);

    CHECK(response.result.outcome == outcome::ok);
    REQUIRE(response.coordinates.size() == 1);
    CHECK(response.coordinates.front() == "oresmd://test/series_ticks_handler");
    REQUIRE(response.first_instant.has_value());
    CHECK(*response.first_instant == t0);
    CHECK(response.gaps_us == std::vector<std::int64_t>{0, 3000000});
    CHECK(response.coordinate_indexes == std::vector<int>{0, 0});
    CHECK(response.values == std::vector<double>{1.25, 2.5});
}

TEST_CASE("a_token_without_the_read_permission_is_refused_forbidden", tags) {
    handler_fixture f;
    const auto t0 = now_second();
    ores::marketdata::tests::insert_tick(f.owner, f.stream, f.number, t0, 1.0);

    const auto reply =
        f.request(f.span(t0 - 1s, t0 + 1min),
                  f.token({boost::uuids::to_string(f.party)}, {"marketdata::other:read"}));

    REQUIRE(reply.headers.contains(std::string(ores::nats::headers::x_error)));
    CHECK(reply.headers.at(std::string(ores::nats::headers::x_error)) == "forbidden");
}

TEST_CASE("a_request_without_a_token_is_refused_unauthorized", tags) {
    handler_fixture f;
    const auto t0 = now_second();

    const auto reply = f.request(f.span(t0 - 1s, t0 + 1min), std::nullopt);

    REQUIRE(reply.headers.contains(std::string(ores::nats::headers::x_error)));
    CHECK(reply.headers.at(std::string(ores::nats::headers::x_error)) == "unauthorized");
}

TEST_CASE("a_token_that_sees_another_party_gets_an_empty_reply", tags) {
    handler_fixture f;
    const auto t0 = now_second();
    ores::marketdata::tests::insert_tick(f.owner, f.stream, f.number, t0, 1.0);

    const auto stranger = boost::uuids::to_string(boost::uuids::random_generator{}());
    const auto reply =
        f.request(f.span(t0 - 1s, t0 + 1min), f.token({stranger}, {read_permission}));

    const auto response = decode_reply(reply);
    CHECK(response.result.outcome == outcome::ok);
    CHECK(response.values.empty());
}

TEST_CASE("a_token_that_sees_no_party_is_answered_party_required", tags) {
    handler_fixture f;
    const auto t0 = now_second();
    ores::marketdata::tests::insert_tick(f.owner, f.stream, f.number, t0, 1.0);

    const auto reply = f.request(f.span(t0 - 1s, t0 + 1min), f.token({}, {read_permission}));

    const auto response = decode_reply(reply);
    CHECK(response.result.outcome == outcome::denied);
    CHECK(response.result.code == "party_required");
    CHECK(response.values.empty());
}

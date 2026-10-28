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
#include "ores.marketdata.api/generators/provider_generator.hpp"
#include "ores.marketdata.api/messaging/provider_terms_operations_protocol.hpp"
#include "ores.marketdata.core/messaging/provider_terms_handler.hpp"
#include "ores.marketdata.core/repository/provider_repository.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "tick_handler_support.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

// Proves the acceptance of a provider's terms end to end over real NATS: the handler checks the
// permission, refuses a provider that changed after the person read its terms, and records the
// account of the token. The subject is test-scoped, as the running service holds the real one.

namespace provider_terms_test {

const std::string terms_subject("ores.test.provider_terms.accept");
const std::string terms_accept_permission("marketdata::providers:accept_terms");
const std::string terms_reader("terms-test-terms_reader");

using ores::marketdata::messaging::accept_provider_terms_request;
using ores::marketdata::messaging::accept_provider_terms_response;
using ores::marketdata::tests::system_party_of_the_test_tenant;
using ores::marketdata::tests::test_audience;
using ores::marketdata::tests::test_issuer;
using ores::marketdata::tests::test_secret;

struct terms_fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    std::optional<ores::security::jwt::jwt_authenticator> verifier;
    std::optional<ores::nats::service::subscription> sub;
    boost::uuids::uuid party;
    std::string code;

    terms_fixture()
        : nats(ores::testing::make_nats_options())
        , party(system_party_of_the_test_tenant(db))
        , code("terms-" +
               boost::uuids::to_string(boost::uuids::random_generator{}()).substr(0, 8)) {
        nats.connect();
        REQUIRE(nats.is_connected());
        verifier = ores::security::jwt::jwt_authenticator::create_hs256(
            test_secret, test_issuer, test_audience);
        sub = nats.queue_subscribe(
            terms_subject, "ores.marketdata.service", [this](ores::nats::message msg) {
                ores::marketdata::messaging::provider_terms_handler h(nats, db.context(), verifier);
                h.accept(std::move(msg));
            });
    }

    void seed(bool with_terms) {
        auto generation = ores::testing::make_generation_context(db);
        auto row = ores::marketdata::generators::generate_synthetic_provider(generation);
        row.code = code;
        row.terms_url = with_terms ? "https://terms.example/" + code : "";
        row.terms_credit = with_terms ? "Source: the test bank" : "";
        row.allows_storage = with_terms;
        row.terms_accepted_digest.clear();
        row.terms_accepted_by.clear();
        row.terms_accepted_at.reset();
        ores::marketdata::repository::provider_repository{}.write(db.context(), row);
    }

    ores::marketdata::domain::provider latest() {
        const auto rows =
            ores::marketdata::repository::provider_repository{}.read_latest(db.context(), code);
        REQUIRE(rows.size() == 1);
        return rows.front();
    }

    std::string token(const std::vector<std::string>& roles) {
        auto signer = ores::security::jwt::jwt_authenticator::create_hs256(
            test_secret, test_issuer, test_audience);
        auto claims = ores::security::jwt::jwt_claims::with_ttl(std::chrono::minutes(5));
        claims.subject = terms_reader;
        claims.username = terms_reader;
        claims.issuer = test_issuer;
        claims.audience = test_audience;
        claims.tenant_id = db.tenant_id().to_string();
        claims.party_id = boost::uuids::to_string(party);
        claims.visible_party_ids = {boost::uuids::to_string(party)};
        claims.roles = roles;
        return *signer.create_token(claims);
    }

    ores::nats::message accept(int version, const std::vector<std::string>& roles) {
        accept_provider_terms_request req;
        req.provider_code = code;
        req.provider_version = version;
        std::unordered_map<std::string, std::string> headers;
        headers[std::string(ores::nats::headers::authorization)] =
            std::string(ores::nats::headers::bearer_prefix) + token(roles);
        return nats.request_sync(
            terms_subject, ores::nats::default_wire_codec().encode(req), headers);
    }
};

accept_provider_terms_response decode_terms_reply(const ores::nats::message& reply) {
    REQUIRE_FALSE(reply.headers.contains(std::string(ores::nats::headers::x_error)));
    const auto decoded =
        ores::nats::default_wire_codec().decode<accept_provider_terms_response>(reply.data);
    REQUIRE(decoded);
    return *decoded;
}

}

using provider_terms_test::decode_terms_reply;
using provider_terms_test::terms_accept_permission;
using provider_terms_test::terms_fixture;
using provider_terms_test::terms_reader;

TEST_CASE("terms_read_at_the_current_version_are_accepted_by_the_reader",
          "[provider_terms_handler][integration]") {
    terms_fixture f;
    f.seed(true);
    const auto read = f.latest();

    const auto response = decode_terms_reply(f.accept(read.version, {terms_accept_permission}));

    CHECK(response.success);
    CHECK(response.accepted_by == terms_reader);
    const auto after = f.latest();
    CHECK(after.terms_accepted_by == terms_reader);
    CHECK_FALSE(after.terms_accepted_digest.empty());
    CHECK(after.version > read.version);
}

TEST_CASE("terms_read_at_an_older_version_are_refused_and_nothing_is_recorded",
          "[provider_terms_handler][integration]") {
    terms_fixture f;
    f.seed(true);
    const auto read = f.latest();
    f.seed(true);
    const auto changed = f.latest();
    REQUIRE(changed.version > read.version);

    const auto response = decode_terms_reply(f.accept(read.version, {terms_accept_permission}));

    CHECK_FALSE(response.success);
    CHECK(response.message.find("Read them again") != std::string::npos);
    const auto after = f.latest();
    CHECK(after.version == changed.version);
    CHECK(after.terms_accepted_digest.empty());
}

TEST_CASE("version_zero_accepts_the_terms_as_they_stand", "[provider_terms_handler][integration]") {
    terms_fixture f;
    f.seed(true);
    f.seed(true);

    const auto response = decode_terms_reply(f.accept(0, {terms_accept_permission}));

    CHECK(response.success);
    CHECK_FALSE(f.latest().terms_accepted_digest.empty());
}

TEST_CASE("a_provider_without_terms_is_refused", "[provider_terms_handler][integration]") {
    terms_fixture f;
    f.seed(false);

    const auto response = decode_terms_reply(f.accept(0, {terms_accept_permission}));

    CHECK_FALSE(response.success);
    CHECK(response.message.find("states no terms") != std::string::npos);
}

TEST_CASE("a_token_without_the_accept_permission_is_refused_forbidden",
          "[provider_terms_handler][integration]") {
    terms_fixture f;
    f.seed(true);

    const auto reply = f.accept(f.latest().version, {"marketdata::providers:read"});

    REQUIRE(reply.headers.contains(std::string(ores::nats::headers::x_error)));
    CHECK(reply.headers.at(std::string(ores::nats::headers::x_error)) == "forbidden");
    CHECK(f.latest().terms_accepted_digest.empty());
}

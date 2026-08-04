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
#include "ores.iam.core/service/internal_impersonation_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[service][internal_impersonation_service]");

const std::string test_secret("super-secret-key-for-testing-purposes-only-32bytes!");

}

using namespace ores::logging;
using ores::testing::scoped_database_helper;
using ores::iam::service::internal_impersonation_service;

TEST_CASE("mint_token_produces_a_token_validating_to_the_impersonated_identity", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    const auto tenant_id = h.tenant_id().to_string();
    const auto account_id = boost::uuids::random_generator()();
    const auto party_id = boost::uuids::random_generator()();
    const auto other_visible_id = boost::uuids::random_generator()();

    auto signer = ores::security::jwt::jwt_authenticator::create_hs256(test_secret);
    internal_impersonation_service sut(signer, [&](const std::string&, const boost::uuids::uuid&) {
        return std::vector<boost::uuids::uuid>{party_id, other_visible_id};
    });

    const auto token = sut.mint_token(h.context(), tenant_id, account_id, party_id, "some.user");
    REQUIRE_FALSE(token.empty());

    auto claims = signer.validate(token);
    REQUIRE(claims.has_value());
    CHECK(claims->subject == boost::uuids::to_string(account_id));
    CHECK(claims->username == "some.user");
    CHECK(claims->tenant_id == tenant_id);
    CHECK(claims->party_id == boost::uuids::to_string(party_id));
    CHECK(claims->visible_party_ids.size() == 2);
}

TEST_CASE("mint_token_respects_the_requested_ttl", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    const auto tenant_id = h.tenant_id().to_string();
    const auto account_id = boost::uuids::random_generator()();
    const auto party_id = boost::uuids::random_generator()();

    auto signer = ores::security::jwt::jwt_authenticator::create_hs256(test_secret);
    internal_impersonation_service sut(signer, [](const std::string&, const boost::uuids::uuid&) {
        return std::vector<boost::uuids::uuid>{};
    });

    // 120s, not mint_token's own 60s default: keeps this test able to
    // detect a regression where the ttl argument stops being forwarded.
    // Never below a minute -- a sub-minute TTL races wall-clock scheduling
    // jitter between mint and validate against the verifier's zero leeway.
    const auto token = sut.mint_token(
        h.context(), tenant_id, account_id, party_id, "some.user", std::chrono::seconds{120});
    REQUIRE_FALSE(token.empty());

    auto claims = signer.validate(token);
    REQUIRE(claims.has_value());
    CHECK(claims->expires_at <= claims->issued_at + std::chrono::seconds{121});
}

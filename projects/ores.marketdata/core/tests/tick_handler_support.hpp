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
#ifndef ORES_MARKETDATA_CORE_TESTS_TICK_HANDLER_SUPPORT_HPP
#define ORES_MARKETDATA_CORE_TESTS_TICK_HANDLER_SUPPORT_HPP

#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.security/jwt/jwt_claims.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

namespace ores::marketdata::tests {

// The key, the issuer and the audience that the handlers under test accept. The handlers run on
// test-scoped subjects, because the running service subscribes to the real subjects with its own
// verifier, which would refuse a test token.
inline const std::string test_secret("marketdata-handler-test-secret");
inline const std::string test_issuer("ores.iam.test");
inline const std::string test_audience("ores.marketdata.service");

/// The System party that every test tenant holds, which the token validates against.
inline boost::uuids::uuid
system_party_of_the_test_tenant(ores::testing::scoped_database_helper& h) {
    auto lg(ores::logging::make_logger("ores.marketdata.core.tests"));
    const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
        h.context(),
        "SELECT id::text FROM ores_refdata_parties_tbl "
        "WHERE party_category = 'System' AND valid_to = ores_utility_infinity_timestamp_fn() "
        "LIMIT 1",
        {},
        lg,
        "finding the system party of the test tenant");
    REQUIRE(!rows.empty());
    REQUIRE(rows.front().front().has_value());
    return boost::uuids::string_generator()(*rows.front().front());
}

/// Mints a token that the handler's verifier accepts.
inline std::string mint_token(const std::string& tenant_id,
                              const boost::uuids::uuid& party_id,
                              const std::vector<std::string>& visible_party_ids,
                              const std::vector<std::string>& roles) {
    auto signer = ores::security::jwt::jwt_authenticator::create_hs256(
        test_secret, test_issuer, test_audience);
    auto claims = ores::security::jwt::jwt_claims::with_ttl(std::chrono::minutes(5));
    claims.subject = "series-ticks-test";
    claims.issuer = test_issuer;
    claims.audience = test_audience;
    claims.tenant_id = tenant_id;
    claims.party_id = boost::uuids::to_string(party_id);
    claims.visible_party_ids = visible_party_ids;
    claims.roles = roles;
    return *signer.create_token(claims);
}

}

#endif

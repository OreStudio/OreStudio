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
#include "ores.iam.api/domain/account_credential.hpp"
#include "ores.iam.api/domain/account_credential_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.api/domain/account_credential_table.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <sstream>

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[domain]");

}

using ores::iam::domain::account_credential;
using namespace ores::logging;

TEST_CASE("credential_domain_carries_every_secret", tags) {
    auto lg(make_logger(test_suite));

    const auto account_id = boost::uuids::random_generator()();

    account_credential sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.account_id = account_id;
    sut.password_hash = "5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8";
    sut.service_password_hash = "6b3a55e0261b0304143f805a24924d0c1c44524821305f31d9277843b8a1f10f";
    sut.totp_secret = "JBSWY3DPEHPK3PXP";
    sut.modified_by = "admin";
    BOOST_LOG_SEV(lg, info) << "Credential: " << sut;

    CHECK(sut.account_id == account_id);
    CHECK(sut.password_hash.value() ==
          "5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8");
    CHECK(sut.service_password_hash.value() ==
          "6b3a55e0261b0304143f805a24924d0c1c44524821305f31d9277843b8a1f10f");
    CHECK(sut.totp_secret.value() == "JBSWY3DPEHPK3PXP");
}

TEST_CASE("credential_serialisation_carries_no_secret", tags) {
    auto lg(make_logger(test_suite));

    account_credential acc;
    acc.version = 1;
    acc.id = boost::uuids::random_generator()();
    acc.account_id = boost::uuids::random_generator()();
    acc.password_hash = "hash123";
    acc.service_password_hash = "servicehash456";
    acc.totp_secret = "TOTP789";
    acc.modified_by = "admin";

    std::ostringstream os;
    os << acc;
    const auto json = os.str();

    BOOST_LOG_SEV(lg, info) << "JSON: " << json;

    // The struct holds the secrets so the service can prove them. Nothing that
    // leaves the process carries one: no field name and no value.
    CHECK(!json.empty());
    CHECK(json.find("password_hash") == std::string::npos);
    CHECK(json.find("hash123") == std::string::npos);
    CHECK(json.find("service_password_hash") == std::string::npos);
    CHECK(json.find("servicehash456") == std::string::npos);
    CHECK(json.find("totp_secret") == std::string::npos);
    CHECK(json.find("TOTP789") == std::string::npos);
}

TEST_CASE("credential_convert_multiple_to_json_carries_no_secret", tags) {
    auto lg(make_logger(test_suite));

    std::vector<account_credential> credentials;
    for (int i = 0; i < 3; ++i) {
        account_credential acc;
        acc.version = i + 1;
        acc.id = boost::uuids::random_generator()();
        acc.account_id = boost::uuids::random_generator()();
        acc.password_hash = "hash" + std::to_string(i);
        acc.modified_by = "system";
        credentials.push_back(acc);
    }

    for (const auto& acc : credentials) {
        std::ostringstream os;
        os << acc;
        const auto json = os.str();

        CHECK(!json.empty());
        CHECK(json.find(acc.password_hash.value()) == std::string::npos);
    }
}

TEST_CASE("credential_convert_to_table_shows_the_account", tags) {
    auto lg(make_logger(test_suite));

    account_credential acc;
    acc.version = 1;
    acc.id = boost::uuids::random_generator()();
    acc.account_id = boost::uuids::random_generator()();
    acc.modified_by = "admin";

    std::vector<account_credential> credentials = {acc};
    auto table = convert_to_table(credentials);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find(boost::uuids::to_string(acc.account_id)) != std::string::npos);
}

TEST_CASE("credential_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<account_credential> credentials;
    auto table = convert_to_table(credentials);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty());
}

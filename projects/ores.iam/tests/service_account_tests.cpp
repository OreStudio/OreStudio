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
#include "ores.iam/domain/account.hpp"
#include "ores.iam/domain/account_type.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.iam/domain/account_json_io.hpp" // IWYU pragma: keep.

namespace {

const std::string_view test_suite("ores.iam.tests.service_account");
const std::string tags("[service_account]");

}

using ores::iam::domain::account;
using ores::iam::domain::account_type;
using namespace ores::logging;

TEST_CASE("create_service_account_with_no_password", tags) {
    auto lg(make_logger(test_suite));

    account sut;
    sut.version = 1;
    sut.recorded_by = "system";
    sut.id = boost::uuids::random_generator()();
    sut.tenant_id = boost::uuids::nil_uuid(); // System tenant
    sut.account_type = "service";
    sut.username = "ores.service.binary";
    sut.password_hash = "";  // Service accounts have no password
    sut.password_salt = "";
    sut.totp_secret = "";
    sut.email = "binary@system.ores";
    BOOST_LOG_SEV(lg, info) << "Service Account: " << sut;

    CHECK(sut.account_type == "service");
    CHECK(sut.password_hash.empty());
    CHECK(sut.password_salt.empty());
    CHECK(sut.username == "ores.service.binary");
}

TEST_CASE("create_algorithm_account", tags) {
    auto lg(make_logger(test_suite));

    account sut;
    sut.version = 1;
    sut.recorded_by = "system";
    sut.id = boost::uuids::random_generator()();
    sut.tenant_id = boost::uuids::nil_uuid();
    sut.account_type = "algorithm";
    sut.username = "algo.risk.calc";
    sut.password_hash = "";
    sut.password_salt = "";
    sut.totp_secret = "";
    sut.email = "risk.calc@algorithms.ores";
    BOOST_LOG_SEV(lg, info) << "Algorithm Account: " << sut;

    CHECK(sut.account_type == "algorithm");
    CHECK(sut.password_hash.empty());
}

TEST_CASE("create_llm_account", tags) {
    auto lg(make_logger(test_suite));

    account sut;
    sut.version = 1;
    sut.recorded_by = "system";
    sut.id = boost::uuids::random_generator()();
    sut.tenant_id = boost::uuids::nil_uuid();
    sut.account_type = "llm";
    sut.username = "claude.agent";
    sut.password_hash = "";
    sut.password_salt = "";
    sut.totp_secret = "";
    sut.email = "claude@llm.ores";
    BOOST_LOG_SEV(lg, info) << "LLM Account: " << sut;

    CHECK(sut.account_type == "llm");
    CHECK(sut.password_hash.empty());
}

TEST_CASE("user_account_requires_password", tags) {
    auto lg(make_logger(test_suite));

    account sut;
    sut.version = 1;
    sut.recorded_by = "admin";
    sut.id = boost::uuids::random_generator()();
    sut.tenant_id = boost::uuids::random_generator()();
    sut.account_type = "user";  // Default type
    sut.username = "john.doe";
    sut.password_hash = "5e884898da28047151d0e56f8dc6292773603d0d6aabbdd62a11ef721d1542d8";
    sut.password_salt = "salt_value";
    sut.totp_secret = "";
    sut.email = "john.doe@example.com";
    BOOST_LOG_SEV(lg, info) << "User Account: " << sut;

    CHECK(sut.account_type == "user");
    CHECK(!sut.password_hash.empty());
}

TEST_CASE("account_type_defaults_to_user", tags) {
    auto lg(make_logger(test_suite));

    account sut;
    sut.id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Default Account Type: " << sut.account_type;

    CHECK(sut.account_type == "user");
}

TEST_CASE("account_type_domain_struct", tags) {
    auto lg(make_logger(test_suite));

    account_type sut;
    sut.version = 1;
    sut.type = "service";
    sut.name = "Service";
    sut.description = "Service account for non-human processes";
    sut.display_order = 10;
    sut.recorded_by = "system";
    sut.change_reason_code = "system.initial_load";
    sut.change_commentary = "Initial population";

    CHECK(sut.type == "service");
    CHECK(sut.name == "Service");
    CHECK(sut.display_order == 10);
}

TEST_CASE("account_performed_by_field", tags) {
    auto lg(make_logger(test_suite));

    account sut;
    sut.id = boost::uuids::random_generator()();
    sut.account_type = "service";
    sut.username = "test.service";

    // performed_by is required and defaults to empty string
    CHECK(sut.performed_by.empty());

    // Can be set to an account username
    sut.performed_by = "test.service";
    CHECK(sut.performed_by == "test.service");
}

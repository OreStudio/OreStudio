/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.iam/domain/permission.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.iam/domain/permission_json_io.hpp" // IWYU pragma: keep.

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[domain]");

}

using ores::iam::domain::permission;
using namespace ores::iam::domain::permissions;
using namespace ores::telemetry::log;

TEST_CASE("create_permission_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    permission sut;
    sut.id = boost::uuids::random_generator()();
    sut.code = accounts_create;
    sut.description = "Allows creating new user accounts";
    BOOST_LOG_SEV(lg, info) << "Permission: " << sut;

    CHECK(sut.code == "accounts:create");
    CHECK(!sut.description.empty());
}

TEST_CASE("create_permission_with_wildcard", tags) {
    auto lg(make_logger(test_suite));

    permission sut;
    sut.id = boost::uuids::random_generator()();
    sut.code = all;
    sut.description = "Grants all permissions (superuser)";
    BOOST_LOG_SEV(lg, info) << "Permission: " << sut;

    CHECK(sut.code == "*");
    CHECK(!sut.description.empty());
}

TEST_CASE("create_currency_permission", tags) {
    auto lg(make_logger(test_suite));

    permission sut;
    sut.id = boost::uuids::random_generator()();
    sut.code = currencies_read;
    sut.description = "Allows reading currency information";
    BOOST_LOG_SEV(lg, info) << "Permission: " << sut;

    CHECK(sut.code == "currencies:read");
}

TEST_CASE("create_flags_permission", tags) {
    auto lg(make_logger(test_suite));

    permission sut;
    sut.id = boost::uuids::random_generator()();
    sut.code = flags_update;
    sut.description = "Allows updating feature flags";
    BOOST_LOG_SEV(lg, info) << "Permission: " << sut;

    CHECK(sut.code == "flags:update");
}

TEST_CASE("create_roles_permission", tags) {
    auto lg(make_logger(test_suite));

    permission sut;
    sut.id = boost::uuids::random_generator()();
    sut.code = roles_assign;
    sut.description = "Allows assigning roles to accounts";
    BOOST_LOG_SEV(lg, info) << "Permission: " << sut;

    CHECK(sut.code == "roles:assign");
}

TEST_CASE("permission_with_specific_uuid", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator uuid_gen;
    const auto specific_id = uuid_gen("550e8400-e29b-41d4-a716-446655440000");

    permission sut;
    sut.id = specific_id;
    sut.code = accounts_read;
    sut.description = "Read account data";
    BOOST_LOG_SEV(lg, info) << "Permission: " << sut;

    CHECK(sut.code == "accounts:read");
}

TEST_CASE("permission_serialization_to_json", tags) {
    auto lg(make_logger(test_suite));

    permission sut;
    sut.id = boost::uuids::random_generator()();
    sut.code = accounts_delete;
    sut.description = "Allows deleting user accounts";
    BOOST_LOG_SEV(lg, info) << "Permission: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("accounts:delete") != std::string::npos);
    CHECK(json_output.find("Allows deleting user accounts") != std::string::npos);
}

TEST_CASE("create_permission_with_faker", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> resources = {"accounts", "currencies", "flags", "roles"};
    const std::vector<std::string> actions = {"create", "read", "update", "delete"};

    permission sut;
    sut.id = boost::uuids::random_generator()();
    sut.code = resources[faker::number::integer(0, 3)] + ":" +
               actions[faker::number::integer(0, 3)];
    sut.description = std::string(faker::lorem::sentence());
    BOOST_LOG_SEV(lg, info) << "Permission: " << sut;

    CHECK(!sut.code.empty());
    CHECK(sut.code.find(":") != std::string::npos);
    CHECK(!sut.description.empty());
}

TEST_CASE("create_multiple_random_permissions", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> codes = {
        accounts_create, accounts_read, accounts_update, accounts_delete,
        currencies_create, currencies_read, currencies_update, currencies_delete,
        flags_create, flags_read, flags_update, flags_delete
    };

    for (int i = 0; i < 3; ++i) {
        permission sut;
        sut.id = boost::uuids::random_generator()();
        sut.code = codes[faker::number::integer(0, static_cast<int>(codes.size() - 1))];
        sut.description = std::string(faker::lorem::sentence());
        BOOST_LOG_SEV(lg, info) << "Permission " << i << ":" << sut;

        CHECK(!sut.code.empty());
        CHECK(!sut.description.empty());
    }
}

TEST_CASE("well_known_permission_codes_are_consistent", tags) {
    auto lg(make_logger(test_suite));

    CHECK(std::string(accounts_create) == "accounts:create");
    CHECK(std::string(accounts_read) == "accounts:read");
    CHECK(std::string(accounts_update) == "accounts:update");
    CHECK(std::string(accounts_delete) == "accounts:delete");
    CHECK(std::string(accounts_lock) == "accounts:lock");
    CHECK(std::string(accounts_unlock) == "accounts:unlock");
    CHECK(std::string(accounts_reset_password) == "accounts:reset_password");

    CHECK(std::string(currencies_create) == "currencies:create");
    CHECK(std::string(currencies_read) == "currencies:read");
    CHECK(std::string(currencies_update) == "currencies:update");
    CHECK(std::string(currencies_delete) == "currencies:delete");
    CHECK(std::string(currencies_history) == "currencies:history");

    CHECK(std::string(flags_create) == "flags:create");
    CHECK(std::string(flags_read) == "flags:read");
    CHECK(std::string(flags_update) == "flags:update");
    CHECK(std::string(flags_delete) == "flags:delete");

    CHECK(std::string(login_info_read) == "login_info:read");

    CHECK(std::string(roles_create) == "roles:create");
    CHECK(std::string(roles_read) == "roles:read");
    CHECK(std::string(roles_update) == "roles:update");
    CHECK(std::string(roles_delete) == "roles:delete");
    CHECK(std::string(roles_assign) == "roles:assign");
    CHECK(std::string(roles_revoke) == "roles:revoke");

    CHECK(std::string(all) == "*");

    BOOST_LOG_SEV(lg, info) << "All well-known permission codes validated";
}

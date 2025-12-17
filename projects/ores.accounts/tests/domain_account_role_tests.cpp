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
#include "ores.accounts/domain/account_role.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.utility/log/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.accounts.tests");
const std::string tags("[domain]");

}

using ores::accounts::domain::account_role;
using namespace ores::utility::log;

TEST_CASE("create_account_role_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    account_role sut;
    sut.account_id = boost::uuids::random_generator()();
    sut.role_id = boost::uuids::random_generator()();
    sut.assigned_by = "admin";
    sut.assigned_at = "2025-01-15T10:00:00Z";
    BOOST_LOG_SEV(lg, info) << "Account role - account_id: " << sut.account_id
        << ", role_id: " << sut.role_id << ", assigned_by: " << sut.assigned_by
        << ", assigned_at: " << sut.assigned_at;

    CHECK(!sut.account_id.is_nil());
    CHECK(!sut.role_id.is_nil());
    CHECK(sut.assigned_by == "admin");
    CHECK(sut.assigned_at == "2025-01-15T10:00:00Z");
}

TEST_CASE("create_account_role_with_specific_uuids", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator uuid_gen;
    const auto account_uuid = uuid_gen("550e8400-e29b-41d4-a716-446655440000");
    const auto role_uuid = uuid_gen("660e8400-e29b-41d4-a716-446655440001");

    account_role sut;
    sut.account_id = account_uuid;
    sut.role_id = role_uuid;
    sut.assigned_by = "system";
    sut.assigned_at = "2025-01-01T00:00:00Z";
    BOOST_LOG_SEV(lg, info) << "Account role - account_id: " << sut.account_id
        << ", role_id: " << sut.role_id << ", assigned_by: " << sut.assigned_by
        << ", assigned_at: " << sut.assigned_at;

    CHECK(sut.account_id == account_uuid);
    CHECK(sut.role_id == role_uuid);
    CHECK(sut.assigned_by == "system");
}

TEST_CASE("account_role_same_account_multiple_roles", tags) {
    auto lg(make_logger(test_suite));

    const auto account_id = boost::uuids::random_generator()();
    std::vector<account_role> assignments;

    for (int i = 0; i < 3; ++i) {
        account_role ar;
        ar.account_id = account_id;
        ar.role_id = boost::uuids::random_generator()();
        ar.assigned_by = "admin";
        ar.assigned_at = "2025-01-" + std::to_string(15 + i) + "T10:00:00Z";
        assignments.push_back(ar);
        BOOST_LOG_SEV(lg, info) << "Account role " << i << " - account_id: "
            << ar.account_id << ", role_id: " << ar.role_id;
    }

    CHECK(assignments.size() == 3);
    for (const auto& ar : assignments) {
        CHECK(ar.account_id == account_id);
    }
}

TEST_CASE("account_role_same_role_multiple_accounts", tags) {
    auto lg(make_logger(test_suite));

    const auto role_id = boost::uuids::random_generator()();
    std::vector<account_role> assignments;

    for (int i = 0; i < 3; ++i) {
        account_role ar;
        ar.account_id = boost::uuids::random_generator()();
        ar.role_id = role_id;
        ar.assigned_by = "admin";
        ar.assigned_at = "2025-01-15T1" + std::to_string(i) + ":00:00Z";
        assignments.push_back(ar);
        BOOST_LOG_SEV(lg, info) << "Account role " << i << " - account_id: "
            << ar.account_id << ", role_id: " << ar.role_id;
    }

    CHECK(assignments.size() == 3);
    for (const auto& ar : assignments) {
        CHECK(ar.role_id == role_id);
    }
}

TEST_CASE("create_account_role_with_faker", tags) {
    auto lg(make_logger(test_suite));

    account_role sut;
    sut.account_id = boost::uuids::random_generator()();
    sut.role_id = boost::uuids::random_generator()();
    sut.assigned_by = std::string(faker::internet::username());
    sut.assigned_at = "2025-" +
        std::to_string(faker::number::integer(1, 12)) + "-" +
        std::to_string(faker::number::integer(1, 28)) + "T" +
        std::to_string(faker::number::integer(0, 23)) + ":00:00Z";
    BOOST_LOG_SEV(lg, info) << "Account role - account_id: " << sut.account_id
        << ", role_id: " << sut.role_id << ", assigned_by: " << sut.assigned_by
        << ", assigned_at: " << sut.assigned_at;

    CHECK(!sut.account_id.is_nil());
    CHECK(!sut.role_id.is_nil());
    CHECK(!sut.assigned_by.empty());
    CHECK(!sut.assigned_at.empty());
}

TEST_CASE("create_multiple_random_account_roles", tags) {
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 5; ++i) {
        account_role sut;
        sut.account_id = boost::uuids::random_generator()();
        sut.role_id = boost::uuids::random_generator()();
        sut.assigned_by = std::string(faker::person::firstName()) + " " +
            std::string(faker::person::lastName());
        sut.assigned_at = "2025-01-" +
            std::to_string(faker::number::integer(1, 28)) + "T10:00:00Z";
        BOOST_LOG_SEV(lg, info) << "Account role " << i << " - account_id: "
            << sut.account_id << ", role_id: " << sut.role_id
            << ", assigned_by: " << sut.assigned_by;

        CHECK(!sut.account_id.is_nil());
        CHECK(!sut.role_id.is_nil());
        CHECK(!sut.assigned_by.empty());
        CHECK(!sut.assigned_at.empty());
    }
}

TEST_CASE("account_role_default_values", tags) {
    auto lg(make_logger(test_suite));

    account_role sut;
    BOOST_LOG_SEV(lg, info) << "Default account role - account_id: "
        << sut.account_id << ", role_id: " << sut.role_id
        << ", assigned_by: '" << sut.assigned_by << "'"
        << ", assigned_at: '" << sut.assigned_at << "'";

    CHECK(sut.account_id.is_nil());
    CHECK(sut.role_id.is_nil());
    CHECK(sut.assigned_by.empty());
    CHECK(sut.assigned_at.empty());
}

TEST_CASE("account_role_assignment_with_different_assigners", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> assigners = {"admin", "system", "operator", "superuser"};

    for (const auto& assigner : assigners) {
        account_role sut;
        sut.account_id = boost::uuids::random_generator()();
        sut.role_id = boost::uuids::random_generator()();
        sut.assigned_by = assigner;
        sut.assigned_at = "2025-01-15T10:00:00Z";
        BOOST_LOG_SEV(lg, info) << "Account role assigned by: " << sut.assigned_by;

        CHECK(sut.assigned_by == assigner);
    }
}

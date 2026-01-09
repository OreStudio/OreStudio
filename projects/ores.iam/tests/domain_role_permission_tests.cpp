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
#include "ores.iam/domain/role_permission.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[domain]");

}

using ores::iam::domain::role_permission;
using namespace ores::logging;

TEST_CASE("create_role_permission_with_valid_uuids", tags) {
    auto lg(make_logger(test_suite));

    role_permission sut;
    sut.role_id = boost::uuids::random_generator()();
    sut.permission_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Role permission - role_id: "
        << sut.role_id << ", permission_id: " << sut.permission_id;

    CHECK(!sut.role_id.is_nil());
    CHECK(!sut.permission_id.is_nil());
}

TEST_CASE("create_role_permission_with_specific_uuids", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator uuid_gen;
    const auto role_uuid = uuid_gen("550e8400-e29b-41d4-a716-446655440000");
    const auto perm_uuid = uuid_gen("660e8400-e29b-41d4-a716-446655440001");

    role_permission sut;
    sut.role_id = role_uuid;
    sut.permission_id = perm_uuid;
    BOOST_LOG_SEV(lg, info) << "Role permission - role_id: "
        << sut.role_id << ", permission_id: " << sut.permission_id;

    CHECK(sut.role_id == role_uuid);
    CHECK(sut.permission_id == perm_uuid);
}

TEST_CASE("role_permission_same_role_different_permissions", tags) {
    auto lg(make_logger(test_suite));

    const auto role_id = boost::uuids::random_generator()();
    std::vector<role_permission> assignments;

    for (int i = 0; i < 3; ++i) {
        role_permission rp;
        rp.role_id = role_id;
        rp.permission_id = boost::uuids::random_generator()();
        assignments.push_back(rp);
        BOOST_LOG_SEV(lg, info) << "Role permission " << i << " - role_id: "
            << rp.role_id << ", permission_id: " << rp.permission_id;
    }

    CHECK(assignments.size() == 3);
    for (const auto& rp : assignments) {
        CHECK(rp.role_id == role_id);
    }
}

TEST_CASE("role_permission_same_permission_different_roles", tags) {
    auto lg(make_logger(test_suite));

    const auto permission_id = boost::uuids::random_generator()();
    std::vector<role_permission> assignments;

    for (int i = 0; i < 3; ++i) {
        role_permission rp;
        rp.role_id = boost::uuids::random_generator()();
        rp.permission_id = permission_id;
        assignments.push_back(rp);
        BOOST_LOG_SEV(lg, info) << "Role permission " << i << " - role_id: "
            << rp.role_id << ", permission_id: " << rp.permission_id;
    }

    CHECK(assignments.size() == 3);
    for (const auto& rp : assignments) {
        CHECK(rp.permission_id == permission_id);
    }
}

TEST_CASE("create_multiple_random_role_permissions", tags) {
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 5; ++i) {
        role_permission sut;
        sut.role_id = boost::uuids::random_generator()();
        sut.permission_id = boost::uuids::random_generator()();
        BOOST_LOG_SEV(lg, info) << "Role permission " << i << " - role_id: "
            << sut.role_id << ", permission_id: " << sut.permission_id;

        CHECK(!sut.role_id.is_nil());
        CHECK(!sut.permission_id.is_nil());
        CHECK(sut.role_id != sut.permission_id);
    }
}

TEST_CASE("role_permission_default_uuids_are_nil", tags) {
    auto lg(make_logger(test_suite));

    role_permission sut;
    BOOST_LOG_SEV(lg, info) << "Default role permission - role_id: "
        << sut.role_id << ", permission_id: " << sut.permission_id;

    CHECK(sut.role_id.is_nil());
    CHECK(sut.permission_id.is_nil());
}

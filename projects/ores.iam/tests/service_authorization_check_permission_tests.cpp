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
#include "ores.iam/service/authorization_service.hpp"
#include "ores.iam/domain/permission.hpp"

#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[authorization]");

using ores::iam::service::authorization_service;
namespace permissions = ores::iam::domain::permissions;

}

TEST_CASE("check_permission_returns_true_for_exact_match", tags) {
    // Sorted list as required by binary_search
    std::vector<std::string> perms = {
        permissions::accounts_create,
        permissions::accounts_read,
        permissions::roles_read
    };
    std::ranges::sort(perms);

    CHECK(authorization_service::check_permission(perms, permissions::accounts_read));
    CHECK(authorization_service::check_permission(perms, permissions::accounts_create));
    CHECK(authorization_service::check_permission(perms, permissions::roles_read));
}

TEST_CASE("check_permission_returns_false_for_missing_permission", tags) {
    std::vector<std::string> perms = {
        permissions::accounts_read,
        permissions::roles_read
    };
    std::ranges::sort(perms);

    CHECK_FALSE(authorization_service::check_permission(perms, permissions::accounts_delete));
    CHECK_FALSE(authorization_service::check_permission(perms, permissions::roles_assign));
}

TEST_CASE("check_permission_wildcard_grants_all", tags) {
    std::vector<std::string> perms = {permissions::all};

    CHECK(authorization_service::check_permission(perms, permissions::accounts_create));
    CHECK(authorization_service::check_permission(perms, permissions::roles_assign));
    CHECK(authorization_service::check_permission(perms, "anything::at:all"));
}

TEST_CASE("check_permission_empty_list_returns_false", tags) {
    std::vector<std::string> perms;

    CHECK_FALSE(authorization_service::check_permission(perms, permissions::accounts_read));
}

TEST_CASE("check_permission_is_case_sensitive", tags) {
    std::vector<std::string> perms = {permissions::accounts_read};

    CHECK(authorization_service::check_permission(perms, permissions::accounts_read));
    CHECK_FALSE(authorization_service::check_permission(perms, "IAM::ACCOUNTS:READ"));
}

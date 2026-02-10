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
#include "ores.iam/repository/permission_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.iam/domain/permission.hpp"
#include "ores.iam/domain/permission_json_io.hpp" // IWYU pragma: keep.
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[repository]");

using ores::iam::domain::permission;

permission make_permission(ores::testing::database_helper& h) {
    permission p;
    p.tenant_id = h.tenant_id();
    p.id = boost::uuids::random_generator()();
    p.code = std::string(faker::string::alphanumeric(6)) + "::"
        + std::string(faker::string::alphanumeric(6)) + ":"
        + std::string(faker::string::alphanumeric(6));
    p.description = std::string(faker::lorem::sentence());
    return p;
}

}

using namespace ores::logging;

using ores::testing::database_helper;
using ores::iam::repository::permission_repository;

TEST_CASE("write_single_permission", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    permission_repository repo(h.context());
    auto p = make_permission(h);

    BOOST_LOG_SEV(lg, debug) << "Permission: " << p;
    CHECK_NOTHROW(repo.write(p));
}

TEST_CASE("read_latest_permissions", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    permission_repository repo(h.context());
    auto p = make_permission(h);

    BOOST_LOG_SEV(lg, debug) << "Permission: " << p;
    repo.write(p);

    auto read_perms = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read permissions: " << read_perms;

    CHECK(!read_perms.empty());
}

TEST_CASE("read_latest_permission_by_id", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    permission_repository repo(h.context());
    auto p = make_permission(h);
    const auto target_id = p.id;

    BOOST_LOG_SEV(lg, debug) << "Permission: " << p;
    repo.write(p);

    BOOST_LOG_SEV(lg, debug) << "Target ID: " << target_id;

    auto read_perms = repo.read_latest(target_id);
    BOOST_LOG_SEV(lg, debug) << "Read permissions: " << read_perms;

    REQUIRE(read_perms.size() == 1);
    CHECK(read_perms[0].id == target_id);
    CHECK(read_perms[0].code == p.code);
}

TEST_CASE("read_nonexistent_permission", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    permission_repository repo(h.context());

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_perms = repo.read_latest(nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read permissions: " << read_perms;

    CHECK(read_perms.size() == 0);
}

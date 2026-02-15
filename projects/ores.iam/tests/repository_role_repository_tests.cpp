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
#include "ores.iam/repository/role_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.iam/domain/role.hpp"
#include "ores.iam/domain/role_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/generators/role_generator.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::iam::generators;

using ores::testing::database_helper;
using ores::iam::repository::role_repository;

TEST_CASE("write_single_role", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);

    role_repository repo(h.context());
    auto r = generate_synthetic_role(gen_ctx);

    BOOST_LOG_SEV(lg, debug) << "Role: " << r;
    CHECK_NOTHROW(repo.write(r));
}

TEST_CASE("read_latest_roles", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);

    role_repository repo(h.context());
    auto r = generate_synthetic_role(gen_ctx);

    BOOST_LOG_SEV(lg, debug) << "Role: " << r;
    repo.write(r);

    auto read_roles = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read roles: " << read_roles;

    CHECK(!read_roles.empty());
}

TEST_CASE("read_latest_role_by_id", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);

    role_repository repo(h.context());
    auto r = generate_synthetic_role(gen_ctx);
    const auto target_id = r.id;

    BOOST_LOG_SEV(lg, debug) << "Role: " << r;
    repo.write(r);

    BOOST_LOG_SEV(lg, debug) << "Target ID: " << target_id;

    auto read_roles = repo.read_latest(target_id);
    BOOST_LOG_SEV(lg, debug) << "Read roles: " << read_roles;

    REQUIRE(read_roles.size() == 1);
    CHECK(read_roles[0].id == target_id);
    CHECK(read_roles[0].name == r.name);
}

TEST_CASE("read_nonexistent_role", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    role_repository repo(h.context());

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_roles = repo.read_latest(nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read roles: " << read_roles;

    CHECK(read_roles.size() == 0);
}

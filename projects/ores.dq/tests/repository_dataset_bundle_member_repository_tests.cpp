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
#include "ores.dq/repository/dataset_bundle_member_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/dataset_bundle_member_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/generators/dataset_bundle_member_generator.hpp"
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::dq::generators;

using ores::testing::database_helper;
using ores::dq::repository::dataset_bundle_member_repository;

TEST_CASE("write_single_dataset_bundle_member", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    dataset_bundle_member_repository repo(h.context());
    auto member = generate_synthetic_dataset_bundle_member();

    BOOST_LOG_SEV(lg, debug) << "Dataset bundle member: " << member;
    CHECK_NOTHROW(repo.write(member));
}

TEST_CASE("write_multiple_dataset_bundle_members", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    dataset_bundle_member_repository repo(h.context());
    auto members = generate_synthetic_dataset_bundle_members(3);
    BOOST_LOG_SEV(lg, debug) << "Dataset bundle members: " << members;

    CHECK_NOTHROW(repo.write(members));
}

TEST_CASE("read_latest_dataset_bundle_members", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    dataset_bundle_member_repository repo(h.context());
    auto written_members = generate_synthetic_dataset_bundle_members(3);
    BOOST_LOG_SEV(lg, debug) << "Written members: " << written_members;

    repo.write(written_members);

    auto read_members = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read members: " << read_members;

    CHECK(!read_members.empty());
    CHECK(read_members.size() >= written_members.size());
}

TEST_CASE("read_latest_dataset_bundle_members_by_bundle", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    dataset_bundle_member_repository repo(h.context());
    auto member = generate_synthetic_dataset_bundle_member();

    const auto target_bundle_code = member.bundle_code;
    BOOST_LOG_SEV(lg, debug) << "Write member: " << member;
    repo.write(member);

    BOOST_LOG_SEV(lg, debug) << "Target bundle code: " << target_bundle_code;

    auto read_members = repo.read_latest_by_bundle(target_bundle_code);
    BOOST_LOG_SEV(lg, debug) << "Read members: " << read_members;

    REQUIRE(!read_members.empty());
    for (const auto& m : read_members) {
        CHECK(m.bundle_code == target_bundle_code);
    }
}

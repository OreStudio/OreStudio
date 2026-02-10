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
#include "ores.iam/repository/account_party_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.iam/domain/account_party.hpp"
#include "ores.iam/domain/account_party_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/generators/account_party_generator.hpp"
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::iam::generators;

using ores::testing::database_helper;
using ores::iam::domain::account_party;
using ores::iam::repository::account_party_repository;

TEST_CASE("write_single_account_party", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    account_party_repository repo(h.context());
    auto ap = generate_synthetic_account_party();
    ap.tenant_id = h.tenant_id().to_string();

    BOOST_LOG_SEV(lg, debug) << "Account party: " << ap;
    CHECK_NOTHROW(repo.write(ap));
}

TEST_CASE("write_multiple_account_parties", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    account_party_repository repo(h.context());
    auto aps = generate_synthetic_account_parties(5);
    for (auto& ap : aps)
        ap.tenant_id = h.tenant_id().to_string();

    BOOST_LOG_SEV(lg, debug) << "Account parties: " << aps;
    CHECK_NOTHROW(repo.write(aps));
}

TEST_CASE("read_latest_account_parties", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    account_party_repository repo(h.context());
    auto written = generate_synthetic_account_parties(3);
    for (auto& ap : written)
        ap.tenant_id = h.tenant_id().to_string();

    BOOST_LOG_SEV(lg, debug) << "Written account parties: " << written;
    repo.write(written);

    auto read_aps = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read account parties: " << read_aps;

    CHECK(!read_aps.empty());
    CHECK(read_aps.size() >= written.size());
}

TEST_CASE("read_latest_account_parties_by_account", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    account_party_repository repo(h.context());
    auto aps = generate_synthetic_account_parties(3);
    for (auto& ap : aps)
        ap.tenant_id = h.tenant_id().to_string();

    const auto target_account_id = aps.front().account_id;
    BOOST_LOG_SEV(lg, debug) << "Written account parties: " << aps;
    repo.write(aps);

    BOOST_LOG_SEV(lg, debug) << "Target account ID: " << target_account_id;

    auto read_aps = repo.read_latest_by_account(target_account_id);
    BOOST_LOG_SEV(lg, debug) << "Read account parties: " << read_aps;

    REQUIRE(read_aps.size() >= 1);
    CHECK(read_aps[0].account_id == target_account_id);
}

TEST_CASE("read_nonexistent_account_party", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    account_party_repository repo(h.context());

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent account ID: " << nonexistent_id;

    auto read_aps = repo.read_latest_by_account(nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read account parties: " << read_aps;

    CHECK(read_aps.size() == 0);
}

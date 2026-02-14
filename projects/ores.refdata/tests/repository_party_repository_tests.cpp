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
#include "ores.refdata/repository/party_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/party.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/party_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/generators/party_generator.hpp"

using namespace ores::refdata::generators;
using ores::refdata::domain::party;
using ores::refdata::repository::party_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

TEST_CASE("write_single_party", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    party_repository repo(h.context());
    const auto parent_id = repo.read_system_party(h.tenant_id().to_string()).at(0).id;

    auto p = generate_synthetic_party(ctx);
    p.change_reason_code = "system.test";
    p.parent_party_id = parent_id;
    BOOST_LOG_SEV(lg, debug) << "Party: " << p;

    CHECK_NOTHROW(repo.write(p));
}

TEST_CASE("write_multiple_parties", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    party_repository repo(h.context());
    const auto parent_id = repo.read_system_party(h.tenant_id().to_string()).at(0).id;

    auto parties = generate_synthetic_parties(3, ctx);
    for (auto& p : parties) {
        p.change_reason_code = "system.test";
        p.parent_party_id = parent_id;
    }
    BOOST_LOG_SEV(lg, debug) << "Parties: " << parties;

    CHECK_NOTHROW(repo.write(parties));
}

TEST_CASE("read_latest_parties", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    party_repository repo(h.context());
    const auto parent_id = repo.read_system_party(h.tenant_id().to_string()).at(0).id;

    auto written_parties = generate_synthetic_parties(3, ctx);
    for (auto& p : written_parties) {
        p.change_reason_code = "system.test";
        p.parent_party_id = parent_id;
    }
    BOOST_LOG_SEV(lg, debug) << "Written parties: " << written_parties;

    repo.write(written_parties);

    auto read_parties = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read parties: " << read_parties;

    CHECK(read_parties.size() >= written_parties.size());
}

TEST_CASE("read_latest_party_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    party_repository repo(h.context());
    const auto parent_id = repo.read_system_party(h.tenant_id().to_string()).at(0).id;

    auto p = generate_synthetic_party(ctx);
    p.change_reason_code = "system.test";
    p.parent_party_id = parent_id;
    const auto original_full_name = p.full_name;
    BOOST_LOG_SEV(lg, debug) << "Party: " << p;

    repo.write(p);

    p.full_name = original_full_name + " v2";
    repo.write(p);

    auto read_parties = repo.read_latest(p.id);
    BOOST_LOG_SEV(lg, debug) << "Read parties: " << read_parties;

    REQUIRE(read_parties.size() == 1);
    CHECK(read_parties[0].id == p.id);
    CHECK(read_parties[0].full_name == original_full_name + " v2");
}

TEST_CASE("read_nonexistent_party_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    party_repository repo(h.context());

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_parties = repo.read_latest(nonexistent_id);
    BOOST_LOG_SEV(lg, debug) << "Read parties: " << read_parties;

    CHECK(read_parties.size() == 0);
}

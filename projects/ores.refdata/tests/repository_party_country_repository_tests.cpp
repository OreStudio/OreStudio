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
#include "ores.refdata/repository/party_country_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/party_country.hpp"
#include "ores.refdata/domain/party_country_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/repository/party_repository.hpp"
#include "ores.refdata/repository/country_repository.hpp"
#include "ores.refdata/generators/party_generator.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"

using namespace ores::logging;
using namespace ores::refdata::generators;

using ores::refdata::domain::party_country;
using ores::refdata::repository::party_country_repository;
using ores::refdata::repository::party_repository;
using ores::refdata::repository::country_repository;
using ores::testing::scoped_database_helper;

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository][party_country]");

boost::uuids::uuid find_system_party_id(
    party_repository& repo, const std::string& tid) {
    auto parties = repo.read_latest();
    for (const auto& p : parties)
        if (p.tenant_id == tid)
            return p.id;
    throw std::runtime_error("No system party for tenant: " + tid);
}

std::string find_first_country_alpha2_code(
    country_repository& repo, scoped_database_helper& h) {
    auto countries = repo.read_latest(h.context());
    if (countries.empty())
        throw std::runtime_error("No countries found in database");
    return countries.front().alpha2_code;
}

party_country make_party_country(scoped_database_helper& h,
    const boost::uuids::uuid& party_id,
    const std::string& alpha2_code) {
    party_country pc;
    pc.tenant_id = h.tenant_id().to_string();
    pc.party_id = party_id;
    pc.country_alpha2_code = alpha2_code;
    pc.modified_by = h.db_user();
    pc.change_reason_code = "system.test";
    pc.change_commentary = "Synthetic test data";
    pc.performed_by = h.db_user();
    return pc;
}

}

TEST_CASE("write_single_party_country", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    party_repository party_repo(h.context());
    country_repository cty_repo;
    party_country_repository repo(h.context());

    const auto party_id = find_system_party_id(
        party_repo, h.tenant_id().to_string());
    const auto alpha2_code = find_first_country_alpha2_code(cty_repo, h);

    auto pc = make_party_country(h, party_id, alpha2_code);
    BOOST_LOG_SEV(lg, debug) << "Party country: " << pc;
    CHECK_NOTHROW(repo.write(pc));
}

TEST_CASE("write_multiple_party_countries", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    party_repository party_repo(h.context());
    country_repository cty_repo;
    party_country_repository repo(h.context());

    const auto system_party_id = find_system_party_id(
        party_repo, h.tenant_id().to_string());

    auto all_countries = cty_repo.read_latest(h.context());
    REQUIRE(all_countries.size() >= 2);

    std::vector<party_country> pcs;
    for (std::size_t i = 0; i < 2; ++i) {
        pcs.push_back(make_party_country(
            h, system_party_id, all_countries[i].alpha2_code));
    }

    BOOST_LOG_SEV(lg, debug) << "Party countries: " << pcs;
    CHECK_NOTHROW(repo.write(pcs));
}

TEST_CASE("read_latest_party_countries_by_party", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    party_repository party_repo(h.context());
    country_repository cty_repo;
    party_country_repository repo(h.context());

    const auto system_party_id = find_system_party_id(
        party_repo, h.tenant_id().to_string());
    const auto alpha2_code = find_first_country_alpha2_code(cty_repo, h);

    auto pc = make_party_country(h, system_party_id, alpha2_code);
    repo.write(pc);

    auto read_pcs = repo.read_latest_by_party(system_party_id);
    BOOST_LOG_SEV(lg, debug) << "Read party countries: " << read_pcs;

    REQUIRE(!read_pcs.empty());
    bool found = false;
    for (const auto& r : read_pcs) {
        if (r.country_alpha2_code == alpha2_code) {
            found = true;
            break;
        }
    }
    CHECK(found);
}

TEST_CASE("read_latest_party_countries_by_country", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    party_repository party_repo(h.context());
    country_repository cty_repo;
    party_country_repository repo(h.context());

    const auto system_party_id = find_system_party_id(
        party_repo, h.tenant_id().to_string());
    const auto alpha2_code = find_first_country_alpha2_code(cty_repo, h);

    auto pc = make_party_country(h, system_party_id, alpha2_code);
    repo.write(pc);

    auto read_pcs = repo.read_latest_by_country(alpha2_code);
    BOOST_LOG_SEV(lg, debug) << "Read party countries: " << read_pcs;

    REQUIRE(read_pcs.size() >= 1);
    CHECK(read_pcs[0].party_id == system_party_id);
}

TEST_CASE("remove_party_country", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;

    party_repository party_repo(h.context());
    country_repository cty_repo;
    party_country_repository repo(h.context());

    const auto system_party_id = find_system_party_id(
        party_repo, h.tenant_id().to_string());
    const auto alpha2_code = find_first_country_alpha2_code(cty_repo, h);

    auto pc = make_party_country(h, system_party_id, alpha2_code);
    repo.write(pc);

    CHECK_NOTHROW(repo.remove(system_party_id, alpha2_code));

    auto read_pcs = repo.read_latest_by_country(alpha2_code);
    CHECK(read_pcs.empty());
}

TEST_CASE("read_nonexistent_party_country", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    party_country_repository repo(h.context());

    const auto nonexistent_id = boost::uuids::random_generator()();
    auto read_pcs = repo.read_latest_by_party(nonexistent_id);
    CHECK(read_pcs.empty());
}

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
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_identifier.hpp"         // IWYU pragma: keep.
#include "ores.refdata.api/domain/party_identifier_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.api/generators/party_identifier_generator.hpp"
#include "ores.refdata.core/repository/party_identifier_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::domain::party_identifier;
using ores::refdata::repository::party_identifier_repository;
using ores::refdata::repository::party_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_party_identifier", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    party_repository party_repo;
    auto party = generate_synthetic_party(ctx);
    party.change_reason_code = "system.test";
    auto existing = party_repo.read_latest(h.context());
    for (const auto& e : existing) {
        if (e.tenant_id == party.tenant_id) {
            party.parent_party_id = e.id;
            break;
        }
    }
    party_repo.write(h.context(), party);

    auto pi = generate_synthetic_party_identifier(ctx);
    pi.change_reason_code = "system.test";
    pi.party_id = party.id;
    BOOST_LOG_SEV(lg, debug) << "Party identifier: " << pi;

    party_identifier_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), pi));
}

TEST_CASE("write_multiple_party_identifiers", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    party_repository party_repo;
    auto party = generate_synthetic_party(ctx);
    party.change_reason_code = "system.test";
    auto existing = party_repo.read_latest(h.context());
    for (const auto& e : existing) {
        if (e.tenant_id == party.tenant_id) {
            party.parent_party_id = e.id;
            break;
        }
    }
    party_repo.write(h.context(), party);

    auto party_identifiers = generate_synthetic_party_identifiers(3, ctx);
    for (auto& pi : party_identifiers) {
        pi.change_reason_code = "system.test";
        pi.party_id = party.id;
    }
    BOOST_LOG_SEV(lg, debug) << "Party identifiers: " << party_identifiers;

    party_identifier_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), party_identifiers));
}

TEST_CASE("read_latest_party_identifiers", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    party_repository party_repo;
    auto party = generate_synthetic_party(ctx);
    party.change_reason_code = "system.test";
    auto existing = party_repo.read_latest(h.context());
    for (const auto& e : existing) {
        if (e.tenant_id == party.tenant_id) {
            party.parent_party_id = e.id;
            break;
        }
    }
    party_repo.write(h.context(), party);
    h.set_party(party.id);

    auto written_party_identifiers = generate_synthetic_party_identifiers(3, ctx);
    for (auto& pi : written_party_identifiers) {
        pi.change_reason_code = "system.test";
        pi.party_id = party.id;
    }
    BOOST_LOG_SEV(lg, debug) << "Written party identifiers: " << written_party_identifiers;

    party_identifier_repository repo;
    repo.write(h.context(), written_party_identifiers);

    auto read_party_identifiers = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read party identifiers: " << read_party_identifiers;

    CHECK(read_party_identifiers.size() >= written_party_identifiers.size());
}

TEST_CASE("read_latest_party_identifier_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    party_repository party_repo;
    auto party = generate_synthetic_party(ctx);
    party.change_reason_code = "system.test";
    auto existing = party_repo.read_latest(h.context());
    for (const auto& e : existing) {
        if (e.tenant_id == party.tenant_id) {
            party.parent_party_id = e.id;
            break;
        }
    }
    party_repo.write(h.context(), party);
    h.set_party(party.id);

    auto pi = generate_synthetic_party_identifier(ctx);
    pi.change_reason_code = "system.test";
    pi.party_id = party.id;
    const auto original_id_value = pi.id_value;
    BOOST_LOG_SEV(lg, debug) << "Party identifier: " << pi;

    party_identifier_repository repo;
    repo.write(h.context(), pi);

    pi.id_value = original_id_value + "_v2";
    repo.write(h.context(), pi);

    auto read_party_identifiers = repo.read_latest(h.context(), boost::uuids::to_string(pi.id));
    BOOST_LOG_SEV(lg, debug) << "Read party identifiers: " << read_party_identifiers;

    REQUIRE(read_party_identifiers.size() == 1);
    CHECK(read_party_identifiers[0].id == pi.id);
    CHECK(read_party_identifiers[0].id_value == original_id_value + "_v2");
}

TEST_CASE("read_nonexistent_party_identifier_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    party_identifier_repository repo;

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_party_identifiers =
        repo.read_latest(h.context(), boost::uuids::to_string(nonexistent_id));
    BOOST_LOG_SEV(lg, debug) << "Read party identifiers: " << read_party_identifiers;

    CHECK(read_party_identifiers.size() == 0);
}

TEST_CASE("as_of_composition_reflects_party_version_history", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    party_repository party_repo;
    auto party = generate_synthetic_party(ctx);
    party.change_reason_code = "system.test";
    auto existing = party_repo.read_latest(h.context());
    for (const auto& e : existing) {
        if (e.tenant_id == party.tenant_id) {
            party.parent_party_id = e.id;
            break;
        }
    }
    party_repo.write(h.context(), party);
    h.set_party(party.id);

    // Fetch v1 of the party — created above, no identifiers yet.
    const auto v1 = party_repo.read_at_version(h.context(), boost::uuids::to_string(party.id), 1);
    REQUIRE(v1.has_value());
    CHECK(v1->version == 1);

    // Adding an identifier bumps the party to v2 (composite versioning —
    // see the "Temporal composite entity versioning" architecture doc).
    auto pi = generate_synthetic_party_identifier(ctx);
    pi.change_reason_code = "system.test";
    pi.party_id = party.id;
    party_identifier_repository identifier_repo;
    identifier_repo.write(h.context(), pi);

    const auto v2 = party_repo.read_at_version(h.context(), boost::uuids::to_string(party.id), 2);
    REQUIRE(v2.has_value());
    CHECK(v2->version == 2);

    // domain::party only surfaces recorded_at (= valid_from), not valid_to;
    // windows are contiguous by construction, so v1's window ends exactly
    // when v2's begins. v2 is still current, so bound its open end with a
    // safely-far-future instant rather than the real infinity sentinel.
    const auto far_future = std::chrono::system_clock::now() + std::chrono::hours(24 * 365 * 100);

    // v1's window predates the identifier: composing "as of v1" must not
    // include it.
    const auto identifiers_at_v1 = identifier_repo.read_by_party_id_as_of(
        h.context(), boost::uuids::to_string(party.id), v1->recorded_at, v2->recorded_at);
    CHECK(identifiers_at_v1.empty());

    // v2's window is exactly when the identifier was created: composing
    // "as of v2" must include it.
    const auto identifiers_at_v2 = identifier_repo.read_by_party_id_as_of(
        h.context(), boost::uuids::to_string(party.id), v2->recorded_at, far_future);
    REQUIRE(identifiers_at_v2.size() == 1);
    CHECK(identifiers_at_v2[0].id == pi.id);

    // Fetching a nonexistent version returns nullopt rather than throwing.
    const auto missing =
        party_repo.read_at_version(h.context(), boost::uuids::to_string(party.id), 999);
    CHECK_FALSE(missing.has_value());
}

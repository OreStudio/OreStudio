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
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/party_status.hpp"         // IWYU pragma: keep.
#include "ores.refdata.api/domain/party_status_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/party_status_generator.hpp"
#include "ores.refdata.core/repository/party_status_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/log/sources/severity_feature.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <string_view>

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::domain::party_status;
using ores::refdata::repository::party_status_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_party_status", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto ps = generate_synthetic_party_status(ctx);
    ps.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Party status: " << ps;

    party_status_repository repo;
    repo.write(h.context(), ps);

    const auto read_party_statuses = repo.read_latest(h.context(), ps.code);
    REQUIRE(read_party_statuses.size() == 1);
    CHECK(read_party_statuses[0].name == ps.name);
}

TEST_CASE("write_multiple_party_statuses", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto party_statuses = generate_synthetic_party_statuses(3, ctx);
    for (auto& ps : party_statuses) {
        ps.change_reason_code = "system.test";
    }
    BOOST_LOG_SEV(lg, debug) << "Party statuses: " << party_statuses;

    party_status_repository repo;
    repo.write(h.context(), party_statuses);

    const auto read_party_statuses = repo.read_latest(h.context());
    for (const auto& written : party_statuses) {
        const auto it = std::ranges::find_if(
            read_party_statuses, [&](const party_status& ps) { return ps.code == written.code; });
        REQUIRE(it != read_party_statuses.end());
        CHECK(it->name == written.name);
    }
}

TEST_CASE("read_latest_party_statuses", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto written_party_statuses = generate_synthetic_party_statuses(3, ctx);
    for (auto& ps : written_party_statuses) {
        ps.change_reason_code = "system.test";
    }
    BOOST_LOG_SEV(lg, debug) << "Written party statuses: " << written_party_statuses;

    party_status_repository repo;
    repo.write(h.context(), written_party_statuses);

    auto read_party_statuses = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read party statuses: " << read_party_statuses;

    for (const auto& written : written_party_statuses) {
        const auto it = std::ranges::find_if(
            read_party_statuses, [&](const party_status& ps) { return ps.code == written.code; });
        REQUIRE(it != read_party_statuses.end());
        CHECK(it->name == written.name);
    }
}

TEST_CASE("read_latest_party_status_by_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto ps = generate_synthetic_party_status(ctx);
    ps.change_reason_code = "system.test";
    const auto original_name = ps.name;
    BOOST_LOG_SEV(lg, debug) << "Party status: " << ps;

    party_status_repository repo;
    repo.write(h.context(), ps);

    ps.name = original_name + " v2";
    repo.write(h.context(), ps);

    auto read_party_statuses = repo.read_latest(h.context(), ps.code);
    BOOST_LOG_SEV(lg, debug) << "Read party statuses: " << read_party_statuses;

    REQUIRE(read_party_statuses.size() == 1);
    CHECK(read_party_statuses[0].code == ps.code);
    CHECK(read_party_statuses[0].name == original_name + " v2");
}

TEST_CASE("read_nonexistent_party_status_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    party_status_repository repo;

    // Write a row the read can answer with, so that an empty answer is the key
    // at work rather than a read that does nothing.
    auto keeper = generate_synthetic_party_status(ctx);
    keeper.change_reason_code = "system.test";
    repo.write(h.context(), keeper);

    const std::string nonexistent_code = "NONEXISTENT_CODE_12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent code: " << nonexistent_code;

    auto read_party_statuses = repo.read_latest(h.context(), nonexistent_code);
    BOOST_LOG_SEV(lg, debug) << "Read party statuses: " << read_party_statuses;

    const auto answered_with_another_key = std::ranges::any_of(
        read_party_statuses, [&](const party_status& ps) { return ps.code == nonexistent_code; });
    CHECK_FALSE(answered_with_another_key);

    // The same read does answer for the key that was written.
    const auto keeper_rows = repo.read_latest(h.context(), keeper.code);
    REQUIRE(keeper_rows.size() == 1);
    CHECK(keeper_rows[0].name == keeper.name);
}

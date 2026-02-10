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
#include "ores.refdata/repository/party_status_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/party_status.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/party_status_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/generators/party_status_generator.hpp"

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
    auto ps = generate_synthetic_party_status();
    ps.tenant_id = h.tenant_id();
    ps.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Party status: " << ps;

    party_status_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), ps));
}

TEST_CASE("write_multiple_party_statuses", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto party_statuses = generate_synthetic_party_statuses(3);
    for (auto& ps : party_statuses) {
        ps.tenant_id = h.tenant_id();
        ps.change_reason_code = "system.test";
    }
    BOOST_LOG_SEV(lg, debug) << "Party statuses: " << party_statuses;

    party_status_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), party_statuses));
}

TEST_CASE("read_latest_party_statuses", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto written_party_statuses = generate_synthetic_party_statuses(3);
    for (auto& ps : written_party_statuses) {
        ps.tenant_id = h.tenant_id();
        ps.change_reason_code = "system.test";
    }
    BOOST_LOG_SEV(lg, debug) << "Written party statuses: "
                             << written_party_statuses;

    party_status_repository repo;
    repo.write(h.context(), written_party_statuses);

    auto read_party_statuses = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read party statuses: "
                             << read_party_statuses;

    CHECK(read_party_statuses.size() >= written_party_statuses.size());
}

TEST_CASE("read_latest_party_status_by_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ps = generate_synthetic_party_status();
    ps.tenant_id = h.tenant_id();
    ps.change_reason_code = "system.test";
    const auto original_name = ps.name;
    BOOST_LOG_SEV(lg, debug) << "Party status: " << ps;

    party_status_repository repo;
    repo.write(h.context(), ps);

    ps.name = original_name + " v2";
    repo.write(h.context(), ps);

    auto read_party_statuses = repo.read_latest(h.context(), ps.code);
    BOOST_LOG_SEV(lg, debug) << "Read party statuses: "
                             << read_party_statuses;

    REQUIRE(read_party_statuses.size() == 1);
    CHECK(read_party_statuses[0].code == ps.code);
    CHECK(read_party_statuses[0].name == original_name + " v2");
}

TEST_CASE("read_nonexistent_party_status_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    party_status_repository repo;

    const std::string nonexistent_code = "NONEXISTENT_CODE_12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent code: " << nonexistent_code;

    auto read_party_statuses = repo.read_latest(h.context(), nonexistent_code);
    BOOST_LOG_SEV(lg, debug) << "Read party statuses: "
                             << read_party_statuses;

    CHECK(read_party_statuses.size() == 0);
}

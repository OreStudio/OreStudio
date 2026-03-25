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
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.reporting.core/repository/report_type_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.reporting.api/domain/report_definition.hpp"
#include "ores.reporting.api/domain/report_definition_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.api/generators/report_definition_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"

namespace {

const std::string_view test_suite("ores.reporting.tests");
const std::string tags("[repository]");

/**
 * @brief Fetches the first available party id from the test database.
 *
 * report_definition.party_id is a FK to the parties table so all writes
 * need a real party that was seeded by the test infrastructure.
 */
boost::uuids::uuid get_test_party_id(ores::testing::database_helper& h) {
    ores::refdata::repository::party_repository party_repo(h.context());
    auto parties = party_repo.read_latest();
    REQUIRE(!parties.empty());
    return parties.front().id;
}

/**
 * @brief Returns a stable seeded report type code for use in FK-constrained writes.
 *
 * report_definition.report_type is a FK to the report_types table. We use a
 * specific seeded code (report_type_1) rather than reading from the table to
 * avoid race conditions with concurrent report_type tests that write and
 * remove their own synthetic codes.
 */
std::string get_test_report_type(ores::testing::database_helper& h) {
    ores::reporting::repository::report_type_repository rt_repo;
    auto types = rt_repo.read_latest(h.context(), "report_type_1");
    REQUIRE(!types.empty());
    return types.front().code;
}

}

using namespace ores::logging;
using namespace ores::reporting::generators;

using ores::testing::database_helper;
using ores::reporting::repository::report_definition_repository;

TEST_CASE("write_single_report_definition", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    const auto party_id = get_test_party_id(h);
    const auto report_type = get_test_report_type(h);

    report_definition_repository repo;
    auto rd = generate_synthetic_report_definition(ctx);
    rd.party_id = party_id;
    rd.report_type = report_type;

    BOOST_LOG_SEV(lg, debug) << "Report definition: " << rd;
    CHECK_NOTHROW(repo.write(h.context(), rd));
}

TEST_CASE("write_multiple_report_definitions", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    const auto party_id = get_test_party_id(h);
    const auto report_type = get_test_report_type(h);

    report_definition_repository repo;
    auto definitions = generate_synthetic_report_definitions(5, ctx);
    for (auto& d : definitions) {
        d.party_id = party_id;
        d.report_type = report_type;
    }
    BOOST_LOG_SEV(lg, debug) << "Report definitions: " << definitions;

    CHECK_NOTHROW(repo.write(h.context(), definitions));
}

TEST_CASE("read_latest_report_definitions", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    const auto party_id = get_test_party_id(h);
    const auto report_type = get_test_report_type(h);

    report_definition_repository repo;
    auto written = generate_synthetic_report_definitions(3, ctx);
    for (auto& d : written) {
        d.party_id = party_id;
        d.report_type = report_type;
    }
    BOOST_LOG_SEV(lg, debug) << "Written definitions: " << written;
    repo.write(h.context(), written);

    auto read = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read definitions: " << read;

    CHECK(!read.empty());
    CHECK(read.size() >= written.size());
}

TEST_CASE("read_latest_report_definition_by_id", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    const auto party_id = get_test_party_id(h);
    const auto report_type = get_test_report_type(h);

    report_definition_repository repo;
    auto definitions = generate_synthetic_report_definitions(5, ctx);
    for (auto& d : definitions) {
        d.party_id = party_id;
        d.report_type = report_type;
    }
    const auto target = definitions.front();
    BOOST_LOG_SEV(lg, debug) << "Written definitions: " << definitions;
    repo.write(h.context(), definitions);

    auto read = repo.read_latest(h.context(), boost::uuids::to_string(target.id));
    BOOST_LOG_SEV(lg, debug) << "Read definitions: " << read;

    REQUIRE(read.size() == 1);
    CHECK(read[0].id == target.id);
    CHECK(read[0].name == target.name);
}

TEST_CASE("read_all_versions_of_report_definition", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    const auto party_id = get_test_party_id(h);
    const auto report_type = get_test_report_type(h);

    report_definition_repository repo;
    auto rd = generate_synthetic_report_definition(ctx);
    rd.party_id = party_id;
    rd.report_type = report_type;
    repo.write(h.context(), rd);

    rd.version = 1;
    rd.description = "updated description";
    repo.write(h.context(), rd);

    auto all = repo.read_all(h.context(), boost::uuids::to_string(rd.id));
    BOOST_LOG_SEV(lg, debug) << "All versions: " << all;

    CHECK(all.size() >= 2);
}

TEST_CASE("remove_report_definition", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    const auto party_id = get_test_party_id(h);
    const auto report_type = get_test_report_type(h);

    report_definition_repository repo;
    auto rd = generate_synthetic_report_definition(ctx);
    rd.party_id = party_id;
    rd.report_type = report_type;
    repo.write(h.context(), rd);

    const auto rd_id = boost::uuids::to_string(rd.id);
    auto before = repo.read_latest(h.context(), rd_id);
    REQUIRE(!before.empty());

    CHECK_NOTHROW(repo.remove(h.context(), rd_id));

    auto after = repo.read_latest(h.context(), rd_id);
    BOOST_LOG_SEV(lg, debug) << "After remove count: " << after.size();
    CHECK(after.empty());
}

TEST_CASE("read_nonexistent_report_definition", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    report_definition_repository repo;
    // Use a valid nil UUID — the column type is uuid so must be valid syntax.
    const std::string nonexistent = "00000000-0000-0000-0000-000000000000";
    BOOST_LOG_SEV(lg, debug) << "Non-existent id: " << nonexistent;

    auto read = repo.read_latest(h.context(), nonexistent);
    BOOST_LOG_SEV(lg, debug) << "Read definitions: " << read;

    CHECK(read.empty());
}

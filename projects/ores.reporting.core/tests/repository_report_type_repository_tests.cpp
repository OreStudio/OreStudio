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
#include "ores.reporting.core/repository/report_type_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.reporting.api/domain/report_type.hpp"
#include "ores.reporting.api/domain/report_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting.api/generators/report_type_generator.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"

namespace {

const std::string_view test_suite("ores.reporting.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::reporting::generators;

using ores::testing::database_helper;
using ores::reporting::repository::report_type_repository;

TEST_CASE("write_single_report_type", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    report_type_repository repo;
    auto rt = generate_synthetic_report_type(ctx);

    BOOST_LOG_SEV(lg, debug) << "Report type: " << rt;
    CHECK_NOTHROW(repo.write(h.context(), rt));
}

TEST_CASE("write_multiple_report_types", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    report_type_repository repo;
    auto report_types = generate_synthetic_report_types(5, ctx);
    BOOST_LOG_SEV(lg, debug) << "Report types: " << report_types;

    CHECK_NOTHROW(repo.write(h.context(), report_types));
}

TEST_CASE("read_latest_report_types", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    report_type_repository repo;
    auto written = generate_synthetic_report_types(3, ctx);
    BOOST_LOG_SEV(lg, debug) << "Written report types: " << written;
    repo.write(h.context(), written);

    auto read = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read report types: " << read;

    CHECK(!read.empty());
    CHECK(read.size() >= written.size());
}

TEST_CASE("read_latest_report_type_by_code", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    report_type_repository repo;
    auto report_types = generate_synthetic_report_types(5, ctx);
    const auto target = report_types.front();
    BOOST_LOG_SEV(lg, debug) << "Written report types: " << report_types;
    repo.write(h.context(), report_types);

    auto read = repo.read_latest(h.context(), target.code);
    BOOST_LOG_SEV(lg, debug) << "Read report types: " << read;

    REQUIRE(read.size() == 1);
    CHECK(read[0].code == target.code);
    CHECK(read[0].name == target.name);
}

TEST_CASE("read_all_versions_of_report_type", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    report_type_repository repo;
    auto rt = generate_synthetic_report_type(ctx);
    repo.write(h.context(), rt);

    rt.version = 1;
    rt.description = "updated description";
    repo.write(h.context(), rt);

    auto all = repo.read_all(h.context(), rt.code);
    BOOST_LOG_SEV(lg, debug) << "All versions: " << all;

    CHECK(all.size() >= 2);
}

TEST_CASE("remove_report_type", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    report_type_repository repo;
    auto rt = generate_synthetic_report_type(ctx);
    repo.write(h.context(), rt);

    auto before = repo.read_latest(h.context(), rt.code);
    REQUIRE(!before.empty());

    CHECK_NOTHROW(repo.remove(h.context(), rt.code));

    auto after = repo.read_latest(h.context(), rt.code);
    BOOST_LOG_SEV(lg, debug) << "After remove count: " << after.size();
    CHECK(after.empty());
}

TEST_CASE("read_nonexistent_report_type", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;

    report_type_repository repo;
    const std::string nonexistent = "NONEXISTENT_REPORT_TYPE_XYZ_12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent code: " << nonexistent;

    auto read = repo.read_latest(h.context(), nonexistent);
    BOOST_LOG_SEV(lg, debug) << "Read report types: " << read;

    CHECK(read.empty());
}

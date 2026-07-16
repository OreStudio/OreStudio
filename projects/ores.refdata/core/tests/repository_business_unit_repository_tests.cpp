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
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/business_unit.hpp"         // IWYU pragma: keep.
#include "ores.refdata.api/domain/business_unit_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/business_unit_generator.hpp"
#include "ores.refdata.core/repository/business_unit_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::domain::business_unit;
using ores::refdata::repository::business_unit_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_business_unit", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto bu = generate_synthetic_business_unit(ctx);
    bu.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Business unit: " << bu;

    business_unit_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), bu));
}

TEST_CASE("write_multiple_business_units", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto units = generate_synthetic_business_units(3, ctx);
    for (auto& bu : units) {
        bu.change_reason_code = "system.test";
    }
    BOOST_LOG_SEV(lg, debug) << "Business units: " << units;

    business_unit_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), units));
}

TEST_CASE("read_latest_business_units", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto written = generate_synthetic_business_units(3, ctx);
    for (auto& bu : written) {
        bu.change_reason_code = "system.test";
    }
    BOOST_LOG_SEV(lg, debug) << "Written business units: " << written;

    business_unit_repository repo;
    repo.write(h.context(), written);

    auto read = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read business units: " << read;

    CHECK(read.size() >= written.size());
}

TEST_CASE("read_latest_business_units_paginated", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto written = generate_synthetic_business_units(5, ctx);
    for (auto& bu : written) {
        bu.change_reason_code = "system.test";
    }

    business_unit_repository repo;
    repo.write(h.context(), written);

    auto page = repo.read_latest(h.context(), 0, 2);
    BOOST_LOG_SEV(lg, debug) << "Paginated business units: " << page;

    CHECK(page.size() == 2);
}

TEST_CASE("get_total_business_unit_count", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto written = generate_synthetic_business_units(3, ctx);
    for (auto& bu : written) {
        bu.change_reason_code = "system.test";
    }

    business_unit_repository repo;
    repo.write(h.context(), written);

    const auto count = repo.get_total_business_unit_count(h.context());
    BOOST_LOG_SEV(lg, debug) << "Total business unit count: " << count;

    CHECK(count >= written.size());
}

TEST_CASE("read_latest_business_unit_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto bu = generate_synthetic_business_unit(ctx);
    bu.change_reason_code = "system.test";
    const auto original_name = bu.unit_name;
    BOOST_LOG_SEV(lg, debug) << "Business unit: " << bu;

    business_unit_repository repo;
    repo.write(h.context(), bu);

    bu.unit_name = original_name + " v2";
    repo.write(h.context(), bu);

    const auto id_str = boost::uuids::to_string(bu.id);
    auto read = repo.read_latest(h.context(), id_str);
    BOOST_LOG_SEV(lg, debug) << "Read business units by id: " << read;

    REQUIRE(read.size() == 1);
    CHECK(read[0].id == bu.id);
    CHECK(read[0].unit_name == original_name + " v2");
}

TEST_CASE("read_all_business_unit_versions", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto bu = generate_synthetic_business_unit(ctx);
    bu.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Business unit: " << bu;

    business_unit_repository repo;
    repo.write(h.context(), bu);

    bu.unit_name = bu.unit_name + " v2";
    repo.write(h.context(), bu);

    const auto id_str = boost::uuids::to_string(bu.id);
    auto all_versions = repo.read_all(h.context(), id_str);
    BOOST_LOG_SEV(lg, debug) << "All versions: " << all_versions;

    CHECK(all_versions.size() >= 2);
}

TEST_CASE("read_business_unit_at_version", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto bu = generate_synthetic_business_unit(ctx);
    bu.change_reason_code = "system.test";
    const auto original_name = bu.unit_name;
    BOOST_LOG_SEV(lg, debug) << "Business unit: " << bu;

    business_unit_repository repo;
    repo.write(h.context(), bu);

    bu.unit_name = original_name + " v2";
    repo.write(h.context(), bu);

    const auto id_str = boost::uuids::to_string(bu.id);
    auto v1 = repo.read_at_version(h.context(), id_str, 1);
    BOOST_LOG_SEV(lg, debug) << "Business unit at version 1: "
                             << (v1 ? v1->unit_name : "(not found)");

    REQUIRE(v1.has_value());
    CHECK(v1->unit_name == original_name);
    CHECK(v1->version == 1);
}

TEST_CASE("remove_business_unit", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto bu = generate_synthetic_business_unit(ctx);
    bu.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Business unit: " << bu;

    business_unit_repository repo;
    repo.write(h.context(), bu);

    const auto id_str = boost::uuids::to_string(bu.id);
    auto before_remove = repo.read_latest(h.context(), id_str);
    REQUIRE(before_remove.size() == 1);

    CHECK_NOTHROW(repo.remove(h.context(), id_str));

    auto after_remove = repo.read_latest(h.context(), id_str);
    BOOST_LOG_SEV(lg, debug) << "After remove: " << after_remove;
    CHECK(after_remove.empty());
}

TEST_CASE("remove_multiple_business_units", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto units = generate_synthetic_business_units(2, ctx);
    for (auto& bu : units) {
        bu.change_reason_code = "system.test";
    }

    business_unit_repository repo;
    repo.write(h.context(), units);

    std::vector<std::string> ids;
    for (const auto& bu : units) {
        ids.push_back(boost::uuids::to_string(bu.id));
    }

    CHECK_NOTHROW(repo.remove(h.context(), ids));

    for (const auto& id_str : ids) {
        auto after_remove = repo.read_latest(h.context(), id_str);
        CHECK(after_remove.empty());
    }
}

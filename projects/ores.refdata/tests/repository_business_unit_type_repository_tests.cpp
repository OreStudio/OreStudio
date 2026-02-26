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
#include "ores.refdata/repository/business_unit_type_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/business_unit_type.hpp" // IWYU pragma: keep.
#include "ores.refdata/domain/business_unit_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata/generators/business_unit_type_generator.hpp"

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::domain::business_unit_type;
using ores::refdata::repository::business_unit_type_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_business_unit_type", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto ut = generate_synthetic_business_unit_type(ctx);
    ut.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Business unit type: " << ut;

    business_unit_type_repository repo(h.context());
    CHECK_NOTHROW(repo.write(ut));
}

TEST_CASE("write_multiple_business_unit_types", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto unit_types = generate_synthetic_business_unit_types(3, ctx);
    for (auto& ut : unit_types) {
        ut.change_reason_code = "system.test";
    }
    BOOST_LOG_SEV(lg, debug) << "Business unit types: " << unit_types;

    business_unit_type_repository repo(h.context());
    CHECK_NOTHROW(repo.write(unit_types));
}

TEST_CASE("read_latest_business_unit_types", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto written_types = generate_synthetic_business_unit_types(3, ctx);
    for (auto& ut : written_types) {
        ut.change_reason_code = "system.test";
    }
    BOOST_LOG_SEV(lg, debug) << "Written business unit types: " << written_types;

    business_unit_type_repository repo(h.context());
    repo.write(written_types);

    auto read_types = repo.read_latest();
    BOOST_LOG_SEV(lg, debug) << "Read business unit types: " << read_types;

    CHECK(read_types.size() >= written_types.size());
}

TEST_CASE("read_latest_business_unit_type_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto ut = generate_synthetic_business_unit_type(ctx);
    ut.change_reason_code = "system.test";
    const auto original_name = ut.name;
    BOOST_LOG_SEV(lg, debug) << "Business unit type: " << ut;

    business_unit_type_repository repo(h.context());
    repo.write(ut);

    ut.name = original_name + " v2";
    repo.write(ut);

    auto read_types = repo.read_latest(ut.id);
    BOOST_LOG_SEV(lg, debug) << "Read business unit types by id: " << read_types;

    REQUIRE(read_types.size() == 1);
    CHECK(read_types[0].id == ut.id);
    CHECK(read_types[0].name == original_name + " v2");
}

TEST_CASE("read_latest_business_unit_type_by_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto ut = generate_synthetic_business_unit_type(ctx);
    ut.change_reason_code = "system.test";
    const auto original_name = ut.name;
    BOOST_LOG_SEV(lg, debug) << "Business unit type: " << ut;

    business_unit_type_repository repo(h.context());
    repo.write(ut);

    ut.name = original_name + " v2";
    repo.write(ut);

    auto read_types = repo.read_latest_by_code(ut.code);
    BOOST_LOG_SEV(lg, debug) << "Read business unit types by code: " << read_types;

    REQUIRE(read_types.size() == 1);
    CHECK(read_types[0].code == ut.code);
    CHECK(read_types[0].name == original_name + " v2");
}

TEST_CASE("read_all_business_unit_type_versions", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto ut = generate_synthetic_business_unit_type(ctx);
    ut.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Business unit type: " << ut;

    business_unit_type_repository repo(h.context());
    repo.write(ut);

    ut.name = ut.name + " v2";
    repo.write(ut);

    auto all_versions = repo.read_all(ut.id);
    BOOST_LOG_SEV(lg, debug) << "All versions: " << all_versions;

    CHECK(all_versions.size() >= 2);
}

TEST_CASE("remove_business_unit_type", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto ut = generate_synthetic_business_unit_type(ctx);
    ut.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Business unit type: " << ut;

    business_unit_type_repository repo(h.context());
    repo.write(ut);

    auto before_remove = repo.read_latest(ut.id);
    REQUIRE(before_remove.size() == 1);

    CHECK_NOTHROW(repo.remove(ut.id));

    auto after_remove = repo.read_latest(ut.id);
    BOOST_LOG_SEV(lg, debug) << "After remove: " << after_remove;
    CHECK(after_remove.empty());
}

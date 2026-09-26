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
#include "ores.refdata.api/domain/business_unit_type.hpp"         // IWYU pragma: keep.
#include "ores.refdata.api/domain/business_unit_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/business_unit_type_generator.hpp"
#include "ores.refdata.core/repository/business_unit_type_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

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

    business_unit_type_repository repo;
    repo.write(h.context(), ut);

    const auto id_str = boost::uuids::to_string(ut.id);
    const auto rows = repo.read_latest(h.context(), id_str);
    REQUIRE(rows.size() == 1);
    CHECK(rows[0].name == ut.name);
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

    business_unit_type_repository repo;
    repo.write(h.context(), unit_types);

    const auto rows = repo.read_latest(h.context());
    for (const auto& written : unit_types) {
        const auto it = std::ranges::find_if(rows, [&](const business_unit_type& t) {
            return t.id == written.id;
        });
        REQUIRE(it != rows.end());
        CHECK(it->name == written.name);
    }
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

    business_unit_type_repository repo;
    repo.write(h.context(), written_types);

    auto read_types = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read business unit types: " << read_types;

    for (const auto& expected : written_types) {
        const auto it = std::ranges::find_if(read_types, [&](const business_unit_type& t) {
            return t.id == expected.id;
        });
        REQUIRE(it != read_types.end());
        CHECK(it->name == expected.name);
    }
}

TEST_CASE("read_latest_business_unit_type_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto ut = generate_synthetic_business_unit_type(ctx);
    ut.change_reason_code = "system.test";
    const auto original_name = ut.name;
    BOOST_LOG_SEV(lg, debug) << "Business unit type: " << ut;

    business_unit_type_repository repo;
    repo.write(h.context(), ut);

    ut.name = original_name + " v2";
    repo.write(h.context(), ut);

    const auto id_str = boost::uuids::to_string(ut.id);
    auto read_types = repo.read_latest(h.context(), id_str);
    BOOST_LOG_SEV(lg, debug) << "Read business unit types by id: " << read_types;

    REQUIRE(read_types.size() == 1);
    CHECK(read_types[0].id == ut.id);
    CHECK(read_types[0].name == original_name + " v2");
}

TEST_CASE("read_all_business_unit_type_versions", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto ut = generate_synthetic_business_unit_type(ctx);
    ut.change_reason_code = "system.test";
    const auto original_name = ut.name;
    BOOST_LOG_SEV(lg, debug) << "Business unit type: " << ut;

    business_unit_type_repository repo;
    repo.write(h.context(), ut);

    ut.name = ut.name + " v2";
    repo.write(h.context(), ut);

    const auto id_str = boost::uuids::to_string(ut.id);
    auto all_versions = repo.read_all(h.context(), id_str);
    BOOST_LOG_SEV(lg, debug) << "All versions: " << all_versions;

    const auto v1 = std::ranges::find_if(all_versions, [](const business_unit_type& t) {
        return t.version == 1;
    });
    REQUIRE(v1 != all_versions.end());
    CHECK(v1->name == original_name);

    const auto v2 = std::ranges::find_if(all_versions, [](const business_unit_type& t) {
        return t.version == 2;
    });
    REQUIRE(v2 != all_versions.end());
    CHECK(v2->name == original_name + " v2");
}

TEST_CASE("remove_business_unit_type", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto ut = generate_synthetic_business_unit_type(ctx);
    ut.change_reason_code = "system.test";
    auto keeper = generate_synthetic_business_unit_type(ctx);
    keeper.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Business unit type: " << ut;

    business_unit_type_repository repo;
    repo.write(h.context(), ut);
    repo.write(h.context(), keeper);

    const auto id_str = boost::uuids::to_string(ut.id);
    auto before_remove = repo.read_latest(h.context(), id_str);
    REQUIRE(before_remove.size() == 1);
    CHECK(before_remove[0].name == ut.name);

    repo.remove(h.context(), id_str);

    auto after_remove = repo.read_latest(h.context(), id_str);
    BOOST_LOG_SEV(lg, debug) << "After remove: " << after_remove;

    // The removed row is gone from the read, and the same read still answers
    // for the row that was not removed.
    const auto removed_still_present = std::ranges::any_of(
        after_remove, [&](const business_unit_type& t) { return t.id == ut.id; });
    CHECK_FALSE(removed_still_present);

    const auto keeper_rows = repo.read_latest(h.context(), boost::uuids::to_string(keeper.id));
    REQUIRE(keeper_rows.size() == 1);
    CHECK(keeper_rows[0].name == keeper.name);
}

TEST_CASE("remove_multiple_business_unit_types", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto unit_types = generate_synthetic_business_unit_types(2, ctx);
    for (auto& ut : unit_types) {
        ut.change_reason_code = "system.test";
    }

    auto keeper = generate_synthetic_business_unit_type(ctx);
    keeper.change_reason_code = "system.test";

    business_unit_type_repository repo;
    repo.write(h.context(), unit_types);
    repo.write(h.context(), keeper);

    std::vector<std::string> ids;
    for (const auto& ut : unit_types) {
        ids.push_back(boost::uuids::to_string(ut.id));
    }

    repo.remove(h.context(), ids);

    // Each removed row is gone from the read.
    for (const auto& id_str : ids) {
        auto after_remove = repo.read_latest(h.context(), id_str);
        const auto removed_still_present = std::ranges::any_of(
            after_remove, [&](const business_unit_type& t) {
                return boost::uuids::to_string(t.id) == id_str;
            });
        CHECK_FALSE(removed_still_present);
    }

    // The row that was not removed is still returned by the same read.
    const auto keeper_rows = repo.read_latest(h.context(), boost::uuids::to_string(keeper.id));
    REQUIRE(keeper_rows.size() == 1);
    CHECK(keeper_rows[0].name == keeper.name);
}

TEST_CASE("read_latest_business_unit_types_paginated", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto written = generate_synthetic_business_unit_types(5, ctx);
    for (auto& ut : written) {
        ut.change_reason_code = "system.test";
    }

    business_unit_type_repository repo;
    repo.write(h.context(), written);

    auto page = repo.read_latest(h.context(), 0, 2);
    BOOST_LOG_SEV(lg, debug) << "Paginated business unit types: " << page;

    // The page is the front of the full read: it returns the same rows, in the
    // same key order, and each one carries the content the full read reports.
    const auto all = repo.read_latest(h.context());
    REQUIRE(all.size() > 2);
    REQUIRE(page.size() == 2);
    CHECK(page[0].id == all[0].id);
    CHECK(page[0].name == all[0].name);
    CHECK(page[1].id == all[1].id);
    CHECK(page[1].name == all[1].name);
}

TEST_CASE("get_total_business_unit_type_count", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto written = generate_synthetic_business_unit_types(3, ctx);
    for (auto& ut : written) {
        ut.change_reason_code = "system.test";
    }

    business_unit_type_repository repo;
    repo.write(h.context(), written);

    const auto count = repo.get_total_type_count(h.context());
    BOOST_LOG_SEV(lg, debug) << "Total business unit type count: " << count;

    CHECK(count >= written.size());
}

TEST_CASE("read_business_unit_type_at_version", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto ut = generate_synthetic_business_unit_type(ctx);
    ut.change_reason_code = "system.test";
    const auto original_name = ut.name;
    BOOST_LOG_SEV(lg, debug) << "Business unit type: " << ut;

    business_unit_type_repository repo;
    repo.write(h.context(), ut);

    ut.name = original_name + " v2";
    repo.write(h.context(), ut);

    const auto id_str = boost::uuids::to_string(ut.id);
    auto v1 = repo.read_at_version(h.context(), id_str, 1);
    BOOST_LOG_SEV(lg, debug) << "Business unit type at version 1: "
                             << (v1 ? v1->name : "(not found)");

    REQUIRE(v1.has_value());
    CHECK(v1->name == original_name);
    CHECK(v1->version == 1);
}

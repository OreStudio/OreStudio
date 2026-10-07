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
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/regulatory_book_type.hpp"         // IWYU pragma: keep.
#include "ores.refdata.api/domain/regulatory_book_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/regulatory_book_type_generator.hpp"
#include "ores.refdata.core/repository/regulatory_book_type_repository.hpp"
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
using ores::refdata::domain::regulatory_book_type;
using ores::refdata::repository::regulatory_book_type_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_regulatory_book_type", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto rbt = generate_synthetic_regulatory_book_type(ctx);
    rbt.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Regulatory book type: " << rbt;

    regulatory_book_type_repository repo;
    repo.write(h.context(), rbt);

    const auto read_regulatory_book_types = repo.read_latest(h.context(), rbt.code);
    REQUIRE(read_regulatory_book_types.size() == 1);
    CHECK(read_regulatory_book_types[0].name == rbt.name);
}

TEST_CASE("write_multiple_regulatory_book_types", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto regulatory_book_types = generate_synthetic_regulatory_book_types(3, ctx);
    for (auto& rbt : regulatory_book_types) {
        rbt.change_reason_code = "system.test";
    }
    BOOST_LOG_SEV(lg, debug) << "Regulatory book types: " << regulatory_book_types;

    regulatory_book_type_repository repo;
    repo.write(h.context(), regulatory_book_types);

    const auto read_regulatory_book_types = repo.read_latest(h.context());
    for (const auto& written : regulatory_book_types) {
        const auto it =
            std::ranges::find_if(read_regulatory_book_types, [&](const regulatory_book_type& rbt) {
                return rbt.code == written.code;
            });
        REQUIRE(it != read_regulatory_book_types.end());
        CHECK(it->name == written.name);
    }
}

TEST_CASE("read_latest_regulatory_book_types", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto written_regulatory_book_types = generate_synthetic_regulatory_book_types(3, ctx);
    for (auto& rbt : written_regulatory_book_types) {
        rbt.change_reason_code = "system.test";
    }
    BOOST_LOG_SEV(lg, debug) << "Written regulatory book types: " << written_regulatory_book_types;

    regulatory_book_type_repository repo;
    repo.write(h.context(), written_regulatory_book_types);

    auto read_regulatory_book_types = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read regulatory book types: " << read_regulatory_book_types;

    for (const auto& written : written_regulatory_book_types) {
        const auto it =
            std::ranges::find_if(read_regulatory_book_types, [&](const regulatory_book_type& rbt) {
                return rbt.code == written.code;
            });
        REQUIRE(it != read_regulatory_book_types.end());
        CHECK(it->name == written.name);
    }
}

TEST_CASE("read_latest_regulatory_book_type_by_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto rbt = generate_synthetic_regulatory_book_type(ctx);
    rbt.change_reason_code = "system.test";
    const auto original_name = rbt.name;
    BOOST_LOG_SEV(lg, debug) << "Regulatory book type: " << rbt;

    regulatory_book_type_repository repo;
    repo.write(h.context(), rbt);

    rbt.name = original_name + " v2";
    repo.write(h.context(), rbt);

    auto read_regulatory_book_types = repo.read_latest(h.context(), rbt.code);
    BOOST_LOG_SEV(lg, debug) << "Read regulatory book types: " << read_regulatory_book_types;

    REQUIRE(read_regulatory_book_types.size() == 1);
    CHECK(read_regulatory_book_types[0].code == rbt.code);
    CHECK(read_regulatory_book_types[0].name == original_name + " v2");
}

TEST_CASE("read_nonexistent_regulatory_book_type_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    regulatory_book_type_repository repo;

    // Write a row the read can answer with, so that an empty answer is the key
    // at work rather than a read that does nothing.
    auto keeper = generate_synthetic_regulatory_book_type(ctx);
    keeper.change_reason_code = "system.test";
    repo.write(h.context(), keeper);

    const std::string nonexistent_code = "NONEXISTENT_CODE_12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent code: " << nonexistent_code;

    auto read_regulatory_book_types = repo.read_latest(h.context(), nonexistent_code);
    BOOST_LOG_SEV(lg, debug) << "Read regulatory book types: " << read_regulatory_book_types;

    const auto answered_with_another_key =
        std::ranges::any_of(read_regulatory_book_types, [&](const regulatory_book_type& rbt) {
            return rbt.code == nonexistent_code;
        });
    CHECK_FALSE(answered_with_another_key);

    // The same read does answer for the key that was written.
    const auto keeper_rows = repo.read_latest(h.context(), keeper.code);
    REQUIRE(keeper_rows.size() == 1);
    CHECK(keeper_rows[0].name == keeper.name);
}

TEST_CASE("read_regulatory_book_types_versions_by_key", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto rbt = generate_synthetic_regulatory_book_type(ctx);
    rbt.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Regulatory book type: " << rbt;

    regulatory_book_type_repository repo;
    repo.write(h.context(), rbt);

    CHECK_FALSE(repo.read_at_version(h.context(), rbt.code, 99).has_value());
    const auto versions = repo.read_all(h.context(), rbt.code);
    REQUIRE(versions.size() == 1);
    CHECK(versions[0].code == rbt.code);
}

TEST_CASE("read_regulatory_book_types_versions_resolve_each_version", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto rbt = generate_synthetic_regulatory_book_type(ctx);
    rbt.change_reason_code = "system.test";
    const auto original_name = rbt.name;
    BOOST_LOG_SEV(lg, debug) << "Regulatory book type v1: " << rbt;

    regulatory_book_type_repository repo;
    repo.write(h.context(), rbt);

    rbt.name = original_name + " v2";
    BOOST_LOG_SEV(lg, debug) << "Regulatory book type v2: " << rbt;
    repo.write(h.context(), rbt);

    const auto versions = repo.read_all(h.context(), rbt.code);
    REQUIRE(versions.size() == 2);
    CHECK(versions[0].name == original_name + " v2");
    CHECK(versions[1].name == original_name);

    const auto at_v1 = repo.read_at_version(h.context(), rbt.code, 1);
    REQUIRE(at_v1.has_value());
    CHECK(at_v1->name == original_name);

    const auto at_v2 = repo.read_at_version(h.context(), rbt.code, 2);
    REQUIRE(at_v2.has_value());
    CHECK(at_v2->name == original_name + " v2");
}

TEST_CASE("read_latest_regulatory_book_types_includes_the_written_row", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto rbt = generate_synthetic_regulatory_book_type(ctx);
    rbt.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Regulatory book type: " << rbt;

    regulatory_book_type_repository repo;
    repo.write(h.context(), rbt);

    const auto read_regulatory_book_types = repo.read_latest(h.context());
    const auto found = std::ranges::any_of(read_regulatory_book_types,
                                           [&](const auto& v) { return v.code == rbt.code; });
    CHECK(found);
}

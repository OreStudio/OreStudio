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
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/regulatory_book_type.hpp"         // IWYU pragma: keep.
#include "ores.refdata.api/domain/regulatory_book_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/regulatory_book_type_generator.hpp"
#include "ores.refdata.core/repository/regulatory_book_type_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <thread>

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

std::string now_as_of() {
    return ores::platform::time::datetime::to_db_string(std::chrono::system_clock::now());
}

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
    CHECK_NOTHROW(repo.write(h.context(), rbt));
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
    CHECK_NOTHROW(repo.write(h.context(), regulatory_book_types));
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

    CHECK(read_regulatory_book_types.size() >= written_regulatory_book_types.size());
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
    regulatory_book_type_repository repo;

    const std::string nonexistent_code = "NONEXISTENT_CODE_12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent code: " << nonexistent_code;

    auto read_regulatory_book_types = repo.read_latest(h.context(), nonexistent_code);
    BOOST_LOG_SEV(lg, debug) << "Read regulatory book types: " << read_regulatory_book_types;

    CHECK(read_regulatory_book_types.size() == 0);
}

TEST_CASE("read_regulatory_book_type_at_timepoint_before_creation_is_empty", tags) {
    auto lg(make_logger(test_suite));

    const auto as_of_before = now_as_of();
    std::this_thread::sleep_for(std::chrono::seconds(1));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto rbt = generate_synthetic_regulatory_book_type(ctx);
    rbt.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Regulatory book type: " << rbt;

    regulatory_book_type_repository repo;
    repo.write(h.context(), rbt);

    auto read_regulatory_book_types = repo.read_at_timepoint(h.context(), as_of_before, rbt.code);
    BOOST_LOG_SEV(lg, debug) << "Read regulatory book types at timepoint before creation: "
                              << read_regulatory_book_types;

    CHECK(read_regulatory_book_types.size() == 0);
}

TEST_CASE("read_regulatory_book_type_at_timepoint_resolves_prior_version", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto rbt = generate_synthetic_regulatory_book_type(ctx);
    rbt.change_reason_code = "system.test";
    const auto original_name = rbt.name;
    BOOST_LOG_SEV(lg, debug) << "Regulatory book type v1: " << rbt;

    regulatory_book_type_repository repo;
    repo.write(h.context(), rbt);

    std::this_thread::sleep_for(std::chrono::seconds(1));
    const auto as_of_mid = now_as_of();
    std::this_thread::sleep_for(std::chrono::seconds(1));

    rbt.name = original_name + " v2";
    BOOST_LOG_SEV(lg, debug) << "Regulatory book type v2: " << rbt;
    repo.write(h.context(), rbt);

    std::this_thread::sleep_for(std::chrono::seconds(1));
    const auto as_of_after = now_as_of();

    auto read_at_mid = repo.read_at_timepoint(h.context(), as_of_mid, rbt.code);
    BOOST_LOG_SEV(lg, debug) << "Read regulatory book type at mid timepoint: " << read_at_mid;
    REQUIRE(read_at_mid.size() == 1);
    CHECK(read_at_mid[0].name == original_name);

    auto read_at_after = repo.read_at_timepoint(h.context(), as_of_after, rbt.code);
    BOOST_LOG_SEV(lg, debug) << "Read regulatory book type at after timepoint: " << read_at_after;
    REQUIRE(read_at_after.size() == 1);
    CHECK(read_at_after[0].name == original_name + " v2");
}

TEST_CASE("read_regulatory_book_type_at_timepoint_without_code_filter", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto rbt = generate_synthetic_regulatory_book_type(ctx);
    rbt.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Regulatory book type: " << rbt;

    regulatory_book_type_repository repo;
    repo.write(h.context(), rbt);

    std::this_thread::sleep_for(std::chrono::seconds(1));
    const auto as_of = now_as_of();

    auto read_regulatory_book_types = repo.read_at_timepoint(h.context(), as_of);
    BOOST_LOG_SEV(lg, debug) << "Read regulatory book types at timepoint: " << read_regulatory_book_types;

    const auto found = std::ranges::any_of(
        read_regulatory_book_types, [&](const auto& v) { return v.code == rbt.code; });
    CHECK(found);
}

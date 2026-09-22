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
#include "ores.refdata.api/domain/book_status.hpp"         // IWYU pragma: keep.
#include "ores.refdata.api/domain/book_status_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/book_status_generator.hpp"
#include "ores.refdata.core/repository/book_status_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <algorithm>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository]");

}

using namespace ores::refdata::generators;
using ores::refdata::domain::book_status;
using ores::refdata::repository::book_status_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_book_status", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto bs = generate_synthetic_book_status(ctx);
    bs.change_reason_code = "system.test";
    BOOST_LOG_SEV(lg, debug) << "Book status: " << bs;

    book_status_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), bs));
}

TEST_CASE("write_multiple_book_statuses", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto book_statuses = generate_synthetic_book_statuses(3, ctx);
    for (auto& bs : book_statuses) {
        bs.change_reason_code = "system.test";
    }
    BOOST_LOG_SEV(lg, debug) << "Book statuses: " << book_statuses;

    book_status_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), book_statuses));
}

TEST_CASE("read_latest_book_statuses", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto written_book_statuses = generate_synthetic_book_statuses(3, ctx);
    for (auto& bs : written_book_statuses) {
        bs.change_reason_code = "system.test";
    }
    BOOST_LOG_SEV(lg, debug) << "Written book statuses: " << written_book_statuses;

    book_status_repository repo;
    repo.write(h.context(), written_book_statuses);

    auto read_book_statuses = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read book statuses: " << read_book_statuses;

    CHECK(read_book_statuses.size() >= written_book_statuses.size());
}

TEST_CASE("read_latest_book_status_by_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto bs = generate_synthetic_book_status(ctx);
    bs.change_reason_code = "system.test";
    const auto original_name = bs.name;
    BOOST_LOG_SEV(lg, debug) << "Book status: " << bs;

    book_status_repository repo;
    repo.write(h.context(), bs);

    bs.name = original_name + " v2";
    repo.write(h.context(), bs);

    auto read_book_statuses = repo.read_latest(h.context(), bs.code);
    BOOST_LOG_SEV(lg, debug) << "Read book statuses: " << read_book_statuses;

    REQUIRE(read_book_statuses.size() == 1);
    CHECK(read_book_statuses[0].code == bs.code);
    CHECK(read_book_statuses[0].name == original_name + " v2");
}

TEST_CASE("read_nonexistent_book_status_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    book_status_repository repo;

    const std::string nonexistent_code = "NONEXISTENT_CODE_12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent code: " << nonexistent_code;

    auto read_book_statuses = repo.read_latest(h.context(), nonexistent_code);
    BOOST_LOG_SEV(lg, debug) << "Read book statuses: " << read_book_statuses;

    CHECK(read_book_statuses.size() == 0);
}

TEST_CASE("read_book_status_versions_by_code", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto bs = generate_synthetic_book_status(ctx);
    bs.change_reason_code = "system.test";

    book_status_repository repo;
    repo.write(h.context(), bs);

    // A version that was never written has no row to answer with.
    CHECK_FALSE(repo.read_at_version(h.context(), bs.code, 99).has_value());

    const auto versions = repo.read_all(h.context(), bs.code);
    REQUIRE(versions.size() == 1);
    CHECK(versions[0].code == bs.code);
}

TEST_CASE("read_book_status_versions_resolve_each_version", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto bs = generate_synthetic_book_status(ctx);
    bs.change_reason_code = "system.test";
    const auto original_name = bs.name;
    BOOST_LOG_SEV(lg, debug) << "Book status v1: " << bs;

    book_status_repository repo;
    repo.write(h.context(), bs);

    bs.name = original_name + " v2";
    BOOST_LOG_SEV(lg, debug) << "Book status v2: " << bs;
    repo.write(h.context(), bs);

    // The version axis is a read of its own: every version is addressable,
    // and the newest is the one a latest read returns.
    const auto versions = repo.read_all(h.context(), bs.code);
    REQUIRE(versions.size() == 2);
    CHECK(versions[0].name == original_name + " v2");
    CHECK(versions[1].name == original_name);

    const auto at_v1 = repo.read_at_version(h.context(), bs.code, 1);
    REQUIRE(at_v1.has_value());
    CHECK(at_v1->name == original_name);

    const auto at_v2 = repo.read_at_version(h.context(), bs.code, 2);
    REQUIRE(at_v2.has_value());
    CHECK(at_v2->name == original_name + " v2");

    const auto latest = repo.read_latest(h.context(), bs.code);
    REQUIRE(latest.size() == 1);
    CHECK(latest[0].name == original_name + " v2");
}

TEST_CASE("read_latest_book_statuses_includes_the_written_row", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto bs = generate_synthetic_book_status(ctx);
    bs.change_reason_code = "system.test";

    book_status_repository repo;
    repo.write(h.context(), bs);

    const auto rows = repo.read_latest(h.context());
    const auto found =
        std::ranges::any_of(rows, [&](const auto& v) { return v.code == bs.code; });
    CHECK(found);
}

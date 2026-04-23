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
#include "ores.compute.core/repository/app_version_platform_repository.hpp"

#include <algorithm>
#include <string>
#include <vector>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.compute.core/repository/platform_repository.hpp"

namespace {

const std::string_view test_suite("ores.compute.tests");
const std::string tags("[repository][app_version_platform]");

using ores::testing::database_helper;
using ores::compute::domain::app_version_platform;
using ores::compute::domain::compute_platform;
using ores::compute::repository::app_version_platform_repository;
using ores::compute::repository::platform_repository;
using namespace ores::logging;

/// Look up the seeded compute platforms; most tests need at least two
/// distinct rows to exercise add/remove semantics on the junction.
std::vector<compute_platform> seeded_platforms(database_helper& h) {
    platform_repository repo;
    auto platforms = repo.read_active(h.context());
    REQUIRE(platforms.size() >= 2);
    return platforms;
}

app_version_platform make_row(const database_helper& h,
    const boost::uuids::uuid& av_id, const compute_platform& p,
    const std::string& uri_suffix) {
    app_version_platform r;
    r.tenant_id = h.tenant_id();
    r.app_version_id = av_id;
    r.platform_id = p.id;
    r.platform_code = p.code;
    r.package_uri =
        "/api/v1/storage/compute/packages/test/" + p.code + "/" + uri_suffix;
    return r;
}

/// list_for_version reports platform_code via a JOIN — callers compare
/// codes rather than ids, so surface them sorted for stable equality.
std::vector<std::string> sorted_codes(
    const std::vector<app_version_platform>& rows) {
    std::vector<std::string> codes;
    codes.reserve(rows.size());
    for (const auto& r : rows) codes.push_back(r.platform_code);
    std::sort(codes.begin(), codes.end());
    return codes;
}

} // namespace

TEST_CASE("replace_for_version_initial_insert_lists_all_rows", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const auto platforms = seeded_platforms(h);
    const auto av_id = boost::uuids::random_generator()();
    const auto av_id_str = boost::uuids::to_string(av_id);

    app_version_platform_repository repo;
    std::vector<app_version_platform> rows {
        make_row(h, av_id, platforms[0], "pkg-a.tar.gz"),
        make_row(h, av_id, platforms[1], "pkg-b.tar.gz"),
    };

    CHECK_NOTHROW(repo.replace_for_version(h.context(), av_id_str, rows,
        h.db_user(), h.db_user(), "system.new_record", "initial insert"));

    const auto listed = repo.list_for_version(h.context(), av_id_str);
    CHECK(listed.size() == 2);
    const auto got = sorted_codes(listed);
    const auto want = std::vector<std::string>{platforms[0].code, platforms[1].code};
    auto want_sorted = want;
    std::sort(want_sorted.begin(), want_sorted.end());
    CHECK(got == want_sorted);
    for (const auto& r : listed)
        CHECK(!r.package_uri.empty());
}

TEST_CASE("replace_for_version_swap_removes_old_adds_new", tags) {
    database_helper h;
    const auto platforms = seeded_platforms(h);
    REQUIRE(platforms.size() >= 3);

    const auto av_id = boost::uuids::random_generator()();
    const auto av_id_str = boost::uuids::to_string(av_id);

    app_version_platform_repository repo;
    // Seed with platforms[0] + platforms[1].
    repo.replace_for_version(h.context(), av_id_str, {
        make_row(h, av_id, platforms[0], "old-a.tar.gz"),
        make_row(h, av_id, platforms[1], "old-b.tar.gz"),
    }, h.db_user(), h.db_user(), "system.new_record", "initial");

    // Replace with platforms[0] + platforms[2]; platforms[1] should be gone.
    repo.replace_for_version(h.context(), av_id_str, {
        make_row(h, av_id, platforms[0], "new-a.tar.gz"),
        make_row(h, av_id, platforms[2], "new-c.tar.gz"),
    }, h.db_user(), h.db_user(), "system.new_record", "swap");

    const auto listed = repo.list_for_version(h.context(), av_id_str);
    CHECK(listed.size() == 2);
    const auto got = sorted_codes(listed);
    auto want = std::vector<std::string>{platforms[0].code, platforms[2].code};
    std::sort(want.begin(), want.end());
    CHECK(got == want);

    // URI for the platform that survived must be the new one, not the old one.
    for (const auto& r : listed) {
        if (r.platform_code == platforms[0].code)
            CHECK(r.package_uri.find("new-a.tar.gz") != std::string::npos);
        else if (r.platform_code == platforms[2].code)
            CHECK(r.package_uri.find("new-c.tar.gz") != std::string::npos);
    }
}

TEST_CASE("replace_for_version_empty_set_drops_all_active", tags) {
    database_helper h;
    const auto platforms = seeded_platforms(h);
    const auto av_id = boost::uuids::random_generator()();
    const auto av_id_str = boost::uuids::to_string(av_id);

    app_version_platform_repository repo;
    repo.replace_for_version(h.context(), av_id_str, {
        make_row(h, av_id, platforms[0], "seed.tar.gz"),
    }, h.db_user(), h.db_user(), "system.new_record", "seed");
    REQUIRE(repo.list_for_version(h.context(), av_id_str).size() == 1);

    repo.replace_for_version(h.context(), av_id_str, {},
        h.db_user(), h.db_user(), "system.new_record", "clear");

    CHECK(repo.list_for_version(h.context(), av_id_str).empty());
}

TEST_CASE("replace_for_version_is_idempotent", tags) {
    database_helper h;
    const auto platforms = seeded_platforms(h);
    const auto av_id = boost::uuids::random_generator()();
    const auto av_id_str = boost::uuids::to_string(av_id);

    std::vector<app_version_platform> rows {
        make_row(h, av_id, platforms[0], "same.tar.gz"),
        make_row(h, av_id, platforms[1], "same.tar.gz"),
    };

    app_version_platform_repository repo;
    repo.replace_for_version(h.context(), av_id_str, rows,
        h.db_user(), h.db_user(), "system.new_record", "first");
    repo.replace_for_version(h.context(), av_id_str, rows,
        h.db_user(), h.db_user(), "system.new_record", "same again");

    const auto listed = repo.list_for_version(h.context(), av_id_str);
    CHECK(listed.size() == 2);
    const auto got = sorted_codes(listed);
    auto want = std::vector<std::string>{platforms[0].code, platforms[1].code};
    std::sort(want.begin(), want.end());
    CHECK(got == want);
}

TEST_CASE("list_for_version_returns_empty_for_unknown_id", tags) {
    database_helper h;
    const auto unknown = boost::uuids::to_string(
        boost::uuids::random_generator()());

    app_version_platform_repository repo;
    CHECK(repo.list_for_version(h.context(), unknown).empty());
}

TEST_CASE("list_for_version_filters_by_app_version", tags) {
    database_helper h;
    const auto platforms = seeded_platforms(h);
    const auto av_a = boost::uuids::random_generator()();
    const auto av_b = boost::uuids::random_generator()();

    app_version_platform_repository repo;
    repo.replace_for_version(h.context(), boost::uuids::to_string(av_a), {
        make_row(h, av_a, platforms[0], "a0.tar.gz"),
        make_row(h, av_a, platforms[1], "a1.tar.gz"),
    }, h.db_user(), h.db_user(), "system.new_record", "av_a");
    repo.replace_for_version(h.context(), boost::uuids::to_string(av_b), {
        make_row(h, av_b, platforms[0], "b0.tar.gz"),
    }, h.db_user(), h.db_user(), "system.new_record", "av_b");

    const auto a_rows = repo.list_for_version(
        h.context(), boost::uuids::to_string(av_a));
    const auto b_rows = repo.list_for_version(
        h.context(), boost::uuids::to_string(av_b));

    CHECK(a_rows.size() == 2);
    CHECK(b_rows.size() == 1);
    for (const auto& r : a_rows)
        CHECK(r.package_uri.find("/a") != std::string::npos);
    for (const auto& r : b_rows)
        CHECK(r.package_uri.find("/b") != std::string::npos);
}

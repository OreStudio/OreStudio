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
#include "ores.compute.core/repository/platform_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

// The canonical junction surface writes and removes rows one claim at a time:
// the repository offers no operation that replaces a whole set for one parent,
// because the protocol retired that operation. A caller that wants the set
// replaced composes the writes and the removals itself.

namespace {

const std::string tags("[repository][app_version_platform]");

using ores::testing::database_helper;
using ores::compute::domain::app_version_platform;
using ores::compute::domain::compute_platform;
using ores::compute::repository::app_version_platform_repository;
using ores::compute::repository::platform_repository;
using namespace ores::logging;

/// Look up the seeded compute platforms; most tests need at least two
/// distinct rows to exercise the junction.
std::vector<compute_platform> seeded_platforms(database_helper& h) {
    platform_repository repo;
    auto platforms = repo.read_active(h.context());
    REQUIRE(platforms.size() >= 2);
    return platforms;
}

app_version_platform make_row(database_helper& h,
                              const boost::uuids::uuid& av_id,
                              const compute_platform& p,
                              const std::string& uri_suffix) {
    app_version_platform r;
    r.tenant_id = h.tenant_id().to_string();
    r.app_version_id = av_id;
    r.platform_id = p.id;
    r.platform_code = p.code;
    r.package_uri = "/api/v1/storage/compute/packages/test/" + p.code + "/" + uri_suffix;
    return r;
}

} // namespace

TEST_CASE("read_latest_by_app_version_returns_empty_for_unknown_id", tags) {
    database_helper h;
    const auto unknown = boost::uuids::random_generator()();

    app_version_platform_repository repo(h.context());
    CHECK(repo.read_latest_by_app_version(unknown).empty());
}

TEST_CASE("read_latest_by_app_version_reports_the_rows_written_for_each_parent", tags) {
    database_helper h;
    const auto platforms = seeded_platforms(h);
    const auto av_a = boost::uuids::random_generator()();
    const auto av_b = boost::uuids::random_generator()();

    app_version_platform_repository repo(h.context());
    repo.write({make_row(h, av_a, platforms[0], "a0.tar.gz"),
                make_row(h, av_a, platforms[1], "a1.tar.gz")});
    repo.write({make_row(h, av_b, platforms[0], "b0.tar.gz")});

    const auto a_rows = repo.read_latest_by_app_version(av_a);
    const auto b_rows = repo.read_latest_by_app_version(av_b);

    CHECK(a_rows.size() == 2);
    CHECK(b_rows.size() == 1);
    for (const auto& r : a_rows)
        CHECK(r.package_uri.find("/a") != std::string::npos);
    for (const auto& r : b_rows)
        CHECK(r.package_uri.find("/b") != std::string::npos);
}

TEST_CASE("remove_by_app_version_leaves_the_other_parent_alone", tags) {
    database_helper h;
    const auto platforms = seeded_platforms(h);
    const auto av_a = boost::uuids::random_generator()();
    const auto av_b = boost::uuids::random_generator()();

    app_version_platform_repository repo(h.context());
    repo.write({make_row(h, av_a, platforms[0], "a0.tar.gz")});
    repo.write({make_row(h, av_b, platforms[0], "b0.tar.gz")});

    CHECK_NOTHROW(repo.remove_by_app_version(av_a));

    CHECK(repo.read_latest_by_app_version(av_a).empty());
    CHECK(repo.read_latest_by_app_version(av_b).size() == 1);
}

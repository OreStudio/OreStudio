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
#include "ores.refdata.api/domain/cds_convention.hpp"         // IWYU pragma: keep.
#include "ores.refdata.api/domain/cds_convention_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.api/generators/cds_convention_generator.hpp"
#include "ores.refdata.core/repository/cds_convention_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include <algorithm>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[repository][workspace_inheritance]");

}

using namespace ores::refdata::generators;
using ores::refdata::domain::cds_convention;
using ores::refdata::repository::cds_convention_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("child_workspace_with_no_local_rows_inherits_parent_rows", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);

    // Row written with no explicit workspace override: defaults to the Live
    // sentinel, per cds_convention's domain struct default.
    auto convention = generate_synthetic_cds_convention(gen_ctx);
    const auto live_id = convention.id;
    cds_convention_repository repo;
    repo.write(h.context(), convention);

    // A workspace that has never had this row written to it directly --
    // resolution chain [child, Live] means data absent from child should
    // fall through to Live.
    const auto child_workspace_id = boost::uuids::to_string(boost::uuids::random_generator()());
    const auto live_workspace_id = boost::uuids::to_string(ores::utility::uuid::live_workspace_id());

    auto ctx_with_chain =
        h.context().with_workspace_resolution({child_workspace_id, live_workspace_id});

    const auto results = repo.read_latest(ctx_with_chain);

    const auto found = std::ranges::find_if(
        results, [&](const auto& c) { return c.id == live_id; });
    REQUIRE(found != results.end());
}

TEST_CASE("empty_resolution_chain_falls_back_to_exact_match", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto gen_ctx = ores::testing::make_generation_context(h);

    auto convention = generate_synthetic_cds_convention(gen_ctx);
    const auto live_id = convention.id;
    cds_convention_repository repo;
    repo.write(h.context(), convention);

    // No resolution chain set (default context): exact-match behaviour is
    // unchanged from before this task.
    const auto results = repo.read_latest(h.context());

    const auto found = std::ranges::find_if(
        results, [&](const auto& c) { return c.id == live_id; });
    REQUIRE(found != results.end());
}

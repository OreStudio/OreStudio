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
#include "ores.compute.api/domain/result.hpp"
#include "ores.compute.api/generators/app_generator.hpp"
#include "ores.compute.api/generators/app_version_generator.hpp"
#include "ores.compute.api/generators/batch_generator.hpp"
#include "ores.compute.api/generators/result_generator.hpp"
#include "ores.compute.api/generators/workunit_generator.hpp"
#include "ores.compute.api/messaging/result_protocol.hpp"
#include "ores.compute.core/repository/app_repository.hpp"
#include "ores.compute.core/repository/app_version_repository.hpp"
#include "ores.compute.core/repository/batch_repository.hpp"
#include "ores.compute.core/repository/result_repository.hpp"
#include "ores.compute.core/repository/workunit_repository.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "test_helpers.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

/*
 * A result states when a node took it and how long the engine ran, and each is unknown, not
 * zero, until the run reports it. The two columns round-trip through the repository, and a
 * row written without them reads back as unknown.
 */

namespace {

using ores::compute::repository::result_repository;
using ores::testing::scoped_database_helper;

std::string repository_result_timing_unique_marker() {
    auto text = boost::uuids::to_string(boost::uuids::random_generator()());
    text.erase(std::remove(text.begin(), text.end(), '-'), text.end());
    return "out" + text.substr(0, 10);
}

std::optional<ores::compute::domain::result> read_marked(const ores::database::context& ctx,
                                                         const std::string& marker) {
    for (const auto& r : result_repository{}.read_latest(ctx))
        if (r.error_message == marker)
            return r;
    return std::nullopt;
}

} // namespace

TEST_CASE("a result keeps the time a node took it and how long the engine ran",
          "[repository][timing]") {
    scoped_database_helper h;
    const auto ctx = party_context(h);
    const auto workunit = seed_workunit(h, ctx);
    const auto marker = repository_result_timing_unique_marker();

    auto gen_ctx = ores::testing::make_generation_context(h);
    auto r = ores::compute::generators::generate_synthetic_result(gen_ctx);
    r.change_reason_code = "system.test";
    r.party_id = *ctx.party_id();
    r.workunit_id = workunit;
    r.error_message = marker;
    const auto taken = std::chrono::floor<std::chrono::milliseconds>(
        std::chrono::system_clock::now() - std::chrono::minutes(2));
    r.started_at = taken;
    r.duration_ms = 1234;
    result_repository{}.write(ctx, r);

    const auto read = read_marked(ctx, marker);
    REQUIRE(read.has_value());
    // A timestamp reads back in whole seconds, as every timestamp of the repository does.
    CHECK(std::chrono::floor<std::chrono::seconds>(read->started_at) ==
          std::chrono::floor<std::chrono::seconds>(taken));
    REQUIRE(read->duration_ms.has_value());
    CHECK(*read->duration_ms == 1234);
}

TEST_CASE("a result written without the two columns reads back as unknown",
          "[repository][timing]") {
    scoped_database_helper h;
    const auto ctx = party_context(h);
    const auto workunit = seed_workunit(h, ctx);
    const auto marker = repository_result_timing_unique_marker();

    auto gen_ctx = ores::testing::make_generation_context(h);
    auto r = ores::compute::generators::generate_synthetic_result(gen_ctx);
    r.change_reason_code = "system.test";
    r.party_id = *ctx.party_id();
    r.workunit_id = workunit;
    r.error_message = marker;
    r.started_at = {};
    r.duration_ms = std::nullopt;
    result_repository{}.write(ctx, r);

    const auto read = read_marked(ctx, marker);
    REQUIRE(read.has_value());
    CHECK(read->started_at == std::chrono::system_clock::time_point{});
    CHECK_FALSE(read->duration_ms.has_value());
}

TEST_CASE("a later version of a result keeps the duration and not a zero", "[repository][timing]") {
    scoped_database_helper h;
    const auto ctx = party_context(h);
    const auto workunit = seed_workunit(h, ctx);
    const auto marker = repository_result_timing_unique_marker();

    auto gen_ctx = ores::testing::make_generation_context(h);
    auto r = ores::compute::generators::generate_synthetic_result(gen_ctx);
    r.change_reason_code = "system.test";
    r.party_id = *ctx.party_id();
    r.workunit_id = workunit;
    r.error_message = marker;
    r.duration_ms = 77;
    result_repository{}.write(ctx, r);

    auto again = *read_marked(ctx, marker);
    again.change_reason_code = "system.test";
    again.server_state = 5;
    result_repository{}.write(ctx, again);

    const auto read = read_marked(ctx, marker);
    REQUIRE(read.has_value());
    CHECK(read->server_state == 5);
    REQUIRE(read->duration_ms.has_value());
    CHECK(*read->duration_ms == 77);
}

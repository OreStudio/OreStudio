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
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>
#include <vector>

/*
 * A result list narrows by outcome, which answers which of the runs failed. The
 * filter applies before the page and the count, and it only narrows what the row
 * policy lets the session read.
 */

namespace {

const std::string tags("[repository][filter]");

using ores::compute::messaging::results_filter;
using ores::compute::repository::result_repository;
using ores::testing::scoped_database_helper;

std::string unique_marker() {
    auto text = boost::uuids::to_string(boost::uuids::random_generator()());
    text.erase(std::remove(text.begin(), text.end(), '-'), text.end());
    return "out" + text.substr(0, 10);
}

/// A party in the test tenant, because a result write is scoped to a party.
ores::database::context party_context(scoped_database_helper& h) {
    using ores::refdata::repository::party_repository;
    auto gen_ctx = ores::testing::make_generation_context(h);
    party_repository repo;
    auto party = ores::refdata::generators::generate_synthetic_party(gen_ctx);
    party.change_reason_code = "system.test";
    for (const auto& e : repo.read_latest(h.context())) {
        if (e.tenant_id == party.tenant_id) {
            party.parent_party_id = e.id;
            break;
        }
    }
    repo.write(h.context(), party);
    return h.context().with_party(h.tenant_id(), party.id, {party.id}, h.db_user());
}

/// The workunit a result belongs to, with the batch, app and app version above it.
/// The insert trigger of a row refuses a key that names no active parent.
boost::uuids::uuid seed_workunit(scoped_database_helper& h, const ores::database::context& ctx) {
    namespace gen = ores::compute::generators;
    namespace repo = ores::compute::repository;
    auto gen_ctx = ores::testing::make_generation_context(h);
    auto app = gen::generate_synthetic_app(gen_ctx);
    auto version = gen::generate_synthetic_app_version(gen_ctx);
    auto batch = gen::generate_synthetic_batch(gen_ctx);
    auto workunit = gen::generate_synthetic_workunit(gen_ctx);
    for (auto* row : {&app.change_reason_code,
                      &version.change_reason_code,
                      &batch.change_reason_code,
                      &workunit.change_reason_code})
        *row = "system.test";
    batch.party_id = *ctx.party_id();
    workunit.party_id = *ctx.party_id();
    repo::app_repository{}.write(ctx, app);
    version.app_id = app.id;
    repo::app_version_repository{}.write(ctx, version);
    repo::batch_repository{}.write(ctx, batch);
    workunit.batch_id = batch.id;
    workunit.app_version_id = version.id;
    repo::workunit_repository{}.write(ctx, workunit);
    return workunit.id;
}

void write_result(scoped_database_helper& h,
                  const ores::database::context& ctx,
                  const boost::uuids::uuid& workunit_id,
                  const std::string& marker,
                  int outcome) {
    auto gen_ctx = ores::testing::make_generation_context(h);
    auto r = ores::compute::generators::generate_synthetic_result(gen_ctx);
    r.change_reason_code = "system.test";
    r.party_id = *ctx.party_id();
    r.workunit_id = workunit_id;
    r.outcome = outcome;
    r.error_message = marker;
    result_repository{}.write(ctx, r);
}

std::size_t count_marked(const ores::database::context& ctx,
                         const std::string& marker,
                         const results_filter& filter) {
    std::size_t n = 0;
    for (const auto& r : result_repository{}.read_latest(ctx, 0, 1000, {}, std::optional(filter)))
        if (r.error_message == marker)
            ++n;
    return n;
}

} // namespace

TEST_CASE("a result list narrows by outcome and finds the runs that failed", tags) {
    scoped_database_helper h;
    const auto ctx = party_context(h);
    const auto m = unique_marker();
    const auto workunit = seed_workunit(h, ctx);
    write_result(h, ctx, workunit, m, 1);
    write_result(h, ctx, workunit, m, 3);
    write_result(h, ctx, workunit, m, 3);
    write_result(h, ctx, workunit, m, 4);

    results_filter failed;
    failed.outcome = 3;
    results_filter none;
    none.outcome = 0;
    results_filter any_failure;
    any_failure.outcome_one_of = std::vector<int>{3, 4};

    CHECK(count_marked(ctx, m, failed) == 2);
    CHECK(count_marked(ctx, m, none) == 0);
    CHECK(count_marked(ctx, m, any_failure) == 3);
    CHECK(count_marked(ctx, m, results_filter{}) == 4);
}

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
#include "ores.compute.api/domain/batch.hpp"
#include "ores.compute.api/generators/batch_generator.hpp"
#include "ores.compute.core/repository/batch_repository.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>

/*
 * The batch model declares the recording time sortable and orders by it,
 * newest first. A batch has a random identifier, so the key says nothing about
 * when the batch was written, and each case writes its batches one after the
 * other with a pause between them.
 */

namespace {

using ores::compute::repository::batch_repository;
using ores::testing::scoped_database_helper;
using ores::utility::domain::order;

std::string unique_marker() {
    auto text = boost::uuids::to_string(boost::uuids::random_generator()());
    text.erase(std::remove(text.begin(), text.end(), '-'), text.end());
    return "ord" + text.substr(0, 10);
}

/// A party in the test tenant, because a batch write is scoped to a party.
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

/// Writes one batch per suffix, in turn, and returns the references in write order.
std::vector<std::string> write_in_turn(scoped_database_helper& h,
                                       const ores::database::context& ctx,
                                       const std::string& marker,
                                       int count) {
    auto gen_ctx = ores::testing::make_generation_context(h);
    std::vector<std::string> refs;
    for (int i = 0; i < count; ++i) {
        auto b = ores::compute::generators::generate_synthetic_batch(gen_ctx);
        b.change_reason_code = "system.test";
        b.party_id = *ctx.party_id();
        b.external_ref = marker + "_" + std::to_string(i);
        batch_repository{}.write(ctx, b);
        refs.push_back(b.external_ref);
        std::this_thread::sleep_for(std::chrono::milliseconds(20));
    }
    return refs;
}

/// Every batch of the party in the stated order, keeping only the marked ones.
std::vector<std::string>
refs_in_order(const ores::database::context& ctx, const std::string& marker, const order& o) {
    std::vector<std::string> r;
    for (const auto& b : batch_repository{}.read_latest(ctx, 0, 1000, o))
        if (b.external_ref.starts_with(marker))
            r.push_back(b.external_ref);
    return r;
}

}

TEST_CASE("batches read newest first when no order is stated", "[repository][stated_order]") {
    scoped_database_helper h;
    const auto ctx = party_context(h);
    const auto m = unique_marker();
    auto written = write_in_turn(h, ctx, m, 3);
    std::ranges::reverse(written);

    CHECK(refs_in_order(ctx, m, {}) == written);
}

TEST_CASE("a descending order reverses the default and reads oldest first",
          "[repository][stated_order]") {
    scoped_database_helper h;
    const auto ctx = party_context(h);
    const auto m = unique_marker();
    const auto written = write_in_turn(h, ctx, m, 3);

    CHECK(refs_in_order(ctx, m, {.descending = true}) == written);
}

TEST_CASE("the recording time is honoured in both directions when it is stated",
          "[repository][stated_order]") {
    scoped_database_helper h;
    const auto ctx = party_context(h);
    const auto m = unique_marker();
    const auto written = write_in_turn(h, ctx, m, 3);
    auto newest_first = written;
    std::ranges::reverse(newest_first);

    CHECK(refs_in_order(ctx, m, {.field = "recorded_at"}) == written);
    CHECK(refs_in_order(ctx, m, {.field = "recorded_at", .descending = true}) == newest_first);
}

TEST_CASE("the store refuses an order by a field the model does not declare",
          "[repository][stated_order]") {
    scoped_database_helper h;
    const auto ctx = party_context(h);

    CHECK(batch_repository::is_sortable("recorded_at"));
    CHECK_FALSE(batch_repository::is_sortable("valid_from"));
    CHECK_FALSE(batch_repository::is_sortable("external_ref"));
    CHECK_THROWS_AS(batch_repository{}.read_latest(ctx, 0, 10, {.field = "status"}),
                    std::invalid_argument);
}

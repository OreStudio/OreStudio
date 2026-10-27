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
#include "ores.compute.api/messaging/batch_protocol.hpp"
#include "ores.compute.core/repository/batch_repository.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>
#include <vector>

/*
 * A batch list narrows by status, by party and by a part of its reference. The
 * filter applies before the page and the count, so the total is the number of
 * batches that match, and a condition can only narrow what the policy lets the
 * session read.
 */

namespace {

using ores::compute::messaging::batches_filter;
using ores::compute::repository::batch_repository;
using ores::testing::scoped_database_helper;

std::string unique_marker() {
    auto text = boost::uuids::to_string(boost::uuids::random_generator()());
    text.erase(std::remove(text.begin(), text.end(), '-'), text.end());
    return "flt" + text.substr(0, 10);
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

void write_batch(scoped_database_helper& h,
                 const ores::database::context& ctx,
                 const std::string& ref,
                 const std::string& status) {
    auto gen_ctx = ores::testing::make_generation_context(h);
    auto b = ores::compute::generators::generate_synthetic_batch(gen_ctx);
    b.change_reason_code = "system.test";
    b.party_id = *ctx.party_id();
    b.external_ref = ref;
    b.status = status;
    batch_repository{}.write(ctx, b);
}

std::vector<std::string>
refs(const ores::database::context& ctx, const std::string& marker, const batches_filter& filter) {
    std::vector<std::string> r;
    for (const auto& b : batch_repository{}.read_latest(ctx, 0, 1000, {}, std::optional(filter)))
        if (b.external_ref.starts_with(marker))
            r.push_back(b.external_ref);
    std::ranges::sort(r);
    return r;
}

} // namespace

TEST_CASE("a batch list narrows by status and counts only the matches", "[repository][filter]") {
    scoped_database_helper h;
    const auto ctx = party_context(h);
    const auto m = unique_marker();
    write_batch(h, ctx, m + "_a", "open");
    write_batch(h, ctx, m + "_b", "closed");
    write_batch(h, ctx, m + "_c", "closed");

    batches_filter closed;
    closed.status = "closed";

    CHECK(refs(ctx, m, closed) == std::vector<std::string>{m + "_b", m + "_c"});
    CHECK(batch_repository{}.get_total_batch_count(ctx, std::optional(closed)) >= 2);
}

TEST_CASE("a batch list narrows by a part of the reference, ignoring case",
          "[repository][filter]") {
    scoped_database_helper h;
    const auto ctx = party_context(h);
    const auto m = unique_marker();
    write_batch(h, ctx, m + "_soak_1", "open");
    write_batch(h, ctx, m + "_other", "open");

    batches_filter find;
    find.search = "SOAK";

    CHECK(refs(ctx, m, find) == std::vector<std::string>{m + "_soak_1"});
}

TEST_CASE("a party filter cannot widen a read past the party of the session",
          "[repository][filter]") {
    scoped_database_helper h;
    const auto ctx = party_context(h);
    const auto m = unique_marker();
    write_batch(h, ctx, m + "_mine", "open");

    batches_filter other;
    other.party_id = std::optional<boost::uuids::uuid>(boost::uuids::random_generator()());
    batches_filter mine;
    mine.party_id = std::optional<boost::uuids::uuid>(*ctx.party_id());

    CHECK(refs(ctx, m, other).empty());
    CHECK(refs(ctx, m, mine) == std::vector<std::string>{m + "_mine"});
}

TEST_CASE("every condition of a filter must hold", "[repository][filter]") {
    scoped_database_helper h;
    const auto ctx = party_context(h);
    const auto m = unique_marker();
    write_batch(h, ctx, m + "_soak_open", "open");
    write_batch(h, ctx, m + "_soak_closed", "closed");

    batches_filter both;
    both.status = "closed";
    both.search = "soak";

    CHECK(refs(ctx, m, both) == std::vector<std::string>{m + "_soak_closed"});
}

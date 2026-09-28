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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workspace.api/generators/workspace_generator.hpp"
#include "ores.workspace.core/repository/workspace_repository.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

// The workspace entity's CRUD set is generated, and the generated eventing test
// exercises its write path end to end. This file covers what that test does
// not: the two repository methods the workspace operations add, the ancestor
// resolution chain and the trade-scope whitelist.
//
// Both need a party-scoped session. The table's restrictive party policy hides
// a workspace whose party the session cannot see, so the party is seeded first
// and the session is pointed at it before any read.

namespace {

const std::string_view test_suite("workspace.tests");
const std::string tags("[repository][workspace]");

using ores::workspace::repository::workspace_repository;

struct seeded {
    boost::uuids::uuid party_id;
    boost::uuids::uuid owner_id;
};

// Seeds the party and account every workspace references, and returns their
// identifiers.
seeded seed_party_and_account(ores::testing::scoped_database_helper& h,
                              ores::utility::generation::generation_context& gctx) {
    auto& party_ctx = h.context();

    auto party = ores::refdata::generators::generate_synthetic_party(gctx);
    party.change_reason_code = "system.test";
    // Only one root party (parent_party_id null) is allowed per tenant, so
    // attach the synthetic party to the existing root rather than to nothing.
    for (const auto& existing :
         ores::refdata::repository::party_repository().read_latest(party_ctx)) {
        if (existing.tenant_id == party.tenant_id) {
            party.parent_party_id = existing.id;
            break;
        }
    }
    ores::refdata::repository::party_repository().write(party_ctx, party);

    auto account = ores::iam::generators::generate_synthetic_account(gctx);
    account.change_reason_code = "system.test";
    ores::iam::repository::account_repository().write(party_ctx, account);

    return {.party_id = party.id, .owner_id = account.id};
}

std::string seed_workspace(ores::database::context ctx,
                           ores::utility::generation::generation_context& gctx,
                           const seeded& s,
                           std::optional<boost::uuids::uuid> parent = std::nullopt) {
    auto ws = ores::workspace::generators::generate_synthetic_workspace(gctx);
    ws.change_reason_code = "system.test";
    ws.party_id = s.party_id;
    ws.owner_id = s.owner_id;
    ws.parent_workspace_id = parent;

    workspace_repository().write(ctx, ws);
    return boost::uuids::to_string(ws.id);
}

long trade_scope_count(ores::database::context ctx, const std::string& workspace_id) {
    auto logger = ores::logging::make_logger(test_suite);
    const auto rows = ores::database::repository::execute_parameterized_string_query(
        ctx,
        "SELECT count(*)::text FROM ores_workspace_trade_scope_tbl"
        " WHERE workspace_id = $1::uuid",
        {workspace_id},
        logger,
        "Counting trade scope entries");
    if (rows.empty())
        throw std::runtime_error("count query returned no row");
    return std::stol(rows.front());
}

} // namespace

using ores::testing::make_generation_context;
using ores::testing::scoped_database_helper;

TEST_CASE("resolution_order walks a workspace's parent chain, closest first", tags) {
    scoped_database_helper h;
    auto gctx = make_generation_context(h);
    const auto s = seed_party_and_account(h, gctx);
    h.set_party(s.party_id);
    auto ctx = h.context();

    const auto parent_id = seed_workspace(ctx, gctx, s);
    const auto child_id = seed_workspace(ctx, gctx, s, boost::uuids::string_generator{}(parent_id));

    const auto chain = workspace_repository().resolution_order(ctx, child_id);
    REQUIRE(chain.size() == 2);
    CHECK(chain.at(0) == child_id);
    CHECK(chain.at(1) == parent_id);
}

TEST_CASE("trade scope replaces and clears a workspace's whitelist", tags) {
    scoped_database_helper h;
    auto gctx = make_generation_context(h);
    const auto s = seed_party_and_account(h, gctx);
    h.set_party(s.party_id);
    auto ctx = h.context();

    const auto workspace_id = seed_workspace(ctx, gctx, s);

    boost::uuids::random_generator gen;
    const std::vector<boost::uuids::uuid> two_trades{gen(), gen()};

    workspace_repository().set_trade_scope(ctx, workspace_id, two_trades);
    CHECK(trade_scope_count(ctx, workspace_id) == 2);

    workspace_repository().set_trade_scope(ctx, workspace_id, {two_trades.front()});
    CHECK(trade_scope_count(ctx, workspace_id) == 1);

    workspace_repository().clear_trade_scope(ctx, workspace_id);
    CHECK(trade_scope_count(ctx, workspace_id) == 0);
}

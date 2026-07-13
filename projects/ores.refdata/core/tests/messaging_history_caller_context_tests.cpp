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
#include "ores.refdata.core/messaging/history_caller_context.hpp"
#include "ores.testing/database_helper.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[messaging][history_caller_context]");

}

using ores::refdata::messaging::pack_history_caller_context;
using ores::refdata::messaging::unpack_history_caller_context;
using ores::testing::database_helper;

TEST_CASE("unpack_history_caller_context_round_trips_tenant_id", tags) {
    database_helper h;
    const auto& ctx = h.context();

    const auto packed = pack_history_caller_context(ctx);
    const auto unpacked = unpack_history_caller_context(ctx, packed);

    CHECK(unpacked.tenant_id() == ctx.tenant_id());
}

TEST_CASE("unpack_history_caller_context_round_trips_actor_and_roles", tags) {
    database_helper h;
    const auto ctx = h.context().with_roles({"refdata::currencies:read", "refdata::currencies:write"});

    const auto packed = pack_history_caller_context(ctx);
    const auto unpacked = unpack_history_caller_context(ctx, packed);

    CHECK(unpacked.actor() == ctx.actor());
    CHECK(unpacked.roles() == ctx.roles());
}

TEST_CASE("unpack_history_caller_context_round_trips_party_and_visible_parties", tags) {
    database_helper h;
    boost::uuids::random_generator gen;
    const auto party_id = gen();
    const std::vector<boost::uuids::uuid> visible{party_id, gen()};

    const auto ctx = h.context().with_party(h.tenant_id(), party_id, visible, "alice");

    const auto packed = pack_history_caller_context(ctx);
    const auto unpacked = unpack_history_caller_context(ctx, packed);

    REQUIRE(unpacked.party_id().has_value());
    CHECK(*unpacked.party_id() == party_id);
    CHECK(unpacked.visible_party_ids() == visible);
}

TEST_CASE("unpack_history_caller_context_round_trips_workspace", tags) {
    database_helper h;
    const auto ctx = h.context().with_workspace("some-workspace-id");

    const auto packed = pack_history_caller_context(ctx);
    const auto unpacked = unpack_history_caller_context(ctx, packed);

    CHECK(unpacked.workspace_id() == "some-workspace-id");
}

TEST_CASE("unpack_history_caller_context_throws_on_malformed_input", tags) {
    database_helper h;
    const auto& ctx = h.context();

    CHECK_THROWS_AS(unpack_history_caller_context(ctx, "not json"), std::runtime_error);
}

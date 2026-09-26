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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <string>

namespace {

const std::string tags("[handler_helpers]");
const std::string test_suite("ores.service.tests");

struct payload {
    std::string name;
    int count = 0;
};

// A domain object of the shape stamp() writes to.
struct record {
    std::string tenant_id;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
};

struct party_scoped_record {
    std::string tenant_id;
    boost::uuids::uuid party_id{};
};

}

using namespace ores::service::messaging;

TEST_CASE("has_permission accepts an exact match and refuses an unrelated permission", tags) {
    ores::testing::database_helper h;
    const auto ctx = h.context().with_roles({"iam::accounts:create"});

    REQUIRE(has_permission(ctx, "iam::accounts:create"));
    REQUIRE_FALSE(has_permission(ctx, "iam::accounts:delete"));
    REQUIRE_FALSE(has_permission(ctx, "refdata::parties:create"));
}

TEST_CASE("has_permission accepts a component wildcard but not another component", tags) {
    ores::testing::database_helper h;
    const auto ctx = h.context().with_roles({"iam::*"});

    REQUIRE(has_permission(ctx, "iam::accounts:delete"));
    REQUIRE_FALSE(has_permission(ctx, "refdata::parties:delete"));
}

TEST_CASE("has_permission accepts the global wildcard", tags) {
    ores::testing::database_helper h;
    const auto ctx = h.context().with_roles({"*"});

    REQUIRE(has_permission(ctx, "refdata::parties:delete"));
}

// A token minted before RBAC carries no permission list. The handler treats
// that as pass-through, and the behaviour is pinned here so a change to it is
// a deliberate one.
TEST_CASE("has_permission passes a token that carries no permissions at all", tags) {
    ores::testing::database_helper h;
    const auto ctx = h.context().with_roles({});

    REQUIRE(has_permission(ctx, "iam::accounts:create"));
}

TEST_CASE("stamp writes the context tenant and the acting user onto the object", tags) {
    ores::testing::database_helper h;
    const auto ctx = h.context().with_tenant(h.tenant_id(), "alice");
    record r;

    stamp(r, ctx);

    REQUIRE(r.tenant_id == ctx.tenant_id().to_string());
    REQUIRE(r.modified_by == "alice");
    REQUIRE(r.performed_by == ctx.service_account());
    REQUIRE(r.change_reason_code == "system.new_record");
}

TEST_CASE("stamp keeps the client's change reason and applies the requested default", tags) {
    ores::testing::database_helper h;
    const auto ctx = h.context().with_tenant(h.tenant_id(), "alice");
    record supplied;
    supplied.change_reason_code = "user.trade_edit";

    stamp(supplied, ctx);
    REQUIRE(supplied.change_reason_code == "user.trade_edit");

    record empty;
    stamp(empty, ctx, change_reasons::update);
    REQUIRE(empty.change_reason_code == "system.update");
}

// stamp() matches fields by name, so a party_id field is always overwritten
// from the context. That is wrong for a type whose party_id is a
// client-supplied target rather than the row's own scope, which is why the
// generic stamp documents the exception instead of guessing.
TEST_CASE("stamp overwrites the party id from the context", tags) {
    ores::testing::database_helper h;
    const auto other_party = boost::uuids::string_generator()("22222222-2222-2222-2222-222222222222");
    const auto ctx =
        h.context().with_party(h.tenant_id(), other_party, {other_party}, "alice");
    party_scoped_record r;

    stamp(r, ctx);

    REQUIRE(r.party_id == other_party);
}

TEST_CASE("delegated_actor prefers the acting user and falls back to the service account", tags) {
    ores::testing::database_helper h;

    const auto with_user = h.context().with_tenant(h.tenant_id(), "alice");
    REQUIRE(delegated_actor(with_user) == "alice");

    const auto without_user = h.context().with_tenant(h.tenant_id(), "");
    REQUIRE(delegated_actor(without_user) == h.context().service_account());
}

TEST_CASE("log_handler_entry returns the correlation id the caller supplied", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    ores::nats::message msg;
    msg.subject = "iam.v1.accounts.list";
    msg.headers[std::string(ores::nats::headers::nats_correlation_id)] = "corr-1";

    REQUIRE(log_handler_entry(lg, msg) == "corr-1");
}

TEST_CASE("decode returns the object the wire codec encoded", tags) {
    ores::nats::message msg;
    msg.subject = "test.subject";
    const payload original{"alpha", 3};
    const auto bytes = ores::nats::default_wire_codec().encode(original);
    msg.data.assign(bytes.begin(), bytes.end());

    const auto decoded = decode<payload>(msg);

    REQUIRE(decoded.has_value());
    REQUIRE(decoded->name == "alpha");
    REQUIRE(decoded->count == 3);
}

TEST_CASE("decode returns nothing for a payload the codec cannot read", tags) {
    ores::nats::message msg;
    msg.subject = "test.subject";
    // 0xc1 is the byte msgpack reserves and never assigns.
    msg.data = {std::byte{0xc1}};

    REQUIRE_FALSE(decode<payload>(msg).has_value());
}

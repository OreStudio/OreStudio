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
#include "ores.refdata.api/generators/counterparty_generator.hpp"
#include "ores.refdata.core/repository/counterparty_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/generators/trade_activity_generator.hpp"
#include "ores.trading.api/generators/trade_additional_field_generator.hpp"
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.trading.api/generators/trade_identifier_generator.hpp"
#include "ores.trading.api/generators/trade_party_role_generator.hpp"
#include "ores.trading.api/messaging/trade_identifier_protocol.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.trading.core/repository/trade_activity_repository.hpp"
#include "ores.trading.core/repository/trade_additional_field_repository.hpp"
#include "ores.trading.core/repository/trade_identifier_repository.hpp"
#include "ores.trading.core/repository/trade_party_role_repository.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "trade_parent_seed.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

/*
 * The trade model declares the counterparty and the trade type filterable, the
 * trade type searchable and sortable. Each case writes trades against
 * counterparties no other test uses, so a filter on it answers only them.
 */

namespace {

using ores::trading::messaging::trades_filter;
using ores::trading::repository::trade_repository;
using uuid = boost::uuids::uuid;

struct fixture {
    ores::testing::database_helper h;
    ores::database::context ctx = ores::trading::tests::write_parent_party(h);
    uuid counterparty = write_counterparty();
    uuid other_counterparty = write_counterparty();

    uuid write_counterparty() {
        auto gen = ores::testing::make_generation_context(h);
        auto c = ores::refdata::generators::generate_synthetic_counterparty(gen);
        c.change_reason_code = "system.test";
        ores::refdata::repository::counterparty_repository().write(ctx, c);
        return c.id;
    }

    uuid write(const std::string& type, const uuid& counterparty_id) {
        auto gen = ores::testing::make_generation_context(h);
        auto t = ores::trading::generators::generate_synthetic_trade(gen);
        t.party_id = *ctx.party_id();
        t.counterparty_id = counterparty_id;
        t.counterparty_scope = ores::trading::domain::counterparty_scope::external;
        t.trade_type = type;
        trade_repository().write(ctx, t);
        return t.id;
    }

    uuid write_scoped(const ores::trading::domain::counterparty_scope scope) {
        auto gen = ores::testing::make_generation_context(h);
        auto t = ores::trading::generators::generate_synthetic_trade(gen);
        t.party_id = *ctx.party_id();
        if (scope != ores::trading::domain::counterparty_scope::intra_entity)
            t.counterparty_id = counterparty;
        else
            t.counterparty_id = std::nullopt;
        t.counterparty_scope = scope;
        trade_repository().write(ctx, t);
        return t.id;
    }

    uuid write_activity() {
        auto gen = ores::testing::make_generation_context(h);
        auto activity = ores::trading::generators::generate_synthetic_trade_activity(gen);
        activity.party_id = *ctx.party_id();
        ores::trading::repository::trade_activity_repository().write(ctx, activity);
        return activity.id;
    }

    void write_identifier(const uuid& trade_id, const std::string& type, const std::string& value) {
        auto gen = ores::testing::make_generation_context(h);
        auto id = ores::trading::generators::generate_synthetic_trade_identifier(gen);
        id.trade_id = trade_id;
        id.trade_activity_id = write_activity();
        id.id_type = type;
        id.id_value = value;
        ores::trading::repository::trade_identifier_repository().write(ctx, id);
    }

    void write_additional_field(const uuid& trade_id, int sequence, const std::string& name) {
        auto gen = ores::testing::make_generation_context(h);
        auto field = ores::trading::generators::generate_synthetic_trade_additional_field(gen);
        field.trade_id = trade_id;
        field.trade_activity_id = write_activity();
        field.sequence_number = sequence;
        field.name = name;
        ores::trading::repository::trade_additional_field_repository().write(ctx, field);
    }

    void write_party_role(const uuid& trade_id) {
        auto gen = ores::testing::make_generation_context(h);
        auto role = ores::trading::generators::generate_synthetic_trade_party_role(gen);
        role.trade_id = trade_id;
        role.trade_activity_id = write_activity();
        ores::trading::repository::trade_party_role_repository().write(ctx, role);
    }

    uuid write_intra_entity(const std::string& type) {
        auto gen = ores::testing::make_generation_context(h);
        auto t = ores::trading::generators::generate_synthetic_trade(gen);
        t.party_id = *ctx.party_id();
        t.counterparty_id = std::nullopt;
        t.counterparty_scope = ores::trading::domain::counterparty_scope::intra_entity;
        t.trade_type = type;
        trade_repository().write(ctx, t);
        return t.id;
    }

    std::vector<ores::trading::domain::trade> read(const trades_filter& f,
                                                   const ores::utility::domain::order& order = {}) {
        return trade_repository().read_latest(ctx, 0, 1000, order, f);
    }
};

trades_filter on(const uuid& counterparty) {
    return {.counterparty_id = std::optional<uuid>{counterparty}};
}

std::vector<uuid> ids(const std::vector<ores::trading::domain::trade>& trades) {
    std::vector<uuid> r;
    for (const auto& t : trades)
        r.push_back(t.id);
    std::ranges::sort(r);
    return r;
}

std::vector<uuid> sorted(std::vector<uuid> v) {
    std::ranges::sort(v);
    return v;
}

}

TEST_CASE("a counterparty narrows the trade list, and the total follows",
          "[repository][list_filter]") {
    fixture f;
    const auto a = f.write("Swap", f.counterparty);
    const auto b = f.write("FxForward", f.counterparty);
    f.write("Swap", f.other_counterparty);

    CHECK(ids(f.read(on(f.counterparty))) == sorted({a, b}));
    CHECK(trade_repository().get_total_trade_count(f.ctx, on(f.counterparty)) == 2);
}

TEST_CASE("an empty counterparty member reads the trades that have no counterparty",
          "[repository][list_filter]") {
    fixture f;
    const auto internal = f.write_intra_entity("Swap");
    f.write("Swap", f.counterparty);

    trades_filter filter;
    filter.counterparty_id = std::optional<uuid>{};
    CHECK(ids(f.read(filter)) == std::vector<uuid>{internal});
}

TEST_CASE("a trade type narrows the list, and the members that are set must all hold",
          "[repository][list_filter]") {
    fixture f;
    const auto a = f.write("Swap", f.counterparty);
    f.write("FxForward", f.counterparty);
    f.write("Swap", f.other_counterparty);

    auto filter = on(f.counterparty);
    filter.trade_type = "Swap";
    CHECK(ids(f.read(filter)) == std::vector<uuid>{a});
}

TEST_CASE("a one-of member reads the trades of the types it lists", "[repository][list_filter]") {
    fixture f;
    const auto a = f.write("Swap", f.counterparty);
    const auto b = f.write("FxForward", f.counterparty);
    f.write("CapFloor", f.counterparty);

    auto filter = on(f.counterparty);
    filter.trade_type_one_of = std::vector<std::string>{"Swap", "FxForward"};
    CHECK(ids(f.read(filter)) == sorted({a, b}));

    filter.trade_type_one_of = std::vector<std::string>{};
    CHECK(f.read(filter).empty());
}

TEST_CASE("a counterparty one-of member reads the trades of the counterparties it lists",
          "[repository][list_filter]") {
    fixture f;
    const auto a = f.write("Swap", f.counterparty);
    const auto b = f.write("Swap", f.other_counterparty);

    trades_filter filter;
    filter.counterparty_id_one_of = std::vector<uuid>{f.counterparty, f.other_counterparty};
    CHECK(ids(f.read(filter)) == sorted({a, b}));
}

TEST_CASE("a search matches the trade type, ignoring case, and literally",
          "[repository][list_filter]") {
    fixture f;
    f.write("Swap", f.counterparty);
    const auto b = f.write("FxForward", f.counterparty);

    auto filter = on(f.counterparty);
    filter.search = "FXFOR";
    CHECK(ids(f.read(filter)) == std::vector<uuid>{b});

    filter.search = "%";
    CHECK(f.read(filter).empty());

    filter.search = "";
    CHECK(f.read(filter).size() == 2);
}

TEST_CASE("the trade type is sortable and nothing else is", "[repository][list_filter]") {
    fixture f;
    const auto swap = f.write("Swap", f.counterparty);
    const auto forward = f.write("FxForward", f.counterparty);

    const auto up = f.read(on(f.counterparty), {.field = "trade_type"});
    REQUIRE(up.size() == 2);
    CHECK(up[0].id == forward);
    CHECK(up[1].id == swap);

    const auto down = f.read(on(f.counterparty), {.field = "trade_type", .descending = true});
    REQUIRE(down.size() == 2);
    CHECK(down[0].id == swap);

    CHECK_THROWS_AS(f.read(on(f.counterparty), {.field = "counterparty_id"}),
                    std::invalid_argument);
}

TEST_CASE("an enum member narrows the list by the enum's text", "[repository][list_filter]") {
    using ores::trading::domain::counterparty_scope;
    fixture f;
    const auto external = f.write_scoped(counterparty_scope::external);
    const auto inter = f.write_scoped(counterparty_scope::inter_entity);
    f.write_scoped(counterparty_scope::intra_entity);

    trades_filter filter;
    filter.counterparty_scope = counterparty_scope::external;
    CHECK(ids(f.read(filter)) == std::vector<uuid>{external});

    filter.counterparty_scope.reset();
    filter.counterparty_scope_one_of =
        std::vector{counterparty_scope::external, counterparty_scope::inter_entity};
    CHECK(ids(f.read(filter)) == sorted({external, inter}));
}

TEST_CASE("a child table reads the rows of one trade by its trade id",
          "[repository][list_filter]") {
    fixture f;
    const auto a = f.write("Swap", f.counterparty);
    const auto b = f.write("Swap", f.counterparty);
    f.write_identifier(a, "UTI", "ID-A-1");
    f.write_identifier(a, "USI", "ID-A-2");
    f.write_identifier(b, "UTI", "ID-B-1");

    ores::trading::repository::trade_identifier_repository repo;
    const auto of_a = repo.read_latest_by_trade_id(f.ctx, boost::uuids::to_string(a), 0, 100);
    CHECK(of_a.size() == 2);
    CHECK(repo.get_total_identifier_count_by_trade_id(f.ctx, boost::uuids::to_string(a)) == 2);
    for (const auto& i : of_a) {
        CHECK(i.trade_id == a);
        CHECK(i.id_value != "ID-B-1");
    }

    ores::trading::messaging::trade_identifiers_filter other;
    other.trade_id_one_of = std::vector<uuid>{b};
    const auto key = boost::uuids::to_string(a);
    CHECK(repo.read_latest_by_trade_id(f.ctx, key, 0, 100, {}, other).empty());
    CHECK(repo.get_total_identifier_count_by_trade_id(f.ctx, key, other) == 0);

    other.trade_id_one_of = std::vector<uuid>{a, b};
    CHECK(repo.read_latest_by_trade_id(f.ctx, key, 0, 100, {}, other).size() == 2);
}

TEST_CASE("every child of a trade reads by its trade id", "[repository][list_filter]") {
    fixture f;
    const auto a = f.write("Swap", f.counterparty);
    const auto b = f.write("Swap", f.counterparty);
    f.write_additional_field(a, 1, "First");
    f.write_additional_field(a, 2, "Second");
    f.write_additional_field(b, 1, "Other");
    f.write_party_role(a);
    f.write_party_role(b);

    const auto key = boost::uuids::to_string(a);
    ores::trading::repository::trade_additional_field_repository fields;
    CHECK(fields.read_latest_by_trade_id(f.ctx, key, 0, 100).size() == 2);
    CHECK(fields.get_total_trade_additional_field_count_by_trade_id(f.ctx, key) == 2);

    ores::trading::repository::trade_party_role_repository roles;
    CHECK(roles.read_latest_by_trade_id(f.ctx, key, 0, 100).size() == 1);
    CHECK(roles.get_total_role_count_by_trade_id(f.ctx, key) == 1);
}

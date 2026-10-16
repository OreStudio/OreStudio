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
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.trading.api/domain/bond_instrument.hpp"
#include "ores.trading.api/domain/bond_issue.hpp"
#include "ores.trading.api/domain/bond_leg.hpp"
#include "ores.trading.api/generators/bond_instrument_generator.hpp"
#include "ores.trading.api/generators/bond_leg_generator.hpp"
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.trading.core/repository/bond_instrument_entity.hpp"
#include "ores.trading.core/repository/bond_instrument_repository.hpp"
#include "ores.trading.core/repository/bond_issue_repository.hpp"
#include "ores.trading.core/repository/bond_leg_entity.hpp"
#include "ores.trading.core/repository/bond_leg_repository.hpp"
#include "ores.trading.core/repository/trade_entity.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include "trade_parent_seed.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace {

const std::string_view test_suite("ores.trading.core.repository.bond_composite_versioning.tests");
const std::string tags("[repository][bond][versioning]");

using ores::database::context;
using ores::testing::database_helper;
using ores::trading::domain::bond_instrument;
using ores::trading::domain::bond_issue;
using ores::trading::domain::bond_leg;
using ores::trading::repository::bond_instrument_repository;
using ores::trading::repository::bond_issue_repository;
using ores::trading::repository::bond_leg_entity;
using ores::trading::repository::bond_leg_repository;
using ores::trading::repository::trade_entity;
using namespace ores::logging;

/*
 * The whole version history of one trade, oldest first, read from the table
 * rather than from the repository. The window bounds the as-of read takes are
 * the very values the store wrote, so a test that feeds them back exercises the
 * query the way a reader of a historical version does.
 */
std::vector<trade_entity> read_trade_versions(context ctx, const std::string& trade_id) {
    using namespace ores::database::repository;
    using namespace sqlgen::literals;

    auto lg = make_logger(test_suite);
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_entity>> |
                       sqlgen::where("tenant_id"_c == tid && "id"_c == trade_id) |
                       sqlgen::order_by("version"_c);

    return execute_read_query<trade_entity, trade_entity>(
        ctx,
        query,
        [](const auto& entities) { return entities; },
        lg,
        "Reading raw trade version rows");
}

/* The leg rows themselves, so a leg counted as "current" is a row that exists. */
std::vector<bond_leg_entity> read_leg_versions(context ctx, const std::string& trade_id) {
    using namespace ores::database::repository;
    using namespace sqlgen::literals;

    auto lg = make_logger(test_suite);
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_leg_entity>> |
                       sqlgen::where("tenant_id"_c == tid && "trade_id"_c == trade_id) |
                       sqlgen::order_by("valid_from"_c);

    return execute_read_query<bond_leg_entity, bond_leg_entity>(
        ctx, query, [](const auto& entities) { return entities; }, lg, "Reading raw bond leg rows");
}

/*
 * Writes the bond's terms row. An instrument names the issue that holds them,
 * so the issue has to exist first.
 *
 * @return The issue id the instrument must name.
 */
boost::uuids::uuid write_issue(context ctx, database_helper& h) {
    bond_issue v;
    v.issue_id = boost::uuids::random_generator()();
    v.tenant_id = h.tenant_id();
    v.security_id = "XS0000000001";
    v.issuer = "ACME";
    v.face_value = ores::utility::decimal::decimal::from_string("1000").value();
    v.issue_date = ores::platform::time::datetime::from_iso8601_date("2024-01-15");
    v.settlement_days = 2;
    v.modified_by = h.db_user();
    v.performed_by = "ores";
    v.change_reason_code = "system.test";
    v.change_commentary = "Composite versioning: issue";

    bond_issue_repository().write(ctx, v);
    return v.issue_id;
}

/*
 * Writes the bond's header row. The instrument is keyed by the trade it belongs
 * to, so its own insert is the first write against that trade's composite.
 */
void write_instrument(context ctx,
                      database_helper& h,
                      const boost::uuids::uuid& trade_id,
                      const boost::uuids::uuid& activity_id,
                      const boost::uuids::uuid& issue_id) {
    auto gen = ores::testing::make_generation_context(h);

    auto v = ores::trading::generators::generate_synthetic_bond_instrument(gen);
    v.identity.trade_id = trade_id;
    v.identity.trade_activity_id = activity_id;
    v.identity.tenant_id = h.tenant_id();
    v.issue_id = issue_id;
    v.audit.modified_by = h.db_user();
    v.audit.performed_by = "ores";
    v.audit.change_reason_code = "system.test";
    v.audit.change_commentary = "Composite versioning: header";

    bond_instrument_repository().write(ctx, v);
}

/* Writes one leg. A leg write is the rewrite the composite has to notice. */
void write_leg(context ctx,
               database_helper& h,
               const boost::uuids::uuid& trade_id,
               const boost::uuids::uuid& activity_id,
               const std::string& leg_role) {
    auto gen = ores::testing::make_generation_context(h);

    auto v = ores::trading::generators::generate_synthetic_bond_leg(gen);
    v.trade_id = trade_id;
    v.trade_activity_id = activity_id;
    v.leg_role = leg_role;
    v.leg_number = 1;
    v.tenant_id = h.tenant_id();
    v.modified_by = h.db_user();
    v.performed_by = "ores";
    v.change_reason_code = "system.test";
    v.change_commentary = "Composite versioning: leg rewritten";

    bond_leg_repository().write(ctx, v);
}

}

/*
 * A bond leg is a part of the trade, not an entity of its own. Rewriting one
 * therefore advances the trade's version in the same transaction, which is what
 * makes the trade's windows say when the legs were stable.
 *
 * The two writes before it are parts as well — the bond instrument and the
 * activity that wrote it — so the trade is already several versions old when
 * the leg lands. The test measures the leg's own contribution rather than the
 * absolute count, which the other parts would otherwise decide.
 */
TEST_CASE("rewriting_a_bond_leg_advances_the_parent_trade_version", tags) {
    database_helper h;

    /*
     * The party is written first and its context kept, so the trade anchor, the
     * bond rows and every read below live under the one party this test can
     * see: a trade is party-isolated, and a context that named any other party
     * would read back nothing.
     */
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = ores::trading::tests::write_parent_party(h);
    const auto party_id = *ctx.party_id();

    auto trade = ores::trading::generators::generate_synthetic_trade(gen);
    trade.party_id = party_id;
    ores::trading::repository::trade_repository().write(ctx, trade);
    const auto trade_id = trade.id;
    const auto id_str = boost::uuids::to_string(trade_id);
    const auto activity_id = ores::trading::tests::write_parent_activity(h);

    const auto issue_id = write_issue(ctx, h);
    write_instrument(ctx, h, trade_id, activity_id, issue_id);
    const auto before_leg = read_trade_versions(ctx, id_str);

    write_leg(ctx, h, trade_id, activity_id, "trs_funding");
    const auto after_leg = read_trade_versions(ctx, id_str);

    REQUIRE(after_leg.size() == before_leg.size() + 1);
    CHECK(after_leg.back().version == before_leg.back().version + 1);
}

/*
 * The trade's windows are the source of truth for "as of", and its legs are
 * what a window is asked about. The same trade, asked at two points on its own
 * timeline, answers differently: the window the leg was written into carries
 * it, and the window that preceded that write does not.
 *
 * Each window is widened by a second on each side. The read takes a
 * std::chrono::time_point while the store keeps microseconds, and the versions
 * a bond write produces land microseconds apart inside one transaction, so a
 * bound taken straight from one version row can fall inside its neighbour.
 */
TEST_CASE("as_of_read_reports_the_legs_current_at_each_parent_version", tags) {
    database_helper h;

    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = ores::trading::tests::write_parent_party(h);
    const auto party_id = *ctx.party_id();

    auto trade = ores::trading::generators::generate_synthetic_trade(gen);
    trade.party_id = party_id;
    ores::trading::repository::trade_repository().write(ctx, trade);
    const auto trade_id = trade.id;
    const auto id_str = boost::uuids::to_string(trade_id);
    const auto activity_id = ores::trading::tests::write_parent_activity(h);

    const auto issue_id = write_issue(ctx, h);
    write_instrument(ctx, h, trade_id, activity_id, issue_id);
    const auto before_leg = read_trade_versions(ctx, id_str);

    write_leg(ctx, h, trade_id, activity_id, "trs_funding");
    const auto after_leg = read_trade_versions(ctx, id_str);
    REQUIRE(after_leg.size() == before_leg.size() + 1);

    using ores::database::repository::timestamp_to_timepoint;
    const auto second = std::chrono::seconds(1);
    const auto opens_at = [](const auto& v) {
        return timestamp_to_timepoint(v.valid_from);
    };

    bond_leg_repository leg_repo;

    /*
     * A window that closed at the trade's booking contains no leg: the leg was
     * written microseconds later, and its own window does not overlap this one.
     */
    const auto booking = opens_at(before_leg.front());
    const auto earlier = leg_repo.read_by_trade_id_as_of(ctx, id_str, booking, booking);
    CHECK(earlier.empty());

    /* The leg was written inside the trade's current window, so it is current. */
    const auto current = leg_repo.read_by_trade_id_as_of(
        ctx, id_str, opens_at(after_leg.back()) - second, opens_at(after_leg.back()) + second);
    REQUIRE(current.size() == 1);
    CHECK(current[0].leg_role == "trs_funding");
    CHECK(current[0].version == 1);

    /* The leg is a row in the store, not a version counter on the trade. */
    const auto rows = read_leg_versions(ctx, id_str);
    REQUIRE(rows.size() == 1);
    CHECK(rows[0].leg_role.value() == "trs_funding");
}

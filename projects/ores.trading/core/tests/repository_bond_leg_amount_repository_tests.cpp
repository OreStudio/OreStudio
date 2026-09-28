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
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.trading.api/domain/bond_leg_amount_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/bond_leg_amount_entity.hpp"
#include "ores.trading.core/repository/bond_leg_amount_repository.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "trade_parent_seed.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace {

const std::string_view test_suite("ores.trading.core.repository.bond_leg_amount.tests");
const std::string tags("[repository][bond_leg_amount][decimal]");

using ores::testing::database_helper;
using ores::trading::domain::bond_leg_amount;
using ores::trading::repository::bond_leg_amount_entity;
using ores::trading::repository::bond_leg_amount_repository;
using ores::utility::decimal::decimal;
using namespace ores::logging;

/*
 * A money column is a decimal, and the entity member is the exact decimal
 * string the numeric column stores. Each row states a value that a binary
 * float cannot hold exactly, the text the domain reads back, and the text
 * numeric(28, 10) stores.
 */
struct probe {
    const char* text;
    const char* canonical;
    const char* stored;
};

const probe probes[] = {
    {"0.1", "0.1", "0.1000000000"},
    {"1e-10", "0.0000000001", "0.0000000001"},
    {"0.0425", "0.0425", "0.0425000000"},
    {"12345678.1234567890", "12345678.123456789", "12345678.1234567890"},
    {"999999999999999999.9999999999",
     "999999999999999999.9999999999",
     "999999999999999999.9999999999"},
};

constexpr int probe_count = sizeof(probes) / sizeof(probes[0]);

bond_leg_amount make_amount(database_helper& h,
                            const boost::uuids::uuid& trade_id,
                            int sequence_number,
                            const char* text) {
    bond_leg_amount r;
    r.trade_id = trade_id;
    r.leg_role = "bond";
    r.leg_number = 1;
    r.amount_role = "notional";
    r.sequence_number = sequence_number;
    r.tenant_id = h.tenant_id();
    r.value = decimal::from_string(text).value();
    r.modified_by = h.db_user();
    r.performed_by = "ores";
    r.change_reason_code = "system.external_data_import";
    r.change_commentary = "Decimal binding proof";
    return r;
}

/*
 * Reads the rows as the repository's own entity, so the member checked is the
 * std::string sqlgen binds to the numeric column and not the decimal the
 * mapper renders from it. This is what proves the seam.
 */
std::vector<bond_leg_amount_entity> read_entities(ores::database::context ctx,
                                                  const std::string& trade_id) {
    using namespace ores::database::repository;
    using namespace sqlgen::literals;

    auto lg = make_logger(test_suite);
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<bond_leg_amount_entity>> |
                       sqlgen::where("tenant_id"_c == tid && "trade_id"_c == trade_id &&
                                     "valid_to"_c == max.value()) |
                       sqlgen::order_by("sequence_number"_c);

    return execute_read_query<bond_leg_amount_entity, bond_leg_amount_entity>(
        ctx,
        query,
        [](const auto& entities) { return entities; },
        lg,
        "Reading raw bond leg amount rows");
}

}

TEST_CASE("bond_leg_amount_decimal_round_trips_exactly", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party_id = boost::uuids::random_generator()();
    auto ctx = h.context().with_party(h.tenant_id(), party_id, {party_id}, h.db_user());

    const auto trade_id = ores::trading::tests::write_parent_trade(h);
    const auto id_str = boost::uuids::to_string(trade_id);

    bond_leg_amount_repository repo;
    for (int i = 0; i < probe_count; ++i) {
        BOOST_LOG_SEV(lg, debug) << "Writing amount: " << probes[i].text;
        CHECK_NOTHROW(repo.write(ctx, make_amount(h, trade_id, i + 1, probes[i].text)));
    }

    for (int i = 0; i < probe_count; ++i) {
        const auto read = repo.read_latest(
            ctx, id_str, "bond", "1", "notional", std::to_string(i + 1));
        REQUIRE(read.size() == 1);
        BOOST_LOG_SEV(lg, debug) << "Read back: " << read[0].value.to_string();
        CHECK(read[0].value.to_string() == probes[i].canonical);
    }

    const auto entities = read_entities(ctx, id_str);
    REQUIRE(entities.size() == static_cast<std::size_t>(probe_count));
    for (int i = 0; i < probe_count; ++i) {
        BOOST_LOG_SEV(lg, info) << "Stored text: " << entities[i].value;
        CHECK(entities[i].value == probes[i].stored);
    }

    for (int i = 0; i < probe_count; ++i)
        repo.remove(ctx, id_str, "bond", "1", "notional", std::to_string(i + 1));
}

TEST_CASE("bond_leg_amount_decimal_keeps_a_value_a_double_would_round", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party_id = boost::uuids::random_generator()();
    auto ctx = h.context().with_party(h.tenant_id(), party_id, {party_id}, h.db_user());

    const auto trade_id = ores::trading::tests::write_parent_trade(h);
    const auto id_str = boost::uuids::to_string(trade_id);

    const std::string exact = "999999999999999999.9999999999";
    bond_leg_amount_repository repo;
    REQUIRE_NOTHROW(repo.write(ctx, make_amount(h, trade_id, 1, exact.c_str())));

    const auto read = repo.read_latest(ctx, id_str, "bond", "1", "notional", "1");
    REQUIRE(read.size() == 1);
    CHECK(read[0].value.to_string() == exact);

    const double rounded = 999999999999999999.9999999999;
    CHECK(decimal::from_double(rounded).value().to_string() != exact);

    repo.remove(ctx, id_str, "bond", "1", "notional", "1");
}

TEST_CASE("bond_leg_amount_refuses_a_value_wider_than_the_column", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party_id = boost::uuids::random_generator()();
    auto ctx = h.context().with_party(h.tenant_id(), party_id, {party_id}, h.db_user());

    const auto trade_id = ores::trading::tests::write_parent_trade(h);
    const auto id_str = boost::uuids::to_string(trade_id);

    /*
     * The value the type holds exactly, and the column cannot: 20 integer
     * digits against numeric(28, 10)'s 18. The store refuses it, and this
     * test states the refusal rather than hiding the value behind a cast.
     */
    const std::string too_wide = "12345678901234567890.1234567890";
    CHECK(decimal::from_string(too_wide).value().to_string() ==
          "12345678901234567890.123456789");

    bond_leg_amount_repository repo;
    bool refused = false;
    std::string message;
    try {
        repo.write(ctx, make_amount(h, trade_id, 1, too_wide.c_str()));
    } catch (const std::exception& e) {
        refused = true;
        message = e.what();
    }

    BOOST_LOG_SEV(lg, info) << "Refused: " << message;
    CHECK(refused);
    CHECK(message.find("numeric") != std::string::npos);
}

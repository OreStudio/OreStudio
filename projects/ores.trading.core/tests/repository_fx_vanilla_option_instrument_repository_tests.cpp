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
#include "ores.trading.core/repository/fx_vanilla_option_instrument_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/fx_vanilla_option_instrument_json_io.hpp" // IWYU pragma: keep.
#include "ores.testing/database_helper.hpp"

namespace {

const std::string_view test_suite("ores.trading.core.repository.fx_vanilla_option.tests");
const std::string tags("[repository][fx][fx_vanilla_option]");

using ores::testing::database_helper;
using ores::trading::repository::fx_vanilla_option_instrument_repository;
using ores::trading::domain::fx_vanilla_option_instrument;
using namespace ores::logging;

/*
 * Test data derived from:
 *   external/ore/examples/Products/Example_Trades/FX_Option_European.xml
 * via fx_instrument_mapper::forward_fx_option.
 * See tests/test_data/fx_vanilla_option_instrument.json for the full mapping snapshot.
 */
fx_vanilla_option_instrument make_instrument(database_helper& h) {
    fx_vanilla_option_instrument r;
    r.instrument_id      = boost::uuids::random_generator()();
    r.tenant_id          = h.tenant_id();
    r.trade_type_code    = "FxOption";
    r.bought_currency    = "EUR";
    r.bought_amount      = 1000000.0;
    r.sold_currency      = "USD";
    r.sold_amount        = 1100000.0;
    r.option_type        = "Call";
    r.expiry_date        = "2033-02-20";
    r.exercise_style     = "European";
    r.settlement         = "Cash";
    r.modified_by        = h.db_user();
    r.performed_by       = "ores";
    r.change_reason_code = "system.external_data_import";
    r.change_commentary  = "Imported from ORE XML";
    return r;
}

} // namespace

TEST_CASE("fx_vanilla_option_instrument_write_and_read_latest", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party_id = boost::uuids::random_generator()();
    auto ctx = h.context().with_party(h.tenant_id(), party_id, {party_id}, h.db_user());

    auto instr = make_instrument(h);
    const auto id_str = boost::uuids::to_string(instr.instrument_id);
    BOOST_LOG_SEV(lg, debug) << "Writing FX vanilla option instrument: " << instr;

    fx_vanilla_option_instrument_repository repo;
    CHECK_NOTHROW(repo.write(ctx, instr));

    const auto read = repo.read_latest(ctx, id_str);
    REQUIRE(read.size() == 1);
    CHECK(read[0].trade_type_code == "FxOption");
    CHECK(read[0].bought_currency == "EUR");
    CHECK(read[0].bought_amount == 1000000.0);
    CHECK(read[0].sold_currency == "USD");
    CHECK(read[0].sold_amount == 1100000.0);
    CHECK(read[0].option_type == "Call");
    CHECK(read[0].expiry_date == "2033-02-20");
    CHECK(read[0].exercise_style == "European");
    CHECK(read[0].settlement == "Cash");
    BOOST_LOG_SEV(lg, debug) << "Read FX vanilla option instrument: " << read[0];
}

TEST_CASE("fx_vanilla_option_instrument_read_nonexistent", tags) {
    database_helper h;
    const auto party_id = boost::uuids::random_generator()();
    auto ctx = h.context().with_party(h.tenant_id(), party_id, {party_id}, h.db_user());

    fx_vanilla_option_instrument_repository repo;
    const std::string nonexistent = "00000000-0000-0000-0000-000000000001";
    const auto read = repo.read_latest(ctx, nonexistent);
    CHECK(read.empty());
}

TEST_CASE("fx_vanilla_option_instrument_remove", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party_id = boost::uuids::random_generator()();
    auto ctx = h.context().with_party(h.tenant_id(), party_id, {party_id}, h.db_user());

    auto instr = make_instrument(h);
    const auto id_str = boost::uuids::to_string(instr.instrument_id);

    fx_vanilla_option_instrument_repository repo;
    repo.write(ctx, instr);

    const auto before = repo.read_latest(ctx, id_str);
    REQUIRE(!before.empty());

    CHECK_NOTHROW(repo.remove(ctx, id_str));

    const auto after = repo.read_latest(ctx, id_str);
    BOOST_LOG_SEV(lg, debug) << "After remove count: " << after.size();
    CHECK(after.empty());
}

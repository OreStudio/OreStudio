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
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.trading.api/generators/trade_activity_generator.hpp"
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.trading.core/repository/trade_activity_repository.hpp"
#include "ores.trading.core/repository/trade_product_repository.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

using namespace ores::trading;

namespace {

/// A context that writes as a new party of the test tenant, as booking does.
ores::database::context
trade_product_party_context(ores::testing::scoped_database_helper& h,
                            ores::utility::generation::generation_context& gen) {
    ores::refdata::repository::party_repository parties;
    auto party = ores::refdata::generators::generate_synthetic_party(gen);
    party.change_reason_code = "system.test";
    for (const auto& e : parties.read_latest(h.context()))
        if (e.tenant_id == party.tenant_id) {
            party.parent_party_id = e.id;
            break;
        }
    parties.write(h.context(), party);
    return h.context().with_party(h.tenant_id(), party.id, {party.id}, h.db_user());
}

/// A trade product for a trade booked now, as a party of the test tenant.
domain::trade_product trade_product_booked(ores::testing::scoped_database_helper& h,
                                           const ores::database::context& party_ctx,
                                           ores::utility::generation::generation_context& gen,
                                           const std::string& product,
                                           const std::string& form) {
    // The audit author is stated here rather than taken from the generator's
    // default, so the row is refused for what the test is about and not for a
    // missing field the caller cannot see.
    const auto booked_by = h.db_user();

    auto trade = generators::generate_synthetic_trade(gen);
    trade.party_id = *party_ctx.party_id();
    trade.modified_by = booked_by;
    trade.performed_by = booked_by;
    repository::trade_repository().write(party_ctx, trade);

    auto activity = generators::generate_synthetic_trade_activity(gen);
    activity.party_id = *party_ctx.party_id();
    activity.modified_by = booked_by;
    activity.performed_by = booked_by;
    repository::trade_activity_repository().write(party_ctx, activity);

    domain::trade_product row;
    row.trade_id = trade.id;
    row.trade_activity_id = activity.id;
    row.product_code = product;
    row.booking_form = form;
    row.change_reason_code = "system.test";
    row.modified_by = booked_by;
    return row;
}

TEST_CASE("a trade's product and form are written with it and read back by the trade",
          "[trade_product][database]") {
    ores::testing::scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto party_ctx = trade_product_party_context(h, gen);

    const auto row = trade_product_booked(h, party_ctx, gen, "FxBasketOption", "scripted");
    repository::trade_product_repository().write(party_ctx, row);

    const auto read = repository::trade_product_repository().read_latest(
        party_ctx, boost::uuids::to_string(row.trade_id));
    REQUIRE(read.size() == 1u);
    CHECK(read[0].product_code == "FxBasketOption");
    CHECK(read[0].booking_form == "scripted");
}

TEST_CASE("a form other than native or scripted is refused", "[trade_product][database]") {
    ores::testing::scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto party_ctx = trade_product_party_context(h, gen);

    const auto row = trade_product_booked(h, party_ctx, gen, "Swap", "spreadsheet");

    CHECK_THROWS(repository::trade_product_repository().write(party_ctx, row));
}

TEST_CASE("a trade product with no product is refused", "[trade_product][database]") {
    ores::testing::scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto party_ctx = trade_product_party_context(h, gen);

    const auto row = trade_product_booked(h, party_ctx, gen, "", "native");

    CHECK_THROWS(repository::trade_product_repository().write(party_ctx, row));
}

TEST_CASE("another party does not see a trade's product", "[trade_product][database]") {
    ores::testing::scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto owner_ctx = trade_product_party_context(h, gen);
    const auto other_ctx = trade_product_party_context(h, gen);

    const auto row = trade_product_booked(h, owner_ctx, gen, "Swap", "native");
    repository::trade_product_repository().write(owner_ctx, row);

    const auto id = boost::uuids::to_string(row.trade_id);
    CHECK(repository::trade_product_repository().read_latest(owner_ctx, id).size() == 1u);
    CHECK(repository::trade_product_repository().read_latest(other_ctx, id).empty());
}

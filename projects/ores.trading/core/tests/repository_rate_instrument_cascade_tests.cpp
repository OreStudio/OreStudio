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
#include "ores.logging/make_logger.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.trading.api/domain/fra_instrument_json_io.hpp"  // IWYU pragma: keep.
#include "ores.trading.api/domain/rate_instrument_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/domain/swap_leg_json_io.hpp"        // IWYU pragma: keep.
#include "ores.trading.api/generators/fra_instrument_generator.hpp"
#include "ores.trading.api/generators/rate_instrument_generator.hpp"
#include "ores.trading.api/generators/swap_leg_generator.hpp"
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.trading.core/repository/fra_instrument_repository.hpp"
#include "ores.trading.core/repository/rate_instrument_repository.hpp"
#include "ores.trading.core/repository/swap_leg_repository.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "trade_parent_seed.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <string>

namespace {

const std::string_view test_suite("ores.trading.core.repository.rate_instrument.cascade.tests");
const std::string tags("[repository][rate_instrument]");

using ores::testing::database_helper;
using ores::trading::domain::fra_instrument;
using ores::trading::domain::rate_instrument;
using ores::trading::domain::swap_leg;
using ores::trading::repository::fra_instrument_repository;
using ores::trading::repository::rate_instrument_repository;
using ores::trading::repository::swap_leg_repository;
using ores::trading::repository::trade_repository;
using ores::utility::decimal::decimal;
using namespace ores::logging;

/*
 * The family header. The trade, the activity and the party are the test's own
 * anchors; the rest is stated as a literal so the read back can be checked
 * against exactly what was written.
 */
rate_instrument make_header(database_helper& h,
                            const boost::uuids::uuid& trade_id,
                            const boost::uuids::uuid& activity_id,
                            const boost::uuids::uuid& party_id) {
    auto gen = ores::testing::make_generation_context(h);
    auto r = ores::trading::generators::generate_synthetic_rate_instrument(gen);
    r.identity.trade_id = trade_id;
    r.identity.trade_activity_id = activity_id;
    r.identity.party_id = party_id;
    r.identity.trade_type_code = "ForwardRateAgreement";
    r.start_date = ores::platform::time::datetime::from_iso8601_date("2025-01-15");
    r.maturity_date = ores::platform::time::datetime::from_iso8601_date("2025-07-15");
    r.description = "FRA family cascade delete proof";
    return r;
}

/*
 * The FRA fact row: flat, keyed by the trade it joins the header through, and
 * carrying none of the header's identity columns.
 */
fra_instrument make_fact(database_helper& h,
                         const boost::uuids::uuid& trade_id,
                         const boost::uuids::uuid& activity_id) {
    auto gen = ores::testing::make_generation_context(h);
    auto r = ores::trading::generators::generate_synthetic_fra_instrument(gen);
    r.trade_id = trade_id;
    r.trade_activity_id = activity_id;
    r.currency = "USD";
    r.rate_index = "SOFR";
    r.long_short = "Long";
    r.strike = 0.0425;
    r.notional = decimal::from_string("1000000").value();
    return r;
}

/*
 * One swap leg of the same trade. The reference-data codes come from the
 * generator, which states codes the store's validators accept.
 */
swap_leg make_leg(database_helper& h,
                  const boost::uuids::uuid& trade_id,
                  const boost::uuids::uuid& activity_id,
                  const boost::uuids::uuid& party_id) {
    auto gen = ores::testing::make_generation_context(h);
    auto r = ores::trading::generators::generate_synthetic_swap_leg(gen);
    r.identity.trade_id = trade_id;
    r.identity.leg_number = 1;
    r.identity.trade_activity_id = activity_id;
    r.identity.party_id = party_id;
    return r;
}

}

TEST_CASE("rate_instrument_delete_closes_the_whole_family", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    // The party is written first and its context kept, so the trade anchor and
    // the family rows live under the one party this test can read back.
    const auto ctx = ores::trading::tests::write_parent_party(h);
    const auto party_id = *ctx.party_id();

    auto trade = ores::trading::generators::generate_synthetic_trade(gen);
    trade.party_id = party_id;
    trade.trade_type = "ForwardRateAgreement";
    trade_repository().write(ctx, trade);
    const auto trade_id = trade.id;

    const auto activity_id = ores::trading::tests::write_parent_activity(h);
    const auto id_str = boost::uuids::to_string(trade_id);

    rate_instrument_repository header_repo;
    fra_instrument_repository fact_repo;
    swap_leg_repository leg_repo;

    const auto header = make_header(h, trade_id, activity_id, party_id);
    const auto fact = make_fact(h, trade_id, activity_id);
    const auto leg = make_leg(h, trade_id, activity_id, party_id);

    REQUIRE_NOTHROW(header_repo.write(ctx, header));
    REQUIRE_NOTHROW(fact_repo.write(ctx, fact));
    REQUIRE_NOTHROW(leg_repo.write(ctx, leg));

    const auto headers = header_repo.read_latest(ctx, id_str);
    REQUIRE(headers.size() == 1);
    CHECK(headers[0].identity.trade_type_code == "ForwardRateAgreement");
    CHECK(headers[0].identity.party_id == party_id);
    CHECK(headers[0].start_date == ores::platform::time::datetime::from_iso8601_date("2025-01-15"));
    CHECK(headers[0].maturity_date ==
          ores::platform::time::datetime::from_iso8601_date("2025-07-15"));
    CHECK(headers[0].description == "FRA family cascade delete proof");

    const auto facts = fact_repo.read_latest(ctx, id_str);
    REQUIRE(facts.size() == 1);
    CHECK(facts[0].trade_id == trade_id);
    CHECK(facts[0].currency == "USD");
    CHECK(facts[0].rate_index == "SOFR");
    CHECK(facts[0].long_short == "Long");
    CHECK(facts[0].strike == 0.0425);
    CHECK(facts[0].notional.to_double() == 1000000.0);

    const auto legs = leg_repo.read_latest(ctx, id_str, "1");
    REQUIRE(legs.size() == 1);
    CHECK(legs[0].identity.trade_id == trade_id);
    CHECK(legs[0].identity.leg_number == 1);
    CHECK(legs[0].currency == "USD");

    const auto status = header_repo.remove(ctx, id_str, std::nullopt);
    BOOST_LOG_SEV(lg, debug) << "Header removal status: " << static_cast<int>(status);
    CHECK(status == rate_instrument_repository::remove_status::removed);

    CHECK(header_repo.read_latest(ctx, id_str).empty());
    CHECK(fact_repo.read_latest(ctx, id_str).empty());
    CHECK(leg_repo.read_latest(ctx, id_str, "1").empty());

    const auto anchor = trade_repository().read_latest(ctx, id_str);
    CHECK(anchor.size() == 1);
}

TEST_CASE("rate_instrument_refuses_a_trade_type_the_anchor_does_not_hold", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto ctx = ores::trading::tests::write_parent_party(h);
    const auto party_id = *ctx.party_id();

    auto trade = ores::trading::generators::generate_synthetic_trade(gen);
    trade.party_id = party_id;
    trade.trade_type = "ForwardRateAgreement";
    trade_repository().write(ctx, trade);

    const auto activity_id = ores::trading::tests::write_parent_activity(h);
    rate_instrument_repository header_repo;

    SECTION("a trade type other than the anchor's") {
        auto header = make_header(h, trade.id, activity_id, party_id);
        header.identity.trade_type_code = "Swap";
        CHECK_THROWS_WITH(header_repo.write(ctx, header),
                          Catch::Matchers::ContainsSubstring("must be the trade"));
    }

    SECTION("the anchor's own party and trade type") {
        const auto header = make_header(h, trade.id, activity_id, party_id);
        CHECK_NOTHROW(header_repo.write(ctx, header));
    }
}

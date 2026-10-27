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
#include "ores.trading.api/messaging/fx_forward_instrument_protocol.hpp"
#include "ores.trading.core/service/fx_forward_instrument_service.hpp"
#include "test_suite.hpp"
#include "trade_parent_seed.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

using namespace ores::logging;
using ores::testing::database_helper;
using ores::trading::messaging::fx_forward_instrument_key;
using ores::trading::messaging::fx_forward_instrument_write;
using ores::trading::messaging::get_many_fx_forward_instruments_request;
using ores::trading::messaging::put_fx_forward_instrument_request;
using ores::trading::service::fx_forward_instrument_service;

fx_forward_instrument_write make_write(database_helper& h, const std::string& description) {
    fx_forward_instrument_write w;
    w.trade_id = ores::trading::tests::write_parent_trade(h);
    w.trade_activity_id = ores::trading::tests::write_parent_activity(h);
    w.trade_type_code = "FxForward";
    w.bought_currency = "EUR";
    w.bought_amount = ores::utility::decimal::decimal::from_string("100").value();
    w.sold_currency = "USD";
    w.sold_amount = ores::utility::decimal::decimal::from_string("110").value();
    w.value_date = ores::platform::time::datetime::from_iso8601_date("2026-02-10");
    w.settlement = "Cash";
    w.description = description;
    return w;
}

fx_forward_instrument_key key_of(const fx_forward_instrument_write& w) {
    fx_forward_instrument_key key;
    key.trade_id = w.trade_id;
    return key;
}

} // namespace

TEST_CASE("get_many_answers_one_entry_per_key_in_the_order_asked",
          "[service][fx][fx_forward][get_many]") {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party_id = boost::uuids::random_generator()();
    auto ctx = h.context().with_party(h.tenant_id(), party_id, {party_id}, h.db_user());
    fx_forward_instrument_service svc(ctx);

    const auto first = make_write(h, "first row");
    const auto third = make_write(h, "third row");
    for (const auto& w : {first, third}) {
        put_fx_forward_instrument_request req;
        req.change.write = w;
        const auto resp = svc.put_fx_forward_instrument(req);
        REQUIRE(resp.result.outcome == ores::utility::domain::outcome::ok);
    }

    // The middle key names nothing, so an implementation that dropped absent
    // keys would answer two entries and a caller would pair the wrong row with
    // the wrong key.
    fx_forward_instrument_key missing;
    missing.trade_id = boost::uuids::random_generator()();

    get_many_fx_forward_instruments_request req;
    req.keys = {key_of(first), missing, key_of(third)};
    const auto resp = svc.get_many_fx_forward_instruments(req);

    REQUIRE(resp.entries.size() == 3);
    CHECK(resp.entries[0].key.trade_id == first.trade_id);
    REQUIRE(resp.entries[0].fx_forward_instrument.has_value());
    // The row is identified, not just present: a join that cross-wires the
    // entries would still answer three of them.
    CHECK(resp.entries[0].fx_forward_instrument->identity.trade_id == first.trade_id);
    CHECK(resp.entries[0].fx_forward_instrument->description == "first row");
    CHECK(resp.entries[1].key.trade_id == missing.trade_id);
    CHECK_FALSE(resp.entries[1].fx_forward_instrument.has_value());
    CHECK(resp.entries[2].key.trade_id == third.trade_id);
    REQUIRE(resp.entries[2].fx_forward_instrument.has_value());
    CHECK(resp.entries[2].fx_forward_instrument->identity.trade_id == third.trade_id);
    CHECK(resp.entries[2].fx_forward_instrument->description == "third row");

    // The batch read after a write answers the rows the write landed.
    const auto batch_first = make_write(h, "batch first");
    const auto batch_second = make_write(h, "batch second");
    ores::trading::messaging::put_many_fx_forward_instruments_request batch_req;
    for (const auto& w : {batch_first, batch_second}) {
        ores::trading::messaging::fx_forward_instrument_change change;
        change.write = w;
        batch_req.changes.push_back(change);
    }
    const auto batch_resp = svc.put_many_fx_forward_instruments(batch_req);
    REQUIRE(batch_resp.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE(batch_resp.fx_forward_instruments.size() == 2);
    CHECK(batch_resp.fx_forward_instruments[0].identity.trade_id == batch_first.trade_id);
    CHECK(batch_resp.fx_forward_instruments[0].description == "batch first");
    CHECK(batch_resp.fx_forward_instruments[1].identity.trade_id == batch_second.trade_id);
    CHECK(batch_resp.fx_forward_instruments[1].description == "batch second");

    BOOST_LOG_SEV(lg, debug) << "Answered " << resp.entries.size() << " entries.";
}

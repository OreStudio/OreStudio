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
#include "ores.qt.headless/IInstrumentFormPopulator.hpp"
#include "ores.qt.headless/parse_trade_instrument.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/domain/product_type.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/domain/trade_instrument.hpp"
#include "ores.trading.api/messaging/trade_protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <rfl.hpp>
#include <rfl/json.hpp>

namespace {

using namespace ores::trading::domain;
using namespace ores::trading::messaging;

const std::string tags("[instrument_parse_dispatch]");

// --- Test helpers ---

trade make_test_trade(product_type pt, const std::string& trade_type) {
    trade t;
    t.identity.id = boost::uuids::random_generator()();
    t.identity.party_id = boost::uuids::random_generator()();
    t.identity.version = 1;
    t.classification.product_type = pt;
    t.classification.trade_type = trade_type;
    t.classification.activity_type_code = "new_booking";
    t.lifecycle.trade_date = "2026-01-15";
    t.lifecycle.effective_date = "2026-01-20";
    t.lifecycle.termination_date = "2031-01-20";
    return t;
}

std::string make_response_json(trade t, trade_instrument instr) {
    get_trade_instrument_response resp;
    resp.success = true;
    resp.message = "";
    resp.trade = std::move(t);
    resp.instrument = std::move(instr);
    return rfl::json::write(resp);
}

// --- Concrete test populators (one per leaf type under test) ---

struct FxForwardPopulator : ores::qt::IInstrumentFormPopulator {
    fx_forward_instrument got;
    bool called = false;
    void populate(const fx_forward_instrument& i) override {
        got = i;
        called = true;
    }
};

struct FxVanillaOptionPopulator : ores::qt::IInstrumentFormPopulator {
    fx_vanilla_option_instrument got;
    bool called = false;
    void populate(const fx_vanilla_option_instrument& i) override {
        got = i;
        called = true;
    }
};

struct BondPopulator : ores::qt::IInstrumentFormPopulator {
    bond_instrument got;
    bool called = false;
    void populate(const bond_instrument& i) override {
        got = i;
        called = true;
    }
};

struct CreditPopulator : ores::qt::IInstrumentFormPopulator {
    credit_instrument got;
    bool called = false;
    void populate(const credit_instrument& i) override {
        got = i;
        called = true;
    }
};

struct CommodityPopulator : ores::qt::IInstrumentFormPopulator {
    commodity_instrument got;
    bool called = false;
    void populate(const commodity_instrument& i) override {
        got = i;
        called = true;
    }
};

struct ScriptedPopulator : ores::qt::IInstrumentFormPopulator {
    scripted_instrument got;
    bool called = false;
    void populate(const scripted_instrument& i) override {
        got = i;
        called = true;
    }
};

struct VanillaSwapPopulator : ores::qt::IInstrumentFormPopulator {
    vanilla_swap_instrument got;
    bool called = false;
    std::vector<swap_leg> got_legs;
    void populate(const vanilla_swap_instrument& i, std::vector<swap_leg> legs) override {
        got = i;
        called = true;
        got_legs = std::move(legs);
    }
};

struct FraPopulator : ores::qt::IInstrumentFormPopulator {
    fra_instrument got;
    bool called = false;
    std::vector<swap_leg> got_legs;
    void populate(const fra_instrument& i, std::vector<swap_leg> legs) override {
        got = i;
        called = true;
        got_legs = std::move(legs);
    }
};

struct EquityOptionPopulator : ores::qt::IInstrumentFormPopulator {
    equity_option_instrument got;
    bool called = false;
    void populate(const equity_option_instrument& i) override {
        got = i;
        called = true;
    }
};

struct EquityForwardPopulator : ores::qt::IInstrumentFormPopulator {
    equity_forward_instrument got;
    bool called = false;
    void populate(const equity_forward_instrument& i) override {
        got = i;
        called = true;
    }
};

} // namespace

// =============================================================================
// Bond
// =============================================================================

TEST_CASE("parse_trade_instrument_dispatches_bond", tags) {
    bond_instrument instr;
    instr.identity.instrument_id = boost::uuids::random_generator()();
    instr.identity.trade_type_code = "Bond";

    auto json =
        make_response_json(make_test_trade(product_type::bond, "Bond"), trade_instrument{instr});

    BondPopulator pop;
    auto result = ores::qt::parse_trade_instrument(json, pop);

    REQUIRE(result.has_value());
    CHECK(result->classification.product_type == product_type::bond);
    REQUIRE(pop.called);
    CHECK(pop.got.identity.instrument_id == instr.identity.instrument_id);
    CHECK(pop.got.identity.trade_type_code == "Bond");
}

// =============================================================================
// Credit
// =============================================================================

TEST_CASE("parse_trade_instrument_dispatches_credit", tags) {
    credit_instrument instr;
    instr.identity.instrument_id = boost::uuids::random_generator()();
    instr.identity.trade_type_code = "CreditDefaultSwap";

    auto json = make_response_json(make_test_trade(product_type::credit, "CreditDefaultSwap"),
                                   trade_instrument{instr});

    CreditPopulator pop;
    auto result = ores::qt::parse_trade_instrument(json, pop);

    REQUIRE(result.has_value());
    CHECK(result->classification.product_type == product_type::credit);
    REQUIRE(pop.called);
    CHECK(pop.got.identity.instrument_id == instr.identity.instrument_id);
}

// =============================================================================
// Commodity
// =============================================================================

TEST_CASE("parse_trade_instrument_dispatches_commodity", tags) {
    commodity_instrument instr;
    instr.identity.instrument_id = boost::uuids::random_generator()();
    instr.identity.trade_type_code = "Commodity";

    auto json = make_response_json(make_test_trade(product_type::commodity, "Commodity"),
                                   trade_instrument{instr});

    CommodityPopulator pop;
    auto result = ores::qt::parse_trade_instrument(json, pop);

    REQUIRE(result.has_value());
    CHECK(result->classification.product_type == product_type::commodity);
    REQUIRE(pop.called);
    CHECK(pop.got.identity.instrument_id == instr.identity.instrument_id);
}

// =============================================================================
// Scripted
// =============================================================================

TEST_CASE("parse_trade_instrument_dispatches_scripted", tags) {
    scripted_instrument instr;
    instr.identity.instrument_id = boost::uuids::random_generator()();
    instr.identity.trade_type_code = "ScriptedTrade";

    auto json = make_response_json(make_test_trade(product_type::scripted, "ScriptedTrade"),
                                   trade_instrument{instr});

    ScriptedPopulator pop;
    auto result = ores::qt::parse_trade_instrument(json, pop);

    REQUIRE(result.has_value());
    CHECK(result->classification.product_type == product_type::scripted);
    REQUIRE(pop.called);
    CHECK(pop.got.identity.instrument_id == instr.identity.instrument_id);
}

// =============================================================================
// FX — forward
// =============================================================================

TEST_CASE("parse_trade_instrument_dispatches_fx_forward", tags) {
    fx_forward_instrument instr;
    instr.identity.instrument_id = boost::uuids::random_generator()();
    instr.identity.trade_type_code = "FxForward";
    instr.bought_currency = "EUR";
    instr.sold_currency = "USD";
    instr.bought_amount = 1'000'000.0;
    instr.sold_amount = 1'100'000.0;
    instr.value_date = "2026-06-30";

    auto json = make_response_json(make_test_trade(product_type::fx, "FxForward"),
                                   trade_instrument{fx_instrument_variant{instr}});

    FxForwardPopulator pop;
    auto result = ores::qt::parse_trade_instrument(json, pop);

    REQUIRE(result.has_value());
    CHECK(result->classification.product_type == product_type::fx);
    REQUIRE(pop.called);
    CHECK(pop.got.identity.instrument_id == instr.identity.instrument_id);
    CHECK(pop.got.bought_currency == "EUR");
    CHECK(pop.got.sold_currency == "USD");
    CHECK(pop.got.value_date == "2026-06-30");
}

// =============================================================================
// FX — vanilla option
// =============================================================================

TEST_CASE("parse_trade_instrument_dispatches_fx_vanilla_option", tags) {
    fx_vanilla_option_instrument instr;
    instr.identity.instrument_id = boost::uuids::random_generator()();
    instr.identity.trade_type_code = "FxOption";

    auto json = make_response_json(make_test_trade(product_type::fx, "FxOption"),
                                   trade_instrument{fx_instrument_variant{instr}});

    FxVanillaOptionPopulator pop;
    auto result = ores::qt::parse_trade_instrument(json, pop);

    REQUIRE(result.has_value());
    CHECK(result->classification.product_type == product_type::fx);
    REQUIRE(pop.called);
    CHECK(pop.got.identity.instrument_id == instr.identity.instrument_id);
}

// =============================================================================
// Rates / swap — FRA
// =============================================================================

TEST_CASE("parse_trade_instrument_dispatches_fra", tags) {
    fra_instrument instr;
    instr.identity.instrument_id = boost::uuids::random_generator()();
    instr.identity.trade_type_code = "ForwardRateAgreement";

    swap_leg leg;
    leg.identity.leg_number = 1;

    auto json =
        make_response_json(make_test_trade(product_type::swap, "ForwardRateAgreement"),
                           trade_instrument{swap_instrument_data{
                               .instrument = rates_instrument_variant{instr}, .legs = {leg}}});

    FraPopulator pop;
    auto result = ores::qt::parse_trade_instrument(json, pop);

    REQUIRE(result.has_value());
    CHECK(result->classification.product_type == product_type::swap);
    REQUIRE(pop.called);
    CHECK(pop.got.identity.instrument_id == instr.identity.instrument_id);
    REQUIRE(pop.got_legs.size() == 1);
    CHECK(pop.got_legs[0].identity.leg_number == 1);
}

// =============================================================================
// Rates / swap — vanilla swap
// =============================================================================

TEST_CASE("parse_trade_instrument_dispatches_vanilla_swap", tags) {
    vanilla_swap_instrument instr;
    instr.identity.instrument_id = boost::uuids::random_generator()();
    instr.identity.trade_type_code = "Swap";

    swap_leg leg1;
    leg1.identity.leg_number = 1;
    swap_leg leg2;
    leg2.identity.leg_number = 2;

    auto json = make_response_json(
        make_test_trade(product_type::swap, "Swap"),
        trade_instrument{swap_instrument_data{.instrument = rates_instrument_variant{instr},
                                              .legs = {leg1, leg2}}});

    VanillaSwapPopulator pop;
    auto result = ores::qt::parse_trade_instrument(json, pop);

    REQUIRE(result.has_value());
    CHECK(result->classification.product_type == product_type::swap);
    REQUIRE(pop.called);
    CHECK(pop.got.identity.instrument_id == instr.identity.instrument_id);
    REQUIRE(pop.got_legs.size() == 2);
    CHECK(pop.got_legs[0].identity.leg_number == 1);
    CHECK(pop.got_legs[1].identity.leg_number == 2);
}

// =============================================================================
// Equity — option
// =============================================================================

TEST_CASE("parse_trade_instrument_dispatches_equity_option", tags) {
    equity_option_instrument instr;
    instr.identity.instrument_id = boost::uuids::random_generator()();
    instr.identity.trade_type_code = "EquityOption";
    instr.underlying_name = "AAPL";
    instr.currency = "USD";
    instr.option_type = "Call";

    auto json = make_response_json(make_test_trade(product_type::equity, "EquityOption"),
                                   trade_instrument{equity_instrument_variant{instr}});

    EquityOptionPopulator pop;
    auto result = ores::qt::parse_trade_instrument(json, pop);

    REQUIRE(result.has_value());
    CHECK(result->classification.product_type == product_type::equity);
    REQUIRE(pop.called);
    CHECK(pop.got.identity.instrument_id == instr.identity.instrument_id);
    CHECK(pop.got.underlying_name == "AAPL");
    CHECK(pop.got.option_type == "Call");
}

// =============================================================================
// Equity — forward
// =============================================================================

TEST_CASE("parse_trade_instrument_dispatches_equity_forward", tags) {
    equity_forward_instrument instr;
    instr.identity.instrument_id = boost::uuids::random_generator()();
    instr.identity.trade_type_code = "EquityForward";

    auto json = make_response_json(make_test_trade(product_type::equity, "EquityForward"),
                                   trade_instrument{equity_instrument_variant{instr}});

    EquityForwardPopulator pop;
    auto result = ores::qt::parse_trade_instrument(json, pop);

    REQUIRE(result.has_value());
    CHECK(result->classification.product_type == product_type::equity);
    REQUIRE(pop.called);
    CHECK(pop.got.identity.instrument_id == instr.identity.instrument_id);
}

// =============================================================================
// Error cases
// =============================================================================

TEST_CASE("parse_trade_instrument_returns_nullopt_for_unknown_product_type", tags) {
    auto json = make_response_json(make_test_trade(product_type::unknown, "Something"),
                                   trade_instrument{bond_instrument{}});

    ores::qt::IInstrumentFormPopulator pop;
    auto result = ores::qt::parse_trade_instrument(json, pop);

    CHECK_FALSE(result.has_value());
}

TEST_CASE("parse_trade_instrument_returns_nullopt_for_unknown_fx_trade_type", tags) {
    auto json =
        make_response_json(make_test_trade(product_type::fx, "UnknownFxType"),
                           trade_instrument{fx_instrument_variant{fx_forward_instrument{}}});

    ores::qt::IInstrumentFormPopulator pop;
    auto result = ores::qt::parse_trade_instrument(json, pop);

    CHECK_FALSE(result.has_value());
}

TEST_CASE("parse_trade_instrument_returns_nullopt_for_server_error", tags) {
    get_trade_instrument_response resp;
    resp.success = false;
    resp.message = "trade not found";

    ores::qt::IInstrumentFormPopulator pop;
    auto result = ores::qt::parse_trade_instrument(rfl::json::write(resp), pop);

    CHECK_FALSE(result.has_value());
}

TEST_CASE("parse_trade_instrument_returns_nullopt_for_malformed_json", tags) {
    ores::qt::IInstrumentFormPopulator pop;
    auto result = ores::qt::parse_trade_instrument("{not valid json!!!", pop);

    CHECK_FALSE(result.has_value());
}

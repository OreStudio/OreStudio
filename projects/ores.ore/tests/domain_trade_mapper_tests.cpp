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
#include "ores.ore/domain/trade_mapper.hpp"

#include <boost/uuid/nil_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][domain][trade_mapper]");

}

using ores::ore::domain::trade_mapper;
using ores::ore::domain::oreTradeType;
using ores::ore::domain::envelope;
using ores::ore::domain::envelope_CounterParty_t;
using ores::ore::domain::_NettingSetId_t;
using ores::ore::domain::nettingSetGroup_group_t;
using ores::ore::domain::envelope_PortfolioIds_t;
using ores::ore::domain::envelope_PortfolioIds_t_PortfolioId_t;
using namespace ores::logging;

// =============================================================================
// map(trade) -> trading::domain::trade
// =============================================================================

TEST_CASE("map_trade_with_swap_type", tags) {
    auto lg(make_logger(test_suite));

    ores::ore::domain::trade ore_trade;
    static_cast<xsd::string&>(ore_trade.id) = "Swap_20y";
    ore_trade.TradeType = oreTradeType::Swap;

    const auto result = trade_mapper::map(ore_trade);
    BOOST_LOG_SEV(lg, debug) << "Mapped trade: " << result.external_id;

    CHECK(result.external_id == "Swap_20y");
    CHECK(result.trade_type == "Swap");
    CHECK(result.lifecycle_event == "New");
    CHECK(result.modified_by == "ores");
    CHECK(result.change_reason_code == "system.external_data_import");
    CHECK(result.change_commentary == "Imported from ORE XML portfolio");
}

TEST_CASE("map_trade_with_fx_forward_type", tags) {
    auto lg(make_logger(test_suite));

    ores::ore::domain::trade ore_trade;
    static_cast<xsd::string&>(ore_trade.id) = "FxFwd_EUR_USD_1";
    ore_trade.TradeType = oreTradeType::FxForward;

    const auto result = trade_mapper::map(ore_trade);
    BOOST_LOG_SEV(lg, debug) << "Mapped trade: " << result.external_id;

    CHECK(result.external_id == "FxFwd_EUR_USD_1");
    CHECK(result.trade_type == "FxForward");
}

TEST_CASE("map_trade_all_trade_types_produce_non_empty_string", tags) {
    auto lg(make_logger(test_suite));

    struct test_case {
        oreTradeType input;
        std::string expected;
    };

    const std::vector<test_case> cases = {
        {oreTradeType::Swap, "Swap"},
        {oreTradeType::Swaption, "Swaption"},
        {oreTradeType::FxForward, "FxForward"},
        {oreTradeType::FxOption, "FxOption"},
        {oreTradeType::CapFloor, "CapFloor"},
        {oreTradeType::EquityOption, "EquityOption"},
        {oreTradeType::Bond, "Bond"},
        {oreTradeType::CreditDefaultSwap, "CreditDefaultSwap"},
        {oreTradeType::CommodityForward, "CommodityForward"},
        {oreTradeType::ForwardRateAgreement, "ForwardRateAgreement"},
        {oreTradeType::CrossCurrencySwap, "CrossCurrencySwap"},
        {oreTradeType::InflationSwap, "InflationSwap"},
    };

    for (const auto& tc : cases) {
        ores::ore::domain::trade ore_trade;
        static_cast<xsd::string&>(ore_trade.id) = "Trade_1";
        ore_trade.TradeType = tc.input;

        const auto result = trade_mapper::map(ore_trade);
        INFO("Trade type: " << tc.expected);
        CHECK(result.trade_type == tc.expected);
    }
}

TEST_CASE("map_trade_with_envelope_and_netting_set", tags) {
    auto lg(make_logger(test_suite));

    ores::ore::domain::trade ore_trade;
    static_cast<xsd::string&>(ore_trade.id) = "Swap_1";
    ore_trade.TradeType = oreTradeType::Swap;

    envelope env;
    envelope_CounterParty_t cp;
    static_cast<xsd::string&>(cp) = "CPTY_A";
    env.CounterParty = cp;

    nettingSetGroup_group_t nsg;
    _NettingSetId_t nsi;
    static_cast<xsd::string&>(nsi) = "NS_CPTY_A";
    nsg.NettingSetId = nsi;
    env.nettingSetGroup = nsg;

    ore_trade.Envelope = env;

    const auto result = trade_mapper::map(ore_trade);
    BOOST_LOG_SEV(lg, debug) << "Mapped trade: " << result.external_id
                             << " netting_set: " << result.netting_set_id;

    CHECK(result.external_id == "Swap_1");
    CHECK(result.netting_set_id == "NS_CPTY_A");
}

TEST_CASE("map_trade_without_envelope_leaves_netting_set_empty", tags) {
    auto lg(make_logger(test_suite));

    ores::ore::domain::trade ore_trade;
    static_cast<xsd::string&>(ore_trade.id) = "Swap_1";
    ore_trade.TradeType = oreTradeType::Swap;
    // No Envelope set

    const auto result = trade_mapper::map(ore_trade);
    BOOST_LOG_SEV(lg, debug) << "Mapped trade: " << result.external_id;

    CHECK(result.netting_set_id.empty());
}

TEST_CASE("map_trade_leaves_uuid_fields_as_nil", tags) {
    auto lg(make_logger(test_suite));

    ores::ore::domain::trade ore_trade;
    static_cast<xsd::string&>(ore_trade.id) = "Swap_1";
    ore_trade.TradeType = oreTradeType::Swap;

    const auto result = trade_mapper::map(ore_trade);
    const auto nil = boost::uuids::nil_uuid();

    CHECK(result.id == nil);
    CHECK(result.party_id == nil);
    CHECK(result.book_id == nil);
    CHECK(result.portfolio_id == nil);
    CHECK_FALSE(result.counterparty_id.has_value());
}

// =============================================================================
// map(portfolio) -> vector<trading::domain::trade>
// =============================================================================

TEST_CASE("map_empty_portfolio_to_empty_vector", tags) {
    auto lg(make_logger(test_suite));

    ores::ore::domain::portfolio p;
    const auto result = trade_mapper::map(p);
    BOOST_LOG_SEV(lg, debug) << "Mapped " << result.size() << " trades";

    CHECK(result.empty());
}

TEST_CASE("map_portfolio_with_multiple_trades", tags) {
    auto lg(make_logger(test_suite));

    ores::ore::domain::trade swap;
    static_cast<xsd::string&>(swap.id) = "Swap_1";
    swap.TradeType = oreTradeType::Swap;

    ores::ore::domain::trade fxfwd;
    static_cast<xsd::string&>(fxfwd.id) = "FxFwd_1";
    fxfwd.TradeType = oreTradeType::FxForward;

    ores::ore::domain::trade cap;
    static_cast<xsd::string&>(cap.id) = "Cap_1";
    cap.TradeType = oreTradeType::CapFloor;

    ores::ore::domain::portfolio p;
    p.Trade.push_back(swap);
    p.Trade.push_back(fxfwd);
    p.Trade.push_back(cap);

    const auto result = trade_mapper::map(p);
    BOOST_LOG_SEV(lg, debug) << "Mapped " << result.size() << " trades";

    REQUIRE(result.size() == 3);
    CHECK(result[0].external_id == "Swap_1");
    CHECK(result[0].trade_type == "Swap");
    CHECK(result[1].external_id == "FxFwd_1");
    CHECK(result[1].trade_type == "FxForward");
    CHECK(result[2].external_id == "Cap_1");
    CHECK(result[2].trade_type == "CapFloor");
}

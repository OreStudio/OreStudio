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
#include "ores.ore.core/domain/portfolio_projection.hpp"
#include "ores.ore.core/xml/record_diff.hpp"
#include "ores.trading.api/domain/instrument_batch_mapper.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][xml][portfolio][projection]");

using namespace ores::logging;
using namespace ores::ore::domain;
namespace pf = ores::ore::xml::generated::portfolio;

const char* const document_text = R"(<Portfolio>
  <Trade id="trade-1">
    <TradeType>ScriptedTrade</TradeType>
    <Envelope>
      <CounterParty>CPTY_A</CounterParty>
      <NettingSetId>NS-1</NettingSetId>
      <PortfolioIds><PortfolioId>PF-1</PortfolioId></PortfolioIds>
      <AdditionalFields><Trader>marco</Trader></AdditionalFields>
    </Envelope>
    <ScriptedTradeData>
      <ScriptName>heston</ScriptName>
      <Script><NPV>Option</NPV><Code>Payoff = 1;</Code></Script>
      <Data>
        <Number><Name>PutCall</Name><Value>1</Value></Number>
        <Index><Name>Underlying_0</Name><Value>SPX</Value></Index>
      </Data>
    </ScriptedTradeData>
  </Trade>
</Portfolio>)";

}

TEST_CASE("a_portfolio_document_projects_its_trades_and_their_envelopes", tags) {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(document_text);
    REQUIRE(document.trade.size() == 1);

    // The writing binds back to the record the reading produced.
    const auto written = portfolio_projection::write(document);
    const auto again = portfolio_projection::read(written);
    CHECK_FALSE(ores::ore::xml::compare_records(
        *pf::portfolio_document().root, &document, &again));

    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 1);
    CHECK(items[0].ore_id == "trade-1");
    CHECK(items[0].anchor.trade_type == "ScriptedTrade");
    REQUIRE(items[0].envelope.has_value());
    CHECK(items[0].envelope->counter_party == std::optional<std::string>("CPTY_A"));
    CHECK(items[0].envelope->netting_set_id == std::optional<std::string>("NS-1"));
    REQUIRE(items[0].envelope->portfolio_ids.has_value());
    CHECK(items[0].envelope->portfolio_ids->at(0) == "PF-1");
    REQUIRE(items[0].envelope->additional_fields.has_value());
    REQUIRE(items[0].envelope->additional_fields->size() == 1);
    CHECK(items[0].envelope->additional_fields->at(0).name == "Trader");
    CHECK(items[0].envelope->additional_fields->at(0).value == "marco");

    const auto* instrument =
        std::get_if<ores::trading::domain::scripted_instrument>(&items[0].instrument);
    REQUIRE(instrument != nullptr);
    CHECK(instrument->script_name == "heston");
    CHECK(instrument->script_body == "Payoff = 1;");
    CHECK(instrument->parameters_json == "{\"NPV\":\"Option\",\"PutCall\":\"1\"}");
    CHECK(instrument->underlyings_json == "[\"SPX\"]");
}

TEST_CASE("a_portfolio_document_is_built_from_the_trades_and_their_instruments", tags) {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(document_text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 1);

    const auto trade_id = boost::uuids::string_generator{}(
        "01234567-89ab-cdef-0123-456789abcdef");

    ores::trading::messaging::trade_export_item item;
    item.ore_id = items[0].ore_id;
    item.anchor.id = trade_id;
    item.anchor.trade_type = items[0].anchor.trade_type;
    item.envelope = items[0].envelope;

    ores::trading::domain::instrument_batch batch;
    auto instrument = std::get<ores::trading::domain::scripted_instrument>(items[0].instrument);
    instrument.identity.trade_id = trade_id;
    batch.scripted_instruments.push_back(std::move(instrument));

    const auto built = portfolio_projection::reverse({item}, batch);
    REQUIRE(built.trade.size() == 1);

    const auto back = portfolio_projection::map(built);
    REQUIRE(back.size() == 1);
    CHECK(back[0].ore_id == "trade-1");
    CHECK(back[0].anchor.trade_type == "ScriptedTrade");
    REQUIRE(back[0].envelope.has_value());
    CHECK(back[0].envelope->counter_party == std::optional<std::string>("CPTY_A"));
    REQUIRE(back[0].envelope->additional_fields.has_value());
    CHECK(back[0].envelope->additional_fields->at(0).value == "marco");

    const auto* round =
        std::get_if<ores::trading::domain::scripted_instrument>(&back[0].instrument);
    REQUIRE(round != nullptr);
    CHECK(round->script_name == "heston");
    CHECK(round->script_body == "Payoff = 1;");
    CHECK(round->parameters_json == "{\"NPV\":\"Option\",\"PutCall\":\"1\"}");
    CHECK(round->underlyings_json == "[\"SPX\"]");
}

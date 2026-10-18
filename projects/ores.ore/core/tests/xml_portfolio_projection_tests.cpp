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
#include <chrono>
#include <optional>
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

const char* const swap_document_text = R"(<Portfolio>
  <Trade id="swap-1">
    <TradeType>Swap</TradeType>
    <Envelope>
      <CounterParty>CPTY_A</CounterParty>
      <NettingSetId>NS-1</NettingSetId>
    </Envelope>
    <SwapData>
      <LegData>
        <Payer>true</Payer>
        <LegType>Fixed</LegType>
        <Currency>USD</Currency>
        <DayCounter>ACT/360</DayCounter>
        <Notionals><Notional>1000000</Notional></Notionals>
        <ScheduleData>
          <Rules>
            <StartDate>2024-01-15</StartDate>
            <EndDate>2029-01-15</EndDate>
            <Tenor>1Y</Tenor>
            <Convention>MF</Convention>
          </Rules>
        </ScheduleData>
        <FixedLegData><Rates><Rate>0.05</Rate></Rates></FixedLegData>
      </LegData>
      <LegData>
        <Payer>false</Payer>
        <LegType>Floating</LegType>
        <Currency>USD</Currency>
        <DayCounter>ACT/360</DayCounter>
        <Notionals><Notional>1000000</Notional></Notionals>
        <ScheduleData>
          <Rules>
            <StartDate>2024-01-15</StartDate>
            <EndDate>2029-01-15</EndDate>
            <Tenor>6M</Tenor>
            <Convention>MF</Convention>
          </Rules>
        </ScheduleData>
        <FloatingLegData>
          <Index>USD-LIBOR-3M</Index>
          <Spreads><Spread>0.001</Spread></Spreads>
        </FloatingLegData>
      </LegData>
    </SwapData>
  </Trade>
</Portfolio>)";

const char* const cap_floor_document_text = R"(<Portfolio>
  <Trade id="capfloor-1">
    <TradeType>CapFloor</TradeType>
    <Envelope>
      <CounterParty>CPTY_B</CounterParty>
      <NettingSetId>NS-2</NettingSetId>
    </Envelope>
    <CapFloorData>
      <LongShort>Long</LongShort>
      <LegData>
        <Payer>false</Payer>
        <LegType>Floating</LegType>
        <Currency>EUR</Currency>
        <DayCounter>ACT/360</DayCounter>
        <Notionals><Notional>5000000</Notional></Notionals>
        <ScheduleData>
          <Rules>
            <StartDate>2024-03-01</StartDate>
            <EndDate>2029-03-01</EndDate>
            <Tenor>3M</Tenor>
            <Convention>MF</Convention>
          </Rules>
        </ScheduleData>
        <FloatingLegData>
          <Index>EURIBOR-3M</Index>
          <Spreads><Spread>0.0025</Spread></Spreads>
        </FloatingLegData>
      </LegData>
      <Caps><Cap>0.04</Cap></Caps>
    </CapFloorData>
  </Trade>
</Portfolio>)";

const char* const swaption_document_text = R"(<Portfolio>
  <Trade id="swaption-1">
    <TradeType>Swaption</TradeType>
    <Envelope>
      <CounterParty>CPTY_C</CounterParty>
      <NettingSetId>NS-3</NettingSetId>
    </Envelope>
    <SwaptionData>
      <OptionData>
        <LongShort>Long</LongShort>
        <Style>European</Style>
        <Settlement>Cash</Settlement>
        <ExerciseDates><ExerciseDate>2024-06-15</ExerciseDate></ExerciseDates>
        <Premiums>
          <Premium>
            <Amount>25000</Amount>
            <Currency>USD</Currency>
            <PayDate>2024-01-17</PayDate>
          </Premium>
        </Premiums>
      </OptionData>
      <LegData>
        <Payer>true</Payer>
        <LegType>Fixed</LegType>
        <Currency>USD</Currency>
        <DayCounter>ACT/360</DayCounter>
        <Notionals><Notional>1000000</Notional></Notionals>
        <ScheduleData>
          <Rules>
            <StartDate>2024-06-15</StartDate>
            <EndDate>2034-06-15</EndDate>
            <Tenor>1Y</Tenor>
            <Convention>MF</Convention>
          </Rules>
        </ScheduleData>
        <FixedLegData><Rates><Rate>0.045</Rate></Rates></FixedLegData>
      </LegData>
    </SwaptionData>
  </Trade>
</Portfolio>)";

const char* const remaining_products_document_text = R"(<Portfolio>
  <Trade id="fra-1">
    <TradeType>ForwardRateAgreement</TradeType>
    <Envelope>
      <CounterParty>CPTY_A</CounterParty>
      <NettingSetId>NS-1</NettingSetId>
    </Envelope>
    <ForwardRateAgreementData>
      <StartDate>2024-01-15</StartDate>
      <EndDate>2024-07-15</EndDate>
      <Currency>USD</Currency>
      <Index>USD-LIBOR-6M</Index>
      <LongShort>Long</LongShort>
      <Strike>0.03</Strike>
      <Notional>1000000</Notional>
    </ForwardRateAgreementData>
  </Trade>
  <Trade id="knockout-1">
    <TradeType>KnockOutSwap</TradeType>
    <Envelope>
      <CounterParty>CPTY_A</CounterParty>
      <NettingSetId>NS-1</NettingSetId>
    </Envelope>
    <KnockOutSwapData>
      <BarrierData>
        <Type>UpAndOut</Type>
        <Levels><Level>0.06</Level></Levels>
      </BarrierData>
      <BarrierStartDate>2024-02-01</BarrierStartDate>
      <LegData>
        <Payer>false</Payer>
        <LegType>Floating</LegType>
        <Currency>USD</Currency>
        <Notionals><Notional>2000000</Notional></Notionals>
        <ScheduleData>
          <Rules>
            <StartDate>2024-01-15</StartDate>
            <EndDate>2026-01-15</EndDate>
            <Tenor>6M</Tenor>
            <Convention>MF</Convention>
          </Rules>
        </ScheduleData>
        <FloatingLegData>
          <Index>USD-LIBOR-6M</Index>
          <Spreads><Spread>0.005</Spread></Spreads>
        </FloatingLegData>
      </LegData>
    </KnockOutSwapData>
  </Trade>
  <Trade id="inflation-1">
    <TradeType>InflationSwap</TradeType>
    <Envelope>
      <CounterParty>CPTY_A</CounterParty>
      <NettingSetId>NS-1</NettingSetId>
    </Envelope>
    <InflationSwapData>
      <LegData>
        <Payer>false</Payer>
        <LegType>CPI</LegType>
        <Currency>GBP</Currency>
        <Notionals><Notional>1000000</Notional></Notionals>
        <ScheduleData>
          <Rules>
            <StartDate>2024-01-15</StartDate>
            <EndDate>2034-01-15</EndDate>
            <Tenor>1Y</Tenor>
            <Convention>MF</Convention>
          </Rules>
        </ScheduleData>
        <CPILegData>
          <Index>UKRPI</Index>
          <BaseCPI>195.5</BaseCPI>
          <ObservationLag>3M</ObservationLag>
        </CPILegData>
      </LegData>
    </InflationSwapData>
  </Trade>
  <Trade id="flexi-1">
    <TradeType>FlexiSwap</TradeType>
    <Envelope>
      <CounterParty>CPTY_A</CounterParty>
      <NettingSetId>NS-1</NettingSetId>
    </Envelope>
    <FlexiSwapData>
      <LowerNotionalBounds currency="USD">
        <Notional startDate="2024-01-15">500000</Notional>
        <Notional startDate="2025-01-15">400000</Notional>
      </LowerNotionalBounds>
      <OptionLongShort>Long</OptionLongShort>
      <LegData>
        <Payer>true</Payer>
        <LegType>Fixed</LegType>
        <Currency>USD</Currency>
        <Notionals><Notional>1000000</Notional></Notionals>
        <ScheduleData>
          <Rules>
            <StartDate>2024-01-15</StartDate>
            <EndDate>2029-01-15</EndDate>
            <Tenor>1Y</Tenor>
            <Convention>MF</Convention>
          </Rules>
        </ScheduleData>
        <FixedLegData><Rates><Rate>0.04</Rate></Rates></FixedLegData>
      </LegData>
    </FlexiSwapData>
  </Trade>
  <Trade id="bgs-1">
    <TradeType>BalanceGuaranteedSwap</TradeType>
    <Envelope>
      <CounterParty>CPTY_A</CounterParty>
      <NettingSetId>NS-1</NettingSetId>
    </Envelope>
    <BalanceGuaranteedSwapData>
      <ReferenceSecurity>XS1234567890</ReferenceSecurity>
      <Tranches>
        <Tranche>
          <Description>senior</Description>
          <SecurityId>TR-1</SecurityId>
          <Seniority>1</Seniority>
          <Notionals><Notional startDate="2024-01-15">750000</Notional></Notionals>
        </Tranche>
        <ScheduleData>
          <Rules>
            <StartDate>2024-01-15</StartDate>
            <EndDate>2034-01-15</EndDate>
            <Tenor>1Y</Tenor>
            <Convention>MF</Convention>
          </Rules>
        </ScheduleData>
      </Tranches>
      <LegData>
        <Payer>true</Payer>
        <LegType>Fixed</LegType>
        <Currency>USD</Currency>
        <Notionals><Notional>750000</Notional></Notionals>
        <ScheduleData>
          <Rules>
            <StartDate>2024-01-15</StartDate>
            <EndDate>2034-01-15</EndDate>
            <Tenor>1Y</Tenor>
            <Convention>MF</Convention>
          </Rules>
        </ScheduleData>
        <FixedLegData><Rates><Rate>0.055</Rate></Rates></FixedLegData>
      </LegData>
    </BalanceGuaranteedSwapData>
  </Trade>
  <Trade id="callable-1">
    <TradeType>CallableSwap</TradeType>
    <Envelope>
      <CounterParty>CPTY_A</CounterParty>
      <NettingSetId>NS-1</NettingSetId>
    </Envelope>
    <CallableSwapData>
      <OptionData>
        <LongShort>Long</LongShort>
        <ExerciseDates>
          <ExerciseDate>2025-01-15</ExerciseDate>
          <ExerciseDate>2026-01-15</ExerciseDate>
        </ExerciseDates>
      </OptionData>
      <LegData>
        <Payer>true</Payer>
        <LegType>Fixed</LegType>
        <Currency>USD</Currency>
        <Notionals><Notional>1000000</Notional></Notionals>
        <ScheduleData>
          <Rules>
            <StartDate>2024-01-15</StartDate>
            <EndDate>2029-01-15</EndDate>
            <Tenor>1Y</Tenor>
            <Convention>MF</Convention>
          </Rules>
        </ScheduleData>
        <FixedLegData><Rates><Rate>0.04</Rate></Rates></FixedLegData>
      </LegData>
    </CallableSwapData>
  </Trade>
</Portfolio>)";

auto date_of(int year, unsigned month, unsigned day) -> std::chrono::year_month_day {
    return {std::chrono::year{year}, std::chrono::month{month}, std::chrono::day{day}};
}

/**
 * @brief The trade the batch rebuilds from its instrument, read back again.
 *
 * The rows a family stores are what the export reads, so the instrument is
 * stamped with one trade id and handed to the batch the way an import does.
 */
auto rebuild(const ores::ore::xml::trade_import_item& item) -> ores::ore::xml::trade_import_item {
    const auto trade_id = boost::uuids::string_generator{}("01234567-89ab-cdef-0123-456789abcdef");

    auto instrument = item.instrument;
    ores::trading::domain::stamp_ids(instrument, trade_id);

    ores::trading::domain::instrument_batch batch;
    ores::trading::domain::append_instrument(batch, instrument);

    ores::trading::messaging::trade_export_item export_item;
    export_item.ore_id = item.ore_id;
    export_item.anchor.id = trade_id;
    export_item.anchor.trade_type = item.anchor.trade_type;
    export_item.envelope = item.envelope;

    const auto built = portfolio_projection::reverse({export_item}, batch);
    const auto back = portfolio_projection::map(built);
    if (back.empty())
        return {};
    return back.front();
}

}

TEST_CASE("a_portfolio_document_projects_the_legs_of_a_vanilla_swap", tags) {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(swap_document_text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 1);
    CHECK(items[0].anchor.trade_type == "Swap");

    const auto* instrument =
        std::get_if<ores::trading::domain::swap_instrument_data>(&items[0].instrument);
    REQUIRE(instrument != nullptr);
    CHECK(instrument->header.identity.trade_type_code == "Swap");
    CHECK(instrument->header.start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 1, 15)));
    CHECK(instrument->header.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2029, 1, 15)));
    CHECK(std::get_if<ores::trading::domain::vanilla_swap_instrument>(&instrument->facts) !=
          nullptr);

    REQUIRE(instrument->legs.size() == 2);
    CHECK(instrument->legs[0].payer == std::optional<bool>(true));
    CHECK(instrument->legs[0].leg_type_code == "Fixed");
    CHECK(instrument->legs[0].currency == "USD");
    CHECK(instrument->legs[0].day_count_fraction_code == "ACT/360");
    CHECK(instrument->legs[0].payment_frequency_code == "Annual");
    CHECK(instrument->legs[1].payer == std::optional<bool>(false));
    CHECK(instrument->legs[1].leg_type_code == "Floating");
    CHECK(instrument->legs[1].payment_frequency_code == "Semiannual");
    CHECK(instrument->legs[1].floating_index_code == "USD-LIBOR-3M");

    REQUIRE(instrument->leg_amounts.size() == 2);
    CHECK(instrument->leg_amounts[0].leg_number == 1);
    CHECK(instrument->leg_amounts[0].amount.to_string() == "1000000");
    CHECK(instrument->leg_amounts[1].leg_number == 2);
    CHECK(instrument->leg_amounts[1].amount.to_string() == "1000000");

    REQUIRE(instrument->leg_rates.size() == 2);
    CHECK(instrument->leg_rates[0].rate_role == "fixed");
    CHECK(instrument->leg_rates[0].value.to_string() == "0.05");
    CHECK(instrument->leg_rates[1].rate_role == "spread");
    CHECK(instrument->leg_rates[1].value.to_string() == "0.001");

    const auto back = rebuild(items[0]);
    const auto* round = std::get_if<ores::trading::domain::swap_instrument_data>(&back.instrument);
    REQUIRE(round != nullptr);
    CHECK(back.anchor.trade_type == "Swap");
    CHECK(round->header.start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 1, 15)));
    CHECK(round->header.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2029, 1, 15)));
    REQUIRE(round->legs.size() == 2);
    CHECK(round->legs[0].leg_type_code == "Fixed");
    CHECK(round->legs[1].leg_type_code == "Floating");
    CHECK(round->legs[1].floating_index_code == "USD-LIBOR-3M");
    REQUIRE(round->leg_amounts.size() == 2);
    CHECK(round->leg_amounts[0].amount.to_string() == "1000000");
    CHECK(round->leg_amounts[1].amount.to_string() == "1000000");
    REQUIRE(round->leg_rates.size() == 2);
    CHECK(round->leg_rates[0].value.to_string() == "0.05");
    CHECK(round->leg_rates[1].value.to_string() == "0.001");
}

TEST_CASE("a_portfolio_document_projects_a_cap_floor_and_its_strike", tags) {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(cap_floor_document_text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 1);
    CHECK(items[0].anchor.trade_type == "CapFloor");

    const auto* instrument =
        std::get_if<ores::trading::domain::swap_instrument_data>(&items[0].instrument);
    REQUIRE(instrument != nullptr);
    CHECK(instrument->header.identity.trade_type_code == "CapFloor");
    CHECK(instrument->header.start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 3, 1)));
    CHECK(instrument->header.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2029, 3, 1)));
    CHECK(std::get_if<ores::trading::domain::cap_floor_instrument>(&instrument->facts) != nullptr);

    REQUIRE(instrument->legs.size() == 1);
    CHECK(instrument->legs[0].leg_type_code == "Floating");
    CHECK(instrument->legs[0].currency == "EUR");
    CHECK(instrument->legs[0].payment_frequency_code == "Quarterly");
    CHECK(instrument->legs[0].floating_index_code == "EURIBOR-3M");

    REQUIRE(instrument->leg_amounts.size() == 1);
    CHECK(instrument->leg_amounts[0].amount.to_string() == "5000000");
    REQUIRE(instrument->leg_rates.size() == 1);
    CHECK(instrument->leg_rates[0].rate_role == "spread");
    CHECK(instrument->leg_rates[0].value.to_string() == "0.0025");

    REQUIRE(instrument->strikes.size() == 1);
    REQUIRE(instrument->strikes[0].bare_value.has_value());
    CHECK(instrument->strikes[0].bare_value->to_string() == "0.04");

    REQUIRE(instrument->schedules.size() == 1);
    CHECK(instrument->schedules[0].owner_role == "swap");
    CHECK(instrument->schedules[0].schedule_role == "schedule");
    CHECK(instrument->schedules[0].schedule_kind == "rules");
    CHECK(instrument->schedules[0].tenor == std::optional<std::string>("3M"));
    CHECK(instrument->schedules[0].convention == std::optional<std::string>("MF"));
    CHECK(instrument->schedules[0].end_date ==
          std::optional<std::chrono::year_month_day>(date_of(2029, 3, 1)));

    const auto back = rebuild(items[0]);
    const auto* round = std::get_if<ores::trading::domain::swap_instrument_data>(&back.instrument);
    REQUIRE(round != nullptr);
    CHECK(back.anchor.trade_type == "CapFloor");
    CHECK(round->header.start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 3, 1)));
    CHECK(round->header.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2029, 3, 1)));
    REQUIRE(round->legs.size() == 1);
    CHECK(round->legs[0].leg_type_code == "Floating");
    CHECK(round->legs[0].floating_index_code == "EURIBOR-3M");
    REQUIRE(round->leg_amounts.size() == 1);
    CHECK(round->leg_amounts[0].amount.to_string() == "5000000");
    REQUIRE(round->leg_rates.size() == 1);
    CHECK(round->leg_rates[0].value.to_string() == "0.0025");
    REQUIRE(round->strikes.size() == 1);
    REQUIRE(round->strikes[0].bare_value.has_value());
    CHECK(round->strikes[0].bare_value->to_string() == "0.04");
}

TEST_CASE("a_portfolio_document_projects_a_swaption_and_its_option", tags) {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(swaption_document_text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 1);
    CHECK(items[0].anchor.trade_type == "Swaption");

    const auto* instrument =
        std::get_if<ores::trading::domain::swap_instrument_data>(&items[0].instrument);
    REQUIRE(instrument != nullptr);
    CHECK(instrument->header.identity.trade_type_code == "Swaption");
    CHECK(instrument->header.start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 6, 15)));
    CHECK(instrument->header.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2034, 6, 15)));

    const auto* fact = std::get_if<ores::trading::domain::swaption_instrument>(&instrument->facts);
    REQUIRE(fact != nullptr);
    CHECK(fact->exercise_type == "European");
    CHECK(fact->expiry_date == date_of(2024, 6, 15));

    REQUIRE(instrument->options.size() == 1);
    CHECK(instrument->options[0].long_short == "Long");
    CHECK(instrument->options[0].style == std::optional<std::string>("European"));
    CHECK(instrument->options[0].settlement == std::optional<std::string>("Cash"));

    REQUIRE(instrument->option_premiums.size() == 1);
    CHECK(instrument->option_premiums[0].amount.to_string() == "25000");
    CHECK(instrument->option_premiums[0].currency == "USD");
    CHECK(instrument->option_premiums[0].pay_date == date_of(2024, 1, 17));

    REQUIRE(instrument->legs.size() == 1);
    CHECK(instrument->legs[0].leg_type_code == "Fixed");
    REQUIRE(instrument->leg_rates.size() == 1);
    CHECK(instrument->leg_rates[0].value.to_string() == "0.045");

    REQUIRE(instrument->schedules.size() == 1);
    CHECK(instrument->schedules[0].owner_role == "option");
    CHECK(instrument->schedules[0].schedule_role == "exercise_dates");
    REQUIRE(instrument->schedule_dates.size() == 1);
    CHECK(instrument->schedule_dates[0].schedule_date == date_of(2024, 6, 15));

    const auto back = rebuild(items[0]);
    const auto* round = std::get_if<ores::trading::domain::swap_instrument_data>(&back.instrument);
    REQUIRE(round != nullptr);
    CHECK(back.anchor.trade_type == "Swaption");
    CHECK(round->header.start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 6, 15)));
    CHECK(round->header.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2034, 6, 15)));
    const auto* round_fact = std::get_if<ores::trading::domain::swaption_instrument>(&round->facts);
    REQUIRE(round_fact != nullptr);
    CHECK(round_fact->exercise_type == "European");
    CHECK(round_fact->expiry_date == date_of(2024, 6, 15));
    REQUIRE(round->options.size() == 1);
    CHECK(round->options[0].style == std::optional<std::string>("European"));
    REQUIRE(round->option_premiums.size() == 1);
    CHECK(round->option_premiums[0].amount.to_string() == "25000");
    CHECK(round->option_premiums[0].pay_date == date_of(2024, 1, 17));
    REQUIRE(round->legs.size() == 1);
    CHECK(round->legs[0].leg_type_code == "Fixed");
    REQUIRE(round->schedule_dates.size() == 1);
    CHECK(round->schedule_dates[0].schedule_date == date_of(2024, 6, 15));
}

TEST_CASE("a_portfolio_document_projects_the_remaining_rates_products", tags) {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(remaining_products_document_text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 6);

    const auto* fra =
        std::get_if<ores::trading::domain::swap_instrument_data>(&items[0].instrument);
    REQUIRE(fra != nullptr);
    REQUIRE(items[0].anchor.trade_type == "ForwardRateAgreement");
    CHECK(fra->header.start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 1, 15)));
    CHECK(fra->header.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 7, 15)));
    const auto* fra_fact = std::get_if<ores::trading::domain::fra_instrument>(&fra->facts);
    REQUIRE(fra_fact != nullptr);
    CHECK(fra_fact->currency == "USD");
    CHECK(fra_fact->rate_index == "USD-LIBOR-6M");
    CHECK(fra_fact->long_short == "Long");
    CHECK(fra_fact->notional.to_string() == "1000000");
    CHECK(fra_fact->strike.to_string() == "0.03");
    REQUIRE(fra->legs.size() == 1);
    REQUIRE(fra->leg_rates.size() == 1);
    CHECK(fra->leg_rates[0].rate_role == "fixed");
    CHECK(fra->leg_rates[0].value.to_string() == "0.03");

    const auto* knock_out =
        std::get_if<ores::trading::domain::swap_instrument_data>(&items[1].instrument);
    REQUIRE(knock_out != nullptr);
    REQUIRE(items[1].anchor.trade_type == "KnockOutSwap");
    const auto* knock_out_fact =
        std::get_if<ores::trading::domain::knock_out_swap_instrument>(&knock_out->facts);
    REQUIRE(knock_out_fact != nullptr);
    CHECK(knock_out_fact->barrier_type == "UpAndOut");
    CHECK(knock_out_fact->barrier_start_date == date_of(2024, 2, 1));
    CHECK(knock_out_fact->barrier_level.to_string() == "0.06");
    REQUIRE(knock_out->leg_rates.size() == 1);
    CHECK(knock_out->leg_rates[0].value.to_string() == "0.005");

    const auto* inflation =
        std::get_if<ores::trading::domain::swap_instrument_data>(&items[2].instrument);
    REQUIRE(inflation != nullptr);
    REQUIRE(items[2].anchor.trade_type == "InflationSwap");
    const auto* inflation_fact =
        std::get_if<ores::trading::domain::inflation_swap_instrument>(&inflation->facts);
    REQUIRE(inflation_fact != nullptr);
    CHECK(inflation_fact->inflation_index_code == "UKRPI");
    CHECK(inflation_fact->base_cpi == std::optional<double>(195.5));
    CHECK(inflation_fact->lag_convention == "3M");
    REQUIRE(inflation->legs.size() == 1);
    CHECK(inflation->legs[0].leg_type_code == "CPI");

    const auto* flexi =
        std::get_if<ores::trading::domain::swap_instrument_data>(&items[3].instrument);
    REQUIRE(flexi != nullptr);
    REQUIRE(items[3].anchor.trade_type == "FlexiSwap");
    const auto* flexi_fact =
        std::get_if<ores::trading::domain::flexi_swap_instrument>(&flexi->facts);
    REQUIRE(flexi_fact != nullptr);
    CHECK(flexi_fact->option_long_short == "Long");
    REQUIRE(flexi->lower_notionals.size() == 2);
    CHECK(flexi->lower_notionals[0].bound_number == 1);
    CHECK(flexi->lower_notionals[0].currency == std::optional<std::string>("USD"));
    CHECK(flexi->lower_notionals[0].notional.to_string() == "500000");
    CHECK(flexi->lower_notionals[1].start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2025, 1, 15)));
    CHECK(flexi->lower_notionals[1].notional.to_string() == "400000");

    const auto* balance =
        std::get_if<ores::trading::domain::swap_instrument_data>(&items[4].instrument);
    REQUIRE(balance != nullptr);
    REQUIRE(items[4].anchor.trade_type == "BalanceGuaranteedSwap");
    const auto* balance_fact =
        std::get_if<ores::trading::domain::balance_guaranteed_swap_instrument>(&balance->facts);
    REQUIRE(balance_fact != nullptr);
    CHECK(balance_fact->reference_security == "XS1234567890");
    REQUIRE(balance->tranches.size() == 1);
    CHECK(balance->tranches[0].security_id == "TR-1");
    CHECK(balance->tranches[0].description == std::optional<std::string>("senior"));
    CHECK(balance->tranches[0].seniority == 1);
    REQUIRE(balance->tranche_notionals.size() == 1);
    CHECK(balance->tranche_notionals[0].tranche_number == 1);
    CHECK(balance->tranche_notionals[0].notional.to_string() == "750000");
    REQUIRE(balance->schedules.size() == 1);
    CHECK(balance->schedules[0].owner_role == "tranches");

    const auto* callable =
        std::get_if<ores::trading::domain::swap_instrument_data>(&items[5].instrument);
    REQUIRE(callable != nullptr);
    REQUIRE(items[5].anchor.trade_type == "CallableSwap");
    REQUIRE(callable->call_dates.size() == 2);
    CHECK(callable->call_dates[0].call_date == date_of(2025, 1, 15));
    CHECK(callable->call_dates[1].call_date == date_of(2026, 1, 15));
    CHECK(callable->header.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2029, 1, 15)));

    const auto fra_again_item = rebuild(items[0]);
    const auto* fra_again =
        std::get_if<ores::trading::domain::swap_instrument_data>(&fra_again_item.instrument);
    REQUIRE(fra_again != nullptr);
    const auto* fra_fact_again =
        std::get_if<ores::trading::domain::fra_instrument>(&fra_again->facts);
    REQUIRE(fra_fact_again != nullptr);
    CHECK(fra_fact_again->currency == "USD");
    CHECK(fra_fact_again->rate_index == "USD-LIBOR-6M");
    CHECK(fra_fact_again->notional.to_string() == "1000000");
    CHECK(fra_fact_again->strike.to_string() == "0.03");

    const auto knock_out_again_item = rebuild(items[1]);
    const auto* knock_out_again =
        std::get_if<ores::trading::domain::swap_instrument_data>(&knock_out_again_item.instrument);
    REQUIRE(knock_out_again != nullptr);
    const auto* knock_out_fact_again =
        std::get_if<ores::trading::domain::knock_out_swap_instrument>(&knock_out_again->facts);
    REQUIRE(knock_out_fact_again != nullptr);
    CHECK(knock_out_fact_again->barrier_type == "UpAndOut");
    CHECK(knock_out_fact_again->barrier_level.to_string() == "0.06");

    const auto inflation_again_item = rebuild(items[2]);
    const auto* inflation_again =
        std::get_if<ores::trading::domain::swap_instrument_data>(&inflation_again_item.instrument);
    REQUIRE(inflation_again != nullptr);
    const auto* inflation_fact_again =
        std::get_if<ores::trading::domain::inflation_swap_instrument>(&inflation_again->facts);
    REQUIRE(inflation_fact_again != nullptr);
    CHECK(inflation_fact_again->inflation_index_code == "UKRPI");
    CHECK(inflation_fact_again->base_cpi == std::optional<double>(195.5));
    CHECK(inflation_fact_again->lag_convention == "3M");

    const auto flexi_again_item = rebuild(items[3]);
    const auto* flexi_again =
        std::get_if<ores::trading::domain::swap_instrument_data>(&flexi_again_item.instrument);
    REQUIRE(flexi_again != nullptr);
    REQUIRE(flexi_again->lower_notionals.size() == 2);
    CHECK(flexi_again->lower_notionals[0].currency == std::optional<std::string>("USD"));
    CHECK(flexi_again->lower_notionals[0].notional.to_string() == "500000");
    CHECK(flexi_again->lower_notionals[1].notional.to_string() == "400000");

    const auto balance_again_item = rebuild(items[4]);
    const auto* balance_again =
        std::get_if<ores::trading::domain::swap_instrument_data>(&balance_again_item.instrument);
    REQUIRE(balance_again != nullptr);
    const auto* balance_fact_again =
        std::get_if<ores::trading::domain::balance_guaranteed_swap_instrument>(
            &balance_again->facts);
    REQUIRE(balance_fact_again != nullptr);
    CHECK(balance_fact_again->reference_security == "XS1234567890");
    REQUIRE(balance_again->tranches.size() == 1);
    CHECK(balance_again->tranches[0].security_id == "TR-1");
    REQUIRE(balance_again->tranche_notionals.size() == 1);
    CHECK(balance_again->tranche_notionals[0].notional.to_string() == "750000");

    const auto callable_again_item = rebuild(items[5]);
    const auto* callable_again =
        std::get_if<ores::trading::domain::swap_instrument_data>(&callable_again_item.instrument);
    REQUIRE(callable_again != nullptr);
    REQUIRE(callable_again->call_dates.size() == 2);
    CHECK(callable_again->call_dates[0].call_date == date_of(2025, 1, 15));
    CHECK(callable_again->call_dates[1].call_date == date_of(2026, 1, 15));
}

TEST_CASE("a_portfolio_document_projects_its_trades_and_their_envelopes", tags) {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(document_text);
    REQUIRE(document.trade.size() == 1);

    // The writing binds back to the record the reading produced.
    const auto written = portfolio_projection::write(document);
    const auto again = portfolio_projection::read(written);
    CHECK_FALSE(ores::ore::xml::compare_records(*pf::portfolio_document().root, &document, &again));

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

    const auto trade_id = boost::uuids::string_generator{}("01234567-89ab-cdef-0123-456789abcdef");

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

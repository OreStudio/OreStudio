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
#include "ores.ore.core/domain/index_reference.hpp"
#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/domain/portfolio_projection.hpp"
#include "ores.ore.core/xml/record.hpp"
#include "ores.ore.core/xml/record_diff.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.trading.api/domain/instrument_batch_mapper.hpp"
#include "test_suite.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace {

/// The carrier a scripted instrument travels in, with the rows beside its own.
const ores::trading::domain::scripted_instrument_data*
scripted_data_of(const ores::trading::domain::trade_instrument& instrument) {
    return std::get_if<ores::trading::domain::scripted_instrument_data>(&instrument);
}

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

const char* const fx_plain_document_text = R"(<Portfolio>
  <Trade id="fx-forward-1">
    <TradeType>FxForward</TradeType>
    <Envelope>
      <CounterParty>CPTY_A</CounterParty>
      <NettingSetId>NS-1</NettingSetId>
    </Envelope>
    <FxForwardData>
      <ValueDate>2024-07-15</ValueDate>
      <BoughtCurrency>EUR</BoughtCurrency>
      <BoughtAmount>1000000</BoughtAmount>
      <SoldCurrency>USD</SoldCurrency>
      <SoldAmount>1100000</SoldAmount>
      <Settlement>Physical</Settlement>
    </FxForwardData>
  </Trade>
  <Trade id="fx-swap-1">
    <TradeType>FxSwap</TradeType>
    <Envelope>
      <CounterParty>CPTY_A</CounterParty>
      <NettingSetId>NS-1</NettingSetId>
    </Envelope>
    <FxSwapData>
      <NearDate>2024-02-15</NearDate>
      <NearBoughtCurrency>USD</NearBoughtCurrency>
      <NearBoughtAmount>500000</NearBoughtAmount>
      <NearSoldCurrency>JPY</NearSoldCurrency>
      <NearSoldAmount>75000000</NearSoldAmount>
      <FarDate>2024-08-15</FarDate>
      <FarBoughtAmount>75000000</FarBoughtAmount>
      <FarSoldAmount>500000</FarSoldAmount>
    </FxSwapData>
  </Trade>
</Portfolio>)";

const char* const fx_product_document_text = R"(<Portfolio>
  <Trade id="fx-barrier-1">
    <TradeType>FxBarrierOption</TradeType>
    <Envelope>
      <CounterParty>CPTY_B</CounterParty>
      <NettingSetId>NS-2</NettingSetId>
    </Envelope>
    <FxBarrierOptionData>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Call</OptionType>
        <ExerciseDates><ExerciseDate>2024-09-15</ExerciseDate></ExerciseDates>
      </OptionData>
      <BarrierData>
        <Type>UpAndOut</Type>
        <Levels><Level>1.15</Level></Levels>
      </BarrierData>
      <BoughtCurrency>EUR</BoughtCurrency>
      <BoughtAmount>1000000</BoughtAmount>
      <SoldCurrency>USD</SoldCurrency>
      <SoldAmount>1100000</SoldAmount>
    </FxBarrierOptionData>
  </Trade>
  <Trade id="fx-touch-1">
    <TradeType>FxTouchOption</TradeType>
    <Envelope>
      <CounterParty>CPTY_C</CounterParty>
      <NettingSetId>NS-3</NettingSetId>
    </Envelope>
    <FxTouchOptionData>
      <OptionData>
        <LongShort>Long</LongShort>
        <ExerciseDates><ExerciseDate>2024-10-15</ExerciseDate></ExerciseDates>
      </OptionData>
      <BarrierData>
        <Type>DownAndOut</Type>
        <Levels><Level>0.85</Level></Levels>
      </BarrierData>
      <ForeignCurrency>EUR</ForeignCurrency>
      <DomesticCurrency>USD</DomesticCurrency>
      <PayoffCurrency>USD</PayoffCurrency>
      <PayoffAmount>100000</PayoffAmount>
    </FxTouchOptionData>
  </Trade>
</Portfolio>)";

const char* const equity_option_document_text = R"(<Portfolio>
  <Trade id="equity-option-1">
    <TradeType>EquityOption</TradeType>
    <Envelope>
      <CounterParty>CPTY_E</CounterParty>
      <NettingSetId>NS-9</NettingSetId>
    </Envelope>
    <EquityOptionData>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Call</OptionType>
        <Style>European</Style>
        <Settlement>Cash</Settlement>
        <ExerciseDates><ExerciseDate>2025-06-20</ExerciseDate></ExerciseDates>
      </OptionData>
      <Name>RIC:.SPX</Name>
      <Currency>USD</Currency>
      <Quantity>775</Quantity>
      <Strike>2800.5</Strike>
    </EquityOptionData>
  </Trade>
</Portfolio>)";

const char* const equity_position_document_text = R"(<Portfolio>
  <Trade id="equity-position-1">
    <TradeType>EquityPosition</TradeType>
    <EquityPositionData>
      <Quantity>5000</Quantity>
      <Underlying>
        <Type>Equity</Type>
        <Name>.SPX</Name>
      </Underlying>
    </EquityPositionData>
  </Trade>
  <Trade id="equity-option-position-1">
    <TradeType>EquityOptionPosition</TradeType>
    <EquityOptionPositionData>
      <Quantity>1000</Quantity>
      <Underlying>
        <Underlying>
          <Type>Equity</Type>
          <Name>.SPX</Name>
          <Weight>0.5</Weight>
        </Underlying>
        <OptionData>
          <LongShort>Long</LongShort>
          <OptionType>Call</OptionType>
          <Style>European</Style>
          <Settlement>Cash</Settlement>
        </OptionData>
        <Strike>3300</Strike>
      </Underlying>
      <Underlying>
        <Underlying>
          <Type>Equity</Type>
          <Name>.SX5E</Name>
          <Weight>0.5</Weight>
        </Underlying>
        <OptionData>
          <LongShort>Short</LongShort>
          <OptionType>Put</OptionType>
          <Style>American</Style>
          <Settlement>Physical</Settlement>
        </OptionData>
        <Strike>3400</Strike>
      </Underlying>
    </EquityOptionPositionData>
  </Trade>
</Portfolio>)";

const char* const equity_barrier_and_accumulator_document_text = R"(<Portfolio>
  <Trade id="equity-barrier-1">
    <TradeType>EquityBarrierOption</TradeType>
    <EquityBarrierOptionData>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Call</OptionType>
        <Style>European</Style>
        <Settlement>Cash</Settlement>
        <ExerciseDates><ExerciseDate>2026-08-15</ExerciseDate></ExerciseDates>
      </OptionData>
      <BarrierData>
        <Type>UpAndOut</Type>
        <Levels><Level>3500</Level></Levels>
      </BarrierData>
      <Name>RIC:.SPX</Name>
      <Currency>USD</Currency>
      <Strike>3200</Strike>
      <Quantity>1000</Quantity>
    </EquityBarrierOptionData>
  </Trade>
  <Trade id="equity-accumulator-1">
    <TradeType>EquityAccumulator</TradeType>
    <EquityAccumulatorData>
      <Currency>EUR</Currency>
      <FixingAmount>30</FixingAmount>
      <Strike>4000</Strike>
      <Underlying>
        <Type>Equity</Type>
        <Name>.STOXX50</Name>
      </Underlying>
      <OptionData>
        <LongShort>Long</LongShort>
        <PayoffType>Decumulator</PayoffType>
      </OptionData>
      <StartDate>2025-02-05</StartDate>
      <ObservationDates>
        <Rules>
          <StartDate>2025-02-05</StartDate>
          <EndDate>2026-02-05</EndDate>
          <Tenor>1D</Tenor>
          <Convention>F</Convention>
        </Rules>
      </ObservationDates>
      <RangeBounds>
        <RangeBound>
          <RangeTo>4000</RangeTo>
          <Leverage>1</Leverage>
        </RangeBound>
        <RangeBound>
          <RangeFrom>4000</RangeFrom>
          <Leverage>2</Leverage>
        </RangeBound>
      </RangeBounds>
      <Barriers>
        <BarrierData>
          <Type>DownAndOut</Type>
          <Levels><Level>3500</Level></Levels>
        </BarrierData>
        <BarrierData>
          <Type>FixingFloor</Type>
          <Levels><Level>1</Level></Levels>
        </BarrierData>
      </Barriers>
    </EquityAccumulatorData>
  </Trade>
</Portfolio>)";

const char* const commodity_swap_and_forward_document_text = R"(<Portfolio>
  <Trade id="cmd-1">
    <TradeType>CommodityForward</TradeType>
    <Envelope>
      <CounterParty>CPTY_C</CounterParty>
      <NettingSetId>NS-COM</NettingSetId>
    </Envelope>
    <CommodityForwardData>
      <Position>Long</Position>
      <Maturity>2026-03-31</Maturity>
      <Name>NGAS</Name>
      <Currency>USD</Currency>
      <Strike>3.25</Strike>
      <Quantity>1000</Quantity>
    </CommodityForwardData>
  </Trade>
  <Trade id="cmd-2">
    <TradeType>CommoditySwap</TradeType>
    <SwapData>
      <LegData>
        <LegType>CommodityFixed</LegType>
        <Payer>false</Payer>
        <Currency>USD</Currency>
        <CommodityFixedLegData>
          <Quantities><Quantity>1000</Quantity></Quantities>
          <Prices><Price>3.5</Price></Prices>
        </CommodityFixedLegData>
      </LegData>
      <LegData>
        <LegType>CommodityFloating</LegType>
        <Payer>true</Payer>
        <Currency>USD</Currency>
        <CommodityFloatingLegData>
          <Name>NGAS</Name>
          <PriceType>FutureSettlement</PriceType>
          <Quantities><Quantity>1000</Quantity></Quantities>
        </CommodityFloatingLegData>
        <ScheduleData>
          <Rules>
            <StartDate>2025-01-15</StartDate>
            <EndDate>2026-01-15</EndDate>
            <Tenor>1M</Tenor>
            <Convention>MF</Convention>
          </Rules>
        </ScheduleData>
      </LegData>
    </SwapData>
  </Trade>
</Portfolio>)";

const char* const commodity_average_price_and_basket_document_text = R"(<Portfolio>
  <Trade id="cmd-3">
    <TradeType>CommodityAveragePriceOption</TradeType>
    <CommodityAveragePriceOptionData>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Call</OptionType>
        <Style>European</Style>
        <ExerciseDates><ExerciseDate>2026-06-30</ExerciseDate></ExerciseDates>
      </OptionData>
      <Name>WTI</Name>
      <Currency>USD</Currency>
      <Quantity>500</Quantity>
      <Strike>75.5</Strike>
      <PriceType>Spot</PriceType>
      <StartDate>2025-03-01</StartDate>
      <EndDate>2026-06-30</EndDate>
      <PaymentCalendar>US-NYSE</PaymentCalendar>
      <PaymentLag>5</PaymentLag>
      <PaymentConvention>Following</PaymentConvention>
      <PricingCalendar>US-NYSE</PricingCalendar>
    </CommodityAveragePriceOptionData>
  </Trade>
  <Trade id="cmd-4">
    <TradeType>CommodityBasketOption</TradeType>
    <CommodityBasketOptionData>
      <Currency>EUR</Currency>
      <Notional>100</Notional>
      <Strike>50</Strike>
      <Underlyings>
        <Underlying>
          <Type>Commodity</Type>
          <Name>GOLD</Name>
          <Weight>0.6</Weight>
        </Underlying>
        <Underlying>
          <Type>Commodity</Type>
          <Name>SILVER</Name>
          <Weight>0.4</Weight>
        </Underlying>
      </Underlyings>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Put</OptionType>
        <Style>American</Style>
        <ExerciseDates><ExerciseDate>2026-12-31</ExerciseDate></ExerciseDates>
      </OptionData>
    </CommodityBasketOptionData>
  </Trade>
</Portfolio>)";

const char* const composite_document_text = R"(<Portfolio>
  <Trade id="composite-1">
    <TradeType>CompositeTrade</TradeType>
    <Envelope>
      <CounterParty>CPTY_C</CounterParty>
      <NettingSetId>NS-3</NettingSetId>
    </Envelope>
    <CompositeTradeData>
      <Currency>USD</Currency>
      <BasketName>ACME-BASKET</BasketName>
      <Components>
        <Trade id="component-swap"><TradeType>Swap</TradeType></Trade>
        <SubTrade id="component-fx"><SubTradeType>FxForward</SubTradeType></SubTrade>
      </Components>
    </CompositeTradeData>
  </Trade>
  <Trade id="trs-1">
    <TradeType>TotalReturnSwap</TradeType>
    <TotalReturnSwapData>
      <UnderlyingData>
        <Trade id="trs-underlying"><TradeType>EquityPosition</TradeType></Trade>
      </UnderlyingData>
      <ReturnData>
        <Payer>Client</Payer>
        <Currency>USD</Currency>
        <ScheduleData/>
      </ReturnData>
    </TotalReturnSwapData>
  </Trade>
  <Trade id="cfd-1">
    <TradeType>ContractForDifference</TradeType>
    <ContractForDifferenceData>
      <UnderlyingData>
        <SubTrade id="cfd-underlying"><SubTradeType>Swap</SubTradeType></SubTrade>
      </UnderlyingData>
      <ReturnData>
        <Payer>Client</Payer>
        <Currency>EUR</Currency>
        <ScheduleData/>
      </ReturnData>
    </ContractForDifferenceData>
  </Trade>
</Portfolio>)";

const char* const required_terms_document_text = R"(<Portfolio>
  <Trade id="scripted-bare-1">
    <TradeType>ScriptedTrade</TradeType>
    <ScriptedTradeData>
      <ScriptName>heston</ScriptName>
      <Script><NPV>Option</NPV><Code>Payoff = 1;</Code></Script>
      <Data/>
    </ScriptedTradeData>
  </Trade>
  <Trade id="double-digital-1">
    <TradeType>DoubleDigitalOption</TradeType>
    <DoubleDigitalOptionData>
      <Expiry>2027-07-24</Expiry>
      <Settlement>2027-07-26</Settlement>
      <BinaryPayout>1000000</BinaryPayout>
      <BinaryLevel1>1.2</BinaryLevel1>
      <BinaryLevel2>0.009</BinaryLevel2>
      <Type1>Call</Type1>
      <Type2>Put</Type2>
      <Position>Long</Position>
      <Underlying1><Type>FX</Type><Name>TR20H-EUR-USD</Name></Underlying1>
      <Underlying2><Type>Commodity</Type><Name>NYMEX:CL</Name></Underlying2>
      <PayCcy>USD</PayCcy>
    </DoubleDigitalOptionData>
  </Trade>
  <Trade id="cliquet-1">
    <TradeType>EquityCliquetOption</TradeType>
    <EquityCliquetOptionData>
      <Name>RIC:.SPX</Name>
      <Currency>USD</Currency>
      <Notional>1000</Notional>
      <LongShort>Long</LongShort>
      <OptionType>Call</OptionType>
      <Moneyness>1</Moneyness>
      <ScheduleData/>
    </EquityCliquetOptionData>
  </Trade>
  <Trade id="digital-1">
    <TradeType>EquityDigitalOption</TradeType>
    <EquityDigitalOptionData>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Call</OptionType>
        <Style>European</Style>
        <ExerciseDates><ExerciseDate>2026-07-17</ExerciseDate></ExerciseDates>
      </OptionData>
      <Strike>3300</Strike>
      <PayoffAmount>1000</PayoffAmount>
      <Name>RIC:.SPX</Name>
      <Quantity>1000</Quantity>
    </EquityDigitalOptionData>
  </Trade>
  <Trade id="worst-of-1">
    <TradeType>EquityWorstOfBasketSwap</TradeType>
    <EquityWorstOfBasketSwapData>
      <LongShort>Long</LongShort>
      <Currency>USD</Currency>
      <Quantity>1000</Quantity>
      <Underlyings>
        <Underlying><Type>Equity</Type><Name>RIC:.SPX</Name></Underlying>
        <Underlying><Type>Equity</Type><Name>RIC:.NDX</Name></Underlying>
      </Underlyings>
      <InitialPrices>
        <InitialPrice>3200</InitialPrice>
        <InitialPrice>11500</InitialPrice>
      </InitialPrices>
      <FloatingPeriodSchedule>
        <Rules>
          <StartDate>2025-02-11</StartDate>
          <Tenor>6M</Tenor>
          <Convention>MF</Convention>
        </Rules>
      </FloatingPeriodSchedule>
      <FloatingPayDates>
        <Rules>
          <StartDate>2025-02-11</StartDate>
          <Tenor>6M</Tenor>
          <Convention>MF</Convention>
        </Rules>
      </FloatingPayDates>
      <KnockOutLevels><KnockOutLevel>3500</KnockOutLevel></KnockOutLevels>
      <FixedTriggerLevels><FixedTriggerLevel>3400</FixedTriggerLevel></FixedTriggerLevels>
      <FixedRate>0.01</FixedRate>
      <FloatingIndex>USD-LIBOR-3M</FloatingIndex>
      <FloatingDayCountFraction>Actual/360</FloatingDayCountFraction>
    </EquityWorstOfBasketSwapData>
  </Trade>
</Portfolio>)";

auto date_of(int year, unsigned month, unsigned day) -> std::chrono::year_month_day {
    return {std::chrono::year{year}, std::chrono::month{month}, std::chrono::day{day}};
}

/**
 * @brief The document the batch rebuilds from one trade's instrument.
 *
 * The rows a family stores are what the export reads, so the instrument is
 * stamped with one trade id and handed to the batch the way an import does.
 */
auto exported(const ores::ore::xml::trade_import_item& item) -> portfolio_projection::document {
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

    return portfolio_projection::reverse({export_item}, batch);
}

/// The trade the batch rebuilds from its instrument, read back again.
auto rebuild(const ores::ore::xml::trade_import_item& item) -> ores::ore::xml::trade_import_item {
    const auto back = portfolio_projection::map(exported(item));
    if (back.empty())
        return {};
    return back.front();
}

/// The rows of one owner, so a test about one schedule ignores the legs' own.
template <typename Row>
std::vector<const Row*> owned_by(const std::vector<Row>& rows, std::string_view owner_role) {
    std::vector<const Row*> out;
    for (const auto& row : rows)
        if (row.owner_role == owner_role)
            out.push_back(&row);
    return out;
}

}

TEST_CASE("a_portfolio_document_projects_the_legs_of_a_vanilla_swap",
          "[ore][xml][portfolio][projection]") {
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
    CHECK(instrument->legs[1].floating_index_code ==
          "oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR&tenor=3M");

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
    CHECK(round->legs[1].floating_index_code ==
          "oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR&tenor=3M");
    REQUIRE(round->leg_amounts.size() == 2);
    CHECK(round->leg_amounts[0].amount.to_string() == "1000000");
    CHECK(round->leg_amounts[1].amount.to_string() == "1000000");
    REQUIRE(round->leg_rates.size() == 2);
    CHECK(round->leg_rates[0].value.to_string() == "0.05");
    CHECK(round->leg_rates[1].value.to_string() == "0.001");
}

TEST_CASE("a_portfolio_document_projects_a_cap_floor_and_its_strike",
          "[ore][xml][portfolio][projection]") {
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

TEST_CASE("a_portfolio_document_projects_a_swaption_and_its_option",
          "[ore][xml][portfolio][projection]") {
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

    const auto option_schedules = owned_by(instrument->schedules, "option");
    REQUIRE(option_schedules.size() == 1);
    CHECK(option_schedules[0]->schedule_role == "exercise_dates");
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

TEST_CASE("a_portfolio_document_projects_the_remaining_rates_products",
          "[ore][xml][portfolio][projection]") {
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
    CHECK(fra_fact->rate_index == "oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR&tenor=6M");
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
    CHECK(owned_by(balance->schedules, "tranches").size() == 1);

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
    CHECK(fra_fact_again->rate_index ==
          "oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR&tenor=6M");
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

TEST_CASE("a_portfolio_document_projects_an_inflation_swap_on_a_year_on_year_leg",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const char* const text = R"(<Portfolio>
  <Trade id="yy-1">
    <TradeType>InflationSwap</TradeType>
    <Envelope>
      <CounterParty>CPTY_A</CounterParty>
      <NettingSetId>NS-1</NettingSetId>
    </Envelope>
    <InflationSwapData>
      <LegData>
        <Payer>false</Payer>
        <LegType>YY</LegType>
        <Currency>EUR</Currency>
        <Notionals><Notional>1000000</Notional></Notionals>
        <DayCounter>ACT/ACT</DayCounter>
        <ScheduleData>
          <Rules>
            <StartDate>2024-01-15</StartDate>
            <EndDate>2034-01-15</EndDate>
            <Tenor>1Y</Tenor>
            <Convention>MF</Convention>
          </Rules>
        </ScheduleData>
        <YYLegData>
          <Index>EUHICPXT</Index>
          <FixingDays>2</FixingDays>
          <ObservationLag>3M</ObservationLag>
        </YYLegData>
      </LegData>
    </InflationSwapData>
  </Trade>
</Portfolio>)";

    // The swap's fact states the index and lag the leg states, so the row the
    // table refuses with no index is never written for a year-on-year swap.
    const auto check = [](const auto& items) {
        REQUIRE(items.size() == 1);
        const auto* swap =
            std::get_if<ores::trading::domain::swap_instrument_data>(&items[0].instrument);
        REQUIRE(swap != nullptr);
        const auto* fact =
            std::get_if<ores::trading::domain::inflation_swap_instrument>(&swap->facts);
        REQUIRE(fact != nullptr);
        CHECK(fact->inflation_index_code == "EUHICPXT");
        CHECK(fact->lag_convention == "3M");
        CHECK_FALSE(fact->base_cpi.has_value());
        REQUIRE(swap->legs.size() == 1);
        CHECK(swap->legs[0].leg_type_code == "YY");
        CHECK(swap->legs[0].observation_lag == std::optional<std::string>("3M"));
    };

    const auto items = portfolio_projection::map(portfolio_projection::read(text));
    check(items);
    const auto again = portfolio_projection::map(
        portfolio_projection::read(portfolio_projection::write(exported(items[0]))));
    check(again);
}

TEST_CASE("a_portfolio_document_projects_its_trades_and_their_envelopes",
          "[ore][xml][portfolio][projection]") {
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

    const auto* scripted = scripted_data_of(items[0].instrument);
    REQUIRE(scripted != nullptr);
    CHECK(scripted->instrument.script_name == "heston");
    CHECK(scripted->instrument.script_body == "Payoff = 1;");
    CHECK(scripted->instrument.script_npv == "Option");

    // The script's variables are rows, and its result is the column beside the
    // body rather than one of them.
    REQUIRE(scripted->parameters.size() == 2);
    CHECK(scripted->parameters[0].name == "PutCall");
    CHECK(scripted->parameters[0].declared_type == "Number");
    CHECK(scripted->parameters[0].shape == "scalar");
    CHECK(scripted->parameters[0].value == std::optional<std::string>("1"));
    CHECK(scripted->parameters[1].name == "Underlying_0");
    CHECK(scripted->parameters[1].declared_type == "Index");
    CHECK(scripted->parameters[1].value == std::optional<std::string>("SPX"));
    CHECK(scripted->parameter_values.empty());
}

TEST_CASE("a_portfolio_document_is_built_from_the_trades_and_their_instruments",
          "[ore][xml][portfolio][projection]") {
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
    auto instrument = items[0].instrument;
    ores::trading::domain::stamp_ids(instrument, trade_id);
    ores::trading::domain::append_instrument(batch, instrument);

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

    const auto* round = scripted_data_of(back[0].instrument);
    REQUIRE(round != nullptr);
    CHECK(round->instrument.script_name == "heston");
    CHECK(round->instrument.script_body == "Payoff = 1;");
    CHECK(round->instrument.script_npv == "Option");
    REQUIRE(round->parameters.size() == 2);
    CHECK(round->parameters[0].name == "PutCall");
    CHECK(round->parameters[0].value == std::optional<std::string>("1"));
    CHECK(round->parameters[1].name == "Underlying_0");
    CHECK(round->parameters[1].value == std::optional<std::string>("SPX"));
}

TEST_CASE("a_portfolio_document_projects_an_fx_forward_and_an_fx_swap",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(fx_plain_document_text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 2);

    const auto* forward =
        std::get_if<ores::trading::domain::fx_instrument_data>(&items[0].instrument);
    REQUIRE(forward != nullptr);
    CHECK(items[0].anchor.trade_type == "FxForward");
    REQUIRE(forward->forwards.size() == 1);
    CHECK(forward->forwards[0].identity.trade_type_code == "FxForward");
    CHECK(forward->forwards[0].audit.modified_by == "ores");
    CHECK(forward->forwards[0].value_date == date_of(2024, 7, 15));
    CHECK(forward->forwards[0].bought_currency == "EUR");
    CHECK(forward->forwards[0].bought_amount.to_string() == "1000000");
    CHECK(forward->forwards[0].sold_currency == "USD");
    CHECK(forward->forwards[0].sold_amount.to_string() == "1100000");
    CHECK(forward->forwards[0].settlement == "Physical");

    const auto* swap = std::get_if<ores::trading::domain::fx_instrument_data>(&items[1].instrument);
    REQUIRE(swap != nullptr);
    CHECK(items[1].anchor.trade_type == "FxSwap");
    REQUIRE(swap->forwards.size() == 1);
    CHECK(swap->forwards[0].identity.trade_type_code == "FxSwap");
    CHECK(swap->forwards[0].value_date == date_of(2024, 2, 15));
    CHECK(swap->forwards[0].bought_currency == "USD");
    CHECK(swap->forwards[0].bought_amount.to_string() == "500000");
    CHECK(swap->forwards[0].sold_currency == "JPY");
    CHECK(swap->forwards[0].sold_amount.to_string() == "75000000");

    const auto forward_again = rebuild(items[0]);
    const auto* forward_round =
        std::get_if<ores::trading::domain::fx_instrument_data>(&forward_again.instrument);
    REQUIRE(forward_round != nullptr);
    CHECK(forward_again.anchor.trade_type == "FxForward");
    REQUIRE(forward_round->forwards.size() == 1);
    CHECK(forward_round->forwards[0].value_date == date_of(2024, 7, 15));
    CHECK(forward_round->forwards[0].bought_currency == "EUR");
    CHECK(forward_round->forwards[0].bought_amount.to_string() == "1000000");
    CHECK(forward_round->forwards[0].sold_currency == "USD");
    CHECK(forward_round->forwards[0].sold_amount.to_string() == "1100000");
    CHECK(forward_round->forwards[0].settlement == "Physical");

    const auto swap_again = rebuild(items[1]);
    const auto* swap_round =
        std::get_if<ores::trading::domain::fx_instrument_data>(&swap_again.instrument);
    REQUIRE(swap_round != nullptr);
    CHECK(swap_again.anchor.trade_type == "FxSwap");
    REQUIRE(swap_round->forwards.size() == 1);
    CHECK(swap_round->forwards[0].value_date == date_of(2024, 2, 15));
    CHECK(swap_round->forwards[0].bought_currency == "USD");
    CHECK(swap_round->forwards[0].bought_amount.to_string() == "500000");
    CHECK(swap_round->forwards[0].sold_currency == "JPY");
    CHECK(swap_round->forwards[0].sold_amount.to_string() == "75000000");
}

TEST_CASE("a_portfolio_document_projects_an_fx_barrier_option_and_an_fx_touch_option",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(fx_product_document_text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 2);

    const auto* barrier =
        std::get_if<ores::trading::domain::fx_instrument_data>(&items[0].instrument);
    REQUIRE(barrier != nullptr);
    CHECK(items[0].anchor.trade_type == "FxBarrierOption");
    REQUIRE(barrier->barrier_options.size() == 1);
    const auto& barrier_row = barrier->barrier_options[0];
    CHECK(barrier_row.identity.trade_type_code == "FxBarrierOption");
    CHECK(barrier_row.option_type == "Call");
    CHECK(barrier_row.expiry_date == date_of(2024, 9, 15));
    CHECK(barrier_row.bought_currency == "EUR");
    CHECK(barrier_row.bought_amount.to_string() == "1000000");
    CHECK(barrier_row.sold_currency == "USD");
    CHECK(barrier_row.sold_amount.to_string() == "1100000");
    CHECK(barrier_row.barrier_type == "UpAndOut");
    // An Up style engages the upper level, so the lower column stays absent.
    CHECK_FALSE(barrier_row.lower_barrier.has_value());
    REQUIRE(barrier_row.upper_barrier.has_value());
    CHECK(barrier_row.upper_barrier->to_string() == "1.15");

    const auto* touch =
        std::get_if<ores::trading::domain::fx_instrument_data>(&items[1].instrument);
    REQUIRE(touch != nullptr);
    CHECK(items[1].anchor.trade_type == "FxTouchOption");
    REQUIRE(touch->digital_options.size() == 1);
    const auto& touch_row = touch->digital_options[0];
    CHECK(touch_row.identity.trade_type_code == "FxTouchOption");
    CHECK(touch_row.long_short == "Long");
    CHECK(touch_row.foreign_currency == "EUR");
    CHECK(touch_row.domestic_currency == "USD");
    CHECK(touch_row.payoff_currency == "USD");
    CHECK(touch_row.payoff_amount.to_string() == "100000");
    CHECK(touch_row.expiry_date == date_of(2024, 10, 15));
    CHECK(touch_row.barrier_type == "DownAndOut");
    REQUIRE(touch_row.lower_barrier.has_value());
    CHECK(touch_row.lower_barrier->to_string() == "0.85");
    CHECK_FALSE(touch_row.upper_barrier.has_value());

    const auto barrier_again = rebuild(items[0]);
    const auto* barrier_round =
        std::get_if<ores::trading::domain::fx_instrument_data>(&barrier_again.instrument);
    REQUIRE(barrier_round != nullptr);
    CHECK(barrier_again.anchor.trade_type == "FxBarrierOption");
    REQUIRE(barrier_round->barrier_options.size() == 1);
    CHECK(barrier_round->barrier_options[0].option_type == "Call");
    CHECK(barrier_round->barrier_options[0].expiry_date == date_of(2024, 9, 15));
    CHECK(barrier_round->barrier_options[0].bought_currency == "EUR");
    CHECK(barrier_round->barrier_options[0].bought_amount.to_string() == "1000000");
    CHECK(barrier_round->barrier_options[0].sold_currency == "USD");
    CHECK(barrier_round->barrier_options[0].sold_amount.to_string() == "1100000");
    CHECK(barrier_round->barrier_options[0].barrier_type == "UpAndOut");
    CHECK_FALSE(barrier_round->barrier_options[0].lower_barrier.has_value());
    REQUIRE(barrier_round->barrier_options[0].upper_barrier.has_value());
    CHECK(barrier_round->barrier_options[0].upper_barrier->to_string() == "1.15");

    const auto touch_again = rebuild(items[1]);
    const auto* touch_round =
        std::get_if<ores::trading::domain::fx_instrument_data>(&touch_again.instrument);
    REQUIRE(touch_round != nullptr);
    CHECK(touch_again.anchor.trade_type == "FxTouchOption");
    REQUIRE(touch_round->digital_options.size() == 1);
    CHECK(touch_round->digital_options[0].long_short == "Long");
    CHECK(touch_round->digital_options[0].foreign_currency == "EUR");
    CHECK(touch_round->digital_options[0].domestic_currency == "USD");
    CHECK(touch_round->digital_options[0].payoff_currency == "USD");
    CHECK(touch_round->digital_options[0].payoff_amount.to_string() == "100000");
    CHECK(touch_round->digital_options[0].expiry_date == date_of(2024, 10, 15));
    CHECK(touch_round->digital_options[0].barrier_type == "DownAndOut");
    REQUIRE(touch_round->digital_options[0].lower_barrier.has_value());
    CHECK(touch_round->digital_options[0].lower_barrier->to_string() == "0.85");
}

TEST_CASE("a_portfolio_document_projects_the_rest_of_the_fx_products",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const char* const text = R"(<Portfolio>
  <Trade id="fx-digital-1">
    <TradeType>FxDigitalOption</TradeType>
    <FxDigitalOptionData>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Put</OptionType>
        <ExerciseDates><ExerciseDate>2024-11-15</ExerciseDate></ExerciseDates>
      </OptionData>
      <Strike>1.075</Strike>
      <PayoffAmount>250000</PayoffAmount>
      <ForeignCurrency>EUR</ForeignCurrency>
      <DomesticCurrency>USD</DomesticCurrency>
    </FxDigitalOptionData>
  </Trade>
  <Trade id="fx-kiko-1">
    <TradeType>FxKIKOBarrierOption</TradeType>
    <FxKIKOBarrierOptionData>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Call</OptionType>
        <ExerciseDates><ExerciseDate>2024-12-15</ExerciseDate></ExerciseDates>
      </OptionData>
      <Barriers>
        <BarrierData><Type>DownAndIn</Type><Levels><Level>0.90</Level></Levels></BarrierData>
        <BarrierData><Type>KnockOut</Type><Levels><Level>1.10</Level></Levels></BarrierData>
      </Barriers>
      <BoughtCurrency>EUR</BoughtCurrency>
      <BoughtAmount>1000000</BoughtAmount>
      <SoldCurrency>USD</SoldCurrency>
      <SoldAmount>1100000</SoldAmount>
    </FxKIKOBarrierOptionData>
  </Trade>
  <Trade id="fx-accumulator-1">
    <TradeType>FxAccumulator</TradeType>
    <FxAccumulatorData>
      <Currency>USD</Currency>
      <FixingAmount>1000</FixingAmount>
      <Strike>1.05</Strike>
      <Underlying><Type>FX</Type><Name>EUR-USD</Name></Underlying>
      <OptionData><LongShort>Short</LongShort></OptionData>
      <StartDate>2024-01-20</StartDate>
      <ObservationDates>
        <Rules>
          <StartDate>2024-01-20</StartDate>
          <Tenor>1D</Tenor>
          <Convention>MF</Convention>
        </Rules>
      </ObservationDates>
      <RangeBounds>
        <RangeBound startDate="2024-01-20">
          <RangeFrom>1.00</RangeFrom>
          <RangeTo>1.10</RangeTo>
          <Leverage>1.05</Leverage>
          <Strike>1.05</Strike>
          <StrikeAdjustment>0.001</StrikeAdjustment>
        </RangeBound>
      </RangeBounds>
      <Barriers>
        <BarrierData><Type>UpAndOut</Type><Levels><Level>1.20</Level></Levels></BarrierData>
      </Barriers>
    </FxAccumulatorData>
  </Trade>
  <Trade id="fx-variance-1">
    <TradeType>FxVarianceSwap</TradeType>
    <FxVarianceSwapData>
      <StartDate>2024-01-15</StartDate>
      <EndDate>2025-01-15</EndDate>
      <Currency>USD</Currency>
      <Name>EUR-USD</Name>
      <LongShort>Long</LongShort>
      <Strike>0.08</Strike>
      <Notional>2000000</Notional>
      <Calendar>TARGET</Calendar>
      <MomentType>Variance</MomentType>
    </FxVarianceSwapData>
  </Trade>
</Portfolio>)";

    const auto document = portfolio_projection::read(text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 4);

    const auto* digital =
        std::get_if<ores::trading::domain::fx_instrument_data>(&items[0].instrument);
    REQUIRE(digital != nullptr);
    REQUIRE(digital->digital_options.size() == 1);
    CHECK(digital->digital_options[0].option_type == "Put");
    CHECK(digital->digital_options[0].expiry_date == date_of(2024, 11, 15));
    CHECK(digital->digital_options[0].foreign_currency == "EUR");
    CHECK(digital->digital_options[0].domestic_currency == "USD");
    // The payoff defaults to the foreign currency, which is what the schema
    // states it in when the document names none.
    CHECK(digital->digital_options[0].payoff_currency == "EUR");
    CHECK(digital->digital_options[0]
              .strike.value_or(ores::utility::decimal::decimal{})
              .to_string() == "1.075");
    CHECK(digital->digital_options[0].payoff_amount.to_string() == "250000");

    const auto* kiko = std::get_if<ores::trading::domain::fx_instrument_data>(&items[1].instrument);
    REQUIRE(kiko != nullptr);
    REQUIRE(kiko->barrier_options.size() == 1);
    CHECK(kiko->barrier_options[0].option_type == "Call");
    CHECK(kiko->barrier_options[0].barrier_type == "DownAndIn");
    REQUIRE(kiko->barrier_options[0].lower_barrier.has_value());
    CHECK(kiko->barrier_options[0].lower_barrier->to_string() == "0.9");
    REQUIRE(kiko->barrier_options[0].upper_barrier.has_value());
    CHECK(kiko->barrier_options[0].upper_barrier->to_string() == "1.1");

    const auto* accumulator =
        std::get_if<ores::trading::domain::fx_instrument_data>(&items[2].instrument);
    REQUIRE(accumulator != nullptr);
    REQUIRE(accumulator->accumulators.size() == 1);
    const auto& accumulator_row = accumulator->accumulators[0];
    CHECK(accumulator_row.currency == "USD");
    CHECK(ores::ore::domain::underlying_name_of(accumulator_row.underlying_code) == "EUR-USD");
    CHECK(accumulator_row.fixing_amount.to_string() == "1000");
    CHECK(accumulator_row.strike.to_string() == "1.05");
    // The mapper states the accumulator's direction as long without reading the
    // document's own, so the document's Short does not reach the row.
    CHECK(accumulator_row.long_short == "Long");
    CHECK(accumulator_row.start_date == date_of(2024, 1, 20));
    REQUIRE(accumulator_row.knock_out_barrier.has_value());
    CHECK(accumulator_row.knock_out_barrier->to_string() == "1.2");
    REQUIRE(accumulator->range_bounds.size() == 1);
    CHECK(accumulator->range_bounds[0].sequence_number == 1);
    CHECK(accumulator->range_bounds[0].start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 1, 20)));
    CHECK(accumulator->range_bounds[0].range_from->to_string() == "1");
    CHECK(accumulator->range_bounds[0].range_to->to_string() == "1.1");
    CHECK(accumulator->range_bounds[0].leverage == std::optional<double>(1.05));
    CHECK(accumulator->range_bounds[0].strike_adjustment->to_string() == "0.001");

    const auto* variance =
        std::get_if<ores::trading::domain::fx_instrument_data>(&items[3].instrument);
    REQUIRE(variance != nullptr);
    REQUIRE(variance->variance_swaps.size() == 1);
    CHECK(ores::ore::domain::underlying_name_of(variance->variance_swaps[0].underlying_code) ==
          "EUR-USD");
    CHECK(variance->variance_swaps[0].start_date == date_of(2024, 1, 15));
    CHECK(variance->variance_swaps[0].end_date == date_of(2025, 1, 15));
    CHECK(variance->variance_swaps[0].currency == "USD");
    CHECK(variance->variance_swaps[0].strike == 0.08);
    CHECK(variance->variance_swaps[0].notional.to_string() == "2000000");
    CHECK(variance->variance_swaps[0].moment_type == "Variance");

    const auto digital_again = rebuild(items[0]);
    const auto* digital_round =
        std::get_if<ores::trading::domain::fx_instrument_data>(&digital_again.instrument);
    REQUIRE(digital_round != nullptr);
    REQUIRE(digital_round->digital_options.size() == 1);
    CHECK(digital_round->digital_options[0].option_type == "Put");
    CHECK(digital_round->digital_options[0].expiry_date == date_of(2024, 11, 15));
    CHECK(digital_round->digital_options[0].foreign_currency == "EUR");
    CHECK(digital_round->digital_options[0].payoff_currency == "EUR");
    CHECK(digital_round->digital_options[0].payoff_amount.to_string() == "250000");
    CHECK(digital_round->digital_options[0]
              .strike.value_or(ores::utility::decimal::decimal{})
              .to_string() == "1.075");

    const auto kiko_again = rebuild(items[1]);
    const auto* kiko_round =
        std::get_if<ores::trading::domain::fx_instrument_data>(&kiko_again.instrument);
    REQUIRE(kiko_round != nullptr);
    REQUIRE(kiko_round->barrier_options.size() == 1);
    CHECK(kiko_round->barrier_options[0].barrier_type == "DownAndIn");
    CHECK(kiko_round->barrier_options[0].lower_barrier->to_string() == "0.9");
    CHECK(kiko_round->barrier_options[0].upper_barrier->to_string() == "1.1");

    const auto accumulator_again = rebuild(items[2]);
    const auto* accumulator_round =
        std::get_if<ores::trading::domain::fx_instrument_data>(&accumulator_again.instrument);
    REQUIRE(accumulator_round != nullptr);
    REQUIRE(accumulator_round->accumulators.size() == 1);
    CHECK(ores::ore::domain::underlying_name_of(
              accumulator_round->accumulators[0].underlying_code) == "EUR-USD");
    CHECK(accumulator_round->accumulators[0].long_short == "Long");
    CHECK(accumulator_round->accumulators[0].strike.to_string() == "1.05");
    CHECK(accumulator_round->accumulators[0].knock_out_barrier->to_string() == "1.2");
    REQUIRE(accumulator_round->range_bounds.size() == 1);
    CHECK(accumulator_round->range_bounds[0].range_from->to_string() == "1");
    CHECK(accumulator_round->range_bounds[0].leverage == std::optional<double>(1.05));

    // A leverage is a multiplier held as a double, and the document it is
    // written back to states the digits it was read with.
    const auto accumulator_text = portfolio_projection::write(exported(items[2]));
    CHECK(accumulator_text.find("<Leverage>1.05</Leverage>") != std::string::npos);

    const auto variance_again = rebuild(items[3]);
    const auto* variance_round =
        std::get_if<ores::trading::domain::fx_instrument_data>(&variance_again.instrument);
    REQUIRE(variance_round != nullptr);
    REQUIRE(variance_round->variance_swaps.size() == 1);
    CHECK(ores::ore::domain::underlying_name_of(
              variance_round->variance_swaps[0].underlying_code) == "EUR-USD");
    CHECK(variance_round->variance_swaps[0].strike == 0.08);
    CHECK(variance_round->variance_swaps[0].notional.to_string() == "2000000");
    CHECK(variance_round->variance_swaps[0].end_date == date_of(2025, 1, 15));
}

TEST_CASE("a_portfolio_document_projects_a_bond_and_its_fixed_leg",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const char* const text = R"(<Portfolio>
  <Trade id="bond-1">
    <TradeType>Bond</TradeType>
    <Envelope>
      <CounterParty>CPTY_C</CounterParty>
      <NettingSetId>NS-B</NettingSetId>
    </Envelope>
    <BondData>
      <IssuerId>CPTY_C</IssuerId>
      <CreditCurveId>CPTY_C</CreditCurveId>
      <SecurityId>SECURITY_1</SecurityId>
      <ReferenceCurveId>EUR-EURIBOR-6M</ReferenceCurveId>
      <SettlementDays>2</SettlementDays>
      <Calendar>TARGET</Calendar>
      <IssueDate>2025-02-03</IssueDate>
      <BondNotional>10000000</BondNotional>
      <LegData>
        <LegType>Fixed</LegType>
        <Payer>false</Payer>
        <Currency>EUR</Currency>
        <Notionals><Notional>10000000</Notional></Notionals>
        <DayCounter>ACT/ACT</DayCounter>
        <PaymentConvention>F</PaymentConvention>
        <FixedLegData><Rates><Rate>0.05</Rate></Rates></FixedLegData>
        <ScheduleData>
          <Rules>
            <StartDate>2025-02-03</StartDate>
            <EndDate>2035-02-03</EndDate>
            <Tenor>1Y</Tenor>
            <Calendar>TARGET</Calendar>
            <Convention>F</Convention>
          </Rules>
        </ScheduleData>
      </LegData>
    </BondData>
  </Trade>
</Portfolio>)";

    const auto document = portfolio_projection::read(text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 1);
    CHECK(items[0].anchor.trade_type == "Bond");

    const auto* instrument =
        std::get_if<ores::trading::domain::bond_instrument_data>(&items[0].instrument);
    REQUIRE(instrument != nullptr);
    CHECK(instrument->instrument.identity.trade_type_code == "Bond");
    CHECK(instrument->instrument.audit.modified_by == "ores");
    CHECK(instrument->issue.security_id == "SECURITY_1");
    CHECK(instrument->issue.issuer == "CPTY_C");
    CHECK(instrument->issue.credit_curve_id == std::optional<std::string>("CPTY_C"));
    CHECK(instrument->issue.reference_curve_id == std::optional<std::string>("EUR-EURIBOR-6M"));
    CHECK(instrument->issue.settlement_days == 2);
    CHECK(instrument->issue.calendar == std::optional<std::string>("TARGET"));
    CHECK(instrument->issue.issue_date ==
          std::optional<std::chrono::year_month_day>(date_of(2025, 2, 3)));
    REQUIRE(instrument->instrument.notional.has_value());
    CHECK(instrument->instrument.notional->to_string() == "10000000");
    // The issue row repeats the first leg's notional as the face value.
    REQUIRE(instrument->issue.face_value.has_value());
    CHECK(instrument->issue.face_value->to_string() == "10000000");
    CHECK(instrument->issue.issue_id == instrument->instrument.issue_id);

    REQUIRE(instrument->bond_legs.size() == 1);
    const auto& leg = instrument->bond_legs[0];
    CHECK(leg.payer == std::optional<bool>(false));
    CHECK(leg.leg_type == std::optional<std::string>("Fixed"));
    CHECK(leg.currency == std::optional<std::string>("EUR"));
    CHECK(leg.day_counter == std::optional<std::string>("ACT/ACT"));
    CHECK(leg.payment_convention == std::optional<std::string>("F"));
    REQUIRE(leg.notionals.size() == 1);
    CHECK(leg.notionals[0].value.to_string() == "10000000");
    REQUIRE(leg.schedule.rules.size() == 1);
    CHECK(leg.schedule.rules[0].start_date == "2025-02-03");
    CHECK(leg.schedule.rules[0].end_date == std::optional<std::string>("2035-02-03"));
    CHECK(leg.schedule.rules[0].tenor == "1Y");
    CHECK(leg.schedule.rules[0].calendar == std::optional<std::string>("TARGET"));
    CHECK(leg.schedule.rules[0].convention == "F");
    REQUIRE(leg.rate.has_value());
    REQUIRE(leg.rate->fixed.has_value());
    REQUIRE(leg.rate->fixed->rates.size() == 1);
    CHECK(leg.rate->fixed->rates[0].value.to_string() == "0.05");

    const auto back = rebuild(items[0]);
    const auto* round = std::get_if<ores::trading::domain::bond_instrument_data>(&back.instrument);
    REQUIRE(round != nullptr);
    CHECK(back.anchor.trade_type == "Bond");
    CHECK(round->issue.security_id == "SECURITY_1");
    CHECK(round->issue.issuer == "CPTY_C");
    CHECK(round->issue.reference_curve_id == std::optional<std::string>("EUR-EURIBOR-6M"));
    CHECK(round->issue.settlement_days == 2);
    CHECK(round->issue.issue_date ==
          std::optional<std::chrono::year_month_day>(date_of(2025, 2, 3)));
    REQUIRE(round->instrument.notional.has_value());
    CHECK(round->instrument.notional->to_string() == "10000000");
    REQUIRE(round->bond_legs.size() == 1);
    CHECK(round->bond_legs[0].leg_type == std::optional<std::string>("Fixed"));
    CHECK(round->bond_legs[0].currency == std::optional<std::string>("EUR"));
    REQUIRE(round->bond_legs[0].notionals.size() == 1);
    CHECK(round->bond_legs[0].notionals[0].value.to_string() == "10000000");
    REQUIRE(round->bond_legs[0].rate.has_value());
    REQUIRE(round->bond_legs[0].rate->fixed.has_value());
    REQUIRE(round->bond_legs[0].rate->fixed->rates.size() == 1);
    CHECK(round->bond_legs[0].rate->fixed->rates[0].value.to_string() == "0.05");
    REQUIRE(round->bond_legs[0].schedule.rules.size() == 1);
    CHECK(round->bond_legs[0].schedule.rules[0].start_date == "2025-02-03");
    CHECK(round->bond_legs[0].schedule.rules[0].tenor == "1Y");
}

TEST_CASE("a_portfolio_document_projects_a_callable_bond_and_its_call_dates",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const char* const text = R"(<Portfolio>
  <Trade id="callable-bond-1">
    <TradeType>CallableBond</TradeType>
    <Envelope>
      <CounterParty>CPTY_C</CounterParty>
      <NettingSetId>NS-CB</NettingSetId>
    </Envelope>
    <CallableBondData>
      <BondData>
        <SecurityId>CallableBond_CALL</SecurityId>
        <BondNotional>100000000</BondNotional>
      </BondData>
      <CallData>
        <ScheduleData>
          <Dates>
            <Calendar>TARGET</Calendar>
            <Dates>
              <Date>2024-05-01</Date>
              <Date>2027-04-01</Date>
            </Dates>
          </Dates>
        </ScheduleData>
        <Styles><Style>American</Style></Styles>
        <Prices>
          <Price>1</Price>
          <Price startDate="2026-01-01">1.01</Price>
        </Prices>
        <PriceTypes><PriceType>Dirty</PriceType></PriceTypes>
        <IncludeAccruals><IncludeAccrual>false</IncludeAccrual></IncludeAccruals>
      </CallData>
    </CallableBondData>
  </Trade>
</Portfolio>)";

    const auto document = portfolio_projection::read(text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 1);
    CHECK(items[0].anchor.trade_type == "CallableBond");

    const auto* instrument =
        std::get_if<ores::trading::domain::bond_instrument_data>(&items[0].instrument);
    REQUIRE(instrument != nullptr);
    CHECK(instrument->instrument.identity.trade_type_code == "CallableBond");
    CHECK(instrument->issue.security_id == "CallableBond_CALL");
    REQUIRE(instrument->instrument.notional.has_value());
    CHECK(instrument->instrument.notional->to_string() == "100000000");
    REQUIRE(instrument->call_dates.size() == 2);
    CHECK(instrument->call_dates[0].sequence_number == 1);
    CHECK(instrument->call_dates[0].call_date == date_of(2024, 5, 1));
    CHECK(instrument->call_dates[0].issue_id == instrument->issue.issue_id);
    CHECK(instrument->call_dates[1].sequence_number == 2);
    CHECK(instrument->call_dates[1].call_date == date_of(2027, 4, 1));

    const auto back = rebuild(items[0]);
    const auto* round = std::get_if<ores::trading::domain::bond_instrument_data>(&back.instrument);
    REQUIRE(round != nullptr);
    CHECK(back.anchor.trade_type == "CallableBond");
    CHECK(round->issue.security_id == "CallableBond_CALL");
    REQUIRE(round->call_dates.size() == 2);
    CHECK(round->call_dates[0].call_date == date_of(2024, 5, 1));
    CHECK(round->call_dates[1].call_date == date_of(2027, 4, 1));

    // The written document states the call block's required lists, so it binds
    // again and the call dates survive the document.
    const auto text_again = portfolio_projection::write(exported(items[0]));
    const auto again = portfolio_projection::map(portfolio_projection::read(text_again));
    REQUIRE(again.size() == 1);
    const auto* again_instrument =
        std::get_if<ores::trading::domain::bond_instrument_data>(&again[0].instrument);
    REQUIRE(again_instrument != nullptr);
    REQUIRE(again_instrument->call_dates.size() == 2);
    CHECK(again_instrument->call_dates[0].call_date == date_of(2024, 5, 1));
    CHECK(again_instrument->call_dates[1].call_date == date_of(2027, 4, 1));

    // Each call keeps the terms the document states for its date: the dated
    // price applies from 2026, so the first call is at par and the second is
    // not, and no term is replaced by a stand-in on the way out.
    for (const auto* calls : {&instrument->call_dates, &again_instrument->call_dates}) {
        CHECK((*calls)[0].style == "American");
        CHECK((*calls)[1].style == "American");
        CHECK((*calls)[0].price.to_string() == "1");
        CHECK((*calls)[1].price.to_string() == "1.01");
        CHECK((*calls)[0].price_type == "Dirty");
        CHECK((*calls)[1].include_accrual == "false");
    }
}

TEST_CASE("a_callable_bond_whose_call_price_is_not_positive_maps_no_bond",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const char* const text = R"(<Portfolio>
  <Trade id="callable-bond-zero">
    <TradeType>CallableBond</TradeType>
    <CallableBondData>
      <BondData>
        <SecurityId>CallableBond_ZERO</SecurityId>
        <BondNotional>100</BondNotional>
      </BondData>
      <CallData>
        <ScheduleData>
          <Dates><Dates><Date>2027-04-01</Date></Dates></Dates>
        </ScheduleData>
        <Styles><Style>Bermudan</Style></Styles>
        <Prices><Price>0</Price></Prices>
        <PriceTypes><PriceType>Clean</PriceType></PriceTypes>
        <IncludeAccruals><IncludeAccrual>true</IncludeAccrual></IncludeAccruals>
      </CallData>
    </CallableBondData>
  </Trade>
</Portfolio>)";

    // The table refuses a call price that is not positive, so the reading
    // refuses it first and the trade carries no bond a store would reject.
    const auto items = portfolio_projection::map(portfolio_projection::read(text));
    REQUIRE(items.size() == 1);
    CHECK(std::holds_alternative<std::monostate>(items[0].instrument));
}

TEST_CASE("a_portfolio_document_projects_a_bond_option_and_its_redemption",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const char* const text = R"(<Portfolio>
  <Trade id="bond-option-1">
    <TradeType>BondOption</TradeType>
    <Envelope>
      <CounterParty>CPTY</CounterParty>
      <NettingSetId>NS</NettingSetId>
    </Envelope>
    <BondOptionData>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Call</OptionType>
        <Style>European</Style>
        <Settlement>Physical</Settlement>
        <ExerciseDates><ExerciseDate>2028-02-02</ExerciseDate></ExerciseDates>
      </OptionData>
      <StrikeData>
        <StrikePrice>
          <Value>1</Value>
          <Currency>EUR</Currency>
        </StrikePrice>
      </StrikeData>
      <Redemption>100.00</Redemption>
      <PriceType>Dirty</PriceType>
      <KnocksOut>false</KnocksOut>
      <BondData>
        <SecurityId>ISIN:IE00BH3SQ895</SecurityId>
        <BondNotional>10000000</BondNotional>
      </BondData>
    </BondOptionData>
  </Trade>
</Portfolio>)";

    const auto document = portfolio_projection::read(text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 1);
    CHECK(items[0].anchor.trade_type == "BondOption");

    const auto* instrument =
        std::get_if<ores::trading::domain::bond_instrument_data>(&items[0].instrument);
    REQUIRE(instrument != nullptr);
    CHECK(instrument->instrument.identity.trade_type_code == "BondOption");
    CHECK(instrument->issue.security_id == "ISIN:IE00BH3SQ895");
    REQUIRE(instrument->option.has_value());
    CHECK(instrument->option->option_type == "Call");
    REQUIRE(instrument->option_data.has_value());
    CHECK(instrument->option_data->long_short == "Long");
    CHECK(instrument->option_data->style == std::optional<std::string>("European"));
    CHECK(instrument->option_data->settlement == std::optional<std::string>("Physical"));
    REQUIRE(instrument->strike_data.has_value());
    REQUIRE(instrument->strike_data->price_value.has_value());
    CHECK(instrument->strike_data->price_value->to_string() == "1");
    CHECK(instrument->strike_data->price_currency == std::optional<std::string>("EUR"));
    CHECK(instrument->option_redemption == std::optional<std::string>("100.00"));
    CHECK(instrument->option_price_type == std::optional<std::string>("Dirty"));
    CHECK(instrument->option_knocks_out == std::optional<std::string>("false"));
    REQUIRE(instrument->option_exercise_dates.size() == 1);
    CHECK(instrument->option_exercise_dates[0] == "2028-02-02");

    const auto back = rebuild(items[0]);
    const auto* round = std::get_if<ores::trading::domain::bond_instrument_data>(&back.instrument);
    REQUIRE(round != nullptr);
    CHECK(back.anchor.trade_type == "BondOption");
    CHECK(round->issue.security_id == "ISIN:IE00BH3SQ895");
    REQUIRE(round->option.has_value());
    CHECK(round->option->option_type == "Call");
    REQUIRE(round->option_data.has_value());
    CHECK(round->option_data->long_short == "Long");
    CHECK(round->option_data->style == std::optional<std::string>("European"));
    CHECK(round->option_data->settlement == std::optional<std::string>("Physical"));
    REQUIRE(round->strike_data.has_value());
    REQUIRE(round->strike_data->price_value.has_value());
    CHECK(round->strike_data->price_value->to_string() == "1");
    CHECK(round->option_redemption == std::optional<std::string>("100.00"));
    CHECK(round->option_price_type == std::optional<std::string>("Dirty"));
    REQUIRE(round->option_exercise_dates.size() == 1);
    CHECK(round->option_exercise_dates[0] == "2028-02-02");
}

TEST_CASE("a_portfolio_document_projects_a_credit_default_swap_and_its_premium_leg",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const char* const text = R"(<Portfolio>
  <Trade id="cds-1">
    <TradeType>CreditDefaultSwap</TradeType>
    <Envelope>
      <CounterParty>CPTY_D</CounterParty>
      <NettingSetId>NS-C</NettingSetId>
    </Envelope>
    <CreditDefaultSwapData>
      <IssuerId>ACME</IssuerId>
      <CreditCurveId>ACME-SR</CreditCurveId>
      <ReferenceObligation>ACME-5Y</ReferenceObligation>
      <SettlesAccrual>Y</SettlesAccrual>
      <PaysAtDefaultTime>true</PaysAtDefaultTime>
      <ProtectionPaymentTime>atDefault</ProtectionPaymentTime>
      <ProtectionStart>2024-06-03</ProtectionStart>
      <UpfrontDate>2024-06-05</UpfrontDate>
      <UpfrontFee>1250.5</UpfrontFee>
      <FixedRecoveryRate>0.4</FixedRecoveryRate>
      <LegData>
        <Payer>true</Payer>
        <LegType>Fixed</LegType>
        <Currency>USD</Currency>
        <DayCounter>ACT/360</DayCounter>
        <PaymentConvention>MF</PaymentConvention>
        <Notionals><Notional>10000000</Notional></Notionals>
        <ScheduleData>
          <Rules>
            <StartDate>2024-06-03</StartDate>
            <EndDate>2029-06-03</EndDate>
            <Tenor>3M</Tenor>
            <Convention>MF</Convention>
          </Rules>
        </ScheduleData>
        <FixedLegData><Rates><Rate>0.01</Rate></Rates></FixedLegData>
      </LegData>
      <TradeDate>2024-06-01</TradeDate>
      <CashSettlementDays>3</CashSettlementDays>
    </CreditDefaultSwapData>
  </Trade>
</Portfolio>)";

    const auto document = portfolio_projection::read(text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 1);
    CHECK(items[0].anchor.trade_type == "CreditDefaultSwap");

    const auto* instrument =
        std::get_if<ores::trading::domain::credit_instrument_data>(&items[0].instrument);
    REQUIRE(instrument != nullptr);
    CHECK(instrument->instrument.identity.trade_type_code == "CreditDefaultSwap");
    CHECK(instrument->instrument.audit.modified_by == "ores");
    CHECK(instrument->instrument.reference_entity_id == "ACME");
    CHECK(instrument->instrument.reference_entity_id_type == "Internal");
    CHECK(instrument->instrument.start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 6, 3)));
    CHECK(instrument->instrument.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2029, 6, 3)));

    REQUIRE(instrument->credit_default_swap_instruments.size() == 1);
    const auto& fact = instrument->credit_default_swap_instruments[0];
    CHECK(fact.credit_curve_id == std::optional<std::string>("ACME-SR"));
    CHECK(fact.issuer_id == std::optional<std::string>("ACME"));
    CHECK(fact.reference_obligation == std::optional<std::string>("ACME-5Y"));
    CHECK(fact.settles_accrual == std::optional<bool>(true));
    CHECK(fact.rebates_accrual == std::nullopt);
    CHECK(fact.pays_at_default_time == std::optional<bool>(true));
    CHECK(fact.protection_payment_time == std::optional<std::string>("atDefault"));
    CHECK(fact.protection_start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 6, 3)));
    CHECK(fact.upfront_date == std::optional<std::chrono::year_month_day>(date_of(2024, 6, 5)));
    REQUIRE(fact.upfront_fee.has_value());
    CHECK(fact.upfront_fee->to_string() == "1250.5");
    CHECK(fact.recovery_rate == std::optional<double>(0.4));
    CHECK(fact.trade_date == std::optional<std::chrono::year_month_day>(date_of(2024, 6, 1)));
    CHECK(fact.cash_settlement_days == std::optional<int>(3));
    CHECK(fact.modified_by == "ores");

    REQUIRE(instrument->legs.size() == 1);
    CHECK(instrument->legs[0].leg_group == std::optional<std::string>("premium"));
    CHECK(instrument->legs[0].payer == std::optional<bool>(true));
    CHECK(instrument->legs[0].leg_type_code == "Fixed");
    CHECK(instrument->legs[0].currency == "USD");
    CHECK(instrument->legs[0].day_count_fraction_code == "ACT/360");
    CHECK(instrument->legs[0].payment_frequency_code == "Quarterly");

    REQUIRE(instrument->leg_amounts.size() == 1);
    CHECK(instrument->leg_amounts[0].leg_number == 1);
    CHECK(instrument->leg_amounts[0].amount.to_string() == "10000000");
    REQUIRE(instrument->leg_rates.size() == 1);
    CHECK(instrument->leg_rates[0].rate_role == "fixed");
    CHECK(instrument->leg_rates[0].value.to_string() == "0.01");

    const auto back = rebuild(items[0]);
    const auto* round =
        std::get_if<ores::trading::domain::credit_instrument_data>(&back.instrument);
    REQUIRE(round != nullptr);
    CHECK(back.anchor.trade_type == "CreditDefaultSwap");
    CHECK(round->instrument.reference_entity_id == "ACME");
    CHECK(round->instrument.reference_entity_id_type == "Internal");
    CHECK(round->instrument.start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 6, 3)));
    CHECK(round->instrument.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2029, 6, 3)));
    REQUIRE(round->credit_default_swap_instruments.size() == 1);
    const auto& round_fact = round->credit_default_swap_instruments[0];
    CHECK(round_fact.credit_curve_id == std::optional<std::string>("ACME-SR"));
    CHECK(round_fact.issuer_id == std::optional<std::string>("ACME"));
    CHECK(round_fact.reference_obligation == std::optional<std::string>("ACME-5Y"));
    CHECK(round_fact.settles_accrual == std::optional<bool>(true));
    CHECK(round_fact.pays_at_default_time == std::optional<bool>(true));
    CHECK(round_fact.protection_payment_time == std::optional<std::string>("atDefault"));
    REQUIRE(round_fact.upfront_fee.has_value());
    CHECK(round_fact.upfront_fee->to_string() == "1250.5");
    CHECK(round_fact.recovery_rate == std::optional<double>(0.4));
    CHECK(round_fact.cash_settlement_days == std::optional<int>(3));
    REQUIRE(round->legs.size() == 1);
    CHECK(round->legs[0].leg_group == std::optional<std::string>("premium"));
    CHECK(round->legs[0].leg_type_code == "Fixed");
    CHECK(round->legs[0].payment_frequency_code == "Quarterly");
    REQUIRE(round->leg_amounts.size() == 1);
    CHECK(round->leg_amounts[0].amount.to_string() == "10000000");
    REQUIRE(round->leg_rates.size() == 1);
    CHECK(round->leg_rates[0].value.to_string() == "0.01");
}

TEST_CASE("a_portfolio_document_projects_a_credit_linked_swap_and_its_payment_groups",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const char* const text = R"(<Portfolio>
  <Trade id="cls-1">
    <TradeType>CreditLinkedSwap</TradeType>
    <Envelope>
      <CounterParty>CPTY_D</CounterParty>
      <NettingSetId>NS-C</NettingSetId>
    </Envelope>
    <CreditLinkedSwapData>
      <CreditCurveId>ACME-SR</CreditCurveId>
      <SettlesAccrual>Y</SettlesAccrual>
      <FixedRecoveryRate>0.35</FixedRecoveryRate>
      <DefaultPaymentTime>atDefault</DefaultPaymentTime>
      <IndependentPayments>
        <LegData>
          <Payer>false</Payer>
          <LegType>Fixed</LegType>
          <Currency>USD</Currency>
          <DayCounter>ACT/360</DayCounter>
          <Notionals><Notional>5000000</Notional></Notionals>
          <ScheduleData>
            <Rules>
              <StartDate>2024-05-01</StartDate>
              <EndDate>2026-05-01</EndDate>
              <Tenor>6M</Tenor>
              <Convention>MF</Convention>
            </Rules>
          </ScheduleData>
          <FixedLegData><Rates><Rate>0.02</Rate></Rates></FixedLegData>
        </LegData>
      </IndependentPayments>
      <ContingentPayments>
        <LegData>
          <Payer>true</Payer>
          <LegType>Floating</LegType>
          <Currency>USD</Currency>
          <DayCounter>ACT/360</DayCounter>
          <Notionals><Notional>5000000</Notional></Notionals>
          <ScheduleData>
            <Rules>
              <StartDate>2024-07-01</StartDate>
              <EndDate>2029-07-01</EndDate>
              <Tenor>3M</Tenor>
              <Convention>MF</Convention>
            </Rules>
          </ScheduleData>
          <FloatingLegData>
            <Index>USD-LIBOR-3M</Index>
            <Spreads><Spread>0.0015</Spread></Spreads>
          </FloatingLegData>
        </LegData>
      </ContingentPayments>
    </CreditLinkedSwapData>
  </Trade>
</Portfolio>)";

    const auto document = portfolio_projection::read(text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 1);
    CHECK(items[0].anchor.trade_type == "CreditLinkedSwap");

    const auto* instrument =
        std::get_if<ores::trading::domain::credit_instrument_data>(&items[0].instrument);
    REQUIRE(instrument != nullptr);
    CHECK(instrument->instrument.identity.trade_type_code == "CreditLinkedSwap");
    CHECK(instrument->instrument.reference_entity_id == "ACME-SR");
    CHECK(instrument->instrument.reference_entity_id_type == "Internal");
    // The header's dates are those of the first contingent payment leg.
    CHECK(instrument->instrument.start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 7, 1)));
    CHECK(instrument->instrument.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2029, 7, 1)));

    REQUIRE(instrument->credit_linked_swap_instruments.size() == 1);
    const auto& fact = instrument->credit_linked_swap_instruments[0];
    CHECK(fact.credit_curve_id == "ACME-SR");
    CHECK(fact.settles_accrual == std::optional<bool>(true));
    CHECK(fact.recovery_rate == std::optional<double>(0.35));
    CHECK(fact.default_payment_time == std::optional<std::string>("atDefault"));

    REQUIRE(instrument->legs.size() == 2);
    CHECK(instrument->legs[0].identity.leg_number == 1);
    CHECK(instrument->legs[0].leg_group == std::optional<std::string>("independent"));
    CHECK(instrument->legs[0].leg_type_code == "Fixed");
    CHECK(instrument->legs[0].payment_frequency_code == "Semiannual");
    CHECK(instrument->legs[1].leg_group == std::optional<std::string>("contingent"));
    CHECK(instrument->legs[1].leg_type_code == "Floating");
    CHECK(instrument->legs[1].payment_frequency_code == "Quarterly");
    CHECK(instrument->legs[1].floating_index_code ==
          "oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR&tenor=3M");

    REQUIRE(instrument->leg_amounts.size() == 2);
    CHECK(instrument->leg_amounts[0].leg_number == 1);
    CHECK(instrument->leg_amounts[0].amount.to_string() == "5000000");
    CHECK(instrument->leg_amounts[1].leg_number == 2);
    REQUIRE(instrument->leg_rates.size() == 2);
    CHECK(instrument->leg_rates[0].rate_role == "fixed");
    CHECK(instrument->leg_rates[0].value.to_string() == "0.02");
    CHECK(instrument->leg_rates[1].rate_role == "spread");
    CHECK(instrument->leg_rates[1].value.to_string() == "0.0015");

    const auto back = rebuild(items[0]);
    const auto* round =
        std::get_if<ores::trading::domain::credit_instrument_data>(&back.instrument);
    REQUIRE(round != nullptr);
    CHECK(back.anchor.trade_type == "CreditLinkedSwap");
    CHECK(round->instrument.reference_entity_id == "ACME-SR");
    CHECK(round->instrument.reference_entity_id_type == "Internal");
    CHECK(round->instrument.start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 7, 1)));
    CHECK(round->instrument.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2029, 7, 1)));
    REQUIRE(round->credit_linked_swap_instruments.size() == 1);
    CHECK(round->credit_linked_swap_instruments[0].recovery_rate == std::optional<double>(0.35));
    REQUIRE(round->legs.size() == 2);
    CHECK(round->legs[0].leg_group == std::optional<std::string>("independent"));
    CHECK(round->legs[1].leg_group == std::optional<std::string>("contingent"));
    CHECK(round->legs[1].floating_index_code ==
          "oresmd://ir/USD?type=fixing&index=ibor&name=LIBOR&tenor=3M");
    REQUIRE(round->leg_rates.size() == 2);
    CHECK(round->leg_rates[1].value.to_string() == "0.0015");
}

TEST_CASE("a_portfolio_document_projects_an_index_swap_and_a_synthetic_cdo",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const char* const text = R"(<Portfolio>
  <Trade id="index-cds-1">
    <TradeType>IndexCreditDefaultSwap</TradeType>
    <Envelope>
      <CounterParty>CPTY_E</CounterParty>
      <NettingSetId>NS-I</NettingSetId>
    </Envelope>
    <IndexCreditDefaultSwapData>
      <IssuerId>ITRAXX</IssuerId>
      <CreditCurveId>ITRAXX-S28-5Y</CreditCurveId>
      <SettlesAccrual>Y</SettlesAccrual>
      <ProtectionStart>2024-03-20</ProtectionStart>
      <UpfrontFee>25000</UpfrontFee>
      <LegData>
        <Payer>true</Payer>
        <LegType>Fixed</LegType>
        <Currency>EUR</Currency>
        <DayCounter>ACT/360</DayCounter>
        <Notionals><Notional>25000000</Notional></Notionals>
        <ScheduleData>
          <Rules>
            <StartDate>2024-03-20</StartDate>
            <EndDate>2029-03-20</EndDate>
            <Tenor>3M</Tenor>
            <Convention>MF</Convention>
          </Rules>
        </ScheduleData>
        <FixedLegData><Rates><Rate>0.01</Rate></Rates></FixedLegData>
      </LegData>
      <TradeDate>2024-03-18</TradeDate>
      <CashSettlementDays>2</CashSettlementDays>
      <BasketData>
        <Name>
          <IssuerId>ACME</IssuerId>
          <CreditCurveId>ACME-SR</CreditCurveId>
          <Notional>1000000</Notional>
          <Weight>0.5</Weight>
          <Currency>EUR</Currency>
        </Name>
        <Name>
          <IssuerId>BETA</IssuerId>
          <CreditCurveId>BETA-SR</CreditCurveId>
          <Notional>1000000</Notional>
          <Weight>0.5</Weight>
          <Currency>EUR</Currency>
        </Name>
      </BasketData>
    </IndexCreditDefaultSwapData>
  </Trade>
  <Trade id="cdo-1">
    <TradeType>SyntheticCDO</TradeType>
    <Envelope>
      <CounterParty>CPTY_E</CounterParty>
      <NettingSetId>NS-I</NettingSetId>
    </Envelope>
    <CdoData>
      <Qualifier>CDO-1</Qualifier>
      <ProtectionStart>2024-01-10</ProtectionStart>
      <UpfrontDate>2024-01-12</UpfrontDate>
      <UpfrontFee>1500</UpfrontFee>
      <SettlesAccrual>N</SettlesAccrual>
      <FixedRecoveryRate>0.4</FixedRecoveryRate>
      <AttachmentPoint>0.03</AttachmentPoint>
      <DetachmentPoint>0.06</DetachmentPoint>
      <LegData>
        <Payer>false</Payer>
        <LegType>Fixed</LegType>
        <Currency>EUR</Currency>
        <DayCounter>ACT/360</DayCounter>
        <Notionals><Notional>20000000</Notional></Notionals>
        <ScheduleData>
          <Rules>
            <StartDate>2024-01-10</StartDate>
            <EndDate>2029-01-10</EndDate>
            <Tenor>3M</Tenor>
            <Convention>MF</Convention>
          </Rules>
        </ScheduleData>
        <FixedLegData><Rates><Rate>0.045</Rate></Rates></FixedLegData>
      </LegData>
      <BasketData>
        <Name>
          <IssuerId>ACME</IssuerId>
          <CreditCurveId>ACME-SR</CreditCurveId>
          <Currency>EUR</Currency>
        </Name>
      </BasketData>
    </CdoData>
  </Trade>
</Portfolio>)";

    const auto document = portfolio_projection::read(text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 2);
    CHECK(items[0].anchor.trade_type == "IndexCreditDefaultSwap");
    CHECK(items[1].anchor.trade_type == "SyntheticCDO");

    const auto* index_swap =
        std::get_if<ores::trading::domain::credit_instrument_data>(&items[0].instrument);
    REQUIRE(index_swap != nullptr);
    CHECK(index_swap->instrument.reference_entity_id == "ITRAXX-S28-5Y");
    CHECK(index_swap->instrument.reference_entity_id_type == "Internal");
    REQUIRE(index_swap->index_credit_default_swap_instruments.size() == 1);
    const auto& index_fact = index_swap->index_credit_default_swap_instruments[0];
    CHECK(index_fact.credit_curve_id == "ITRAXX-S28-5Y");
    CHECK(index_fact.issuer_id == std::optional<std::string>("ITRAXX"));
    CHECK(index_fact.settles_accrual == std::optional<bool>(true));
    REQUIRE(index_fact.upfront_fee.has_value());
    CHECK(index_fact.upfront_fee->to_string() == "25000");
    CHECK(index_fact.trade_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 3, 18)));
    CHECK(index_fact.cash_settlement_days == std::optional<int>(2));

    REQUIRE(index_swap->constituents.size() == 2);
    CHECK(index_swap->constituents[0].sequence_number == 1);
    CHECK(index_swap->constituents[0].issuer_id == "ACME");
    CHECK(index_swap->constituents[0].credit_curve_id == std::optional<std::string>("ACME-SR"));
    REQUIRE(index_swap->constituents[0].notional.has_value());
    CHECK(index_swap->constituents[0].notional->to_string() == "1000000");
    CHECK(index_swap->constituents[0].weight == std::optional<double>(0.5));
    CHECK(index_swap->constituents[0].currency == std::optional<std::string>("EUR"));
    CHECK(index_swap->constituents[1].sequence_number == 2);
    CHECK(index_swap->constituents[1].issuer_id == "BETA");

    const auto* cdo =
        std::get_if<ores::trading::domain::credit_instrument_data>(&items[1].instrument);
    REQUIRE(cdo != nullptr);
    CHECK(cdo->instrument.reference_entity_id == "CDO-1");
    CHECK(cdo->instrument.reference_entity_id_type == "Internal");
    CHECK(cdo->instrument.start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 1, 10)));
    CHECK(cdo->instrument.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2029, 1, 10)));
    REQUIRE(cdo->synthetic_cdo_instruments.size() == 1);
    const auto& cdo_fact = cdo->synthetic_cdo_instruments[0];
    CHECK(cdo_fact.qualifier == "CDO-1");
    CHECK(cdo_fact.protection_start_date == date_of(2024, 1, 10));
    REQUIRE(cdo_fact.upfront_fee.has_value());
    CHECK(cdo_fact.upfront_fee->to_string() == "1500");
    CHECK(cdo_fact.settles_accrual == std::optional<bool>(false));
    CHECK(cdo_fact.attachment_point == 0.03);
    CHECK(cdo_fact.detachment_point == 0.06);
    CHECK(cdo_fact.recovery_rate == std::optional<double>(0.4));
    REQUIRE(cdo->legs.size() == 1);
    CHECK(cdo->legs[0].leg_group == std::optional<std::string>("premium"));
    REQUIRE(cdo->constituents.size() == 1);
    CHECK(cdo->constituents[0].issuer_id == "ACME");
    CHECK(!cdo->constituents[0].notional.has_value());
    CHECK(!cdo->constituents[0].weight.has_value());

    const auto index_again = rebuild(items[0]);
    const auto* index_round =
        std::get_if<ores::trading::domain::credit_instrument_data>(&index_again.instrument);
    REQUIRE(index_round != nullptr);
    CHECK(index_again.anchor.trade_type == "IndexCreditDefaultSwap");
    REQUIRE(index_round->constituents.size() == 2);
    CHECK(index_round->constituents[0].issuer_id == "ACME");
    CHECK(index_round->constituents[0].weight == std::optional<double>(0.5));
    CHECK(index_round->constituents[1].issuer_id == "BETA");
    REQUIRE(index_round->index_credit_default_swap_instruments.size() == 1);
    REQUIRE(index_round->index_credit_default_swap_instruments[0].upfront_fee.has_value());
    CHECK(index_round->index_credit_default_swap_instruments[0].upfront_fee->to_string() ==
          "25000");

    const auto cdo_again = rebuild(items[1]);
    const auto* cdo_round =
        std::get_if<ores::trading::domain::credit_instrument_data>(&cdo_again.instrument);
    REQUIRE(cdo_round != nullptr);
    CHECK(cdo_again.anchor.trade_type == "SyntheticCDO");
    REQUIRE(cdo_round->synthetic_cdo_instruments.size() == 1);
    const auto& cdo_round_fact = cdo_round->synthetic_cdo_instruments[0];
    CHECK(cdo_round_fact.qualifier == "CDO-1");
    CHECK(cdo_round_fact.protection_start_date == date_of(2024, 1, 10));
    REQUIRE(cdo_round_fact.upfront_fee.has_value());
    CHECK(cdo_round_fact.upfront_fee->to_string() == "1500");
    CHECK(cdo_round_fact.attachment_point == 0.03);
    CHECK(cdo_round_fact.detachment_point == 0.06);
    REQUIRE(cdo_round->constituents.size() == 1);
    CHECK(cdo_round->constituents[0].issuer_id == "ACME");
}

TEST_CASE("a_portfolio_document_projects_a_risk_participation_agreement",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const char* const text = R"(<Portfolio>
  <Trade id="rpa-1">
    <TradeType>RiskParticipationAgreement</TradeType>
    <Envelope>
      <CounterParty>CPTY_F</CounterParty>
      <NettingSetId>NS-R</NettingSetId>
    </Envelope>
    <RiskParticipationAgreementData>
      <ParticipationRate>0.5</ParticipationRate>
      <ProtectionStart>2024-02-01</ProtectionStart>
      <ProtectionEnd>2029-02-01</ProtectionEnd>
      <CreditCurveId>ACME-SR</CreditCurveId>
      <IssuerId>ACME</IssuerId>
      <SettlesAccrual>true</SettlesAccrual>
      <FixedRecoveryRate>0.4</FixedRecoveryRate>
      <ProtectionFee>
        <LegData>
          <Payer>false</Payer>
          <LegType>Fixed</LegType>
          <Currency>USD</Currency>
          <DayCounter>ACT/360</DayCounter>
          <Notionals><Notional>2000000</Notional></Notionals>
          <ScheduleData>
            <Rules>
              <StartDate>2024-02-01</StartDate>
              <EndDate>2025-02-01</EndDate>
              <Tenor>3M</Tenor>
              <Convention>MF</Convention>
            </Rules>
          </ScheduleData>
          <FixedLegData><Rates><Rate>0.03</Rate></Rates></FixedLegData>
        </LegData>
      </ProtectionFee>
      <Underlying>
        <OptionData>
          <LongShort>Long</LongShort>
          <Style>European</Style>
          <ExerciseDates><ExerciseDate>2029-02-01</ExerciseDate></ExerciseDates>
        </OptionData>
        <NakedOption>false</NakedOption>
        <LegData>
          <Payer>true</Payer>
          <LegType>Cashflow</LegType>
          <Currency>USD</Currency>
          <DayCounter>ACT/360</DayCounter>
          <CashflowData>
            <Cashflow><Amount date="2029-02-01">250000</Amount></Cashflow>
          </CashflowData>
        </LegData>
      </Underlying>
    </RiskParticipationAgreementData>
  </Trade>
</Portfolio>)";

    const auto document = portfolio_projection::read(text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 1);
    CHECK(items[0].anchor.trade_type == "RiskParticipationAgreement");

    const auto* instrument =
        std::get_if<ores::trading::domain::credit_instrument_data>(&items[0].instrument);
    REQUIRE(instrument != nullptr);
    CHECK(instrument->instrument.identity.trade_type_code == "RiskParticipationAgreement");
    CHECK(instrument->instrument.reference_entity_id == "ACME");
    CHECK(instrument->instrument.reference_entity_id_type == "Internal");
    CHECK(instrument->instrument.start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 2, 1)));
    CHECK(instrument->instrument.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2029, 2, 1)));

    REQUIRE(instrument->risk_participation_agreement_instruments.size() == 1);
    const auto& fact = instrument->risk_participation_agreement_instruments[0];
    CHECK(fact.participation_rate == 0.5);
    CHECK(fact.credit_curve_id == "ACME-SR");
    CHECK(fact.issuer_id == std::optional<std::string>("ACME"));
    CHECK(fact.settles_accrual == std::optional<bool>(true));
    CHECK(fact.recovery_rate == std::optional<double>(0.4));
    CHECK(fact.naked_option == std::optional<bool>(false));

    REQUIRE(instrument->legs.size() == 2);
    CHECK(instrument->legs[0].leg_group == std::optional<std::string>("protection_fee"));
    CHECK(instrument->legs[0].leg_type_code == "Fixed");
    CHECK(instrument->legs[0].payment_frequency_code == "Quarterly");
    CHECK(instrument->legs[1].leg_group == std::optional<std::string>("underlying"));
    CHECK(instrument->legs[1].leg_type_code == "Cashflow");
    REQUIRE(instrument->leg_amounts.size() == 2);
    CHECK(instrument->leg_amounts[0].amount.to_string() == "2000000");
    CHECK(instrument->leg_amounts[1].amount.to_string() == "250000");
    CHECK(instrument->leg_amounts[1].start_date == date_of(2029, 2, 1));
    REQUIRE(instrument->leg_rates.size() == 1);
    CHECK(instrument->leg_rates[0].rate_role == "fixed");
    CHECK(instrument->leg_rates[0].value.to_string() == "0.03");

    // The protection fee leg keeps its own schedule, which is the period it
    // states rather than the protection period the header holds.
    REQUIRE(instrument->schedules.size() == 2);
    CHECK(instrument->schedules[0].owner_role == "swap");
    CHECK(instrument->schedules[0].owner_number == 1);
    CHECK(instrument->schedules[0].schedule_role == "schedule");
    CHECK(instrument->schedules[0].start_date == date_of(2024, 2, 1));
    CHECK(instrument->schedules[0].end_date ==
          std::optional<std::chrono::year_month_day>(date_of(2025, 2, 1)));
    CHECK(instrument->schedules[1].owner_role == "option");
    CHECK(instrument->schedules[1].schedule_role == "exercise_dates");

    REQUIRE(instrument->options.size() == 1);
    CHECK(instrument->options[0].long_short == "Long");
    CHECK(instrument->options[0].style == std::optional<std::string>("European"));
    REQUIRE(instrument->schedule_dates.size() == 1);
    CHECK(instrument->schedule_dates[0].schedule_date == date_of(2029, 2, 1));

    const auto back = rebuild(items[0]);
    const auto* round =
        std::get_if<ores::trading::domain::credit_instrument_data>(&back.instrument);
    REQUIRE(round != nullptr);
    CHECK(back.anchor.trade_type == "RiskParticipationAgreement");
    CHECK(round->instrument.reference_entity_id == "ACME");
    CHECK(round->instrument.reference_entity_id_type == "Internal");
    CHECK(round->instrument.start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 2, 1)));
    CHECK(round->instrument.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2029, 2, 1)));
    REQUIRE(round->risk_participation_agreement_instruments.size() == 1);
    const auto& round_fact = round->risk_participation_agreement_instruments[0];
    CHECK(round_fact.participation_rate == 0.5);
    CHECK(round_fact.credit_curve_id == "ACME-SR");
    CHECK(round_fact.issuer_id == std::optional<std::string>("ACME"));
    CHECK(round_fact.settles_accrual == std::optional<bool>(true));
    CHECK(round_fact.recovery_rate == std::optional<double>(0.4));
    CHECK(round_fact.naked_option == std::optional<bool>(false));
    REQUIRE(round->legs.size() == 2);
    CHECK(round->legs[0].leg_group == std::optional<std::string>("protection_fee"));
    CHECK(round->legs[1].leg_group == std::optional<std::string>("underlying"));
    CHECK(round->legs[1].leg_type_code == "Cashflow");
    REQUIRE(round->leg_amounts.size() == 2);
    CHECK(round->leg_amounts[1].amount.to_string() == "250000");
    REQUIRE(round->leg_rates.size() == 1);
    CHECK(round->leg_rates[0].value.to_string() == "0.03");
    REQUIRE(round->schedules.size() == 2);
    CHECK(round->schedules[0].owner_number == 1);
    CHECK(round->schedules[0].end_date ==
          std::optional<std::chrono::year_month_day>(date_of(2025, 2, 1)));
    REQUIRE(round->options.size() == 1);
    CHECK(round->options[0].style == std::optional<std::string>("European"));
    REQUIRE(round->schedule_dates.size() == 1);
    CHECK(round->schedule_dates[0].schedule_date == date_of(2029, 2, 1));
}

TEST_CASE("a_portfolio_document_projects_an_index_swap_option_and_its_wrapped_swap",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const char* const text = R"(<Portfolio>
  <Trade id="index-option-1">
    <TradeType>IndexCreditDefaultSwapOption</TradeType>
    <Envelope>
      <CounterParty>CPTY_G</CounterParty>
      <NettingSetId>NS-O</NettingSetId>
    </Envelope>
    <IndexCreditDefaultSwapOptionData>
      <Strike>0.02</Strike>
      <KnockOut>Y</KnockOut>
      <IndexTerm>5Y</IndexTerm>
      <StrikeType>Spread</StrikeType>
      <TradeDate>2024-04-02</TradeDate>
      <FrontEndProtectionStartDate>2024-03-20</FrontEndProtectionStartDate>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Call</OptionType>
        <Style>European</Style>
        <ExerciseDates><ExerciseDate>2024-06-20</ExerciseDate></ExerciseDates>
      </OptionData>
      <IndexCreditDefaultSwapData>
        <IssuerId>ITRAXX</IssuerId>
        <CreditCurveId>ITRAXX-S28-5Y</CreditCurveId>
        <SettlesAccrual>Y</SettlesAccrual>
        <ProtectionStart>2024-03-20</ProtectionStart>
        <LegData>
          <Payer>false</Payer>
          <LegType>Fixed</LegType>
          <Currency>EUR</Currency>
          <DayCounter>ACT/360</DayCounter>
          <Notionals><Notional>10000000</Notional></Notionals>
          <ScheduleData>
            <Rules>
              <StartDate>2024-03-20</StartDate>
              <EndDate>2029-03-20</EndDate>
              <Tenor>3M</Tenor>
              <Convention>MF</Convention>
            </Rules>
          </ScheduleData>
          <FixedLegData><Rates><Rate>0.01</Rate></Rates></FixedLegData>
        </LegData>
        <BasketData>
          <Name>
            <IssuerId>ACME</IssuerId>
            <CreditCurveId>ACME-SR</CreditCurveId>
            <Weight>1</Weight>
          </Name>
        </BasketData>
      </IndexCreditDefaultSwapData>
    </IndexCreditDefaultSwapOptionData>
  </Trade>
</Portfolio>)";

    const auto document = portfolio_projection::read(text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 1);
    CHECK(items[0].anchor.trade_type == "IndexCreditDefaultSwapOption");

    const auto* instrument =
        std::get_if<ores::trading::domain::credit_instrument_data>(&items[0].instrument);
    REQUIRE(instrument != nullptr);
    CHECK(instrument->instrument.identity.trade_type_code == "IndexCreditDefaultSwapOption");
    CHECK(instrument->instrument.reference_entity_id == "ITRAXX-S28-5Y");
    CHECK(instrument->instrument.reference_entity_id_type == "Internal");

    REQUIRE(instrument->index_credit_default_swap_option_instruments.size() == 1);
    const auto& fact = instrument->index_credit_default_swap_option_instruments[0];
    CHECK(fact.knock_out == std::optional<bool>(true));
    CHECK(fact.index_term == std::optional<std::string>("5Y"));
    CHECK(fact.strike_type == std::optional<std::string>("Spread"));
    CHECK(fact.trade_date == std::optional<std::chrono::year_month_day>(date_of(2024, 4, 2)));
    CHECK(fact.front_end_protection_start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 3, 20)));

    REQUIRE(instrument->wrapped_index_swap.has_value());
    CHECK(instrument->wrapped_index_swap->credit_curve_id == "ITRAXX-S28-5Y");
    CHECK(instrument->wrapped_index_swap->issuer_id == std::optional<std::string>("ITRAXX"));
    CHECK(instrument->wrapped_index_swap->settles_accrual == std::optional<bool>(true));

    REQUIRE(instrument->strikes.size() == 1);
    REQUIRE(instrument->strikes[0].bare_value.has_value());
    CHECK(instrument->strikes[0].bare_value->to_string() == "0.02");
    REQUIRE(instrument->options.size() == 1);
    CHECK(instrument->options[0].long_short == "Long");
    CHECK(instrument->options[0].option_type == std::optional<std::string>("Call"));
    CHECK(instrument->options[0].style == std::optional<std::string>("European"));
    REQUIRE(instrument->schedule_dates.size() == 1);
    CHECK(instrument->schedule_dates[0].schedule_date == date_of(2024, 6, 20));
    REQUIRE(instrument->legs.size() == 1);
    CHECK(instrument->legs[0].leg_group == std::optional<std::string>("premium"));
    REQUIRE(instrument->constituents.size() == 1);
    CHECK(instrument->constituents[0].issuer_id == "ACME");
    CHECK(instrument->constituents[0].weight == std::optional<double>(1.0));

    const auto back = rebuild(items[0]);
    const auto* round =
        std::get_if<ores::trading::domain::credit_instrument_data>(&back.instrument);
    REQUIRE(round != nullptr);
    CHECK(back.anchor.trade_type == "IndexCreditDefaultSwapOption");
    CHECK(round->instrument.reference_entity_id == "ITRAXX-S28-5Y");
    CHECK(round->instrument.reference_entity_id_type == "Internal");
    REQUIRE(round->index_credit_default_swap_option_instruments.size() == 1);
    const auto& round_fact = round->index_credit_default_swap_option_instruments[0];
    CHECK(round_fact.knock_out == std::optional<bool>(true));
    CHECK(round_fact.index_term == std::optional<std::string>("5Y"));
    CHECK(round_fact.strike_type == std::optional<std::string>("Spread"));
    CHECK(round_fact.front_end_protection_start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 3, 20)));
    REQUIRE(round->wrapped_index_swap.has_value());
    CHECK(round->wrapped_index_swap->credit_curve_id == "ITRAXX-S28-5Y");
    REQUIRE(round->strikes.size() == 1);
    REQUIRE(round->strikes[0].bare_value.has_value());
    CHECK(round->strikes[0].bare_value->to_string() == "0.02");
    REQUIRE(round->options.size() == 1);
    CHECK(round->options[0].option_type == std::optional<std::string>("Call"));
    REQUIRE(round->schedule_dates.size() == 1);
    CHECK(round->schedule_dates[0].schedule_date == date_of(2024, 6, 20));
    REQUIRE(round->constituents.size() == 1);
    CHECK(round->constituents[0].issuer_id == "ACME");
    CHECK(round->constituents[0].weight == std::optional<double>(1.0));
}

TEST_CASE("a_portfolio_document_projects_an_equity_option_and_its_underlying",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(equity_option_document_text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 1);
    CHECK(items[0].anchor.trade_type == "EquityOption");
    CHECK(items[0].envelope.has_value());
    CHECK(items[0].envelope->counter_party == std::optional<std::string>("CPTY_E"));

    const auto* data =
        std::get_if<ores::trading::domain::equity_instrument_data>(&items[0].instrument);
    REQUIRE(data != nullptr);
    const auto* option =
        std::get_if<ores::trading::domain::equity_option_instrument>(&data->instrument);
    REQUIRE(option != nullptr);
    CHECK(option->identity.trade_type_code == "EquityOption");
    CHECK(ores::ore::domain::underlying_name_of(option->underlying_name) == "RIC:.SPX");
    CHECK(option->currency == "USD");
    CHECK(option->notional.to_string() == "775");
    CHECK(option->option_type == "Call");
    CHECK(option->exercise_type == "European");
    CHECK(option->expiry_date == date_of(2025, 6, 20));
    CHECK(option->strike.to_string() == "2800.5");

    const auto back = rebuild(items[0]);
    CHECK(back.anchor.trade_type == "EquityOption");
    CHECK(back.envelope.has_value());
    CHECK(back.envelope->netting_set_id == std::optional<std::string>("NS-9"));
    const auto* round_data =
        std::get_if<ores::trading::domain::equity_instrument_data>(&back.instrument);
    REQUIRE(round_data != nullptr);
    const auto* round =
        std::get_if<ores::trading::domain::equity_option_instrument>(&round_data->instrument);
    REQUIRE(round != nullptr);
    CHECK(round->identity.trade_type_code == "EquityOption");
    CHECK(ores::ore::domain::underlying_name_of(round->underlying_name) == "RIC:.SPX");
    CHECK(round->currency == "USD");
    CHECK(round->notional.to_string() == "775");
    CHECK(round->option_type == "Call");
    CHECK(round->exercise_type == "European");
    CHECK(round->expiry_date == date_of(2025, 6, 20));
    CHECK(round->strike.to_string() == "2800.5");
}

TEST_CASE("an_outperformance_option_holds_each_equity_as_a_row_and_its_prices_as_columns",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));
    using ores::utility::decimal::decimal;

    const auto document = portfolio_projection::read(R"(<Portfolio>
  <Trade id="EqOutperf">
    <TradeType>EquityOutperformanceOption</TradeType>
    <Envelope><CounterParty>CPTY</CounterParty><NettingSetId>NS</NettingSetId></Envelope>
    <EquityOutperformanceOptionData>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Call</OptionType>
        <Style>European</Style>
        <Settlement>Cash</Settlement>
        <ExerciseDates><ExerciseDate>2025-03-03</ExerciseDate></ExerciseDates>
      </OptionData>
      <Currency>USD</Currency>
      <Notional>500000</Notional>
      <Underlying1><Type>Equity</Type><Name>RIC:.SPX</Name></Underlying1>
      <Underlying2><Type>Equity</Type><Name>RIC:.NDX</Name></Underlying2>
      <InitialPrice1>11500</InitialPrice1>
      <InitialPrice2>14000</InitialPrice2>
      <StrikeReturn>0.01</StrikeReturn>
      <KnockInPrice>12000</KnockInPrice>
    </EquityOutperformanceOptionData>
  </Trade>
</Portfolio>)");
    const auto check = [](const ores::ore::xml::trade_import_item& item) {
        const auto* data =
            std::get_if<ores::trading::domain::equity_instrument_data>(&item.instrument);
        REQUIRE(data != nullptr);
        const auto* option =
            std::get_if<ores::trading::domain::equity_option_instrument>(&data->instrument);
        REQUIRE(option != nullptr);
        CHECK(option->identity.trade_type_code == "EquityOutperformanceOption");
        CHECK(ores::ore::domain::underlying_name_of(option->underlying_name) == "RIC:.SPX");
        CHECK(option->strike == *decimal::from_string("0.01"));
        CHECK(option->initial_price_1 == decimal::from_string("11500"));
        CHECK(option->initial_price_2 == decimal::from_string("14000"));
        CHECK(option->knock_in_price == decimal::from_string("12000"));
        CHECK_FALSE(option->knock_out_price.has_value());
        std::map<std::string, std::string> names;
        for (const auto& row : data->instrument_underlyings)
            names[row.underlying_role] = ores::ore::domain::underlying_name_of(row.name);
        CHECK(names == std::map<std::string, std::string>{{"underlying1", "RIC:.SPX"},
                                                          {"underlying2", "RIC:.NDX"}});
    };

    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 1);
    check(items[0]);
    check(rebuild(items[0]));
}

TEST_CASE("a_basket_and_a_rainbow_hold_their_members_option_block_and_schedule_as_rows",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(R"(<Portfolio>
  <Trade id="AsianBasketOption#1">
    <TradeType>EquityBasketOption</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <EquityBasketOptionData>
      <Currency>USD</Currency>
      <Notional>1</Notional>
      <Strike>5000</Strike>
      <Underlyings>
        <Underlying><Type>Equity</Type><Name>RIC:.SPX</Name><Weight>1.0</Weight></Underlying>
        <Underlying>
          <Type>Equity</Type><Name>RIC:.STOXX50E</Name><Currency>EUR</Currency><Weight>1.0</Weight>
        </Underlying>
      </Underlyings>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Call</OptionType>
        <PayoffType>Asian</PayoffType>
        <ExerciseDates><ExerciseDate>2020-02-15</ExerciseDate></ExerciseDates>
      </OptionData>
      <Settlement>2020-02-20</Settlement>
      <ObservationDates>
        <Rules>
          <StartDate>2019-02-05</StartDate>
          <EndDate>2020-02-05</EndDate>
          <Tenor>1M</Tenor>
          <Calendar>US</Calendar>
          <Convention>U</Convention>
          <TermConvention>U</TermConvention>
          <Rule>Forward</Rule>
        </Rules>
      </ObservationDates>
    </EquityBasketOptionData>
  </Trade>
  <Trade id="BestOfAssetOrCashRainbowOption#1">
    <TradeType>EquityRainbowOption</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <EquityRainbowOptionData>
      <Currency>USD</Currency>
      <Notional>1</Notional>
      <Strike>2000</Strike>
      <Underlyings>
        <Underlying><Type>Equity</Type><Name>RIC:.SPX</Name><Weight>1.0</Weight></Underlying>
        <Underlying>
          <Type>Equity</Type><Name>RIC:.STOXX50E</Name><Currency>EUR</Currency><Weight>1.0</Weight>
        </Underlying>
      </Underlyings>
      <OptionData>
        <LongShort>Long</LongShort>
        <PayoffType>BestOfAssetOrCash</PayoffType>
        <ExerciseDates><ExerciseDate>2020-02-15</ExerciseDate></ExerciseDates>
      </OptionData>
      <Settlement>2020-02-20</Settlement>
    </EquityRainbowOptionData>
  </Trade>
</Portfolio>)");
    const auto value_of = [](const ores::trading::domain::scripted_instrument_data& data,
                             std::string_view name) -> std::string {
        for (const auto& p : data.parameters)
            if (p.name == name)
                return p.value.value_or("");
        return "<absent>";
    };
    const auto check = [&](const ores::ore::xml::trade_import_item& item,
                           std::string_view payoff,
                           bool observed) {
        const auto* data = scripted_data_of(item.instrument);
        REQUIRE(data != nullptr);
        CHECK(value_of(*data, "Currency") == "USD");
        CHECK(value_of(*data, "Settlement") == "2020-02-20");
        CHECK_FALSE(value_of(*data, "Strike") == "<absent>");
        REQUIRE(data->instrument_underlyings.size() == 2);
        CHECK(ores::ore::domain::underlying_name_of(data->instrument_underlyings[1].name) ==
              "RIC:.STOXX50E");
        CHECK(data->instrument_underlyings[1].currency == std::optional<std::string>("EUR"));
        CHECK(data->instrument_underlyings[1].weight == 1.0);
        REQUIRE(data->options.size() == 1);
        CHECK(data->options[0].payoff_type == std::optional<std::string>(payoff));
        CHECK(ores::ore::domain::option_block::exercise_dates_of(*data).size() == 1u);
        CHECK(ores::ore::domain::option_block::has_product_schedule(*data, "observationdates") ==
              observed);
    };

    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 2);
    check(items[0], "Asian", true);
    check(rebuild(items[0]), "Asian", true);
    check(items[1], "BestOfAssetOrCash", false);
    check(rebuild(items[1]), "BestOfAssetOrCash", false);
}

TEST_CASE("a_best_entry_and_a_strike_resettable_hold_their_terms_and_observation_dates_as_rows",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(R"(<Portfolio>
  <Trade id="SCRIPTED_BEO_FX">
    <TradeType>FxBestEntryOption</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <FxBestEntryOptionData>
      <LongShort>Long</LongShort>
      <Notional>1000000</Notional>
      <Multiplier>1.0</Multiplier>
      <Strike>1.0</Strike>
      <Cap>0.2</Cap>
      <TriggerLevel>0.98</TriggerLevel>
      <ResetMinimum>0.95</ResetMinimum>
      <Underlying><Type>FX</Type><Name>ECB-EUR-USD</Name></Underlying>
      <StrikeObservationDates>
        <Dates>
          <Dates>
            <Date>2022-02-28</Date>
            <Date>2022-03-31</Date>
            <Date>2022-04-29</Date>
          </Dates>
        </Dates>
      </StrikeObservationDates>
      <ExpiryDate>2023-01-31</ExpiryDate>
      <SettlementDate>2023-02-02</SettlementDate>
      <StrikeDate>2022-01-31</StrikeDate>
      <Premium>0</Premium>
      <PremiumDate>2022-02-02</PremiumDate>
      <Currency>USD</Currency>
    </FxBestEntryOptionData>
  </Trade>
  <Trade id="SCRIPTED_SRO_FX">
    <TradeType>FxStrikeResettableOption</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <FxStrikeResettableOptionData>
      <LongShort>Long</LongShort>
      <OptionType>Call</OptionType>
      <Currency>USD</Currency>
      <Strike>1.10</Strike>
      <ResetStrike>1.08</ResetStrike>
      <Quantity>1000000</Quantity>
      <TriggerType>Down</TriggerType>
      <TriggerPrice>1.02</TriggerPrice>
      <ExpiryDate>2023-01-31</ExpiryDate>
      <SettlementDate>2023-02-02</SettlementDate>
      <Premium>0</Premium>
      <PremiumDate>2022-02-02</PremiumDate>
      <Underlying><Type>FX</Type><Name>ECB-EUR-USD</Name></Underlying>
      <ObservationDates>
        <Dates>
          <Dates>
            <Date>2022-04-29</Date>
            <Date>2022-07-29</Date>
            <Date>2022-10-31</Date>
          </Dates>
        </Dates>
      </ObservationDates>
    </FxStrikeResettableOptionData>
  </Trade>
</Portfolio>)");
    const auto value_of = [](const ores::trading::domain::scripted_instrument_data& data,
                             std::string_view name) -> std::string {
        for (const auto& p : data.parameters)
            if (p.name == name)
                return p.value.value_or("");
        return "<absent>";
    };
    const auto dates_of = [](const ores::trading::domain::scripted_instrument_data& data,
                             std::string_view role) {
        std::size_t n = 0;
        for (const auto& d : data.schedule_dates)
            if (d.schedule_role == role)
                ++n;
        return n;
    };
    const auto check_best_entry = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data = scripted_data_of(item.instrument);
        REQUIRE(data != nullptr);
        CHECK(value_of(*data, "TriggerLevel") == "0.98");
        CHECK(value_of(*data, "StrikeDate") == "2022-01-31");
        CHECK(value_of(*data, "Currency") == "USD");
        REQUIRE(data->instrument_underlyings.size() == 1);
        CHECK(data->instrument_underlyings[0].underlying_type == "FX");
        CHECK(dates_of(*data, "strikeobservationdates") == 3u);
    };
    const auto check_strike_resettable = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data = scripted_data_of(item.instrument);
        REQUIRE(data != nullptr);
        CHECK(value_of(*data, "TriggerType") == "Down");
        CHECK(value_of(*data, "ResetStrike") == "1.08");
        CHECK(value_of(*data, "OptionType") == "Call");
        REQUIRE(data->instrument_underlyings.size() == 1);
        CHECK(dates_of(*data, "observationdates") == 3u);
    };

    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 2);
    check_best_entry(items[0]);
    check_best_entry(rebuild(items[0]));
    check_strike_resettable(items[1]);
    check_strike_resettable(rebuild(items[1]));
}

TEST_CASE("a_window_and_a_generic_barrier_hold_their_barriers_schedule_and_kiko_type_as_rows",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(R"(<Portfolio>
  <Trade id="SCRIPTED_WBO_FX">
    <TradeType>FxWindowBarrierOption</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <FxWindowBarrierOptionData>
      <FixingAmount>1000000</FixingAmount>
      <Currency>USD</Currency>
      <Strike>1.08</Strike>
      <Underlying><Type>FX</Type><Name>ECB-EUR-USD</Name></Underlying>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Call</OptionType>
        <ExerciseDates><ExerciseDate>2023-01-31</ExerciseDate></ExerciseDates>
      </OptionData>
      <StartDate>2022-04-29</StartDate>
      <EndDate>2022-10-31</EndDate>
      <BarrierData>
        <Type>DownAndOut</Type>
        <Levels><Level>0.98</Level></Levels>
      </BarrierData>
    </FxWindowBarrierOptionData>
  </Trade>
  <Trade id="GenericBarrier#1">
    <TradeType>EquityGenericBarrierOption</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <EquityGenericBarrierOptionData>
      <Underlying><Type>Equity</Type><Name>RIC:.SPX</Name></Underlying>
      <PayCurrency>USD</PayCurrency>
      <OptionData>
        <LongShort>Long</LongShort>
        <PayoffType>Vanilla</PayoffType>
        <OptionType>Call</OptionType>
        <ExerciseDates><ExerciseDate>2023-06-06</ExerciseDate></ExerciseDates>
      </OptionData>
      <SettlementDate>2023-06-08</SettlementDate>
      <Quantity>1000</Quantity>
      <Strike>4000</Strike>
      <Barriers>
        <ScheduleData>
          <Rules>
            <StartDate>2021-07-10</StartDate>
            <EndDate>2023-06-06</EndDate>
            <Tenor>1D</Tenor>
            <Calendar>TGT,US</Calendar>
            <Convention>F</Convention>
            <TermConvention>F</TermConvention>
            <Rule>Forward</Rule>
          </Rules>
        </ScheduleData>
        <BarrierData>
          <Type>DownAndOut</Type>
          <Levels><Level>3600</Level></Levels>
          <Rebate>1000000</Rebate>
          <RebateCurrency>USD</RebateCurrency>
          <RebatePayTime>atExpiry</RebatePayTime>
          <StrictComparison>1</StrictComparison>
        </BarrierData>
        <BarrierData>
          <Type>UpAndIn</Type>
          <Levels><Level>4400</Level></Levels>
        </BarrierData>
        <KikoType>KoAfterKi</KikoType>
      </Barriers>
      <TransatlanticBarrier>
        <BarrierData>
          <Type>UpAndOut</Type>
          <Levels><Level>4400</Level></Levels>
          <Rebate>2000000</Rebate>
        </BarrierData>
      </TransatlanticBarrier>
    </EquityGenericBarrierOptionData>
  </Trade>
</Portfolio>)");
    const auto value_of = [](const ores::trading::domain::scripted_instrument_data& data,
                             std::string_view name) -> std::string {
        for (const auto& p : data.parameters)
            if (p.name == name)
                return p.value.value_or("");
        return "<absent>";
    };
    const auto barriers_in = [](const ores::trading::domain::scripted_instrument_data& data,
                                std::string_view role) {
        std::vector<const ores::trading::domain::instrument_barrier*> rows;
        for (const auto& b : data.barriers)
            if (b.barrier_role == role)
                rows.push_back(&b);
        return rows;
    };
    const auto check_window = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data = scripted_data_of(item.instrument);
        REQUIRE(data != nullptr);
        CHECK(value_of(*data, "StartDate") == "2022-04-29");
        CHECK(value_of(*data, "EndDate") == "2022-10-31");
        REQUIRE(data->options.size() == 1);
        const auto barriers = barriers_in(*data, "barrier");
        REQUIRE(barriers.size() == 1);
        CHECK(barriers[0]->barrier_type == "DownAndOut");
        REQUIRE(data->barrier_levels.size() == 1);
    };
    const auto check_generic = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data = scripted_data_of(item.instrument);
        REQUIRE(data != nullptr);
        CHECK(value_of(*data, "Barriers.KikoType") == "KoAfterKi");
        CHECK(value_of(*data, "SettlementDate") == "2023-06-08");
        const auto barriers = barriers_in(*data, "barrier");
        REQUIRE(barriers.size() == 2);
        CHECK(barriers[0]->rebate_currency == std::optional<std::string>("USD"));
        CHECK(barriers[1]->barrier_type == "UpAndIn");
        CHECK(barriers_in(*data, "transatlantic").size() == 1u);
        CHECK(ores::ore::domain::option_block::has_product_schedule(*data, "barrier_schedule"));
        REQUIRE(data->instrument_underlyings.size() == 1);
    };

    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 2);
    check_window(items[0]);
    check_window(rebuild(items[0]));
    check_generic(items[1]);
    check_generic(rebuild(items[1]));
}

TEST_CASE("the_variance_swaps_and_a_forward_volatility_agreement_hold_their_terms_as_rows",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(R"(<Portfolio>
  <Trade id="FxBasketVarianceSwap#1">
    <TradeType>FxBasketVarianceSwap</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <FxBasketVarianceSwapData>
      <LongShort>Short</LongShort>
      <Currency>EUR</Currency>
      <Notional>1000</Notional>
      <Strike>0.07</Strike>
      <Underlyings>
        <Underlying><Type>FX</Type><Name>ECB-EUR-USD</Name><Weight>0.5</Weight></Underlying>
        <Underlying><Type>FX</Type><Name>ECB-EUR-GBP</Name><Weight>0.5</Weight></Underlying>
      </Underlyings>
      <SquaredPayoff>false</SquaredPayoff>
      <ValuationSchedule>
        <Rules>
          <StartDate>2022-01-31</StartDate>
          <EndDate>2024-01-31</EndDate>
          <Tenor>1D</Tenor>
          <Calendar>EUR,GBP</Calendar>
          <Convention>F</Convention>
          <TermConvention>F</TermConvention>
          <Rule>Forward</Rule>
        </Rules>
      </ValuationSchedule>
      <SettlementDate>2024-02-02</SettlementDate>
    </FxBasketVarianceSwapData>
  </Trade>
  <Trade id="EquityPairwiseVarianceSwap#1">
    <TradeType>EquityPairwiseVarianceSwap</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <EquityPairwiseVarianceSwapData>
      <LongShort>Long</LongShort>
      <Underlyings>
        <Value>EQ-RIC:.STOXX50E</Value>
        <Value>EQ-RIC:.SPX</Value>
      </Underlyings>
      <UnderlyingStrikes><Value>0.3385</Value><Value>0.3903</Value></UnderlyingStrikes>
      <UnderlyingNotionals><Value>577.75</Value><Value>603.54</Value></UnderlyingNotionals>
      <BasketNotional>1339.28</BasketNotional>
      <BasketStrike>0.3161</BasketStrike>
      <ValuationSchedule>
        <Rules>
          <StartDate>2021-06-25</StartDate>
          <EndDate>2022-06-24</EndDate>
          <Tenor>1D</Tenor>
          <Calendar>US</Calendar>
          <Convention>F</Convention>
          <Rule>Forward</Rule>
        </Rules>
      </ValuationSchedule>
      <AccrualLag>3</AccrualLag>
      <PayoffLimit>5</PayoffLimit>
      <Cap>2.5</Cap>
      <Floor>0</Floor>
      <SettlementDate>2022-06-29</SettlementDate>
      <PayCcy>USD</PayCcy>
    </EquityPairwiseVarianceSwapData>
  </Trade>
  <Trade id="FxForwardVolatilityAgreement#1">
    <TradeType>FxForwardVolatilityAgreement</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <FxForwardVolatilityAgreementData>
      <FvaDate>2019-07-25</FvaDate>
      <OptionExpiry>2020-01-27</OptionExpiry>
      <PremiumDate>2019-07-29</PremiumDate>
      <Underlying><Type>FX</Type><Name>ECB-GBP-USD</Name></Underlying>
      <LongShort>Long</LongShort>
      <UnderlyingStrike>1.29844</UnderlyingStrike>
      <ImpliedVolStrike>0.09675</ImpliedVolStrike>
      <Quantity>89000000</Quantity>
      <PayCcy>USD</PayCcy>
      <SettlementDate>2020-01-29</SettlementDate>
    </FxForwardVolatilityAgreementData>
  </Trade>
</Portfolio>)");
    const auto value_of = [](const ores::trading::domain::scripted_instrument_data& data,
                             std::string_view name) -> std::string {
        for (const auto& p : data.parameters)
            if (p.name == name)
                return p.value.value_or("");
        return "<absent>";
    };
    const auto list_of = [](const ores::trading::domain::scripted_instrument_data& data,
                            std::string_view name) {
        std::vector<std::string> values;
        for (const auto& p : data.parameters)
            if (p.name == name)
                for (const auto& v : data.parameter_values)
                    if (v.parameter_sequence_number == p.sequence_number)
                        values.push_back(v.value);
        return values;
    };
    const auto check_basket = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data = scripted_data_of(item.instrument);
        REQUIRE(data != nullptr);
        CHECK(value_of(*data, "SquaredPayoff") == "false");
        CHECK(value_of(*data, "Strike") == "0.07");
        REQUIRE(data->instrument_underlyings.size() == 2);
        CHECK(data->instrument_underlyings[1].weight == 0.5);
        CHECK(ores::ore::domain::option_block::has_product_schedule(*data, "valuationschedule"));
    };
    std::vector<std::string> first_read;
    const auto check_pairwise = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data = scripted_data_of(item.instrument);
        REQUIRE(data != nullptr);
        const auto names = list_of(*data, "Underlyings");
        REQUIRE(names.size() == 2);
        CHECK(names[0] != "EQ-RIC:.STOXX50E");
        if (first_read.empty())
            first_read = names;
        CHECK(names == first_read);
        CHECK(list_of(*data, "UnderlyingStrikes") == std::vector<std::string>{"0.3385", "0.3903"});
        CHECK(value_of(*data, "AccrualLag") == "3");
        CHECK(value_of(*data, "PayCcy") == "USD");
        CHECK(ores::ore::domain::option_block::has_product_schedule(*data, "valuationschedule"));
    };
    const auto check_fva = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data = scripted_data_of(item.instrument);
        REQUIRE(data != nullptr);
        CHECK(value_of(*data, "FvaDate") == "2019-07-25");
        CHECK(value_of(*data, "ImpliedVolStrike") == "0.09675");
        CHECK(value_of(*data, "UnderlyingStrike") == "1.29844");
        REQUIRE(data->instrument_underlyings.size() == 1);
    };

    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 3);
    check_basket(items[0]);
    check_basket(rebuild(items[0]));
    check_pairwise(items[1]);
    check_pairwise(rebuild(items[1]));
    check_fva(items[2]);
    check_fva(rebuild(items[2]));
}

TEST_CASE("a_european_option_barrier_and_an_fx_worst_of_hold_their_terms_as_rows",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(R"(<Portfolio>
  <Trade id="SCRIPTED_EOB">
    <TradeType>EuropeanOptionBarrier</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <EuropeanOptionBarrierData>
      <Quantity>1000000</Quantity>
      <PutCall>Call</PutCall>
      <LongShort>Long</LongShort>
      <Strike>1.08</Strike>
      <PremiumAmount>0</PremiumAmount>
      <PremiumCurrency>USD</PremiumCurrency>
      <PremiumDate>2022-02-02</PremiumDate>
      <OptionExpiry>2023-01-31</OptionExpiry>
      <OptionUnderlying><Type>FX</Type><Name>ECB-EUR-USD</Name></OptionUnderlying>
      <BarrierUnderlying><Type>FX</Type><Name>ECB-GBP-USD</Name></BarrierUnderlying>
      <BarrierLevel>0.98</BarrierLevel>
      <BarrierType>DownAndOut</BarrierType>
      <BarrierStyle>European</BarrierStyle>
      <SettlementDate>2023-02-02</SettlementDate>
      <PayCcy>USD</PayCcy>
    </EuropeanOptionBarrierData>
  </Trade>
  <Trade id="SCRIPTED_WOBS_FX">
    <TradeType>FxWorstOfBasketSwap</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <FxWorstOfBasketSwapData>
      <LongShort>Long</LongShort>
      <Currency>EUR</Currency>
      <Quantity>1000000</Quantity>
      <InitialFixedRate>0.02</InitialFixedRate>
      <Underlyings>
        <Underlying><Type>FX</Type><Name>ECB-EUR-USD</Name></Underlying>
        <Underlying><Type>FX</Type><Name>ECB-EUR-GBP</Name></Underlying>
      </Underlyings>
      <InitialPrices><InitialPrice>1.115</InitialPrice><InitialPrice>0.835</InitialPrice></InitialPrices>
      <FloatingPeriodSchedule>
        <Dates>
          <Dates><Date>2022-01-31</Date><Date>2022-07-29</Date><Date>2023-01-31</Date></Dates>
          <Calendar>TARGET</Calendar>
          <Convention>MF</Convention>
        </Dates>
      </FloatingPeriodSchedule>
      <FloatingPayDates>
        <Dates>
          <Dates><Date>2022-08-02</Date><Date>2023-02-02</Date></Dates>
          <Calendar>TARGET</Calendar>
          <Convention>MF</Convention>
        </Dates>
      </FloatingPayDates>
      <FloatingIndex>EUR-EURIBOR-3M</FloatingIndex>
      <FloatingSpread>0.0</FloatingSpread>
      <FloatingDayCountFraction>Actual/360</FloatingDayCountFraction>
      <FloatingFixingSchedule>
        <DerivedSchedule>
          <BaseSchedule>FloatingPeriodSchedule</BaseSchedule>
          <Shift>-2D</Shift>
          <Calendar>TARGET</Calendar>
          <Convention>P</Convention>
        </DerivedSchedule>
      </FloatingFixingSchedule>
      <KnockOutLevels><KnockOutLevel>1.0</KnockOutLevel><KnockOutLevel>1.0</KnockOutLevel></KnockOutLevels>
      <FixedTriggerLevels><FixedTriggerLevel>0.75</FixedTriggerLevel><FixedTriggerLevel>0.75</FixedTriggerLevel></FixedTriggerLevels>
      <FixedRate>0.05</FixedRate>
      <BermudanKnockIn>false</BermudanKnockIn>
      <KnockInLevel>0.75</KnockInLevel>
      <AccumulatingFixedCoupons>false</AccumulatingFixedCoupons>
      <AccruingFixedCoupons>false</AccruingFixedCoupons>
      <Strike>1.0</Strike>
    </FxWorstOfBasketSwapData>
  </Trade>
</Portfolio>)");
    const auto value_of = [](const ores::trading::domain::scripted_instrument_data& data,
                             std::string_view name) -> std::string {
        for (const auto& p : data.parameters)
            if (p.name == name)
                return p.value.value_or("");
        return "<absent>";
    };
    const auto count_of = [](const ores::trading::domain::scripted_instrument_data& data,
                             std::string_view name) {
        std::size_t n = 0;
        for (const auto& p : data.parameters)
            if (p.name == name)
                for (const auto& v : data.parameter_values)
                    if (v.parameter_sequence_number == p.sequence_number)
                        ++n;
        return n;
    };
    const auto check_barrier = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data = scripted_data_of(item.instrument);
        REQUIRE(data != nullptr);
        CHECK(value_of(*data, "BarrierStyle") == "European");
        CHECK(value_of(*data, "BarrierLevel") == "0.98");
        REQUIRE(data->instrument_underlyings.size() == 2);
        CHECK(data->instrument_underlyings[0].underlying_role == "optionunderlying");
        CHECK(data->instrument_underlyings[1].underlying_role == "barrierunderlying");
    };
    const auto check_worst_of = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data = scripted_data_of(item.instrument);
        REQUIRE(data != nullptr);
        CHECK(count_of(*data, "InitialPrices") == 2u);
        CHECK(count_of(*data, "KnockOutLevels") == 2u);
        CHECK(value_of(*data, "FloatingIndex") == "EUR-EURIBOR-3M");
        CHECK(value_of(*data, "BermudanKnockIn") == "false");
        CHECK(data->instrument_underlyings.size() == 2u);
        const auto fixing = std::ranges::find_if(data->schedules, [](const auto& s) {
            return s.schedule_role == "floatingfixingschedule";
        });
        REQUIRE(fixing != data->schedules.end());
        CHECK(fixing->shift == std::optional<std::string>("-2D"));
        CHECK(ores::ore::domain::option_block::has_product_schedule(*data, "floatingpaydates"));
    };

    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 2);
    check_barrier(items[0]);
    check_barrier(rebuild(items[0]));
    check_worst_of(items[1]);
    check_worst_of(rebuild(items[1]));
}

TEST_CASE("an_option_on_an_equity_future_and_on_a_bond_future_hold_their_terms_as_rows",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(R"(<Portfolio>
  <Trade id="EquityFutureOption#1">
    <TradeType>EquityFutureOption</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <EquityFutureOptionData>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Call</OptionType>
        <Style>American</Style>
        <Settlement>Cash</Settlement>
        <PayOffAtExpiry>true</PayOffAtExpiry>
        <ExerciseDates><ExerciseDate>2022-03-01</ExerciseDate></ExerciseDates>
      </OptionData>
      <Name>RIC:.SPX</Name>
      <Currency>USD</Currency>
      <Strike>2147.56</Strike>
      <Quantity>17000</Quantity>
      <FutureExpiryDate>2022-03-18</FutureExpiryDate>
    </EquityFutureOptionData>
  </Trade>
  <Trade id="BondFutureOption#1">
    <TradeType>BondFutureOption</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <BondFutureOptionData>
      <OptionData>
        <LongShort>Short</LongShort>
        <OptionType>Call</OptionType>
        <Style>American</Style>
        <ExerciseDates><ExerciseDate>2026-04-10</ExerciseDate></ExerciseDates>
      </OptionData>
      <ContractName>TYM26</ContractName>
      <ContractNotional>1000000</ContractNotional>
      <Strike>1.1175</Strike>
    </BondFutureOptionData>
  </Trade>
</Portfolio>)");
    const auto value_of = [](const ores::trading::domain::scripted_instrument_data& data,
                             std::string_view name) -> std::string {
        for (const auto& p : data.parameters)
            if (p.name == name)
                return p.value.value_or("");
        return "<absent>";
    };
    const auto check_equity = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data = scripted_data_of(item.instrument);
        REQUIRE(data != nullptr);
        CHECK(value_of(*data, "Name") == "RIC:.SPX");
        CHECK(value_of(*data, "FutureExpiryDate") == "2022-03-18");
        CHECK(value_of(*data, "Quantity") == "17000");
        REQUIRE(data->options.size() == 1);
        CHECK(data->options[0].style == std::optional<std::string>("American"));
        CHECK(data->options[0].pay_off_at_expiry.has_value());
    };
    const auto check_bond = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data = scripted_data_of(item.instrument);
        REQUIRE(data != nullptr);
        CHECK(value_of(*data, "ContractName") == "TYM26");
        CHECK(value_of(*data, "Strike") == "1.1175");
        REQUIRE(data->options.size() == 1);
        CHECK(data->options[0].long_short == "Short");
        CHECK(ores::ore::domain::option_block::exercise_dates_of(*data).size() == 1u);
    };

    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 2);
    check_equity(items[0]);
    check_equity(rebuild(items[0]));
    check_bond(items[1]);
    check_bond(rebuild(items[1]));
}

TEST_CASE("an_equity_double_touch_and_an_fx_asian_option_hold_their_terms_as_rows",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(R"(<Portfolio>
  <Trade id="EquityDoubleTouch#1">
    <TradeType>EquityDoubleTouchOption</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <EquityDoubleTouchOptionData>
      <OptionData>
        <LongShort>Long</LongShort>
        <PayOffAtExpiry>true</PayOffAtExpiry>
        <ExerciseDates><ExerciseDate>2027-01-05</ExerciseDate></ExerciseDates>
      </OptionData>
      <BarrierData>
        <Type>KnockIn</Type>
        <Levels><Level>4500</Level><Level>5500</Level></Levels>
      </BarrierData>
      <Underlying><Type>Equity</Type><Name>RIC:.SPX</Name></Underlying>
      <PayoffCurrency>USD</PayoffCurrency>
      <PayoffAmount>100000</PayoffAmount>
    </EquityDoubleTouchOptionData>
  </Trade>
  <Trade id="FxAsianOption#1">
    <TradeType>FxAsianOption</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <FxAsianOptionData>
      <Currency>USD</Currency>
      <Quantity>1000000</Quantity>
      <Strike>1.08</Strike>
      <Underlying><Type>FX</Type><Name>ECB-EUR-USD</Name><Weight>1.0</Weight></Underlying>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Call</OptionType>
        <PayoffType>Asian</PayoffType>
        <ExerciseDates><ExerciseDate>2022-03-01</ExerciseDate></ExerciseDates>
      </OptionData>
      <Settlement>2022-03-05</Settlement>
      <ObservationDates>
        <Rules>
          <StartDate>2022-02-01</StartDate>
          <EndDate>2022-03-01</EndDate>
          <Tenor>1D</Tenor>
          <Calendar>TGT,US</Calendar>
          <Convention>F</Convention>
          <TermConvention>F</TermConvention>
          <Rule>Forward</Rule>
        </Rules>
      </ObservationDates>
    </FxAsianOptionData>
  </Trade>
</Portfolio>)");
    const auto check_touch = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data =
            std::get_if<ores::trading::domain::equity_instrument_data>(&item.instrument);
        REQUIRE(data != nullptr);
        const auto* row =
            std::get_if<ores::trading::domain::equity_digital_option_instrument>(&data->instrument);
        REQUIRE(row != nullptr);
        CHECK(row->identity.trade_type_code == "EquityDoubleTouchOption");
        REQUIRE(data->barriers.size() == 1);
        CHECK(data->barriers[0].barrier_type == "KnockIn");
        REQUIRE(data->barrier_levels.size() == 2);
    };
    const auto check_asian = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data = scripted_data_of(item.instrument);
        REQUIRE(data != nullptr);
        REQUIRE(data->options.size() == 1);
        CHECK(data->options[0].payoff_type == std::optional<std::string>("Asian"));
        REQUIRE(data->instrument_underlyings.size() == 1);
        CHECK(data->instrument_underlyings[0].weight == 1.0);
        CHECK(ores::ore::domain::option_block::has_product_schedule(*data, "observationdates"));
    };

    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 2);
    check_touch(items[0]);
    check_touch(rebuild(items[0]));
    check_asian(items[1]);
    check_asian(rebuild(items[1]));
}

TEST_CASE("a_swaption_straddle_and_a_rate_digital_option_hold_their_terms_as_rows",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(R"(<Portfolio>
  <Trade id="SwaptionStraddle#1">
    <TradeType>SwaptionStraddle</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <SwaptionStraddleData>
      <OptionData>
        <LongShort>Long</LongShort>
        <Style>European</Style>
        <Settlement>Cash</Settlement>
        <SettlementMethod>ParYieldCurve</SettlementMethod>
        <ExerciseDates>
          <ExerciseDate>2027-01-02</ExerciseDate>
        </ExerciseDates>
      </OptionData>
      <LegData>
        <Payer>true</Payer>
        <LegType>Fixed</LegType>
        <Currency>EUR</Currency>
        <PaymentConvention>MF</PaymentConvention>
        <DayCounter>30/360</DayCounter>
        <Notionals>
          <Notional>5790664.33</Notional>
        </Notionals>
        <ScheduleData>
          <Rules>
            <StartDate>2027-01-04</StartDate>
            <EndDate>2034-01-04</EndDate>
            <Tenor>1Y</Tenor>
            <Calendar>TARGET</Calendar>
            <Convention>MF</Convention>
            <TermConvention>MF</TermConvention>
            <Rule>Forward</Rule>
          </Rules>
        </ScheduleData>
        <FixedLegData>
          <Rates>
            <Rate>0.0239</Rate>
          </Rates>
        </FixedLegData>
      </LegData>
      <LegData>
        <Payer>false</Payer>
        <LegType>Floating</LegType>
        <Currency>EUR</Currency>
        <PaymentConvention>MF</PaymentConvention>
        <DayCounter>A360</DayCounter>
        <Notionals>
          <Notional>5790664.33</Notional>
        </Notionals>
        <ScheduleData>
          <Rules>
            <StartDate>2027-01-04</StartDate>
            <EndDate>2034-01-04</EndDate>
            <Tenor>6M</Tenor>
            <Calendar>TARGET</Calendar>
            <Convention>MF</Convention>
            <TermConvention>MF</TermConvention>
            <Rule>Forward</Rule>
          </Rules>
        </ScheduleData>
        <FloatingLegData>
          <Index>EUR-EURIBOR-6M</Index>
          <IsInArrears>false</IsInArrears>
          <FixingDays>2</FixingDays>
          <Spreads>
            <Spread>0</Spread>
          </Spreads>
        </FloatingLegData>
      </LegData>
    </SwaptionStraddleData>
  </Trade>
  <Trade id="RateDigitalOption#1">
    <TradeType>RateDigitalOption</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <RateDigitalOptionData>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Call</OptionType>
        <ExerciseDates><ExerciseDate>2027-06-20</ExerciseDate></ExerciseDates>
      </OptionData>
      <Index>EUR-EURIBOR-6M</Index>
      <Strike>0.02</Strike>
      <PayoffAmount>10000.0</PayoffAmount>
      <PayoffCurrency>EUR</PayoffCurrency>
      <FixingDate>2027-06-18</FixingDate>
      <PaymentDate>2027-06-20</PaymentDate>
    </RateDigitalOptionData>
  </Trade>
</Portfolio>)");
    const auto check_straddle = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data =
            std::get_if<ores::trading::domain::swap_instrument_data>(&item.instrument);
        REQUIRE(data != nullptr);
        CHECK(data->header.identity.trade_type_code == "SwaptionStraddle");
        CHECK(std::holds_alternative<ores::trading::domain::swaption_instrument>(data->facts));
        CHECK(data->legs.size() == 2u);
        REQUIRE(data->options.size() == 1);
        CHECK(data->options[0].settlement == std::optional<std::string>("Cash"));
    };
    const auto check_digital = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data = scripted_data_of(item.instrument);
        REQUIRE(data != nullptr);
        std::string index;
        for (const auto& p : data->parameters)
            if (p.name == "Index")
                index = p.value.value_or("");
        CHECK(index == "EUR-EURIBOR-6M");
        REQUIRE(data->options.size() == 1);
        CHECK(data->options[0].option_type == std::optional<std::string>("Call"));
    };

    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 2);
    check_straddle(items[0]);
    check_straddle(rebuild(items[0]));
    check_digital(items[1]);
    check_digital(rebuild(items[1]));
}

TEST_CASE("a_single_name_cds_option_and_a_cbo_hold_their_terms_as_rows",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(R"(<Portfolio>
  <Trade id="CDSOption#1">
    <TradeType>CreditDefaultSwapOption</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <CreditDefaultSwapOptionData>
      <CreditDefaultSwapData>
        <IssuerId>CPTY_A</IssuerId>
        <CreditCurveId>CPTY_A</CreditCurveId>
        <SettlesAccrual>true</SettlesAccrual>
        <ProtectionPaymentTime>atDefault</ProtectionPaymentTime>
        <LegData>
          <LegType>Fixed</LegType>
          <Payer>true</Payer>
          <Currency>EUR</Currency>
          <PaymentConvention>F</PaymentConvention>
          <DayCounter>A360</DayCounter>
          <Notionals><Notional>10000000</Notional></Notionals>
          <ScheduleData>
            <Rules>
              <StartDate>2016-06-01</StartDate>
              <EndDate>2021-03-01</EndDate>
              <Tenor>3M</Tenor>
              <Calendar>WeekendsOnly</Calendar>
              <Convention>Following</Convention>
              <TermConvention>Unadjusted</TermConvention>
              <Rule>CDS2015</Rule>
            </Rules>
          </ScheduleData>
          <FixedLegData><Rates><Rate>0.0058</Rate></Rates></FixedLegData>
        </LegData>
      </CreditDefaultSwapData>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Call</OptionType>
        <Style>European</Style>
        <Settlement>Cash</Settlement>
        <PayOffAtExpiry>false</PayOffAtExpiry>
        <ExerciseDates><ExerciseDate>2016-06-01</ExerciseDate></ExerciseDates>
      </OptionData>
      <Strike>0.01</Strike>
      <StrikeType>Spread</StrikeType>
      <KnockOut>true</KnockOut>
      <Term>5Y</Term>
    </CreditDefaultSwapOptionData>
  </Trade>
  <Trade id="CBO#1">
    <TradeType>CBO</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <CBOData>
      <CBOInvestment>
        <TrancheName>JuniorNote</TrancheName>
        <Notional>1802750.20</Notional>
        <StructureId>Pegasus</StructureId>
      </CBOInvestment>
    </CBOData>
  </Trade>
</Portfolio>)");
    const auto check_option = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data =
            std::get_if<ores::trading::domain::credit_instrument_data>(&item.instrument);
        REQUIRE(data != nullptr);
        CHECK(data->instrument.identity.trade_type_code == "CreditDefaultSwapOption");
        REQUIRE(data->wrapped_swap.has_value());
        CHECK(data->wrapped_swap->issuer_id == std::optional<std::string>("CPTY_A"));
        CHECK(data->credit_default_swap_instruments.empty());
        REQUIRE(data->index_credit_default_swap_option_instruments.size() == 1);
        const auto& fact = data->index_credit_default_swap_option_instruments.front();
        CHECK(fact.knock_out == std::optional<bool>(true));
        CHECK(fact.index_term == std::optional<std::string>("5Y"));
        CHECK(fact.strike_type == std::optional<std::string>("Spread"));
        REQUIRE(data->strikes.size() == 1);
        CHECK(data->legs.size() == 1u);
        REQUIRE(data->options.size() == 1);
    };
    const auto check_cbo = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data = scripted_data_of(item.instrument);
        REQUIRE(data != nullptr);
        std::map<std::string, std::string> values;
        for (const auto& p : data->parameters)
            values[p.name] = p.value.value_or("");
        CHECK(values["CBOInvestment.TrancheName"] == "JuniorNote");
        CHECK(values["CBOInvestment.Notional"].starts_with("1802750.2"));
        CHECK(values["CBOInvestment.StructureId"] == "Pegasus");
    };

    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 2);
    check_option(items[0]);
    check_option(rebuild(items[0]));
    check_cbo(items[1]);
    check_cbo(rebuild(items[1]));
}

TEST_CASE("a_cash_position_holds_its_currency_and_amount_as_rows",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(R"(<Portfolio>
  <Trade id="Cash#1">
    <TradeType>CashPosition</TradeType>
    <Envelope><CounterParty>CPTY_A</CounterParty><NettingSetId>CPTY_A</NettingSetId></Envelope>
    <CashPositionData>
      <Currency>EUR</Currency>
      <Amount>1000000</Amount>
    </CashPositionData>
  </Trade>
</Portfolio>)");
    const auto check = [&](const ores::ore::xml::trade_import_item& item) {
        const auto* data = scripted_data_of(item.instrument);
        REQUIRE(data != nullptr);
        std::map<std::string, std::string> values;
        for (const auto& p : data->parameters)
            values[p.name] = p.value.value_or("");
        CHECK(values["Currency"] == "EUR");
        CHECK(values["Amount"] == "1000000");
    };

    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 1);
    check(items[0]);
    check(rebuild(items[0]));
}

TEST_CASE("a_trade_the_writer_refuses_is_named_in_the_refusal",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    // The reader refuses an underlying with no type, so the trade is built
    // with the writer the export uses.
    portfolio_projection::document document;
    ores::ore::xml::record_writer root(*pf::portfolio_document().root, &document);
    auto trade = root.append("Trade");
    trade.set_spelling("TradeType", "EquityPosition", "trade type");
    trade.set_text("id", "untyped-underlying-1");
    auto data = trade.child("EquityPositionData");
    data.set_decimal("Quantity", *ores::utility::decimal::decimal::from_string("5000"));
    data.append("Underlying").set_text("Name", ".SPX");
    root.state("Trade");

    try {
        static_cast<void>(portfolio_projection::write(document));
        FAIL("the writer accepted an underlying with no type");
    } catch (const std::runtime_error& e) {
        const std::string message = e.what();
        CHECK(message.find("required element 'Type'") != std::string::npos);
        CHECK(message.find("Trade 'untyped-underlying-1'") != std::string::npos);
    }
}

TEST_CASE("a_trade_the_writer_refuses_is_left_out_and_the_rest_are_written",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    portfolio_projection::document document;
    ores::ore::xml::record_writer root(*pf::portfolio_document().root, &document);
    const auto position = [&](const std::string& id, bool typed) {
        auto trade = root.append("Trade");
        trade.set_spelling("TradeType", "EquityPosition", "trade type");
        trade.set_text("id", id);
        auto data = trade.child("EquityPositionData");
        data.set_decimal("Quantity", *ores::utility::decimal::decimal::from_string("5000"));
        auto underlying = data.append("Underlying");
        if (typed)
            underlying.set_text("Type", "Equity");
        underlying.set_text("Name", ".SPX");
    };
    position("untyped-underlying-1", false);
    position("typed-underlying-2", true);
    root.state("Trade");

    const auto written = portfolio_projection::write_leaving_out(document);

    REQUIRE(written.refused.size() == 1u);
    CHECK(written.refused[0].trade_id == "untyped-underlying-1");
    CHECK(written.refused[0].trade_type == "EquityPosition");
    CHECK(written.refused[0].message.find("required element 'Type'") != std::string::npos);
    CHECK(written.xml.find("typed-underlying-2") != std::string::npos);
    CHECK(written.xml.find("untyped-underlying-1") == std::string::npos);
}

TEST_CASE("a_portfolio_with_no_trade_the_writer_accepts_is_refused",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    portfolio_projection::document document;
    ores::ore::xml::record_writer root(*pf::portfolio_document().root, &document);
    auto trade = root.append("Trade");
    trade.set_spelling("TradeType", "EquityPosition", "trade type");
    trade.set_text("id", "untyped-underlying-1");
    auto data = trade.child("EquityPositionData");
    data.set_decimal("Quantity", *ores::utility::decimal::decimal::from_string("5000"));
    data.append("Underlying").set_text("Name", ".SPX");
    root.state("Trade");

    CHECK_THROWS_AS(portfolio_projection::write_leaving_out(document), std::runtime_error);
}

TEST_CASE("a_portfolio_document_projects_an_equity_position_and_its_option_entries",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(equity_position_document_text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 2);
    CHECK(items[0].anchor.trade_type == "EquityPosition");
    CHECK(items[1].anchor.trade_type == "EquityOptionPosition");

    const auto* position =
        std::get_if<ores::trading::domain::equity_instrument_data>(&items[0].instrument);
    REQUIRE(position != nullptr);
    const auto* plain =
        std::get_if<ores::trading::domain::equity_position_instrument>(&position->instrument);
    REQUIRE(plain != nullptr);
    CHECK(plain->identity.trade_type_code == "EquityPosition");
    CHECK(plain->quantity == 5000.0);
    CHECK(ores::ore::domain::underlying_name_of(plain->underlying_name) == ".SPX");
    CHECK(position->underlyings.empty());

    const auto* entries =
        std::get_if<ores::trading::domain::equity_instrument_data>(&items[1].instrument);
    REQUIRE(entries != nullptr);
    const auto* held =
        std::get_if<ores::trading::domain::equity_position_instrument>(&entries->instrument);
    REQUIRE(held != nullptr);
    CHECK(held->identity.trade_type_code == "EquityOptionPosition");
    CHECK(held->quantity == 1000.0);
    CHECK(ores::ore::domain::underlying_name_of(held->underlying_name) == ".SPX");
    REQUIRE(entries->underlyings.size() == 2);
    CHECK(entries->underlyings[0].sequence_number == 1);
    CHECK(ores::ore::domain::underlying_name_of(entries->underlyings[0].underlying_name) == ".SPX");
    CHECK(entries->underlyings[0].strike.to_string() == "3300");
    CHECK(entries->underlyings[0].weight == std::optional<double>(0.5));
    CHECK(entries->underlyings[0].long_short == "Long");
    CHECK(entries->underlyings[0].option_type == "Call");
    CHECK(entries->underlyings[0].exercise_type == "European");
    CHECK(entries->underlyings[0].settlement_type == "Cash");
    CHECK(entries->underlyings[1].sequence_number == 2);
    CHECK(ores::ore::domain::underlying_name_of(entries->underlyings[1].underlying_name) ==
          ".SX5E");
    CHECK(entries->underlyings[1].strike.to_string() == "3400");
    CHECK(entries->underlyings[1].long_short == "Short");
    CHECK(entries->underlyings[1].option_type == "Put");
    CHECK(entries->underlyings[1].exercise_type == "American");
    CHECK(entries->underlyings[1].settlement_type == "Physical");

    const auto plain_again = rebuild(items[0]);
    const auto* plain_round_data =
        std::get_if<ores::trading::domain::equity_instrument_data>(&plain_again.instrument);
    REQUIRE(plain_round_data != nullptr);
    const auto* plain_round = std::get_if<ores::trading::domain::equity_position_instrument>(
        &plain_round_data->instrument);
    REQUIRE(plain_round != nullptr);
    CHECK(plain_again.anchor.trade_type == "EquityPosition");
    CHECK(plain_round->identity.trade_type_code == "EquityPosition");
    CHECK(plain_round->quantity == 5000.0);
    CHECK(ores::ore::domain::underlying_name_of(plain_round->underlying_name) == ".SPX");
    CHECK(plain_round_data->underlyings.empty());

    const auto entries_again = rebuild(items[1]);
    const auto* round_data =
        std::get_if<ores::trading::domain::equity_instrument_data>(&entries_again.instrument);
    REQUIRE(round_data != nullptr);
    const auto* round =
        std::get_if<ores::trading::domain::equity_position_instrument>(&round_data->instrument);
    REQUIRE(round != nullptr);
    CHECK(entries_again.anchor.trade_type == "EquityOptionPosition");
    CHECK(round->identity.trade_type_code == "EquityOptionPosition");
    CHECK(round->quantity == 1000.0);
    REQUIRE(round_data->underlyings.size() == 2);
    CHECK(round_data->underlyings[0].sequence_number == 1);
    CHECK(ores::ore::domain::underlying_name_of(round_data->underlyings[0].underlying_name) ==
          ".SPX");
    CHECK(round_data->underlyings[0].strike.to_string() == "3300");
    CHECK(round_data->underlyings[0].weight == std::optional<double>(0.5));
    CHECK(round_data->underlyings[0].settlement_type == "Cash");
    CHECK(round_data->underlyings[1].sequence_number == 2);
    CHECK(ores::ore::domain::underlying_name_of(round_data->underlyings[1].underlying_name) ==
          ".SX5E");
    CHECK(round_data->underlyings[1].strike.to_string() == "3400");
    CHECK(round_data->underlyings[1].settlement_type == "Physical");
}

TEST_CASE("a_portfolio_document_projects_an_equity_barrier_option_and_an_accumulator",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(equity_barrier_and_accumulator_document_text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 2);
    CHECK(items[0].anchor.trade_type == "EquityBarrierOption");
    CHECK(items[1].anchor.trade_type == "EquityAccumulator");

    const auto* barrier_data =
        std::get_if<ores::trading::domain::equity_instrument_data>(&items[0].instrument);
    REQUIRE(barrier_data != nullptr);
    const auto* barrier = std::get_if<ores::trading::domain::equity_barrier_option_instrument>(
        &barrier_data->instrument);
    REQUIRE(barrier != nullptr);
    CHECK(barrier->identity.trade_type_code == "EquityBarrierOption");
    CHECK(ores::ore::domain::underlying_name_of(barrier->underlying_name) == "RIC:.SPX");
    CHECK(barrier->currency == "USD");
    CHECK(barrier->notional.to_string() == "1000");
    CHECK(barrier->option_type == "Call");
    CHECK(barrier->exercise_type == "European");
    CHECK(barrier->expiry_date == date_of(2026, 8, 15));
    CHECK(barrier->strike.to_string() == "3200");
    CHECK(barrier->lower_barrier_type == "UpAndOut");
    CHECK(barrier->lower_barrier.to_string() == "3500");
    CHECK_FALSE(barrier->upper_barrier.has_value());

    const auto* accumulator_data =
        std::get_if<ores::trading::domain::equity_instrument_data>(&items[1].instrument);
    REQUIRE(accumulator_data != nullptr);
    const auto* accumulator = std::get_if<ores::trading::domain::equity_accumulator_instrument>(
        &accumulator_data->instrument);
    REQUIRE(accumulator != nullptr);
    CHECK(accumulator->identity.trade_type_code == "EquityAccumulator");
    CHECK(ores::ore::domain::underlying_name_of(accumulator->underlying_name) == ".STOXX50");
    CHECK(accumulator->currency == "EUR");
    CHECK(accumulator->fixing_amount.to_string() == "30");
    CHECK(accumulator->strike.to_string() == "4000");
    CHECK(accumulator->start_date == date_of(2025, 2, 5));
    REQUIRE(accumulator->knock_out_level.has_value());
    CHECK(accumulator->knock_out_level->to_string() == "3500");
    // A bound is a row, so the two ranges the document states are two rows.
    REQUIRE(accumulator_data->range_bounds.size() == 2);
    CHECK_FALSE(accumulator_data->range_bounds[0].range_from.has_value());
    REQUIRE(accumulator_data->range_bounds[0].range_to.has_value());
    CHECK(accumulator_data->range_bounds[0].range_to->to_string() == "4000");
    REQUIRE(accumulator_data->range_bounds[0].leverage.has_value());
    CHECK(accumulator_data->range_bounds[0].leverage.value() == 1.0);
    REQUIRE(accumulator_data->range_bounds[1].range_from.has_value());
    CHECK(accumulator_data->range_bounds[1].range_from->to_string() == "4000");
    REQUIRE(accumulator_data->range_bounds[1].leverage.has_value());
    CHECK(accumulator_data->range_bounds[1].leverage.value() == 2.0);

    const auto barrier_again = rebuild(items[0]);
    const auto* barrier_round_data =
        std::get_if<ores::trading::domain::equity_instrument_data>(&barrier_again.instrument);
    REQUIRE(barrier_round_data != nullptr);
    const auto* barrier_round =
        std::get_if<ores::trading::domain::equity_barrier_option_instrument>(
            &barrier_round_data->instrument);
    REQUIRE(barrier_round != nullptr);
    CHECK(barrier_again.anchor.trade_type == "EquityBarrierOption");
    CHECK(barrier_round->identity.trade_type_code == "EquityBarrierOption");
    CHECK(ores::ore::domain::underlying_name_of(barrier_round->underlying_name) == "RIC:.SPX");
    CHECK(barrier_round->currency == "USD");
    CHECK(barrier_round->notional.to_string() == "1000");
    CHECK(barrier_round->option_type == "Call");
    CHECK(barrier_round->exercise_type == "European");
    CHECK(barrier_round->expiry_date == date_of(2026, 8, 15));
    CHECK(barrier_round->strike.to_string() == "3200");
    CHECK(barrier_round->lower_barrier_type == "UpAndOut");
    CHECK(barrier_round->lower_barrier.to_string() == "3500");

    const auto accumulator_again = rebuild(items[1]);
    const auto* round_data =
        std::get_if<ores::trading::domain::equity_instrument_data>(&accumulator_again.instrument);
    REQUIRE(round_data != nullptr);
    const auto* round =
        std::get_if<ores::trading::domain::equity_accumulator_instrument>(&round_data->instrument);
    REQUIRE(round != nullptr);
    CHECK(accumulator_again.anchor.trade_type == "EquityAccumulator");
    CHECK(round->identity.trade_type_code == "EquityAccumulator");
    CHECK(ores::ore::domain::underlying_name_of(round->underlying_name) == ".STOXX50");
    CHECK(round->currency == "EUR");
    CHECK(round->fixing_amount.to_string() == "30");
    CHECK(round->strike.to_string() == "4000");
    CHECK(round->start_date == date_of(2025, 2, 5));
    // The bounds survive the write and the read, so the round trip keeps them.
    REQUIRE(round_data->range_bounds.size() == 2);
    REQUIRE(round_data->range_bounds[0].range_to.has_value());
    CHECK(round_data->range_bounds[0].range_to->to_string() == "4000");
    REQUIRE(round_data->range_bounds[0].leverage.has_value());
    CHECK(round_data->range_bounds[0].leverage.value() == 1.0);
    REQUIRE(round_data->range_bounds[1].range_from.has_value());
    CHECK(round_data->range_bounds[1].range_from->to_string() == "4000");
    REQUIRE(round_data->range_bounds[1].leverage.has_value());
    CHECK(round_data->range_bounds[1].leverage.value() == 2.0);
}

TEST_CASE("a_portfolio_document_projects_a_commodity_forward_and_a_commodity_swap",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(commodity_swap_and_forward_document_text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 2);
    CHECK(items[0].anchor.trade_type == "CommodityForward");
    CHECK(items[1].anchor.trade_type == "CommoditySwap");

    const auto* forward_data =
        std::get_if<ores::trading::domain::commodity_instrument_data>(&items[0].instrument);
    REQUIRE(forward_data != nullptr);
    CHECK(forward_data->constituents.empty());
    const auto& forward = forward_data->instrument;
    CHECK(forward.identity.trade_type_code == "CommodityForward");
    CHECK(ores::ore::domain::underlying_name_of(forward.commodity_code) == "NGAS");
    CHECK(forward.currency == "USD");
    CHECK(forward.quantity == 1000.0);
    REQUIRE(forward.fixed_price.has_value());
    CHECK(forward.fixed_price->to_string() == "3.25");
    CHECK(forward.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2026, 3, 31)));
    CHECK_FALSE(forward.start_date.has_value());
    CHECK(forward.audit.change_reason_code == "system.external_data_import");

    const auto* swap_data =
        std::get_if<ores::trading::domain::commodity_instrument_data>(&items[1].instrument);
    REQUIRE(swap_data != nullptr);
    const auto& swap = swap_data->instrument;
    CHECK(swap.identity.trade_type_code == "CommoditySwap");
    CHECK(ores::ore::domain::underlying_name_of(swap.commodity_code) == "NGAS");
    CHECK(swap.currency == "USD");
    CHECK(swap.quantity == 1000.0);
    CHECK(swap.start_date == std::optional<std::chrono::year_month_day>(date_of(2025, 1, 15)));
    CHECK(swap.maturity_date == std::optional<std::chrono::year_month_day>(date_of(2026, 1, 15)));
    CHECK_FALSE(swap.fixed_price.has_value());

    const auto forward_again = rebuild(items[0]);
    CHECK(forward_again.anchor.trade_type == "CommodityForward");
    CHECK(forward_again.envelope.has_value());
    CHECK(forward_again.envelope->netting_set_id == std::optional<std::string>("NS-COM"));
    const auto* forward_round_data =
        std::get_if<ores::trading::domain::commodity_instrument_data>(&forward_again.instrument);
    REQUIRE(forward_round_data != nullptr);
    const auto& forward_round = forward_round_data->instrument;
    CHECK(forward_round.identity.trade_type_code == "CommodityForward");
    CHECK(ores::ore::domain::underlying_name_of(forward_round.commodity_code) == "NGAS");
    CHECK(forward_round.currency == "USD");
    CHECK(forward_round.quantity == 1000.0);
    REQUIRE(forward_round.fixed_price.has_value());
    CHECK(forward_round.fixed_price->to_string() == "3.25");
    CHECK(forward_round.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2026, 3, 31)));

    const auto swap_again = rebuild(items[1]);
    const auto* swap_round_data =
        std::get_if<ores::trading::domain::commodity_instrument_data>(&swap_again.instrument);
    REQUIRE(swap_round_data != nullptr);
    const auto& swap_round = swap_round_data->instrument;
    CHECK(swap_again.anchor.trade_type == "CommoditySwap");
    CHECK(swap_round.identity.trade_type_code == "CommoditySwap");
    CHECK(ores::ore::domain::underlying_name_of(swap_round.commodity_code) == "NGAS");
    CHECK(swap_round.currency == "USD");
    CHECK(swap_round.quantity == 1000.0);
    CHECK(swap_round.start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2025, 1, 15)));
    CHECK(swap_round.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2026, 1, 15)));
}

TEST_CASE("a_portfolio_document_projects_a_commodity_average_price_option_and_a_basket",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document =
        portfolio_projection::read(commodity_average_price_and_basket_document_text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 2);
    CHECK(items[0].anchor.trade_type == "CommodityAveragePriceOption");
    CHECK(items[1].anchor.trade_type == "CommodityBasketOption");

    const auto* apo_data =
        std::get_if<ores::trading::domain::commodity_instrument_data>(&items[0].instrument);
    REQUIRE(apo_data != nullptr);
    const auto& apo = apo_data->instrument;
    CHECK(apo.identity.trade_type_code == "CommodityAveragePriceOption");
    CHECK(ores::ore::domain::underlying_name_of(apo.commodity_code) == "WTI");
    CHECK(apo.currency == "USD");
    CHECK(apo.quantity == 500.0);
    REQUIRE(apo.strike_price.has_value());
    CHECK(apo.strike_price->to_string() == "75.5");
    CHECK(apo.option_type == "Call");
    CHECK(apo.exercise_type == "European");
    CHECK(apo.maturity_date == std::optional<std::chrono::year_month_day>(date_of(2026, 6, 30)));
    CHECK(apo.averaging_start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2025, 3, 1)));
    CHECK(apo.averaging_end_date ==
          std::optional<std::chrono::year_month_day>(date_of(2026, 6, 30)));
    CHECK(apo.average_type.empty());

    const auto* basket_data =
        std::get_if<ores::trading::domain::commodity_instrument_data>(&items[1].instrument);
    REQUIRE(basket_data != nullptr);
    const auto& basket = basket_data->instrument;
    CHECK(basket.identity.trade_type_code == "CommodityBasketOption");
    CHECK(ores::ore::domain::underlying_name_of(basket.commodity_code) == "GOLD");
    CHECK(basket.currency == "EUR");
    CHECK(basket.quantity == 100.0);
    REQUIRE(basket.strike_price.has_value());
    CHECK(basket.strike_price->to_string() == "50");
    CHECK(basket.option_type == "Put");
    CHECK(basket.exercise_type == "American");
    CHECK(basket.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2026, 12, 31)));
    REQUIRE(basket_data->constituents.size() == 2);
    CHECK(basket_data->constituents[0].sequence_number == 1);
    CHECK(ores::ore::domain::underlying_name_of(basket_data->constituents[0].underlying_code) ==
          "GOLD");
    CHECK(basket_data->constituents[0].weight == std::optional<double>(0.6));
    CHECK(basket_data->constituents[0].change_reason_code == "system.external_data_import");
    CHECK(basket_data->constituents[1].sequence_number == 2);
    CHECK(ores::ore::domain::underlying_name_of(basket_data->constituents[1].underlying_code) ==
          "SILVER");
    CHECK(basket_data->constituents[1].weight == std::optional<double>(0.4));

    const auto apo_again = rebuild(items[0]);
    const auto* apo_round_data =
        std::get_if<ores::trading::domain::commodity_instrument_data>(&apo_again.instrument);
    REQUIRE(apo_round_data != nullptr);
    const auto& apo_round = apo_round_data->instrument;
    CHECK(apo_again.anchor.trade_type == "CommodityAveragePriceOption");
    CHECK(apo_round.identity.trade_type_code == "CommodityAveragePriceOption");
    CHECK(ores::ore::domain::underlying_name_of(apo_round.commodity_code) == "WTI");
    CHECK(apo_round.currency == "USD");
    CHECK(apo_round.quantity == 500.0);
    REQUIRE(apo_round.strike_price.has_value());
    CHECK(apo_round.strike_price->to_string() == "75.5");
    CHECK(apo_round.option_type == "Call");
    CHECK(apo_round.exercise_type == "European");
    CHECK(apo_round.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2026, 6, 30)));
    CHECK(apo_round.averaging_start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2025, 3, 1)));
    CHECK(apo_round.averaging_end_date ==
          std::optional<std::chrono::year_month_day>(date_of(2026, 6, 30)));

    const auto basket_again = rebuild(items[1]);
    const auto* basket_round_data =
        std::get_if<ores::trading::domain::commodity_instrument_data>(&basket_again.instrument);
    REQUIRE(basket_round_data != nullptr);
    const auto& basket_round = basket_round_data->instrument;
    CHECK(basket_again.anchor.trade_type == "CommodityBasketOption");
    CHECK(basket_round.identity.trade_type_code == "CommodityBasketOption");
    CHECK(ores::ore::domain::underlying_name_of(basket_round.commodity_code) == "GOLD");
    CHECK(basket_round.currency == "EUR");
    CHECK(basket_round.quantity == 100.0);
    REQUIRE(basket_round.strike_price.has_value());
    CHECK(basket_round.strike_price->to_string() == "50");
    CHECK(basket_round.option_type == "Put");
    CHECK(basket_round.exercise_type == "American");
    CHECK(basket_round.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2026, 12, 31)));
    REQUIRE(basket_round_data->constituents.size() == 2);
    CHECK(basket_round_data->constituents[0].sequence_number == 1);
    CHECK(ores::ore::domain::underlying_name_of(
              basket_round_data->constituents[0].underlying_code) == "GOLD");
    CHECK(basket_round_data->constituents[0].weight == std::optional<double>(0.6));
    CHECK(basket_round_data->constituents[1].sequence_number == 2);
    CHECK(ores::ore::domain::underlying_name_of(
              basket_round_data->constituents[1].underlying_code) == "SILVER");
    CHECK(basket_round_data->constituents[1].weight == std::optional<double>(0.4));
}

TEST_CASE("a_portfolio_document_projects_a_composite_trade_and_its_components",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(composite_document_text);
    const auto items = portfolio_projection::map(document);
    // Each component is a trade of its own, placed right after its composite
    // and naming it, so the import can join them in a structure.
    // The total return swap's and the contract for difference's underlyings
    // follow each of them in the same way.
    REQUIRE(items.size() == 7);
    CHECK(items[0].anchor.trade_type == "CompositeTrade");
    CHECK(!items[0].membership);
    // A component's ordinal counts within its list: the Trade and the SubTrade
    // are each the first of their kind.
    const std::pair<std::string_view, int> expected[] = {{"trade", 1}, {"sub_trade", 1}};
    for (int i = 1; i <= 2; ++i) {
        INFO("component " << i);
        REQUIRE(items[i].membership.has_value());
        CHECK(items[i].membership->parent_ore_id == items[0].ore_id);
        CHECK(items[i].membership->role == expected[i - 1].first);
        CHECK(items[i].membership->sequence_number == expected[i - 1].second);
        CHECK(!std::holds_alternative<std::monostate>(items[i].instrument));
    }
    CHECK(items[1].ore_id == "component-swap");
    CHECK(items[2].ore_id == "component-fx");
    CHECK(items[3].anchor.trade_type == "TotalReturnSwap");
    CHECK(items[5].anchor.trade_type == "ContractForDifference");
    for (const int i : {4, 6}) {
        INFO("underlying " << i);
        REQUIRE(items[i].membership.has_value());
        CHECK(items[i].membership->parent_ore_id == items[i - 1].ore_id);
    }

    const auto* composite =
        std::get_if<ores::trading::domain::composite_instrument_data>(&items[0].instrument);
    REQUIRE(composite != nullptr);
    CHECK(composite->instrument.identity.trade_type_code == "CompositeTrade");
    CHECK(composite->instrument.description == "ACME-BASKET");

    const auto back = rebuild(items[0]);
    CHECK(back.anchor.trade_type == "CompositeTrade");
    const auto* round =
        std::get_if<ores::trading::domain::composite_instrument_data>(&back.instrument);
    REQUIRE(round != nullptr);
    CHECK(round->instrument.identity.trade_type_code == "CompositeTrade");
    CHECK(round->instrument.description == "ACME-BASKET");

    const auto written = portfolio_projection::write(exported(items[0]));
    const auto reread_items = portfolio_projection::map(portfolio_projection::read(written));
    REQUIRE(reread_items.size() == 1);
    CHECK(reread_items[0].anchor.trade_type == "CompositeTrade");
    const auto* reread =
        std::get_if<ores::trading::domain::composite_instrument_data>(&reread_items[0].instrument);
    REQUIRE(reread != nullptr);
    CHECK(reread->instrument.identity.trade_type_code == "CompositeTrade");
    CHECK(reread->instrument.description == "ACME-BASKET");
}

TEST_CASE("a_portfolio_document_projects_a_total_return_swap_and_a_contract_for_difference",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(composite_document_text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 7);

    const auto* trs =
        std::get_if<ores::trading::domain::composite_instrument_data>(&items[3].instrument);
    REQUIRE(trs != nullptr);
    CHECK(trs->instrument.identity.trade_type_code == "TotalReturnSwap");
    CHECK(trs->instrument.description == "EquityPosition");

    const auto* cfd =
        std::get_if<ores::trading::domain::composite_instrument_data>(&items[5].instrument);
    REQUIRE(cfd != nullptr);
    CHECK(cfd->instrument.identity.trade_type_code == "ContractForDifference");
    CHECK(cfd->instrument.description == "Swap");

    const auto trs_again = rebuild(items[3]);
    REQUIRE(std::get_if<ores::trading::domain::composite_instrument_data>(&trs_again.instrument) !=
            nullptr);
    CHECK(trs_again.anchor.trade_type == "TotalReturnSwap");
    CHECK(std::get<ores::trading::domain::composite_instrument_data>(trs_again.instrument)
              .instrument.identity.trade_type_code == "TotalReturnSwap");

    const auto cfd_again = rebuild(items[5]);
    REQUIRE(std::get_if<ores::trading::domain::composite_instrument_data>(&cfd_again.instrument) !=
            nullptr);
    CHECK(cfd_again.anchor.trade_type == "ContractForDifference");
    CHECK(std::get<ores::trading::domain::composite_instrument_data>(cfd_again.instrument)
              .instrument.identity.trade_type_code == "ContractForDifference");
}

TEST_CASE("a_written_equity_position_states_the_type_of_its_underlying",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto items =
        portfolio_projection::map(portfolio_projection::read(equity_position_document_text));
    REQUIRE(items.size() == 2);

    // The schema requires a type on every underlying, so a position the row
    // names only by its equity would not be written at all.
    for (const auto& item : items) {
        INFO("trade " << item.ore_id);
        const auto written = portfolio_projection::write(exported(item));
        CHECK(written.find("<Type>Equity</Type>") != std::string::npos);
        const auto again = portfolio_projection::map(portfolio_projection::read(written));
        REQUIRE(again.size() == 1);
        CHECK(again.front().anchor.trade_type == item.anchor.trade_type);
    }
}

TEST_CASE("a_written_product_states_the_terms_the_schema_requires",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(required_terms_document_text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 5);

    // A writing that leaves a required element unstated does not bind again, so
    // each of these products is written and read back before the values are
    // compared.
    for (const auto& item : items) {
        INFO("trade " << item.ore_id);
        const auto written = portfolio_projection::write(exported(item));
        const auto again = portfolio_projection::map(portfolio_projection::read(written));
        REQUIRE(again.size() == 1);
        CHECK(again.front().ore_id == item.ore_id);
        CHECK(again.front().anchor.trade_type == item.anchor.trade_type);
    }

    // The scripted row states no variable of its own, so the data element the
    // schema requires carries the script's own result and nothing else.
    const auto* scripted = scripted_data_of(items[0].instrument);
    REQUIRE(scripted != nullptr);
    CHECK(scripted->instrument.script_name == "heston");
    CHECK(scripted->instrument.script_npv == "Option");
    CHECK(scripted->parameters.empty());
    CHECK(scripted->parameter_values.empty());

    // A product ORE reads through a class of its own states its terms as
    // parameters, and its underlyings as rows carrying the type the flat table
    // has no parameter for.
    const auto* double_digital = scripted_data_of(items[1].instrument);
    REQUIRE(double_digital != nullptr);
    REQUIRE(double_digital->instrument_underlyings.size() == 2);
    const auto& first_underlying = double_digital->instrument_underlyings[0];
    const auto& second_underlying = double_digital->instrument_underlyings[1];
    CHECK(first_underlying.underlying_role == "underlying1");
    CHECK(first_underlying.underlying_type == "FX");
    CHECK(first_underlying.name == "oresmd://fx/EUR?type=fixing&index=fx&source=TR20H&ccy=USD");
    CHECK(second_underlying.underlying_role == "underlying2");
    CHECK(second_underlying.underlying_type == "Commodity");
    CHECK(second_underlying.name == "oresmd://commodity/NYMEX:CL?type=fixing&index=commodity");

    const auto parameter_named = [](const ores::trading::domain::scripted_instrument_data& data,
                                    std::string_view name) {
        const auto found = std::ranges::find_if(
            data.parameters, [&](const auto& parameter) { return parameter.name == name; });
        return found == data.parameters.end() ? nullptr : &*found;
    };
    const auto* position = parameter_named(*double_digital, "Position");
    REQUIRE(position != nullptr);
    CHECK(position->declared_type == "enumeration");
    CHECK(position->value == std::optional<std::string>("Long"));
    const auto* payout = parameter_named(*double_digital, "BinaryPayout");
    REQUIRE(payout != nullptr);
    CHECK(payout->declared_type == "decimal");
    CHECK(payout->value == std::optional<std::string>("1000000"));

    const auto double_digital_round = rebuild(items[1]);
    const auto* double_digital_again = scripted_data_of(double_digital_round.instrument);
    REQUIRE(double_digital_again != nullptr);
    CHECK(double_digital_again->parameters.size() == double_digital->parameters.size());
    for (std::size_t i = 0; i < double_digital->parameters.size(); ++i) {
        CHECK(double_digital_again->parameters[i].name == double_digital->parameters[i].name);
        CHECK(double_digital_again->parameters[i].value == double_digital->parameters[i].value);
    }
    REQUIRE(double_digital_again->instrument_underlyings.size() == 2);
    for (std::size_t i = 0; i < 2; ++i) {
        CHECK(double_digital_again->instrument_underlyings[i].name ==
              double_digital->instrument_underlyings[i].name);
        CHECK(double_digital_again->instrument_underlyings[i].underlying_type ==
              double_digital->instrument_underlyings[i].underlying_type);
    }

    // The cliquet row keeps no frequency, so the schedule element the schema
    // requires is stated without a rule rather than dropped.
    const auto* cliquet_data =
        std::get_if<ores::trading::domain::equity_instrument_data>(&items[2].instrument);
    REQUIRE(cliquet_data != nullptr);
    const auto* cliquet =
        std::get_if<ores::trading::domain::equity_option_instrument>(&cliquet_data->instrument);
    REQUIRE(cliquet != nullptr);
    CHECK(ores::ore::domain::underlying_name_of(cliquet->underlying_name) == "RIC:.SPX");
    CHECK(cliquet->currency == "USD");
    CHECK(cliquet->notional.to_string() == "1000");
    CHECK(cliquet->option_type == "Call");
    CHECK(cliquet->cliquet_frequency.empty());

    const auto cliquet_round = rebuild(items[2]);
    const auto* cliquet_again_data =
        std::get_if<ores::trading::domain::equity_instrument_data>(&cliquet_round.instrument);
    REQUIRE(cliquet_again_data != nullptr);
    const auto* cliquet_again = std::get_if<ores::trading::domain::equity_option_instrument>(
        &cliquet_again_data->instrument);
    REQUIRE(cliquet_again != nullptr);
    CHECK(cliquet_again->underlying_name == cliquet->underlying_name);
    CHECK(cliquet_again->notional.to_string() == cliquet->notional.to_string());
    CHECK(cliquet_again->option_type == cliquet->option_type);

    // The row keeps the payoff amount, and the quantity the schema requires
    // states the one unit the export pays out.
    const auto* digital_data =
        std::get_if<ores::trading::domain::equity_instrument_data>(&items[3].instrument);
    REQUIRE(digital_data != nullptr);
    const auto* digital = std::get_if<ores::trading::domain::equity_digital_option_instrument>(
        &digital_data->instrument);
    REQUIRE(digital != nullptr);
    CHECK(ores::ore::domain::underlying_name_of(digital->underlying_name) == "RIC:.SPX");
    CHECK(digital->notional.to_string() == "1000");
    REQUIRE(digital->strike.has_value());
    CHECK(digital->strike->to_string() == "3300");
    CHECK(digital->option_type == "Call");

    const auto digital_round = rebuild(items[3]);
    const auto* digital_again_data =
        std::get_if<ores::trading::domain::equity_instrument_data>(&digital_round.instrument);
    REQUIRE(digital_again_data != nullptr);
    const auto* digital_again =
        std::get_if<ores::trading::domain::equity_digital_option_instrument>(
            &digital_again_data->instrument);
    REQUIRE(digital_again != nullptr);
    CHECK(digital_again->underlying_name == digital->underlying_name);
    CHECK(digital_again->notional.to_string() == digital->notional.to_string());
    REQUIRE(digital_again->strike.has_value());
    CHECK(digital_again->strike->to_string() == digital->strike->to_string());

    // Each level list spells its leaf its own way, so the lists survive only
    // when the writing names the leaf the schema names.
    const auto* worst_of_data =
        std::get_if<ores::trading::domain::equity_instrument_data>(&items[4].instrument);
    REQUIRE(worst_of_data != nullptr);
    const auto* worst_of =
        std::get_if<ores::trading::domain::equity_swap_instrument>(&worst_of_data->instrument);
    REQUIRE(worst_of != nullptr);
    // The basket is rows now, in the document's order, not an encoded string.
    REQUIRE(worst_of_data->basket_constituents.size() == 2);
    CHECK(ores::ore::domain::underlying_name_of(
              worst_of_data->basket_constituents[0].underlying_name) == "RIC:.SPX");
    CHECK(worst_of_data->basket_constituents[0].underlying_kind == "Equity");
    CHECK(worst_of_data->basket_constituents[1].underlying_kind == "Equity");
    CHECK(ores::ore::domain::underlying_name_of(
              worst_of_data->basket_constituents[1].underlying_name) == "RIC:.NDX");
    CHECK(worst_of_data->basket_constituents[0].sequence_number == 1);
    CHECK(worst_of_data->basket_constituents[1].sequence_number == 2);
    // A price per constituent, a level per period: two names, one date.
    REQUIRE(worst_of_data->basket_constituents.size() == 2);
    CHECK(worst_of_data->basket_constituents[0]
              .initial_price.value_or(ores::utility::decimal::decimal{})
              .to_string() == "3200");
    CHECK(worst_of_data->basket_constituents[1]
              .initial_price.value_or(ores::utility::decimal::decimal{})
              .to_string() == "11500");
    REQUIRE(worst_of_data->periods.size() == 1);
    CHECK(worst_of_data->periods[0].knock_out_level == 3500.0);
    CHECK(worst_of_data->periods[0].fixed_trigger_level == 3400.0);

    const auto worst_of_round = rebuild(items[4]);
    const auto* worst_of_again_data =
        std::get_if<ores::trading::domain::equity_instrument_data>(&worst_of_round.instrument);
    REQUIRE(worst_of_again_data != nullptr);
    const auto* worst_of_again = std::get_if<ores::trading::domain::equity_swap_instrument>(
        &worst_of_again_data->instrument);
    REQUIRE(worst_of_again != nullptr);
    REQUIRE(worst_of_again_data->basket_constituents.size() == 2);
    CHECK(ores::ore::domain::underlying_name_of(
              worst_of_again_data->basket_constituents[0].underlying_name) == "RIC:.SPX");
    CHECK(worst_of_again_data->basket_constituents[0].underlying_kind == "Equity");
    CHECK(ores::ore::domain::underlying_name_of(
              worst_of_again_data->basket_constituents[1].underlying_name) == "RIC:.NDX");
    REQUIRE(worst_of_again_data->basket_constituents.size() == 2);
    CHECK(worst_of_again_data->basket_constituents[0]
              .initial_price.value_or(ores::utility::decimal::decimal{})
              .to_string() == "3200");
    CHECK(worst_of_again_data->basket_constituents[1]
              .initial_price.value_or(ores::utility::decimal::decimal{})
              .to_string() == "11500");
    REQUIRE(worst_of_again_data->periods.size() == 1);
    CHECK(worst_of_again_data->periods[0].knock_out_level == 3500.0);
    CHECK(worst_of_again_data->periods[0].fixed_trigger_level == 3400.0);
}

TEST_CASE("a_collar_keeps_both_arms_of_its_strike_lists", "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    const std::string text = R"(<Portfolio>
  <Trade id="collar-1">
    <TradeType>CapFloor</TradeType>
    <CapFloorData>
      <LongShort>Long</LongShort>
      <LegData>
        <Payer>false</Payer>
        <LegType>Floating</LegType>
        <Currency>EUR</Currency>
        <DayCounter>ACT/360</DayCounter>
        <Notionals><Notional>1000000</Notional></Notionals>
        <ScheduleData>
          <Rules>
            <StartDate>2024-03-01</StartDate>
            <EndDate>2029-03-01</EndDate>
            <Tenor>3M</Tenor>
            <Convention>MF</Convention>
          </Rules>
        </ScheduleData>
        <FloatingLegData><Index>EURIBOR-3M</Index></FloatingLegData>
      </LegData>
      <Caps><Cap>0.03</Cap><Cap>0.05</Cap></Caps>
      <Floors><Floor>0.0121</Floor></Floors>
    </CapFloorData>
  </Trade>
</Portfolio>)";

    const auto items = portfolio_projection::map(portfolio_projection::read(text));
    REQUIRE(items.size() == 1);
    const auto* instrument =
        std::get_if<ores::trading::domain::swap_instrument_data>(&items[0].instrument);
    REQUIRE(instrument != nullptr);
    REQUIRE(instrument->strikes.size() == 3);

    const auto role = [&instrument](std::string_view name) {
        return std::count_if(instrument->strikes.begin(),
                             instrument->strikes.end(),
                             [name](const auto& row) { return row.strike_role == name; });
    };
    CHECK(role("cap") == 2);
    CHECK(role("floor") == 1);
    CHECK(instrument->strikes[0].strike_role == "cap");
    CHECK(instrument->strikes[0].sequence_number == 1);
    CHECK(instrument->strikes[0].bare_value->to_string() == "0.03");
    CHECK(instrument->strikes[1].sequence_number == 2);
    CHECK(instrument->strikes[1].bare_value->to_string() == "0.05");
    CHECK(instrument->strikes[2].strike_role == "floor");
    CHECK(instrument->strikes[2].sequence_number == 1);
    CHECK(instrument->strikes[2].bare_value->to_string() == "0.0121");

    // The writing returns each row to its own list, and the reads back agree.
    const auto back = rebuild(items[0]);
    const auto* round = std::get_if<ores::trading::domain::swap_instrument_data>(&back.instrument);
    REQUIRE(round != nullptr);
    REQUIRE(round->strikes.size() == 3);
    const auto round_role = [&round](std::string_view name, int sequence) {
        const auto found =
            std::find_if(round->strikes.begin(), round->strikes.end(), [&](const auto& row) {
                return row.strike_role == name && row.sequence_number == sequence;
            });
        return found == round->strikes.end() ? std::string{} : found->bare_value->to_string();
    };
    CHECK(round_role("cap", 1) == "0.03");
    CHECK(round_role("cap", 2) == "0.05");
    CHECK(round_role("floor", 1) == "0.0121");
}

TEST_CASE("a_trade_type_the_schema_does_not_name_is_not_written",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    ores::trading::messaging::trade_export_item item;
    item.ore_id = "unknown-1";
    item.anchor.trade_type = "NotATradeType";
    const auto document = portfolio_projection::reverse({item}, {});
    CHECK(document.trade.empty());
}

TEST_CASE("a_trade_whose_product_cannot_be_read_still_imports",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    // The schedule states a date the trading domain cannot hold, so the rates
    // family refuses the instrument. The trade is still the trade: its anchor
    // and its envelope are kept and the failure is logged.
    const std::string text = R"(<Portfolio>
  <Trade id="broken-1">
    <TradeType>Swap</TradeType>
    <Envelope><CounterParty>CPTY_C</CounterParty></Envelope>
    <SwapData>
      <LegData>
        <Payer>false</Payer>
        <LegType>Fixed</LegType>
        <Currency>EUR</Currency>
        <DayCounter>ACT/360</DayCounter>
        <Notionals><Notional>1000000</Notional></Notionals>
        <ScheduleData>
          <Dates><Dates><Date>16/12/2022</Date></Dates></Dates>
        </ScheduleData>
      </LegData>
    </SwapData>
  </Trade>
</Portfolio>)";

    const auto items = portfolio_projection::map(portfolio_projection::read(text));
    REQUIRE(items.size() == 1);
    CHECK(items[0].ore_id == "broken-1");
    CHECK(items[0].anchor.trade_type == "Swap");
    REQUIRE(items[0].envelope.has_value());
    CHECK(items[0].envelope->counter_party == std::optional<std::string>("CPTY_C"));
    CHECK(std::holds_alternative<std::monostate>(items[0].instrument));
}

// A trade holds one instrument kind, so it keeps that kind in a column its
// holder owns instead of a member for every kind. A trade sized for all of them
// costs more to create than to read.
TEST_CASE("a_portfolio_trade_is_not_sized_for_every_instrument_kind",
          "[ore][xml][portfolio][projection]") {
    CHECK(sizeof(pf::trade) < 1024);
    CHECK(sizeof(pf::component_trade) < 1024);
    CHECK(sizeof(pf::component_sub_trade) < 1024);
}

TEST_CASE("a_scripted_trade_states_what_the_population_writes_and_reads_it_back",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    /*
     * The population states the script's variables as rows and its underlyings
     * as rows of their own. The export writes them into the document's element
     * and the import reads them back into the same rows, so a scripted trade
     * survives the round trip rather than exporting as a name with nothing
     * behind it.
     */
    const auto scripted_data = [](std::string trade_type, std::string script_name) {
        ores::trading::domain::scripted_instrument row;
        row.identity.trade_type_code = trade_type;
        row.script_name = std::move(script_name);
        return ores::trading::domain::scripted_instrument_data{std::move(row)};
    };
    const auto add_parameter = [](ores::trading::domain::scripted_instrument_data& data,
                                  std::string name,
                                  std::string declared_type,
                                  std::string value) {
        ores::trading::domain::scripted_parameter parameter;
        parameter.sequence_number = static_cast<int>(data.parameters.size()) + 1;
        parameter.name = std::move(name);
        parameter.declared_type = std::move(declared_type);
        parameter.shape = "scalar";
        parameter.value = std::move(value);
        data.parameters.push_back(std::move(parameter));
    };
    const auto add_list = [](ores::trading::domain::scripted_instrument_data& data,
                             std::string name,
                             std::string declared_type,
                             const std::vector<std::string>& values) {
        ores::trading::domain::scripted_parameter parameter;
        parameter.sequence_number = static_cast<int>(data.parameters.size()) + 1;
        parameter.name = std::move(name);
        parameter.declared_type = std::move(declared_type);
        parameter.shape = "vector";
        const auto parameter_sequence_number = parameter.sequence_number;
        data.parameters.push_back(std::move(parameter));

        int sequence_number = 0;
        for (const auto& value : values) {
            ores::trading::domain::scripted_parameter_value member;
            member.parameter_sequence_number = parameter_sequence_number;
            member.sequence_number = ++sequence_number;
            member.value = value;
            data.parameter_values.push_back(std::move(member));
        }
    };
    const auto add_underlying = [](ores::trading::domain::scripted_instrument_data& data,
                                   std::string role,
                                   std::string underlying_type,
                                   std::string address) {
        ores::trading::domain::instrument_underlying underlying;
        underlying.underlying_role = std::move(role);
        int sequence_number = 0;
        for (const auto& row : data.instrument_underlyings)
            if (row.underlying_role == underlying.underlying_role)
                ++sequence_number;
        underlying.sequence_number = ++sequence_number;
        underlying.underlying_type = std::move(underlying_type);
        underlying.name = std::move(address);
        data.instrument_underlyings.push_back(std::move(underlying));
    };

    const auto rebuilt_data = [&](ores::trading::domain::scripted_instrument_data data) {
        ores::ore::xml::trade_import_item item;
        item.ore_id = "Scripted-1";
        item.anchor.trade_type = data.instrument.identity.trade_type_code;
        item.instrument = std::move(data);
        const auto round = rebuild(item);
        const auto* carrier =
            std::get_if<ores::trading::domain::scripted_instrument_data>(&round.instrument);
        REQUIRE(carrier != nullptr);
        return *carrier;
    };

    auto generic = scripted_data("ScriptedTrade", "AsianBasketOption");
    add_parameter(generic, "Notional", "Number", "1000000.00");
    add_parameter(generic, "PutCall", "Number", "Call");
    add_parameter(generic, "Underlying", "Index", equity_address_of("GB0005405286"));
    const auto generic_again = rebuilt_data(std::move(generic));
    CHECK(generic_again.instrument.script_name == "AsianBasketOption");
    REQUIRE(generic_again.parameters.size() == 3);
    CHECK(generic_again.parameters[0].name == "Notional");
    CHECK(generic_again.parameters[0].value == std::optional<std::string>("1000000.00"));
    CHECK(generic_again.parameters[1].name == "PutCall");
    CHECK(generic_again.parameters[1].value == std::optional<std::string>("Call"));
    CHECK(generic_again.parameters[2].name == "Underlying");
    CHECK(generic_again.parameters[2].value ==
          std::optional<std::string>(equity_address_of("GB0005405286")));

    // A body without the variable its result lands in is still the body the row
    // read, so the export states it rather than losing it.
    auto body_only = scripted_data("ScriptedTrade", "heston");
    body_only.instrument.script_body = "Payoff = 1;";
    const auto body_only_again = rebuilt_data(std::move(body_only));
    CHECK(body_only_again.instrument.script_body == "Payoff = 1;");
    CHECK(body_only_again.instrument.script_npv.empty());

    auto digital = scripted_data("DoubleDigitalOption", "DoubleDigitalOption");
    add_parameter(digital, "Expiry", "text", "2029-10-31");
    add_parameter(digital, "Settlement", "text", "2029-12-31");
    add_parameter(digital, "BinaryPayout", "decimal", "1000000");
    add_parameter(digital, "BinaryLevel1", "decimal", "1.05");
    add_parameter(digital, "BinaryLevel2", "decimal", "1.2");
    add_parameter(digital, "Type1", "text", "Equity");
    add_parameter(digital, "Type2", "text", "Equity");
    add_parameter(digital, "Position", "enumeration", "Long");
    add_parameter(digital, "PayCcy", "enumeration", "GBP");
    add_underlying(digital, "underlying1", "Equity", equity_address_of("GB0005405286"));
    add_underlying(digital, "underlying2", "Equity", equity_address_of("US0378331005"));
    const auto digital_again = rebuilt_data(std::move(digital));
    CHECK(digital_again.instrument.script_name == "DoubleDigitalOption");
    REQUIRE(digital_again.parameters.size() == 9);
    CHECK(digital_again.parameters[4].name == "BinaryLevel2");
    CHECK(digital_again.parameters[4].value == std::optional<std::string>("1.2"));
    CHECK(digital_again.parameters[7].name == "Position");
    CHECK(digital_again.parameters[7].value == std::optional<std::string>("Long"));
    REQUIRE(digital_again.instrument_underlyings.size() == 2);
    CHECK(digital_again.instrument_underlyings[0].underlying_role == "underlying1");
    CHECK(digital_again.instrument_underlyings[1].name == equity_address_of("US0378331005"));

    auto performance = scripted_data("PerformanceOption_01", "PerformanceOption_01");
    add_parameter(performance, "NotionalAmount", "decimal", "1000000");
    add_parameter(performance, "ParticipationRate", "decimal", "1.0");
    add_parameter(performance, "ValuationDate", "text", "2029-10-31");
    add_parameter(performance, "SettlementDate", "text", "2029-12-31");
    add_list(performance, "StrikePrices", "decimal", {"0.9", "1.0", "1.1"});
    add_parameter(performance, "Strike", "decimal", "0.9");
    add_parameter(performance, "Position", "enumeration", "Long");
    add_parameter(performance, "PayCcy", "enumeration", "GBP");
    add_underlying(performance, "underlying", "Commodity", commodity_address_of("NYMEX:CL"));
    add_underlying(performance, "underlying", "Commodity", commodity_address_of("ICE:B"));
    const auto performance_again = rebuilt_data(std::move(performance));
    CHECK(performance_again.instrument.script_name == "PerformanceOption_01");
    const auto prices =
        std::ranges::find_if(performance_again.parameters, [](const auto& parameter) {
            return parameter.name == "StrikePrices";
        });
    REQUIRE(prices != performance_again.parameters.end());
    CHECK(prices->shape == "vector");
    REQUIRE(performance_again.parameter_values.size() == 3);
    CHECK(performance_again.parameter_values[2].value == "1.1");
    REQUIRE(performance_again.instrument_underlyings.size() == 2);
    CHECK(performance_again.instrument_underlyings[1].underlying_role == "underlying");
    CHECK(performance_again.instrument_underlyings[1].sequence_number == 2);

    auto note = scripted_data("Autocallable_01", "Autocallable_01");
    add_parameter(note, "NotionalAmount", "decimal", "1000000");
    add_parameter(note, "DeterminationLevel", "decimal", "0.7");
    add_parameter(note, "TriggerLevel", "decimal", "1.0");
    add_parameter(note, "Position", "enumeration", "Long");
    add_parameter(note, "PayCcy", "enumeration", "EUR");
    {
        ores::trading::domain::scripted_parameter fixings;
        fixings.sequence_number = static_cast<int>(note.parameters.size()) + 1;
        fixings.name = "FixingDates";
        fixings.declared_type = "Event";
        fixings.shape = "schedule";
        note.parameters.push_back(fixings);
        ores::trading::domain::instrument_schedule rules;
        rules.owner_role = "scripted";
        rules.owner_number = fixings.sequence_number;
        rules.schedule_role = "event";
        rules.sequence_number = 1;
        rules.schedule_kind = "rules";
        rules.start_date = std::chrono::year{2027} / 1 / 4;
        rules.end_date = std::chrono::year{2029} / 1 / 4;
        rules.tenor = "1Y";
        rules.calendar = "TARGET";
        rules.convention = "F";
        rules.rule = "Forward";
        note.schedules.push_back(rules);
    }
    add_list(note, "AccumulationFactors", "decimal", {"0.05", "0.1", "0.15"});
    add_parameter(note, "Cap", "decimal", "0.15");
    add_underlying(note, "underlying", "Equity", equity_address_of("RIC:.SPX"));
    const auto note_again = rebuilt_data(std::move(note));
    CHECK(note_again.instrument.script_name == "Autocallable_01");
    const auto named = [&](const char* name) {
        return std::ranges::find_if(note_again.parameters,
                                    [&](const auto& parameter) { return parameter.name == name; });
    };
    REQUIRE(named("NotionalAmount") != note_again.parameters.end());
    CHECK(named("NotionalAmount")->value == std::optional<std::string>("1000000"));
    REQUIRE(named("FixingDates") != note_again.parameters.end());
    CHECK(named("FixingDates")->shape == "schedule");
    REQUIRE(note_again.schedules.size() == 1);
    CHECK(note_again.schedules[0].tenor == "1Y");
    REQUIRE(named("AccumulationFactors") != note_again.parameters.end());
    CHECK(note_again.parameter_values.size() == 3);
    REQUIRE(named("Cap") != note_again.parameters.end());
    REQUIRE(note_again.instrument_underlyings.size() == 1);
}

TEST_CASE("a_named_product_names_its_index_variable_by_address",
          "[ore][xml][portfolio][projection]") {
    auto lg(make_logger(test_suite));

    // The generic form capitalises its kinds and a named product states the type
    // attribute its schema gives the variable, so the same index kind is spelled
    // two ways. Both name market data, and inside the store that is an address.
    const std::string text = R"(<Portfolio>
  <Trade id="basket-1">
    <TradeType>ScriptedTrade</TradeType>
    <AsianBasketOptionData>
      <Expiry type="event">2025-08-31</Expiry>
      <Settlement type="event">2025-09-05</Settlement>
      <ObservationDates type="event">
        <ScheduleData>
          <Rules>
            <StartDate>2024-08-22</StartDate>
            <EndDate>2025-08-22</EndDate>
            <Tenor>1D</Tenor>
            <Calendar>US</Calendar>
            <Convention>F</Convention>
            <Rule>Forward</Rule>
          </Rules>
        </ScheduleData>
      </ObservationDates>
      <PutCall type="optionType">Call</PutCall>
      <LongShort type="longShort">Long</LongShort>
      <Notional type="number">1000000</Notional>
      <Strike type="number">5000</Strike>
      <Underlyings type="index">
        <Value>EQ-RIC:.SPX</Value>
        <Value>EQ-RIC:.STOXX50E</Value>
      </Underlyings>
      <Weights type="number"><Value>1.0</Value><Value>1.0</Value></Weights>
      <PayCcy type="currency">USD</PayCcy>
    </AsianBasketOptionData>
  </Trade>
</Portfolio>)";

    const auto items = portfolio_projection::map(portfolio_projection::read(text));
    REQUIRE(items.size() == 1);
    const auto* basket = scripted_data_of(items[0].instrument);
    REQUIRE(basket != nullptr);
    CHECK(basket->instrument.script_name == "AsianBasketOption");
    CHECK(basket->instrument.data_element == std::optional<std::string>("AsianBasketOptionData"));

    const auto parameter_named = [](const ores::trading::domain::scripted_instrument_data& data,
                                    std::string_view name) {
        const auto found = std::ranges::find_if(
            data.parameters, [&](const auto& parameter) { return parameter.name == name; });
        return found == data.parameters.end() ? nullptr : &*found;
    };
    const auto* underlyings = parameter_named(*basket, "Underlyings");
    REQUIRE(underlyings != nullptr);
    CHECK(underlyings->declared_type == "index");
    CHECK(underlyings->shape == "vector");

    const auto values_of = [&](const ores::trading::domain::scripted_instrument_data& data,
                               int parameter_sequence_number) {
        std::vector<std::string> values;
        for (const auto& row : data.parameter_values)
            if (row.parameter_sequence_number == parameter_sequence_number)
                values.push_back(row.value);
        return values;
    };
    const auto addresses = values_of(*basket, underlyings->sequence_number);
    REQUIRE(addresses.size() == 2);
    for (const auto& address : addresses) {
        INFO("value " << address);
        CHECK(address.starts_with("oresmd://equity/"));
    }
    CHECK(addresses[0].find("RIC:.SPX") != std::string::npos);

    // The address is the store's spelling; the document keeps ORE's.
    const auto written = portfolio_projection::write(exported(items[0]));
    CHECK(written.find("<Value>EQ-RIC:.SPX</Value>") != std::string::npos);
    const auto basket_round = rebuild(items[0]);
    const auto* again = scripted_data_of(basket_round.instrument);
    REQUIRE(again != nullptr);
    const auto* again_underlyings = parameter_named(*again, "Underlyings");
    REQUIRE(again_underlyings != nullptr);
    CHECK(values_of(*again, again_underlyings->sequence_number) == addresses);
}

TEST_CASE("a_cms_spread_leg_holds_both_indices_as_addresses_and_writes_them_back",
          "[ore][xml][portfolio][projection]") {
    const auto text =
        ores::platform::filesystem::file::read_content(ores::testing::project_root::resolve(
            "external/ore/examples/Products/Example_Trades/IR_Swap_CMS_Spread_Option.xml"));
    const auto items = portfolio_projection::map(portfolio_projection::read(text));
    REQUIRE(!items.empty());

    const auto legs_of = [](const auto& item) {
        const auto* swap =
            std::get_if<ores::trading::domain::swap_instrument_data>(&item.instrument);
        REQUIRE(swap != nullptr);
        return swap->legs;
    };
    const auto spread_leg = [](const auto& legs) {
        const auto found = std::ranges::find_if(
            legs, [](const auto& leg) { return leg.floating_index_code_2.has_value(); });
        REQUIRE(found != legs.end());
        return *found;
    };

    const auto leg = spread_leg(legs_of(items[0]));
    CHECK(leg.floating_index_code.starts_with("oresmd://"));
    CHECK(leg.floating_index_code_2->starts_with("oresmd://"));

    const auto again = spread_leg(legs_of(rebuild(items[0])));
    CHECK(again.floating_index_code == leg.floating_index_code);
    CHECK(again.floating_index_code_2 == leg.floating_index_code_2);
}

TEST_CASE("an_underlying_name_returns_from_its_address_as_ore_states_it",
          "[ore][xml][portfolio][projection]") {
    using namespace ores::ore::domain;
    for (const auto* name : {"MSFT.O", ".NDX", "GB0005405286", "RIC:.SPX", "ISIN:XS0982710740"}) {
        INFO(name);
        const auto address = underlying_address_of("Equity", name);
        CHECK(address.starts_with("oresmd://"));
        CHECK(underlying_name_of(address) == name);
    }
    for (const auto* name : {"NYMEX:CL", "ICE:B"}) {
        INFO(name);
        const auto address = commodity_address_of(name);
        CHECK(address.starts_with("oresmd://"));
        CHECK(underlying_name_of(address) == name);
    }
    const auto fx = fx_address_of("ECB-EUR-USD");
    CHECK(fx.starts_with("oresmd://"));
    CHECK(underlying_name_of(fx) == "ECB-EUR-USD");
    CHECK(underlying_name_of(underlying_address_of("Bond", "ISIN:XS0982710740")) ==
          "ISIN:XS0982710740");
}

TEST_CASE("an_underlying_no_family_can_hold_stays_as_written",
          "[ore][xml][portfolio][projection]") {
    using namespace ores::ore::domain;
    CHECK(underlying_address_of("Equity", "") == "");
    CHECK(underlying_address_of("Equity", "EQ-MSFT.O") == "EQ-MSFT.O");
    CHECK(underlying_address_of("InterestRate", "EUR-EURIBOR-6M") == "EUR-EURIBOR-6M");
    CHECK(underlying_name_of("EQ-MSFT.O") == "EQ-MSFT.O");
}

TEST_CASE("every_converted_index_column_of_ores_product_portfolio_holds_an_address",
          "[ore][xml][portfolio][projection]") {
    const auto text = ores::platform::filesystem::file::read_content(
        ores::testing::project_root::resolve("external/ore/examples/Products/Input/portfolio.xml"));
    ores::trading::domain::instrument_batch batch;
    for (const auto& item : portfolio_projection::map(portfolio_projection::read(text)))
        ores::trading::domain::append_instrument(batch, item.instrument);

    std::map<std::string, std::pair<int, std::vector<std::string>>> columns;
    const auto hold = [&](const std::string& column, const auto& value) {
        std::string stored;
        if constexpr (requires { value.has_value(); }) {
            if (!value)
                return;
            stored = *value;
        } else {
            stored = value;
        }
        if (stored.empty())
            return;
        auto& [seen, names] = columns[column];
        ++seen;
        if (!stored.starts_with("oresmd://"))
            names.push_back(stored);
    };
    for (const auto& row : batch.swap_legs) {
        hold("swap_leg.fx_index", row.fx_index);
        hold("swap_leg.fx_reset_fx_index", row.fx_reset_fx_index);
    }
    for (const auto& row : batch.swap_leg_indexings)
        hold("swap_leg_indexing.index_name", row.index_name);
    for (const auto& row : batch.fx_asian_forward_instruments)
        hold("fx_asian_forward.fx_index", row.fx_index);
    for (const auto& row : batch.fx_forward_instruments)
        hold("fx_forward.settlement_fx_index", row.settlement_fx_index);
    for (const auto& row : batch.instrument_options)
        hold("instrument_option.settlement_fx_index", row.settlement_fx_index);
    for (const auto& row : batch.bond_trs)
        hold("bond_trs.funding_index", row.funding_index);
    for (const auto& row : batch.bond_repos)
        hold("bond_repo.repo_index", row.repo_index);
    for (const auto& row : batch.instrument_underlyings)
        if (row.underlying_type == "Equity" || row.underlying_type == "Commodity" ||
            row.underlying_type == "FX" || row.underlying_type == "Bond")
            hold("instrument_underlying.name", row.name);
    for (const auto& row : batch.equity_option_instruments)
        hold("equity_option.underlying_name", row.underlying_name);
    for (const auto& row : batch.equity_barrier_option_instruments) {
        hold("equity_barrier_option.underlying_name", row.underlying_name);
        hold("equity_barrier_option.barrier_fixing_index", row.barrier_fixing_index);
    }
    for (const auto& row : batch.equity_basket_constituents)
        hold("equity_basket_constituent.underlying_name", row.underlying_name);
    for (const auto& row : batch.commodity_instruments)
        hold("commodity.commodity_code", row.commodity_code);
    for (const auto& row : batch.commodity_basket_constituents)
        hold("commodity_basket_constituent.underlying_code", row.underlying_code);
    for (const auto& row : batch.fx_barrier_option_instruments)
        hold("fx_barrier_option.barrier_fixing_index", row.barrier_fixing_index);
    for (const auto& row : batch.scripted_parameters)
        if (row.declared_type == "Index")
            hold("scripted_parameter.index", row.value);

    // A TaRF names its pair as an underlying, without the FX- prefix, and the
    // underlying mapper reads that into an address too.
    for (const auto& [column, result] : columns) {
        INFO(column);
        CHECK(result.second.empty());
    }
    for (const auto* column : {"swap_leg.fx_index",
                               "swap_leg.fx_reset_fx_index",
                               "swap_leg_indexing.index_name",
                               "fx_asian_forward.fx_index",
                               "fx_forward.settlement_fx_index",
                               "instrument_underlying.name",
                               "equity_option.underlying_name",
                               "commodity.commodity_code"}) {
        INFO(column);
        CHECK(columns.contains(column));
    }
}

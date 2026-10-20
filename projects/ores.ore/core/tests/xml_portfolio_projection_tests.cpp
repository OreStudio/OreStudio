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
    <CommoditySwapData>
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
    </CommoditySwapData>
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

TEST_CASE("a_portfolio_document_projects_an_fx_forward_and_an_fx_swap", tags) {
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

TEST_CASE("a_portfolio_document_projects_an_fx_barrier_option_and_an_fx_touch_option", tags) {
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

TEST_CASE("a_portfolio_document_projects_the_rest_of_the_fx_products", tags) {
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
          <Leverage>2</Leverage>
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
    CHECK(accumulator_row.underlying_code == "EUR-USD");
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
    CHECK(accumulator->range_bounds[0].leverage == std::optional<double>(2.0));
    CHECK(accumulator->range_bounds[0].strike_adjustment->to_string() == "0.001");

    const auto* variance =
        std::get_if<ores::trading::domain::fx_instrument_data>(&items[3].instrument);
    REQUIRE(variance != nullptr);
    REQUIRE(variance->variance_swaps.size() == 1);
    CHECK(variance->variance_swaps[0].underlying_code == "EUR-USD");
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
    CHECK(accumulator_round->accumulators[0].underlying_code == "EUR-USD");
    CHECK(accumulator_round->accumulators[0].long_short == "Long");
    CHECK(accumulator_round->accumulators[0].strike.to_string() == "1.05");
    CHECK(accumulator_round->accumulators[0].knock_out_barrier->to_string() == "1.2");
    REQUIRE(accumulator_round->range_bounds.size() == 1);
    CHECK(accumulator_round->range_bounds[0].range_from->to_string() == "1");
    CHECK(accumulator_round->range_bounds[0].leverage == std::optional<double>(2.0));

    const auto variance_again = rebuild(items[3]);
    const auto* variance_round =
        std::get_if<ores::trading::domain::fx_instrument_data>(&variance_again.instrument);
    REQUIRE(variance_round != nullptr);
    REQUIRE(variance_round->variance_swaps.size() == 1);
    CHECK(variance_round->variance_swaps[0].underlying_code == "EUR-USD");
    CHECK(variance_round->variance_swaps[0].strike == 0.08);
    CHECK(variance_round->variance_swaps[0].notional.to_string() == "2000000");
    CHECK(variance_round->variance_swaps[0].end_date == date_of(2025, 1, 15));
}

TEST_CASE("a_portfolio_document_projects_a_bond_and_its_fixed_leg", tags) {
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

TEST_CASE("a_portfolio_document_projects_a_callable_bond_and_its_call_dates", tags) {
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
        <Prices><Price>1</Price></Prices>
        <PriceTypes><PriceType>Clean</PriceType></PriceTypes>
        <IncludeAccruals><IncludeAccrual>true</IncludeAccrual></IncludeAccruals>
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
}

TEST_CASE("a_portfolio_document_projects_a_bond_option_and_its_redemption", tags) {
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

TEST_CASE("a_portfolio_document_projects_a_credit_default_swap_and_its_premium_leg", tags) {
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
    CHECK(instrument->instrument.reference_entity == "ACME");
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
    CHECK(round->instrument.reference_entity == "ACME");
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

TEST_CASE("a_portfolio_document_projects_a_credit_linked_swap_and_its_payment_groups", tags) {
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
    CHECK(instrument->instrument.reference_entity == "ACME-SR");
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
    CHECK(instrument->legs[1].floating_index_code == "USD-LIBOR-3M");

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
    CHECK(round->instrument.reference_entity == "ACME-SR");
    CHECK(round->instrument.start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2024, 7, 1)));
    CHECK(round->instrument.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2029, 7, 1)));
    REQUIRE(round->credit_linked_swap_instruments.size() == 1);
    CHECK(round->credit_linked_swap_instruments[0].recovery_rate == std::optional<double>(0.35));
    REQUIRE(round->legs.size() == 2);
    CHECK(round->legs[0].leg_group == std::optional<std::string>("independent"));
    CHECK(round->legs[1].leg_group == std::optional<std::string>("contingent"));
    CHECK(round->legs[1].floating_index_code == "USD-LIBOR-3M");
    REQUIRE(round->leg_rates.size() == 2);
    CHECK(round->leg_rates[1].value.to_string() == "0.0015");
}

TEST_CASE("a_portfolio_document_projects_an_index_swap_and_a_synthetic_cdo", tags) {
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
    CHECK(index_swap->instrument.reference_entity == "ITRAXX-S28-5Y");
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
    CHECK(cdo->instrument.reference_entity == "CDO-1");
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

TEST_CASE("a_portfolio_document_projects_a_risk_participation_agreement", tags) {
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
    CHECK(instrument->instrument.reference_entity == "ACME");
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
    CHECK(round->instrument.reference_entity == "ACME");
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

TEST_CASE("a_portfolio_document_projects_an_index_swap_option_and_its_wrapped_swap", tags) {
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
    CHECK(instrument->instrument.reference_entity == "ITRAXX-S28-5Y");

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
    CHECK(round->instrument.reference_entity == "ITRAXX-S28-5Y");
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

TEST_CASE("a_portfolio_document_projects_an_equity_option_and_its_underlying", tags) {
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
    CHECK(option->underlying_name == "RIC:.SPX");
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
    CHECK(round->underlying_name == "RIC:.SPX");
    CHECK(round->currency == "USD");
    CHECK(round->notional.to_string() == "775");
    CHECK(round->option_type == "Call");
    CHECK(round->exercise_type == "European");
    CHECK(round->expiry_date == date_of(2025, 6, 20));
    CHECK(round->strike.to_string() == "2800.5");
}

TEST_CASE("a_portfolio_document_projects_an_equity_position_and_its_option_entries", tags) {
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
    CHECK(plain->underlying_name == ".SPX");
    CHECK(position->underlyings.empty());

    const auto* entries =
        std::get_if<ores::trading::domain::equity_instrument_data>(&items[1].instrument);
    REQUIRE(entries != nullptr);
    const auto* held =
        std::get_if<ores::trading::domain::equity_position_instrument>(&entries->instrument);
    REQUIRE(held != nullptr);
    CHECK(held->identity.trade_type_code == "EquityOptionPosition");
    CHECK(held->quantity == 1000.0);
    CHECK(held->underlying_name == ".SPX");
    REQUIRE(entries->underlyings.size() == 2);
    CHECK(entries->underlyings[0].sequence_number == 1);
    CHECK(entries->underlyings[0].underlying_name == ".SPX");
    CHECK(entries->underlyings[0].strike.to_string() == "3300");
    CHECK(entries->underlyings[0].weight == std::optional<double>(0.5));
    CHECK(entries->underlyings[0].long_short == "Long");
    CHECK(entries->underlyings[0].option_type == "Call");
    CHECK(entries->underlyings[0].exercise_type == "European");
    CHECK(entries->underlyings[0].settlement_type == "Cash");
    CHECK(entries->underlyings[1].sequence_number == 2);
    CHECK(entries->underlyings[1].underlying_name == ".SX5E");
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
    CHECK(plain_round->underlying_name == ".SPX");
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
    CHECK(round_data->underlyings[0].underlying_name == ".SPX");
    CHECK(round_data->underlyings[0].strike.to_string() == "3300");
    CHECK(round_data->underlyings[0].weight == std::optional<double>(0.5));
    CHECK(round_data->underlyings[0].settlement_type == "Cash");
    CHECK(round_data->underlyings[1].sequence_number == 2);
    CHECK(round_data->underlyings[1].underlying_name == ".SX5E");
    CHECK(round_data->underlyings[1].strike.to_string() == "3400");
    CHECK(round_data->underlyings[1].settlement_type == "Physical");
}

TEST_CASE("a_portfolio_document_projects_an_equity_barrier_option_and_an_accumulator", tags) {
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
    CHECK(barrier->underlying_name == "RIC:.SPX");
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
    CHECK(accumulator->underlying_name == ".STOXX50");
    CHECK(accumulator->currency == "EUR");
    CHECK(accumulator->fixing_amount.to_string() == "30");
    CHECK(accumulator->strike.to_string() == "4000");
    CHECK(accumulator->start_date == date_of(2025, 2, 5));
    REQUIRE(accumulator->knock_out_level.has_value());
    CHECK(accumulator->knock_out_level->to_string() == "3500");
    CHECK(accumulator->range_bounds_json ==
          R"([{"RangeTo":4000,"Leverage":1},{"RangeFrom":4000,"Leverage":2}])");

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
    CHECK(barrier_round->underlying_name == "RIC:.SPX");
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
    CHECK(round->underlying_name == ".STOXX50");
    CHECK(round->currency == "EUR");
    CHECK(round->fixing_amount.to_string() == "30");
    CHECK(round->strike.to_string() == "4000");
    CHECK(round->start_date == date_of(2025, 2, 5));
    CHECK(round->range_bounds_json ==
          R"([{"RangeTo":4000,"Leverage":1},{"RangeFrom":4000,"Leverage":2}])");
}

TEST_CASE("a_portfolio_document_projects_a_commodity_forward_and_a_commodity_swap", tags) {
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
    CHECK(forward.commodity_code == "NGAS");
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
    CHECK(swap.commodity_code == "NGAS");
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
    CHECK(forward_round.commodity_code == "NGAS");
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
    CHECK(swap_round.commodity_code == "NGAS");
    CHECK(swap_round.currency == "USD");
    CHECK(swap_round.quantity == 1000.0);
    CHECK(swap_round.start_date ==
          std::optional<std::chrono::year_month_day>(date_of(2025, 1, 15)));
    CHECK(swap_round.maturity_date ==
          std::optional<std::chrono::year_month_day>(date_of(2026, 1, 15)));
}

TEST_CASE("a_portfolio_document_projects_a_commodity_average_price_option_and_a_basket", tags) {
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
    CHECK(apo.commodity_code == "WTI");
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
    CHECK(basket.commodity_code == "GOLD");
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
    CHECK(basket_data->constituents[0].underlying_code == "GOLD");
    CHECK(basket_data->constituents[0].weight == std::optional<double>(0.6));
    CHECK(basket_data->constituents[0].change_reason_code == "system.external_data_import");
    CHECK(basket_data->constituents[1].sequence_number == 2);
    CHECK(basket_data->constituents[1].underlying_code == "SILVER");
    CHECK(basket_data->constituents[1].weight == std::optional<double>(0.4));

    const auto apo_again = rebuild(items[0]);
    const auto* apo_round_data =
        std::get_if<ores::trading::domain::commodity_instrument_data>(&apo_again.instrument);
    REQUIRE(apo_round_data != nullptr);
    const auto& apo_round = apo_round_data->instrument;
    CHECK(apo_again.anchor.trade_type == "CommodityAveragePriceOption");
    CHECK(apo_round.identity.trade_type_code == "CommodityAveragePriceOption");
    CHECK(apo_round.commodity_code == "WTI");
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
    CHECK(basket_round.commodity_code == "GOLD");
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
    CHECK(basket_round_data->constituents[0].underlying_code == "GOLD");
    CHECK(basket_round_data->constituents[0].weight == std::optional<double>(0.6));
    CHECK(basket_round_data->constituents[1].sequence_number == 2);
    CHECK(basket_round_data->constituents[1].underlying_code == "SILVER");
    CHECK(basket_round_data->constituents[1].weight == std::optional<double>(0.4));
}

TEST_CASE("a_portfolio_document_projects_a_composite_trade_and_its_components", tags) {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(composite_document_text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 3);
    CHECK(items[0].anchor.trade_type == "CompositeTrade");
    CHECK(items[1].anchor.trade_type == "TotalReturnSwap");
    CHECK(items[2].anchor.trade_type == "ContractForDifference");

    const auto* composite =
        std::get_if<ores::trading::domain::composite_instrument_data>(&items[0].instrument);
    REQUIRE(composite != nullptr);
    CHECK(composite->instrument.identity.trade_type_code == "CompositeTrade");
    CHECK(composite->instrument.description == "ACME-BASKET");
    REQUIRE(composite->legs.size() == 2);
    CHECK(composite->legs[0].identity.leg_sequence == 1);
    CHECK(composite->legs[0].constituent_trade_id == "component-swap");
    CHECK(composite->legs[0].audit.change_reason_code == "system.external_data_import");
    CHECK(composite->legs[1].identity.leg_sequence == 2);
    CHECK(composite->legs[1].constituent_trade_id == "component-fx");
    CHECK(composite->legs[1].audit.change_reason_code == "system.external_data_import");

    const auto back = rebuild(items[0]);
    CHECK(back.anchor.trade_type == "CompositeTrade");
    const auto* round =
        std::get_if<ores::trading::domain::composite_instrument_data>(&back.instrument);
    REQUIRE(round != nullptr);
    CHECK(round->instrument.identity.trade_type_code == "CompositeTrade");
    CHECK(round->instrument.description == "ACME-BASKET");
    // The trading domain keeps a component's id and the basket name, not the
    // component's product, so the write states neither the components nor
    // anything they held. The loss is stated here rather than left implicit.
    CHECK(round->legs.empty());

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

TEST_CASE("a_portfolio_document_projects_a_total_return_swap_and_a_contract_for_difference", tags) {
    auto lg(make_logger(test_suite));

    const auto document = portfolio_projection::read(composite_document_text);
    const auto items = portfolio_projection::map(document);
    REQUIRE(items.size() == 3);

    const auto* trs =
        std::get_if<ores::trading::domain::composite_instrument_data>(&items[1].instrument);
    REQUIRE(trs != nullptr);
    CHECK(trs->instrument.identity.trade_type_code == "TotalReturnSwap");
    CHECK(trs->instrument.description == "EquityPosition");
    CHECK(trs->legs.empty());

    const auto* cfd =
        std::get_if<ores::trading::domain::composite_instrument_data>(&items[2].instrument);
    REQUIRE(cfd != nullptr);
    CHECK(cfd->instrument.identity.trade_type_code == "ContractForDifference");
    CHECK(cfd->instrument.description == "Swap");
    CHECK(cfd->legs.empty());

    const auto trs_again = rebuild(items[1]);
    REQUIRE(std::get_if<ores::trading::domain::composite_instrument_data>(&trs_again.instrument) !=
            nullptr);
    CHECK(trs_again.anchor.trade_type == "TotalReturnSwap");
    CHECK(std::get<ores::trading::domain::composite_instrument_data>(trs_again.instrument)
              .instrument.identity.trade_type_code == "TotalReturnSwap");

    const auto cfd_again = rebuild(items[2]);
    REQUIRE(std::get_if<ores::trading::domain::composite_instrument_data>(&cfd_again.instrument) !=
            nullptr);
    CHECK(cfd_again.anchor.trade_type == "ContractForDifference");
    CHECK(std::get<ores::trading::domain::composite_instrument_data>(cfd_again.instrument)
              .instrument.identity.trade_type_code == "ContractForDifference");
}

TEST_CASE("a_written_product_states_the_terms_the_schema_requires", tags) {
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

    // The scripted row states no parameter of its own, so the data element the
    // schema requires carries the script's own result and nothing else.
    const auto* scripted =
        std::get_if<ores::trading::domain::scripted_instrument>(&items[0].instrument);
    REQUIRE(scripted != nullptr);
    CHECK(scripted->script_name == "heston");
    CHECK(scripted->parameters_json == R"({"NPV":"Option"})");
    CHECK(scripted->underlyings_json.empty());

    // The flat table has no column for an underlying's type, so the type rides
    // in the parameters and comes back out of the document it is written to.
    const auto* double_digital =
        std::get_if<ores::trading::domain::scripted_instrument>(&items[1].instrument);
    REQUIRE(double_digital != nullptr);
    CHECK(double_digital->underlyings_json == R"(["TR20H-EUR-USD","NYMEX:CL"])");
    CHECK(double_digital->parameters_json.find(R"("Underlying2Type":"Commodity")") !=
          std::string::npos);

    const auto double_digital_round = rebuild(items[1]);
    const auto* double_digital_again =
        std::get_if<ores::trading::domain::scripted_instrument>(&double_digital_round.instrument);
    REQUIRE(double_digital_again != nullptr);
    CHECK(double_digital_again->underlyings_json == double_digital->underlyings_json);
    CHECK(double_digital_again->parameters_json == double_digital->parameters_json);

    // The cliquet row keeps no frequency, so the schedule element the schema
    // requires is stated without a rule rather than dropped.
    const auto* cliquet_data =
        std::get_if<ores::trading::domain::equity_instrument_data>(&items[2].instrument);
    REQUIRE(cliquet_data != nullptr);
    const auto* cliquet =
        std::get_if<ores::trading::domain::equity_option_instrument>(&cliquet_data->instrument);
    REQUIRE(cliquet != nullptr);
    CHECK(cliquet->underlying_name == "RIC:.SPX");
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
    CHECK(digital->underlying_name == "RIC:.SPX");
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
    CHECK(worst_of->basket_json == R"(["RIC:.SPX","RIC:.NDX"])");
    CHECK(worst_of->initial_prices_json == "[3200,11500]");
    CHECK(worst_of->knock_out_levels_json == "[3500]");
    CHECK(worst_of->fixed_trigger_levels_json == "[3400]");

    const auto worst_of_round = rebuild(items[4]);
    const auto* worst_of_again_data =
        std::get_if<ores::trading::domain::equity_instrument_data>(&worst_of_round.instrument);
    REQUIRE(worst_of_again_data != nullptr);
    const auto* worst_of_again = std::get_if<ores::trading::domain::equity_swap_instrument>(
        &worst_of_again_data->instrument);
    REQUIRE(worst_of_again != nullptr);
    CHECK(worst_of_again->basket_json == worst_of->basket_json);
    CHECK(worst_of_again->initial_prices_json == worst_of->initial_prices_json);
    CHECK(worst_of_again->knock_out_levels_json == worst_of->knock_out_levels_json);
    CHECK(worst_of_again->fixed_trigger_levels_json == worst_of->fixed_trigger_levels_json);
}

TEST_CASE("a_collar_keeps_both_arms_of_its_strike_lists", tags) {
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


TEST_CASE("a_trade_type_the_schema_does_not_name_is_not_written", tags) {
    auto lg(make_logger(test_suite));

    ores::trading::messaging::trade_export_item item;
    item.ore_id = "unknown-1";
    item.anchor.trade_type = "NotATradeType";
    const auto document = portfolio_projection::reverse({item}, {});
    CHECK(document.trade.empty());
}

TEST_CASE("a_trade_whose_product_cannot_be_read_still_imports", tags) {
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
TEST_CASE("a_portfolio_trade_is_not_sized_for_every_instrument_kind", tags) {
    CHECK(sizeof(pf::trade) < 1024);
    CHECK(sizeof(pf::component_trade) < 1024);
    CHECK(sizeof(pf::component_sub_trade) < 1024);
}

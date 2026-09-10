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
#include "ores.ore.core/domain/bond_instrument_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/trade_mapper.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <utility>
#include <vector>

/**
 * @file xml_bond_fact_mapper_roundtrip_tests.cpp
 * @brief Fact-row fidelity tests for the bond mapper: the repo leg, the
 * TRS funding leg, the future row, the ascot row, the whole exercise
 * date list and the issue's child rows.
 *
 * The example documents state one exercise date, and none of them
 * states a call schedule, a conversion ratio list or a future, so the
 * shapes the acceptance names are authored inline, after the
 * reference-data documents that carry them.
 */

namespace {

const std::string_view test_suite("ores.ore.bond.fact.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][bond][fact]");

using ores::ore::domain::bond_instrument_mapper;
using ores::ore::domain::oreTradeType;
using ores::ore::domain::portfolio;
using ores::ore::domain::trade_mapper;
using ores::trading::domain::bond_instrument_data;
using namespace ores::logging;
using Catch::Approx;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve("external/ore/examples/Products/Example_Trades/" +
                                                filename);
}

bond_instrument_data map_example(const std::string& filename, std::size_t index) {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(p.Trade.size() > index);
    auto r = trade_mapper::map_bond_instrument(p.Trade[index]);
    REQUIRE(r.has_value());
    return *r;
}

bond_instrument_data map_inline(const std::string& xml) {
    portfolio p;
    ores::ore::domain::load_data(xml, p);
    REQUIRE(p.Trade.size() == 1);
    auto r = trade_mapper::map_bond_instrument(p.Trade.front());
    REQUIRE(r.has_value());
    return *r;
}

} // namespace

// =============================================================================
// The coupon leg keeps its own schedule
// =============================================================================

TEST_CASE("the_coupon_leg_keeps_a_start_date_the_issue_date_does_not_hold", tags) {
    auto lg(make_logger(test_suite));

    // The issue date and the leg's schedule start are two data, and the
    // corpus holds documents where they differ. Export used to rebuild
    // the schedule from the issue terms, which wrote the issue date as
    // the schedule start. An xsd::optional assigned to the plain date
    // member made that worse: it compiles as operator=(char) and stores
    // a control byte. The dates here differ, so a rebuild shows up.
    const std::string xml = R"(
<Portfolio>
  <Trade id="Bond_Schedule_Mirror">
    <TradeType>Bond</TradeType>
    <BondData>
      <SecurityId>ISIN:XS1234567890</SecurityId>
      <IssueDate>2025-02-01</IssueDate>
      <LegData>
        <LegType>Fixed</LegType>
        <Payer>false</Payer>
        <Currency>EUR</Currency>
        <Notionals>
          <Notional>1000000</Notional>
        </Notionals>
        <DayCounter>ACT/ACT</DayCounter>
        <PaymentConvention>F</PaymentConvention>
        <ScheduleData>
          <Rules>
            <StartDate>2025-02-03</StartDate>
            <EndDate>2035-02-03</EndDate>
            <Tenor>1Y</Tenor>
            <Calendar>EUR</Calendar>
            <Convention>MF</Convention>
            <TermConvention>MF</TermConvention>
            <Rule>Forward</Rule>
            <EndOfMonth>true</EndOfMonth>
          </Rules>
        </ScheduleData>
        <FixedLegData>
          <Rates>
            <Rate>0.05</Rate>
          </Rates>
        </FixedLegData>
      </LegData>
    </BondData>
  </Trade>
</Portfolio>
)";
    const auto r = map_inline(xml);
    CHECK(r.issue.issue_date == "2025-02-01");
    CHECK(r.issue.maturity_date == "2035-02-03");
    CHECK(r.issue.coupon_frequency_code == "1Y");

    REQUIRE(r.bond_leg.schedule.rules.size() == 1);
    const auto& carried = r.bond_leg.schedule.rules.front();
    CHECK(carried.start_date == "2025-02-03");
    CHECK(carried.calendar == "EUR");
    CHECK(carried.term_convention == "MF");
    CHECK(carried.rule == "Forward");
    REQUIRE(carried.end_of_month);
    CHECK(*carried.end_of_month);
    REQUIRE(r.bond_leg.leg_type);
    CHECK(*r.bond_leg.leg_type == "Fixed");
    REQUIRE(r.bond_leg.currency);
    CHECK(*r.bond_leg.currency == "EUR");
    REQUIRE(r.bond_leg.day_counter);
    CHECK(*r.bond_leg.day_counter == "ACT/ACT");
    REQUIRE(r.bond_leg.payment_convention);
    CHECK(*r.bond_leg.payment_convention == "F");
    REQUIRE(r.bond_leg.payer);
    CHECK(!*r.bond_leg.payer);

    const auto rt = bond_instrument_mapper::reverse_bond(r);
    REQUIRE(rt.BondData);
    REQUIRE(rt.BondData->LegData.size() == 1);
    REQUIRE(rt.BondData->LegData.front().ScheduleData);
    REQUIRE(rt.BondData->LegData.front().ScheduleData->Rules.size() == 1);
    const auto& rule = rt.BondData->LegData.front().ScheduleData->Rules.front();
    CHECK(rule.StartDate == "2025-02-03");
    REQUIRE(rule.EndDate);
    CHECK(std::string(*rule.EndDate) == "2035-02-03");
    CHECK(std::string(rule.Tenor) == "1Y");
    REQUIRE(rule.Calendar);
    CHECK(std::string(*rule.Calendar) == "EUR");
    REQUIRE(rule.TermConvention);
    CHECK(to_string(*rule.TermConvention) == "MF");
    REQUIRE(rule.Rule);
    CHECK(to_string(*rule.Rule) == "Forward");
    CHECK(rt.BondData->LegData.front().LegType == ores::ore::domain::legType::Fixed);

    BOOST_LOG_SEV(lg, info)
        << "The coupon leg keeps a start date the issue date does not hold.";
}

// =============================================================================
// An element the document states empty is not an element it omits
// =============================================================================

TEST_CASE("an_empty_schedule_element_survives_the_round_trip", tags) {
    auto lg(make_logger(test_suite));

    // Every member here is optional in the schema, and the corpus states
    // some of them empty. Presence is itself data: a document that states
    // an empty FirstDate, one that omits it and one that states a date are
    // three documents, and the container has to tell them apart.
    const std::string xml = R"(
<Portfolio>
  <Trade id="Bond_Empty_Elements">
    <TradeType>Bond</TradeType>
    <BondData>
      <SecurityId>ISIN:XS1234567890</SecurityId>
      <LegData>
        <LegType>Fixed</LegType>
        <Payer>false</Payer>
        <ScheduleData>
          <Rules>
            <StartDate>2025-02-03</StartDate>
            <Tenor>1Y</Tenor>
            <Convention>MF</Convention>
            <EndOfMonth/>
            <FirstDate/>
            <LastDate/>
          </Rules>
        </ScheduleData>
      </LegData>
    </BondData>
  </Trade>
</Portfolio>
)";
    const auto r = map_inline(xml);
    REQUIRE(r.bond_leg.schedule.rules.size() == 1);
    const auto& carried = r.bond_leg.schedule.rules.front();
    REQUIRE(carried.first_date);
    CHECK(carried.first_date->empty());
    REQUIRE(carried.last_date);
    CHECK(carried.last_date->empty());
    CHECK(!carried.end_date);
    CHECK(!carried.calendar);
    // The generated reader pre-fills bool_::Y for a bool element it finds
    // empty, so an empty EndOfMonth reads as on.
    REQUIRE(carried.end_of_month);
    CHECK(*carried.end_of_month);

    const auto rt = bond_instrument_mapper::reverse_bond(r);
    REQUIRE(rt.BondData);
    REQUIRE(rt.BondData->LegData.size() == 1);
    REQUIRE(rt.BondData->LegData.front().ScheduleData);
    REQUIRE(rt.BondData->LegData.front().ScheduleData->Rules.size() == 1);
    const auto& rule = rt.BondData->LegData.front().ScheduleData->Rules.front();
    CHECK(rule.FirstDate == "");
    CHECK(rule.LastDate == "");
    CHECK(!rule.EndDate);
    CHECK(!rule.Calendar);
    REQUIRE(rule.EndOfMonth);
    CHECK(to_string(*rule.EndOfMonth) == "Y");

    BOOST_LOG_SEV(lg, info) << "An empty schedule element survives the round trip.";
}

TEST_CASE("a_legs_payment_terms_survive_the_round_trip", tags) {
    auto lg(make_logger(test_suite));

    // The payment terms, the payment calendar and the two flags have no
    // column in the nine tables, so the leg is their only home on the
    // mapper path. LegType belongs to the same group: a leg that has no
    // fact row used to come back as Fixed whatever the document said.
    const std::string xml = R"(
<Portfolio>
  <Trade id="Bond_Leg_Terms">
    <TradeType>Bond</TradeType>
    <BondData>
      <SecurityId>ISIN:XS1234567890</SecurityId>
      <LegData>
        <LegType>Floating</LegType>
        <Payer>true</Payer>
        <Currency>GBP</Currency>
        <PaymentConvention>MF</PaymentConvention>
        <PaymentLag>2D</PaymentLag>
        <NotionalPaymentLag>3</NotionalPaymentLag>
        <PaymentCalendar>GBP</PaymentCalendar>
        <DayCounter>A365</DayCounter>
        <LastPeriodDayCounter>ACT/ACT</LastPeriodDayCounter>
        <StrictNotionalDates>true</StrictNotionalDates>
      </LegData>
    </BondData>
  </Trade>
</Portfolio>
)";
    const auto r = map_inline(xml);
    REQUIRE(r.bond_leg.leg_type);
    CHECK(*r.bond_leg.leg_type == "Floating");
    REQUIRE(r.bond_leg.payer);
    CHECK(*r.bond_leg.payer);
    REQUIRE(r.bond_leg.currency);
    CHECK(*r.bond_leg.currency == "GBP");
    REQUIRE(r.bond_leg.payment_convention);
    CHECK(*r.bond_leg.payment_convention == "MF");
    REQUIRE(r.bond_leg.payment_lag);
    CHECK(*r.bond_leg.payment_lag == "2D");
    REQUIRE(r.bond_leg.notional_payment_lag);
    CHECK(*r.bond_leg.notional_payment_lag == 3);
    REQUIRE(r.bond_leg.payment_calendar);
    CHECK(*r.bond_leg.payment_calendar == "GBP");
    REQUIRE(r.bond_leg.day_counter);
    CHECK(*r.bond_leg.day_counter == "A365");
    REQUIRE(r.bond_leg.last_period_day_counter);
    CHECK(*r.bond_leg.last_period_day_counter == "ACT/ACT");
    REQUIRE(r.bond_leg.strict_notional_dates);
    CHECK(*r.bond_leg.strict_notional_dates);

    const auto rt = bond_instrument_mapper::reverse_bond(r);
    REQUIRE(rt.BondData);
    REQUIRE(rt.BondData->LegData.size() == 1);
    const auto& leg = rt.BondData->LegData.front();
    CHECK(leg.LegType == ores::ore::domain::legType::Floating);
    CHECK(leg.Payer);
    REQUIRE(leg.Currency);
    CHECK(std::string(*leg.Currency) == "GBP");
    REQUIRE(leg.PaymentConvention);
    CHECK(to_string(*leg.PaymentConvention) == "MF");
    REQUIRE(leg.PaymentLag);
    CHECK(std::string(*leg.PaymentLag) == "2D");
    REQUIRE(leg.NotionalPaymentLag);
    CHECK(*leg.NotionalPaymentLag == 3);
    REQUIRE(leg.PaymentCalendar);
    CHECK(std::string(*leg.PaymentCalendar) == "GBP");
    REQUIRE(leg.DayCounter);
    CHECK(to_string(*leg.DayCounter) == "A365");
    REQUIRE(leg.LastPeriodDayCounter);
    CHECK(to_string(*leg.LastPeriodDayCounter) == "ACT/ACT");
    REQUIRE(leg.StrictNotionalDates);
    CHECK(*leg.StrictNotionalDates);

    BOOST_LOG_SEV(lg, info) << "A leg's payment terms survive the round trip.";
}

TEST_CASE("the_issue_row_stands_in_for_a_leg_a_row_set_holds", tags) {
    auto lg(make_logger(test_suite));

    // The issue row mirrors the coupon leg's currency and day counter, and
    // the document's own statement wins when there is one. A payload built
    // from a row set holds no leg, so the row is all there is to go on.
    const std::string xml = R"(
<Portfolio>
  <Trade id="Bond_From_Row">
    <TradeType>Bond</TradeType>
    <BondData>
      <SecurityId>ISIN:XS1234567890</SecurityId>
      <LegData>
        <LegType>Fixed</LegType>
        <Payer>false</Payer>
        <Currency>SEK</Currency>
        <DayCounter>A365</DayCounter>
      </LegData>
    </BondData>
  </Trade>
</Portfolio>
)";
    const auto r = map_inline(xml);
    REQUIRE(r.bond_leg.currency);
    CHECK(*r.bond_leg.currency == "SEK");
    REQUIRE(r.bond_leg.day_counter);
    CHECK(*r.bond_leg.day_counter == "A365");

    auto from_row = r;
    from_row.bond_leg = {};

    const auto rt = bond_instrument_mapper::reverse_bond(from_row);
    REQUIRE(rt.BondData);
    REQUIRE(rt.BondData->LegData.size() == 1);
    const auto& leg = rt.BondData->LegData.front();
    REQUIRE(leg.Currency);
    CHECK(std::string(*leg.Currency) == "SEK");
    REQUIRE(leg.DayCounter);
    CHECK(to_string(*leg.DayCounter) == "A365");

    BOOST_LOG_SEV(lg, info) << "The issue row stands in for a leg a row set holds.";
}

TEST_CASE("a_forward_bonds_coupon_leg_keeps_its_own_schedule", tags) {
    auto lg(make_logger(test_suite));

    // Every forward bond in the corpus states an issue date and a
    // schedule start that differ, so this product is where the rebuild
    // showed. The forward path assembles the same container. The
    // schema requires LongInForward, and the forward bond's settlement
    // block and that flag ride nowhere yet, so the fixture states them
    // and the test asserts only on the schedule.
    const std::string xml = R"(
<Portfolio>
  <Trade id="FwdBond_Schedule">
    <TradeType>ForwardBond</TradeType>
    <ForwardBondData>
      <BondData>
        <IssuerId>CPTY_C</IssuerId>
        <SecurityId>SECURITY_1</SecurityId>
        <IssueDate>2025-02-01</IssueDate>
        <LegData>
          <LegType>Fixed</LegType>
          <Payer>false</Payer>
          <Currency>EUR</Currency>
          <Notionals>
            <Notional>10000000</Notional>
          </Notionals>
          <DayCounter>ACT/ACT</DayCounter>
          <PaymentConvention>F</PaymentConvention>
          <ScheduleData>
            <Rules>
              <StartDate>2025-02-03</StartDate>
              <EndDate>2035-02-03</EndDate>
              <Tenor>1Y</Tenor>
              <Calendar>TARGET</Calendar>
              <Convention>F</Convention>
              <TermConvention>F</TermConvention>
              <Rule>Forward</Rule>
            </Rules>
          </ScheduleData>
          <FixedLegData>
            <Rates>
              <Rate>0.05</Rate>
            </Rates>
          </FixedLegData>
        </LegData>
      </BondData>
      <SettlementData>
        <ForwardMaturityDate>20160808</ForwardMaturityDate>
        <Amount>6300000.00</Amount>
        <SettlementDirty>true</SettlementDirty>
      </SettlementData>
      <LongInForward>true</LongInForward>
    </ForwardBondData>
  </Trade>
</Portfolio>
)";
    const auto r = map_inline(xml);
    CHECK(r.instrument.identity.trade_type_code == "ForwardBond");
    CHECK(r.issue.issue_date == "2025-02-01");

    const auto rt = bond_instrument_mapper::reverse_forward_bond(r);
    REQUIRE(rt.ForwardBondData);
    REQUIRE(rt.ForwardBondData->BondData.LegData.size() == 1);
    REQUIRE(rt.ForwardBondData->BondData.LegData.front().ScheduleData);
    REQUIRE(rt.ForwardBondData->BondData.LegData.front().ScheduleData->Rules.size() == 1);
    const auto& rule = rt.ForwardBondData->BondData.LegData.front().ScheduleData->Rules.front();
    CHECK(rule.StartDate == "2025-02-03");
    REQUIRE(rule.Calendar);
    CHECK(std::string(*rule.Calendar) == "TARGET");

    BOOST_LOG_SEV(lg, info) << "A forward bond's coupon leg keeps its own schedule.";
}

// =============================================================================
// The repo leg and the coupon frequency repair
// =============================================================================

TEST_CASE("bond_repo_leg_keeps_the_tenor_the_issue_does_not_take", tags) {
    auto lg(make_logger(test_suite));
    const auto r = map_example("Cash_BondRepo_and_Bond.xml", 0);

    CHECK(r.instrument.identity.trade_type_code == "BondRepo");
    CHECK(r.issue.security_id == "ISIN:US912828X703");

    // The repo leg's schedule used to land in the issue's coupon
    // frequency. The issue states no terms of its own here, so it takes
    // none from the leg either.
    CHECK(r.issue.coupon_frequency_code.empty());
    CHECK(r.issue.currency.empty());
    CHECK(r.issue.face_value == 0.0);
    CHECK(r.issue.maturity_date.empty());

    REQUIRE(r.repo);
    CHECK(r.repo->repo_type == "Fixed");
    CHECK(r.repo->repo_rate == Approx(0.0178).epsilon(0.0001));

    REQUIRE(r.repo_leg.payer);
    CHECK(*r.repo_leg.payer);
    REQUIRE(r.repo_leg.schedule.rules.size() == 1);
    const auto& rule = r.repo_leg.schedule.rules.front();
    CHECK(rule.tenor == "1Y");
    CHECK(rule.start_date == "2024-02-12");
    CHECK(rule.end_date == "2026-05-14");
    CHECK(rule.calendar == "US");
    CHECK(rule.convention == "MF");
    CHECK(rule.rule == "Forward");

    // A repo leg has no issue row to mirror these onto, so the leg itself
    // is their only home.
    REQUIRE(r.repo_leg.currency);
    CHECK(*r.repo_leg.currency == "USD");
    REQUIRE(r.repo_leg.day_counter);
    CHECK(*r.repo_leg.day_counter == "A360");
    REQUIRE(r.repo_leg.payment_convention);
    CHECK(*r.repo_leg.payment_convention == "F");

    const auto rt = bond_instrument_mapper::reverse_bond_repo(r);
    REQUIRE(rt.BondRepoData);

    // No phantom coupon leg: the issue holds no terms to emit one.
    CHECK(rt.BondRepoData->BondData.LegData.empty());

    const auto& leg = rt.BondRepoData->RepoData.LegData;
    CHECK(leg.Payer);
    CHECK(leg.LegType == ores::ore::domain::legType::Fixed);
    REQUIRE(leg.Currency);
    CHECK(std::string(*leg.Currency) == "USD");
    REQUIRE(leg.DayCounter);
    CHECK(to_string(*leg.DayCounter) == "A360");
    REQUIRE(leg.PaymentConvention);
    CHECK(to_string(*leg.PaymentConvention) == "F");
    REQUIRE(leg.ScheduleData);
    REQUIRE(leg.ScheduleData->Rules.size() == 1);
    CHECK(std::string(leg.ScheduleData->Rules.front().Tenor) == "1Y");
    REQUIRE(leg.legDataType);
    REQUIRE(leg.legDataType->FixedLegData);
    REQUIRE(!leg.legDataType->FixedLegData->Rates.Rate.empty());
    CHECK(static_cast<double>(leg.legDataType->FixedLegData->Rates.Rate.front()) ==
          Approx(0.0178).epsilon(0.0001));

    BOOST_LOG_SEV(lg, info) << "BondRepo fact row and leg schedule mapped.";
}

// =============================================================================
// The TRS funding leg and the price type
// =============================================================================

TEST_CASE("bond_trs_carries_the_price_type_and_the_funding_schedule", tags) {
    auto lg(make_logger(test_suite));
    const auto r = map_example("Credit_Bond_TRS.xml", 0);

    CHECK(r.instrument.identity.trade_type_code == "BondTRS");
    REQUIRE(r.trs);

    // No schema field selects a return type, so TotalReturn is the model
    // default the column check admits rather than a value read from the
    // document.
    CHECK(r.trs->return_type == "TotalReturn");
    CHECK(r.trs->funding_leg_type == "Fixed");
    CHECK(r.trs->funding_rate == Approx(-0.0055).epsilon(0.0001));

    CHECK(r.trs_price_type == "Dirty");
    REQUIRE(r.trs_funding_leg.payer);
    CHECK(!*r.trs_funding_leg.payer);
    REQUIRE(r.trs_funding_leg.schedule.rules.size() == 1);
    const auto& rule = r.trs_funding_leg.schedule.rules.front();
    CHECK(rule.tenor == "3M");
    CHECK(rule.start_date == "2025-02-13");
    CHECK(rule.end_date == "2025-05-17");
    CHECK(rule.calendar == "TARGET");
    CHECK(rule.convention == "F");
    CHECK(rule.rule == "Forward");

    const auto rt = bond_instrument_mapper::reverse_bond_trs(r);
    REQUIRE(rt.BondTRSData);
    CHECK(std::string(rt.BondTRSData->TotalReturnData.PriceType) == "Dirty");
    CHECK(!rt.BondTRSData->FundingData.LegData.Payer);
    REQUIRE(rt.BondTRSData->FundingData.LegData.ScheduleData);
    REQUIRE(rt.BondTRSData->FundingData.LegData.ScheduleData->Rules.size() == 1);
    CHECK(std::string(rt.BondTRSData->FundingData.LegData.ScheduleData->Rules.front().Tenor) ==
          "3M");

    BOOST_LOG_SEV(lg, info) << "BondTRS fact row and funding schedule mapped.";
}

TEST_CASE("bond_trs_price_type_and_payer_come_from_the_document", tags) {
    auto lg(make_logger(test_suite));

    // The example document states Dirty and a payer of false, so a Clean
    // price type and a payer of true prove neither value is fixed.
    const std::string xml = R"(
<Portfolio>
  <Trade id="TRS_Clean">
    <TradeType>BondTRS</TradeType>
    <BondTRSData>
      <BondData>
        <SecurityId>ISIN:XS1234567890</SecurityId>
      </BondData>
      <TotalReturnData>
        <Payer>false</Payer>
        <PriceType>Clean</PriceType>
        <ScheduleData>
          <Dates>
            <Calendar>GBP</Calendar>
            <Dates>
              <Date>2025-01-15</Date>
              <Date>2025-07-15</Date>
            </Dates>
          </Dates>
        </ScheduleData>
      </TotalReturnData>
      <FundingData>
        <LegData>
          <LegType>Fixed</LegType>
          <Payer>true</Payer>
          <Currency>GBP</Currency>
          <Notionals>
            <Notional>500000</Notional>
          </Notionals>
          <ScheduleData>
            <Rules>
              <StartDate>2025-01-15</StartDate>
              <EndDate>2026-01-15</EndDate>
              <Tenor>6M</Tenor>
              <Calendar>GBP</Calendar>
              <Convention>MF</Convention>
              <Rule>Forward</Rule>
            </Rules>
          </ScheduleData>
          <DayCounter>A360</DayCounter>
          <PaymentConvention>F</PaymentConvention>
          <FixedLegData>
            <Rates>
              <Rate>0.0125</Rate>
            </Rates>
          </FixedLegData>
        </LegData>
      </FundingData>
    </BondTRSData>
  </Trade>
</Portfolio>
)";
    const auto r = map_inline(xml);

    CHECK(r.instrument.identity.trade_type_code == "BondTRS");
    CHECK(r.trs_price_type == "Clean");
    REQUIRE(r.trs_funding_leg.payer);
    CHECK(*r.trs_funding_leg.payer);
    REQUIRE(r.trs_funding_leg.schedule.rules.size() == 1);
    CHECK(r.trs_funding_leg.schedule.rules.front().tenor == "6M");
    REQUIRE(r.trs);
    CHECK(r.trs->funding_leg_type == "Fixed");
    CHECK(r.trs->funding_rate == Approx(0.0125).epsilon(0.0001));

    const auto rt = bond_instrument_mapper::reverse_bond_trs(r);
    REQUIRE(rt.BondTRSData);
    CHECK(std::string(rt.BondTRSData->TotalReturnData.PriceType) == "Clean");
    CHECK(rt.BondTRSData->FundingData.LegData.Payer);

    BOOST_LOG_SEV(lg, info) << "BondTRS price type and payer read from the document.";
}

// =============================================================================
// The future row
// =============================================================================

TEST_CASE("bond_future_maps_every_fact_column_and_the_basket", tags) {
    auto lg(make_logger(test_suite));

    // No example document states a BondFuture, so the trade is authored
    // after the schema's bondFutureData.
    const std::string xml = R"(
<Portfolio>
  <Trade id="Future">
    <TradeType>BondFuture</TradeType>
    <BondFutureData>
      <ContractName>Euro-Bund-Future</ContractName>
      <ContractNotional>100000</ContractNotional>
      <LongShort>Long</LongShort>
      <Currency>EUR</Currency>
      <ContractMonth>2026-03</ContractMonth>
      <DeliverableGrade>Bund</DeliverableGrade>
      <FairPrice>132.45</FairPrice>
      <Settlement>Physical</Settlement>
      <SettlementDirty>true</SettlementDirty>
      <RootDate>2026-03-10</RootDate>
      <ExpiryBasis>Annual</ExpiryBasis>
      <SettlementBasis>Annual</SettlementBasis>
      <ExpiryLag>2</ExpiryLag>
      <SettlementLag>3</SettlementLag>
      <LastTradingDate>2026-03-06</LastTradingDate>
      <LastDeliveryDate>2026-03-10</LastDeliveryDate>
      <DeliveryBasket>
        <Id>DE0001102325</Id>
        <Id>DE0001102333</Id>
      </DeliveryBasket>
    </BondFutureData>
  </Trade>
</Portfolio>
)";
    const auto r = map_inline(xml);

    CHECK(r.instrument.identity.trade_type_code == "BondFuture");
    REQUIRE(r.future);
    const auto& f = *r.future;
    CHECK(f.contract_name == "Euro-Bund-Future");
    CHECK(f.contract_notional == Approx(100000.0));
    CHECK(f.long_short == "Long");
    CHECK(f.currency == "EUR");
    CHECK(f.contract_month == "2026-03");
    CHECK(f.deliverable_grade == "Bund");
    CHECK(f.fair_price == Approx(132.45));
    CHECK(f.settlement == "Physical");
    CHECK(f.settlement_dirty);
    CHECK(f.root_date == "2026-03-10");
    CHECK(f.expiry_basis == "Annual");
    CHECK(f.settlement_basis == "Annual");
    CHECK(f.expiry_lag == 2);
    CHECK(f.settlement_lag == 3);
    CHECK(f.last_trading_date == "2026-03-06");
    CHECK(f.last_delivery_date == "2026-03-10");
    CHECK(f.modified_by == "ores");
    CHECK(f.change_reason_code == "system.external_data_import");

    // A future carries no bond terms, so the issue row its NOT NULL
    // issue_id points at is minted empty.
    CHECK(r.issue.security_id.empty());
    CHECK(r.instrument.issue_id == r.issue.issue_id);
    CHECK(r.future_delivery_basket == std::vector<std::string>{"DE0001102325", "DE0001102333"});

    const auto rt = bond_instrument_mapper::reverse_bond_future(r);
    REQUIRE(rt.BondFutureData);
    CHECK(std::string(rt.BondFutureData->ContractName) == "Euro-Bund-Future");
    CHECK(std::string(rt.BondFutureData->LongShort) == "Long");
    CHECK(std::stod(std::string(rt.BondFutureData->ContractNotional)) == Approx(100000.0));
    REQUIRE(rt.BondFutureData->Currency);
    CHECK(ores::ore::domain::to_string(*rt.BondFutureData->Currency) == "EUR");
    REQUIRE(rt.BondFutureData->SettlementDirty);
    CHECK(std::string(*rt.BondFutureData->SettlementDirty) == "true");
    REQUIRE(rt.BondFutureData->DeliveryBasket);
    REQUIRE(rt.BondFutureData->DeliveryBasket->Id.size() == 2);
    CHECK(std::string(rt.BondFutureData->DeliveryBasket->Id[0]) == "DE0001102325");
    CHECK(std::string(rt.BondFutureData->DeliveryBasket->Id[1]) == "DE0001102333");

    BOOST_LOG_SEV(lg, info) << "BondFuture fact row and delivery basket mapped.";
}

// =============================================================================
// The ascot row and its swap leg
// =============================================================================

TEST_CASE("ascot_row_option_type_and_swap_leg_survive", tags) {
    auto lg(make_logger(test_suite));
    const auto r = map_example("Cash_Ascot.xml", 0);

    CHECK(r.instrument.identity.trade_type_code == "Ascot");
    REQUIRE(r.ascot);
    CHECK(r.ascot->ascot_option_type == "Call");
    CHECK(r.option_exercise_dates == std::vector<std::string>{"2030-10-08"});

    // The document states no conversion terms, so no target rows exist.
    CHECK(r.conversion_targets.empty());

    REQUIRE(r.ascot_swap_leg.payer);
    CHECK(!*r.ascot_swap_leg.payer);
    REQUIRE(r.ascot_swap_leg.schedule.rules.size() == 1);
    const auto& rule = r.ascot_swap_leg.schedule.rules.front();
    CHECK(rule.tenor == "3M");
    CHECK(rule.start_date == "2021-10-08");
    CHECK(rule.end_date == "2030-10-08");
    CHECK(rule.calendar == "TARGET");
    CHECK(rule.convention == "ModifiedFollowing");
    CHECK(rule.rule == "Backward");

    const auto rt = bond_instrument_mapper::reverse_ascot(r);
    REQUIRE(rt.AscotData);
    REQUIRE(rt.AscotData->OptionData.OptionType);
    CHECK(std::string(*rt.AscotData->OptionData.OptionType) == "Call");
    REQUIRE(rt.AscotData->OptionData.exerciseDatesGroup);
    REQUIRE(rt.AscotData->OptionData.exerciseDatesGroup->ExerciseDates);
    REQUIRE(rt.AscotData->OptionData.exerciseDatesGroup->ExerciseDates->ExerciseDate.size() == 1);
    CHECK(std::string(
              rt.AscotData->OptionData.exerciseDatesGroup->ExerciseDates->ExerciseDate.front()) ==
          "2030-10-08");
    CHECK(!rt.AscotData->ReferenceSwapData.LegData.Payer);
    REQUIRE(rt.AscotData->ReferenceSwapData.LegData.ScheduleData);
    CHECK(std::string(rt.AscotData->ReferenceSwapData.LegData.ScheduleData->Rules.front().Tenor) ==
          "3M");

    BOOST_LOG_SEV(lg, info) << "Ascot fact row and reference swap leg mapped.";
}

// =============================================================================
// The whole exercise date list
// =============================================================================

TEST_CASE("bond_option_keeps_every_exercise_date", tags) {
    auto lg(make_logger(test_suite));

    // The example document states one exercise date; the schedule's full
    // list is what the container carries, in document order.
    const std::string xml = R"(
<Portfolio>
  <Trade id="Bond_Call_Option">
    <TradeType>BondOption</TradeType>
    <BondOptionData>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Call</OptionType>
        <Style>Bermudan</Style>
        <ExerciseDates>
          <ExerciseDate>2026-01-15</ExerciseDate>
          <ExerciseDate>2027-01-15</ExerciseDate>
          <ExerciseDate>2028-01-15</ExerciseDate>
        </ExerciseDates>
      </OptionData>
      <Strike>102.5</Strike>
      <BondData>
        <SecurityId>ISIN:US912828X703</SecurityId>
      </BondData>
    </BondOptionData>
  </Trade>
</Portfolio>
)";
    const auto r = map_inline(xml);

    CHECK(r.instrument.identity.trade_type_code == "BondOption");
    REQUIRE(r.option);
    CHECK(r.option->option_type == "Call");
    CHECK(r.option->option_strike == Approx(102.5));
    CHECK(r.option_exercise_dates ==
          std::vector<std::string>{"2026-01-15", "2027-01-15", "2028-01-15"});

    const auto rt = bond_instrument_mapper::reverse_bond_option(r);
    REQUIRE(rt.BondOptionData);
    REQUIRE(rt.BondOptionData->OptionData.exerciseDatesGroup);
    REQUIRE(rt.BondOptionData->OptionData.exerciseDatesGroup->ExerciseDates);
    const auto& dates =
        rt.BondOptionData->OptionData.exerciseDatesGroup->ExerciseDates->ExerciseDate;
    REQUIRE(dates.size() == 3);
    CHECK(std::string(dates[0]) == "2026-01-15");
    CHECK(std::string(dates[1]) == "2027-01-15");
    CHECK(std::string(dates[2]) == "2028-01-15");

    BOOST_LOG_SEV(lg, info) << "BondOption exercise date list mapped whole.";
}

// =============================================================================
// The issue's child rows
// =============================================================================

TEST_CASE("callable_bond_call_dates_become_rows", tags) {
    auto lg(make_logger(test_suite));

    // Credit_CallableBond.xml states no CallData, so the call schedule is
    // authored after the reference-data document that carries one.
    const std::string xml = R"(
<Portfolio>
  <Trade id="Callable">
    <TradeType>CallableBond</TradeType>
    <CallableBondData>
      <BondData>
        <SecurityId>ISIN:XS9988776655</SecurityId>
      </BondData>
      <CallData>
        <ScheduleData>
          <Dates>
            <Calendar/>
            <Tenor/>
            <Dates>
              <Date>2024-05-01</Date>
              <Date>2027-04-01</Date>
            </Dates>
          </Dates>
        </ScheduleData>
        <Styles>
          <Style>American</Style>
        </Styles>
        <Prices>
          <Price>1</Price>
        </Prices>
        <PriceTypes>
          <PriceType>Clean</PriceType>
        </PriceTypes>
        <IncludeAccruals>
          <IncludeAccrual>true</IncludeAccrual>
        </IncludeAccruals>
      </CallData>
    </CallableBondData>
  </Trade>
</Portfolio>
)";
    const auto r = map_inline(xml);

    CHECK(r.instrument.identity.trade_type_code == "CallableBond");
    REQUIRE(r.call_dates.size() == 2);
    CHECK(r.call_dates[0].sequence_number == 1);
    CHECK(r.call_dates[0].call_date == "2024-05-01");
    CHECK(r.call_dates[1].sequence_number == 2);
    CHECK(r.call_dates[1].call_date == "2027-04-01");
    CHECK(r.call_dates[0].issue_id == r.issue.issue_id);
    CHECK(r.call_dates[1].issue_id == r.issue.issue_id);
    CHECK(r.call_dates[0].modified_by == "ores");
    CHECK(r.call_dates[0].change_reason_code == "system.external_data_import");

    const auto rt = bond_instrument_mapper::reverse_callable_bond(r);
    REQUIRE(rt.CallableBondData);
    REQUIRE(rt.CallableBondData->CallData);
    const auto& blocks = rt.CallableBondData->CallData->ScheduleData.Dates;
    REQUIRE(blocks.size() == 1);
    REQUIRE(blocks.front().Dates.Date.size() == 2);
    CHECK(std::string(blocks.front().Dates.Date[0]) == "2024-05-01");
    CHECK(std::string(blocks.front().Dates.Date[1]) == "2027-04-01");

    BOOST_LOG_SEV(lg, info) << "Callable bond call dates mapped to issue child rows.";
}

TEST_CASE("convertible_conversion_ratios_become_rows", tags) {
    auto lg(make_logger(test_suite));

    // Cash_ConvertibleBond.xml states no ConversionData, so the ratio
    // list is authored after the reference-data document that carries one.
    const std::string xml = R"(
<Portfolio>
  <Trade id="Convertible">
    <TradeType>ConvertibleBond</TradeType>
    <ConvertibleBondData>
      <BondData>
        <SecurityId>ISIN:US531229AR32</SecurityId>
      </BondData>
      <ConversionData>
        <ConversionRatios>
          <ConversionRatio>0.0188796</ConversionRatio>
          <ConversionRatio>0.5</ConversionRatio>
        </ConversionRatios>
        <Underlying>
          <Type>Equity</Type>
          <Name>RIC:ON.O</Name>
        </Underlying>
      </ConversionData>
    </ConvertibleBondData>
  </Trade>
</Portfolio>
)";
    const auto r = map_inline(xml);

    CHECK(r.instrument.identity.trade_type_code == "ConvertibleBond");
    REQUIRE(r.conversion_targets.size() == 2);
    CHECK(r.conversion_targets[0].sequence_number == 1);
    CHECK(r.conversion_targets[0].underlying_id == "RIC:ON.O");
    CHECK(r.conversion_targets[0].conversion_ratio == Approx(0.0188796).epsilon(0.0001));
    CHECK(r.conversion_targets[1].sequence_number == 2);
    CHECK(r.conversion_targets[1].underlying_id == "RIC:ON.O");
    CHECK(r.conversion_targets[1].conversion_ratio == Approx(0.5));
    CHECK(r.conversion_targets[0].issue_id == r.issue.issue_id);
    CHECK(r.conversion_targets[0].performed_by == "ores");

    const auto rt = bond_instrument_mapper::reverse_convertible_bond(r);
    REQUIRE(rt.ConvertibleBondData);
    REQUIRE(rt.ConvertibleBondData->ConversionData);
    REQUIRE(rt.ConvertibleBondData->ConversionData->ConversionRatios);
    const auto& ratios = rt.ConvertibleBondData->ConversionData->ConversionRatios->ConversionRatio;
    REQUIRE(ratios.size() == 2);
    CHECK(static_cast<double>(ratios[0]) == Approx(0.0188796).epsilon(0.0001));
    CHECK(static_cast<double>(ratios[1]) == Approx(0.5));
    REQUIRE(rt.ConvertibleBondData->ConversionData->Underlying);
    CHECK(std::string(rt.ConvertibleBondData->ConversionData->Underlying->Name) == "RIC:ON.O");

    BOOST_LOG_SEV(lg, info) << "Convertible conversion ratios mapped to issue child rows.";
}

// =============================================================================
// Product coverage
// =============================================================================

TEST_CASE("bond_product_coverage_names_every_code", tags) {
    auto lg(make_logger(test_suite));

    // The ten bond codes of the deliverable's coverage findings. Nine
    // have an import arm, and the example tree states each of them but
    // for BondFuture, which the mapper suite authors. BondPosition is the
    // recorded exception: no document states it on its own, because ORE
    // carries it as a sub-trade of another type.
    const std::vector<std::pair<oreTradeType, std::string>> covered = {
        {oreTradeType::Bond, "Bond"},
        {oreTradeType::ForwardBond, "ForwardBond"},
        {oreTradeType::BondFuture, "BondFuture"},
        {oreTradeType::BondOption, "BondOption"},
        {oreTradeType::BondTRS, "BondTRS"},
        {oreTradeType::BondRepo, "BondRepo"},
        {oreTradeType::CallableBond, "CallableBond"},
        {oreTradeType::ConvertibleBond, "ConvertibleBond"},
        {oreTradeType::Ascot, "Ascot"}};

    for (const auto& [code, name] : covered) {
        ores::ore::domain::trade t;
        t.TradeType = code;
        const auto r = trade_mapper::map_bond_instrument(t);
        INFO("Bond code: " << name);
        REQUIRE(r.has_value());
        CHECK(r->instrument.identity.trade_type_code == name);
    }

    ores::ore::domain::trade position;
    position.TradeType = oreTradeType::BondPosition;
    CHECK(!trade_mapper::map_bond_instrument(position).has_value());

    BOOST_LOG_SEV(lg, info) << "Bond product coverage covers nine of the ten codes.";
}

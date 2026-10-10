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
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/swap_instrument_mapper.hpp"
#include "ores.platform/time/datetime.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

using Catch::Approx;
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

/**
 * @file xml_ir_mapper_roundtrip_tests.cpp
 * @brief Thing 3: Mapper fidelity tests for IR/CCS/FRA/CapFloor.
 *
 * For each example trade:
 *   1. Parse ORE XML using ores.ore XSD types.
 *   2. Forward-map to ORES domain types (instrument + swap_legs).
 *   3. Verify key fields were captured correctly.
 *   4. Reverse-map back to ORE XSD types.
 *   5. Verify the reconstructed ORE type has the fields we mapped.
 *
 * These tests check field-level fidelity for the fields the ORES domain model
 * currently captures. They do NOT do full-text XML comparison — that is the
 * job of Thing 1 (golden roundtrip tests). The gaps between what these tests
 * cover and the full ORE XML schema are reported by the Python coverage check.
 */

namespace {

const std::string_view test_suite("ores.ore.ir.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][ir]");

using ores::ore::domain::portfolio;
using ores::ore::domain::swap_instrument_mapper;
using namespace ores::logging;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve("external/ore/examples/Products/Example_Trades/" +
                                                filename);
}

ores::ore::domain::trade load_first_trade(const std::string& filename) {
    using ores::platform::filesystem::file;
    const auto path = example_path(filename);
    const std::string content = file::read_content(path);
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    return p.Trade.front();
}

// The family header holds a date as an optional calendar date; the ORE XML
// holds its ISO-8601 spelling. An absent date renders as an empty string.
std::string ore_iso(const std::optional<std::chrono::year_month_day>& d) {
    return d ? ores::platform::time::datetime::to_iso8601_date(*d) : std::string{};
}

} // namespace

// =============================================================================
// Vanilla Swap mapper tests
// =============================================================================


/**
 * The leg's economics are rows beside it, so a test reads the child of the
 * leg it is asserting on rather than a column of the leg.
 */
static double leg_notional(const ores::trading::domain::swap_instrument_data& data,
                           int leg_number) {
    for (const auto& a : data.leg_amounts)
        if (a.leg_number == leg_number)
            return a.amount.to_double();
    return 0.0;
}

static double leg_rate(const ores::trading::domain::swap_instrument_data& data,
                       int leg_number,
                       const std::string& rate_role) {
    for (const auto& r : data.leg_rates)
        if (r.leg_number == leg_number && r.rate_role == rate_role)
            return r.value;
    return 0.0;
}

TEST_CASE("mapper_roundtrip_swap_vanilla_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("IR_Swap_Vanilla.xml");

    const auto result = swap_instrument_mapper::forward_swap(t);
    const auto& legs = result.legs;

    CHECK(ore_iso(result.header.start_date) == "2023-02-21");
    CHECK(ore_iso(result.header.maturity_date) == "2043-02-21");

    REQUIRE(legs.size() == 2);
    CHECK(legs[0].leg_type_code == "Fixed");
    CHECK(legs[0].currency == "EUR");
    CHECK(leg_notional(result, 1) == Approx(10000000.0).epsilon(0.001));
    CHECK(leg_rate(result, 1, "fixed") == Approx(0.021).epsilon(0.0001));

    CHECK(legs[1].leg_type_code == "Floating");
    CHECK(legs[1].currency == "EUR");
    CHECK(legs[1].floating_index_code == "EUR-EURIBOR-6M");
    CHECK(leg_notional(result, 2) == Approx(10000000.0).epsilon(0.001));
    BOOST_LOG_SEV(lg, info) << "Swap forward-mapper test passed";
}

TEST_CASE("mapper_roundtrip_swap_vanilla_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("IR_Swap_Vanilla.xml");
    const auto result = swap_instrument_mapper::forward_swap(t);

    const auto reconstructed = swap_instrument_mapper::reverse_swap(
        result.header,
        std::get<ores::trading::domain::vanilla_swap_instrument>(result.facts),
        result.legs,
        result.leg_amounts,
        result.leg_rates);

    REQUIRE(reconstructed.SwapData.operator bool());
    CHECK(reconstructed.SwapData->LegData.size() == 2);

    const auto& leg0 = reconstructed.SwapData->LegData[0];
    CHECK(leg0.LegType == ores::ore::domain::legType::Fixed);
    REQUIRE(leg0.ScheduleData);
    CHECK(!leg0.ScheduleData->Rules.empty());
    CHECK(std::string(leg0.ScheduleData->Rules[0].StartDate) == "2023-02-21");

    BOOST_LOG_SEV(lg, info) << "Swap reverse-mapper test passed";
}

// =============================================================================
// FRA mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_fra_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("IR_FRA.xml");

    const auto result = swap_instrument_mapper::forward_fra(t);
    const auto& instr = std::get<ores::trading::domain::fra_instrument>(result.facts);
    const auto& legs = result.legs;

    CHECK(ore_iso(result.header.start_date) == "2026-10-19");
    CHECK(ore_iso(result.header.maturity_date) == "2027-04-20");
    CHECK(instr.currency == "EUR");
    CHECK(instr.notional.to_double() == Approx(100000000.0).epsilon(0.001));

    REQUIRE(legs.size() == 1);
    CHECK(legs[0].floating_index_code == "EUR-EURIBOR-6M");
    CHECK(leg_rate(result, 1, "fixed") == Approx(0.005).epsilon(0.00001));
    CHECK(legs[0].currency == "EUR");
    BOOST_LOG_SEV(lg, info) << "FRA forward-mapper test passed";
}

TEST_CASE("mapper_roundtrip_fra_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("IR_FRA.xml");
    const auto result = swap_instrument_mapper::forward_fra(t);

    const auto reconstructed = swap_instrument_mapper::reverse_fra(
        result.header,
        std::get<ores::trading::domain::fra_instrument>(result.facts),
        result.legs,
        result.leg_amounts,
        result.leg_rates);

    REQUIRE(reconstructed.ForwardRateAgreementData.operator bool());
    const auto& fra = *reconstructed.ForwardRateAgreementData;
    CHECK(std::string(fra.StartDate) == "2026-10-19");
    CHECK(std::string(fra.EndDate) == "2027-04-20");
    CHECK(ores::ore::domain::to_string(fra.Currency) == "EUR");
    CHECK(std::string(fra.Index) == "EUR-EURIBOR-6M");
    CHECK(static_cast<float>(fra.Strike) == Approx(0.005f).epsilon(0.00001f));
    BOOST_LOG_SEV(lg, info) << "FRA reverse-mapper test passed";
}

// =============================================================================
// CapFloor mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_capfloor_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("IR_Cap_on_IBOR.xml");

    const auto result = swap_instrument_mapper::forward_capfloor(t);
    const auto& legs = result.legs;

    CHECK(ore_iso(result.header.start_date) == "2023-10-11");
    CHECK(ore_iso(result.header.maturity_date) == "2038-10-10");

    REQUIRE(legs.size() == 1);
    CHECK(legs[0].leg_type_code == "Floating");
    CHECK(legs[0].currency == "EUR");
    CHECK(leg_notional(result, 1) == Approx(3000000.0).epsilon(0.001));
    CHECK(legs[0].floating_index_code == "EUR-EURIBOR-6M");
    BOOST_LOG_SEV(lg, info) << "CapFloor forward-mapper test passed";
}

TEST_CASE("mapper_roundtrip_capfloor_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("IR_Cap_on_IBOR.xml");
    const auto result = swap_instrument_mapper::forward_capfloor(t);

    const auto reconstructed = swap_instrument_mapper::reverse_capfloor(
        result.header,
        std::get<ores::trading::domain::cap_floor_instrument>(result.facts),
        result.legs,
        result.leg_amounts,
        result.leg_rates,
        result.strikes,
        result.schedules,
        result.schedule_dates);

    REQUIRE(reconstructed.CapFloorData.operator bool());
    const auto& cf = *reconstructed.CapFloorData;
    CHECK(cf.LegData.LegType == ores::ore::domain::legType::Floating);
    CHECK(ores::ore::domain::to_string(cf.LegData.Currency) == "EUR");
    CHECK(cf.LegData.ScheduleData.Rules.size() == 1);
    CHECK(std::string(cf.LegData.ScheduleData.Rules[0].StartDate) == "2023-10-11");
    CHECK(std::string(cf.LegData.legDataType.FloatingLegData->Index) == "EUR-EURIBOR-6M");
    BOOST_LOG_SEV(lg, info) << "CapFloor reverse-mapper test passed";
}

TEST_CASE("mapper_roundtrip_capfloor_shared_children", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("IR_Cap_on_IBOR.xml");
    const auto result = swap_instrument_mapper::forward_capfloor(t);

    // The document's single cap becomes one shared strike row.
    REQUIRE(result.strikes.size() == 1u);
    REQUIRE(result.strikes.front().bare_value.has_value());
    CHECK(result.strikes.front().bare_value->to_double() == Approx(0.025).epsilon(0.0001));

    // The underlying swap leg's rule block becomes one schedule row and no
    // date rows.
    REQUIRE(result.schedules.size() == 1u);
    CHECK(result.schedules.front().owner_role == "swap");
    CHECK(result.schedules.front().owner_number == 1);
    CHECK(result.schedules.front().schedule_role == "schedule");
    CHECK(result.schedules.front().schedule_kind == "rules");
    CHECK(ore_iso(result.schedules.front().start_date) == "2023-10-11");
    CHECK(result.schedules.front().tenor.value_or("") == "6M");
    CHECK(result.schedule_dates.empty());

    const auto reconstructed = swap_instrument_mapper::reverse_capfloor(
        result.header,
        std::get<ores::trading::domain::cap_floor_instrument>(result.facts),
        result.legs,
        result.leg_amounts,
        result.leg_rates,
        result.strikes,
        result.schedules,
        result.schedule_dates);

    REQUIRE(reconstructed.CapFloorData.operator bool());
    const auto& cf = *reconstructed.CapFloorData;
    REQUIRE(cf.Caps.operator bool());
    REQUIRE(cf.Caps->Cap.size() == 1u);
    CHECK(static_cast<float>(cf.Caps->Cap.front()) == Approx(0.025f).epsilon(0.0001f));

    // The stored schedule is the document's own, so the whole rule block
    // survives rather than being regenerated from the header's dates.
    REQUIRE(cf.LegData.ScheduleData.Rules.size() == 1u);
    CHECK(std::string(cf.LegData.ScheduleData.Rules[0].StartDate) == "2023-10-11");
    REQUIRE(cf.LegData.ScheduleData.Rules[0].EndDate.operator bool());
    CHECK(std::string(*cf.LegData.ScheduleData.Rules[0].EndDate) == "2038-10-10");
    CHECK(std::string(cf.LegData.ScheduleData.Rules[0].Tenor) == "6M");
    REQUIRE(cf.LegData.ScheduleData.Rules[0].Calendar.operator bool());
    CHECK(std::string(*cf.LegData.ScheduleData.Rules[0].Calendar) == "EUR");
    BOOST_LOG_SEV(lg, info) << "CapFloor shared-children round-trip test passed";
}

// =============================================================================
// CCS mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_ccs_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("IRFX_Cross_Currency_Swap_rebalancing.xml");

    const auto result = swap_instrument_mapper::forward_swap(t);
    REQUIRE(std::holds_alternative<ores::trading::domain::vanilla_swap_instrument>(result.facts));
    const auto& legs = result.legs;

    REQUIRE(legs.size() >= 2);
    BOOST_LOG_SEV(lg, info) << "CCS forward-mapper test passed, legs: " << legs.size();
}

// =============================================================================
// KnockOutSwap mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_knock_out_swap_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("Exotic_KnockOutSwap.xml");

    const auto result = swap_instrument_mapper::forward_knock_out_swap(t);
    REQUIRE(std::holds_alternative<ores::trading::domain::knock_out_swap_instrument>(result.facts));
    const auto& instr = std::get<ores::trading::domain::knock_out_swap_instrument>(result.facts);

    CHECK(result.header.identity.trade_type_code == "KnockOutSwap");
    CHECK(instr.barrier_type == "UpAndOut");
    CHECK(instr.barrier_level == Approx(0.05).epsilon(0.0001));
    CHECK(ore_iso(result.header.start_date) == "2024-05-02");
    CHECK(ore_iso(result.header.maturity_date) == "2029-05-02");
    CHECK(ores::platform::time::datetime::to_iso8601_date(instr.barrier_start_date) ==
          "2027-05-03");

    REQUIRE(result.legs.size() == 2);
    CHECK(result.legs[0].leg_type_code == "Floating");
    CHECK(result.legs[0].floating_index_code == "USD-SOFR");
    CHECK(leg_notional(result, 1) == Approx(100000000.0).epsilon(0.001));
    CHECK(result.legs[1].leg_type_code == "Fixed");
    CHECK(leg_rate(result, 2, "fixed") == Approx(0.05).epsilon(0.0001));
    BOOST_LOG_SEV(lg, info) << "KnockOutSwap forward-mapper test passed";
}

TEST_CASE("mapper_roundtrip_knock_out_swap_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("Exotic_KnockOutSwap.xml");
    const auto result = swap_instrument_mapper::forward_knock_out_swap(t);

    const auto reconstructed = swap_instrument_mapper::reverse_knock_out_swap(
        result.header,
        std::get<ores::trading::domain::knock_out_swap_instrument>(result.facts),
        result.legs,
        result.leg_amounts,
        result.leg_rates);

    REQUIRE(reconstructed.TradeType == ores::ore::domain::oreTradeType::KnockOutSwap);
    REQUIRE(reconstructed.KnockOutSwapData.operator bool());
    const auto& d = *reconstructed.KnockOutSwapData;
    CHECK(d.BarrierData.Type == ores::ore::domain::barrierType::UpAndOut);
    CHECK(std::string(d.BarrierStartDate) == "2027-05-03");
    REQUIRE(d.BarrierData.Levels.Level.size() == 1);
    CHECK(static_cast<float>(d.BarrierData.Levels.Level.front()) == Approx(0.05f).epsilon(0.0001f));
    REQUIRE(d.LegData.size() == 2);
    CHECK(d.LegData[0].LegType == ores::ore::domain::legType::Floating);
    CHECK(d.LegData[1].LegType == ores::ore::domain::legType::Fixed);
    BOOST_LOG_SEV(lg, info) << "KnockOutSwap reverse-mapper test passed";
}

// =============================================================================
// InflationSwap mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_inflation_swap_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("Inflation_CPI_Swap.xml");
    const auto result = swap_instrument_mapper::forward_inflation_swap(t);
    REQUIRE(result.header.identity.trade_type_code == "InflationSwap");

    const auto reconstructed = swap_instrument_mapper::reverse_inflation_swap(
        result.header,
        std::get<ores::trading::domain::inflation_swap_instrument>(result.facts),
        result.legs,
        result.leg_amounts,
        result.leg_rates);

    CHECK(reconstructed.TradeType == ores::ore::domain::oreTradeType::InflationSwap);
    REQUIRE(reconstructed.InflationSwapData.operator bool());
    const auto& legs = reconstructed.InflationSwapData->LegData;
    REQUIRE(legs.size() == 2);
    CHECK(legs[0].LegType == ores::ore::domain::legType::Floating);
    REQUIRE(legs[0].Currency.operator bool());
    CHECK(std::string(*legs[0].Currency) == "GBP");
    CHECK(legs[1].LegType == ores::ore::domain::legType::CPI);
    REQUIRE(legs[1].Currency.operator bool());
    CHECK(std::string(*legs[1].Currency) == "GBP");
    REQUIRE(legs[0].ScheduleData);
    REQUIRE(!legs[0].ScheduleData->Rules.empty());
    CHECK(std::string(legs[0].ScheduleData->Rules[0].StartDate) == "2023-07-10");
    REQUIRE(legs[0].ScheduleData->Rules[0].EndDate.operator bool());
    CHECK(std::string(*legs[0].ScheduleData->Rules[0].EndDate) == "2028-07-09");
    BOOST_LOG_SEV(lg, info) << "InflationSwap reverse-mapper test passed";
}

// =============================================================================
// FlexiSwap mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_flexi_swap_forward", tags) {
    const auto t = load_first_trade("IR_Flexi_Swap.xml");
    const auto result = swap_instrument_mapper::forward_flexi_swap(t);
    REQUIRE(result.header.identity.trade_type_code == "FlexiSwap");

    const auto& fact = std::get<ores::trading::domain::flexi_swap_instrument>(result.facts);
    CHECK(fact.option_long_short == "Short");

    REQUIRE(result.lower_notionals.size() == 36);
    CHECK(result.lower_notionals.front().sequence_number == 1);
    CHECK(result.lower_notionals.front().bound_number == 1);
    CHECK(result.lower_notionals.back().sequence_number == 36);
    CHECK(result.lower_notionals.back().notional.to_double() == 0.0);
    CHECK(result.lower_notionals.front().notional.to_double() == 451389568.0);
}

TEST_CASE("mapper_roundtrip_flexi_swap_reverse", tags) {
    const auto t = load_first_trade("IR_Flexi_Swap.xml");
    const auto result = swap_instrument_mapper::forward_flexi_swap(t);

    const auto reconstructed = swap_instrument_mapper::reverse_flexi_swap(
        result.header,
        std::get<ores::trading::domain::flexi_swap_instrument>(result.facts),
        result.legs,
        result.leg_amounts,
        result.leg_rates,
        result.lower_notionals);

    CHECK(reconstructed.TradeType == ores::ore::domain::oreTradeType::FlexiSwap);
    REQUIRE(reconstructed.FlexiSwapData.operator bool());
    const auto& original = *t.FlexiSwapData;
    const auto& rebuilt = *reconstructed.FlexiSwapData;

    CHECK(rebuilt.OptionLongShort == original.OptionLongShort);
    REQUIRE(rebuilt.LowerNotionalBounds.size() == original.LowerNotionalBounds.size());
    for (std::size_t b = 0; b < original.LowerNotionalBounds.size(); ++b) {
        const auto& want = original.LowerNotionalBounds[b].Notional;
        const auto& got = rebuilt.LowerNotionalBounds[b].Notional;
        REQUIRE(got.size() == want.size());
        for (std::size_t i = 0; i < want.size(); ++i)
            CHECK(static_cast<float>(got[i]) == static_cast<float>(want[i]));
    }
    CHECK(rebuilt.LegData.size() == 2);
}

TEST_CASE("mapper_flexi_swap_keeps_each_lower_bound_block_and_its_currency", tags) {
    ores::ore::domain::trade t;
    t.TradeType = ores::ore::domain::oreTradeType::FlexiSwap;
    ores::ore::domain::flexiSwapData fd;
    for (const auto& [currency, values] :
         std::vector<std::pair<std::string, std::vector<std::pair<std::string, float>>>>{
             {"EUR", {{"", 100.f}, {"2024-01-15", 80.f}}}, {"USD", {{"", 50.f}}}}) {
        ores::ore::domain::flexiSwapData_LowerNotionalBounds_t block;
        block.currency = xsd::string(currency);
        for (const auto& [start, value] : values) {
            ores::ore::domain::flexiSwapData_LowerNotionalBounds_t_Notional_t n;
            static_cast<float&>(n) = value;
            if (!start.empty())
                n.startDate = xsd::string(start);
            block.Notional.push_back(n);
        }
        fd.LowerNotionalBounds.push_back(block);
    }
    t.FlexiSwapData = fd;

    const auto result = swap_instrument_mapper::forward_flexi_swap(t);
    REQUIRE(result.lower_notionals.size() == 3);
    CHECK(result.lower_notionals[0].bound_number == 1);
    CHECK(result.lower_notionals[1].bound_number == 1);
    CHECK(result.lower_notionals[2].bound_number == 2);
    REQUIRE(result.lower_notionals[2].currency.has_value());
    CHECK(*result.lower_notionals[2].currency == "USD");
    CHECK(!result.lower_notionals[0].start_date.has_value());
    CHECK(result.lower_notionals[1].start_date.has_value());

    const auto rebuilt = swap_instrument_mapper::reverse_flexi_swap(
        result.header,
        std::get<ores::trading::domain::flexi_swap_instrument>(result.facts),
        result.legs,
        result.leg_amounts,
        result.leg_rates,
        result.lower_notionals);
    REQUIRE(rebuilt.FlexiSwapData.operator bool());
    const auto& bounds = rebuilt.FlexiSwapData->LowerNotionalBounds;
    REQUIRE(bounds.size() == 2);
    CHECK(bounds[0].Notional.size() == 2);
    CHECK(bounds[1].Notional.size() == 1);
    REQUIRE(bounds[0].currency.operator bool());
    CHECK(std::string(*bounds[0].currency) == "EUR");
    REQUIRE(bounds[1].currency.operator bool());
    CHECK(std::string(*bounds[1].currency) == "USD");
}

// =============================================================================
// BalanceGuaranteedSwap mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_balance_guaranteed_swap_forward", tags) {
    const auto t = load_first_trade("IR_BalanceGuaranteedSwap.xml");
    const auto result = swap_instrument_mapper::forward_balance_guaranteed_swap(t);
    REQUIRE(result.header.identity.trade_type_code == "BalanceGuaranteedSwap");

    const auto& fact =
        std::get<ores::trading::domain::balance_guaranteed_swap_instrument>(result.facts);
    CHECK(fact.reference_security == "ISIN:XS0983610930");

    REQUIRE(result.tranches.size() == 2);
    CHECK(result.tranches[0].sequence_number == 1);
    REQUIRE(result.tranches[0].description.has_value());
    CHECK(*result.tranches[0].description == "Class A");
    CHECK(result.tranches[0].security_id == "ISIN:XS0983610930");
    CHECK(result.tranches[0].seniority == 1);
    CHECK(result.tranches[1].security_id == "NA");
    CHECK(result.tranches[1].seniority == 2);

    // The first tranche states twelve notionals, the second fourteen, and
    // the first notional of each states no date.
    std::size_t first = 0;
    std::size_t second = 0;
    for (const auto& n : result.tranche_notionals) {
        if (n.tranche_number == 1)
            ++first;
        if (n.tranche_number == 2)
            ++second;
    }
    CHECK(first == 12);
    CHECK(second == 14);
    REQUIRE(result.tranche_notionals.size() == 26);
    CHECK(!result.tranche_notionals.front().start_date.has_value());
    CHECK(result.tranche_notionals[1].start_date.has_value());

    // The tranche schedule lives in the shared schedule rows.
    std::size_t tranche_schedules = 0;
    for (const auto& s : result.schedules)
        if (s.owner_role == "tranches")
            ++tranche_schedules;
    CHECK(tranche_schedules == 1);
}

TEST_CASE("mapper_roundtrip_balance_guaranteed_swap_reverse", tags) {
    const auto t = load_first_trade("IR_BalanceGuaranteedSwap.xml");
    const auto result = swap_instrument_mapper::forward_balance_guaranteed_swap(t);

    const auto reconstructed = swap_instrument_mapper::reverse_balance_guaranteed_swap(
        result.header,
        std::get<ores::trading::domain::balance_guaranteed_swap_instrument>(result.facts),
        result.legs,
        result.leg_amounts,
        result.leg_rates,
        result.tranches,
        result.tranche_notionals,
        result.schedules,
        result.schedule_dates);

    CHECK(reconstructed.TradeType == ores::ore::domain::oreTradeType::BalanceGuaranteedSwap);
    REQUIRE(reconstructed.BalanceGuaranteedSwapData.operator bool());
    const auto& original = *t.BalanceGuaranteedSwapData;
    const auto& rebuilt = *reconstructed.BalanceGuaranteedSwapData;

    CHECK(std::string(rebuilt.ReferenceSecurity) == std::string(original.ReferenceSecurity));
    REQUIRE(rebuilt.Tranches.Tranche.size() == original.Tranches.Tranche.size());
    for (std::size_t i = 0; i < original.Tranches.Tranche.size(); ++i) {
        const auto& want = original.Tranches.Tranche[i];
        const auto& got = rebuilt.Tranches.Tranche[i];
        CHECK(std::string(got.SecurityId) == std::string(want.SecurityId));
        CHECK(got.Seniority == want.Seniority);
        REQUIRE(got.Description.operator bool());
        CHECK(std::string(*got.Description) == std::string(*want.Description));
        REQUIRE(got.Notionals.Notional.size() == want.Notionals.Notional.size());
        for (std::size_t n = 0; n < want.Notionals.Notional.size(); ++n) {
            const auto& wn = want.Notionals.Notional[n];
            const auto& gn = got.Notionals.Notional[n];
            CHECK(static_cast<float>(gn) == static_cast<float>(wn));
            CHECK(gn.startDate.operator bool() == wn.startDate.operator bool());
            if (wn.startDate && gn.startDate)
                CHECK(std::string(*gn.startDate) == std::string(*wn.startDate));
        }
    }

    REQUIRE(rebuilt.Tranches.ScheduleData.Rules.size() == 1);
    const auto& rule = rebuilt.Tranches.ScheduleData.Rules.front();
    CHECK(std::string(rule.StartDate) == "2021-12-27");
    REQUIRE(rule.EndDate.operator bool());
    CHECK(std::string(*rule.EndDate) == "2035-12-28");
    CHECK(std::string(rule.Tenor) == "6M");
    CHECK(rebuilt.LegData.size() == 2);
}

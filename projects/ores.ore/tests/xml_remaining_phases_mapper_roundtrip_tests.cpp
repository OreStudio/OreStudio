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
#include "ores.ore/domain/domain.hpp"
#include "ores.ore/domain/trade_mapper.hpp"
#include "ores.ore/domain/fx_instrument_mapper.hpp"
#include "ores.ore/domain/equity_instrument_mapper.hpp"
#include "ores.ore/domain/scripted_instrument_mapper.hpp"
#include "ores.ore/domain/composite_instrument_mapper.hpp"
#include "ores.ore/domain/bond_instrument_mapper.hpp"

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

using Catch::Approx;

/**
 * @file xml_remaining_phases_mapper_roundtrip_tests.cpp
 * @brief Mapper fidelity tests for instrument types added in Phase 10:
 *   FxDoubleBarrierOption, FxEuropeanBarrierOption, FxKIKOBarrierOption,
 *   EquityDoubleBarrierOption, EquityEuropeanBarrierOption,
 *   DoubleDigitalOption, PerformanceOption_01, KnockOutSwap,
 *   TotalReturnSwap, ContractForDifference, BondRepo.
 */

namespace {

const std::string_view test_suite(
    "ores.ore.remaining.phases.mapper.roundtrip.tests");
const std::string tags(
    "[ore][xml][mapper][roundtrip][remaining]");

using ores::ore::domain::portfolio;
using ores::ore::domain::fx_instrument_mapper;
using ores::ore::domain::fx_mapping_result;
using ores::trading::domain::fx_barrier_option_instrument;
using ores::ore::domain::equity_instrument_mapper;
using ores::ore::domain::equity_mapping_result;
using ores::ore::domain::scripted_instrument_mapper;
using ores::ore::domain::scripted_mapping_result;
using ores::ore::domain::composite_instrument_mapper;
using ores::ore::domain::composite_mapping_result;
using ores::ore::domain::bond_instrument_mapper;
using ores::ore::domain::bond_mapping_result;
using namespace ores::logging;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve(
        "external/ore/examples/Products/Example_Trades/" + filename);
}

fx_mapping_result load_and_map_fx(const std::string& filename) {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    auto r = ores::ore::domain::trade_mapper::map_fx_instrument(
        p.Trade.front());
    REQUIRE(r.has_value());
    return *r;
}

equity_mapping_result load_and_map_equity(const std::string& filename) {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    auto r = ores::ore::domain::trade_mapper::map_equity_instrument(
        p.Trade.front());
    REQUIRE(r.has_value());
    return *r;
}

scripted_mapping_result load_and_map_scripted(const std::string& filename) {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    auto r = ores::ore::domain::trade_mapper::map_scripted_instrument(
        p.Trade.front());
    REQUIRE(r.has_value());
    return *r;
}

composite_mapping_result load_and_map_composite(const std::string& filename) {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    auto r = ores::ore::domain::trade_mapper::map_composite_instrument(
        p.Trade.front());
    REQUIRE(r.has_value());
    return *r;
}

bond_mapping_result load_and_map_bond(const std::string& filename,
        std::size_t index = 0) {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(p.Trade.size() > index);
    auto r = ores::ore::domain::trade_mapper::map_bond_instrument(
        p.Trade[index]);
    REQUIRE(r.has_value());
    return *r;
}

} // namespace

// =============================================================================
// FxDoubleBarrierOption
// =============================================================================

TEST_CASE("fx_double_barrier_option_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_fx("FX_DoubleBarrierOption.xml");
    const auto& instr = std::get<fx_barrier_option_instrument>(r.instrument);

    CHECK(instr.trade_type_code == "FxDoubleBarrierOption");
    CHECK(!instr.bought_currency.empty());
    CHECK(!instr.sold_currency.empty());
    CHECK(!instr.option_type.empty());

    BOOST_LOG_SEV(lg, info) << "FxDoubleBarrierOption forward test passed";
}

TEST_CASE("fx_double_barrier_option_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_fx("FX_DoubleBarrierOption.xml");
    const auto& instr = std::get<fx_barrier_option_instrument>(r.instrument);

    const auto rt = fx_instrument_mapper::reverse_fx_double_barrier_option(instr);
    REQUIRE(rt.FxDoubleBarrierOptionData.operator bool());

    BOOST_LOG_SEV(lg, info) << "FxDoubleBarrierOption reverse test passed";
}

// =============================================================================
// FxEuropeanBarrierOption
// =============================================================================

TEST_CASE("fx_european_barrier_option_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_fx("FX_FxEuropeanBarrierOption.xml");
    const auto& instr = std::get<fx_barrier_option_instrument>(r.instrument);

    CHECK(instr.trade_type_code == "FxEuropeanBarrierOption");
    CHECK(!instr.bought_currency.empty());
    CHECK(!instr.sold_currency.empty());

    BOOST_LOG_SEV(lg, info) << "FxEuropeanBarrierOption forward test passed";
}

TEST_CASE("fx_european_barrier_option_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_fx("FX_FxEuropeanBarrierOption.xml");
    const auto& instr = std::get<fx_barrier_option_instrument>(r.instrument);

    const auto rt = fx_instrument_mapper::reverse_fx_european_barrier_option(instr);
    REQUIRE(rt.FxEuropeanBarrierOptionData.operator bool());

    BOOST_LOG_SEV(lg, info) << "FxEuropeanBarrierOption reverse test passed";
}

// =============================================================================
// FxKIKOBarrierOption
// =============================================================================

TEST_CASE("fx_kiko_barrier_option_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_fx("FX_KIKO_Barrier_Option.xml");
    const auto& instr = std::get<fx_barrier_option_instrument>(r.instrument);

    CHECK(instr.trade_type_code == "FxKIKOBarrierOption");
    CHECK(!instr.option_type.empty());

    BOOST_LOG_SEV(lg, info) << "FxKIKOBarrierOption forward test passed";
}

TEST_CASE("fx_kiko_barrier_option_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_fx("FX_KIKO_Barrier_Option.xml");
    const auto& instr = std::get<fx_barrier_option_instrument>(r.instrument);

    const auto rt = fx_instrument_mapper::reverse_fx_kiko_barrier_option(instr);
    REQUIRE(rt.FxKIKOBarrierOptionData.operator bool());

    BOOST_LOG_SEV(lg, info) << "FxKIKOBarrierOption reverse test passed";
}

// =============================================================================
// EquityDoubleBarrierOption
// =============================================================================

TEST_CASE("equity_double_barrier_option_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_equity("Equity_Double_Barrier_Option.xml");
    const auto& instr =
        std::get<ores::trading::domain::equity_barrier_option_instrument>(
            r.instrument);

    CHECK(instr.trade_type_code == "EquityDoubleBarrierOption");
    CHECK(!instr.option_type.empty());
    CHECK(!instr.underlying_name.empty());

    BOOST_LOG_SEV(lg, info) << "EquityDoubleBarrierOption forward test passed";
}

TEST_CASE("equity_double_barrier_option_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_equity("Equity_Double_Barrier_Option.xml");
    const auto& instr =
        std::get<ores::trading::domain::equity_barrier_option_instrument>(
            r.instrument);

    const auto rt = equity_instrument_mapper::reverse_equity_double_barrier_option(
        instr);
    REQUIRE(rt.EquityDoubleBarrierOptionData.operator bool());

    BOOST_LOG_SEV(lg, info) << "EquityDoubleBarrierOption reverse test passed";
}

// =============================================================================
// EquityEuropeanBarrierOption
// =============================================================================

TEST_CASE("equity_european_barrier_option_forward", tags) {
    auto lg(make_logger(test_suite));
    // Reuse EquityBarrierOption file (same structure, different trade type)
    const auto r = load_and_map_equity("Equity_European_Barrier_Option.xml");
    const auto& instr =
        std::get<ores::trading::domain::equity_barrier_option_instrument>(
            r.instrument);

    CHECK(instr.trade_type_code == "EquityEuropeanBarrierOption");
    CHECK(!instr.option_type.empty());

    BOOST_LOG_SEV(lg, info) << "EquityEuropeanBarrierOption forward test passed";
}

TEST_CASE("equity_european_barrier_option_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_equity("Equity_European_Barrier_Option.xml");
    const auto& instr =
        std::get<ores::trading::domain::equity_barrier_option_instrument>(
            r.instrument);

    const auto rt = equity_instrument_mapper::reverse_equity_european_barrier_option(
        instr);
    REQUIRE(rt.EquityEuropeanBarrierOptionData.operator bool());

    BOOST_LOG_SEV(lg, info) << "EquityEuropeanBarrierOption reverse test passed";
}

// =============================================================================
// DoubleDigitalOption
// =============================================================================

TEST_CASE("double_digital_option_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_scripted("Exotic_Double_Digital_Option.xml");

    CHECK(r.instrument.trade_type_code == "DoubleDigitalOption");
    CHECK(!r.instrument.underlyings_json.empty());
    CHECK(!r.instrument.parameters_json.empty());

    BOOST_LOG_SEV(lg, info) << "DoubleDigitalOption forward test passed. "
                            << "underlyings=" << r.instrument.underlyings_json;
}

TEST_CASE("double_digital_option_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_scripted("Exotic_Double_Digital_Option.xml");

    const auto rt = scripted_instrument_mapper::reverse_double_digital_option(
        r.instrument);
    REQUIRE(rt.DoubleDigitalOptionData.operator bool());

    BOOST_LOG_SEV(lg, info) << "DoubleDigitalOption reverse test passed";
}

// =============================================================================
// PerformanceOption_01
// =============================================================================

TEST_CASE("performance_option_01_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_scripted("Exotic_PerformanceOption_01_FX.xml");

    CHECK(r.instrument.trade_type_code == "PerformanceOption_01");
    CHECK(!r.instrument.underlyings_json.empty());
    CHECK(!r.instrument.parameters_json.empty());

    BOOST_LOG_SEV(lg, info) << "PerformanceOption_01 forward test passed. "
                            << "underlyings=" << r.instrument.underlyings_json;
}

TEST_CASE("performance_option_01_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_scripted("Exotic_PerformanceOption_01_FX.xml");

    const auto rt = scripted_instrument_mapper::reverse_performance_option_01(
        r.instrument);
    REQUIRE(rt.PerformanceOption01Data.operator bool());

    BOOST_LOG_SEV(lg, info) << "PerformanceOption_01 reverse test passed";
}

// =============================================================================
// KnockOutSwap
// =============================================================================

TEST_CASE("knock_out_swap_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_scripted("Exotic_KnockOutSwap.xml");

    CHECK(r.instrument.trade_type_code == "KnockOutSwap");
    CHECK(!r.instrument.parameters_json.empty());

    BOOST_LOG_SEV(lg, info) << "KnockOutSwap forward test passed. "
                            << "params=" << r.instrument.parameters_json;
}

TEST_CASE("knock_out_swap_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_scripted("Exotic_KnockOutSwap.xml");

    const auto rt = scripted_instrument_mapper::reverse_knock_out_swap(
        r.instrument);
    REQUIRE(rt.KnockOutSwapData.operator bool());

    BOOST_LOG_SEV(lg, info) << "KnockOutSwap reverse test passed";
}

// =============================================================================
// TotalReturnSwap
// =============================================================================

TEST_CASE("total_return_swap_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_composite("Hybrid_GenericTRS_with_Bond.xml");

    CHECK(r.instrument.trade_type_code == "TotalReturnSwap");
    CHECK(!r.instrument.description.empty());

    BOOST_LOG_SEV(lg, info) << "TotalReturnSwap forward test passed. "
                            << "description=" << r.instrument.description;
}

TEST_CASE("total_return_swap_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_composite("Hybrid_GenericTRS_with_Bond.xml");

    const auto rt = composite_instrument_mapper::reverse_total_return_swap(
        r.instrument);
    CHECK(rt.TradeType == ores::ore::domain::oreTradeType::TotalReturnSwap);

    BOOST_LOG_SEV(lg, info) << "TotalReturnSwap reverse test passed";
}

// =============================================================================
// ContractForDifference
// =============================================================================

TEST_CASE("contract_for_difference_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_composite("Hybrid_CFD.xml");

    CHECK(r.instrument.trade_type_code == "ContractForDifference");

    BOOST_LOG_SEV(lg, info) << "ContractForDifference forward test passed";
}

TEST_CASE("contract_for_difference_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_composite("Hybrid_CFD.xml");

    const auto rt = composite_instrument_mapper::reverse_contract_for_difference(
        r.instrument);
    CHECK(rt.TradeType == ores::ore::domain::oreTradeType::ContractForDifference);

    BOOST_LOG_SEV(lg, info) << "ContractForDifference reverse test passed";
}

// =============================================================================
// BondRepo (first trade in Cash_BondRepo_and_Bond.xml)
// =============================================================================

TEST_CASE("bond_repo_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_bond("Cash_BondRepo_and_Bond.xml", 0);

    CHECK(r.instrument.trade_type_code == "BondRepo");
    CHECK(!r.instrument.security_id.empty());

    BOOST_LOG_SEV(lg, info) << "BondRepo forward test passed. "
                            << "security_id=" << r.instrument.security_id;
}

TEST_CASE("bond_repo_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_bond("Cash_BondRepo_and_Bond.xml", 0);

    const auto rt = bond_instrument_mapper::reverse_bond_repo(r.instrument);
    REQUIRE(rt.BondRepoData.operator bool());
    CHECK(!std::string(rt.BondRepoData->BondData.SecurityId).empty());

    BOOST_LOG_SEV(lg, info) << "BondRepo reverse test passed";
}

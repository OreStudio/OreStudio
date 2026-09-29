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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <map>
#include <sstream>
#include <string>

/**
 * @file xml_roundtrip_conventions_tests.cpp
 * @brief The conventions kind: what rounds trips, and what the mapper drops.
 *
 * The document carries twenty-six convention categories and the mapper models
 * twenty-four. The files that use only those twenty-four round trip. The rest
 * do not,
 * because a category the mapper does not model is content the export cannot
 * write, and the measurement below says which categories those are and how many
 * files each one costs.
 */

namespace {

const std::string tags("[ore][xml][roundtrip][conventions]");

using namespace ores::ore::domain;

std::filesystem::path corpus_root() {
    return ores::testing::project_root::resolve("external/ore/examples");
}

std::string read(const std::filesystem::path& path) {
    std::ifstream in(path, std::ios::binary);
    std::ostringstream buffer;
    buffer << in.rdbuf();
    return buffer.str();
}

conventions load(const std::filesystem::path& path) {
    conventions document;
    load_data(read(path), document);
    return document;
}

ores::ore::xml::roundtrip_kind conventions_kind() {
    return ores::ore::xml::make_roundtrip_kind<conventions, mapped_conventions>(
        "conventions",
        "conventions",
        &conventions_mapper::map,
        &conventions_mapper::reverse,
        conventions_difference);
}

int element_count(const std::string& xml, const std::string& name) {
    const std::string open = "<" + name;
    int count = 0;
    std::size_t at = 0;
    while ((at = xml.find(open, at)) != std::string::npos) {
        const auto next = at + open.size();
        if (next < xml.size() && (xml[next] == '>' || xml[next] == ' ' || xml[next] == '/'))
            ++count;
        at = next;
    }
    return count;
}

}

// A category lands one at a time, and each one asserts its own elements survive
// the export. The whole kind cannot be asserted until every category has, so a
// case per category is how the work is proven as it goes.
TEST_CASE("conventions_swap_index_round_trips", tags) {
    int files_with_swap_index = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.SwapIndex.empty())
            continue;

        ++files_with_swap_index;
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.swap_index.size() == document.SwapIndex.size());

        for (std::size_t i = 0; i < document.SwapIndex.size(); ++i) {
            const std::string id(document.SwapIndex[i].Id);
            CHECK(mapped.swap_index[i].id == id);
            CHECK(mapped.swap_index[i].conventions ==
                  std::string(document.SwapIndex[i].Conventions));
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "SwapIndex");
        INFO(path.string() + ": the document has " +
             std::to_string(document.SwapIndex.size()) + " SwapIndex element(s), the export " +
             std::to_string(written));
        CHECK(written == static_cast<int>(document.SwapIndex.size()));
    }

    // Forty-seven of the seventy-two at the commit this was written at.
    CHECK(files_with_swap_index == 47);
}

TEST_CASE("conventions_future_round_trips", tags) {
    int files_with_future = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.Future.empty())
            continue;

        ++files_with_future;
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.future.size() == document.Future.size());

        for (std::size_t i = 0; i < document.Future.size(); ++i) {
            const std::string id(document.Future[i].Id);
            CHECK(mapped.future[i].id == id);
            CHECK(mapped.future[i].index == std::string(document.Future[i].Index));
            if (document.Future[i].DateGenerationRule)
                CHECK(mapped.future[i].date_generation_rule ==
                      to_string(*document.Future[i].DateGenerationRule));
            if (document.Future[i].OvernightIndexFutureNettingType)
                CHECK(mapped.future[i].netting_type ==
                      to_string(*document.Future[i].OvernightIndexFutureNettingType));
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "Future");
        INFO(path.string() + ": the document has " + std::to_string(document.Future.size()) +
             " Future element(s), the export " + std::to_string(written));
        CHECK(written == static_cast<int>(document.Future.size()));
    }

    // Thirty-nine of the seventy-two at the commit this was written at.
    CHECK(files_with_future == 39);
}

TEST_CASE("conventions_fx_option_round_trips", tags) {
    int files_with_fx_option = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.FxOption.empty())
            continue;

        ++files_with_fx_option;
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.fx_option.size() == document.FxOption.size());

        for (std::size_t i = 0; i < document.FxOption.size(); ++i) {
            CHECK(mapped.fx_option[i].id == std::string(document.FxOption[i].Id));
            CHECK(mapped.fx_option[i].atm_type == std::string(document.FxOption[i].AtmType));
            CHECK(mapped.fx_option[i].delta_type == std::string(document.FxOption[i].DeltaType));
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "FxOption");
        INFO(path.string() + ": the document has " + std::to_string(document.FxOption.size()) +
             " FxOption element(s), the export " + std::to_string(written));
        CHECK(written == static_cast<int>(document.FxOption.size()));
    }

    // Thirty-six of the seventy-two at the commit this was written at.
    CHECK(files_with_fx_option == 36);
}

TEST_CASE("conventions_average_ois_round_trips", tags) {
    int files_with_average_ois = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.AverageOIS.empty())
            continue;

        ++files_with_average_ois;
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.average_ois.size() == document.AverageOIS.size());

        for (std::size_t i = 0; i < document.AverageOIS.size(); ++i) {
            CHECK(mapped.average_ois[i].id == std::string(document.AverageOIS[i].Id));
            CHECK(mapped.average_ois[i].index == std::string(document.AverageOIS[i].Index));
            CHECK(mapped.average_ois[i].spot_lag ==
                  static_cast<int>(document.AverageOIS[i].SpotLag));
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "AverageOIS");
        INFO(path.string() + ": the document has " +
             std::to_string(document.AverageOIS.size()) + " AverageOIS element(s), the export " +
             std::to_string(written));
        CHECK(written == static_cast<int>(document.AverageOIS.size()));
    }

    // Thirty-six of the seventy-two at the commit this was written at, and the
    // seven that carried nothing else unmodelled are why it went in first.
    CHECK(files_with_average_ois == 36);
}

TEST_CASE("conventions_cross_currency_basis_round_trips", tags) {
    int files_with_basis = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.CrossCurrencyBasis.empty())
            continue;

        ++files_with_basis;
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.cross_currency_basis.size() == document.CrossCurrencyBasis.size());

        for (std::size_t i = 0; i < document.CrossCurrencyBasis.size(); ++i) {
            const auto& in = document.CrossCurrencyBasis[i];
            const auto& out = mapped.cross_currency_basis[i];
            CHECK(out.id == std::string(in.Id));
            CHECK(out.flat_index == std::string(in.FlatIndex));
            CHECK(out.spread_index == std::string(in.SpreadIndex));
            CHECK(out.settlement_days == static_cast<int>(in.SettlementDays));
            if (in.SpreadFixingDays)
                CHECK(out.spread_fixing_days == static_cast<int>(*in.SpreadFixingDays));
            if (in.FlatRateCutoff)
                CHECK(out.flat_rate_cutoff == static_cast<int>(*in.FlatRateCutoff));
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "CrossCurrencyBasis");
        INFO(path.string() + ": the document has " +
             std::to_string(document.CrossCurrencyBasis.size()) +
             " CrossCurrencyBasis element(s), the export " + std::to_string(written));
        CHECK(written == static_cast<int>(document.CrossCurrencyBasis.size()));
    }

    // Forty-nine of the seventy-two at the commit this was written at. It is the
    // largest category in the document, and three files carried nothing else.
    CHECK(files_with_basis == 49);
}

TEST_CASE("conventions_commodity_forward_round_trips", tags) {
    int files_with_commodity_forward = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.CommodityForward.empty())
            continue;

        ++files_with_commodity_forward;
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.commodity_forward.size() == document.CommodityForward.size());

        for (std::size_t i = 0; i < document.CommodityForward.size(); ++i) {
            const auto& in = document.CommodityForward[i];
            const auto& out = mapped.commodity_forward[i];
            CHECK(out.id == std::string(in.Id));
            if (in.SpotDays)
                CHECK(out.spot_days == static_cast<int>(*in.SpotDays));
            if (in.PointsFactor)
                CHECK(out.points_factor == static_cast<double>(*in.PointsFactor));
            if (in.AdvanceCalendar)
                CHECK(out.advance_calendar == std::string(*in.AdvanceCalendar));
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "CommodityForward");
        INFO(path.string() + ": the document has " +
             std::to_string(document.CommodityForward.size()) +
             " CommodityForward element(s), the export " + std::to_string(written));
        CHECK(written == static_cast<int>(document.CommodityForward.size()));

        // The mapper stores a canonical spelling of the business day convention,
        // so the export may differ from the document in that. What has to
        // survive is the value: read the export back, map it again, and require
        // the same entity field for field.
        conventions reparsed;
        load_data(exported, reparsed);
        REQUIRE(reparsed.CommodityForward.size() == document.CommodityForward.size());

        const auto remapped = conventions_mapper::map(reparsed);
        for (std::size_t i = 0; i < document.CommodityForward.size(); ++i) {
            INFO(path.string() + ": CommodityForward element " + std::to_string(i));
            CHECK(remapped.commodity_forward[i] == mapped.commodity_forward[i]);
        }
    }

    // Two of the seventy-two files carry it, in five elements between them.
    CHECK(files_with_commodity_forward == 2);
}

TEST_CASE("conventions_bond_yield_round_trips", tags) {
    int files_with_bond_yield = 0;
    std::size_t elements_with_bond_yield = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.BondYield.empty())
            continue;

        ++files_with_bond_yield;
        elements_with_bond_yield += document.BondYield.size();
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.bond_yield.size() == document.BondYield.size());

        for (std::size_t i = 0; i < document.BondYield.size(); ++i) {
            const auto& in = document.BondYield[i];
            const auto& out = mapped.bond_yield[i];
            CHECK(out.id == std::string(in.Id));
            CHECK(out.compounding == std::string(in.Compounding));
            if (in.PriceType)
                CHECK(out.price_type == std::string(*in.PriceType));
            if (in.Accuracy)
                CHECK(out.accuracy == static_cast<double>(*in.Accuracy));
            if (in.Guess)
                CHECK(out.guess == static_cast<double>(*in.Guess));
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "BondYield");
        INFO(path.string() + ": the document has " + std::to_string(document.BondYield.size()) +
             " BondYield element(s), the export " + std::to_string(written));
        CHECK(written == static_cast<int>(document.BondYield.size()));

        // The frequency is normalised to the canonical code the other tables
        // hold, so the export may differ from the document there. What has to
        // survive is the value: read the export back, map it again, and require
        // the same entity field for field.
        conventions reparsed;
        load_data(exported, reparsed);
        REQUIRE(reparsed.BondYield.size() == document.BondYield.size());

        const auto remapped = conventions_mapper::map(reparsed);
        for (std::size_t i = 0; i < document.BondYield.size(); ++i) {
            INFO(path.string() + ": BondYield element " + std::to_string(i));
            CHECK(remapped.bond_yield[i] == mapped.bond_yield[i]);
        }
    }

    // One of the seventy-two files carries it, in five elements.
    CHECK(files_with_bond_yield == 1);
    CHECK(elements_with_bond_yield == 5);
}

TEST_CASE("conventions_commodity_future_round_trips", tags) {
    int files_with_commodity_future = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.CommodityFuture.empty())
            continue;

        ++files_with_commodity_future;
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.commodity_future.size() == document.CommodityFuture.size());

        for (std::size_t i = 0; i < document.CommodityFuture.size(); ++i) {
            const auto& in = document.CommodityFuture[i];
            const auto& out = mapped.commodity_future[i];
            CHECK(out.id == std::string(in.Id));
            CHECK(out.contract_frequency == to_string(in.ContractFrequency));
            CHECK(out.calendar == std::string(in.Calendar));
            if (in.AveragingData)
                CHECK(out.averaging_commodity_name ==
                      std::string(in.AveragingData->CommodityName));
            if (in.ProhibitedExpiries)
                CHECK(out.prohibited_expiries.has_value());
            if (in.FutureContinuationMappings)
                CHECK(out.future_continuation_mappings.has_value());
            if (in.OptionContinuationMappings)
                CHECK(out.option_continuation_mappings.has_value());
        }

        // The four list-bearing fields are written as text columns now, so the
        // mapper must not report any of them as skipped.
        for (const auto& [name, count] : mapped.unmodelled) {
            INFO(path.string() + ": unexpected skip " + name);
            CHECK(name.rfind("CommodityFuture.", 0) != 0);
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "CommodityFuture");
        INFO(path.string() + ": the document has " +
             std::to_string(document.CommodityFuture.size()) +
             " CommodityFuture element(s), the export " + std::to_string(written));
        CHECK(written == static_cast<int>(document.CommodityFuture.size()));

        // The mapper stores canonical spellings of the frequency and the
        // business day conventions, so the export may differ from the document
        // in those. What has to survive is the value: read the export back, map
        // it again, and require the same entity field for field.
        conventions reparsed;
        load_data(exported, reparsed);
        REQUIRE(reparsed.CommodityFuture.size() == document.CommodityFuture.size());

        const auto remapped = conventions_mapper::map(reparsed);
        for (std::size_t i = 0; i < document.CommodityFuture.size(); ++i) {
            INFO(path.string() + ": CommodityFuture element " + std::to_string(i));
            CHECK(remapped.commodity_future[i] == mapped.commodity_future[i]);
        }
    }

    // Five of the seventy-two files carry it, and two of them carried nothing
    // else unmodelled when it went in.
    CHECK(files_with_commodity_future == 5);
}

TEST_CASE("conventions_cms_spread_option_round_trips", tags) {
    int files_with_cms = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.CmsSpreadOption.empty())
            continue;

        ++files_with_cms;
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.cms_spread_option.size() == document.CmsSpreadOption.size());

        for (std::size_t i = 0; i < document.CmsSpreadOption.size(); ++i) {
            const auto& in = document.CmsSpreadOption[i];
            const auto& out = mapped.cms_spread_option[i];
            CHECK(out.id == std::string(in.Id));
            CHECK(out.forward_start == std::string(in.ForwardStart));
            CHECK(out.spot_days == std::string(in.SpotDays));
            CHECK(out.swap_tenor == std::string(in.SwapTenor));
            CHECK(out.fixing_days == static_cast<int>(in.FixingDays));
            CHECK(out.calendar == std::string(in.Calendar));
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "CmsSpreadOption");
        INFO(path.string() + ": the document has " +
             std::to_string(document.CmsSpreadOption.size()) +
             " CmsSpreadOption element(s), the export " + std::to_string(written));
        CHECK(written == static_cast<int>(document.CmsSpreadOption.size()));

        // The mapper stores canonical spellings of the day count and the roll
        // convention, so the export may differ from the document in those. What
        // has to survive is the value: read the export back, map it again, and
        // require the same entity field for field.
        conventions reparsed;
        load_data(exported, reparsed);
        REQUIRE(reparsed.CmsSpreadOption.size() == document.CmsSpreadOption.size());

        const auto remapped = conventions_mapper::map(reparsed);
        for (std::size_t i = 0; i < document.CmsSpreadOption.size(); ++i) {
            INFO(path.string() + ": CmsSpreadOption element " + std::to_string(i));
            CHECK(remapped.cms_spread_option[i] == mapped.cms_spread_option[i]);
        }
    }

    // Twelve of the seventy-two files carry it, and eleven of them carried
    // nothing else unmodelled when it went in.
    CHECK(files_with_cms == 12);
}

TEST_CASE("conventions_cross_currency_fix_float_round_trips", tags) {
    int files_with_fix_float = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.CrossCurrencyFixFloat.empty())
            continue;

        ++files_with_fix_float;
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.cross_currency_fix_float.size() == document.CrossCurrencyFixFloat.size());

        for (std::size_t i = 0; i < document.CrossCurrencyFixFloat.size(); ++i) {
            const auto& in = document.CrossCurrencyFixFloat[i];
            const auto& out = mapped.cross_currency_fix_float[i];
            CHECK(out.id == std::string(in.Id));
            CHECK(out.settlement_days == static_cast<int>(in.SettlementDays));
            CHECK(out.settlement_calendar == std::string(in.SettlementCalendar));
            CHECK(out.fixed_currency == to_string(in.FixedCurrency));
            CHECK(out.index == std::string(in.Index));
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "CrossCurrencyFixFloat");
        INFO(path.string() + ": the document has " +
             std::to_string(document.CrossCurrencyFixFloat.size()) +
             " CrossCurrencyFixFloat element(s), the export " + std::to_string(written));
        CHECK(written == static_cast<int>(document.CrossCurrencyFixFloat.size()));

        // The mapper stores canonical spellings of the frequency, the two
        // conventions and the day count, so the export may differ from the
        // document in those. What has to survive is the value: read the export
        // back, map it again, and require the same entity field for field.
        conventions reparsed;
        load_data(exported, reparsed);
        REQUIRE(reparsed.CrossCurrencyFixFloat.size() == document.CrossCurrencyFixFloat.size());

        const auto remapped = conventions_mapper::map(reparsed);
        for (std::size_t i = 0; i < document.CrossCurrencyFixFloat.size(); ++i) {
            INFO(path.string() + ": CrossCurrencyFixFloat element " + std::to_string(i));
            CHECK(remapped.cross_currency_fix_float[i] == mapped.cross_currency_fix_float[i]);
        }
    }

    // Eighteen of the seventy-two files carry it, and six of them carried
    // nothing else unmodelled when it went in.
    CHECK(files_with_fix_float == 18);
}

// The corpus sets none of the element's nine optional fields, so the walk above
// cannot reach the half of the mapping that reads them. This document sets all
// nine, including a zero-valued fixing days and a false resettable flag, which
// are the two cases a mapper that tested the value rather than the presence
// would drop.
TEST_CASE("conventions_cross_currency_fix_float_optional_fields_survive", tags) {
    const std::string text = R"(<Conventions>
  <CrossCurrencyFixFloat>
    <Id>USD-TRY-XCCY-FIX-FLOAT-CONVENTIONS</Id>
    <SettlementDays>2</SettlementDays>
    <SettlementCalendar>US,UK,TRY</SettlementCalendar>
    <SettlementConvention>F</SettlementConvention>
    <FixedCurrency>TRY</FixedCurrency>
    <FixedFrequency>Semiannual</FixedFrequency>
    <FixedConvention>MF</FixedConvention>
    <FixedDayCounter>A360</FixedDayCounter>
    <Index>USD-LIBOR-3M</Index>
    <EOM>true</EOM>
    <IsResettable>true</IsResettable>
    <FloatIndexIsResettable>false</FloatIndexIsResettable>
    <IncludeSpread>true</IncludeSpread>
    <Lookback>1D</Lookback>
    <FixingDays>0</FixingDays>
    <RateCutoff>2</RateCutoff>
    <IsAveraged>true</IsAveraged>
    <ObservationShift>true</ObservationShift>
  </CrossCurrencyFixFloat>
</Conventions>)";

    conventions document;
    load_data(text, document);
    REQUIRE(document.CrossCurrencyFixFloat.size() == 1);

    const auto mapped = conventions_mapper::map(document);
    REQUIRE(mapped.cross_currency_fix_float.size() == 1);
    const auto& out = mapped.cross_currency_fix_float[0];
    CHECK(out.eom.has_value());
    CHECK(out.is_resettable.has_value());
    CHECK(out.float_index_is_resettable.has_value());
    CHECK(out.include_spread.has_value());
    REQUIRE(out.lookback.has_value());
    CHECK(*out.lookback == "1D");
    REQUIRE(out.fixing_days.has_value());
    CHECK(*out.fixing_days == 0);
    REQUIRE(out.rate_cutoff.has_value());
    CHECK(*out.rate_cutoff == 2);
    CHECK(out.is_averaged.has_value());
    CHECK(out.observation_shift.has_value());

    const std::string exported = save_data(conventions_mapper::reverse(mapped));
    conventions reparsed;
    load_data(exported, reparsed);
    const auto remapped = conventions_mapper::map(reparsed);
    REQUIRE(remapped.cross_currency_fix_float.size() == 1);
    CHECK(remapped.cross_currency_fix_float[0] == out);
}

TEST_CASE("conventions_inflation_swap_round_trips", tags) {
    int files_with_inflation_swap = 0;
    int files_with_a_publication_schedule = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.InflationSwap.empty())
            continue;

        ++files_with_inflation_swap;
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.inflation_swap.size() == document.InflationSwap.size());

        bool carries_a_schedule = false;
        for (std::size_t i = 0; i < document.InflationSwap.size(); ++i) {
            const auto& in = document.InflationSwap[i];
            const auto& out = mapped.inflation_swap[i];
            CHECK(out.id == std::string(in.Id));
            CHECK(out.index == std::string(in.Index));
            CHECK(out.fix_calendar == std::string(in.FixCalendar));
            CHECK(out.inflation_calendar == std::string(in.InflationCalendar));
            if (in.PublicationSchedule)
                carries_a_schedule = true;
        }

        // A schedule of rules, dates and derived groups has no column to hold
        // it, so the mapper counts it. A file that carries one must therefore
        // name it, and must not be in the round-trip set.
        if (carries_a_schedule) {
            ++files_with_a_publication_schedule;
            CHECK(mapped.unmodelled.count("InflationSwap.PublicationSchedule") == 1);
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "InflationSwap");
        INFO(path.string() + ": the document has " +
             std::to_string(document.InflationSwap.size()) +
             " InflationSwap element(s), the export " + std::to_string(written));
        CHECK(written == static_cast<int>(document.InflationSwap.size()));

        // The mapper stores canonical spellings of the conventions and the day
        // count, so the export may differ from the document in those. What has
        // to survive is the value: read the export back, map it again, and
        // require the same entity field for field.
        conventions reparsed;
        load_data(exported, reparsed);
        REQUIRE(reparsed.InflationSwap.size() == document.InflationSwap.size());

        const auto remapped = conventions_mapper::map(reparsed);
        for (std::size_t i = 0; i < document.InflationSwap.size(); ++i) {
            INFO(path.string() + ": InflationSwap element " + std::to_string(i));
            CHECK(remapped.inflation_swap[i] == mapped.inflation_swap[i]);
        }
    }

    // Twenty-two of the seventy-two files carry it, and three of them carried
    // nothing else unmodelled when it went in. Two carry a publication
    // schedule, and both carry other unmodelled categories as well.
    CHECK(files_with_inflation_swap == 22);
    CHECK(files_with_a_publication_schedule == 2);
}

TEST_CASE("conventions_bma_basis_swap_round_trips", tags) {
    int files_with_bma = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.BMABasisSwap.empty())
            continue;

        ++files_with_bma;
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.bma_basis_swap.size() == document.BMABasisSwap.size());

        for (std::size_t i = 0; i < document.BMABasisSwap.size(); ++i) {
            const auto& in = document.BMABasisSwap[i];
            const auto& out = mapped.bma_basis_swap[i];
            CHECK(out.id == std::string(in.Id));
            CHECK(out.index == std::string(in.Index));
            CHECK(out.bma_index == std::string(in.BMAIndex));
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "BMABasisSwap");
        INFO(path.string() + ": the document has " + std::to_string(document.BMABasisSwap.size()) +
             " BMABasisSwap element(s), the export " + std::to_string(written));
        CHECK(written == static_cast<int>(document.BMABasisSwap.size()));

        // The mapper stores a canonical spelling of the two payment
        // conventions, so the export writes a spelling the document may not
        // have used. What has to survive is the value: read the export back,
        // map it again, and require the same entity field for field.
        conventions reparsed;
        load_data(exported, reparsed);
        REQUIRE(reparsed.BMABasisSwap.size() == document.BMABasisSwap.size());

        const auto remapped = conventions_mapper::map(reparsed);
        for (std::size_t i = 0; i < document.BMABasisSwap.size(); ++i) {
            INFO(path.string() + ": BMABasisSwap element " + std::to_string(i));
            CHECK(remapped.bma_basis_swap[i] == mapped.bma_basis_swap[i]);
        }
    }

    // Sixteen of the seventy-two files carry it, and three of them carried
    // nothing else unmodelled when it went in.
    CHECK(files_with_bma == 16);
}

TEST_CASE("conventions_zero_inflation_index_round_trips", tags) {
    int files_with_inflation_index = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.ZeroInflationIndex.empty())
            continue;

        ++files_with_inflation_index;
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.zero_inflation_index.size() == document.ZeroInflationIndex.size());

        for (std::size_t i = 0; i < document.ZeroInflationIndex.size(); ++i) {
            const auto& in = document.ZeroInflationIndex[i];
            const auto& out = mapped.zero_inflation_index[i];
            CHECK(out.id == std::string(in.Id));
            CHECK(out.region_name == std::string(in.RegionName));
            CHECK(out.region_code == std::string(in.RegionCode));
            CHECK(out.frequency == to_string(in.Frequency));
            CHECK(out.availability_lag == std::string(in.AvailabilityLag));
            CHECK(out.currency == to_string(in.Currency));
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "ZeroInflationIndex");
        INFO(path.string() + ": the document has " +
             std::to_string(document.ZeroInflationIndex.size()) +
             " ZeroInflationIndex element(s), the export " + std::to_string(written));
        CHECK(written == static_cast<int>(document.ZeroInflationIndex.size()));

        // The document spells a boolean thirteen ways, so the stored flag is
        // compared the only way that does not restate the mapper's spelling
        // table: read the export back, map it again, and require the same
        // entity field for field.
        conventions reparsed;
        load_data(exported, reparsed);
        REQUIRE(reparsed.ZeroInflationIndex.size() == document.ZeroInflationIndex.size());

        const auto remapped = conventions_mapper::map(reparsed);
        for (std::size_t i = 0; i < document.ZeroInflationIndex.size(); ++i) {
            INFO(path.string() + ": ZeroInflationIndex element " + std::to_string(i));
            CHECK(remapped.zero_inflation_index[i] == mapped.zero_inflation_index[i]);
        }
    }

    // Six of the seventy-two files carry it, and five of them carried nothing
    // else unmodelled when it went in.
    CHECK(files_with_inflation_index == 6);
}

// The entity holds the index's seven scalar fields and not its rebasing events,
// which ORE types as a list of doubles and the refdata schema has no column
// for. No shipped file sets one, so the corpus walk above cannot show what
// happens when one appears. This case builds that document and asserts the
// mapper counts the field instead of dropping it: a skip that is counted keeps
// the file out of the round-trip set, and a skip that is silent loses content.
TEST_CASE("conventions_zero_inflation_index_rebasing_events_are_counted", tags) {
    zeroInflationIndexType index;
    static_cast<std::string&>(index.Id) = "UKRPI";
    static_cast<std::string&>(index.RegionName) = "UK";
    static_cast<std::string&>(index.RegionCode) = "UK";
    index.Revised = bool_::False;
    index.Frequency = frequencyType::Monthly;
    static_cast<std::string&>(index.AvailabilityLag) = "1M";
    index.Currency = currencyCode::GBP;

    conventions document;
    document.ZeroInflationIndex.push_back(index);
    CHECK(conventions_mapper::map(document).unmodelled.empty());

    zeroInflationIndexType_RebasingEvents_t events;
    events.Event.push_back(zeroInflationIndexType_RebasingEvents_t_Event_t(1.5));
    index.RebasingEvents = events;
    document.ZeroInflationIndex.clear();
    document.ZeroInflationIndex.push_back(index);

    const auto mapped = conventions_mapper::map(document);
    REQUIRE(mapped.unmodelled.count("ZeroInflationIndex.RebasingEvents") == 1);
    CHECK(mapped.unmodelled.at("ZeroInflationIndex.RebasingEvents") == 1);
}

TEST_CASE("conventions_tenor_basis_swap_round_trips", tags) {
    int files_with_basis_swap = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.TenorBasisSwap.empty())
            continue;

        ++files_with_basis_swap;
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.tenor_basis_swap.size() == document.TenorBasisSwap.size());

        for (std::size_t i = 0; i < document.TenorBasisSwap.size(); ++i) {
            const auto& in = document.TenorBasisSwap[i];
            const auto& out = mapped.tenor_basis_swap[i];
            CHECK(out.id == std::string(in.Id));
            if (in.PayIndex)
                CHECK(out.pay_index == std::string(*in.PayIndex));
            if (in.ReceiveIndex)
                CHECK(out.receive_index == std::string(*in.ReceiveIndex));
            if (in.LongIndex)
                CHECK(out.long_index == std::string(*in.LongIndex));
            if (in.ShortIndex)
                CHECK(out.short_index == std::string(*in.ShortIndex));
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "TenorBasisSwap");
        INFO(path.string() + ": the document has " +
             std::to_string(document.TenorBasisSwap.size()) +
             " TenorBasisSwap element(s), the export " + std::to_string(written));
        CHECK(written == static_cast<int>(document.TenorBasisSwap.size()));

        // The mapper stores a canonical spelling of the sub-periods coupon
        // type, so the export writes a spelling the document may not have used.
        // What has to survive is the value: reading the export back and mapping
        // it again must land on the same entity, field for field.
        conventions reparsed;
        load_data(exported, reparsed);
        REQUIRE(reparsed.TenorBasisSwap.size() == document.TenorBasisSwap.size());

        const auto remapped = conventions_mapper::map(reparsed);
        for (std::size_t i = 0; i < document.TenorBasisSwap.size(); ++i) {
            INFO(path.string() + ": TenorBasisSwap element " + std::to_string(i));
            CHECK(remapped.tenor_basis_swap[i] == mapped.tenor_basis_swap[i]);
        }
    }

    // Twenty-nine of the seventy-two files carry it, and five of them carried
    // nothing else unmodelled when it went in.
    CHECK(files_with_basis_swap == 29);
}

TEST_CASE("conventions_tenor_basis_two_swap_round_trips", tags) {
    int files_with_two_swap = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto document = load(path);
        if (document.TenorBasisTwoSwap.empty())
            continue;

        ++files_with_two_swap;
        const auto mapped = conventions_mapper::map(document);
        REQUIRE(mapped.tenor_basis_two_swap.size() == document.TenorBasisTwoSwap.size());

        for (std::size_t i = 0; i < document.TenorBasisTwoSwap.size(); ++i) {
            const auto& in = document.TenorBasisTwoSwap[i];
            const auto& out = mapped.tenor_basis_two_swap[i];
            CHECK(out.id == std::string(in.Id));
            CHECK(out.calendar == std::string(in.Calendar));
            CHECK(out.long_index == std::string(in.LongIndex));
            CHECK(out.short_index == std::string(in.ShortIndex));
        }

        const std::string exported = save_data(conventions_mapper::reverse(mapped));
        const int written = element_count(exported, "TenorBasisTwoSwap");
        INFO(path.string() + ": the document has " +
             std::to_string(document.TenorBasisTwoSwap.size()) +
             " TenorBasisTwoSwap element(s), the export " + std::to_string(written));
        CHECK(written == static_cast<int>(document.TenorBasisTwoSwap.size()));

        // The mapper stores a canonical spelling of each frequency, convention
        // and day counter, so the export writes a spelling the document may not
        // have used. What has to survive is the value: reading the export back
        // and mapping it again must land on the same entity, field for field.
        conventions reparsed;
        load_data(exported, reparsed);
        REQUIRE(reparsed.TenorBasisTwoSwap.size() == document.TenorBasisTwoSwap.size());

        const auto remapped = conventions_mapper::map(reparsed);
        for (std::size_t i = 0; i < document.TenorBasisTwoSwap.size(); ++i) {
            INFO(path.string() + ": TenorBasisTwoSwap element " + std::to_string(i));
            CHECK(remapped.tenor_basis_two_swap[i] == mapped.tenor_basis_two_swap[i]);
        }
    }

    // The largest single category left when it went in: it is what stood between
    // twelve files and a clean round trip on its own.
    CHECK(files_with_two_swap == 46);
}

TEST_CASE("conventions_files_using_only_modelled_categories_round_trip", tags) {
    const auto kind = conventions_kind();
    int walked = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        // A file that carries a category the mapper does not model cannot round
        // trip, and saying which those are is the measurement's job rather than
        // this case's.
        if (!conventions_mapper::map(load(path)).unmodelled.empty())
            continue;

        ++walked;
        const auto outcome = kind.check(path);
        INFO(outcome.detail);
        CHECK(outcome.passed);
    }

    // Sixty-nine of the seventy-two once CommodityFuture landed: the two files
    // it cleared on its own on top of the sixty-seven already clean. The count
    // is asserted so that a change in it is noticed rather than absorbed.
    CHECK(walked == 69);
}

// Hidden by default, and run on demand:
//   ores.ore.core.tests "[.][conventions]"
// The categories below are the ones the corpus needs, ordered by how many files
// use them. Modelling starts at the top of that list.
// Which category, modelled next, would clear the most files outright. A file
// round trips only when every one of its categories is modelled, so the
// shortest path is the category that appears alone most often.
TEST_CASE("conventions_files_cleared_by_each_category", "[.][conventions][measurement]") {
    std::map<std::string, int> cleared_by;
    std::map<int, int> files_by_category_count;
    int files = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto mapped = conventions_mapper::map(load(path));
        ++files;
        ++files_by_category_count[static_cast<int>(mapped.unmodelled.size())];
        if (mapped.unmodelled.size() == 1)
            ++cleared_by[mapped.unmodelled.begin()->first];
    }

    WARN("conventions files=" + std::to_string(files));
    for (const auto& [count, file_count] : files_by_category_count) {
        WARN("  " + std::to_string(count) + " unmodelled category(ies) in " +
             std::to_string(file_count) + " file(s)");
    }
    for (const auto& [name, file_count] : cleared_by) {
        WARN("  " + name + " alone would clear " + std::to_string(file_count) + " file(s)");
    }

    CHECK(files == 72);
}

TEST_CASE("conventions_unmodelled_categories_measurement", "[.][conventions][measurement]") {
    std::map<std::string, int> files_by_category;
    std::map<std::string, int> elements_by_category;
    int files = 0;
    int files_with_unmodelled = 0;

    for (const auto& path : ores::ore::xml::files_of_kind("conventions", corpus_root())) {
        const auto mapped = conventions_mapper::map(load(path));
        ++files;
        if (!mapped.unmodelled.empty())
            ++files_with_unmodelled;
        for (const auto& [name, count] : mapped.unmodelled) {
            ++files_by_category[name];
            elements_by_category[name] += static_cast<int>(count);
        }
    }

    WARN("conventions files=" + std::to_string(files) +
         " with unmodelled categories=" + std::to_string(files_with_unmodelled));
    for (const auto& [name, count] : files_by_category) {
        WARN("  " + name + " in " + std::to_string(count) + " file(s), " +
             std::to_string(elements_by_category[name]) + " element(s)");
    }

    CHECK(files == 72);
}

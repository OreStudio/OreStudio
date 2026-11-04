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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/xml/document.hpp"
#include "ores.ore.core/xml/document_kind.hpp"
#include "ores.ore.core/xml/roundtrip.hpp"
#include "ores.ore.core/xml/value_differ.hpp"
#include "ores.ore.core/xml/value_fingerprint.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include "test_suite.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cctype>
#include <cstddef>
#include <filesystem>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <string_view>
#include <vector>

/**
 * @file xml_portfolio_corpus_content_roundtrip_tests.cpp
 * @brief Every trade of the ORE example corpus comes back as it was read.
 *
 * Each portfolio is read into the domain and written back, in memory, and each
 * trade is compared with the trade it was read as, value by value. A value the
 * model loses, adds or changes fails here, unless the losses below name it.
 */

namespace {

namespace fs = std::filesystem;
using namespace ores::logging;
using namespace ores::ore::xml;

/**
 * @brief The differences the model is known to make, as trade type, change and
 * path.
 *
 * Asserted for equality: a new difference fails, and a difference that stops
 * happening fails until its entry is removed, so the list can only shrink. An
 * entry is a gap in what we model, and each one is a reason to fix the model
 * rather than a licence to keep it.
 */
const std::set<std::string>& known_losses() {
    static const std::set<std::string> losses = {
        "CommodityAveragePriceOption changed Trade/CommodityAveragePriceOptionData/Strike",
        "CommodityOptionStrip lost Trade/CommodityOptionStripData/Calls/LongShorts/LongShort",
        "CommodityOptionStrip lost Trade/CommodityOptionStripData/Calls/Strikes/Strike",
        "CommodityOptionStrip lost Trade/CommodityOptionStripData/Premiums/Premium/Amount",
        "CommodityOptionStrip lost Trade/CommodityOptionStripData/Premiums/Premium/Currency",
        "CommodityOptionStrip lost Trade/CommodityOptionStripData/Premiums/Premium/PayDate",
        "CommodityOptionStrip lost Trade/CommodityOptionStripData/Puts/LongShorts/LongShort",
        "CommodityOptionStrip lost Trade/CommodityOptionStripData/Puts/Strikes/Strike",
        "EquityDigitalOption lost Trade/EquityDigitalOptionData/PayoffCurrency",
        "EquityWorstOfBasketSwap changed Trade/EquityWorstOfBasketSwapData/FixedRate",
        "EquityWorstOfBasketSwap changed Trade/EquityWorstOfBasketSwapData/FloatingIndex",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/AccruingFixedCoupons",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/AccumulatingFixedCoupons",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/BermudanKnockIn",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/FloatingFixingSchedule/DerivedSchedule/BaseSchedule",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/FloatingFixingSchedule/DerivedSchedule/Calendar",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/FloatingFixingSchedule/DerivedSchedule/Convention",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/FloatingFixingSchedule/DerivedSchedule/Shift",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/FloatingSpread",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/InitialFixedRate",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/KnockInLevel",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/Underlyings/Underlying[1]/IdentifierType",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/Underlyings/Underlying[2]/IdentifierType",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Barriers/KikoType",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Barriers/ScheduleData/Rules/Calendar",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Barriers/ScheduleData/Rules/Convention",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Barriers/ScheduleData/Rules/EndDate",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Barriers/ScheduleData/Rules/Rule",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Barriers/ScheduleData/Rules/StartDate",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Barriers/ScheduleData/Rules/Tenor",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Quantity",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/SettlementDate",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Strike",
        "FxSwap changed Trade/FxSwapData/FarBoughtAmount",
        "FxSwap changed Trade/FxSwapData/FarSoldAmount",
        "FxSwap lost Trade/FxSwapData/FarDate",
        "PerformanceOption_01 lost Trade/PerformanceOption01Data/StrikeIncluded",
        "PerformanceOption_01 lost Trade/PerformanceOption01Data/Underlyings/Underlying[1]/PriceType",
        "PerformanceOption_01 lost Trade/PerformanceOption01Data/Underlyings/Underlying[1]/Weight",
        "PerformanceOption_01 lost Trade/PerformanceOption01Data/Underlyings/Underlying[2]/PriceType",
        "PerformanceOption_01 lost Trade/PerformanceOption01Data/Underlyings/Underlying[2]/Weight",
        "PerformanceOption_01 lost Trade/PerformanceOption01Data/Underlyings/Underlying[3]/Weight",
        "PerformanceOption_01 lost Trade/PerformanceOption01Data/Underlyings/Underlying[4]/Weight",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/Derivative[4]/SubTrade/@id",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Identifier",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Quantity",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Underlying[1]/IdentifierType",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Underlying[1]/Name",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Underlying[1]/Type",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Underlying[1]/Weight",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Underlying[2]/IdentifierType",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Underlying[2]/Name",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Underlying[2]/Type",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Underlying[2]/Weight",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/EquityOptionPositionData/Underlying[1]/OptionData/ExerciseDates/ExerciseDate",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/EquityOptionPositionData/Underlying[1]/Underlying/IdentifierType",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/EquityOptionPositionData/Underlying[2]/OptionData/ExerciseDates/ExerciseDate",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/EquityOptionPositionData/Underlying[2]/Underlying/IdentifierType",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade[2]/EquityOptionPositionData/Underlying[1]/OptionData/ExerciseDates/ExerciseDate",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade[2]/EquityOptionPositionData/Underlying[1]/Underlying/IdentifierType",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade[2]/EquityOptionPositionData/Underlying[2]/OptionData/ExerciseDates/ExerciseDate",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade[2]/EquityOptionPositionData/Underlying[2]/Underlying/IdentifierType",
    };
    return losses;
}

/**
 * @brief The ORE trade types no family maps, which come back as their type and
 * envelope alone.
 *
 * Asserted for equality, as the losses are.
 */
const std::set<std::string>& known_unmapped_types() {
    static const std::set<std::string> types = {};
    return types;
}

/// The last element of a path, without the position of a repeated element.
std::string_view leaf_of(std::string_view path) {
    const auto slash = path.rfind('/');
    auto leaf = slash == std::string_view::npos ? path : path.substr(slash + 1);
    const auto bracket = leaf.find('[');
    return bracket == std::string_view::npos ? leaf : leaf.substr(0, bracket);
}

/**
 * @brief The value as ORE reads it, for the spellings ORE reads as one.
 *
 * Only ORE's own equivalences, taken from its parsers (OREData
 * ored/utilities/parsers.cpp): parseBool, parseBusinessDayConvention and
 * parseDate, which reads a date of eight characters as it reads one of ten. A
 * spelling ORE reads differently, such as F and MF, stays a difference.
 */
std::string ore_equivalent(std::string_view path, const std::string& value) {
    static const std::map<std::string, std::string, std::less<>> booleans = {{"Y", "true"},
                                                                             {"YES", "true"},
                                                                             {"TRUE", "true"},
                                                                             {"True", "true"},
                                                                             {"true", "true"},
                                                                             {"N", "false"},
                                                                             {"NO", "false"},
                                                                             {"FALSE", "false"},
                                                                             {"False", "false"},
                                                                             {"false", "false"}};
    static const std::map<std::string, std::string, std::less<>> conventions = {
        {"F", "Following"},
        {"Following", "Following"},
        {"FOLLOWING", "Following"},
        {"MF", "ModifiedFollowing"},
        {"ModifiedFollowing", "ModifiedFollowing"},
        {"Modified Following", "ModifiedFollowing"},
        {"MODIFIEDF", "ModifiedFollowing"},
        {"MODFOLLOWING", "ModifiedFollowing"},
        {"P", "Preceding"},
        {"Preceding", "Preceding"},
        {"PRECEDING", "Preceding"},
        {"MP", "ModifiedPreceding"},
        {"ModifiedPreceding", "ModifiedPreceding"},
        {"Modified Preceding", "ModifiedPreceding"},
        {"MODIFIEDP", "ModifiedPreceding"},
        {"U", "Unadjusted"},
        {"Unadjusted", "Unadjusted"},
        {"INDIFF", "Unadjusted"},
        {"NONE", "Unadjusted"},
        {"HalfMonthModifiedFollowing", "HalfMonthModifiedFollowing"},
        {"HMMF", "HalfMonthModifiedFollowing"},
        {"Half Month Modified Following", "HalfMonthModifiedFollowing"},
        {"HALFMONTHMF", "HalfMonthModifiedFollowing"},
        {"HalfMonthMF", "HalfMonthModifiedFollowing"},
        {"NEAREST", "Nearest"},
    };

    if (const auto found = booleans.find(value); found != booleans.end())
        return found->second;
    const auto leaf = leaf_of(path);
    if (leaf.ends_with("Convention")) {
        if (const auto found = conventions.find(value); found != conventions.end())
            return found->second;
    }
    if (leaf.find("Date") != std::string_view::npos && value.size() == 8 &&
        std::ranges::all_of(value, [](unsigned char c) { return std::isdigit(c) != 0; }))
        return value.substr(0, 4) + "-" + value.substr(4, 2) + "-" + value.substr(6, 2);
    return value;
}

/// A fingerprint with every value as ORE reads it, sorted again.
std::vector<value_entry> as_ore_reads(std::vector<value_entry> values) {
    for (auto& entry : values)
        entry.value = ore_equivalent(entry.path, entry.value);
    std::ranges::sort(values, [](const value_entry& lhs, const value_entry& rhs) {
        return lhs.path != rhs.path ? lhs.path < rhs.path : lhs.value < rhs.value;
    });
    return values;
}

/// The trade type a trade's fingerprint states.
std::string trade_type_of(const keyed_fingerprint& trade) {
    for (const auto& value : trade.values)
        if (value.path == "Trade/TradeType")
            return value.value;
    return {};
}

std::string_view name_of(value_change change) {
    switch (change) {
        case value_change::lost:
            return "lost";
        case value_change::added:
            return "added";
        case value_change::changed:
            break;
    }
    return "changed";
}

/// One line per difference, which the known losses name.
std::string loss_of(const std::string& trade_type, const value_difference& difference) {
    return trade_type + " " + std::string(name_of(difference.change)) + " " + difference.path;
}

/// Lists a set, one entry a line, for a failure message.
std::string listed(const std::set<std::string>& entries) {
    std::string out;
    for (const auto& entry : entries)
        out += "\n  " + entry;
    return out;
}

}

TEST_CASE("every trade in the ORE example corpus comes back as it was read",
          "[ore][xml][roundtrip][external]") {
    auto lg(make_logger(test_suite));
    using ores::platform::filesystem::file;

    const auto input =
        ores::testing::project_root::resolve("external/ore/examples/Products/Example_Trades");
    REQUIRE(fs::exists(input));

    std::set<std::string> losses;
    std::set<std::string> unmapped;
    // An example of each loss, so a failure shows the values and the file.
    std::map<std::string, std::string> example_of;
    std::size_t portfolios = 0;
    std::size_t trades = 0;

    for (const auto& entry : fs::recursive_directory_iterator(input)) {
        if (!entry.is_regular_file() || entry.path().extension() != ".xml")
            continue;
        if (detect_document_kind(entry.path()) != document_kind::portfolio)
            continue;
        ++portfolios;

        const auto relative = fs::relative(entry.path(), input).generic_string();
        const auto text = file::read_content(entry.path());
        const auto written = roundtrip_portfolio(text, relative);
        for (const auto& type : written.passthrough_types)
            unmapped.insert(type);

        const auto before = document::read(text);
        const auto after = document::read(written.xml);
        REQUIRE(before);
        REQUIRE(after);

        std::map<std::string, std::vector<value_entry>> written_trades;
        for (auto& trade : value_fingerprints_of(*after, "Trade", "id"))
            written_trades[trade.key] = as_ore_reads(std::move(trade.values));

        for (const auto& trade : value_fingerprints_of(*before, "Trade", "id")) {
            ++trades;
            const auto type = trade_type_of(trade);
            const auto found = written_trades.find(trade.key);
            const auto& other =
                found == written_trades.end() ? std::vector<value_entry>{} : found->second;
            for (const auto& difference : diff_values(as_ore_reads(trade.values), other)) {
                const auto loss = loss_of(type, difference);
                losses.insert(loss);
                example_of.try_emplace(
                    loss, relative + " trade " + trade.key + ": " + describe(difference));
            }
        }
    }

    INFO("portfolios=" << portfolios << " trades=" << trades << " differences=" << losses.size()
                       << " known=" << known_losses().size());
    CHECK(portfolios > 0);

    if (losses != known_losses()) {
        std::set<std::string> new_losses;
        std::set<std::string> fixed;
        for (const auto& loss : losses)
            if (!known_losses().contains(loss))
                new_losses.insert(loss);
        for (const auto& loss : known_losses())
            if (!losses.contains(loss))
                fixed.insert(loss);

        std::string detail = "the trades the model writes back differ from what it read.";
        if (!new_losses.empty()) {
            detail += "\nNew differences, with an example of each:";
            for (const auto& loss : new_losses)
                detail += "\n  " + loss + "\n      " + example_of[loss];
        }
        if (!fixed.empty())
            detail += "\nNo longer happening, so remove from known_losses():" + listed(fixed);
        FAIL_CHECK(detail);
    }

    if (unmapped != known_unmapped_types()) {
        std::string detail =
            "the trade types no family maps changed. Mapped none of:" + listed(unmapped) +
            "\nKnown as unmapped:" + listed(known_unmapped_types());
        FAIL_CHECK(detail);
    }
}

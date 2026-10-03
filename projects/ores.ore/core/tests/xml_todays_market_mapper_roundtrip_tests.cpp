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
#include "ores.ore.core/domain/todays_market_mapper.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <set>
#include <string>
#include <vector>

/**
 * @file xml_todays_market_mapper_roundtrip_tests.cpp
 * @brief The today's market document, mapped and mapped back.
 *
 * The walk is the proof over the corpus. The cases below it cover what the
 * corpus cannot: a document that uses all twenty-four collections and all
 * twenty-four bindings, including the one collection no corpus file uses, the
 * optional attributes written empty and left out, and a row the mapper does not
 * know.
 */

namespace {

const std::string tags("[ore][xml][roundtrip][todaysmarket]");

std::filesystem::path corpus_root() {
    return ores::testing::project_root::resolve("external/ore/examples");
}

ores::ore::xml::roundtrip_kind todays_market_kind() {
    return ores::ore::xml::make_roundtrip_kind<ores::ore::domain::todaysmarket,
                                               ores::ore::domain::mapped_todays_market>(
        "today's market",
        "todaysmarket",
        &ores::ore::domain::todays_market_mapper::map,
        &ores::ore::domain::todays_market_mapper::reverse,
        ores::ore::xml::parsed_text_difference<ores::ore::domain::todaysmarket>);
}

using namespace ores::ore::domain;

template <typename Wrapper, typename Entry>
void add(xsd::vector<Wrapper>& collections,
         xsd::vector<Entry> Wrapper::*entries,
         const char* id,
         std::vector<Entry> values) {
    Wrapper w;
    if (id)
        w.id = std::string(id);
    for (auto& v : values)
        (w.*entries).push_back(std::move(v));
    collections.push_back(std::move(w));
}

template <typename Entry>
Entry named(const std::string& name, const std::string& target) {
    Entry e;
    e.name = name;
    static_cast<xsd::string&>(e) = target;
    return e;
}

template <typename Entry>
Entry keyed(std::optional<std::string> key,
            std::optional<currencyCode> currency,
            const std::string& target) {
    Entry e;
    if (key)
        e.key = *key;
    if (currency)
        e.currency = *currency;
    static_cast<xsd::string&>(e) = target;
    return e;
}

// Every collection the schema allows, each with an entry, and one configuration
// that binds all of them. Securities is left without an id, which the schema
// allows, so the absent id is proven to stay absent.
todaysmarket every_collection() {
    todaysmarket d;
    add(d.YieldCurves,
        &yieldCurvesType::YieldCurve,
        "default",
        {named<yieldCurvesType_YieldCurve_t>("EUR1D", "Yield/EUR/EUR1D")});
    add(d.IndexForwardingCurves,
        &indexForwardingCurvesType::Index,
        "default",
        {named<indexForwardingCurvesType_Index_t>("EUR-EURIBOR-6M", "Yield/EUR/EUR6M")});
    add(d.ZeroInflationIndexCurves,
        &zeroInflationIndexCurvesType::ZeroInflationIndexCurve,
        "default",
        {named<zeroInflationIndexCurvesType_ZeroInflationIndexCurve_t>("EUHICPXT",
                                                                       "Inflation/EUHICPXT/ZC")});
    add(d.YYInflationIndexCurves,
        &yyInflationIndexCurvesType::YYInflationIndexCurve,
        "default",
        {named<yyInflationIndexCurvesType_YYInflationIndexCurve_t>("EUHICPXT",
                                                                   "Inflation/EUHICPXT/YY")});
    add(d.YieldVolatilities,
        &yieldVolatilitiesType::YieldVolatility,
        "default",
        {named<yieldVolatilitiesType_YieldVolatility_t>("BOND", "YieldVolatility/BOND")});
    add(d.CDSVolatilities,
        &cdsVolatilitiesType::CDSVolatility,
        "default",
        {named<cdsVolatilitiesType_CDSVolatility_t>("CPTY_A", "CDSVolatility/CPTY_A")});
    add(d.DefaultCurves,
        &defaultCurvesType::DefaultCurve,
        "default",
        {named<defaultCurvesType_DefaultCurve_t>("CPTY_A", "Default/USD/CPTY_A")});
    add(d.EquityCurves,
        &equityCurvesType::EquityCurve,
        "default",
        {named<equityCurvesType_EquityCurve_t>("SP5", "Equity/USD/SP5")});
    add(d.EquityVolatilities,
        &equityVolatilitiesType::EquityVolatility,
        "default",
        {named<equityVolatilitiesType_EquityVolatility_t>("SP5", "EquityVolatility/USD/SP5")});
    add(d.Securities,
        &securitiesType::Security,
        nullptr,
        {named<securitiesType_Security_t>("BOND1", "Security/BOND1")});
    add(d.BaseCorrelations,
        &baseCorrelationsType::BaseCorrelation,
        "default",
        {named<baseCorrelationsType_BaseCorrelation_t>("CDXIG", "BaseCorrelation/CDXIG")});
    add(d.CommodityCurves,
        &commodityCurvesType::CommodityCurve,
        "default",
        {named<commodityCurvesType_CommodityCurve_t>("GOLD", "Commodity/USD/GOLD")});
    add(d.CommodityVolatilities,
        &commodityVolatilitiesType::CommodityVolatility,
        "default",
        {named<commodityVolatilitiesType_CommodityVolatility_t>("GOLD",
                                                                "CommodityVolatility/USD/GOLD")});
    add(d.Correlations,
        &correlationsType::Correlation,
        "default",
        {named<correlationsType_Correlation_t>("A&B", "Correlation/A&B")});
    add(d.BondFutureVolatilities,
        &bondFutureVolatilitiesType::BondFutureVolatility,
        "default",
        {named<bondFutureVolatilitiesType_BondFutureVolatility_t>("TY",
                                                                  "BondFutureVolatility/TY")});
    add(d.IntradayPowerPriceCurves,
        &intradayPowerPriceCurvesType::IntradayPowerPriceCurve,
        "default",
        {named<intradayPowerPriceCurvesType_IntradayPowerPriceCurve_t>("PJM", "Power/PJM")});
    add(d.ZeroInflationCapFloorVolatilities,
        &zeroInflationCapFloorVolatilitiesType::ZeroInflationCapFloorVolatility,
        "default",
        {named<zeroInflationCapFloorVolatilitiesType_ZeroInflationCapFloorVolatility_t>(
            "EUHICPXT", "InflationCapFloorVolatility/EUHICPXT")});
    add(d.YYInflationCapFloorVolatilities,
        &yyInflationCapFloorVolatilitiesType::YYInflationCapFloorVolatility,
        "default",
        {named<yyInflationCapFloorVolatilitiesType_YYInflationCapFloorVolatility_t>(
            "EUHICPXT", "YYInflationCapFloorVolatility/EUHICPXT")});

    discountCurvesType_DiscountingCurve_t eur;
    eur.currency = "EUR";
    static_cast<xsd::string&>(eur) = "Yield/EUR/EUR1D";
    add(d.DiscountingCurves, &discountCurvesType::DiscountingCurve, "default", {eur});
    add(d.DiscountingCurves, &discountCurvesType::DiscountingCurve, "inccy", {eur});

    fxSpotsType_FxSpot_t spot;
    spot.pair = "EURUSD";
    static_cast<xsd::string&>(spot) = "FX/EUR/USD";
    add(d.FxSpots, &fxSpotsType::FxSpot, "default", {spot});

    fxVolatilitiesType_FxVolatility_t fx_vol;
    fx_vol.pair = "EURUSD";
    static_cast<xsd::string&>(fx_vol) = "FXVolatility/EUR/USD";
    add(d.FxVolatilities, &fxVolatilitiesType::FxVolatility, "default", {fx_vol});

    // Both optional attributes present, the key written empty, and both absent.
    using swaption = swaptionVolatilitiesType_SwaptionVolatility_t;
    add(d.SwaptionVolatilities,
        &swaptionVolatilitiesType::SwaptionVolatility,
        "default",
        {keyed<swaption>("EUR-EURIBOR-6M", currencyCode::EUR, "SwaptionVolatility/EUR/A"),
         keyed<swaption>("", std::nullopt, "SwaptionVolatility/EUR/B"),
         keyed<swaption>(std::nullopt, std::nullopt, "SwaptionVolatility/EUR/C")});
    using cap_floor = capFloorVolatilitiesType_CapFloorVolatility_t;
    add(d.CapFloorVolatilities,
        &capFloorVolatilitiesType::CapFloorVolatility,
        "default",
        {keyed<cap_floor>(std::nullopt, currencyCode::USD, "CapFloorVolatility/USD")});

    swapIndexCurvesType_SwapIndex_t swap_index;
    swap_index.name = "EUR-CMS-30Y";
    swap_index.Discounting = "EUR-EONIA";
    add(d.SwapIndexCurves, &swapIndexCurvesType::SwapIndex, "default", {swap_index});

    configurationType c;
    c.id = "default";
    c.YieldCurvesId = configurationType_YieldCurvesId_t(std::string("default"));
    c.DiscountingCurvesId = configurationType_DiscountingCurvesId_t(std::string("inccy"));
    c.IndexForwardingCurvesId = configurationType_IndexForwardingCurvesId_t(std::string("default"));
    c.SwapIndexCurvesId = configurationType_SwapIndexCurvesId_t(std::string("default"));
    c.ZeroInflationIndexCurvesId =
        configurationType_ZeroInflationIndexCurvesId_t(std::string("default"));
    c.ZeroInflationCapFloorVolatilitiesId =
        configurationType_ZeroInflationCapFloorVolatilitiesId_t(std::string("default"));
    c.YYInflationIndexCurvesId =
        configurationType_YYInflationIndexCurvesId_t(std::string("default"));
    c.FxSpotsId = configurationType_FxSpotsId_t(std::string("default"));
    c.BaseCorrelationsId = configurationType_BaseCorrelationsId_t(std::string("default"));
    c.FxVolatilitiesId = configurationType_FxVolatilitiesId_t(std::string("default"));
    c.SwaptionVolatilitiesId = configurationType_SwaptionVolatilitiesId_t(std::string("default"));
    c.YieldVolatilitiesId = configurationType_YieldVolatilitiesId_t(std::string("default"));
    c.CapFloorVolatilitiesId = configurationType_CapFloorVolatilitiesId_t(std::string("default"));
    c.CDSVolatilitiesId = configurationType_CDSVolatilitiesId_t(std::string("default"));
    c.DefaultCurvesId = configurationType_DefaultCurvesId_t(std::string("default"));
    c.YYInflationCapFloorVolatilitiesId =
        configurationType_YYInflationCapFloorVolatilitiesId_t(std::string("default"));
    c.EquityCurvesId = configurationType_EquityCurvesId_t(std::string("default"));
    c.EquityVolatilitiesId = configurationType_EquityVolatilitiesId_t(std::string("default"));
    c.SecuritiesId = configurationType_SecuritiesId_t(std::string("default"));
    c.CommodityCurvesId = configurationType_CommodityCurvesId_t(std::string("default"));
    c.CommodityVolatilitiesId = configurationType_CommodityVolatilitiesId_t(std::string("default"));
    c.CorrelationsId = configurationType_CorrelationsId_t(std::string("default"));
    c.BondFutureVolatilitiesId =
        configurationType_BondFutureVolatilitiesId_t(std::string("default"));
    c.IntradayPowerPriceCurvesId =
        configurationType_IntradayPowerPriceCurvesId_t(std::string("default"));
    d.Configuration.push_back(std::move(c));
    return d;
}

}

TEST_CASE("every today's market document round trips through the entities", tags) {
    const auto walk = ores::ore::xml::walk_kind(todays_market_kind(), corpus_root());

    INFO("files: " << walk.files << ", passed: " << walk.passed);
    for (const auto& failure : walk.failures)
        INFO(failure);

    // The corpus holds ninety-eight documents of this kind, and the prefix
    // selects nothing else, so the walk should see all of them.
    CHECK(walk.files == 98);
    CHECK(walk.passed == walk.files);
}

TEST_CASE("a document with every collection and every binding round trips", tags) {
    const auto original = every_collection();
    const auto mapped = todays_market_mapper::map(original);

    std::set<std::string> collections;
    for (const auto& c : mapped.collections)
        collections.insert(c.collection);
    std::set<std::string> bindings;
    for (const auto& b : mapped.bindings)
        bindings.insert(b.collection);
    CHECK(collections.size() == 24);
    CHECK(bindings == collections);

    const auto rebuilt = todays_market_mapper::reverse(mapped);
    const auto difference = ores::ore::xml::parsed_text_difference(original, rebuilt, "synthetic");
    INFO(difference);
    CHECK(difference.empty());
}

TEST_CASE("an optional attribute left out is stored as null, and one written empty is not", tags) {
    const auto mapped = todays_market_mapper::map(every_collection());

    std::vector<const ores::analytics::domain::todays_market_entry*> swaptions;
    for (const auto& e : mapped.entries) {
        if (e.target.starts_with("SwaptionVolatility/"))
            swaptions.push_back(&e);
    }
    REQUIRE(swaptions.size() == 3);
    CHECK(swaptions[0]->key_value_2 == std::optional<std::string>("EUR"));
    CHECK(swaptions[1]->key_value == std::optional<std::string>(""));
    CHECK(!swaptions[1]->key_value_2);
    CHECK(!swaptions[2]->key_value);

    for (const auto& c : mapped.collections) {
        if (c.collection == "Securities")
            CHECK(!c.collection_id);
        else
            CHECK(c.collection_id);
    }
}

TEST_CASE("a collection or a binding the mapper does not know is an error", tags) {
    auto mapped = todays_market_mapper::map(every_collection());

    auto bad_collection = mapped;
    bad_collection.collections.front().collection = "NoSuchCurves";
    CHECK_THROWS_AS(todays_market_mapper::reverse(bad_collection), std::runtime_error);

    auto bad_binding = mapped;
    bad_binding.bindings.front().collection = "NoSuchCurves";
    CHECK_THROWS_AS(todays_market_mapper::reverse(bad_binding), std::runtime_error);
}

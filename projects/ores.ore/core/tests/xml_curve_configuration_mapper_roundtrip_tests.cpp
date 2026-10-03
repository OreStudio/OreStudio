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
#include "ores.ore.core/domain/curve_configuration_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/xml/roundtrip_harness.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <set>
#include <stdexcept>
#include <string>

/**
 * @file xml_curve_configuration_mapper_roundtrip_tests.cpp
 * @brief The curve configuration document, mapped and mapped back.
 *
 * The walk maps every curve configuration document in the corpus to rows and
 * back, and compares the whole document. The cases below the walk cover what
 * no corpus document writes: the refusal of elements the mapper cannot hold,
 * every segment element in one document, and an FX parametric smile.
 */

namespace {

const std::string tags("[ore][xml][roundtrip][curveconfig]");

using namespace ores::ore::domain;

std::filesystem::path corpus_root() {
    return ores::testing::project_root::resolve("external/ore/examples");
}

std::string compare_documents(const curveconfiguration& original,
                              const curveconfiguration& exported,
                              const std::string& path) {
    return ores::ore::xml::parsed_text_difference(original, exported, path);
}

ores::ore::xml::roundtrip_kind curve_configuration_kind() {
    return ores::ore::xml::make_roundtrip_kind<curveconfiguration, mapped_curve_configuration>(
        "curve configuration documents",
        "curveconfig",
        &curve_configuration_mapper::map,
        &curve_configuration_mapper::reverse,
        &compare_documents);
}

template <typename T>
void set(T& target, const std::string& value) {
    static_cast<std::string&>(target) = value;
}

quoteType quotes(std::initializer_list<const char*> values) {
    quoteType q;
    for (const auto* v : values) {
        quoteType_Quote_t item;
        set(item, v);
        q.Quote.push_back(std::move(item));
    }
    return q;
}

// One yield curve holding one segment of every element the schema allows,
// including the four no corpus file uses, with optional settings both written
// and left out.
curveconfiguration every_segment() {
    curveconfiguration d;
    yieldCurve y;
    set(y.CurveId, "EUR-ALL");
    set(y.CurveDescription, "Every segment element");
    y.Currency = currencyCode::EUR;
    set(y.DiscountCurve, "EUR-ALL");
    y.YieldCurveDayCounter = dayCounter::Actual_365__Fixed_;
    y.Tolerance = 1.0e-12;
    y.Extrapolation = bool_::Y;
    y.BootstrapConfig = bootstrapConfigType{};
    y.Report = yieldCurveReport{};

    directSegmentType direct;
    direct.Type = directSegmentTypeType::Zero;
    direct.Quotes = quotes({"ZERO/RATE/EUR/1Y"});
    y.Segments.Direct.push_back(direct);

    simpleSegmentType simple;
    simple.Type = simpleSegmentTypeType::Deposit;
    simple.Quotes = quotes({"MM/RATE/EUR/0D/1D", "MM/RATE/EUR/1D/1D"});
    simple.Quotes.Quote.back().optional = std::string("true");
    set(simple.Conventions, "EUR-DEPOSIT");
    simple.Priority = 1;
    y.Segments.Simple.push_back(simple);

    aoisSegmentType aois;
    set(aois.Type, "Average OIS");
    compositeQuoteType_CompositeQuote_t composite;
    set(composite.RateQuote, "IR_SWAP/RATE/USD/2D/6M/10Y");
    set(composite.SpreadQuote, "BASIS_SWAP/BASIS_SPREAD/3M/1D/USD/10Y");
    aois.Quotes.CompositeQuote.push_back(composite);
    set(aois.Conventions, "USD-AVERAGE-OIS");
    y.Segments.AverageOIS.push_back(aois);

    tenorBasisSegmentType tenor;
    tenor.Type = tenorBasisSegmentTypeType::Tenor_Basis_Swap;
    tenor.Quotes = quotes({"BASIS_SWAP/BASIS_SPREAD/3M/6M/EUR/5Y"});
    set(tenor.Conventions, "EUR-TENOR-BASIS");
    tenorBasisSegmentType_ProjectionCurvePay_t pay;
    set(pay, "EUR-EURIBOR-3M");
    tenor.ProjectionCurvePay = pay;
    y.Segments.TenorBasis.push_back(tenor);

    crossCurrencySegmentType xccy;
    xccy.Type = crossCurrencySegmentTypeType::FX_Forward;
    xccy.Quotes = quotes({"FXFWD/RATE/EUR/USD/1Y"});
    set(xccy.Conventions, "EUR-USD-FX-CONVENTIONS");
    set(xccy.DiscountCurve, "USD-SOFR");
    set(xccy.SpotRate, "FX/RATE/EUR/USD");
    y.Segments.CrossCurrency.push_back(xccy);

    zeroSpreadType spread;
    spread.Type = zeroSpreadSegmentTypeType::Zero_Spread;
    spread.Quotes = quotes({"ZERO/YIELD_SPREAD/EUR/BANK/1Y"});
    set(spread.Conventions, "EUR-ZERO");
    set(spread.ReferenceCurve, "EUR-ESTR");
    y.Segments.ZeroSpread.push_back(spread);

    discountRatioType ratio;
    ratio.Type = discountRatioTypeType::Discount_Ratio;
    set(ratio.BaseCurve, "USD-SOFR");
    ratio.BaseCurve.currency = "USD";
    set(ratio.NumeratorCurve, "EUR-IN-USD");
    ratio.NumeratorCurve.currency = "EUR";
    set(ratio.DenominatorCurve, "EUR-ESTR");
    ratio.DenominatorCurve.currency = "EUR";
    y.Segments.DiscountRatio.push_back(ratio);

    fittedBondType fitted;
    set(fitted.Type, "FittedBond");
    fitted.Quotes = quotes({"BOND/PRICE/EUR/BOND1"});
    fittedBondType_IborIndexCurves_t ibor_curves;
    fittedBondType_IborIndexCurves_t_IborIndexCurve_t ibor_curve;
    set(ibor_curve, "EUR-EURIBOR-6M");
    ibor_curve.iborIndex = std::string("EUR-EURIBOR-6M");
    ibor_curves.IborIndexCurve.push_back(ibor_curve);
    fitted.IborIndexCurves = ibor_curves;
    fitted.ExtrapolateFlat = true;
    y.Segments.FittedBond.push_back(fitted);

    BondYieldShiftedType shifted;
    set(shifted.Type, "Bond Yield Shifted");
    set(shifted.ReferenceCurve, "EUR-ESTR");
    shifted.Quotes = quotes({"BOND/YIELD/EUR/BOND2"});
    set(shifted.Conventions, "EUR-BOND-YIELD");
    y.Segments.BondYieldShifted.push_back(shifted);

    weightedAverageType average;
    set(average.Type, "Weighted Average");
    set(average.ReferenceCurve1, "EUR-ESTR");
    set(average.ReferenceCurve2, "EUR-EURIBOR-6M");
    average.Weight1 = 0.25f;
    average.Weight2 = 0.75f;
    y.Segments.WeightedAverage.push_back(average);

    yieldPlusDefaultType plus;
    set(plus.Type, "Yield Plus Default");
    set(plus.ReferenceCurve, "EUR-ESTR");
    set(plus.DefaultCurves.DefaultCurve, "BANK_SR_EUR");
    plus.Weights.Weight = 1.0f;
    y.Segments.YieldPlusDefault.push_back(plus);

    iborFallbackType fallback;
    set(fallback.Type, "Ibor Fallback");
    fallback.IborIndex = "EUR-EURIBOR-6M";
    set(fallback.RfrCurve, "EUR-ESTR");
    fallback.RfrIndex = std::string("EUR-ESTR");
    fallback.Spread = 0.001f;
    y.Segments.IborFallback.push_back(fallback);

    d.YieldCurves = yieldCurves{};
    d.YieldCurves->YieldCurve.push_back(y);
    d.CDSVolatilities = cdsVolatilities{};
    return d;
}

}

TEST_CASE("every curve configuration document round trips", tags) {
    const auto walk = ores::ore::xml::walk_kind(curve_configuration_kind(), corpus_root());

    std::string failures;
    for (const auto& failure : walk.failures)
        failures += failure + "\n";
    INFO("files: " << walk.files << ", passed: " << walk.passed);
    INFO(failures);

    CHECK(walk.files > 0);
    CHECK(walk.passed == walk.files);
}

TEST_CASE("a yield curve with every segment element round trips", tags) {
    const auto original = every_segment();
    const auto mapped = curve_configuration_mapper::map(original);

    std::set<std::string> types;
    for (const auto& s : mapped.segments)
        types.insert(s.segment_type);
    CHECK(types.size() == 12);
    CHECK(mapped.sections.size() == 2);
    CHECK(mapped.bootstrap_configs.size() == 1);

    const auto rebuilt = curve_configuration_mapper::reverse(mapped);
    curveconfiguration exported;
    load_data(save_data(rebuilt), exported);
    CHECK(ores::ore::xml::parsed_text_difference(original, exported, "synthetic").empty());
}

TEST_CASE("an equity volatility with a solver configuration is refused", tags) {
    curveconfiguration d;
    d.EquityVolatilities = equityVolatilities{};
    equityVolatility v;
    v.OneDimSolverConfig = oneDimSolverConfigType{};
    d.EquityVolatilities->EquityVolatility.push_back(v);
    CHECK_THROWS_AS(curve_configuration_mapper::map(d), std::runtime_error);
}

TEST_CASE("a commodity volatility with an APO future surface is refused", tags) {
    curveconfiguration d;
    d.CommodityVolatilities = commodityVolatilities{};
    commodityVolatility v;
    v.ApoFutureSurface = volatilityApoFutureSurfaceConfig{};
    d.CommodityVolatilities->CommodityVolatility.push_back(v);
    CHECK_THROWS_AS(curve_configuration_mapper::map(d), std::runtime_error);
}

TEST_CASE("an FX volatility with a parametric smile round trips", tags) {
    curveconfiguration original;
    original.FXVolatilities = fxVolatilities{};
    fxVolatility v;
    static_cast<std::string&>(v.CurveId) = "EURUSD";
    parametricSmileConfig smile;
    for (const auto* name : {"alpha", "rho"}) {
        parametricSmileConfigParameter p;
        static_cast<std::string&>(p.Name) = name;
        p.InitialValue = parametricSmileConfigParameter_InitialValue_t{};
        static_cast<std::string&>(*p.InitialValue) = "0.1";
        p.Calibration = parametricVolatilityParameterCalibration::Calibrated;
        smile.Parameters.Parameter.push_back(p);
    }
    smile.Calibration.MaxCalibrationAttempts = 3;
    smile.Calibration.ExitEarlyErrorThreshold = 0.0005f;
    smile.Calibration.MaxAcceptableError = 0.01f;
    v.ParametricSmileConfiguration = smile;
    original.FXVolatilities->FXVolatility.push_back(v);

    const auto mapped = curve_configuration_mapper::map(original);
    REQUIRE(mapped.parametric_smiles.size() == 1);
    CHECK(mapped.parametric_smile_parameters.size() == 2);

    curveconfiguration exported;
    load_data(save_data(curve_configuration_mapper::reverse(mapped)), exported);
    CHECK(ores::ore::xml::parsed_text_difference(original, exported, "synthetic").empty());
}

TEST_CASE("a CDS volatility with a constant volatility is refused", tags) {
    curveconfiguration d;
    d.CDSVolatilities = cdsVolatilities{};
    cdsVolatility v;
    v.Constant = constantVolatilityConfig{};
    d.CDSVolatilities->CDSVolatility.push_back(v);
    CHECK_THROWS_AS(curve_configuration_mapper::map(d), std::runtime_error);
}

TEST_CASE("a CDS volatility with price information is refused", tags) {
    curveconfiguration d;
    d.CDSVolatilities = cdsVolatilities{};
    cdsVolatility v;
    v.PriceInfo = priceInfoType{};
    d.CDSVolatilities->CDSVolatility.push_back(v);
    CHECK_THROWS_AS(curve_configuration_mapper::map(d), std::runtime_error);
}

TEST_CASE("a base correlation with a recovery grid is refused", tags) {
    curveconfiguration d;
    d.BaseCorrelations = baseCorrelations{};
    baseCorrelation v;
    v.RecoveryGrid = baseCorrelation_RecoveryGrid_t{};
    d.BaseCorrelations->BaseCorrelation.push_back(v);
    CHECK_THROWS_AS(curve_configuration_mapper::map(d), std::runtime_error);
}

TEST_CASE("a report configuration that writes no curve family is refused", tags) {
    curveconfiguration d;
    d.ReportConfiguration = globalReportConfiguration{};
    CHECK_THROWS_AS(curve_configuration_mapper::map(d), std::runtime_error);
}

TEST_CASE("a segment of an unknown type is refused on export", tags) {
    auto mapped = curve_configuration_mapper::map(every_segment());
    REQUIRE(!mapped.segments.empty());
    mapped.segments.front().segment_type = "Nonexistent";
    CHECK_THROWS_AS(curve_configuration_mapper::reverse(mapped), std::runtime_error);
}

TEST_CASE("a quote on a yield curve entry is refused on export", tags) {
    auto mapped = curve_configuration_mapper::map(every_segment());
    REQUIRE(!mapped.quotes.empty());
    mapped.quotes.front().curve_segment_id = boost::uuids::uuid{};
    CHECK_THROWS_AS(curve_configuration_mapper::reverse(mapped), std::runtime_error);
}

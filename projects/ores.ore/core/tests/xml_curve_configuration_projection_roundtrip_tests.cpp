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
#include "ores.ore.core/domain/curve_configuration_projection.hpp"
#include "ores.ore.core/xml/record_diff.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <string>

/**
 * @file xml_curve_configuration_projection_roundtrip_tests.cpp
 * @brief The curve configuration document, projected and projected back.
 *
 * The walk projects every curve configuration document in the corpus to rows
 * and back, and compares the whole document. The cases below the walk cover
 * what no corpus document writes: every segment element in one document, an FX
 * parametric smile, and the refusal of elements the projection cannot hold.
 */

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][xml][roundtrip][curveconfig]");

using namespace ores::logging;
using namespace ores::ore::domain;

namespace fs = std::filesystem;

fs::path corpus_root() {
    return ores::testing::project_root::resolve("external/ore/examples");
}

std::string read_file(const fs::path& path) {
    std::ifstream file(path, std::ios::binary);
    return std::string(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
}

/// Compares two documents as records, naming the first difference.
std::string difference(const curve_configuration_projection::document& lhs,
                       const curve_configuration_projection::document& rhs,
                       const std::string& path) {
    const auto found = ores::ore::xml::compare_records(
        *ores::ore::xml::generated::curve_configuration::curve_configuration_document().root,
        &lhs,
        &rhs);
    if (!found)
        return {};
    return path + ": " + found->path + ": " + found->detail;
}

/// Reads, projects, projects back and writes one document, naming a failure.
std::string round_trip(const std::string& text, const std::string& path) {
    try {
        const auto original = curve_configuration_projection::read(text, path);
        const auto mapped = curve_configuration_projection::map(original);
        const auto rebuilt = curve_configuration_projection::reverse(mapped);
        const auto exported = curve_configuration_projection::read(
            curve_configuration_projection::write(rebuilt), path);
        return difference(original, exported, path);
    } catch (const std::exception& e) {
        return path + ": " + e.what();
    }
}

curve_configuration_projection::document read_document(const std::string& text) {
    return curve_configuration_projection::read(text);
}

}

TEST_CASE("every_curve_configuration_in_the_corpus_round_trips", tags) {
    auto lg(make_logger(test_suite));

    std::size_t documents = 0;
    std::string failures;
    for (const auto& entry : fs::recursive_directory_iterator(corpus_root())) {
        if (!entry.is_regular_file())
            continue;
        const auto name = entry.path().filename().string();
        if (!name.starts_with("curveconfig") || !name.ends_with(".xml"))
            continue;
        ++documents;
        const auto detail = round_trip(read_file(entry.path()), entry.path().string());
        if (!detail.empty())
            failures += detail + "\n";
    }

    INFO("documents: " << documents);
    INFO(failures);
    CHECK(documents == 89);
    CHECK(failures.empty());
}

TEST_CASE("every_segment_element_round_trips", tags) {
    auto lg(make_logger(test_suite));

    // One yield curve holding one segment of every element the schema allows,
    // including the four no corpus file uses.
    const std::string text = R"(<CurveConfiguration>
  <YieldCurves>
    <YieldCurve>
      <CurveId>EUR-ALL</CurveId>
      <CurveDescription>Every segment element</CurveDescription>
      <Currency>EUR</Currency>
      <DiscountCurve>EUR-ALL</DiscountCurve>
      <Segments>
        <Direct>
          <Type>Zero</Type>
          <Quotes><Quote>ZERO/RATE/EUR/1Y</Quote></Quotes>
        </Direct>
        <Simple>
          <Type>Deposit</Type>
          <Conventions>EUR-DEPOSIT</Conventions>
          <Quotes><Quote>MM/RATE/EUR/1D</Quote></Quotes>
        </Simple>
        <AverageOIS>
          <Type>Average OIS</Type>
          <Conventions>USD-AVERAGE-OIS</Conventions>
          <Quotes>
            <CompositeQuote>
              <RateQuote>IR_SWAP/RATE/USD/2D/6M/10Y</RateQuote>
              <SpreadQuote>BASIS_SWAP/BASIS_SPREAD/3M/1D/USD/10Y</SpreadQuote>
            </CompositeQuote>
          </Quotes>
        </AverageOIS>
        <TenorBasis>
          <Type>Tenor Basis Swap</Type>
          <Conventions>EUR-TENOR-BASIS</Conventions>
          <ProjectionCurvePay>EUR-PAY</ProjectionCurvePay>
          <Quotes><Quote>BASIS_SWAP/BASIS_SPREAD/3M/1D/EUR/10Y</Quote></Quotes>
        </TenorBasis>
        <CrossCurrency>
          <Type>FX Forward</Type>
          <Conventions>EUR-USD-FX</Conventions>
          <DiscountCurve>USD-DISC</DiscountCurve>
          <SpotRate>FX-SPOT</SpotRate>
          <Quotes><Quote>FX/RATE/EUR/USD</Quote></Quotes>
        </CrossCurrency>
        <ZeroSpread>
          <Type>Zero Spread</Type>
          <Conventions>EUR-ZERO-SPREAD</Conventions>
          <ReferenceCurve>EUR-REF</ReferenceCurve>
          <Quotes><Quote>ZERO/RATE/EUR/2Y</Quote></Quotes>
        </ZeroSpread>
        <DiscountRatio>
          <Type>Discount Ratio</Type>
          <BaseCurve currency="EUR">EUR-BASE</BaseCurve>
          <NumeratorCurve currency="EUR">EUR-NUM</NumeratorCurve>
          <DenominatorCurve currency="EUR">EUR-DEN</DenominatorCurve>
        </DiscountRatio>
        <FittedBond>
          <Type>FittedBond</Type>
          <Quotes><Quote>BOND/PRICE/EUR/1Y</Quote></Quotes>
        </FittedBond>
        <BondYieldShifted>
          <Type>Bond Yield Shifted</Type>
          <Conventions>EUR-BOND</Conventions>
          <ReferenceCurve>EUR-REF</ReferenceCurve>
          <Quotes><Quote>BOND/YIELD/EUR/1Y</Quote></Quotes>
        </BondYieldShifted>
        <WeightedAverage>
          <Type>Weighted Average</Type>
          <ReferenceCurve1>EUR-A</ReferenceCurve1>
          <ReferenceCurve2>EUR-B</ReferenceCurve2>
          <Weight1>0.6</Weight1>
          <Weight2>0.4</Weight2>
        </WeightedAverage>
        <YieldPlusDefault>
          <Type>Yield Plus Default</Type>
          <ReferenceCurve>EUR-REF</ReferenceCurve>
          <DefaultCurves><DefaultCurve>EUR-DEFAULT</DefaultCurve></DefaultCurves>
          <Weights><Weight>1.0</Weight></Weights>
        </YieldPlusDefault>
        <IborFallback>
          <Type>Ibor Fallback</Type>
          <IborIndex>CHF-LIBOR-SN</IborIndex>
          <RfrCurve>CHF-SARON</RfrCurve>
          <Spread>-0.00055099999999999995</Spread>
        </IborFallback>
      </Segments>
    </YieldCurve>
  </YieldCurves>
</CurveConfiguration>)";

    CHECK(round_trip(text, "every segment element").empty());
}

TEST_CASE("an_fx_parametric_smile_round_trips", tags) {
    auto lg(make_logger(test_suite));

    const std::string text = R"(<CurveConfiguration>
  <FXVolatilities>
    <FXVolatility>
      <CurveId>EURUSD</CurveId>
      <CurveDescription>Smile</CurveDescription>
      <Dimension>Smile</Dimension>
      <ParametricSmileConfiguration>
        <Calibration>
          <MaxCalibrationAttempts>3</MaxCalibrationAttempts>
          <ExitEarlyErrorThreshold>0.01</ExitEarlyErrorThreshold>
          <MaxAcceptableError>0.001</MaxAcceptableError>
        </Calibration>
        <Parameters>
          <Parameter>
            <Name>alpha</Name>
            <InitialValue>0.1</InitialValue>
            <Calibration>Calibrated</Calibration>
          </Parameter>
        </Parameters>
      </ParametricSmileConfiguration>
    </FXVolatility>
  </FXVolatilities>
</CurveConfiguration>)";

    CHECK(round_trip(text, "fx smile").empty());
}

TEST_CASE("an_element_the_projection_cannot_hold_is_refused", tags) {
    auto lg(make_logger(test_suite));

    const std::string text = R"(<CurveConfiguration>
  <InflationCurves>
    <InflationCurve>
      <CurveId>EUHICP</CurveId>
      <CurveDescription>Segments</CurveDescription>
      <NominalTermStructure>EUR-ALL</NominalTermStructure>
      <Type>ZC</Type>
      <Calendar>TARGET</Calendar>
      <Lag>3M</Lag>
      <Frequency>Annual</Frequency>
      <Segments>
        <Segment>
          <Conventions>EUHICP-CONVENTIONS</Conventions>
          <Quotes><Quote>Q</Quote></Quotes>
        </Segment>
      </Segments>
    </InflationCurve>
  </InflationCurves>
</CurveConfiguration>)";

    const auto parsed = read_document(text);
    CHECK_THROWS_WITH(curve_configuration_projection::map(parsed),
                      Catch::Matchers::ContainsSubstring("writes Segments"));
}

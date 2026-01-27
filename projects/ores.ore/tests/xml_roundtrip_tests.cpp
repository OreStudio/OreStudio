/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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

#include <filesystem>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

namespace {

const std::string_view test_suite("ores.ore.roundtrip.tests");
const std::string tags("[ore][xml][roundtrip]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

using ores::ore::domain::currencyConfig;
using ores::ore::domain::currencyDefinition;
using ores::ore::domain::roundingType;
using namespace ores::logging;

/**
 * @brief Compare two currencyDefinition objects field by field.
 */
void require_currency_equal(const currencyDefinition& original,
                            const currencyDefinition& roundtripped,
                            const std::string& context) {
    INFO("Comparing currency: " << context);

    CHECK(std::string(roundtripped.Name) == std::string(original.Name));
    CHECK(std::string(roundtripped.ISOCode) == std::string(original.ISOCode));
    CHECK(std::string(roundtripped.Symbol) == std::string(original.Symbol));
    CHECK(std::string(roundtripped.FractionSymbol) == std::string(original.FractionSymbol));
    CHECK(roundtripped.FractionsPerUnit == original.FractionsPerUnit);
    CHECK(roundtripped.RoundingType == original.RoundingType);
    CHECK(roundtripped.RoundingPrecision == original.RoundingPrecision);

    // Optional fields
    if (original.MinorUnitCodes) {
        REQUIRE(static_cast<bool>(roundtripped.MinorUnitCodes));
        CHECK(std::string(*roundtripped.MinorUnitCodes) == std::string(*original.MinorUnitCodes));
    } else {
        CHECK_FALSE(static_cast<bool>(roundtripped.MinorUnitCodes));
    }

    if (original.NumericCode) {
        REQUIRE(static_cast<bool>(roundtripped.NumericCode));
        CHECK(*roundtripped.NumericCode == *original.NumericCode);
    } else {
        CHECK_FALSE(static_cast<bool>(roundtripped.NumericCode));
    }

    if (original.CurrencyType) {
        REQUIRE(static_cast<bool>(roundtripped.CurrencyType));
        CHECK(std::string(*roundtripped.CurrencyType) == std::string(*original.CurrencyType));
    } else {
        CHECK_FALSE(static_cast<bool>(roundtripped.CurrencyType));
    }
}

/**
 * @brief Compare two currencyConfig objects.
 */
void require_config_equal(const currencyConfig& original,
                          const currencyConfig& roundtripped) {
    REQUIRE(roundtripped.Currency.size() == original.Currency.size());

    for (std::size_t i = 0; i < original.Currency.size(); ++i) {
        require_currency_equal(original.Currency[i], roundtripped.Currency[i],
                              std::string(original.Currency[i].ISOCode));
    }
}

/**
 * @brief Perform a structural roundtrip test on a currencyConfig.
 *
 * Loads XML, serializes it back, parses again, and compares domain objects.
 */
void test_roundtrip(const std::string& xml_content, const std::string& source_name) {
    auto lg(make_logger(test_suite));

    // Step 1: Parse original XML
    BOOST_LOG_SEV(lg, debug) << "Parsing original XML from: " << source_name;
    currencyConfig original;
    ores::ore::domain::load_data(xml_content, original);
    BOOST_LOG_SEV(lg, debug) << "Parsed " << original.Currency.size() << " currencies";

    // Step 2: Serialize back to XML
    BOOST_LOG_SEV(lg, debug) << "Serializing to XML";
    const std::string serialized = ores::ore::domain::save_data(original);
    BOOST_LOG_SEV(lg, trace) << "Serialized XML:\n" << serialized;

    // Step 3: Parse the serialized XML
    BOOST_LOG_SEV(lg, debug) << "Parsing serialized XML";
    currencyConfig roundtripped;
    ores::ore::domain::load_data(serialized, roundtripped);
    BOOST_LOG_SEV(lg, debug) << "Parsed " << roundtripped.Currency.size()
                             << " currencies from roundtrip";

    // Step 4: Compare domain objects
    BOOST_LOG_SEV(lg, debug) << "Comparing original and roundtripped objects";
    require_config_equal(original, roundtripped);

    BOOST_LOG_SEV(lg, info) << "Roundtrip test passed for: " << source_name;
}

}

// =============================================================================
// Inline XML Roundtrip Tests
// =============================================================================

TEST_CASE("currency_config_roundtrip_simple_xml", tags) {
    const std::string simple_xml = R"(
<CurrencyConfig>
  <Currency>
    <Name>United Arab Emirates dirham</Name>
    <ISOCode>AED</ISOCode>
    <NumericCode>784</NumericCode>
    <Symbol></Symbol>
    <FractionSymbol></FractionSymbol>
    <FractionsPerUnit>100</FractionsPerUnit>
    <RoundingType>Closest</RoundingType>
    <RoundingPrecision>2</RoundingPrecision>
    <CurrencyType>Major</CurrencyType>
  </Currency>
</CurrencyConfig>
)";

    test_roundtrip(simple_xml, "simple_xml");
}

TEST_CASE("currency_config_roundtrip_multiple_currencies", tags) {
    const std::string xml = R"(
<CurrencyConfig>
  <Currency>
    <Name>US Dollar</Name>
    <ISOCode>USD</ISOCode>
    <NumericCode>840</NumericCode>
    <Symbol>$</Symbol>
    <FractionSymbol>c</FractionSymbol>
    <FractionsPerUnit>100</FractionsPerUnit>
    <RoundingType>Closest</RoundingType>
    <RoundingPrecision>2</RoundingPrecision>
    <CurrencyType>Major</CurrencyType>
  </Currency>
  <Currency>
    <Name>Euro</Name>
    <ISOCode>EUR</ISOCode>
    <NumericCode>978</NumericCode>
    <Symbol></Symbol>
    <FractionSymbol></FractionSymbol>
    <FractionsPerUnit>100</FractionsPerUnit>
    <RoundingType>Closest</RoundingType>
    <RoundingPrecision>2</RoundingPrecision>
    <CurrencyType>Major</CurrencyType>
  </Currency>
  <Currency>
    <Name>Bitcoin</Name>
    <ISOCode>BTC</ISOCode>
    <NumericCode>0</NumericCode>
    <Symbol>BTC</Symbol>
    <FractionSymbol>sat</FractionSymbol>
    <FractionsPerUnit>100000000</FractionsPerUnit>
    <RoundingType>Floor</RoundingType>
    <RoundingPrecision>8</RoundingPrecision>
    <CurrencyType>Crypto</CurrencyType>
  </Currency>
</CurrencyConfig>
)";

    test_roundtrip(xml, "multiple_currencies");
}

TEST_CASE("currency_config_roundtrip_optional_fields_missing", tags) {
    // Test with optional fields (NumericCode, CurrencyType) missing
    const std::string xml = R"(
<CurrencyConfig>
  <Currency>
    <Name>Test Currency</Name>
    <ISOCode>TST</ISOCode>
    <Symbol>T</Symbol>
    <FractionSymbol></FractionSymbol>
    <FractionsPerUnit>100</FractionsPerUnit>
    <RoundingType>Up</RoundingType>
    <RoundingPrecision>0</RoundingPrecision>
  </Currency>
</CurrencyConfig>
)";

    test_roundtrip(xml, "optional_fields_missing");
}

TEST_CASE("currency_config_roundtrip_all_rounding_types", tags) {
    // Test all rounding type enum values
    const std::string xml = R"(
<CurrencyConfig>
  <Currency>
    <Name>Rounding Up</Name>
    <ISOCode>RUP</ISOCode>
    <Symbol></Symbol>
    <FractionSymbol></FractionSymbol>
    <FractionsPerUnit>100</FractionsPerUnit>
    <RoundingType>Up</RoundingType>
    <RoundingPrecision>2</RoundingPrecision>
  </Currency>
  <Currency>
    <Name>Rounding Down</Name>
    <ISOCode>RDN</ISOCode>
    <Symbol></Symbol>
    <FractionSymbol></FractionSymbol>
    <FractionsPerUnit>100</FractionsPerUnit>
    <RoundingType>Down</RoundingType>
    <RoundingPrecision>2</RoundingPrecision>
  </Currency>
  <Currency>
    <Name>Rounding Closest</Name>
    <ISOCode>RCL</ISOCode>
    <Symbol></Symbol>
    <FractionSymbol></FractionSymbol>
    <FractionsPerUnit>100</FractionsPerUnit>
    <RoundingType>Closest</RoundingType>
    <RoundingPrecision>2</RoundingPrecision>
  </Currency>
  <Currency>
    <Name>Rounding Floor</Name>
    <ISOCode>RFL</ISOCode>
    <Symbol></Symbol>
    <FractionSymbol></FractionSymbol>
    <FractionsPerUnit>100</FractionsPerUnit>
    <RoundingType>Floor</RoundingType>
    <RoundingPrecision>2</RoundingPrecision>
  </Currency>
  <Currency>
    <Name>Rounding Ceiling</Name>
    <ISOCode>RCE</ISOCode>
    <Symbol></Symbol>
    <FractionSymbol></FractionSymbol>
    <FractionsPerUnit>100</FractionsPerUnit>
    <RoundingType>Ceiling</RoundingType>
    <RoundingPrecision>2</RoundingPrecision>
  </Currency>
</CurrencyConfig>
)";

    test_roundtrip(xml, "all_rounding_types");
}

// =============================================================================
// File-Based Roundtrip Tests
// =============================================================================

TEST_CASE("currency_config_roundtrip_currencies_xml", tags) {
    const auto f = ore_path("examples/Input/currencies.xml");
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    test_roundtrip(content, f.string());
}

TEST_CASE("currency_config_roundtrip_example_1", tags) {
    const auto f = ore_path("examples/Legacy/Example_1/Input/currencies.xml");
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    test_roundtrip(content, f.string());
}

TEST_CASE("currency_config_roundtrip_example_41", tags) {
    const auto f = ore_path("examples/Legacy/Example_41/Input/currencies.xml");
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    test_roundtrip(content, f.string());
}

TEST_CASE("currency_config_roundtrip_example_62", tags) {
    const auto f = ore_path("examples/Legacy/Example_62/Input/currencies.xml");
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    test_roundtrip(content, f.string());
}

TEST_CASE("currency_config_roundtrip_ore_api", tags) {
    const auto f = ore_path("examples/ORE-API/Input/currencies.xml");
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    test_roundtrip(content, f.string());
}

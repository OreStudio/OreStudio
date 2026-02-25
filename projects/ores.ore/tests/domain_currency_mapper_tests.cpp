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
#include "ores.ore/domain/currency_mapper.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][domain][currency_mapper]");

}

using ores::ore::domain::currency_mapper;
using ores::ore::domain::currencyConfig;
using ores::ore::domain::currencyDefinition;
using ores::ore::domain::currencyDefinition_CurrencyType_t;
using ores::ore::domain::roundingType;
using ores::refdata::domain::currency;
using namespace ores::logging;

// =============================================================================
// map(currencyDefinition) -> currency
// =============================================================================

TEST_CASE("map_currency_definition_to_domain_with_all_fields", tags) {
    auto lg(make_logger(test_suite));

    currencyDefinition def;
    static_cast<xsd::string&>(def.ISOCode) = "USD";
    static_cast<xsd::string&>(def.Name) = "US Dollar";
    def.NumericCode = 840;
    static_cast<xsd::string&>(def.Symbol) = "$";
    static_cast<xsd::string&>(def.FractionSymbol) = "c";
    def.FractionsPerUnit = 100;
    def.RoundingType = roundingType::Closest;
    def.RoundingPrecision = 2;
    currencyDefinition_CurrencyType_t ct;
    static_cast<xsd::string&>(ct) = "Major";
    def.CurrencyType = ct;

    const auto result = currency_mapper::map(def);
    BOOST_LOG_SEV(lg, debug) << "Mapped currency: " << result.iso_code;

    CHECK(result.iso_code == "USD");
    CHECK(result.name == "US Dollar");
    CHECK(result.numeric_code == "840");
    CHECK(result.symbol == "$");
    CHECK(result.fraction_symbol == "c");
    CHECK(result.fractions_per_unit == 100);
    CHECK(result.rounding_type == "Closest");
    CHECK(result.rounding_precision == 2);
    CHECK(result.monetary_nature == "Major");
    CHECK(result.market_tier.empty());
    CHECK(result.modified_by == "ores");
    CHECK(result.change_reason_code == "system.external_data_import");
    CHECK(result.change_commentary == "Imported from ORE XML");
}

TEST_CASE("map_currency_definition_without_optional_fields", tags) {
    auto lg(make_logger(test_suite));

    currencyDefinition def;
    static_cast<xsd::string&>(def.ISOCode) = "TST";
    static_cast<xsd::string&>(def.Name) = "Test Currency";
    def.FractionsPerUnit = 100;
    def.RoundingType = roundingType::Up;
    def.RoundingPrecision = 0;
    // NumericCode and CurrencyType left unset

    const auto result = currency_mapper::map(def);
    BOOST_LOG_SEV(lg, debug) << "Mapped currency: " << result.iso_code;

    CHECK(result.iso_code == "TST");
    CHECK(result.name == "Test Currency");
    CHECK(result.numeric_code.empty());
    CHECK(result.monetary_nature.empty());
    CHECK(result.market_tier.empty());
    CHECK(result.rounding_type == "Up");
}

TEST_CASE("map_currency_definition_all_rounding_types", tags) {
    auto lg(make_logger(test_suite));

    struct test_case {
        roundingType input;
        std::string expected;
    };

    const std::vector<test_case> cases = {
        {roundingType::Up, "Up"},
        {roundingType::Down, "Down"},
        {roundingType::Closest, "Closest"},
        {roundingType::Floor, "Floor"},
        {roundingType::Ceiling, "Ceiling"}
    };

    for (const auto& tc : cases) {
        currencyDefinition def;
        static_cast<xsd::string&>(def.ISOCode) = "TST";
        static_cast<xsd::string&>(def.Name) = "Test";
        def.FractionsPerUnit = 100;
        def.RoundingType = tc.input;
        def.RoundingPrecision = 2;

        const auto result = currency_mapper::map(def);
        INFO("Rounding type: " << tc.expected);
        CHECK(result.rounding_type == tc.expected);
    }
}

// =============================================================================
// map(currency) -> currencyDefinition
// =============================================================================

TEST_CASE("map_domain_currency_to_definition_with_all_fields", tags) {
    auto lg(make_logger(test_suite));

    currency c;
    c.iso_code = "GBP";
    c.name = "British Pound";
    c.numeric_code = "826";
    c.symbol = "£";
    c.fraction_symbol = "p";
    c.fractions_per_unit = 100;
    c.rounding_type = "Down";
    c.rounding_precision = 2;
    c.monetary_nature = "Major";

    const auto result = currency_mapper::map(c);
    BOOST_LOG_SEV(lg, debug) << "Mapped definition: " << std::string(result.ISOCode);

    CHECK(std::string(result.ISOCode) == "GBP");
    CHECK(std::string(result.Name) == "British Pound");
    REQUIRE(static_cast<bool>(result.NumericCode));
    CHECK(*result.NumericCode == 826);
    CHECK(std::string(result.Symbol) == "£");
    CHECK(std::string(result.FractionSymbol) == "p");
    CHECK(result.FractionsPerUnit == 100);
    CHECK(result.RoundingType == roundingType::Down);
    CHECK(result.RoundingPrecision == 2);
    REQUIRE(static_cast<bool>(result.CurrencyType));
    CHECK(std::string(*result.CurrencyType) == "Major");
}

TEST_CASE("map_domain_currency_without_optional_fields", tags) {
    auto lg(make_logger(test_suite));

    currency c;
    c.iso_code = "TST";
    c.name = "Test Currency";
    c.fractions_per_unit = 100;
    c.rounding_type = "Closest";
    c.rounding_precision = 2;
    // numeric_code and asset_class left empty

    const auto result = currency_mapper::map(c);
    BOOST_LOG_SEV(lg, debug) << "Mapped definition: " << std::string(result.ISOCode);

    CHECK_FALSE(static_cast<bool>(result.NumericCode));
    CHECK_FALSE(static_cast<bool>(result.CurrencyType));
}

TEST_CASE("map_domain_currency_with_non_numeric_code_ignores_it", tags) {
    auto lg(make_logger(test_suite));

    currency c;
    c.iso_code = "TST";
    c.name = "Test Currency";
    c.numeric_code = "not_a_number";
    c.fractions_per_unit = 100;
    c.rounding_type = "Closest";
    c.rounding_precision = 2;

    const auto result = currency_mapper::map(c);
    BOOST_LOG_SEV(lg, debug) << "Mapped definition: " << std::string(result.ISOCode);

    CHECK_FALSE(static_cast<bool>(result.NumericCode));
}

TEST_CASE("map_domain_currency_with_invalid_rounding_type_throws", tags) {
    auto lg(make_logger(test_suite));

    currency c;
    c.iso_code = "TST";
    c.name = "Test Currency";
    c.fractions_per_unit = 100;
    c.rounding_type = "InvalidType";
    c.rounding_precision = 2;

    CHECK_THROWS_AS(currency_mapper::map(c), std::runtime_error);
}

// =============================================================================
// map(currencyConfig) -> vector<currency>
// =============================================================================

TEST_CASE("map_empty_currency_config_to_empty_vector", tags) {
    auto lg(make_logger(test_suite));

    currencyConfig cfg;
    const auto result = currency_mapper::map(cfg);
    BOOST_LOG_SEV(lg, debug) << "Mapped " << result.size() << " currencies";

    CHECK(result.empty());
}

TEST_CASE("map_currency_config_with_multiple_currencies", tags) {
    auto lg(make_logger(test_suite));

    currencyDefinition usd;
    static_cast<xsd::string&>(usd.ISOCode) = "USD";
    static_cast<xsd::string&>(usd.Name) = "US Dollar";
    usd.FractionsPerUnit = 100;
    usd.RoundingType = roundingType::Closest;
    usd.RoundingPrecision = 2;

    currencyDefinition eur;
    static_cast<xsd::string&>(eur.ISOCode) = "EUR";
    static_cast<xsd::string&>(eur.Name) = "Euro";
    eur.FractionsPerUnit = 100;
    eur.RoundingType = roundingType::Closest;
    eur.RoundingPrecision = 2;

    currencyConfig cfg;
    cfg.Currency.push_back(usd);
    cfg.Currency.push_back(eur);

    const auto result = currency_mapper::map(cfg);
    BOOST_LOG_SEV(lg, debug) << "Mapped " << result.size() << " currencies";

    REQUIRE(result.size() == 2);
    CHECK(result[0].iso_code == "USD");
    CHECK(result[1].iso_code == "EUR");
}

// =============================================================================
// map(vector<currency>) -> currencyConfig
// =============================================================================

TEST_CASE("map_empty_currency_vector_to_empty_config", tags) {
    auto lg(make_logger(test_suite));

    const auto result = currency_mapper::map(std::vector<currency>{});
    BOOST_LOG_SEV(lg, debug) << "Mapped " << result.Currency.size() << " definitions";

    CHECK(result.Currency.empty());
}

TEST_CASE("map_currency_vector_with_multiple_currencies", tags) {
    auto lg(make_logger(test_suite));

    currency usd;
    usd.iso_code = "USD";
    usd.name = "US Dollar";
    usd.fractions_per_unit = 100;
    usd.rounding_type = "Closest";
    usd.rounding_precision = 2;

    currency eur;
    eur.iso_code = "EUR";
    eur.name = "Euro";
    eur.fractions_per_unit = 100;
    eur.rounding_type = "Closest";
    eur.rounding_precision = 2;

    const auto result = currency_mapper::map(std::vector<currency>{usd, eur});
    BOOST_LOG_SEV(lg, debug) << "Mapped " << result.Currency.size() << " definitions";

    REQUIRE(result.Currency.size() == 2);
    CHECK(std::string(result.Currency[0].ISOCode) == "USD");
    CHECK(std::string(result.Currency[1].ISOCode) == "EUR");
}

// =============================================================================
// Bidirectional roundtrip: currency -> definition -> currency
// =============================================================================

TEST_CASE("map_currency_roundtrip_preserves_fields", tags) {
    auto lg(make_logger(test_suite));

    currency original;
    original.iso_code = "JPY";
    original.name = "Japanese Yen";
    original.numeric_code = "392";
    original.symbol = "¥";
    original.fraction_symbol = "";
    original.fractions_per_unit = 1;
    original.rounding_type = "Floor";
    original.rounding_precision = 0;
    original.monetary_nature = "Major";

    const auto def = currency_mapper::map(original);
    const auto roundtripped = currency_mapper::map(def);

    CHECK(roundtripped.iso_code == original.iso_code);
    CHECK(roundtripped.name == original.name);
    CHECK(roundtripped.numeric_code == original.numeric_code);
    CHECK(roundtripped.symbol == original.symbol);
    CHECK(roundtripped.fraction_symbol == original.fraction_symbol);
    CHECK(roundtripped.fractions_per_unit == original.fractions_per_unit);
    CHECK(roundtripped.rounding_type == original.rounding_type);
    CHECK(roundtripped.rounding_precision == original.rounding_precision);
    CHECK(roundtripped.monetary_nature == original.monetary_nature);
}

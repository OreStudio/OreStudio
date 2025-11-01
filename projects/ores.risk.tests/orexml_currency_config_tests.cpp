/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include <filesystem>
#include <catch2/catch_test_macros.hpp>
#include "ores.utility/filesystem/file.hpp"
#include "ores.risk/orexml/CurrencyConfig.hpp"

namespace {

const std::string test_data_dir = "../test_data/currencies/";

}

using ores::risk::orexml::CurrencyConfig;

TEST_CASE("read_currency_config_from_simple_xml", "[orexml_currency_config_tests]") {
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
    <Format></Format>
    <CurrencyType>Major</CurrencyType>
  </Currency>
</CurrencyConfig>
)";

    const auto ccy_cfg = CurrencyConfig::from_xml(simple_xml);

    REQUIRE(!ccy_cfg.Currency.empty());
    REQUIRE(ccy_cfg.Currency.size() == 1);

    const auto& first = ccy_cfg.Currency.front();
    CHECK(first.Name == "United Arab Emirates dirham");
    CHECK(first.ISOCode == "AED");
    CHECK(first.NumericCode == "784");
    CHECK(first.FractionsPerUnit == 100);
    CHECK(first.RoundingType == "Closest");
    CHECK(first.RoundingPrecision == 2);

    REQUIRE(first.CurrencyType);
    CHECK(first.CurrencyType.value() == "Major");
}

TEST_CASE("read_currency_config_from_currencies_xml", "[orexml_currency_config_tests]") {
    const auto f = std::filesystem::path(test_data_dir + "currencies.xml");

    using ores::utility::filesystem::file;
    const std::string content = file::read_content(f);

    const auto ccy_cfg = CurrencyConfig::from_xml(content);
    REQUIRE(!ccy_cfg.Currency.empty());
    REQUIRE(ccy_cfg.Currency.size() == 179);

    const auto& first = ccy_cfg.Currency.front();

    CHECK(first.Name == "United Arab Emirates dirham");
    CHECK(first.ISOCode == "AED");
    CHECK(first.NumericCode == "784");
    CHECK(first.FractionsPerUnit == 100);
    CHECK(first.RoundingType == "Closest");
    CHECK(first.RoundingPrecision == 2);

    REQUIRE(first.CurrencyType);
    CHECK(first.CurrencyType.value() == "Major");

    const auto& last = ccy_cfg.Currency.back();

    CHECK(last.Name == "Bitcoin");
    CHECK(last.ISOCode == "BTC");
    CHECK(last.NumericCode == "000");
    CHECK(last.FractionsPerUnit == 100000000);
    CHECK(last.RoundingType == "Closest");
    CHECK(last.RoundingPrecision == 2);

    REQUIRE(first.CurrencyType);
    CHECK(last.CurrencyType.value() == "Crypto");
}

TEST_CASE("read_currency_config_from_currencies_01_xml", "[orexml_currency_config_tests]") {
    const auto f =
        std::filesystem::path(test_data_dir + "currencies_01.xml");

    using ores::utility::filesystem::file;
    const std::string content = file::read_content(f);

    const auto ccy_cfg = CurrencyConfig::from_xml(content);
    REQUIRE(!ccy_cfg.Currency.empty());
    REQUIRE(ccy_cfg.Currency.size() == 2);

    const auto& first = ccy_cfg.Currency.front();

    CHECK(first.Name == "Papua New Guinean kina");
    CHECK(first.ISOCode == "PGK");
    CHECK(first.NumericCode == "598");
    CHECK(first.Symbol == "K");
    CHECK(first.FractionSymbol == "");
    CHECK(first.FractionsPerUnit == 100);
    CHECK(first.RoundingType == "Closest");
    CHECK(first.RoundingPrecision == 2);
    CHECK(first.Format == "%3% %1$.2f");

    REQUIRE(!first.CurrencyType);
}

TEST_CASE("read_currency_config_from_currencies_41_xml", "[orexml_currency_config_tests]") {
    const auto f =
        std::filesystem::path(test_data_dir + "currencies_41.xml");

    using ores::utility::filesystem::file;
    const std::string content = file::read_content(f);

    const auto ccy_cfg = CurrencyConfig::from_xml(content);
    REQUIRE(!ccy_cfg.Currency.empty());
    REQUIRE(ccy_cfg.Currency.size() == 2);
}

TEST_CASE("read_currency_config_from_currencies_42_xml", "[orexml_currency_config_tests]") {
    const auto f =
        std::filesystem::path(test_data_dir + "currencies_42.xml");

    using ores::utility::filesystem::file;
    const std::string content = file::read_content(f);

    const auto ccy_cfg = CurrencyConfig::from_xml(content);
    REQUIRE(!ccy_cfg.Currency.empty());
    REQUIRE(ccy_cfg.Currency.size() == 2);
}

TEST_CASE("read_currency_config_from_currencies_62_xml", "[orexml_currency_config_tests]") {
    const auto f =
        std::filesystem::path(test_data_dir + "currencies_62.xml");

    using ores::utility::filesystem::file;
    const std::string content = file::read_content(f);

    const auto ccy_cfg = CurrencyConfig::from_xml(content);
    REQUIRE(!ccy_cfg.Currency.empty());
    REQUIRE(ccy_cfg.Currency.size() == 2);
}

TEST_CASE("read_currency_config_from_currencies_API_xml", "[orexml_currency_config_tests]") {
    const auto f =
        std::filesystem::path(test_data_dir + "currencies_API.xml");

    using ores::utility::filesystem::file;
    const std::string content = file::read_content(f);

    const auto ccy_cfg = CurrencyConfig::from_xml(content);
    REQUIRE(!ccy_cfg.Currency.empty());
    REQUIRE(ccy_cfg.Currency.size() == 178);
}

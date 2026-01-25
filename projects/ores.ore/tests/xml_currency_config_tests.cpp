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
#include "ores.ore/domain/CurrencyConfig.hpp"

#include <filesystem>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][xml]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

}

using ores::ore::domain::CurrencyConfig;
using namespace ores::logging;

TEST_CASE("read_currency_config_from_simple_xml", tags) {
    auto lg(make_logger(test_suite));

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

    BOOST_LOG_SEV(lg, debug) << "Parsing simple XML string";
    const auto ccy_cfg = CurrencyConfig::from_xml(simple_xml);

    REQUIRE(!ccy_cfg.Currency.empty());
    REQUIRE(ccy_cfg.Currency.size() == 1);
    BOOST_LOG_SEV(lg, debug) << "Parsed " << ccy_cfg.Currency.size() << " currencies";

    const auto& first = ccy_cfg.Currency.front();
    BOOST_LOG_SEV(lg, debug) << "Currency: " << first.ISOCode << " - " << first.Name;

    CHECK(first.Name == "United Arab Emirates dirham");
    CHECK(first.ISOCode == "AED");
    CHECK(first.NumericCode == "784");
    CHECK(first.FractionsPerUnit == 100);
    CHECK(first.RoundingType == "Closest");
    CHECK(first.RoundingPrecision == 2);

    REQUIRE(first.CurrencyType);
    CHECK(first.CurrencyType.value() == "Major");
}

TEST_CASE("read_currency_config_from_currencies_xml", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path("examples/Input/currencies.xml");
    BOOST_LOG_SEV(lg, debug) << "Reading file: " << f;

    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    BOOST_LOG_SEV(lg, debug) << "Parsing XML content";
    const auto ccy_cfg = CurrencyConfig::from_xml(content);
    REQUIRE(!ccy_cfg.Currency.empty());
    REQUIRE(ccy_cfg.Currency.size() == 179);
    BOOST_LOG_SEV(lg, debug) << "Parsed " << ccy_cfg.Currency.size() << " currencies";

    const auto& first = ccy_cfg.Currency.front();
    BOOST_LOG_SEV(lg, debug) << "First currency: " << first.ISOCode << " - " << first.Name;

    CHECK(first.Name == "United Arab Emirates dirham");
    CHECK(first.ISOCode == "AED");
    CHECK(first.NumericCode == "784");
    CHECK(first.FractionsPerUnit == 100);
    CHECK(first.RoundingType == "Closest");
    CHECK(first.RoundingPrecision == 2);

    REQUIRE(first.CurrencyType);
    CHECK(first.CurrencyType.value() == "Major");

    const auto& last = ccy_cfg.Currency.back();
    BOOST_LOG_SEV(lg, debug) << "Last currency: " << last.ISOCode << " - " << last.Name;

    CHECK(last.Name == "Bitcoin");
    CHECK(last.ISOCode == "BTC");
    CHECK(last.NumericCode == "000");
    CHECK(last.FractionsPerUnit == 100000000);
    CHECK(last.RoundingType == "Closest");
    CHECK(last.RoundingPrecision == 2);

    REQUIRE(last.CurrencyType);
    CHECK(last.CurrencyType.value() == "Crypto");
}

TEST_CASE("read_currency_config_from_example_1", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path("examples/Legacy/Example_1/Input/currencies.xml");
    BOOST_LOG_SEV(lg, debug) << "Reading file: " << f;

    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    BOOST_LOG_SEV(lg, debug) << "Parsing XML content";
    const auto ccy_cfg = CurrencyConfig::from_xml(content);
    REQUIRE(!ccy_cfg.Currency.empty());
    REQUIRE(ccy_cfg.Currency.size() == 2);
    BOOST_LOG_SEV(lg, debug) << "Parsed " << ccy_cfg.Currency.size() << " currencies";

    const auto& first = ccy_cfg.Currency.front();
    BOOST_LOG_SEV(lg, debug) << "Currency: " << first.ISOCode << " - " << first.Name;

    CHECK(first.Name == "Papua New Guinean kina");
    CHECK(first.ISOCode == "PGK");
    CHECK(first.NumericCode == "598");
    CHECK(first.Symbol == "K");
    CHECK(first.FractionSymbol == "");
    CHECK(first.FractionsPerUnit == 100);
    CHECK(first.RoundingType == "Closest");
    CHECK(first.RoundingPrecision == 2);

    REQUIRE(!first.CurrencyType);
}

TEST_CASE("read_currency_config_from_example_41", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path("examples/Legacy/Example_41/Input/currencies.xml");
    BOOST_LOG_SEV(lg, debug) << "Reading file: " << f;

    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    BOOST_LOG_SEV(lg, debug) << "Parsing XML content";
    const auto ccy_cfg = CurrencyConfig::from_xml(content);
    REQUIRE(!ccy_cfg.Currency.empty());
    REQUIRE(ccy_cfg.Currency.size() == 2);
    BOOST_LOG_SEV(lg, debug) << "Parsed " << ccy_cfg.Currency.size() << " currencies";
}

TEST_CASE("read_currency_config_from_example_62", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path("examples/Legacy/Example_62/Input/currencies.xml");
    BOOST_LOG_SEV(lg, debug) << "Reading file: " << f;

    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    BOOST_LOG_SEV(lg, debug) << "Parsing XML content";
    const auto ccy_cfg = CurrencyConfig::from_xml(content);
    REQUIRE(!ccy_cfg.Currency.empty());
    REQUIRE(ccy_cfg.Currency.size() == 2);
    BOOST_LOG_SEV(lg, debug) << "Parsed " << ccy_cfg.Currency.size() << " currencies";
}

TEST_CASE("read_currency_config_from_ore_api", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path("examples/ORE-API/Input/currencies.xml");
    BOOST_LOG_SEV(lg, debug) << "Reading file: " << f;

    using ores::platform::filesystem::file;
    const std::string content = file::read_content(f);

    BOOST_LOG_SEV(lg, debug) << "Parsing XML content";
    const auto ccy_cfg = CurrencyConfig::from_xml(content);
    REQUIRE(!ccy_cfg.Currency.empty());
    REQUIRE(ccy_cfg.Currency.size() == 178);
    BOOST_LOG_SEV(lg, debug) << "Parsed " << ccy_cfg.Currency.size() << " currencies";
}

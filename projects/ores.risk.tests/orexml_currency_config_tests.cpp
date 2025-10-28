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
#include <boost/test/unit_test.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include "ores.utility/filesystem/file.hpp"
#include "ores.utility/test/logging.hpp"
#include "ores.risk/orexml/CurrencyConfig.hpp"

namespace {

const std::string test_module("ores.risk.tests");
const std::string test_suite("orexml_currency_config_tests");

const std::string test_data_dir = "../test_data/currencies/";

}

using ores::risk::orexml::CurrencyConfig;

BOOST_AUTO_TEST_SUITE(orexml_currency_config_tests)

BOOST_AUTO_TEST_CASE(read_currency_config_from_simple_xml) {
    SETUP_TEST_LOG_SOURCE_DEBUG("read_currency_config_from_simple_xml");

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
    BOOST_LOG_SEV(lg, debug) << "Input: " << simple_xml;

    const auto ccy_cfg = CurrencyConfig::from_xml(simple_xml);
    BOOST_LOG_SEV(lg, debug) << "Currencies count: "
                             << ccy_cfg.Currency.size();
    BOOST_LOG_SEV(lg, debug) << "Result: " << ccy_cfg;

    BOOST_REQUIRE(!ccy_cfg.Currency.empty());
    BOOST_REQUIRE(ccy_cfg.Currency.size() == 1);

    const auto& first = ccy_cfg.Currency.front();
    BOOST_CHECK_EQUAL(first.Name, "United Arab Emirates dirham");
    BOOST_CHECK_EQUAL(first.ISOCode, "AED");
    BOOST_CHECK_EQUAL(first.NumericCode, "784");
    BOOST_CHECK_EQUAL(first.FractionsPerUnit, 100);
    BOOST_CHECK_EQUAL(first.RoundingType, "Closest");
    BOOST_CHECK_EQUAL(first.RoundingPrecision, 2);

    BOOST_REQUIRE(first.CurrencyType);
    BOOST_CHECK_EQUAL(first.CurrencyType.value(), "Major");
}

BOOST_AUTO_TEST_CASE(read_currency_config_from_currencies_xml) {
    SETUP_TEST_LOG_SOURCE_DEBUG("read_currency_config_from_currencies_xml");

    const auto f = std::filesystem::path(test_data_dir + "currencies.xml");
    BOOST_LOG_SEV(lg, debug) << "Input file: " << f;

    using ores::utility::filesystem::file;
    const std::string content = file::read_content(f);
    BOOST_LOG_SEV(lg, debug) << "Content: " << content;

    const auto ccy_cfg = CurrencyConfig::from_xml(content);
    BOOST_LOG_SEV(lg, debug) << "Currencies count: " << ccy_cfg.Currency.size();
    BOOST_REQUIRE(!ccy_cfg.Currency.empty());
    BOOST_REQUIRE(ccy_cfg.Currency.size() == 179);

    BOOST_LOG_SEV(lg, debug) << "Result: " << ccy_cfg;

    const auto& first = ccy_cfg.Currency.front();
    BOOST_LOG_SEV(lg, debug) << "First: " << first;

    BOOST_CHECK_EQUAL(first.Name, "United Arab Emirates dirham");
    BOOST_CHECK_EQUAL(first.ISOCode, "AED");
    BOOST_CHECK_EQUAL(first.NumericCode, "784");
    BOOST_CHECK_EQUAL(first.FractionsPerUnit, 100);
    BOOST_CHECK_EQUAL(first.RoundingType, "Closest");
    BOOST_CHECK_EQUAL(first.RoundingPrecision, 2);

    BOOST_REQUIRE(first.CurrencyType);
    BOOST_CHECK_EQUAL(first.CurrencyType.value(), "Major");

    const auto& last = ccy_cfg.Currency.back();
    BOOST_LOG_SEV(lg, debug) << "Last: " << last;

    BOOST_CHECK_EQUAL(last.Name, "Bitcoin");
    BOOST_CHECK_EQUAL(last.ISOCode, "BTC");
    BOOST_CHECK_EQUAL(last.NumericCode, "000");
    BOOST_CHECK_EQUAL(last.FractionsPerUnit, 100000000);
    BOOST_CHECK_EQUAL(last.RoundingType, "Closest");
    BOOST_CHECK_EQUAL(last.RoundingPrecision, 2);

    BOOST_REQUIRE(first.CurrencyType);
    BOOST_CHECK_EQUAL(last.CurrencyType.value(), "Crypto");
}

BOOST_AUTO_TEST_CASE(read_currency_config_from_currencies_01_xml) {
    SETUP_TEST_LOG_SOURCE_DEBUG("read_currency_config_from_currencies_01_xml");

    const auto f =
        std::filesystem::path(test_data_dir + "currencies_01.xml");
    BOOST_LOG_SEV(lg, debug) << "Input file: " << f;

    using ores::utility::filesystem::file;
    const std::string content = file::read_content(f);
    BOOST_LOG_SEV(lg, debug) << "Content: " << content;

    const auto ccy_cfg = CurrencyConfig::from_xml(content);
    BOOST_LOG_SEV(lg, debug) << "Currencies count: " << ccy_cfg.Currency.size();
    BOOST_LOG_SEV(lg, debug) << "Result: " << ccy_cfg;
    BOOST_REQUIRE(!ccy_cfg.Currency.empty());
    BOOST_REQUIRE(ccy_cfg.Currency.size() == 2);

    const auto& first = ccy_cfg.Currency.front();
    BOOST_LOG_SEV(lg, debug) << "First: " << first;

    BOOST_CHECK_EQUAL(first.Name, "Papua New Guinean kina");
    BOOST_CHECK_EQUAL(first.ISOCode, "PGK");
    BOOST_CHECK_EQUAL(first.NumericCode, "598");
    BOOST_CHECK_EQUAL(first.Symbol, "K");
    BOOST_CHECK_EQUAL(first.FractionSymbol, "");
    BOOST_CHECK_EQUAL(first.FractionsPerUnit, 100);
    BOOST_CHECK_EQUAL(first.RoundingType, "Closest");
    BOOST_CHECK_EQUAL(first.RoundingPrecision, 2);
    BOOST_CHECK_EQUAL(first.Format, "%3% %1$.2f");

    BOOST_REQUIRE(!first.CurrencyType);
}

BOOST_AUTO_TEST_CASE(read_currency_config_from_currencies_41_xml) {
    SETUP_TEST_LOG_SOURCE_DEBUG("read_currency_config_from_currencies_41_xml");

    const auto f =
        std::filesystem::path(test_data_dir + "currencies_41.xml");
    BOOST_LOG_SEV(lg, debug) << "Input file: " << f;

    using ores::utility::filesystem::file;
    const std::string content = file::read_content(f);
    BOOST_LOG_SEV(lg, debug) << "Content: " << content;

    const auto ccy_cfg = CurrencyConfig::from_xml(content);
    BOOST_LOG_SEV(lg, debug) << "Currencies count: " << ccy_cfg.Currency.size();
    BOOST_LOG_SEV(lg, debug) << "Result: " << ccy_cfg;
    BOOST_REQUIRE(!ccy_cfg.Currency.empty());
    BOOST_REQUIRE(ccy_cfg.Currency.size() == 2);
}

BOOST_AUTO_TEST_CASE(read_currency_config_from_currencies_42_xml) {
    SETUP_TEST_LOG_SOURCE_DEBUG("read_currency_config_from_currencies_42_xml");

    const auto f =
        std::filesystem::path(test_data_dir + "currencies_42.xml");
    BOOST_LOG_SEV(lg, debug) << "Input file: " << f;

    using ores::utility::filesystem::file;
    const std::string content = file::read_content(f);
    BOOST_LOG_SEV(lg, debug) << "Content: " << content;

    const auto ccy_cfg = CurrencyConfig::from_xml(content);
    BOOST_LOG_SEV(lg, debug) << "Currencies count: " << ccy_cfg.Currency.size();
    BOOST_LOG_SEV(lg, debug) << "Result: " << ccy_cfg;
    BOOST_REQUIRE(!ccy_cfg.Currency.empty());
    BOOST_REQUIRE(ccy_cfg.Currency.size() == 2);
}

BOOST_AUTO_TEST_CASE(read_currency_config_from_currencies_62_xml) {
    SETUP_TEST_LOG_SOURCE_DEBUG("read_currency_config_from_currencies_62_xml");

    const auto f =
        std::filesystem::path(test_data_dir + "currencies_62.xml");
    BOOST_LOG_SEV(lg, debug) << "Input file: " << f;

    using ores::utility::filesystem::file;
    const std::string content = file::read_content(f);
    BOOST_LOG_SEV(lg, debug) << "Content: " << content;

    const auto ccy_cfg = CurrencyConfig::from_xml(content);
    BOOST_LOG_SEV(lg, debug) << "Currencies count: " << ccy_cfg.Currency.size();
    BOOST_LOG_SEV(lg, debug) << "Result: " << ccy_cfg;
    BOOST_REQUIRE(!ccy_cfg.Currency.empty());
    BOOST_REQUIRE(ccy_cfg.Currency.size() == 2);
}

BOOST_AUTO_TEST_CASE(read_currency_config_from_currencies_API_xml) {
    SETUP_TEST_LOG_SOURCE_DEBUG("read_currency_config_from_currencies_API_xml");

    const auto f =
        std::filesystem::path(test_data_dir + "currencies_API.xml");
    BOOST_LOG_SEV(lg, debug) << "Input file: " << f;

    using ores::utility::filesystem::file;
    const std::string content = file::read_content(f);
    BOOST_LOG_SEV(lg, debug) << "Content: " << content;

    const auto ccy_cfg = CurrencyConfig::from_xml(content);
    BOOST_LOG_SEV(lg, debug) << "Currencies count: " << ccy_cfg.Currency.size();
    BOOST_LOG_SEV(lg, debug) << "Result: " << ccy_cfg;
    BOOST_REQUIRE(!ccy_cfg.Currency.empty());
    BOOST_REQUIRE(ccy_cfg.Currency.size() == 178);
}

BOOST_AUTO_TEST_SUITE_END()

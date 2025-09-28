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

}

BOOST_AUTO_TEST_SUITE(orexml_currency_config_tests)

BOOST_AUTO_TEST_CASE(currency_config_from_simple_orexml) {
    SETUP_TEST_LOG_SOURCE_DEBUG("currency_config_from_simple_orexml");

    BOOST_LOG_SEV(lg, info) << "test";
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
    const auto currency_config = ores::risk::orexml::from_xml(simple_xml);

    BOOST_TEST_MESSAGE("Simple XML parsed currencies count: " << currency_config.Currency.size());
    BOOST_CHECK(!currency_config.Currency.empty());
    if (!currency_config.Currency.empty()) {
        const auto& first_currency = currency_config.Currency.front();
        BOOST_CHECK_EQUAL(first_currency.Name, "United Arab Emirates dirham");
        BOOST_CHECK_EQUAL(first_currency.ISOCode, "AED");
        BOOST_CHECK_EQUAL(first_currency.NumericCode, "784");
        BOOST_CHECK_EQUAL(first_currency.FractionsPerUnit, 100);
        BOOST_CHECK_EQUAL(first_currency.RoundingType, "Closest");
        BOOST_CHECK_EQUAL(first_currency.RoundingPrecision, 2);
        BOOST_CHECK_EQUAL(first_currency.CurrencyType, "Major");
    }
}

BOOST_AUTO_TEST_CASE(currency_config_from_full_orexml) {
    SETUP_TEST_LOG_SOURCE_DEBUG("currency_config_from_full_orexml");
    BOOST_TEST_MESSAGE("Now testing with full XML file");

    const auto currencies_file =
        std::filesystem::path("assets/test_data/ore_sample_data/currencies/currencies.xml");
    const std::string xml_content = ores::utility::filesystem::read_file_content(currencies_file);
    const auto currency_config = ores::risk::orexml::from_xml(xml_content);
    BOOST_TEST_MESSAGE("Two currencies XML parsed currencies count: " << currency_config.Currency.size());
    BOOST_CHECK(!currency_config.Currency.empty());
    if (!currency_config.Currency.empty()) {
        const auto& first_currency = currency_config.Currency.front();
        BOOST_CHECK_EQUAL(first_currency.Name, "United Arab Emirates dirham");
        BOOST_CHECK_EQUAL(first_currency.ISOCode, "AED");
        BOOST_CHECK_EQUAL(first_currency.NumericCode, "784");
        BOOST_CHECK_EQUAL(first_currency.FractionsPerUnit, 100);
        BOOST_CHECK_EQUAL(first_currency.RoundingType, "Closest");
        BOOST_CHECK_EQUAL(first_currency.RoundingPrecision, 2);
        BOOST_CHECK_EQUAL(first_currency.CurrencyType, "Major");
    }
}

BOOST_AUTO_TEST_SUITE_END()

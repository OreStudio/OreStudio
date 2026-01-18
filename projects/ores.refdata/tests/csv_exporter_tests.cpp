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
#include "ores.refdata/csv/exporter.hpp"

#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.utility/faker/datetime.hpp"
#include "ores.refdata/domain/currency.hpp"

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[csv]");

using ores::utility::faker::datetime;

}

using ores::refdata::csv::exporter;
using ores::refdata::domain::currency;
using namespace ores::logging;

TEST_CASE("export_empty_currency_list_returns_header_only", tags) {
    auto lg(make_logger(test_suite));

    std::vector<currency> empty_list;
    std::string result = exporter::export_currency_config(empty_list);

    BOOST_LOG_SEV(lg, info) << "CSV output for empty list:\n" << result;

    CHECK(!result.empty());
    CHECK(result.find("iso_code") != std::string::npos);
    CHECK(result.find("name") != std::string::npos);
    CHECK(result.find("numeric_code") != std::string::npos);
    CHECK(result.find("symbol") != std::string::npos);
    CHECK(result.find("fractions_per_unit") != std::string::npos);
    CHECK(result.find("rounding_type") != std::string::npos);
    CHECK(result.find("recorded_by") != std::string::npos);
    CHECK(result.find("recorded_at") != std::string::npos);
}

TEST_CASE("export_single_currency_produces_valid_csv", tags) {
    auto lg(make_logger(test_suite));

    currency ccy;
    ccy.iso_code = "USD";
    ccy.name = "United States Dollar";
    ccy.numeric_code = "840";
    ccy.symbol = "$";
    ccy.fraction_symbol = "cent";
    ccy.fractions_per_unit = 100;
    ccy.rounding_type = "Closest";
    ccy.rounding_precision = 2;
    ccy.format = "%3% %1$.2f";
    ccy.currency_type = "Fiat";
    ccy.recorded_by = "admin";
    ccy.recorded_at = datetime::make_timepoint(2025, 1, 15);

    std::vector<currency> currencies = {ccy};
    std::string result = exporter::export_currency_config(currencies);

    BOOST_LOG_SEV(lg, info) << "CSV output:\n" << result;

    CHECK(result.find("USD") != std::string::npos);
    CHECK(result.find("United States Dollar") != std::string::npos);
    CHECK(result.find("840") != std::string::npos);
    CHECK(result.find("$") != std::string::npos);
    CHECK(result.find("100") != std::string::npos);
    CHECK(result.find("Closest") != std::string::npos);
    CHECK(result.find("admin") != std::string::npos);
}

TEST_CASE("export_multiple_currencies_produces_multiple_rows", tags) {
    auto lg(make_logger(test_suite));

    std::vector<currency> currencies;

    currency usd;
    usd.iso_code = "USD";
    usd.name = "United States Dollar";
    usd.numeric_code = "840";
    usd.symbol = "$";
    usd.fraction_symbol = "cent";
    usd.fractions_per_unit = 100;
    usd.rounding_type = "Closest";
    usd.rounding_precision = 2;
    usd.format = "%3% %1$.2f";
    usd.currency_type = "Fiat";
    usd.recorded_by = "admin";
    usd.recorded_at = datetime::make_timepoint(2025, 1, 15);
    currencies.push_back(usd);

    currency eur;
    eur.iso_code = "EUR";
    eur.name = "Euro";
    eur.numeric_code = "978";
    eur.symbol = "€";
    eur.fraction_symbol = "cent";
    eur.fractions_per_unit = 100;
    eur.rounding_type = "Closest";
    eur.rounding_precision = 2;
    eur.format = "%3% %1$.2f";
    eur.currency_type = "Fiat";
    eur.recorded_by = "admin";
    eur.recorded_at = datetime::make_timepoint(2025, 1, 15);
    currencies.push_back(eur);

    currency jpy;
    jpy.iso_code = "JPY";
    jpy.name = "Japanese Yen";
    jpy.numeric_code = "392";
    jpy.symbol = "¥";
    jpy.fraction_symbol = "";
    jpy.fractions_per_unit = 1;
    jpy.rounding_type = "Closest";
    jpy.rounding_precision = 0;
    jpy.format = "%3% %1$.0f";
    jpy.currency_type = "Fiat";
    jpy.recorded_by = "admin";
    jpy.recorded_at = datetime::make_timepoint(2025, 1, 15);
    currencies.push_back(jpy);

    std::string result = exporter::export_currency_config(currencies);

    BOOST_LOG_SEV(lg, info) << "CSV output:\n" << result;

    CHECK(result.find("USD") != std::string::npos);
    CHECK(result.find("EUR") != std::string::npos);
    CHECK(result.find("JPY") != std::string::npos);
    CHECK(result.find("United States Dollar") != std::string::npos);
    CHECK(result.find("Euro") != std::string::npos);
    CHECK(result.find("Japanese Yen") != std::string::npos);
}

TEST_CASE("export_currency_with_comma_in_name_is_escaped", tags) {
    auto lg(make_logger(test_suite));

    currency ccy;
    ccy.iso_code = "TST";
    ccy.name = "Test, Currency";
    ccy.numeric_code = "999";
    ccy.symbol = "T";
    ccy.fraction_symbol = "";
    ccy.fractions_per_unit = 100;
    ccy.rounding_type = "Closest";
    ccy.rounding_precision = 2;
    ccy.format = "%3% %1$.2f";
    ccy.currency_type = "Test";
    ccy.recorded_by = "admin";
    ccy.recorded_at = datetime::make_timepoint(2025, 1, 15);

    std::vector<currency> currencies = {ccy};
    std::string result = exporter::export_currency_config(currencies);

    BOOST_LOG_SEV(lg, info) << "CSV output with comma:\n" << result;

    // Per RFC 4180, fields containing commas should be quoted
    CHECK(result.find("\"Test, Currency\"") != std::string::npos);
}

TEST_CASE("export_currency_with_quotes_in_name_is_escaped", tags) {
    auto lg(make_logger(test_suite));

    currency ccy;
    ccy.iso_code = "TST";
    ccy.name = "Test \"Quoted\" Currency";
    ccy.numeric_code = "999";
    ccy.symbol = "T";
    ccy.fraction_symbol = "";
    ccy.fractions_per_unit = 100;
    ccy.rounding_type = "Closest";
    ccy.rounding_precision = 2;
    ccy.format = "%3% %1$.2f";
    ccy.currency_type = "Test";
    ccy.recorded_by = "admin";
    ccy.recorded_at = datetime::make_timepoint(2025, 1, 15);

    std::vector<currency> currencies = {ccy};
    std::string result = exporter::export_currency_config(currencies);

    BOOST_LOG_SEV(lg, info) << "CSV output with quotes:\n" << result;

    // Per RFC 4180, quotes are escaped by doubling them and the field is quoted
    CHECK(result.find("\"Test \"\"Quoted\"\" Currency\"") != std::string::npos);
}

TEST_CASE("export_currency_with_newline_in_description_is_escaped", tags) {
    auto lg(make_logger(test_suite));

    currency ccy;
    ccy.iso_code = "TST";
    ccy.name = "Test\nCurrency";
    ccy.numeric_code = "999";
    ccy.symbol = "T";
    ccy.fraction_symbol = "";
    ccy.fractions_per_unit = 100;
    ccy.rounding_type = "Closest";
    ccy.rounding_precision = 2;
    ccy.format = "%3% %1$.2f";
    ccy.currency_type = "Test";
    ccy.recorded_by = "admin";
    ccy.recorded_at = datetime::make_timepoint(2025, 1, 15);

    std::vector<currency> currencies = {ccy};
    std::string result = exporter::export_currency_config(currencies);

    BOOST_LOG_SEV(lg, info) << "CSV output with newline:\n" << result;

    // Per RFC 4180, fields containing newlines should be quoted
    CHECK(result.find("\"Test\nCurrency\"") != std::string::npos);
}

TEST_CASE("export_currency_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<currency> currencies;

    for (int i = 0; i < 5; ++i) {
        currency ccy;
        auto fakerCcy = faker::finance::currency();

        ccy.iso_code = fakerCcy.code;
        ccy.name = fakerCcy.name;
        ccy.numeric_code = std::to_string(faker::number::integer(1, 999));
        ccy.symbol = fakerCcy.symbol;
        ccy.fraction_symbol = "";
        ccy.fractions_per_unit = faker::number::integer(1, 10000);
        ccy.rounding_type = "Closest";
        ccy.rounding_precision = faker::number::integer(0, 5);
        ccy.format = "%3% %1$.2f";
        ccy.currency_type = "Fiat";
        ccy.recorded_by = std::string(faker::internet::username());
        ccy.recorded_at = std::chrono::system_clock::now();
        currencies.push_back(ccy);
    }

    std::string result = exporter::export_currency_config(currencies);

    BOOST_LOG_SEV(lg, info) << "CSV output with faker data:\n"
                            << result.substr(0, std::min<size_t>(result.size(), 500));

    CHECK(!result.empty());
    for (const auto& ccy : currencies) {
        CHECK(result.find(ccy.iso_code) != std::string::npos);
    }
}

TEST_CASE("csv_header_contains_all_expected_columns", tags) {
    auto lg(make_logger(test_suite));

    std::vector<currency> empty_list;
    std::string result = exporter::export_currency_config(empty_list);

    BOOST_LOG_SEV(lg, info) << "Checking CSV header columns";

    // Find the header line (first line)
    auto newline_pos = result.find('\n');
    std::string header = result.substr(0, newline_pos);

    CHECK(header.find("iso_code") != std::string::npos);
    CHECK(header.find("name") != std::string::npos);
    CHECK(header.find("numeric_code") != std::string::npos);
    CHECK(header.find("symbol") != std::string::npos);
    CHECK(header.find("fraction_symbol") != std::string::npos);
    CHECK(header.find("fractions_per_unit") != std::string::npos);
    CHECK(header.find("rounding_type") != std::string::npos);
    CHECK(header.find("rounding_precision") != std::string::npos);
    CHECK(header.find("format") != std::string::npos);
    CHECK(header.find("currency_type") != std::string::npos);
    CHECK(header.find("recorded_by") != std::string::npos);
    CHECK(header.find("recorded_at") != std::string::npos);
}

TEST_CASE("csv_output_ends_with_newline", tags) {
    auto lg(make_logger(test_suite));

    currency ccy;
    ccy.iso_code = "USD";
    ccy.name = "United States Dollar";
    ccy.numeric_code = "840";
    ccy.symbol = "$";
    ccy.fraction_symbol = "cent";
    ccy.fractions_per_unit = 100;
    ccy.rounding_type = "Closest";
    ccy.rounding_precision = 2;
    ccy.format = "%3% %1$.2f";
    ccy.currency_type = "Fiat";
    ccy.recorded_by = "admin";
    ccy.recorded_at = datetime::make_timepoint(2025, 1, 15);

    std::vector<currency> currencies = {ccy};
    std::string result = exporter::export_currency_config(currencies);

    BOOST_LOG_SEV(lg, info) << "Checking CSV ends with newline";

    CHECK(!result.empty());
    CHECK(result.back() == '\n');
}

TEST_CASE("export_currency_with_empty_fields", tags) {
    auto lg(make_logger(test_suite));

    currency ccy;
    ccy.iso_code = "TST";
    ccy.name = "";
    ccy.numeric_code = "";
    ccy.symbol = "";
    ccy.fraction_symbol = "";
    ccy.fractions_per_unit = 0;
    ccy.rounding_type = "";
    ccy.rounding_precision = 0;
    ccy.format = "";
    ccy.currency_type = "";
    ccy.recorded_by = "";
    ccy.recorded_at = datetime::make_timepoint(2025, 1, 15);

    std::vector<currency> currencies = {ccy};
    std::string result = exporter::export_currency_config(currencies);

    BOOST_LOG_SEV(lg, info) << "CSV output with empty fields:\n" << result;

    CHECK(!result.empty());
    CHECK(result.find("TST") != std::string::npos);
}

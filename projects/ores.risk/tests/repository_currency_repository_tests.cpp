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
#include "ores.risk/repository/currency_repository.hpp"

#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.utility/log/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.risk/domain/currency.hpp"
#include "ores.risk/generators/currency_generator.hpp"

namespace {

const std::string test_suite("ores.risk.tests");
const std::string database_table("oresdb.currencies");
const std::string tags("[repository]");

}

using namespace ores::risk::generators;
using ores::risk::domain::currency;
using ores::risk::repository::currency_repository;
using ores::testing::database_helper;
using namespace ores::utility::log;

TEST_CASE("write_single_currency", tags) {
    auto lg(make_logger(test_suite));

    ores::testing::database_helper h;
    h.truncate_table(database_table);

    auto currency = generate_synthetic_currency();
    BOOST_LOG_SEV(lg, debug) << "Currency: " << currency;

    currency_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), currency));
}

TEST_CASE("write_multiple_currencies", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    auto currencies = generate_unique_synthetic_currencies(3);
    BOOST_LOG_SEV(lg, debug) << "Currencies: " << currencies;

    currency_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), currencies));
}

TEST_CASE("read_latest_currencies", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    auto written_currencies = generate_unique_synthetic_currencies(3);
    BOOST_LOG_SEV(lg, debug) << "Written currencies: " << written_currencies;

    currency_repository repo;
    repo.write(h.context(), written_currencies);

    auto read_currencies = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read currencies: " << read_currencies;

    CHECK(!read_currencies.empty());
    CHECK(read_currencies.size() == written_currencies.size());
}

TEST_CASE("read_latest_currency_by_iso_code", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    auto currency = generate_synthetic_currency();
    const auto original_name = currency.name;
    BOOST_LOG_SEV(lg, debug) << "Currency: " << currency;

    currency_repository repo;
    repo.write(h.context(), currency);

    currency.name = original_name + " v2";
    repo.write(h.context(), currency);

    auto read_currencies = repo.read_latest(h.context(), currency.iso_code);
    BOOST_LOG_SEV(lg, debug) << "Read currencies: " << read_currencies;

    REQUIRE(read_currencies.size() == 1);
    CHECK(read_currencies[0].iso_code == currency.iso_code);
    CHECK(read_currencies[0].name == original_name + " v2");
}

TEST_CASE("read_all_currencies", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    auto written_currencies = generate_unique_synthetic_currencies(3);
    BOOST_LOG_SEV(lg, debug) << "Written currencies: " << written_currencies;

    currency_repository repo;
    repo.write(h.context(), written_currencies);

    auto read_currencies = repo.read_all(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read currencies: " << read_currencies;

    CHECK(!read_currencies.empty());
    CHECK(read_currencies.size() >= written_currencies.size());
}

TEST_CASE("read_all_currencies_multiple_versions", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    currency_repository repo;

    // Write multiple versions of the same currency
    auto ccy1 = generate_synthetic_currency();
    const auto test_iso_code = ccy1.iso_code;
    const auto test_name = ccy1.name;

    ccy1.name = test_name + " v1";
    BOOST_LOG_SEV(lg, debug) << "Currency 1: " << ccy1;

    auto ccy2 = ccy1;
    ccy2.name = test_name + " v2";
    ccy2.modified_by = "unit test";
    BOOST_LOG_SEV(lg, debug) << "Currency 2: " << ccy2;

    repo.write(h.context(), {ccy1});
    repo.write(h.context(), {ccy2});

    // Read all versions
    auto read_currencies = repo.read_all(h.context(), test_iso_code);
    BOOST_LOG_SEV(lg, debug) << "Read currencies: " << read_currencies;

    CHECK(read_currencies.size() == 2);

    // Verify different versions exist
    bool found_v1 = false, found_v2 = false;
    for (const auto& ccy : read_currencies) {
        if (ccy.iso_code == test_iso_code && ccy.name == test_name + " v1")
            found_v1 = true;
        if (ccy.iso_code == test_iso_code && ccy.name == test_name + " v2")
            found_v2 = true;
    }

    CHECK(found_v1);
    CHECK(found_v2);
}

TEST_CASE("read_nonexistent_iso_code", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    currency_repository repo;

    const std::string nonexistent_iso = "XXX";
    BOOST_LOG_SEV(lg, debug) << "Non-existent ISO code: " << nonexistent_iso;

    auto read_currencies = repo.read_latest(h.context(), nonexistent_iso);
    BOOST_LOG_SEV(lg, debug) << "Read currencies: " << read_currencies;

    CHECK(read_currencies.size() == 0);
}

TEST_CASE("write_and_read_currency_with_unicode_symbols", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    auto currencies = generate_synthetic_unicode_currencies();
    BOOST_LOG_SEV(lg, debug) << "Currencies: " << currencies;

    currency_repository repo;
    repo.write(h.context(), currencies);

    std::vector<std::pair<std::string, std::string>> currency_data = {
        {"EUR", "€"},
        {"GBP", "£"},
        {"JPY", "¥"},
        {"INR", "₹"},
        {"BTC", "₿"},
        {"RUB", "₽"}
    };
    BOOST_LOG_SEV(lg, debug) << "Currency data: " << currency_data;

    // Read back and verify symbols
    auto read_currencies = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read currencies: " << read_currencies;

    CHECK(read_currencies.size() == currencies.size());

    for (const auto& [iso, expected_symbol] : currency_data) {
        BOOST_LOG_SEV(lg, debug) << "Checking: " << iso
                                 << " = " << expected_symbol;
        auto it = std::ranges::find_if(read_currencies,
            [&iso](const currency& c) { return c.iso_code == iso; });

        REQUIRE(it != read_currencies.end());
        CHECK(it->symbol == expected_symbol);
        BOOST_LOG_SEV(lg, debug) << "Verified: " << iso << " = " << it->symbol;
    }
}

TEST_CASE("write_and_read_currency_with_no_fractions", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    h.truncate_table(database_table);

    const auto currencies = generate_synthetic_unicode_currencies();
    const auto& jpy = *
        std::ranges::find_if(currencies, [](const auto& c) {
            return c.iso_code == "JPY";
    });

    BOOST_LOG_SEV(lg, debug) << "Currency with no fractions: " << jpy;

    currency_repository repo;
    repo.write(h.context(), {jpy});

    // Read back and verify
    auto read_currencies = repo.read_latest(h.context(), "JPY");
    BOOST_LOG_SEV(lg, debug) << "Read currencies: " << read_currencies;

    REQUIRE(read_currencies.size() == 1);
    CHECK(read_currencies[0].iso_code == "JPY");
    CHECK(read_currencies[0].fractions_per_unit == 0);
    CHECK(read_currencies[0].rounding_precision == 0);
}

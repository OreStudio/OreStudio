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
#include "ores.cli/config/add_options.hpp"
#include "ores.cli/config/add_currency_options.hpp"
#include "ores.cli/config/add_account_options.hpp"
#include "ores.cli/config/add_feature_flag_options.hpp"
#include "ores.cli/config/add_login_info_options.hpp"

#include <sstream>
#include <variant>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string test_suite("ores.cli.tests");
const std::string tags("[add_options]");

}

using namespace ores::cli::config;
using namespace ores::logging;

TEST_CASE("add_currency_options_required_fields", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing add_currency_options required fields";

    add_currency_options sut;
    sut.iso_code = "USD";
    sut.name = "US Dollar";
    sut.modified_by = "admin";

    CHECK(sut.iso_code == "USD");
    CHECK(sut.name == "US Dollar");
    CHECK(sut.modified_by == "admin");
}

TEST_CASE("add_currency_options_optional_fields", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing add_currency_options optional fields";

    add_currency_options sut;
    sut.iso_code = "EUR";
    sut.name = "Euro";
    sut.modified_by = "admin";
    sut.numeric_code = "978";
    sut.symbol = "€";
    sut.fractions_per_unit = 100;
    sut.currency_type = "fiat";

    REQUIRE(sut.numeric_code.has_value());
    CHECK(*sut.numeric_code == "978");

    REQUIRE(sut.symbol.has_value());
    CHECK(*sut.symbol == "€");

    REQUIRE(sut.fractions_per_unit.has_value());
    CHECK(*sut.fractions_per_unit == 100);

    REQUIRE(sut.currency_type.has_value());
    CHECK(*sut.currency_type == "fiat");
}

TEST_CASE("add_currency_options_streaming", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing add_currency_options streaming";

    add_currency_options sut;
    sut.iso_code = "GBP";
    sut.name = "British Pound";
    sut.modified_by = "system";

    std::ostringstream os;
    os << sut;
    const std::string output = os.str();

    BOOST_LOG_SEV(lg, debug) << "Output: " << output;

    CHECK(!output.empty());
}

TEST_CASE("add_account_options_required_fields", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing add_account_options required fields";

    add_account_options sut;
    sut.username = "johndoe";
    sut.email = "john@example.com";
    sut.password = "secure123";
    sut.modified_by = "admin";

    CHECK(sut.username == "johndoe");
    CHECK(sut.email == "john@example.com");
    CHECK(sut.password == "secure123");
    CHECK(sut.modified_by == "admin");
}

TEST_CASE("add_account_options_admin_flag", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing add_account_options admin flag";

    add_account_options sut;
    sut.username = "admin_user";
    sut.email = "admin@example.com";
    sut.password = "admin123";
    sut.modified_by = "system";
    sut.admin = true;

    REQUIRE(sut.admin.has_value());
    CHECK(*sut.admin);
}

TEST_CASE("add_account_options_non_admin", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing add_account_options non-admin flag";

    add_account_options sut;
    sut.username = "regular_user";
    sut.email = "user@example.com";
    sut.password = "user123";
    sut.modified_by = "admin";
    sut.admin = false;

    REQUIRE(sut.admin.has_value());
    CHECK_FALSE(*sut.admin);
}

TEST_CASE("add_account_options_streaming", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing add_account_options streaming";

    add_account_options sut;
    sut.username = "testuser";
    sut.email = "test@example.com";
    sut.password = "test123";
    sut.modified_by = "cli";

    std::ostringstream os;
    os << sut;
    const std::string output = os.str();

    BOOST_LOG_SEV(lg, debug) << "Output: " << output;

    CHECK(!output.empty());
}

TEST_CASE("add_options_variant_currency", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing add_options variant with currency";

    add_currency_options currency;
    currency.iso_code = "JPY";
    currency.name = "Japanese Yen";
    currency.modified_by = "admin";

    add_options sut = currency;

    CHECK(std::holds_alternative<add_currency_options>(sut));
    CHECK_FALSE(std::holds_alternative<add_account_options>(sut));
}

TEST_CASE("add_options_variant_account", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing add_options variant with account";

    add_account_options account;
    account.username = "newuser";
    account.email = "new@example.com";
    account.password = "pass123";
    account.modified_by = "admin";

    add_options sut = account;

    CHECK(std::holds_alternative<add_account_options>(sut));
    CHECK_FALSE(std::holds_alternative<add_currency_options>(sut));
}

TEST_CASE("add_options_variant_streaming", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing add_options variant streaming";

    add_currency_options currency;
    currency.iso_code = "CHF";
    currency.name = "Swiss Franc";
    currency.modified_by = "admin";

    add_options sut = currency;

    std::ostringstream os;
    os << sut;
    const std::string output = os.str();

    BOOST_LOG_SEV(lg, debug) << "Output: " << output;

    CHECK(!output.empty());
}

TEST_CASE("add_options_visit_pattern", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing add_options std::visit pattern";

    add_currency_options currency;
    currency.iso_code = "AUD";
    currency.name = "Australian Dollar";
    currency.modified_by = "admin";

    add_options sut = currency;

    std::string entity_type;
    std::visit([&entity_type](auto&& opts) {
        using T = std::decay_t<decltype(opts)>;
        if constexpr (std::is_same_v<T, add_currency_options>) {
            entity_type = "currency";
        } else if constexpr (std::is_same_v<T, add_account_options>) {
            entity_type = "account";
        } else if constexpr (std::is_same_v<T, add_feature_flag_options>) {
            entity_type = "feature_flag";
        } else if constexpr (std::is_same_v<T, add_login_info_options>) {
            entity_type = "login_info";
        }
    }, sut);

    CHECK(entity_type == "currency");
}

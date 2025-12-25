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
#include "ores.risk/messaging/protocol.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.telemetry/log/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.utility/datetime/datetime.hpp"
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.risk/generators/currency_generator.hpp"

namespace {

const std::string_view test_suite("ores.risk.tests");
const std::string tags("[messaging]");

}

using namespace ores::telemetry::log;
using namespace ores::risk::messaging;
using ores::risk::domain::currency;
using namespace ores::risk::generators;

TEST_CASE("get_currencies_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_currencies_request original;
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_currencies_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& deserialized = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << deserialized;

    // Request has no fields, so just verify it deserialized successfully
    CHECK(result.has_value());
}

TEST_CASE("get_currencies_response_empty", tags) {
    using namespace ores::telemetry::log;
    auto lg(make_logger(test_suite));

    get_currencies_response resp;
    BOOST_LOG_SEV(lg, debug) << "Empty response: " << resp;

    CHECK(resp.currencies.empty());
}

TEST_CASE("get_currencies_response_with_single_currency", tags) {
    using namespace ores::telemetry::log;
    auto lg(make_logger(test_suite));

    get_currencies_response resp;

    auto ccy = *generate_synthetic_currencies(1).begin();
    resp.currencies.push_back(ccy);
    BOOST_LOG_SEV(lg, debug) << "Response with 1 currency: " << resp;

    CHECK(resp.currencies.size() == 1);
    CHECK(resp.currencies[0].iso_code == ccy.iso_code);
}

TEST_CASE("get_currencies_response_serialize_deserialize", tags) {
    using namespace ores::telemetry::log;
    auto lg(make_logger(test_suite));

    get_currencies_response original;

    auto currencies = generate_synthetic_currencies(5);
    BOOST_LOG_SEV(lg, debug) << "Currencies: " << currencies;
    original.currencies = currencies;

    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.currencies.size()
                             << " currencies";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: "
                             << serialized.size()
                             << " bytes";

    const auto result = get_currencies_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();

    BOOST_LOG_SEV(lg, debug) << "Deserialised response with "
                            << des.currencies.size() << " currencies";

    REQUIRE(des.currencies.size() == original.currencies.size());

    for (size_t i = 0; i < original.currencies.size(); ++i) {
        BOOST_LOG_SEV(lg, debug) << "Comparing currency " << i << ": "
                                << original.currencies[i].iso_code;

        CHECK(des.currencies[i].iso_code == original.currencies[i].iso_code);
        CHECK(des.currencies[i].name == original.currencies[i].name);
        CHECK(des.currencies[i].numeric_code == original.currencies[i].numeric_code);
        CHECK(des.currencies[i].symbol == original.currencies[i].symbol);
        CHECK(des.currencies[i].fractions_per_unit == original.currencies[i].fractions_per_unit);
        CHECK(des.currencies[i].rounding_precision == original.currencies[i].rounding_precision);
    }
}

TEST_CASE("get_currencies_response_large_dataset", tags) {
    using namespace ores::telemetry::log;
    auto lg(make_logger(test_suite));

    get_currencies_response original;
    auto currencies = generate_unique_synthetic_currencies(50);
    BOOST_LOG_SEV(lg, debug) << "Currencies: " << currencies;
    original.currencies = currencies;

    BOOST_LOG_SEV(lg, debug) << "Created response with " << original.currencies.size()
                            << " currencies";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size() << " bytes";

    const auto result = get_currencies_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& deserialized = result.value();

    BOOST_LOG_SEV(lg, debug) << "Successfully deserialized "
                            << deserialized.currencies.size() << " currencies";

    CHECK(deserialized.currencies.size() == currencies.size());
}

TEST_CASE("get_currencies_response_with_special_characters", tags) {
    using namespace ores::telemetry::log;
    auto lg(make_logger(test_suite));

    get_currencies_response resp;
    resp.currencies = generate_synthetic_unicode_currencies();
    BOOST_LOG_SEV(lg, debug) << "Response with special character symbols";

    const auto serialized = resp.serialize();
    const auto result = get_currencies_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& deserialized = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized: " << deserialized;


    for (size_t i = 0; i < resp.currencies.size(); ++i) {
        BOOST_LOG_SEV(lg, debug) << "Currency: " << resp.currencies[i].iso_code
                                << " = " << resp.currencies[i].symbol;

        CHECK(deserialized.currencies[i].iso_code == resp.currencies[i].iso_code);
        CHECK(deserialized.currencies[i].symbol == resp.currencies[i].symbol);
    }
}

TEST_CASE("get_currencies_response_with_empty_fields", tags) {
    using namespace ores::telemetry::log;
    auto lg(make_logger(test_suite));

    get_currencies_response resp;

    currency ccy;
    ccy.iso_code = "XXX";
    ccy.name = "Test Currency";
    ccy.numeric_code = "999";
    ccy.symbol = "";
    ccy.fraction_symbol = "";
    ccy.fractions_per_unit = 1;
    ccy.rounding_type = "";
    ccy.rounding_precision = 0;
    ccy.format = "";
    ccy.currency_type = "";
    ccy.recorded_by = "";
    ccy.recorded_at = {};

    resp.currencies.push_back(ccy);

    BOOST_LOG_SEV(lg, debug) << "Response with empty optional fields";

    const auto serialized = resp.serialize();
    const auto result = get_currencies_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& deserialized = result.value();

    BOOST_LOG_SEV(lg, debug) << "Deserialized currency with empty fields";

    CHECK(deserialized.currencies[0].iso_code == "XXX");
    CHECK(deserialized.currencies[0].symbol.empty());
    CHECK(deserialized.currencies[0].currency_type.empty());
}

TEST_CASE("get_currency_history_response_serialize_deserialize", tags) {
    using namespace ores::telemetry::log;
    using ores::risk::domain::currency_version;
    using ores::risk::domain::currency_version_history;
    auto lg(make_logger(test_suite));

    get_currency_history_response original;
    original.success = true;
    original.message = "History retrieved successfully";
    original.history.iso_code = "USD";

    // Create 3 versions with different version numbers
    for (int i = 3; i >= 1; --i) {
        currency_version ver;
        ver.data.iso_code = "USD";
        ver.data.name = "US Dollar v" + std::to_string(i);
        ver.data.numeric_code = "840";
        ver.data.symbol = "$";
        ver.data.fraction_symbol = "cent";
        ver.data.fractions_per_unit = 100;
        ver.data.rounding_type = "Nearest";
        ver.data.rounding_precision = 2;
        ver.data.format = "#,##0.00";
        ver.data.currency_type = "Major";
        ver.data.recorded_by = "admin";
        ver.data.recorded_at = ores::utility::datetime::datetime::parse_time_point(
            "2025-01-0" + std::to_string(i) + " 10:00:00");
        ver.version_number = i;
        ver.recorded_by = "admin";
        ver.recorded_at = ores::utility::datetime::datetime::parse_time_point(
            "2025-01-0" + std::to_string(i) + " 10:00:00");
        ver.change_summary = "Version " + std::to_string(i);
        original.history.versions.push_back(ver);
    }

    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.history.versions.size()
                             << " versions";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size() << " bytes";

    const auto result = get_currency_history_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();

    CHECK(des.success == true);
    CHECK(des.history.iso_code == "USD");
    REQUIRE(des.history.versions.size() == 3);

    // Verify version numbers are preserved correctly
    CHECK(des.history.versions[0].version_number == 3);
    CHECK(des.history.versions[1].version_number == 2);
    CHECK(des.history.versions[2].version_number == 1);

    BOOST_LOG_SEV(lg, debug) << "All version numbers verified correctly";
}

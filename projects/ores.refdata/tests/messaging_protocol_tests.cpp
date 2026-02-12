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
#include "ores.refdata/messaging/protocol.hpp"
#include "ores.refdata/messaging/party_identifier_protocol.hpp"
#include "ores.refdata/messaging/party_contact_information_protocol.hpp"
#include "ores.refdata/messaging/counterparty_identifier_protocol.hpp"
#include "ores.refdata/messaging/counterparty_contact_information_protocol.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.platform/time/datetime.hpp"
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.refdata/generators/currency_generator.hpp"
#include "ores.refdata/generators/country_generator.hpp"
#include "ores.refdata/generators/party_generator.hpp"
#include "ores.refdata/generators/party_type_generator.hpp"
#include "ores.refdata/generators/party_status_generator.hpp"
#include "ores.refdata/generators/party_id_scheme_generator.hpp"
#include "ores.refdata/generators/party_identifier_generator.hpp"
#include "ores.refdata/generators/party_contact_information_generator.hpp"
#include "ores.refdata/generators/contact_type_generator.hpp"
#include "ores.refdata/generators/counterparty_generator.hpp"
#include "ores.refdata/generators/counterparty_identifier_generator.hpp"
#include "ores.refdata/generators/counterparty_contact_information_generator.hpp"
#include <boost/uuid/random_generator.hpp>

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[messaging]");

}

using namespace ores::logging;
using namespace ores::refdata::messaging;
using ores::refdata::domain::currency;
using namespace ores::refdata::generators;

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
    using namespace ores::logging;
    auto lg(make_logger(test_suite));

    get_currencies_response resp;
    BOOST_LOG_SEV(lg, debug) << "Empty response: " << resp;

    CHECK(resp.currencies.empty());
}

TEST_CASE("get_currencies_response_with_single_currency", tags) {
    using namespace ores::logging;
    auto lg(make_logger(test_suite));

    get_currencies_response resp;

    auto ccy = *generate_synthetic_currencies(1, ores::utility::uuid::tenant_id::system()).begin();
    resp.currencies.push_back(ccy);
    BOOST_LOG_SEV(lg, debug) << "Response with 1 currency: " << resp;

    CHECK(resp.currencies.size() == 1);
    CHECK(resp.currencies[0].iso_code == ccy.iso_code);
}

TEST_CASE("get_currencies_response_serialize_deserialize", tags) {
    using namespace ores::logging;
    auto lg(make_logger(test_suite));

    get_currencies_response original;

    auto currencies = generate_synthetic_currencies(5, ores::utility::uuid::tenant_id::system());
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
    using namespace ores::logging;
    auto lg(make_logger(test_suite));

    get_currencies_response original;
    auto currencies = generate_unique_synthetic_currencies(50, ores::utility::uuid::tenant_id::system());
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
    using namespace ores::logging;
    auto lg(make_logger(test_suite));

    get_currencies_response resp;
    resp.currencies = generate_synthetic_unicode_currencies(ores::utility::uuid::tenant_id::system());
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
    using namespace ores::logging;
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
    using namespace ores::logging;
    using ores::refdata::domain::currency_version;
    using ores::refdata::domain::currency_version_history;
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
        ver.data.recorded_at = ores::platform::time::datetime::parse_time_point(
            "2025-01-0" + std::to_string(i) + " 10:00:00");
        ver.version_number = i;
        ver.recorded_by = "admin";
        ver.recorded_at = ores::platform::time::datetime::parse_time_point(
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

// ============================================================================
// Currency Protocol - Missing Tests
// ============================================================================

TEST_CASE("save_currency_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_currency_request original;
    original.currency = generate_synthetic_currency(ores::utility::uuid::tenant_id::system());
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_currency_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.currency.iso_code == original.currency.iso_code);
    CHECK(des.currency.name == original.currency.name);
    CHECK(des.currency.numeric_code == original.currency.numeric_code);
    CHECK(des.currency.symbol == original.currency.symbol);
    CHECK(des.currency.fractions_per_unit == original.currency.fractions_per_unit);
    CHECK(des.currency.rounding_precision == original.currency.rounding_precision);
}

TEST_CASE("save_currency_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_currency_response original;
    original.success = true;
    original.message = "ok";
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_currency_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
}

TEST_CASE("delete_currency_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_currency_request original;
    original.iso_codes = {"USD", "EUR", "GBP"};
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_currency_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    REQUIRE(des.iso_codes.size() == 3);
    CHECK(des.iso_codes[0] == "USD");
    CHECK(des.iso_codes[1] == "EUR");
    CHECK(des.iso_codes[2] == "GBP");
}

TEST_CASE("delete_currency_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_currency_response original;
    delete_currency_result r1;
    r1.iso_code = "USD";
    r1.success = true;
    r1.message = "deleted";
    original.results.push_back(r1);

    delete_currency_result r2;
    r2.iso_code = "EUR";
    r2.success = false;
    r2.message = "not found";
    original.results.push_back(r2);
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_currency_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    REQUIRE(des.results.size() == 2);
    CHECK(des.results[0].iso_code == "USD");
    CHECK(des.results[0].success == true);
    CHECK(des.results[0].message == "deleted");
    CHECK(des.results[1].iso_code == "EUR");
    CHECK(des.results[1].success == false);
    CHECK(des.results[1].message == "not found");
}

// ============================================================================
// Currency History Protocol - Missing Tests
// ============================================================================

TEST_CASE("get_currency_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_currency_history_request original;
    original.iso_code = "USD";
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_currency_history_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.iso_code == "USD");
}

// ============================================================================
// Country Protocol
// ============================================================================

TEST_CASE("get_countries_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_countries_request original;
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_countries_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.offset == 0);
    CHECK(des.limit == 100);
}

TEST_CASE("get_countries_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_countries_response original;
    original.countries = generate_fictional_countries(3);
    original.total_available_count = 10;
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.countries.size() << " countries";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_countries_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response with "
                             << des.countries.size() << " countries";

    CHECK(des.total_available_count == 10);
    REQUIRE(des.countries.size() == original.countries.size());
    for (size_t i = 0; i < original.countries.size(); ++i) {
        CHECK(des.countries[i].alpha2_code == original.countries[i].alpha2_code);
        CHECK(des.countries[i].name == original.countries[i].name);
    }
}

TEST_CASE("save_country_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_country_request original;
    original.country = generate_fictional_countries(1)[0];
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_country_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.country.alpha2_code == original.country.alpha2_code);
    CHECK(des.country.name == original.country.name);
}

TEST_CASE("save_country_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_country_response original;
    original.success = true;
    original.message = "ok";
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_country_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
}

TEST_CASE("delete_country_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_country_request original;
    original.alpha2_codes = {"AA", "BB", "CC"};
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_country_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    REQUIRE(des.alpha2_codes.size() == 3);
    CHECK(des.alpha2_codes[0] == "AA");
    CHECK(des.alpha2_codes[1] == "BB");
    CHECK(des.alpha2_codes[2] == "CC");
}

TEST_CASE("delete_country_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_country_response original;
    delete_country_result r1;
    r1.alpha2_code = "AA";
    r1.success = true;
    r1.message = "deleted";
    original.results.push_back(r1);

    delete_country_result r2;
    r2.alpha2_code = "BB";
    r2.success = false;
    r2.message = "not found";
    original.results.push_back(r2);
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_country_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    REQUIRE(des.results.size() == 2);
    CHECK(des.results[0].alpha2_code == "AA");
    CHECK(des.results[0].success == true);
    CHECK(des.results[0].message == "deleted");
    CHECK(des.results[1].alpha2_code == "BB");
    CHECK(des.results[1].success == false);
    CHECK(des.results[1].message == "not found");
}

TEST_CASE("get_country_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_country_history_request original;
    original.alpha2_code = "AA";
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_country_history_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.alpha2_code == "AA");
}

TEST_CASE("get_country_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_country_history_response original;
    original.success = true;
    original.message = "ok";
    original.history = generate_fictional_countries(2);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.history.size() << " history entries";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_country_history_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
    REQUIRE(des.history.size() == 2);
    CHECK(des.history[0].alpha2_code == original.history[0].alpha2_code);
    CHECK(des.history[1].alpha2_code == original.history[1].alpha2_code);
}

// ============================================================================
// Party Type Protocol
// ============================================================================

TEST_CASE("get_party_types_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_types_request original;
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_types_request::deserialize(serialized);

    REQUIRE(result.has_value());
    BOOST_LOG_SEV(lg, debug) << "Deserialized request successfully";
}

TEST_CASE("get_party_types_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_types_response original;
    original.types = generate_synthetic_party_types(3);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.types.size() << " types";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_types_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response with "
                             << des.types.size() << " types";

    REQUIRE(des.types.size() == original.types.size());
    for (size_t i = 0; i < original.types.size(); ++i) {
        CHECK(des.types[i].code == original.types[i].code);
        CHECK(des.types[i].name == original.types[i].name);
    }
}

TEST_CASE("save_party_type_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_party_type_request original;
    original.type = generate_synthetic_party_type();
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_party_type_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.type.code == original.type.code);
    CHECK(des.type.name == original.type.name);
}

TEST_CASE("save_party_type_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_party_type_response original;
    original.success = true;
    original.message = "ok";
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_party_type_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
}

TEST_CASE("delete_party_type_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_party_type_request original;
    original.codes = {"PT01", "PT02", "PT03"};
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_party_type_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    REQUIRE(des.codes.size() == 3);
    CHECK(des.codes[0] == "PT01");
    CHECK(des.codes[1] == "PT02");
    CHECK(des.codes[2] == "PT03");
}

TEST_CASE("delete_party_type_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_party_type_response original;
    delete_party_type_result r1;
    r1.code = "PT01";
    r1.success = true;
    r1.message = "deleted";
    original.results.push_back(r1);

    delete_party_type_result r2;
    r2.code = "PT02";
    r2.success = false;
    r2.message = "not found";
    original.results.push_back(r2);
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_party_type_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    REQUIRE(des.results.size() == 2);
    CHECK(des.results[0].code == "PT01");
    CHECK(des.results[0].success == true);
    CHECK(des.results[0].message == "deleted");
    CHECK(des.results[1].code == "PT02");
    CHECK(des.results[1].success == false);
    CHECK(des.results[1].message == "not found");
}

TEST_CASE("get_party_type_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_type_history_request original;
    original.code = "PT01";
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_type_history_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.code == "PT01");
}

TEST_CASE("get_party_type_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_type_history_response original;
    original.success = true;
    original.message = "ok";
    original.versions = generate_synthetic_party_types(2);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.versions.size() << " versions";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_type_history_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
    REQUIRE(des.versions.size() == 2);
    CHECK(des.versions[0].code == original.versions[0].code);
    CHECK(des.versions[1].code == original.versions[1].code);
}

// ============================================================================
// Party Status Protocol
// ============================================================================

TEST_CASE("get_party_statuses_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_statuses_request original;
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_statuses_request::deserialize(serialized);

    REQUIRE(result.has_value());
    BOOST_LOG_SEV(lg, debug) << "Deserialized request successfully";
}

TEST_CASE("get_party_statuses_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_statuses_response original;
    original.statuses = generate_synthetic_party_statuses(3);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.statuses.size() << " statuses";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_statuses_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response with "
                             << des.statuses.size() << " statuses";

    REQUIRE(des.statuses.size() == original.statuses.size());
    for (size_t i = 0; i < original.statuses.size(); ++i) {
        CHECK(des.statuses[i].code == original.statuses[i].code);
        CHECK(des.statuses[i].name == original.statuses[i].name);
    }
}

TEST_CASE("save_party_status_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_party_status_request original;
    original.status = generate_synthetic_party_status();
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_party_status_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.status.code == original.status.code);
    CHECK(des.status.name == original.status.name);
}

TEST_CASE("save_party_status_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_party_status_response original;
    original.success = true;
    original.message = "ok";
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_party_status_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
}

TEST_CASE("delete_party_status_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_party_status_request original;
    original.codes = {"PS01", "PS02", "PS03"};
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_party_status_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    REQUIRE(des.codes.size() == 3);
    CHECK(des.codes[0] == "PS01");
    CHECK(des.codes[1] == "PS02");
    CHECK(des.codes[2] == "PS03");
}

TEST_CASE("delete_party_status_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_party_status_response original;
    delete_party_status_result r1;
    r1.code = "PS01";
    r1.success = true;
    r1.message = "deleted";
    original.results.push_back(r1);

    delete_party_status_result r2;
    r2.code = "PS02";
    r2.success = false;
    r2.message = "not found";
    original.results.push_back(r2);
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_party_status_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    REQUIRE(des.results.size() == 2);
    CHECK(des.results[0].code == "PS01");
    CHECK(des.results[0].success == true);
    CHECK(des.results[0].message == "deleted");
    CHECK(des.results[1].code == "PS02");
    CHECK(des.results[1].success == false);
    CHECK(des.results[1].message == "not found");
}

TEST_CASE("get_party_status_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_status_history_request original;
    original.code = "PS01";
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_status_history_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.code == "PS01");
}

TEST_CASE("get_party_status_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_status_history_response original;
    original.success = true;
    original.message = "ok";
    original.versions = generate_synthetic_party_statuses(2);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.versions.size() << " versions";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_status_history_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
    REQUIRE(des.versions.size() == 2);
    CHECK(des.versions[0].code == original.versions[0].code);
    CHECK(des.versions[1].code == original.versions[1].code);
}

// ============================================================================
// Party ID Scheme Protocol
// ============================================================================

TEST_CASE("get_party_id_schemes_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_id_schemes_request original;
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_id_schemes_request::deserialize(serialized);

    REQUIRE(result.has_value());
    BOOST_LOG_SEV(lg, debug) << "Deserialized request successfully";
}

TEST_CASE("get_party_id_schemes_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_id_schemes_response original;
    original.schemes = generate_synthetic_party_id_schemes(3);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.schemes.size() << " schemes";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_id_schemes_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response with "
                             << des.schemes.size() << " schemes";

    REQUIRE(des.schemes.size() == original.schemes.size());
    for (size_t i = 0; i < original.schemes.size(); ++i) {
        CHECK(des.schemes[i].code == original.schemes[i].code);
        CHECK(des.schemes[i].name == original.schemes[i].name);
    }
}

TEST_CASE("save_party_id_scheme_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_party_id_scheme_request original;
    original.scheme = generate_synthetic_party_id_scheme();
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_party_id_scheme_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.scheme.code == original.scheme.code);
    CHECK(des.scheme.name == original.scheme.name);
}

TEST_CASE("save_party_id_scheme_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_party_id_scheme_response original;
    original.success = true;
    original.message = "ok";
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_party_id_scheme_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
}

TEST_CASE("delete_party_id_scheme_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_party_id_scheme_request original;
    original.codes = {"PIS01", "PIS02", "PIS03"};
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_party_id_scheme_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    REQUIRE(des.codes.size() == 3);
    CHECK(des.codes[0] == "PIS01");
    CHECK(des.codes[1] == "PIS02");
    CHECK(des.codes[2] == "PIS03");
}

TEST_CASE("delete_party_id_scheme_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_party_id_scheme_response original;
    delete_party_id_scheme_result r1;
    r1.code = "PIS01";
    r1.success = true;
    r1.message = "deleted";
    original.results.push_back(r1);

    delete_party_id_scheme_result r2;
    r2.code = "PIS02";
    r2.success = false;
    r2.message = "not found";
    original.results.push_back(r2);
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_party_id_scheme_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    REQUIRE(des.results.size() == 2);
    CHECK(des.results[0].code == "PIS01");
    CHECK(des.results[0].success == true);
    CHECK(des.results[0].message == "deleted");
    CHECK(des.results[1].code == "PIS02");
    CHECK(des.results[1].success == false);
    CHECK(des.results[1].message == "not found");
}

TEST_CASE("get_party_id_scheme_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_id_scheme_history_request original;
    original.code = "PIS01";
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_id_scheme_history_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.code == "PIS01");
}

TEST_CASE("get_party_id_scheme_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_id_scheme_history_response original;
    original.success = true;
    original.message = "ok";
    original.versions = generate_synthetic_party_id_schemes(2);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.versions.size() << " versions";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_id_scheme_history_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
    REQUIRE(des.versions.size() == 2);
    CHECK(des.versions[0].code == original.versions[0].code);
    CHECK(des.versions[1].code == original.versions[1].code);
}

// ============================================================================
// Contact Type Protocol
// ============================================================================

TEST_CASE("get_contact_types_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_contact_types_request original;
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_contact_types_request::deserialize(serialized);

    REQUIRE(result.has_value());
    BOOST_LOG_SEV(lg, debug) << "Deserialized request successfully";
}

TEST_CASE("get_contact_types_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_contact_types_response original;
    original.types = generate_synthetic_contact_types(3);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.types.size() << " types";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_contact_types_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response with "
                             << des.types.size() << " types";

    REQUIRE(des.types.size() == original.types.size());
    for (size_t i = 0; i < original.types.size(); ++i) {
        CHECK(des.types[i].code == original.types[i].code);
        CHECK(des.types[i].name == original.types[i].name);
    }
}

TEST_CASE("save_contact_type_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_contact_type_request original;
    original.type = generate_synthetic_contact_type();
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_contact_type_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.type.code == original.type.code);
    CHECK(des.type.name == original.type.name);
}

TEST_CASE("save_contact_type_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_contact_type_response original;
    original.success = true;
    original.message = "ok";
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_contact_type_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
}

TEST_CASE("delete_contact_type_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_contact_type_request original;
    original.codes = {"CT01", "CT02", "CT03"};
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_contact_type_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    REQUIRE(des.codes.size() == 3);
    CHECK(des.codes[0] == "CT01");
    CHECK(des.codes[1] == "CT02");
    CHECK(des.codes[2] == "CT03");
}

TEST_CASE("delete_contact_type_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_contact_type_response original;
    delete_contact_type_result r1;
    r1.code = "CT01";
    r1.success = true;
    r1.message = "deleted";
    original.results.push_back(r1);

    delete_contact_type_result r2;
    r2.code = "CT02";
    r2.success = false;
    r2.message = "not found";
    original.results.push_back(r2);
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_contact_type_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    REQUIRE(des.results.size() == 2);
    CHECK(des.results[0].code == "CT01");
    CHECK(des.results[0].success == true);
    CHECK(des.results[0].message == "deleted");
    CHECK(des.results[1].code == "CT02");
    CHECK(des.results[1].success == false);
    CHECK(des.results[1].message == "not found");
}

TEST_CASE("get_contact_type_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_contact_type_history_request original;
    original.code = "CT01";
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_contact_type_history_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.code == "CT01");
}

TEST_CASE("get_contact_type_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_contact_type_history_response original;
    original.success = true;
    original.message = "ok";
    original.versions = generate_synthetic_contact_types(2);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.versions.size() << " versions";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_contact_type_history_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
    REQUIRE(des.versions.size() == 2);
    CHECK(des.versions[0].code == original.versions[0].code);
    CHECK(des.versions[1].code == original.versions[1].code);
}

// ============================================================================
// Party Protocol
// ============================================================================

TEST_CASE("get_parties_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_parties_request original;
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_parties_request::deserialize(serialized);

    REQUIRE(result.has_value());
    BOOST_LOG_SEV(lg, debug) << "Deserialized request successfully";
}

TEST_CASE("get_parties_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_parties_response original;
    original.parties = generate_synthetic_parties(3);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.parties.size() << " parties";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_parties_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response with "
                             << des.parties.size() << " parties";

    REQUIRE(des.parties.size() == original.parties.size());
    for (size_t i = 0; i < original.parties.size(); ++i) {
        CHECK(des.parties[i].id == original.parties[i].id);
        CHECK(des.parties[i].full_name == original.parties[i].full_name);
    }
}

TEST_CASE("save_party_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_party_request original;
    original.party = generate_synthetic_party();
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_party_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.party.id == original.party.id);
    CHECK(des.party.full_name == original.party.full_name);
}

TEST_CASE("save_party_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_party_response original;
    original.success = true;
    original.message = "ok";
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_party_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
}

TEST_CASE("delete_party_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    delete_party_request original;
    original.ids = {uuid_gen(), uuid_gen(), uuid_gen()};
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_party_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    REQUIRE(des.ids.size() == 3);
    CHECK(des.ids[0] == original.ids[0]);
    CHECK(des.ids[1] == original.ids[1]);
    CHECK(des.ids[2] == original.ids[2]);
}

TEST_CASE("delete_party_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    delete_party_response original;
    delete_party_result r1;
    r1.id = uuid_gen();
    r1.success = true;
    r1.message = "deleted";
    original.results.push_back(r1);

    delete_party_result r2;
    r2.id = uuid_gen();
    r2.success = false;
    r2.message = "not found";
    original.results.push_back(r2);
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_party_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    REQUIRE(des.results.size() == 2);
    CHECK(des.results[0].id == r1.id);
    CHECK(des.results[0].success == true);
    CHECK(des.results[0].message == "deleted");
    CHECK(des.results[1].id == r2.id);
    CHECK(des.results[1].success == false);
    CHECK(des.results[1].message == "not found");
}

TEST_CASE("get_party_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    get_party_history_request original;
    original.id = uuid_gen();
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_history_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.id == original.id);
}

TEST_CASE("get_party_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_history_response original;
    original.success = true;
    original.message = "ok";
    original.versions = generate_synthetic_parties(2);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.versions.size() << " versions";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_history_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
    REQUIRE(des.versions.size() == 2);
    CHECK(des.versions[0].id == original.versions[0].id);
    CHECK(des.versions[1].id == original.versions[1].id);
}

// ============================================================================
// Counterparty Protocol
// ============================================================================

TEST_CASE("get_counterparties_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_counterparties_request original;
    original.offset = 50;
    original.limit = 25;
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_counterparties_request::deserialize(serialized);

    REQUIRE(result.has_value());
    CHECK(result->offset == 50);
    CHECK(result->limit == 25);
    BOOST_LOG_SEV(lg, debug) << "Deserialized request successfully";
}

TEST_CASE("get_counterparties_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_counterparties_response original;
    original.counterparties = generate_synthetic_counterparties(3);
    original.total_available_count = 42;
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.counterparties.size() << " counterparties";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_counterparties_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response with "
                             << des.counterparties.size() << " counterparties";

    CHECK(des.total_available_count == 42);
    REQUIRE(des.counterparties.size() == original.counterparties.size());
    for (size_t i = 0; i < original.counterparties.size(); ++i) {
        CHECK(des.counterparties[i].id == original.counterparties[i].id);
        CHECK(des.counterparties[i].full_name == original.counterparties[i].full_name);
    }
}

TEST_CASE("save_counterparty_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_counterparty_request original;
    original.counterparty = generate_synthetic_counterparty();
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_counterparty_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.counterparty.id == original.counterparty.id);
    CHECK(des.counterparty.full_name == original.counterparty.full_name);
}

TEST_CASE("save_counterparty_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_counterparty_response original;
    original.success = true;
    original.message = "ok";
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_counterparty_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
}

TEST_CASE("delete_counterparty_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    delete_counterparty_request original;
    original.ids = {uuid_gen(), uuid_gen(), uuid_gen()};
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_counterparty_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    REQUIRE(des.ids.size() == 3);
    CHECK(des.ids[0] == original.ids[0]);
    CHECK(des.ids[1] == original.ids[1]);
    CHECK(des.ids[2] == original.ids[2]);
}

TEST_CASE("delete_counterparty_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    delete_counterparty_response original;
    delete_counterparty_result r1;
    r1.id = uuid_gen();
    r1.success = true;
    r1.message = "deleted";
    original.results.push_back(r1);

    delete_counterparty_result r2;
    r2.id = uuid_gen();
    r2.success = false;
    r2.message = "not found";
    original.results.push_back(r2);
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_counterparty_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    REQUIRE(des.results.size() == 2);
    CHECK(des.results[0].id == r1.id);
    CHECK(des.results[0].success == true);
    CHECK(des.results[0].message == "deleted");
    CHECK(des.results[1].id == r2.id);
    CHECK(des.results[1].success == false);
    CHECK(des.results[1].message == "not found");
}

TEST_CASE("get_counterparty_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    get_counterparty_history_request original;
    original.id = uuid_gen();
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_counterparty_history_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.id == original.id);
}

TEST_CASE("get_counterparty_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_counterparty_history_response original;
    original.success = true;
    original.message = "ok";
    original.versions = generate_synthetic_counterparties(2);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.versions.size() << " versions";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_counterparty_history_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
    REQUIRE(des.versions.size() == 2);
    CHECK(des.versions[0].id == original.versions[0].id);
    CHECK(des.versions[1].id == original.versions[1].id);
}

// ============================================================================
// Party Identifier Protocol
// ============================================================================

TEST_CASE("get_party_identifiers_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_identifiers_request original;
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_identifiers_request::deserialize(serialized);

    REQUIRE(result.has_value());
    BOOST_LOG_SEV(lg, debug) << "Deserialized request successfully";
}

TEST_CASE("get_party_identifiers_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_identifiers_response original;
    original.party_identifiers = generate_synthetic_party_identifiers(3);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.party_identifiers.size()
                             << " party_identifiers";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_identifiers_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response with "
                             << des.party_identifiers.size()
                             << " party_identifiers";

    REQUIRE(des.party_identifiers.size() == original.party_identifiers.size());
    for (size_t i = 0; i < original.party_identifiers.size(); ++i) {
        CHECK(des.party_identifiers[i].id == original.party_identifiers[i].id);
    }
}

TEST_CASE("save_party_identifier_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_party_identifier_request original;
    original.party_identifier = generate_synthetic_party_identifier();
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_party_identifier_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.party_identifier.id == original.party_identifier.id);
}

TEST_CASE("save_party_identifier_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_party_identifier_response original;
    original.success = true;
    original.message = "ok";
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = save_party_identifier_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
}

TEST_CASE("delete_party_identifier_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    delete_party_identifier_request original;
    original.ids = {uuid_gen(), uuid_gen(), uuid_gen()};
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_party_identifier_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    REQUIRE(des.ids.size() == 3);
    CHECK(des.ids[0] == original.ids[0]);
    CHECK(des.ids[1] == original.ids[1]);
    CHECK(des.ids[2] == original.ids[2]);
}

TEST_CASE("delete_party_identifier_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    delete_party_identifier_response original;
    delete_party_identifier_result r1;
    r1.id = uuid_gen();
    r1.success = true;
    r1.message = "deleted";
    original.results.push_back(r1);

    delete_party_identifier_result r2;
    r2.id = uuid_gen();
    r2.success = false;
    r2.message = "not found";
    original.results.push_back(r2);
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = delete_party_identifier_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    REQUIRE(des.results.size() == 2);
    CHECK(des.results[0].id == r1.id);
    CHECK(des.results[0].success == true);
    CHECK(des.results[0].message == "deleted");
    CHECK(des.results[1].id == r2.id);
    CHECK(des.results[1].success == false);
    CHECK(des.results[1].message == "not found");
}

TEST_CASE("get_party_identifier_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    get_party_identifier_history_request original;
    original.id = uuid_gen();
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_identifier_history_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.id == original.id);
}

TEST_CASE("get_party_identifier_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_identifier_history_response original;
    original.success = true;
    original.message = "ok";
    original.versions = generate_synthetic_party_identifiers(2);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.versions.size() << " versions";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_identifier_history_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
    REQUIRE(des.versions.size() == 2);
    CHECK(des.versions[0].id == original.versions[0].id);
    CHECK(des.versions[1].id == original.versions[1].id);
}

// ============================================================================
// Party Contact Information Protocol
// ============================================================================

TEST_CASE("get_party_contact_informations_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_contact_informations_request original;
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result = get_party_contact_informations_request::deserialize(serialized);

    REQUIRE(result.has_value());
    BOOST_LOG_SEV(lg, debug) << "Deserialized request successfully";
}

TEST_CASE("get_party_contact_informations_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_contact_informations_response original;
    original.party_contact_informations =
        generate_synthetic_party_contact_informations(3);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.party_contact_informations.size()
                             << " party_contact_informations";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        get_party_contact_informations_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response with "
                             << des.party_contact_informations.size()
                             << " party_contact_informations";

    REQUIRE(des.party_contact_informations.size() ==
            original.party_contact_informations.size());
    for (size_t i = 0; i < original.party_contact_informations.size(); ++i) {
        CHECK(des.party_contact_informations[i].id ==
              original.party_contact_informations[i].id);
    }
}

TEST_CASE("save_party_contact_information_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_party_contact_information_request original;
    original.party_contact_information =
        generate_synthetic_party_contact_information();
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        save_party_contact_information_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.party_contact_information.id ==
          original.party_contact_information.id);
}

TEST_CASE("save_party_contact_information_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_party_contact_information_response original;
    original.success = true;
    original.message = "ok";
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        save_party_contact_information_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
}

TEST_CASE("delete_party_contact_information_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    delete_party_contact_information_request original;
    original.ids = {uuid_gen(), uuid_gen(), uuid_gen()};
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        delete_party_contact_information_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    REQUIRE(des.ids.size() == 3);
    CHECK(des.ids[0] == original.ids[0]);
    CHECK(des.ids[1] == original.ids[1]);
    CHECK(des.ids[2] == original.ids[2]);
}

TEST_CASE("delete_party_contact_information_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    delete_party_contact_information_response original;
    delete_party_contact_information_result r1;
    r1.id = uuid_gen();
    r1.success = true;
    r1.message = "deleted";
    original.results.push_back(r1);

    delete_party_contact_information_result r2;
    r2.id = uuid_gen();
    r2.success = false;
    r2.message = "not found";
    original.results.push_back(r2);
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        delete_party_contact_information_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    REQUIRE(des.results.size() == 2);
    CHECK(des.results[0].id == r1.id);
    CHECK(des.results[0].success == true);
    CHECK(des.results[0].message == "deleted");
    CHECK(des.results[1].id == r2.id);
    CHECK(des.results[1].success == false);
    CHECK(des.results[1].message == "not found");
}

TEST_CASE("get_party_contact_information_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    get_party_contact_information_history_request original;
    original.id = uuid_gen();
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        get_party_contact_information_history_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.id == original.id);
}

TEST_CASE("get_party_contact_information_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_party_contact_information_history_response original;
    original.success = true;
    original.message = "ok";
    original.versions = generate_synthetic_party_contact_informations(2);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.versions.size() << " versions";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        get_party_contact_information_history_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
    REQUIRE(des.versions.size() == 2);
    CHECK(des.versions[0].id == original.versions[0].id);
    CHECK(des.versions[1].id == original.versions[1].id);
}

// ============================================================================
// Counterparty Identifier Protocol
// ============================================================================

TEST_CASE("get_counterparty_identifiers_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_counterparty_identifiers_request original;
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        get_counterparty_identifiers_request::deserialize(serialized);

    REQUIRE(result.has_value());
    BOOST_LOG_SEV(lg, debug) << "Deserialized request successfully";
}

TEST_CASE("get_counterparty_identifiers_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_counterparty_identifiers_response original;
    original.counterparty_identifiers =
        generate_synthetic_counterparty_identifiers(3);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.counterparty_identifiers.size()
                             << " counterparty_identifiers";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        get_counterparty_identifiers_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response with "
                             << des.counterparty_identifiers.size()
                             << " counterparty_identifiers";

    REQUIRE(des.counterparty_identifiers.size() ==
            original.counterparty_identifiers.size());
    for (size_t i = 0; i < original.counterparty_identifiers.size(); ++i) {
        CHECK(des.counterparty_identifiers[i].id ==
              original.counterparty_identifiers[i].id);
    }
}

TEST_CASE("save_counterparty_identifier_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_counterparty_identifier_request original;
    original.counterparty_identifier =
        generate_synthetic_counterparty_identifier();
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        save_counterparty_identifier_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.counterparty_identifier.id ==
          original.counterparty_identifier.id);
}

TEST_CASE("save_counterparty_identifier_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_counterparty_identifier_response original;
    original.success = true;
    original.message = "ok";
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        save_counterparty_identifier_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
}

TEST_CASE("delete_counterparty_identifier_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    delete_counterparty_identifier_request original;
    original.ids = {uuid_gen(), uuid_gen(), uuid_gen()};
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        delete_counterparty_identifier_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    REQUIRE(des.ids.size() == 3);
    CHECK(des.ids[0] == original.ids[0]);
    CHECK(des.ids[1] == original.ids[1]);
    CHECK(des.ids[2] == original.ids[2]);
}

TEST_CASE("delete_counterparty_identifier_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    delete_counterparty_identifier_response original;
    delete_counterparty_identifier_result r1;
    r1.id = uuid_gen();
    r1.success = true;
    r1.message = "deleted";
    original.results.push_back(r1);

    delete_counterparty_identifier_result r2;
    r2.id = uuid_gen();
    r2.success = false;
    r2.message = "not found";
    original.results.push_back(r2);
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        delete_counterparty_identifier_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    REQUIRE(des.results.size() == 2);
    CHECK(des.results[0].id == r1.id);
    CHECK(des.results[0].success == true);
    CHECK(des.results[0].message == "deleted");
    CHECK(des.results[1].id == r2.id);
    CHECK(des.results[1].success == false);
    CHECK(des.results[1].message == "not found");
}

TEST_CASE("get_counterparty_identifier_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    get_counterparty_identifier_history_request original;
    original.id = uuid_gen();
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        get_counterparty_identifier_history_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.id == original.id);
}

TEST_CASE("get_counterparty_identifier_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_counterparty_identifier_history_response original;
    original.success = true;
    original.message = "ok";
    original.versions = generate_synthetic_counterparty_identifiers(2);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.versions.size() << " versions";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        get_counterparty_identifier_history_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
    REQUIRE(des.versions.size() == 2);
    CHECK(des.versions[0].id == original.versions[0].id);
    CHECK(des.versions[1].id == original.versions[1].id);
}

// ============================================================================
// Counterparty Contact Information Protocol
// ============================================================================

TEST_CASE("get_counterparty_contact_informations_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_counterparty_contact_informations_request original;
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        get_counterparty_contact_informations_request::deserialize(serialized);

    REQUIRE(result.has_value());
    BOOST_LOG_SEV(lg, debug) << "Deserialized request successfully";
}

TEST_CASE("get_counterparty_contact_informations_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_counterparty_contact_informations_response original;
    original.counterparty_contact_informations =
        generate_synthetic_counterparty_contact_informations(3);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.counterparty_contact_informations.size()
                             << " counterparty_contact_informations";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        get_counterparty_contact_informations_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response with "
                             << des.counterparty_contact_informations.size()
                             << " counterparty_contact_informations";

    REQUIRE(des.counterparty_contact_informations.size() ==
            original.counterparty_contact_informations.size());
    for (size_t i = 0;
         i < original.counterparty_contact_informations.size(); ++i) {
        CHECK(des.counterparty_contact_informations[i].id ==
              original.counterparty_contact_informations[i].id);
    }
}

TEST_CASE("save_counterparty_contact_information_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_counterparty_contact_information_request original;
    original.counterparty_contact_information =
        generate_synthetic_counterparty_contact_information();
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        save_counterparty_contact_information_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.counterparty_contact_information.id ==
          original.counterparty_contact_information.id);
}

TEST_CASE("save_counterparty_contact_information_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_counterparty_contact_information_response original;
    original.success = true;
    original.message = "ok";
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        save_counterparty_contact_information_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
}

TEST_CASE("delete_counterparty_contact_information_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    delete_counterparty_contact_information_request original;
    original.ids = {uuid_gen(), uuid_gen(), uuid_gen()};
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        delete_counterparty_contact_information_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    REQUIRE(des.ids.size() == 3);
    CHECK(des.ids[0] == original.ids[0]);
    CHECK(des.ids[1] == original.ids[1]);
    CHECK(des.ids[2] == original.ids[2]);
}

TEST_CASE("delete_counterparty_contact_information_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    delete_counterparty_contact_information_response original;
    delete_counterparty_contact_information_result r1;
    r1.id = uuid_gen();
    r1.success = true;
    r1.message = "deleted";
    original.results.push_back(r1);

    delete_counterparty_contact_information_result r2;
    r2.id = uuid_gen();
    r2.success = false;
    r2.message = "not found";
    original.results.push_back(r2);
    BOOST_LOG_SEV(lg, debug) << "Original response: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        delete_counterparty_contact_information_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    REQUIRE(des.results.size() == 2);
    CHECK(des.results[0].id == r1.id);
    CHECK(des.results[0].success == true);
    CHECK(des.results[0].message == "deleted");
    CHECK(des.results[1].id == r2.id);
    CHECK(des.results[1].success == false);
    CHECK(des.results[1].message == "not found");
}

TEST_CASE("get_counterparty_contact_information_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::random_generator uuid_gen;
    get_counterparty_contact_information_history_request original;
    original.id = uuid_gen();
    BOOST_LOG_SEV(lg, debug) << "Original request: " << original;

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        get_counterparty_contact_information_history_request::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized request: " << des;

    CHECK(des.id == original.id);
}

TEST_CASE("get_counterparty_contact_information_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_counterparty_contact_information_history_response original;
    original.success = true;
    original.message = "ok";
    original.versions =
        generate_synthetic_counterparty_contact_informations(2);
    BOOST_LOG_SEV(lg, debug) << "Original response with "
                             << original.versions.size() << " versions";

    const auto serialized = original.serialize();
    BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

    const auto result =
        get_counterparty_contact_information_history_response::deserialize(serialized);

    REQUIRE(result.has_value());
    const auto& des = result.value();
    BOOST_LOG_SEV(lg, debug) << "Deserialized response: " << des;

    CHECK(des.success == true);
    CHECK(des.message == "ok");
    REQUIRE(des.versions.size() == 2);
    CHECK(des.versions[0].id == original.versions[0].id);
    CHECK(des.versions[1].id == original.versions[1].id);
}

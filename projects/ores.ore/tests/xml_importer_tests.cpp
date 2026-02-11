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
#include "ores.ore/xml/importer.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.testing/project_root.hpp"

namespace {

const std::string_view test_suite("ores.ore.tests");
const std::string tags("[ore][xml][importer]");

std::filesystem::path ore_path(const std::string& relative) {
    return ores::testing::project_root::resolve("external/ore/" + relative);
}

}

using ores::ore::xml::importer;
using ores::refdata::domain::currency;
using namespace ores::logging;

// =============================================================================
// validate_currency tests
// =============================================================================

TEST_CASE("validate_currency_with_all_required_fields", tags) {
    auto lg(make_logger(test_suite));

    currency c;
    c.name = "US Dollar";
    c.iso_code = "USD";
    c.fractions_per_unit = 100;
    c.rounding_type = "Closest";
    c.rounding_precision = 2;

    const auto errors = importer::validate_currency(c);
    BOOST_LOG_SEV(lg, debug) << "Validation errors: '" << errors << "'";

    CHECK(errors.empty());
}

TEST_CASE("validate_currency_with_missing_name", tags) {
    auto lg(make_logger(test_suite));

    currency c;
    c.iso_code = "USD";
    c.fractions_per_unit = 100;
    c.rounding_type = "Closest";
    c.rounding_precision = 2;

    const auto errors = importer::validate_currency(c);
    BOOST_LOG_SEV(lg, debug) << "Validation errors: '" << errors << "'";

    CHECK(!errors.empty());
    CHECK(errors.contains("Name is required"));
}

TEST_CASE("validate_currency_with_missing_iso_code", tags) {
    auto lg(make_logger(test_suite));

    currency c;
    c.name = "US Dollar";
    c.fractions_per_unit = 100;
    c.rounding_type = "Closest";
    c.rounding_precision = 2;

    const auto errors = importer::validate_currency(c);
    BOOST_LOG_SEV(lg, debug) << "Validation errors: '" << errors << "'";

    CHECK(!errors.empty());
    CHECK(errors.contains("ISO code is required"));
}

TEST_CASE("validate_currency_with_missing_rounding_type", tags) {
    auto lg(make_logger(test_suite));

    currency c;
    c.name = "US Dollar";
    c.iso_code = "USD";
    c.fractions_per_unit = 100;
    c.rounding_precision = 2;

    const auto errors = importer::validate_currency(c);
    BOOST_LOG_SEV(lg, debug) << "Validation errors: '" << errors << "'";

    CHECK(!errors.empty());
    CHECK(errors.contains("Rounding type is required"));
}

TEST_CASE("validate_currency_with_zero_fractions_per_unit", tags) {
    auto lg(make_logger(test_suite));

    currency c;
    c.name = "US Dollar";
    c.iso_code = "USD";
    c.fractions_per_unit = 0;
    c.rounding_type = "Closest";
    c.rounding_precision = 2;

    const auto errors = importer::validate_currency(c);
    BOOST_LOG_SEV(lg, debug) << "Validation errors: '" << errors << "'";

    CHECK(!errors.empty());
    CHECK(errors.contains("Fractions per unit must be positive"));
}

TEST_CASE("validate_currency_with_negative_rounding_precision", tags) {
    auto lg(make_logger(test_suite));

    currency c;
    c.name = "US Dollar";
    c.iso_code = "USD";
    c.fractions_per_unit = 100;
    c.rounding_type = "Closest";
    c.rounding_precision = -1;

    const auto errors = importer::validate_currency(c);
    BOOST_LOG_SEV(lg, debug) << "Validation errors: '" << errors << "'";

    CHECK(!errors.empty());
    CHECK(errors.contains("Rounding precision must be non-negative"));
}

TEST_CASE("validate_currency_with_multiple_errors", tags) {
    auto lg(make_logger(test_suite));

    currency c;
    // All required fields left empty/default

    const auto errors = importer::validate_currency(c);
    BOOST_LOG_SEV(lg, debug) << "Validation errors: '" << errors << "'";

    CHECK(!errors.empty());
    CHECK(errors.contains("Name is required"));
    CHECK(errors.contains("ISO code is required"));
    CHECK(errors.contains("Fractions per unit must be positive"));
    CHECK(errors.contains("Rounding type is required"));
}

// =============================================================================
// import_currency_config tests
// =============================================================================

TEST_CASE("import_currency_config_from_currencies_xml", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path("examples/Input/currencies.xml");
    BOOST_LOG_SEV(lg, debug) << "Importing from: " << f;

    const auto currencies = importer::import_currency_config(f);
    BOOST_LOG_SEV(lg, debug) << "Imported " << currencies.size() << " currencies";

    REQUIRE(currencies.size() == 179);

    const auto& first = currencies.front();
    CHECK(first.iso_code == "AED");
    CHECK(first.name == "United Arab Emirates dirham");
    CHECK(first.numeric_code == "784");
    CHECK(first.fractions_per_unit == 100);
    CHECK(first.rounding_type == "Closest");
    CHECK(first.rounding_precision == 2);
    CHECK(first.currency_type == "Major");
    CHECK(first.recorded_by == "ores");
    CHECK(first.change_reason_code == "system.external_data_import");
}

TEST_CASE("import_currency_config_from_example_1", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path("examples/Legacy/Example_1/Input/currencies.xml");
    BOOST_LOG_SEV(lg, debug) << "Importing from: " << f;

    const auto currencies = importer::import_currency_config(f);
    BOOST_LOG_SEV(lg, debug) << "Imported " << currencies.size() << " currencies";

    REQUIRE(currencies.size() == 2);

    const auto& first = currencies.front();
    CHECK(first.iso_code == "PGK");
    CHECK(first.name == "Papua New Guinean kina");
    CHECK(first.numeric_code == "598");
    CHECK(first.symbol == "K");
    CHECK(first.fractions_per_unit == 100);
    CHECK(first.rounding_type == "Closest");
    CHECK(first.rounding_precision == 2);
    CHECK(first.currency_type.empty());
}

TEST_CASE("import_currency_config_maps_all_fields", tags) {
    auto lg(make_logger(test_suite));

    const auto f = ore_path("examples/Input/currencies.xml");
    const auto currencies = importer::import_currency_config(f);
    REQUIRE(!currencies.empty());

    for (const auto& c : currencies) {
        const auto errors = importer::validate_currency(c);
        INFO("Currency " << c.iso_code << " failed validation: " << errors);
        CHECK(errors.empty());
    }

    BOOST_LOG_SEV(lg, debug) << "All " << currencies.size()
                             << " imported currencies pass validation";
}

/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/generators/currency_generator.hpp"
#include "ores.refdata.core/presentation/currency_field_mapper.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[presentation]");

ores::refdata::domain::currency make_currency() {
    ores::refdata::domain::currency c;
    c.version = 1;
    c.iso_code = "USD";
    c.name = "US Dollar";
    c.numeric_code = "840";
    c.symbol = "$";
    c.fraction_symbol = "¢";
    c.fractions_per_unit = 100;
    c.rounding_type = "Closest";
    c.rounding_precision = 2;
    c.format = "%3% %1$.2f";
    c.monetary_nature = "fiat";
    c.market_tier = "major";
    c.change_reason_code = "system.test";
    c.change_commentary = "Initial creation";
    return c;
}

}

using namespace ores::refdata::generators;
using ores::refdata::domain::currency;
using ores::refdata::presentation::currency_field_mapper;
using ores::refdata::repository::currency_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("map_renders_all_fields_in_order", tags) {
    const auto c = make_currency();

    const auto fields = currency_field_mapper::map(c);

    REQUIRE(fields.size() == 14);
    CHECK(fields[0].name == "ISO Code");
    CHECK(fields[0].value == "USD");
    CHECK(fields[1].name == "Name");
    CHECK(fields[1].value == "US Dollar");
    CHECK(fields[2].name == "Numeric Code");
    CHECK(fields[2].value == "840");
    CHECK(fields[3].name == "Symbol");
    CHECK(fields[3].value == "$");
    CHECK(fields[4].name == "Fraction Symbol");
    CHECK(fields[4].value == "¢");
    CHECK(fields[5].name == "Rounding Type");
    CHECK(fields[5].value == "Closest");
    CHECK(fields[6].name == "Format");
    CHECK(fields[6].value == "%3% %1$.2f");
    CHECK(fields[7].name == "Monetary Nature");
    CHECK(fields[7].value == "fiat");
    CHECK(fields[8].name == "Market Tier");
    CHECK(fields[8].value == "major");
    CHECK(fields[9].name == "Fractions Per Unit");
    CHECK(fields[9].value == "100");
    CHECK(fields[10].name == "Rounding Precision");
    CHECK(fields[10].value == "2");
    CHECK(fields[11].name == "Change Reason");
    CHECK(fields[11].value == "system.test");
    CHECK(fields[12].name == "Commentary");
    CHECK(fields[12].value == "Initial creation");
    CHECK(fields[13].name == "Flag");
    CHECK(fields[13].value == "(none)");
}

TEST_CASE("map_renders_image_id_when_set", tags) {
    auto c = make_currency();
    boost::uuids::string_generator gen;
    c.image_id = gen("01234567-89ab-cdef-0123-456789abcdef");

    const auto fields = currency_field_mapper::map(c);

    CHECK(fields[13].name == "Flag");
    CHECK(fields[13].value == "01234567-89ab-cdef-0123-456789abcdef");
}

TEST_CASE("build_versions_diffs_consecutive_pairs", tags) {
    auto v1 = make_currency();
    v1.version = 1;
    v1.modified_by = "alice";

    auto v2 = v1;
    v2.version = 2;
    v2.name = "United States Dollar";
    v2.modified_by = "bob";

    auto v3 = v2;
    v3.version = 3;
    v3.symbol = "US$";
    v3.fractions_per_unit = 1000;
    v3.modified_by = "carol";

    // Newest first, as the repository returns them.
    const auto versions = currency_field_mapper::build_versions({v3, v2, v1});

    REQUIRE(versions.size() == 3);
    CHECK(versions[0].version_number == 3);
    CHECK(versions[0].modified_by == "carol");
    CHECK(versions[0].fields == currency_field_mapper::map(v3));

    REQUIRE(versions[0].changes.entries.size() == 2);
    CHECK(versions[0].changes.entries[0].field_name == "Symbol");
    CHECK(versions[0].changes.entries[0].old_value == "$");
    CHECK(versions[0].changes.entries[0].new_value == "US$");
    CHECK(versions[0].changes.entries[1].field_name == "Fractions Per Unit");
    CHECK(versions[0].changes.entries[1].old_value == "100");
    CHECK(versions[0].changes.entries[1].new_value == "1000");

    REQUIRE(versions[1].changes.entries.size() == 1);
    CHECK(versions[1].changes.entries[0].field_name == "Name");
    CHECK(versions[1].changes.entries[0].old_value == "US Dollar");
    CHECK(versions[1].changes.entries[0].new_value ==
          "United States Dollar");

    CHECK(versions[2].changes.entries.empty());
}

TEST_CASE("build_versions_empty_input_yields_empty_output", tags) {
    CHECK(currency_field_mapper::build_versions({}).empty());
}

TEST_CASE("build_versions_from_repository_history", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto currency = generate_synthetic_currency(ctx);
    currency.change_reason_code = "system.test";
    const auto original_name = currency.name;

    currency_repository repo;
    repo.write(h.context(), currency);

    currency.name = original_name + " v2";
    repo.write(h.context(), currency);

    currency.symbol = currency.symbol + "*";
    repo.write(h.context(), currency);

    const auto history = repo.read_all(h.context(), currency.iso_code);
    BOOST_LOG_SEV(lg, debug) << "History size: " << history.size();
    REQUIRE(history.size() == 3);

    const auto versions = currency_field_mapper::build_versions(history);

    REQUIRE(versions.size() == 3);
    CHECK(versions[0].fields == currency_field_mapper::map(history[0]));

    // Newest version changed the symbol only.
    REQUIRE(versions[0].changes.entries.size() == 1);
    CHECK(versions[0].changes.entries[0].field_name == "Symbol");

    // The middle version changed the name only.
    REQUIRE(versions[1].changes.entries.size() == 1);
    CHECK(versions[1].changes.entries[0].field_name == "Name");
    CHECK(versions[1].changes.entries[0].old_value == original_name);
    CHECK(versions[1].changes.entries[0].new_value == original_name + " v2");

    // The oldest version has nothing to diff against.
    CHECK(versions[2].changes.entries.empty());
}

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
#include "ores.database/domain/outcome_code.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

ores::database::domain::outcome_args currency_args() {
    ores::database::domain::outcome_args a;
    a.entity = "currency";
    a.field = "iso_code";
    a.value = "GBP";
    return a;
}

}

using namespace ores::database::domain;

TEST_CASE("to_string names the code a reply carries", "[outcome]") {
    CHECK(to_string(outcome_code::already_exists) == "already_exists");
    CHECK(to_string(outcome_code::version_conflict) == "version_conflict");
    CHECK(to_string(outcome_code::missing_field) == "missing_field");
    CHECK(to_string(outcome_code::not_found) == "not_found");
    CHECK(to_string(outcome_code::internal_error) == "internal_error");
}

TEST_CASE("outcome_of maps each code to its coarse outcome", "[outcome]") {
    using ores::utility::domain::outcome;
    CHECK(outcome_of(outcome_code::already_exists) == outcome::conflict);
    CHECK(outcome_of(outcome_code::version_conflict) == outcome::conflict);
    CHECK(outcome_of(outcome_code::missing_field) == outcome::invalid);
    CHECK(outcome_of(outcome_code::not_found) == outcome::missing);
    CHECK(outcome_of(outcome_code::internal_error) == outcome::failed);
}

// The expected strings below are the same literals the pgtap test asserts for
// ores_outcome_<code>_fn, so a change to one renderer alone fails a gate.
TEST_CASE("describe names the record and the field", "[outcome]") {
    auto a = currency_args();

    CHECK(describe(outcome_code::not_found, a) == "The currency does not exist.");
    CHECK(describe(outcome_code::already_exists, a) ==
          "The currency with iso_code 'GBP' already exists. State the version you read to "
          "replace it, or ask for a version replace.");
    CHECK(describe(outcome_code::missing_field, a) ==
          "Invalid currency: value cannot be null or empty.");
}

TEST_CASE("describe carries both versions of a version conflict", "[outcome]") {
    auto a = currency_args();
    a.expected = "3";
    a.current = "4";

    CHECK(describe(outcome_code::version_conflict, a) ==
          "The currency with iso_code 'GBP' is at version 4, and this write states version 3.");
}

TEST_CASE("describe fills only the names the message carries", "[outcome]") {
    auto a = currency_args();
    a.field = "day_counter";
    a.limit = "1000";

    CHECK(describe(outcome_code::order_not_supported, a) ==
          "This read of the currency cannot order by day_counter.");
    CHECK(describe(outcome_code::filter_too_large, a) ==
          "The filter on day_counter lists more than 1000 values.");
}

TEST_CASE("refuse names the code and the outcome a caller branches on", "[outcome]") {
    using ores::utility::domain::outcome;

    const auto bare = refuse(outcome_code::version_conflict);
    CHECK(bare.outcome == outcome::conflict);
    CHECK(bare.code == "version_conflict");
    CHECK(bare.message.empty());

    auto a = currency_args();
    a.field = "iso_code";
    const auto described = refuse(outcome_code::not_found, a);
    CHECK(described.outcome == outcome::missing);
    CHECK(described.code == "not_found");
    CHECK(described.message == "The currency does not exist.");
}

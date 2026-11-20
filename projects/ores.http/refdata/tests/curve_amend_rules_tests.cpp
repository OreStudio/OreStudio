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
#include "ores.http/routes/refdata/curve_amend_rules.hpp"
#include "ores.logging/logger.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>

using ores::http::routes::refdata::amend::is_stale;
using ores::http::routes::refdata::amend::refusal_of;
using ores::http::routes::refdata::document::cell;
using ores::http::routes::value_fields;
using namespace ores::logging;

TEST_CASE("a_curve_leaf_refuses_a_value_outside_its_set", "[curve_amend_rules]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the refusal of a curve leaf's value";

    auto enumerated = value_fields{};
    enumerated.value_set = "curve_day_counters";
    enumerated.choices = {{"ACT/360", "ACT/360"}, {"30/360", "30/360"}};
    CHECK(refusal_of(enumerated, "Day counter", true, cell("ACT/365")) ==
          std::optional<std::string>(
              "Day counter must be one of ACT/360, 30/360, and \"ACT/365\" is not."));
    CHECK_FALSE(refusal_of(enumerated, "Day counter", true, cell("ACT/360")).has_value());

    auto ranged = value_fields{};
    ranged.minimum = 0.0;
    ranged.maximum = 1.0;
    CHECK(refusal_of(ranged, "Tolerance", true, cell("-1")) ==
          std::optional<std::string>("Tolerance must be at least 0."));
    CHECK(refusal_of(ranged, "Tolerance", true, cell("2")) ==
          std::optional<std::string>("Tolerance must be at most 1."));
    CHECK(refusal_of(ranged, "Tolerance", true, cell("many")) ==
          std::optional<std::string>("Tolerance must be a number."));
    CHECK_FALSE(refusal_of(ranged, "Tolerance", true, cell("0.5")).has_value());

    auto pattern = value_fields{};
    pattern.pattern = "[A-Z]{3}";
    CHECK(refusal_of(pattern, "Currency", true, cell("eur")) ==
          std::optional<std::string>("Currency must match [A-Z]{3}."));

    auto required = value_fields{};
    CHECK(refusal_of(required, "Currency", true, cell()) ==
          std::optional<std::string>("Currency is required."));
    CHECK(refusal_of(required, "Currency", true, cell("")) ==
          std::optional<std::string>("Currency is required."));
    CHECK_FALSE(refusal_of(required, "Currency", false, cell()).has_value());
}

TEST_CASE("a_curve_amendment_reads_the_version_its_service_refused", "[curve_amend_rules]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the words a stale save is refused with";

    CHECK(is_stale("version conflict: the row moved on"));
    CHECK(is_stale("A version_conflict stopped the save"));
    CHECK(is_stale("The version on the row does not match the one stated"));
    CHECK_FALSE(is_stale("The currency is not one the set holds"));
    CHECK_FALSE(is_stale(""));
}

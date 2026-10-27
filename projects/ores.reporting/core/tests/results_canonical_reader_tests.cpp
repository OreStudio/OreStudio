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
#include "ores.reporting.core/results/canonical_reader.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

using ores::reporting::results::read_canonical_csv;

const std::string header =
    "subject_type,subject_id,measure,currency,counterparty_id,figure,source\n";

}

TEST_CASE("a well formed file is read row by row", "[results][canonical_reader]") {
    const auto outcome = read_canonical_csv(header + "trade,T1,npv,GBP,C1,12.5,npv.csv\n"
                                                    "trade,T1,delta,GBP,C1,-3,npv.csv\n");

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.rows.size() == 2);
    CHECK(outcome.rows[0].subject_id == "T1");
    CHECK(outcome.rows[0].measure == "npv");
    CHECK(outcome.rows[0].currency == "GBP");
    CHECK(outcome.rows[0].figure == 12.5);
    CHECK(outcome.rows[0].counterparty_id == "C1");
    CHECK(outcome.rows[0].source == "npv.csv");
    CHECK(outcome.rows[1].figure == -3.0);
}

TEST_CASE("a file whose header is not the contract is refused whole", "[results][canonical_reader]") {
    const auto outcome = read_canonical_csv("a,b,c\n1,2,3\n");

    REQUIRE(outcome.rows.empty());
    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals[0].find("header") != std::string::npos);
}

TEST_CASE("a row with the wrong number of fields is refused by line", "[results][canonical_reader]") {
    const auto outcome = read_canonical_csv(header + "trade,T1,npv,GBP,C1,1.0\n");

    REQUIRE(outcome.rows.empty());
    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals[0].find("line 2") != std::string::npos);
    CHECK(outcome.refusals[0].find("fields, not seven") != std::string::npos);
}

TEST_CASE("a row with no subject or a figure that is not one is refused",
          "[results][canonical_reader]") {
    const auto outcome =
        read_canonical_csv(header + "trade,,npv,GBP,C1,1.0,npv.csv\ntrade,T2,npv,GBP,C1,n/a,npv.csv\n");

    REQUIRE(outcome.rows.empty());
    REQUIRE(outcome.refusals.size() == 2);
    CHECK(outcome.refusals[0].find("no subject") != std::string::npos);
    CHECK(outcome.refusals[1].find("not a figure") != std::string::npos);
}

TEST_CASE("an empty file is refused rather than read as no results", "[results][canonical_reader]") {
    const auto outcome = read_canonical_csv("");

    CHECK(outcome.rows.empty());
    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals[0].find("empty") != std::string::npos);
}

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
#include "ores.ore.core/results/canonical_rows.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string tags("[ore][results][canonical_rows]");

using ores::ore::results::canonical_csv;
using ores::ore::results::marshal_npv_csv;

}

TEST_CASE("a row states one row for each measure it carries", tags) {
    const std::string csv = "#TradeId,TradeType,NPV,NpvCurrency,NPV(Base),Notional,"
                            "NotionalCurrency\nT1,Swap,12.5,GBP,12.5,1000,GBP\n";

    const auto outcome = marshal_npv_csv(csv, "npv.csv");

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.rows.size() == 3);
    CHECK(outcome.rows[0].subject_id == "T1");
    CHECK(outcome.rows[0].subject_type == "trade");
    CHECK(outcome.rows[0].measure == "npv");
    CHECK(outcome.rows[0].currency == "GBP");
    CHECK(outcome.rows[0].figure == 12.5);
    CHECK(outcome.rows[0].source == "npv.csv");
    CHECK(outcome.rows[1].measure == "npv_base");
    CHECK(outcome.rows[2].measure == "notional");
    CHECK(outcome.rows[2].currency == "GBP");
}

TEST_CASE("a file whose headings are unknown is refused by name", tags) {
    const auto outcome = marshal_npv_csv("#Foo,Bar\n1,2\n", "npv.csv");

    REQUIRE(outcome.rows.empty());
    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals[0].find("Foo") != std::string::npos);
    CHECK(outcome.refusals[0].find("npv.csv") != std::string::npos);
}

TEST_CASE("a row with no trade id or no figure is refused", tags) {
    const std::string csv = "#TradeId,NPV,NpvCurrency\n,1.0,GBP\nT2,n/a,GBP\nT3,3.0,GBP\n";

    const auto outcome = marshal_npv_csv(csv, "npv.csv");

    REQUIRE(outcome.rows.size() == 1);
    CHECK(outcome.rows[0].subject_id == "T3");
    REQUIRE(outcome.refusals.size() == 2);
    CHECK(outcome.refusals[0].find("no trade id") != std::string::npos);
    CHECK(outcome.refusals[1].find("not a figure") != std::string::npos);
}

TEST_CASE("a figure without its currency is refused", tags) {
    const auto outcome = marshal_npv_csv("#TradeId,NPV,NpvCurrency\nT1,1.0,\n", "npv.csv");

    REQUIRE(outcome.rows.empty());
    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals[0].find("no currency") != std::string::npos);
}

TEST_CASE("the canonical csv states its header and a field is quoted when it must be", tags) {
    const std::string csv = "#TradeId,NPV,NpvCurrency\nT1,1.5,GBP\n";

    const auto outcome = marshal_npv_csv(csv, "npv.csv");
    const auto text = canonical_csv(outcome.rows);

    CHECK(text.rfind("subject_type,subject_id,measure,currency,counterparty_id,figure,source\n",
                     0) == 0);
    CHECK(text.find("trade,T1,npv,GBP,,1.5,npv.csv\n") != std::string::npos);
}

TEST_CASE("a file that states no notional projects only the measures it has", tags) {
    const std::string csv = "#TradeId,NPV,NpvCurrency\nT1,1.5,GBP\nT2,2.5,GBP\n";

    const auto outcome = marshal_npv_csv(csv, "npv.csv");

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.rows.size() == 2);
    CHECK(outcome.rows[0].measure == "npv");
    CHECK(outcome.rows[1].measure == "npv");
}

TEST_CASE("a negative figure is a figure", tags) {
    const auto outcome = marshal_npv_csv("#TradeId,NPV,NpvCurrency\nT1,-12.25,GBP\n", "npv.csv");

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.rows.size() == 1);
    CHECK(outcome.rows[0].figure == -12.25);
}

TEST_CASE("a quoted field holding a comma stays one field", tags) {
    const std::string csv = "#TradeId,NPV,NpvCurrency,CounterParty\n"
                            "T1,1.5,GBP,\"Northwind Capital, Ltd\"\n";

    const auto outcome = marshal_npv_csv(csv, "npv.csv");

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.rows.size() == 1);
    CHECK(outcome.rows[0].counterparty_id == "Northwind Capital, Ltd");
}

TEST_CASE("a file with dos line endings and a byte order mark still reads", tags) {
    const std::string csv = "\xEF\xBB\xBF#TradeId,NPV,NpvCurrency\r\nT1,1.5,GBP\r\nT2,2.5,GBP\r\n";

    const auto outcome = marshal_npv_csv(csv, "npv.csv");

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.rows.size() == 2);
    CHECK(outcome.rows[1].subject_id == "T2");
}

TEST_CASE("a row with more fields than the header names is still read by name", tags) {
    const std::string csv = "#TradeId,NPV,NpvCurrency\nT1,1.5,GBP,extra,more\n";

    const auto outcome = marshal_npv_csv(csv, "npv.csv");

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.rows.size() == 1);
    CHECK(outcome.rows[0].subject_id == "T1");
}

TEST_CASE("an empty file is refused by name", tags) {
    const auto outcome = marshal_npv_csv("", "npv.csv");

    CHECK(outcome.rows.empty());
    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals[0].find("npv.csv") != std::string::npos);
}

TEST_CASE("blank lines between rows are not rows", tags) {
    const std::string csv = "#TradeId,NPV,NpvCurrency\n\nT1,1.5,GBP\n\n";

    const auto outcome = marshal_npv_csv(csv, "npv.csv");

    REQUIRE(outcome.refusals.empty());
    CHECK(outcome.rows.size() == 1);
}

TEST_CASE("a row with a figure but no currency is refused with its line", tags) {
    const auto outcome =
        marshal_npv_csv("#TradeId,NPV,NpvCurrency\nT1,1.5,GBP\nT2,2.5,\n", "npv.csv");

    REQUIRE(outcome.rows.size() == 1);
    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals[0].find("line 3") != std::string::npos);
    CHECK(outcome.refusals[0].find("no currency") != std::string::npos);
}

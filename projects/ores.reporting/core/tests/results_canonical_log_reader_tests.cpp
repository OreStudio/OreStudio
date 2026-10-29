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
#include "ores.reporting.core/results/canonical_log_reader.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

using ores::reporting::results::read_canonical_log_csv;

const std::string log_header =
    "ordinal,severity,scope,message,subject,subject_id,cause,detail,event_time,source\n";

}

TEST_CASE("a well formed log file is read row by row", "[results][canonical_log_reader]") {
    const auto outcome = read_canonical_log_csv(
        log_header +
        "1,error,Curve,yield curve building failed,,,HKD-HONIA,,2026-Oct-27 19:42:07,log.txt\n"
        "2,notice,Report,done,,,,,2026-Oct-27 19:42:08,log.txt\n");

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.rows.size() == 2);
    CHECK(outcome.rows[0].severity == "error");
    CHECK(outcome.rows[0].scope == "Curve");
    CHECK(outcome.rows[0].message == "yield curve building failed");
    CHECK(outcome.rows[0].cause == "HKD-HONIA");
    CHECK(outcome.rows[0].detail.empty());
    CHECK(outcome.rows[0].event_time == "2026-Oct-27 19:42:07");
    CHECK(outcome.rows[0].source == "log.txt");
    CHECK(outcome.rows[1].severity == "notice");
}

TEST_CASE("a log file whose header is not the contract is refused whole",
          "[results][canonical_log_reader]") {
    const auto outcome = read_canonical_log_csv("a,b,c\n1,2,3\n");

    CHECK(outcome.rows.empty());
    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals[0].find("header") != std::string::npos);
}

TEST_CASE("a log row with the wrong number of fields is refused by line",
          "[results][canonical_log_reader]") {
    const auto outcome = read_canonical_log_csv(log_header + "1,error,Curve,failed\n");

    CHECK(outcome.rows.empty());
    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals[0].find("record 2") != std::string::npos);
    CHECK(outcome.refusals[0].find("fields, not ten") != std::string::npos);
}

TEST_CASE("a log row with no severity or no message is refused",
          "[results][canonical_log_reader]") {
    const auto outcome =
        read_canonical_log_csv(log_header + "1,,Curve,failed,,,,,2026-Oct-27,log.txt\n"
                                            "2,error,Curve,,,,,,2026-Oct-27,log.txt\n");

    CHECK(outcome.rows.empty());
    REQUIRE(outcome.refusals.size() == 2);
    CHECK(outcome.refusals[0].find("no severity or no message") != std::string::npos);
}

TEST_CASE("a message written over two lines is one record", "[results][canonical_log_reader]") {
    const auto outcome = read_canonical_log_csv(
        log_header +
        "1,error,Trade,\"first line\nsecond line\",trade,T1,,,2026-Oct-27 19:42:07,log.txt\n"
        "2,notice,Report,done,,,,,2026-Oct-27 19:42:08,log.txt\n");

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.rows.size() == 2);
    CHECK(outcome.rows[0].message == "first line\nsecond line");
    CHECK(outcome.rows[1].message == "done");
}

TEST_CASE("a quote that never closes is refused rather than half read",
          "[results][canonical_log_reader]") {
    const auto outcome = read_canonical_log_csv(log_header + "1,error,Trade,\"never closed\n");

    CHECK(outcome.rows.empty());
    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals[0].find("never closes") != std::string::npos);
}

TEST_CASE("a quoted message keeps its comma and its quote", "[results][canonical_log_reader]") {
    const auto outcome = read_canonical_log_csv(
        log_header +
        "1,error,Trade,\"he said \"\"hi\"\", twice\",trade,T1,,,2026-Oct-27 19:42:07,log.txt\n");

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.rows.size() == 1);
    CHECK(outcome.rows[0].message == "he said \"hi\", twice");
    CHECK(outcome.rows[0].subject == "trade");
    CHECK(outcome.rows[0].subject_id == "T1");
}

TEST_CASE("an empty log file is refused rather than read as no events",
          "[results][canonical_log_reader]") {
    const auto outcome = read_canonical_log_csv("");

    CHECK(outcome.rows.empty());
    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals[0].find("empty") != std::string::npos);
}

TEST_CASE("a row keeps the detail and the time the node stated",
          "[results][canonical_log_reader]") {
    const auto outcome = read_canonical_log_csv(
        log_header +
        "1,error,Trade,did not build,,,,\"{\"\"exceptionType\"\":\"\"Error building trade\"\"}\","
        "2026-Oct-27 19:42:07,log.txt\n");

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.rows.size() == 1);
    CHECK(outcome.rows[0].detail == R"({"exceptionType":"Error building trade"})");
    CHECK(outcome.rows[0].event_time == "2026-Oct-27 19:42:07");
    CHECK(outcome.rows[0].source == "log.txt");
}

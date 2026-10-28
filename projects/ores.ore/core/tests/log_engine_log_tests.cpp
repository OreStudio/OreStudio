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
#include "ores.ore.core/log/engine_log.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

using ores::ore::log::field_of;
using ores::ore::log::is_memory_report;
using ores::ore::log::parse_engine_log_json;
using ores::ore::log::parse_engine_log_text;

const std::string curve_line =
    "ALERT    [2026-Oct-27 19:42:07.139446]          (OREData/ored/utilities/log.cpp:214) : "
    "StructuredMessage { \"category\": \"Error\", \"group\": \"Curve\", \"message\": "
    "\"yield curve building failed for curve(s) Yield/HKD/HKD1D\", \"sub_fields\": [ "
    "{ \"name\": \"curveId\", \"value\": \"[IndexCurve(HKD-HONIA)]\" } ] }";

const std::string trade_line =
    "ALERT    [2026-Oct-27 19:42:07.139606]          (OREData/ored/utilities/log.cpp:214) : "
    "StructuredMessage { \"category\": \"Warning\", \"group\": \"Trade\", \"message\": "
    "\"Cannot build all required curves!\", \"sub_fields\": [ "
    "{ \"name\": \"tradeId\", \"value\": \"1d61bfaf-2015-5c23-ad85-85ebe6d9d4c0\" }, "
    "{ \"name\": \"tradeType\", \"value\": \"Bond\" } ] }";

const std::string parameters_line =
    "NOTICE   [2026-Oct-27 20:13:45.960950]    "
    "(OREAnalytics/orea/app/parameters.cpp:144) : group = setup : nThreads = 1";

}

TEST_CASE("a text log yields one event per line, whatever the severity", "[ore][log]") {
    const auto events =
        parse_engine_log_text(parameters_line + "\n" + curve_line + "\n", "log.txt");

    REQUIRE(events.size() == 2);
    CHECK(events[0].ordinal == 1);
    CHECK(events[0].severity == "notice");
    CHECK(events[0].event_time == "2026-Oct-27 20:13:45.960950");
    CHECK(events[0].source == "log.txt");
    CHECK(events[1].severity == "error");
    CHECK(events[1].scope == "Curve");
}

TEST_CASE("a message keeps its own separator", "[ore][log]") {
    const auto events = parse_engine_log_text(parameters_line + "\n", "log.txt");

    REQUIRE(events.size() == 1);
    CHECK(events[0].message == "group = setup : nThreads = 1");
}

TEST_CASE("a structured line takes its group, subject and cause", "[ore][log]") {
    const auto events = parse_engine_log_text(curve_line + "\n" + trade_line + "\n", "log.txt");

    REQUIRE(events.size() == 2);
    CHECK(events[0].message == "yield curve building failed for curve(s) Yield/HKD/HKD1D");
    CHECK(events[0].cause == "HKD-HONIA");
    CHECK(events[1].severity == "warning");
    CHECK(events[1].scope == "Trade");
    CHECK(events[1].subject == "trade");
    CHECK(events[1].subject_id == "1d61bfaf-2015-5c23-ad85-85ebe6d9d4c0");
}

TEST_CASE("a warning is a warning and an alert is an error", "[ore][log]") {
    const auto events =
        parse_engine_log_text("WARNING  [2026-Oct-27 20:00:00.000000] (a.cpp:1) : careful\n"
                              "ERROR    [2026-Oct-27 20:00:00.000000] (a.cpp:2) : broke\n"
                              "ALERT    [2026-Oct-27 20:00:00.000000] (a.cpp:3) : broke loudly\n"
                              "DEBUG    [2026-Oct-27 20:00:00.000000] (a.cpp:4) : detail\n"
                              "INFO     [2026-Oct-27 20:00:00.000000] (a.cpp:5) : news\n",
                              "log.txt");

    REQUIRE(events.size() == 5);
    CHECK(events[0].severity == "warning");
    CHECK(events[1].severity == "error");
    CHECK(events[2].severity == "error");
    CHECK(events[3].severity == "debug");
    CHECK(events[4].severity == "info");
}

TEST_CASE("a line the reader cannot structure is kept as what it says", "[ore][log]") {
    const auto events = parse_engine_log_text(
        "ALERT [2026-Oct-27 20:00:00.000000] (x.cpp:1) : StructuredMessage { not json\n",
        "log.txt");

    REQUIRE(events.size() == 1);
    CHECK(events[0].severity == "error");
    CHECK(events[0].message.find("not json") != std::string::npos);
}

TEST_CASE("blank lines are not events", "[ore][log]") {
    CHECK(parse_engine_log_text("\n\n   \n", "log.txt").empty());
    CHECK(parse_engine_log_text("", "log.txt").empty());
    CHECK(parse_engine_log_json("", "log_structured.json").empty());
}

TEST_CASE("a text log with dos line endings keeps its messages whole", "[ore][log]") {
    const auto events =
        parse_engine_log_text(parameters_line + "\r\n" + curve_line + "\r\n", "log.txt");

    REQUIRE(events.size() == 2);
    CHECK(events[0].message == "group = setup : nThreads = 1");
}

TEST_CASE("a byte order mark does not become part of the first severity", "[ore][log]") {
    const auto events =
        parse_engine_log_text(std::string("\xEF\xBB\xBF") + parameters_line + "\n", "log.txt");

    REQUIRE(events.size() == 1);
    CHECK(events[0].severity == "notice");
}

TEST_CASE("a line with no time and no file still states its message", "[ore][log]") {
    const auto events = parse_engine_log_text("WARNING something is off\n", "log.txt");

    REQUIRE(events.size() == 1);
    CHECK(events[0].severity == "warning");
    CHECK(events[0].message == "WARNING something is off");
}

TEST_CASE("a structured log is read by the same rules", "[ore][log]") {
    const std::string json_log =
        "{\"category\": \"Error\", \"group\": \"Trade\", \"message\": \"did not find object\", "
        "\"sub_fields\": [{\"name\": \"tradeId\", \"value\": "
        "\"033b4053-fe1d-5901-bc2f-7d95afe91f03\"}]}"
        "\n"
        "{\"category\": \"Info\", \"group\": \"Report\", \"message\": \"done\"}\n";

    const auto events = parse_engine_log_json(json_log, "log_structured.json");

    REQUIRE(events.size() == 2);
    CHECK(events[0].severity == "error");
    CHECK(events[0].scope == "Trade");
    CHECK(events[0].subject_id == "033b4053-fe1d-5901-bc2f-7d95afe91f03");
    CHECK(events[1].severity == "info");
    CHECK(events[1].scope == "Report");
}

TEST_CASE("a structured log line that is not an object is kept", "[ore][log]") {
    const auto events = parse_engine_log_json("not json at all\n", "log_structured.json");

    REQUIRE(events.size() == 1);
    CHECK(events[0].message == "not json at all");
}

TEST_CASE("an unknown category reads as notice rather than as nothing", "[ore][log]") {
    const auto events = parse_engine_log_json(
        "{\"category\": \"Trace\", \"group\": \"Report\", \"message\": \"odd\"}\n",
        "log_structured.json");

    REQUIRE(events.size() == 1);
    CHECK(events[0].severity == "notice");
}

TEST_CASE("the canonical log csv states its header and quotes what it must", "[ore][log]") {
    using ores::ore::log::canonical_log_csv;

    const auto events = parse_engine_log_text(curve_line + "\n", "log.txt");
    const auto text = canonical_log_csv(events);

    CHECK(text.rfind("ordinal,severity,scope,message,subject,subject_id,cause,event_time,source\n",
                     0) == 0);
    CHECK(text.find("1,error,Curve,") != std::string::npos);
}

TEST_CASE("a message with a comma is quoted so the row keeps its fields", "[ore][log]") {
    using ores::ore::log::canonical_log_csv;

    const auto events = parse_engine_log_text(
        "NOTICE [2026-Oct-27 20:00:00.000000] (a.cpp:1) : one, two, three\n", "log.txt");
    const auto text = canonical_log_csv(events);

    CHECK(text.find("\"one, two, three\"") != std::string::npos);
}

TEST_CASE("the detail states the failure and never the subject", "[ore][log]") {
    // The fixture states only a trade type, which describes the trade and is not
    // what failed, so the failure's own detail is empty and the field stays in
    // the raw fields.
    const auto events = parse_engine_log_text(trade_line + "\n", "log.txt");

    REQUIRE(events.size() == 1);
    CHECK(events[0].detail.empty());
    CHECK(events[0].fields.find("tradeType") != std::string::npos);
}

TEST_CASE("every sub-field the engine stated is available by name", "[ore][log]") {
    const auto events = parse_engine_log_text(curve_line + "\n" + trade_line + "\n", "log.txt");

    REQUIRE(events.size() == 2);
    CHECK(events[0].fields.find("IndexCurve(HKD-HONIA)") != std::string::npos);
    CHECK(events[1].fields.find("tradeType") != std::string::npos);
    CHECK(events[1].fields.find("Bond") != std::string::npos);
}

TEST_CASE("a message with a newline is quoted so the row stays one record", "[ore][log]") {
    using ores::ore::log::canonical_log_csv;

    ores::ore::log::engine_log_event event;
    event.ordinal = 1;
    event.message = "one\ntwo";
    const auto text = canonical_log_csv({event});

    CHECK(text.find("\"one\ntwo\"") != std::string::npos);
}

TEST_CASE("a message with a carriage return is quoted so the row keeps its fields", "[ore][log]") {
    using ores::ore::log::canonical_log_csv;

    ores::ore::log::engine_log_event event;
    event.ordinal = 1;
    event.message = "one\rtwo";
    const auto text = canonical_log_csv({event});

    CHECK(text.find("\"one\rtwo\"") != std::string::npos);
}

TEST_CASE("a structured log object that states no message keeps its line", "[ore][log]") {
    const auto events = parse_engine_log_json("{\"category\": \"Info\", \"group\": \"Report\"}\n",
                                              "log_structured.json");

    REQUIRE(events.size() == 1);
    CHECK(events[0].severity == "info");
    CHECK(events[0].message.find("Report") != std::string::npos);
}

TEST_CASE("a line the engine wrote states its level, its time and where it logged", "[ore][log]") {
    const auto events = parse_engine_log_text(
        "WARNING    [2024-Dec-31 23:59:59.123456]    (risk/engine.cpp:99) : curve not found\n",
        "log.txt");

    REQUIRE(events.size() == 1);
    CHECK(events[0].engine_format);
    CHECK(events[0].level_word == "WARNING");
    CHECK(events[0].severity == "warning");
    CHECK(events[0].location == "risk/engine.cpp:99");
    CHECK(events[0].event_time == "2024-Dec-31 23:59:59.123456");
    REQUIRE(events[0].occurred_at.has_value());
    const auto since_epoch = std::chrono::duration_cast<std::chrono::microseconds>(
        events[0].occurred_at->time_since_epoch());
    CHECK(since_epoch.count() % 1000000 == 123456);
}

TEST_CASE("a line the engine did not write in its own shape says so", "[ore][log]") {
    // A line with no time and no location is text, not a line of the engine's.
    const auto events = parse_engine_log_text("WARNING something is off\n", "log.txt");

    REQUIRE(events.size() == 1);
    CHECK_FALSE(events[0].engine_format);
    CHECK(events[0].message == "WARNING something is off");
}

TEST_CASE("a fractional second is scaled to microseconds", "[ore][log]") {
    // The engine does not always write six digits: 123 means 123000 microseconds.
    const auto events = parse_engine_log_text(
        "NOTICE    [2024-Jan-01 00:00:01.123]    (x.cpp:1) : msg\n", "log.txt");

    REQUIRE(events.size() == 1);
    REQUIRE(events[0].occurred_at.has_value());
    const auto since_epoch = std::chrono::duration_cast<std::chrono::microseconds>(
        events[0].occurred_at->time_since_epoch());
    CHECK(since_epoch.count() % 1000000 == 123000);
}

TEST_CASE("a time the engine did not write as one is not a value", "[ore][log]") {
    const auto events =
        parse_engine_log_text("NOTICE    [not-a-date]    (x.cpp:1) : msg\n", "log.txt");

    REQUIRE(events.size() == 1);
    CHECK_FALSE(events[0].occurred_at.has_value());
    CHECK_FALSE(events[0].engine_format);
}

TEST_CASE("the engine's memory report reads as a measurement, not a warning", "[ore][log]") {
    const auto events = parse_engine_log_text(
        "WARNING  [2026-Oct-11 09:54:13.735791]    (OREAnalytics/orea/app/oreapp.cpp:257) : "
        "Starting OREApp::analytics(): 52359168|52514816 (peakMemoryUsage (bytes) | memoryUsage "
        "(bytes))\n",
        "log.txt");

    REQUIRE(events.size() == 1);
    CHECK(is_memory_report(events[0]));
    CHECK(events[0].message ==
          "Starting OREApp::analytics(): 52359168|52514816 (peakMemoryUsage (bytes) | memoryUsage "
          "(bytes))");
}

TEST_CASE("a warning that mentions memory is still a warning", "[ore][log]") {
    const auto events = parse_engine_log_text(
        "WARNING  [2026-Oct-11 09:54:13.735791]    (ored/main.cpp:10) : low memory, "
        "peakMemoryUsage is high\n",
        "log.txt");

    REQUIRE(events.size() == 1);
    CHECK_FALSE(is_memory_report(events[0]));
}

TEST_CASE("every sub-field is read by the name the engine gave it", "[ore][log]") {
    const auto events = parse_engine_log_text(trade_line + "\n", "log.txt");

    REQUIRE(events.size() == 1);
    CHECK(field_of(events[0], "tradeId") == "1d61bfaf-2015-5c23-ad85-85ebe6d9d4c0");
    CHECK(field_of(events[0], "tradeType") == "Bond");
    CHECK(field_of(events[0], "curveId").empty());
}

TEST_CASE("a structured message takes the severity it states, and keeps its line's when it "
          "states none",
          "[ore][log]") {
    const auto with_category = parse_engine_log_json(
        "{\"category\": \"Warning\", \"group\": \"Trade\", \"message\": \"careful\"}\n",
        "log_structured.json");
    const auto without_category = parse_engine_log_text(
        "ALERT [2026-Oct-27 20:00:00.000000] (x.cpp:1) : StructuredMessage { \"message\": "
        "\"broke\" }\n",
        "log.txt");

    REQUIRE(with_category.size() == 1);
    REQUIRE(without_category.size() == 1);
    CHECK(with_category[0].severity == "warning");
    CHECK(without_category[0].severity == "error");
}

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
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.core/service/cut_resolution_plan.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <format>
#include <string>
#include <vector>

// The decision these pin: which body a report's selection resolves to, given the
// date the report covers, its scope and the cuts already taken. The resolution
// is a function so that the preview and the run read one answer: a preview
// computed by a second code path would be a preview of that path.

using namespace ores::reporting;
using namespace ores::reporting::service;

namespace {

using namespace std::chrono;
using ores::platform::time::datetime;

const auto covered = datetime::from_iso8601_utc("2026-10-06T00:00:00Z");

boost::uuids::uuid uuid_of(const std::string& text) {
    return boost::uuids::string_generator()(text);
}

domain::cut_kind lookup(const std::string& code,
                        const std::string& family,
                        const std::string& rule,
                        std::optional<std::string> approval_kind = std::nullopt) {
    domain::cut_kind k;
    k.code = code;
    k.family_code = family;
    k.resolution_rule_code = rule;
    k.approval_kind_code = std::move(approval_kind);
    k.retention_months = 25;
    k.retention_destination_code = "cold";
    return k;
}

std::vector<domain::cut_kind> the_seven_kinds() {
    return {
        lookup("EOD", "end_of_day", "same_day_close", "reporting.eod_cut_authorisation"),
        lookup("EOM", "end_of_day", "last_business_day_of_month_close",
               "reporting.eod_cut_authorisation"),
        lookup("EOY", "end_of_day", "last_business_day_of_year_close",
               "reporting.eod_cut_authorisation"),
        lookup("SOD", "start_of_day", "same_day_open"),
        lookup("IPV", "verification", "cut_it_is_built_on", "reporting.ipv_cut_authorisation"),
        lookup("FLASH", "intraday", "the_cut_named"),
        lookup("LIVE", "consumer", "run_instant"),
    };
}

domain::cut cut_of(const std::string& kind_code,
                   const std::string& instant_text,
                   int ordinal = 1) {
    domain::cut cut;
    cut.id = uuid_of(std::format("00000000-0000-0000-0000-{:012}", ordinal));
    cut.kind_code = kind_code;
    cut.scope_code = "ACME-LONDON";
    cut.business_centre_code = "LDN";
    cut.timepoint = datetime::from_iso8601_utc(instant_text);
    cut.timepoint_timezone = "Europe/London";
    cut.take_ordinal = ordinal;
    cut.taken_by = "a.reyes";
    cut.taken_at = cut.timepoint;
    cut.requires_authorisation = kind_code == "EOD" || kind_code == "EOM" ||
                                 kind_code == "EOY" || kind_code == "IPV";
    cut.approved_by = "m.silva";
    cut.approved_at = cut.taken_at;
    cut.retention_months = 25;
    cut.retention_destination_code = "cold";
    return cut;
}

/** A cut whose authorisation has not been recorded yet. */
domain::cut waiting_cut(const std::string& kind_code, const std::string& instant_text) {
    auto cut = cut_of(kind_code, instant_text);
    cut.approved_by.clear();
    cut.approved_at.reset();
    cut.approval_request_id = uuid_of("aaaaaaaa-0000-0000-0000-000000000001");
    return cut;
}

cut_resolution_context context_of(std::chrono::system_clock::time_point covered_date = covered) {
    cut_resolution_context ctx;
    ctx.covered_date = covered_date;
    ctx.scope_code = "ACME-LONDON";
    ctx.business_centre_code = "LDN";
    ctx.party_id = uuid_of("bbbbbbbb-0000-0000-0000-000000000001");
    ctx.timepoint_timezone = "Europe/London";
    ctx.run_instant = datetime::from_iso8601_utc("2026-10-07T07:40:00Z");
    ctx.taken_by = "svc.reporting";
    ctx.preview = true;
    return ctx;
}

cut_selection select(const std::string& kind_code) {
    cut_selection selection;
    selection.kind_code = kind_code;
    return selection;
}

}

TEST_CASE("the last business day of a month skips the weekend", "[service][cut][resolution]") {
    // 2026-10-31 is a Saturday, 2026-05-31 a Sunday, and 2026-12-31 a Thursday.
    CHECK(datetime::to_iso8601_date(last_business_day_of_month(year(2026) / October)) ==
          "2026-10-30");
    CHECK(datetime::to_iso8601_date(last_business_day_of_month(year(2026) / May)) == "2026-05-29");
    CHECK(datetime::to_iso8601_date(last_business_day_of_month(year(2026) / December)) ==
          "2026-12-31");
}

TEST_CASE("the last business day of a year is the last business day of December",
          "[service][cut][resolution]") {
    CHECK(datetime::to_iso8601_date(last_business_day_of_year(year(2027))) == "2027-12-31");
}

TEST_CASE("an end of day selection resolves to the close of the covered date",
          "[service][cut][resolution]") {
    const auto candidates = std::vector<domain::cut>{cut_of("EOD", "2026-10-06T17:00:00Z")};

    const auto resolution =
        resolve_cut_selection(select("EOD"), the_seven_kinds(), candidates, context_of());

    REQUIRE(resolution.ok());
    REQUIRE(resolution.cut.has_value());
    CHECK(resolution.cut->kind_code == "EOD");
}

TEST_CASE("an end of day selection prices from the month end body on a month's last business day",
          "[service][cut][resolution]") {
    // 2026-10-30 is the last business day of October, so both a plain close and
    // the month end cut stand, and the more specific occasion is the one meant.
    const auto month_end = datetime::from_iso8601_utc("2026-10-30T00:00:00Z");
    const auto candidates = std::vector<domain::cut>{
        cut_of("EOD", "2026-10-30T17:00:00Z"),
        cut_of("EOM", "2026-10-30T17:00:00Z", 2)};

    const auto resolution = resolve_cut_selection(
        select("EOD"), the_seven_kinds(), candidates, context_of(month_end));

    REQUIRE(resolution.cut.has_value());
    CHECK(resolution.cut->kind_code == "EOM");
}

TEST_CASE("an end of day selection falls back to the plain close where no month end stands",
          "[service][cut][resolution]") {
    const auto month_end = datetime::from_iso8601_utc("2026-10-30T00:00:00Z");
    const auto candidates = std::vector<domain::cut>{cut_of("EOD", "2026-10-30T17:00:00Z")};

    const auto resolution = resolve_cut_selection(
        select("EOD"), the_seven_kinds(), candidates, context_of(month_end));

    REQUIRE(resolution.cut.has_value());
    CHECK(resolution.cut->kind_code == "EOD");
}

TEST_CASE("a month end selection refuses a plain close of that day",
          "[service][cut][resolution]") {
    const auto month_end = datetime::from_iso8601_utc("2026-10-30T00:00:00Z");
    const auto candidates = std::vector<domain::cut>{cut_of("EOD", "2026-10-30T17:00:00Z")};

    const auto resolution = resolve_cut_selection(
        select("EOM"), the_seven_kinds(), candidates, context_of(month_end));

    CHECK_FALSE(resolution.ok());
    CHECK_FALSE(resolution.refusal.empty());
}

TEST_CASE("a month end selection resolves to that month's last business day",
          "[service][cut][resolution]") {
    // The report covers the 20th; the month end cut is taken ten days later.
    const auto covers_twentieth = datetime::from_iso8601_utc("2026-10-20T00:00:00Z");
    const auto candidates = std::vector<domain::cut>{cut_of("EOM", "2026-10-30T17:00:00Z")};

    const auto resolution = resolve_cut_selection(
        select("EOM"), the_seven_kinds(), candidates, context_of(covers_twentieth));

    REQUIRE(resolution.cut.has_value());
    CHECK(resolution.cut->kind_code == "EOM");
}

TEST_CASE("a year end selection resolves to that year's last business day",
          "[service][cut][resolution]") {
    const auto covers_june = datetime::from_iso8601_utc("2026-06-15T00:00:00Z");
    const auto candidates = std::vector<domain::cut>{cut_of("EOY", "2026-12-31T17:00:00Z")};

    const auto resolution = resolve_cut_selection(
        select("EOY"), the_seven_kinds(), candidates, context_of(covers_june));

    REQUIRE(resolution.cut.has_value());
    CHECK(resolution.cut->kind_code == "EOY");
}

TEST_CASE("a start of day selection resolves to the open of the covered date",
          "[service][cut][resolution]") {
    const auto candidates = std::vector<domain::cut>{cut_of("SOD", "2026-10-06T08:00:00Z"),
                                                     cut_of("EOD", "2026-10-06T17:00:00Z", 2)};

    const auto resolution =
        resolve_cut_selection(select("SOD"), the_seven_kinds(), candidates, context_of());

    REQUIRE(resolution.cut.has_value());
    CHECK(resolution.cut->kind_code == "SOD");
}

TEST_CASE("a verification selection resolves to the verification cut of the covered date",
          "[service][cut][resolution]") {
    const auto candidates = std::vector<domain::cut>{cut_of("IPV", "2026-10-06T17:00:00Z")};

    const auto resolution =
        resolve_cut_selection(select("IPV"), the_seven_kinds(), candidates, context_of());

    REQUIRE(resolution.cut.has_value());
    CHECK(resolution.cut->kind_code == "IPV");
}

TEST_CASE("a cut that waits on an authorisation is refused and names the request",
          "[service][cut][resolution]") {
    const auto candidates = std::vector<domain::cut>{waiting_cut("EOD", "2026-10-06T17:00:00Z")};

    const auto resolution =
        resolve_cut_selection(select("EOD"), the_seven_kinds(), candidates, context_of());

    CHECK_FALSE(resolution.ok());
    CHECK(resolution.refusal.find("aaaaaaaa-0000-0000-0000-000000000001") != std::string::npos);
    CHECK(resolution.refusal.find("waits") != std::string::npos);
}

TEST_CASE("a selection finds nothing and says what it looked for",
          "[service][cut][resolution]") {
    const auto resolution =
        resolve_cut_selection(select("EOD"), the_seven_kinds(), {}, context_of());

    CHECK_FALSE(resolution.ok());
    CHECK(resolution.refusal.find("ACME-LONDON") != std::string::npos);
    CHECK(resolution.refusal.find("2026-10-06") != std::string::npos);
}

TEST_CASE("a kind whose instant the run supplies mints a body at the run's clock",
          "[service][cut][resolution]") {
    const auto resolution =
        resolve_cut_selection(select("LIVE"), the_seven_kinds(), {}, context_of());

    REQUIRE(resolution.ok());
    REQUIRE(resolution.mint.has_value());
    CHECK(resolution.mint->kind_code == "LIVE");
    CHECK(resolution.mint->timepoint ==
          datetime::from_iso8601_utc("2026-10-07T07:40:00Z"));
    CHECK(resolution.mint->taken_by == "svc.reporting");
    CHECK(resolution.mint->scope_code == "ACME-LONDON");
}

TEST_CASE("a run mints no body from parts the assembly could not fix",
          "[service][cut][resolution]") {
    auto ctx = context_of();
    ctx.preview = false;
    ctx.body = cut_body{};

    const auto resolution = resolve_cut_selection(select("LIVE"), the_seven_kinds(), {}, ctx);

    CHECK_FALSE(resolution.ok());
    CHECK(resolution.refusal.find("market data snapshot") != std::string::npos);
}

TEST_CASE("a run mints a body once every part is assembled", "[service][cut][resolution]") {
    auto ctx = context_of();
    ctx.preview = false;
    cut_body body;
    body.market_data_snapshot_id = uuid_of("cccccccc-0000-0000-0000-000000000001");
    body.deal_population_id = uuid_of("cccccccc-0000-0000-0000-000000000002");
    body.reference_data_version = "rd-2026-10-07";
    body.engine_version = "ore-1.2.3";
    body.code_version = "abc1234";
    ctx.body = body;

    const auto resolution = resolve_cut_selection(select("LIVE"), the_seven_kinds(), {}, ctx);

    REQUIRE(resolution.mint.has_value());
    CHECK(resolution.mint->market_data_snapshot_id == body.market_data_snapshot_id);
    CHECK(resolution.mint->deal_population_id == body.deal_population_id);
    CHECK(resolution.mint->engine_version == "ore-1.2.3");
}

TEST_CASE("a pinned cut resolves to itself and no rule is consulted",
          "[service][cut][resolution]") {
    auto flash = cut_of("FLASH", "2026-10-06T11:15:00Z");
    const auto candidates = std::vector<domain::cut>{flash};

    auto selection = select("FLASH");
    selection.cut_id = flash.id;

    const auto resolution =
        resolve_cut_selection(selection, the_seven_kinds(), candidates, context_of());

    REQUIRE(resolution.cut.has_value());
    CHECK(resolution.cut->id == flash.id);
}

TEST_CASE("a pinned cut of another scope is refused", "[service][cut][resolution]") {
    auto flash = cut_of("FLASH", "2026-10-06T11:15:00Z");
    flash.scope_code = "ACME-ZURICH";
    const auto candidates = std::vector<domain::cut>{flash};

    auto selection = select("FLASH");
    selection.cut_id = flash.id;

    const auto resolution =
        resolve_cut_selection(selection, the_seven_kinds(), candidates, context_of());

    CHECK_FALSE(resolution.ok());
    CHECK(resolution.refusal.find("ACME-ZURICH") != std::string::npos);
}

TEST_CASE("a flash kind resolves no date, so a report reaches it by pinning one",
          "[service][cut][resolution]") {
    const auto resolution =
        resolve_cut_selection(select("FLASH"), the_seven_kinds(), {}, context_of());

    CHECK_FALSE(resolution.ok());
    CHECK(resolution.refusal.find("pinning") != std::string::npos);
}

TEST_CASE("a selection naming a kind the store does not hold is refused",
          "[service][cut][resolution]") {
    const auto resolution =
        resolve_cut_selection(select("NOPE"), the_seven_kinds(), {}, context_of());

    CHECK_FALSE(resolution.ok());
    CHECK(resolution.refusal.find("NOPE") != std::string::npos);
}

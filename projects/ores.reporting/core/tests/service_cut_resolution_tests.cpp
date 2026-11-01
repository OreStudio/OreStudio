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
#include "ores.database/domain/context.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/generators/business_centre_generator.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/business_centre_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.reporting.api/generators/cut_family_generator.hpp"
#include "ores.reporting.api/generators/cut_kind_generator.hpp"
#include "ores.reporting.api/generators/cut_resolution_rule_generator.hpp"
#include "ores.reporting.api/generators/report_definition_generator.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/messaging/report_cut_resolution_handler.hpp"
#include "ores.reporting.core/repository/cut_family_repository.hpp"
#include "ores.reporting.core/repository/cut_kind_repository.hpp"
#include "ores.reporting.core/repository/cut_repository.hpp"
#include "ores.reporting.core/repository/cut_resolution_rule_repository.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.reporting.core/service/cut_resolution_plan.hpp"
#include "ores.reporting.core/service/cut_resolution_service.hpp"
#include "ores.reporting.core/service/report_definition_service.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "test_suite.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <format>
#include <optional>
#include <string>
#include <vector>

// The decision these pin: which body a report's selection resolves to, given the
// date the report covers, its scope and the cuts already taken. The resolution
// is a function so that the preview and the run read one answer: a preview
// computed by a second code path would be a preview of that path.

using namespace ores::logging;
using namespace ores::reporting;
using namespace ores::reporting::service;

namespace {

using namespace std::chrono;
using ores::platform::time::datetime;

const auto covered = datetime::from_iso8601_utc("2026-10-06T00:00:00Z");

boost::uuids::uuid cut_uuid_of(const std::string& text) {
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
        lookup("EOM",
               "end_of_day",
               "last_business_day_of_month_close",
               "reporting.eod_cut_authorisation"),
        lookup("EOY",
               "end_of_day",
               "last_business_day_of_year_close",
               "reporting.eod_cut_authorisation"),
        lookup("SOD", "start_of_day", "same_day_open"),
        lookup("IPV", "verification", "cut_it_is_built_on", "reporting.ipv_cut_authorisation"),
        lookup("FLASH", "intraday", "the_cut_named"),
        lookup("LIVE", "consumer", "run_instant"),
    };
}

domain::cut cut_of(const std::string& kind_code, const std::string& instant_text, int ordinal = 1) {
    domain::cut cut;
    cut.id = cut_uuid_of(std::format("00000000-0000-0000-0000-{:012}", ordinal));
    cut.kind_code = kind_code;
    cut.scope_code = "ACME-LONDON";
    cut.business_centre_code = "LDN";
    cut.timepoint = datetime::from_iso8601_utc(instant_text);
    cut.timepoint_timezone = "Europe/London";
    cut.take_ordinal = ordinal;
    cut.taken_by = "a.reyes";
    cut.taken_at = cut.timepoint;
    cut.requires_authorisation =
        kind_code == "EOD" || kind_code == "EOM" || kind_code == "EOY" || kind_code == "IPV";
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
    cut.approval_request_id = cut_uuid_of("aaaaaaaa-0000-0000-0000-000000000001");
    return cut;
}

cut_resolution_context context_of(std::chrono::system_clock::time_point covered_date = covered) {
    cut_resolution_context ctx;
    ctx.covered_date = covered_date;
    ctx.scope_code = "ACME-LONDON";
    ctx.business_centre_code = "LDN";
    ctx.party_id = cut_uuid_of("bbbbbbbb-0000-0000-0000-000000000001");
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

    auto lg(make_logger(test_suite));
    // 2026-10-31 is a Saturday, 2026-05-31 a Sunday, and 2026-12-31 a Thursday.
    CHECK(datetime::to_iso8601_date(last_business_day_of_month(year(2026) / October)) ==
          "2026-10-30");
    CHECK(datetime::to_iso8601_date(last_business_day_of_month(year(2026) / May)) == "2026-05-29");
    CHECK(datetime::to_iso8601_date(last_business_day_of_month(year(2026) / December)) ==
          "2026-12-31");
}

TEST_CASE("the last business day of a year is the last business day of December",
          "[service][cut][resolution]") {

    auto lg(make_logger(test_suite));
    CHECK(datetime::to_iso8601_date(last_business_day_of_year(year(2027))) == "2027-12-31");
}

TEST_CASE("an end of day selection resolves to the close of the covered date",
          "[service][cut][resolution]") {

    auto lg(make_logger(test_suite));
    const auto candidates = std::vector<domain::cut>{cut_of("EOD", "2026-10-06T17:00:00Z")};

    const auto resolution =
        resolve_cut_selection(select("EOD"), the_seven_kinds(), candidates, context_of());

    REQUIRE(resolution.ok());
    REQUIRE(resolution.cut.has_value());
    CHECK(resolution.cut->kind_code == "EOD");
}

TEST_CASE("an end of day selection prices from the month end body on a month's last business day",
          "[service][cut][resolution]") {

    auto lg(make_logger(test_suite));
    // 2026-10-30 is the last business day of October, so both a plain close and
    // the month end cut stand, and the more specific occasion is the one meant.
    const auto month_end = datetime::from_iso8601_utc("2026-10-30T00:00:00Z");
    const auto candidates = std::vector<domain::cut>{cut_of("EOD", "2026-10-30T17:00:00Z"),
                                                     cut_of("EOM", "2026-10-30T17:00:00Z", 2)};

    const auto resolution =
        resolve_cut_selection(select("EOD"), the_seven_kinds(), candidates, context_of(month_end));

    REQUIRE(resolution.cut.has_value());
    CHECK(resolution.cut->kind_code == "EOM");
}

TEST_CASE("an end of day selection falls back to the plain close where no month end stands",
          "[service][cut][resolution]") {

    auto lg(make_logger(test_suite));
    const auto month_end = datetime::from_iso8601_utc("2026-10-30T00:00:00Z");
    const auto candidates = std::vector<domain::cut>{cut_of("EOD", "2026-10-30T17:00:00Z")};

    const auto resolution =
        resolve_cut_selection(select("EOD"), the_seven_kinds(), candidates, context_of(month_end));

    REQUIRE(resolution.cut.has_value());
    CHECK(resolution.cut->kind_code == "EOD");
}

TEST_CASE("a month end selection refuses a plain close of that day", "[service][cut][resolution]") {

    auto lg(make_logger(test_suite));
    const auto month_end = datetime::from_iso8601_utc("2026-10-30T00:00:00Z");
    const auto candidates = std::vector<domain::cut>{cut_of("EOD", "2026-10-30T17:00:00Z")};

    const auto resolution =
        resolve_cut_selection(select("EOM"), the_seven_kinds(), candidates, context_of(month_end));

    CHECK_FALSE(resolution.ok());
    CHECK_FALSE(resolution.refusal.empty());
}

TEST_CASE("a month end selection resolves to that month's last business day",
          "[service][cut][resolution]") {

    auto lg(make_logger(test_suite));
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

    auto lg(make_logger(test_suite));
    const auto covers_june = datetime::from_iso8601_utc("2026-06-15T00:00:00Z");
    const auto candidates = std::vector<domain::cut>{cut_of("EOY", "2026-12-31T17:00:00Z")};

    const auto resolution = resolve_cut_selection(
        select("EOY"), the_seven_kinds(), candidates, context_of(covers_june));

    REQUIRE(resolution.cut.has_value());
    CHECK(resolution.cut->kind_code == "EOY");
}

TEST_CASE("a start of day selection resolves to the open of the covered date",
          "[service][cut][resolution]") {

    auto lg(make_logger(test_suite));
    const auto candidates = std::vector<domain::cut>{cut_of("SOD", "2026-10-06T08:00:00Z"),
                                                     cut_of("EOD", "2026-10-06T17:00:00Z", 2)};

    const auto resolution =
        resolve_cut_selection(select("SOD"), the_seven_kinds(), candidates, context_of());

    REQUIRE(resolution.cut.has_value());
    CHECK(resolution.cut->kind_code == "SOD");
}

TEST_CASE("a verification selection resolves to the verification cut of the covered date",
          "[service][cut][resolution]") {

    auto lg(make_logger(test_suite));
    const auto candidates = std::vector<domain::cut>{cut_of("IPV", "2026-10-06T17:00:00Z")};

    const auto resolution =
        resolve_cut_selection(select("IPV"), the_seven_kinds(), candidates, context_of());

    REQUIRE(resolution.cut.has_value());
    CHECK(resolution.cut->kind_code == "IPV");
}

TEST_CASE("a cut that waits on an authorisation is refused and names the request",
          "[service][cut][resolution]") {

    auto lg(make_logger(test_suite));
    const auto candidates = std::vector<domain::cut>{waiting_cut("EOD", "2026-10-06T17:00:00Z")};

    const auto resolution =
        resolve_cut_selection(select("EOD"), the_seven_kinds(), candidates, context_of());

    CHECK_FALSE(resolution.ok());
    CHECK(resolution.refusal.contains("aaaaaaaa-0000-0000-0000-000000000001"));
    CHECK(resolution.refusal.contains("waits"));
}

TEST_CASE("a selection finds nothing and says what it looked for", "[service][cut][resolution]") {

    auto lg(make_logger(test_suite));
    const auto resolution =
        resolve_cut_selection(select("EOD"), the_seven_kinds(), {}, context_of());

    CHECK_FALSE(resolution.ok());
    CHECK(resolution.refusal.contains("ACME-LONDON"));
    CHECK(resolution.refusal.contains("2026-10-06"));
}

TEST_CASE("a kind whose instant the run supplies mints a body at the run's clock",
          "[service][cut][resolution]") {

    auto lg(make_logger(test_suite));
    const auto resolution =
        resolve_cut_selection(select("LIVE"), the_seven_kinds(), {}, context_of());

    REQUIRE(resolution.ok());
    REQUIRE(resolution.mint.has_value());
    CHECK(resolution.mint->kind_code == "LIVE");
    CHECK(resolution.mint->timepoint == datetime::from_iso8601_utc("2026-10-07T07:40:00Z"));
    CHECK(resolution.mint->taken_by == "svc.reporting");
    CHECK(resolution.mint->scope_code == "ACME-LONDON");
}

TEST_CASE("a run mints no body from parts the assembly could not fix",
          "[service][cut][resolution]") {

    auto lg(make_logger(test_suite));
    auto ctx = context_of();
    ctx.preview = false;
    ctx.body = cut_body{};

    const auto resolution = resolve_cut_selection(select("LIVE"), the_seven_kinds(), {}, ctx);

    CHECK_FALSE(resolution.ok());
    CHECK(resolution.refusal.contains("market data snapshot"));
}

TEST_CASE("a run mints a body once every part is assembled", "[service][cut][resolution]") {

    auto lg(make_logger(test_suite));
    auto ctx = context_of();
    ctx.preview = false;
    cut_body body;
    body.market_data_snapshot_id = cut_uuid_of("cccccccc-0000-0000-0000-000000000001");
    body.deal_population_id = cut_uuid_of("cccccccc-0000-0000-0000-000000000002");
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

    auto lg(make_logger(test_suite));
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

    auto lg(make_logger(test_suite));
    auto flash = cut_of("FLASH", "2026-10-06T11:15:00Z");
    flash.scope_code = "ACME-ZURICH";
    const auto candidates = std::vector<domain::cut>{flash};

    auto selection = select("FLASH");
    selection.cut_id = flash.id;

    const auto resolution =
        resolve_cut_selection(selection, the_seven_kinds(), candidates, context_of());

    CHECK_FALSE(resolution.ok());
    CHECK(resolution.refusal.contains("ACME-ZURICH"));
}

TEST_CASE("a flash kind resolves no date, so a report reaches it by pinning one",
          "[service][cut][resolution]") {

    auto lg(make_logger(test_suite));
    const auto resolution =
        resolve_cut_selection(select("FLASH"), the_seven_kinds(), {}, context_of());

    CHECK_FALSE(resolution.ok());
    CHECK(resolution.refusal.contains("pinning"));
}

TEST_CASE("a selection naming a kind the store does not hold is refused",
          "[service][cut][resolution]") {

    auto lg(make_logger(test_suite));
    const auto resolution =
        resolve_cut_selection(select("NOPE"), the_seven_kinds(), {}, context_of());

    CHECK_FALSE(resolution.ok());
    CHECK(resolution.refusal.contains("NOPE"));
}

TEST_CASE("a cut a later body of its own kind replaced still resolves, and says so",
          "[service][cut][resolution]") {
    auto lg(make_logger(test_suite));

    const auto early = cut_of("EOD", "2026-10-06T17:00:00Z");
    auto retake = cut_of("EOD", "2026-10-06T17:00:00Z", 2);
    retake.timepoint = datetime::from_iso8601_utc("2026-10-06T17:05:00Z");
    const auto candidates = std::vector<domain::cut>{retake, early};

    const auto resolution =
        resolve_cut_selection(select("EOD"), the_seven_kinds(), candidates, context_of());

    REQUIRE(resolution.cut.has_value());
    CHECK(resolution.cut->id == retake.id);
    REQUIRE(resolution.state.has_value());
    CHECK(*resolution.state == cut_state::authorised);

    // The earlier body is superseded and still usable: a run that priced from it
    // reproduces from it.
    CHECK(later_cut_holds_its_place(early, candidates));
    CHECK(derive_state(early, 0, later_cut_holds_its_place(early, candidates)) ==
          cut_state::superseded);
    CHECK_FALSE(later_cut_holds_its_place(retake, candidates));
}

TEST_CASE("a later body that waits on a person holds no place", "[service][cut][resolution]") {
    auto lg(make_logger(test_suite));

    const auto early = cut_of("EOD", "2026-10-06T17:00:00Z");
    auto waiting = waiting_cut("EOD", "2026-10-06T17:00:00Z");
    waiting.timepoint = datetime::from_iso8601_utc("2026-10-06T17:05:00Z");

    CHECK_FALSE(later_cut_holds_its_place(early, {waiting}));
    CHECK(derive_state(early, 0, later_cut_holds_its_place(early, {waiting})) ==
          cut_state::authorised);
}

TEST_CASE("a body of another centre or another party supersedes nothing",
          "[service][cut][resolution]") {
    auto lg(make_logger(test_suite));

    const auto early = cut_of("EOD", "2026-10-06T17:00:00Z");
    auto other_centre = cut_of("EOD", "2026-10-06T17:00:00Z", 2);
    other_centre.timepoint = datetime::from_iso8601_utc("2026-10-06T18:05:00Z");
    other_centre.business_centre_code = "ZRH";
    auto other_party = other_centre;
    other_party.business_centre_code = early.business_centre_code;
    other_party.party_id = cut_uuid_of("bbbbbbbb-0000-0000-0000-000000000099");

    CHECK_FALSE(later_cut_holds_its_place(early, {other_centre, other_party}));
}

TEST_CASE("the indicative mark is the resolved cut's and travels with the answer",
          "[service][cut][resolution]") {
    auto lg(make_logger(test_suite));

    auto flash = cut_of("FLASH", "2026-10-06T11:15:00Z");
    flash.output_is_indicative = true;
    auto selection = select("FLASH");
    selection.cut_id = flash.id;

    const auto resolution =
        resolve_cut_selection(selection, the_seven_kinds(), {flash}, context_of());

    REQUIRE(resolution.cut.has_value());
    CHECK(resolution.output_is_indicative);

    auto official = cut_of("EOD", "2026-10-06T17:00:00Z");
    const auto eod =
        resolve_cut_selection(select("EOD"), the_seven_kinds(), {official}, context_of());
    REQUIRE(eod.cut.has_value());
    CHECK_FALSE(eod.output_is_indicative);
}

TEST_CASE("a kind whose instant the run supplies carries the kind's indicative mark",
          "[service][cut][resolution]") {
    auto lg(make_logger(test_suite));

    auto kinds = the_seven_kinds();
    for (auto& kind : kinds) {
        if (kind.code == "LIVE")
            kind.output_is_indicative = true;
    }

    const auto resolution = resolve_cut_selection(select("LIVE"), kinds, {}, context_of());

    REQUIRE(resolution.mint.has_value());
    CHECK(resolution.output_is_indicative);
}

// ---------------------------------------------------------------------------
// The write path. A resolution a run mints is the only thing that writes a cut,
// so these read the row back from the store: what is asserted is what the store
// holds, not what the service returned.
// ---------------------------------------------------------------------------

namespace {

using dt = ores::platform::time::datetime;

/**
 * @brief A party, two rates centres and a LIVE kind that names no approval kind.
 *
 * The kind states its own retention, indicativeness and basis, so the take has
 * something unlike the seeded reference data to copy.
 */
struct occasion {
    std::string kind_code;
    boost::uuids::uuid party_id;
    std::string business_centre_code;
    std::string other_business_centre_code;
};

occasion seed_occasion(ores::testing::scoped_database_helper& h,
                       ores::utility::generation::generation_context& gen) {
    auto& db = h.context();

    auto family = generators::generate_synthetic_cut_family(gen);
    family.change_reason_code = "system.test";
    repository::cut_family_repository{}.write(db, family);

    // The rule code is the resolution's own, so the row is written once per
    // tenant however many cases run in one process.
    const auto rules_held = repository::cut_resolution_rule_repository{}.read_latest(db);
    const auto rule_held = std::any_of(rules_held.begin(), rules_held.end(), [](const auto& r) {
        return r.code == resolution_rules::run_instant;
    });
    if (!rule_held) {
        auto rule = generators::generate_synthetic_cut_resolution_rule(gen);
        rule.code = std::string(resolution_rules::run_instant);
        rule.change_reason_code = "system.test";
        repository::cut_resolution_rule_repository{}.write(db, rule);
    }

    auto kind = generators::generate_synthetic_cut_kind(gen);
    kind.code = "LIVE-" + boost::uuids::to_string(boost::uuids::random_generator()()).substr(0, 8);
    kind.family_code = family.code;
    kind.resolution_rule_code = std::string(resolution_rules::run_instant);
    kind.approval_kind_code = std::nullopt;
    kind.retention_months = 3;
    kind.retention_destination_code = "cold";
    kind.legal_hold_default = false;
    kind.output_is_indicative = true;
    kind.change_reason_code = "system.test";
    repository::cut_kind_repository{}.write(db, kind);

    auto party = ores::refdata::generators::generate_synthetic_party(gen);
    party.change_reason_code = "system.test";
    for (const auto& held : ores::refdata::repository::party_repository().read_latest(db)) {
        if (held.tenant_id == party.tenant_id) {
            party.parent_party_id = held.id;
            break;
        }
    }
    ores::refdata::repository::party_repository party_repo;
    party_repo.write(db, party);

    ores::refdata::repository::business_centre_repository centres;
    auto centre = ores::refdata::generators::generate_synthetic_business_centre(gen);
    centre.change_reason_code = "system.test";
    centres.write(db, centre);
    auto other = ores::refdata::generators::generate_synthetic_business_centre(gen);
    other.change_reason_code = "system.test";
    centres.write(db, other);

    return occasion{kind.code, party.id, centre.code, other.code};
}

cut_resolution_context run_context(const occasion& occasion,
                                   const std::string& business_centre_code,
                                   const std::string& actor) {
    cut_resolution_context ctx;
    ctx.covered_date = covered;
    ctx.scope_code = "ACME-LONDON";
    ctx.business_centre_code = business_centre_code;
    ctx.party_id = occasion.party_id;
    ctx.timepoint_timezone = "Europe/London";
    ctx.run_instant = dt::from_iso8601_utc("2026-10-07T07:40:00Z");
    ctx.taken_by = actor;

    cut_body body;
    body.market_data_snapshot_id = cut_uuid_of("cccccccc-0000-0000-0000-000000000001");
    body.deal_population_id = cut_uuid_of("cccccccc-0000-0000-0000-000000000002");
    body.reference_data_version = "rd-2026-10-07";
    body.engine_version = "ore-1.2.3";
    body.code_version = "abc1234";
    ctx.body = body;
    ctx.preview = false;
    return ctx;
}

domain::cut read_back(const ores::database::context& db, const boost::uuids::uuid& id) {
    const auto held = repository::cut_repository{}.read_latest(db, boost::uuids::to_string(id));
    REQUIRE(held.size() == 1);
    return held.front();
}

}

TEST_CASE("a minted cut records the kind's retention, indicativeness and basis",
          "[cut][resolution][integration]") {
    auto lg(make_logger(test_suite));

    ores::testing::scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto occasion = seed_occasion(h, gen);

    cut_resolution_service service(h.context());
    const auto resolution =
        service.resolve(select(occasion.kind_code),
                        run_context(occasion, occasion.business_centre_code, h.db_user()));

    REQUIRE(resolution.ok());
    REQUIRE(resolution.cut.has_value());
    CHECK(resolution.output_is_indicative);

    const auto cut = read_back(h.context(), resolution.cut->id);
    BOOST_LOG_SEV(lg, info) << "Minted take " << cut.take_ordinal << " of kind " << cut.kind_code;

    CHECK(cut.retention_months == 3);
    CHECK(cut.retention_destination_code == "cold");
    CHECK(cut.output_is_indicative);
    CHECK_FALSE(cut.requires_authorisation);
    CHECK_FALSE(cut.approval_request_id.has_value());
    CHECK(cut.take_ordinal == 1);
    CHECK(cut.kind_code == occasion.kind_code);
    CHECK(cut.scope_code == "ACME-LONDON");
    CHECK(cut.business_centre_code == occasion.business_centre_code);
    CHECK(cut.party_id == occasion.party_id);
    CHECK(cut.taken_by == h.db_user());
    REQUIRE(cut.taken_at.has_value());
    CHECK(*cut.taken_at == dt::from_iso8601_utc("2026-10-07T07:40:00Z"));
    CHECK(cut.market_data_snapshot_id.has_value());
    CHECK(cut.engine_version == "ore-1.2.3");
}

TEST_CASE("a retake of one occasion takes the next ordinal and another centre its own",
          "[cut][resolution][integration]") {
    auto lg(make_logger(test_suite));

    ores::testing::scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto occasion = seed_occasion(h, gen);

    cut_resolution_service service(h.context());
    const auto first =
        service.resolve(select(occasion.kind_code),
                        run_context(occasion, occasion.business_centre_code, h.db_user()));
    const auto second =
        service.resolve(select(occasion.kind_code),
                        run_context(occasion, occasion.business_centre_code, h.db_user()));
    const auto other_centre =
        service.resolve(select(occasion.kind_code),
                        run_context(occasion, occasion.other_business_centre_code, h.db_user()));

    REQUIRE(first.cut.has_value());
    REQUIRE(second.cut.has_value());
    REQUIRE(other_centre.cut.has_value());

    CHECK(read_back(h.context(), first.cut->id).take_ordinal == 1);
    CHECK(read_back(h.context(), second.cut->id).take_ordinal == 2);
    // The centre is part of the occasion: the same instant at another centre is
    // the first body of that occasion, not the third.
    CHECK(read_back(h.context(), other_centre.cut->id).take_ordinal == 1);
}

TEST_CASE("a body the assembly could not fix refuses the run and mints nothing",
          "[cut][resolution][integration]") {
    auto lg(make_logger(test_suite));

    ores::testing::scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto occasion = seed_occasion(h, gen);

    auto ctx = run_context(occasion, occasion.business_centre_code, h.db_user());
    ctx.body->market_data_snapshot_id.reset();

    cut_resolution_service service(h.context());
    const auto resolution = service.resolve(select(occasion.kind_code), ctx);

    CHECK_FALSE(resolution.ok());
    CHECK(resolution.refusal.contains("market data snapshot"));

    const auto held = repository::cut_repository{}.read_for_occasion_in_window(
        h.context(),
        occasion.party_id,
        occasion.kind_code,
        "ACME-LONDON",
        occasion.business_centre_code,
        dt::from_iso8601_utc("2026-10-07T07:39:00Z"),
        dt::from_iso8601_utc("2026-10-07T07:41:00Z"));
    CHECK(held.empty());
}

TEST_CASE("a run's resolution answers the cut it minted and the mark it carries",
          "[cut][resolution][integration]") {
    auto lg(make_logger(test_suite));

    ores::testing::scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto occasion = seed_occasion(h, gen);
    const auto party_ctx =
        h.context().with_party(h.tenant_id(), occasion.party_id, {occasion.party_id}, h.db_user());

    auto definition = generators::generate_synthetic_report_definition(gen);
    definition.change_reason_code = "system.test";
    definition.party_id = occasion.party_id;
    definition.cut_kind_code = occasion.kind_code;
    definition.cut_id = std::nullopt;
    repository::report_definition_repository{}.write(party_ctx, definition);

    service::report_definition_service definitions(party_ctx);
    service::cut_resolution_service cuts(party_ctx);

    ores::reporting::messaging::resolve_cut_request request;
    request.report_definition_id = definition.id;
    request.covered_date = "2026-10-07";
    request.scope_code = "ACME-LONDON";
    request.business_centre_code = occasion.business_centre_code;
    request.market_data_snapshot_id = "cccccccc-0000-0000-0000-000000000001";
    request.deal_population_id = "cccccccc-0000-0000-0000-000000000002";
    request.reference_data_version = "rd-2026-10-07";
    request.engine_version = "ore-1.2.3";
    request.code_version = "abc1234";

    const auto response = messaging::apply_cut_resolution(definitions, cuts, request, party_ctx);

    CHECK(response.refusal.empty());
    CHECK(response.output_is_indicative);
    CHECK(response.cut_kind_code == occasion.kind_code);
    CHECK(response.cut_state == "in_use");
    REQUIRE_FALSE(response.cut_id.empty());

    const auto cut = read_back(h.context(), boost::uuids::string_generator()(response.cut_id));
    CHECK(cut.party_id == occasion.party_id);
    CHECK(cut.output_is_indicative);
    CHECK(cut.take_ordinal == 1);
}

TEST_CASE("a preview answers the mark and writes nothing", "[cut][resolution][integration]") {
    auto lg(make_logger(test_suite));

    ores::testing::scoped_database_helper h;
    auto gen = ores::testing::make_generation_context(h);
    const auto occasion = seed_occasion(h, gen);
    const auto party_ctx =
        h.context().with_party(h.tenant_id(), occasion.party_id, {occasion.party_id}, h.db_user());

    auto definition = generators::generate_synthetic_report_definition(gen);
    definition.change_reason_code = "system.test";
    definition.party_id = occasion.party_id;
    definition.cut_kind_code = occasion.kind_code;
    definition.cut_id = std::nullopt;
    repository::report_definition_repository{}.write(party_ctx, definition);

    service::report_definition_service definitions(party_ctx);
    service::cut_resolution_service cuts(party_ctx);

    ores::reporting::messaging::resolve_cut_request request;
    request.report_definition_id = definition.id;
    request.covered_date = "2026-10-07";
    request.scope_code = "ACME-LONDON";
    request.business_centre_code = occasion.business_centre_code;
    request.preview = true;

    const auto response = messaging::apply_cut_resolution(definitions, cuts, request, party_ctx);

    CHECK(response.refusal.empty());
    CHECK(response.output_is_indicative);
    CHECK(response.cut_kind_code == occasion.kind_code);
    CHECK(response.cut_id.empty());

    const auto held = repository::cut_repository{}.read_for_occasion_in_window(
        h.context(),
        occasion.party_id,
        occasion.kind_code,
        "ACME-LONDON",
        occasion.business_centre_code,
        dt::from_iso8601_utc("2026-10-01T00:00:00Z"),
        dt::from_iso8601_utc("2026-10-31T00:00:00Z"));
    CHECK(held.empty());
}

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
#include "ores.reporting.core/service/cut_state.hpp"
#include "ores.reporting.core/service/cut_take_plan.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

// The decision these pin: the state a person reads is a function of the facts
// the cut records, and the label a person quotes is a function of the kind, the
// scope and the instant. Neither is stored, so both are worked out here from a
// row and nothing else: no clock, no database and no bus.

using namespace ores::reporting;
using namespace ores::reporting::service;

namespace {

using namespace std::chrono;

system_clock::time_point instant(const std::string& text) {
    using ores::platform::time::datetime;
    return datetime::from_iso8601_utc(text);
}

domain::cut_kind kind(const std::string& code,
                      const std::string& family,
                      std::optional<std::string> approval_kind,
                      int retention_months) {
    domain::cut_kind k;
    k.code = code;
    k.family_code = family;
    k.approval_kind_code = std::move(approval_kind);
    k.retention_months = retention_months;
    k.retention_destination_code = "cold";
    return k;
}

domain::cut taken_cut(const std::string& kind_code) {
    domain::cut cut;
    cut.kind_code = kind_code;
    cut.scope_code = "ACME-LONDON";
    cut.business_centre_code = "LDN";
    cut.timepoint = instant("2026-10-06T17:00:00Z");
    cut.timepoint_timezone = "Europe/London";
    cut.take_ordinal = 1;
    cut.taken_by = "a.reyes";
    cut.taken_at = instant("2026-10-06T17:04:00Z");
    return cut;
}

}

TEST_CASE("a row no take completed and no gap marred is a draft", "[service][cut]") {
    const auto cut = taken_cut("EOD");
    auto draft = cut;
    draft.taken_at.reset();

    CHECK(derive_state(draft, 0) == cut_state::draft);
    CHECK_FALSE(is_usable(derive_state(draft, 0)));
}

TEST_CASE("a row no take completed with a gap is incomplete", "[service][cut]") {
    auto attempt = taken_cut("EOD");
    attempt.taken_at.reset();

    CHECK(derive_state(attempt, 2) == cut_state::incomplete);
    CHECK_FALSE(is_usable(derive_state(attempt, 2)));
}

TEST_CASE("a taken body of a kind that requires an authorisation waits", "[service][cut]") {
    auto cut = taken_cut("EOD");
    cut.requires_authorisation = true;

    CHECK(derive_state(cut, 0) == cut_state::taken);
    CHECK_FALSE(is_usable(derive_state(cut, 0)));
}

TEST_CASE("an authorised body is usable and states its authorisation", "[service][cut]") {
    auto cut = taken_cut("EOD");
    cut.requires_authorisation = true;
    cut.approved_by = "m.silva";
    cut.approved_at = instant("2026-10-06T18:00:00Z");

    CHECK(derive_state(cut, 0) == cut_state::authorised);
    CHECK(is_usable(derive_state(cut, 0)));
}

TEST_CASE("a body of a kind that requires none is in use the moment it is taken", "[service][cut]") {
    auto cut = taken_cut("SOD");
    cut.requires_authorisation = false;

    CHECK(derive_state(cut, 0) == cut_state::in_use);
    CHECK(is_usable(derive_state(cut, 0)));
}

TEST_CASE("a refusal is terminal for that body", "[service][cut]") {
    auto cut = taken_cut("EOD");
    cut.requires_authorisation = true;
    cut.refused_at = instant("2026-10-06T18:00:00Z");
    cut.refusal_comment = "the curve was not approved";

    CHECK(derive_state(cut, 0) == cut_state::refused);
    CHECK_FALSE(is_usable(derive_state(cut, 0)));
}

TEST_CASE("a later cut of the kind and scope supersedes a usable one", "[service][cut]") {
    auto cut = taken_cut("EOD");
    cut.requires_authorisation = true;
    cut.approved_by = "m.silva";
    cut.approved_at = instant("2026-10-06T18:00:00Z");

    CHECK(derive_state(cut, 0, true) == cut_state::superseded);
    CHECK(is_usable(derive_state(cut, 0, true)));
}

TEST_CASE("supersession leaves a body that was never usable where it was", "[service][cut]") {
    auto cut = taken_cut("EOD");
    cut.requires_authorisation = true;

    CHECK(derive_state(cut, 0, true) == cut_state::taken);
}

TEST_CASE("the label names the kind, the scope and the date", "[service][cut]") {
    const auto cut = taken_cut("EOD");
    CHECK(cut_label_of(cut, kind("EOD", "end_of_day", std::nullopt, 25)) ==
          "EOD.ACME-LONDON.2026-10-06");
}

TEST_CASE("a retake states its ordinal so two bodies are told apart", "[service][cut]") {
    auto cut = taken_cut("EOD");
    cut.take_ordinal = 2;

    CHECK(cut_label_of(cut, kind("EOD", "end_of_day", std::nullopt, 25)) ==
          "EOD.ACME-LONDON.2026-10-06.2");
}

TEST_CASE("a body whose instant the consumer supplies carries the instant", "[service][cut]") {
    auto cut = taken_cut("LIVE");
    cut.timepoint = instant("2026-10-07T07:40:00Z");

    CHECK(cut_label_of(cut, kind("LIVE", "consumer", std::nullopt, 1)) ==
          "LIVE.ACME-LONDON.2026-10-07T07:40:00Z");
}

TEST_CASE("a take copies the kind's retention and never the caller's", "[service][cut]") {
    cut_take_plan plan;
    plan.kind_code = "EOY";
    plan.scope_code = "ACME-LONDON";
    plan.business_centre_code = "LDN";
    plan.timepoint = instant("2026-12-31T17:00:00Z");
    plan.timepoint_timezone = "Europe/London";
    plan.taken_by = "a.reyes";
    plan.taken_at = instant("2026-12-31T17:10:00Z");

    const auto eoy = kind("EOY", "end_of_day", std::string("reporting.eod_cut_authorisation"), 120);
    const auto cut = plan_take(plan, eoy, boost::uuids::uuid{}, 1);

    CHECK(cut.retention_months == 120);
    CHECK(cut.retention_destination_code == "cold");
    CHECK(cut.legal_hold == false);
    CHECK(cut.kind_code == "EOY");
    CHECK(cut.party_id == plan.party_id);
    CHECK(cut.take_ordinal == 1);
}

TEST_CASE("a kind that names an approval kind is taken as one that waits", "[service][cut]") {
    cut_take_plan plan;
    plan.kind_code = "IPV";
    plan.taken_by = "a.reyes";
    plan.taken_at = instant("2026-10-06T18:00:00Z");

    const auto signed_kind =
        kind("IPV", "verification", std::string("reporting.ipv_cut_authorisation"), 84);
    const auto unsigned_kind = kind("SOD", "start_of_day", std::nullopt, 25);

    CHECK(plan_take(plan, signed_kind, boost::uuids::uuid{}, 1).requires_authorisation);
    CHECK_FALSE(plan_take(plan, unsigned_kind, boost::uuids::uuid{}, 1).requires_authorisation);
}

TEST_CASE("a retake of one occasion is numbered after the takes held", "[service][cut]") {
    auto first = taken_cut("EOD");
    first.take_ordinal = 1;
    auto second = taken_cut("EOD");
    second.take_ordinal = 2;

    CHECK(next_take_ordinal({}) == 1);
    CHECK(next_take_ordinal({first}) == 2);
    CHECK(next_take_ordinal({first, second}) == 3);
}

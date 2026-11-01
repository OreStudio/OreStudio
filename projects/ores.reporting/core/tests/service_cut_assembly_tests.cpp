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
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.core/service/cut_assembly.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

// The decision these pin: a part of a body is closed when a read answered it,
// and a gap stands with the item, the reason and the remedy when the read
// answered nothing. Nothing here needs a store: the facts are the caller's, so
// the rule that a gap stops the press is stated once and tested without a
// database or a session.

using ores::reporting::domain::cut_kind;
using ores::reporting::service::assemble_cut_body;
using ores::reporting::service::cut_body_facts;

namespace {

/** A kind whose cuts are kept 25 months and wait on a signature. */
cut_kind a_signed_kind() {
    cut_kind kind;
    kind.code = "EOD";
    kind.name = "End of day cut";
    kind.family_code = "end_of_day";
    kind.approval_kind_code = "reporting.eod_cut_authorisation";
    kind.output_is_indicative = false;
    kind.retention_months = 25;
    kind.retention_destination_code = "cold";
    return kind;
}

/** A kind whose cuts are usable the moment they are taken. */
cut_kind an_unsigned_kind() {
    auto kind = a_signed_kind();
    kind.code = "SOD";
    kind.name = "Start of day cut";
    kind.family_code = "start_of_day";
    kind.approval_kind_code = std::nullopt;
    return kind;
}

/** Facts a store that answers every part replies: nothing gap stands. */
cut_body_facts a_full_store() {
    cut_body_facts facts;
    facts.deal_count = 1284;
    facts.market_data_set_count = 7;
    facts.market_data_as_of = "2026-10-06 16:00:00.000000Z";
    facts.reference_data_version = "4 report configuration(s) in force, newest version 7";
    facts.engine_version = "1.8.17.0";
    facts.wrapper_version = "1.0.0";
    facts.taking_account = "a.reyes";
    facts.taking_party = "8f2a4c19-0000-4000-8000-000000000010";
    facts.taking_instant = ores::platform::time::datetime::from_iso8601_utc("2026-10-06T16:02:00Z");
    return facts;
}

/** The part of the answer a code names, so a case reads one row. */
auto part_of(const ores::reporting::service::cut_assembly& assembly, const std::string& code) {
    for (const auto& part : assembly.parts) {
        if (part.part_code == code)
            return part;
    }
    return ores::reporting::messaging::cut_body_part{};
}

}

TEST_CASE("a store that answers every part closes the whole body", "[cut_assembly]") {

    auto lg(ores::logging::make_logger(test_suite));
    const auto assembly = assemble_cut_body(a_signed_kind(), a_full_store());

    REQUIRE(assembly.gaps.empty());
    REQUIRE(assembly.complete());
    REQUIRE(assembly.parts.size() == 6);
    REQUIRE(assembly.deal_count == 1284);
    REQUIRE(assembly.market_data_set_count == 7);
    for (const auto& part : assembly.parts)
        REQUIRE(part.state == "closed");
}

TEST_CASE("the kind's retention and basis are stated in the body", "[cut_assembly]") {

    auto lg(ores::logging::make_logger(test_suite));
    const auto assembly = assemble_cut_body(a_signed_kind(), a_full_store());

    const auto kind = part_of(assembly, "kind");
    REQUIRE(kind.state == "closed");
    REQUIRE(kind.summary.find("EOD") != std::string::npos);
    REQUIRE(kind.summary.find("reporting.eod_cut_authorisation") != std::string::npos);
    REQUIRE(kind.snapshot_note.find("25 months") != std::string::npos);
}

TEST_CASE("a quiet day closes the deals part with a count of zero", "[cut_assembly]") {

    auto lg(ores::logging::make_logger(test_suite));
    auto facts = a_full_store();
    facts.deal_count = 0;

    const auto assembly = assemble_cut_body(a_signed_kind(), facts);

    // A day nothing traded is a body and not a hole: the read answered, so the
    // part is closed and the body records the count it answered.
    REQUIRE(assembly.complete());
    REQUIRE(assembly.gaps.empty());
    REQUIRE(assembly.deal_count == 0);

    const auto part = part_of(assembly, "deal_population");
    REQUIRE(part.state == "closed");
    REQUIRE(part.summary == "0 deals held at the instant");
}

TEST_CASE("a deal population read that answered nothing is a gap", "[cut_assembly]") {

    auto lg(ores::logging::make_logger(test_suite));
    auto facts = a_full_store();
    facts.deal_count = std::nullopt;
    facts.deals_refusal = "The deal population read refused the instant.";

    const auto assembly = assemble_cut_body(a_signed_kind(), facts);

    REQUIRE_FALSE(assembly.complete());
    REQUIRE(assembly.gaps.size() == 1);
    REQUIRE(assembly.gaps.front().part_code == "deal_population");
    REQUIRE(assembly.gaps.front().reason == "The deal population read refused the instant.");

    const auto part = part_of(assembly, "deal_population");
    REQUIRE(part.state == "gapped");
    REQUIRE(part.item == "the deal population");
    REQUIRE(part.remedy.find("state an instant") != std::string::npos);
}

TEST_CASE("a candidate set nobody approved is the gap the journey names", "[cut_assembly]") {

    auto lg(ores::logging::make_logger(test_suite));
    auto facts = a_full_store();
    facts.market_data_set_count = 0;
    facts.market_data_candidate_count = 2;

    const auto assembly = assemble_cut_body(a_signed_kind(), facts);

    REQUIRE(assembly.gaps.size() == 1);
    REQUIRE(assembly.gaps.front().part_code == "market_data");
    REQUIRE(assembly.gaps.front().item == "a curve in the candidate state");
    REQUIRE(part_of(assembly, "market_data").remedy.find("publish") != std::string::npos);
}

TEST_CASE("no engine version is a gap, because the result does not follow from the body",
          "[cut_assembly]") {

    auto lg(ores::logging::make_logger(test_suite));
    auto facts = a_full_store();
    facts.engine_version = std::nullopt;

    const auto assembly = assemble_cut_body(a_signed_kind(), facts);

    REQUIRE(assembly.gaps.size() == 1);
    REQUIRE(assembly.gaps.front().part_code == "engine_code");
    REQUIRE(assembly.gaps.front().reason.find("No engine version recorded") != std::string::npos);
}

TEST_CASE("a part the store answers and records no reference for is a note, not a gap",
          "[cut_assembly]") {

    auto lg(ores::logging::make_logger(test_suite));
    const auto assembly = assemble_cut_body(a_signed_kind(), a_full_store());

    // The store holds no copy of a population, no fixed snapshot and no code
    // revision. A body that refused over each of them could never be taken, so
    // the part says so and the press stands.
    REQUIRE(part_of(assembly, "deal_population").snapshot_note.find("no copy") !=
            std::string::npos);
    REQUIRE(part_of(assembly, "market_data").snapshot_note.find("no snapshot reference") !=
            std::string::npos);
    REQUIRE(part_of(assembly, "engine_code").snapshot_note.find("no code revision") !=
            std::string::npos);
    REQUIRE(part_of(assembly, "engine_code").state == "closed");
}

TEST_CASE("a kind that requires no authorisation closes the same body", "[cut_assembly]") {

    auto lg(ores::logging::make_logger(test_suite));
    const auto assembly = assemble_cut_body(an_unsigned_kind(), a_full_store());

    REQUIRE(assembly.complete());
    REQUIRE(part_of(assembly, "kind").summary.find("no authorisation") != std::string::npos);
}

TEST_CASE("the taking identity names the account, the party and the instant the press records",
          "[cut_assembly]") {

    auto lg(ores::logging::make_logger(test_suite));
    const auto assembly = assemble_cut_body(a_signed_kind(), a_full_store());

    const auto row = part_of(assembly, "taking_identity");
    REQUIRE(row.state == "closed");
    REQUIRE(row.summary == "a.reyes for party 8f2a4c19-0000-4000-8000-000000000010");
    REQUIRE(row.snapshot_note.find("16:02:00") != std::string::npos);
}

TEST_CASE("a preview's taking identity names the instant it will record and no instant of its own",
          "[cut_assembly]") {

    auto lg(ores::logging::make_logger(test_suite));
    auto facts = a_full_store();
    facts.taking_instant = std::nullopt;

    const auto assembly = assemble_cut_body(a_signed_kind(), facts);

    const auto row = part_of(assembly, "taking_identity");
    REQUIRE(row.state == "closed");
    REQUIRE(row.snapshot_note.find("the press records") != std::string::npos);
}

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
#include "book_proposal_fixtures.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.refdata.api/generators/counterparty_change_generator.hpp"
#include "ores.refdata.api/generators/counterparty_identifier_change_generator.hpp"
#include "ores.refdata.api/generators/party_counterparty_change_generator.hpp"
#include "ores.refdata.api/messaging/counterparty_identifier_protocol.hpp"
#include "ores.refdata.api/messaging/counterparty_protocol.hpp"
#include "ores.refdata.api/messaging/party_counterparty_protocol.hpp"
#include "ores.refdata.core/repository/counterparty_change_repository.hpp"
#include "ores.refdata.core/repository/counterparty_identifier_change_repository.hpp"
#include "ores.refdata.core/repository/party_counterparty_change_repository.hpp"
#include "ores.refdata.core/service/counterparty_identifier_service.hpp"
#include "ores.refdata.core/service/counterparty_service.hpp"
#include "ores.refdata.core/service/party_counterparty_service.hpp"
#include "ores.refdata.service/service/counterparty_proposal.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

// A request of counterparty changes holds lines of three kinds, in one request:
// the counterparty, its identifiers and the links that let a party trade with it.
// These cases run the proposal service as an ordinary caller does, against the
// approval policy the schema seeds: every act on the three needs Operations.

namespace {

const std::string tags("[counterparty_proposal]");

using namespace ores::refdata::tests;
using ores::refdata::service::counterparty_lines;
using ores::refdata::service::counterparty_proposal_service;

namespace domain = ores::refdata::domain;
namespace messaging = ores::refdata::messaging;
using ores::utility::domain::precondition;
using ores::utility::domain::precondition_kind;

boost::uuids::uuid fresh() {
    static boost::uuids::random_generator gen;
    return gen();
}

domain::counterparty_change new_counterparty_line(ores::testing::scoped_database_helper& h) {
    auto gctx = ores::testing::make_generation_context(h);
    auto line = ores::refdata::generators::generate_synthetic_counterparty_change(gctx);
    line.entity_id = fresh();
    line.parent_counterparty_id = std::nullopt;
    line.image_id = std::nullopt;
    line.operation = "put";
    line.base_version = 0;
    line.change_reason_code = "system.test";
    return line;
}

domain::counterparty_identifier_change new_identifier_line(ores::testing::scoped_database_helper& h,
                                                           const boost::uuids::uuid& owner) {
    auto gctx = ores::testing::make_generation_context(h);
    auto line = ores::refdata::generators::generate_synthetic_counterparty_identifier_change(gctx);
    line.entity_id = fresh();
    line.counterparty_id = owner;
    line.id_scheme = "LEI";
    line.operation = "put";
    line.base_version = 0;
    line.change_reason_code = "system.test";
    return line;
}

domain::party_counterparty_change new_link_line(ores::testing::scoped_database_helper& h,
                                                const boost::uuids::uuid& party,
                                                const boost::uuids::uuid& counterparty) {
    auto gctx = ores::testing::make_generation_context(h);
    auto line = ores::refdata::generators::generate_synthetic_party_counterparty_change(gctx);
    line.party_id = party;
    line.counterparty_id = counterparty;
    line.operation = "put";
    line.base_version = 0;
    line.change_reason_code = "system.test";
    return line;
}

precondition creates() {
    return {precondition_kind::must_not_exist, std::nullopt};
}

/** Writes the line's counterparty to the live table, as an unapproved write would. */
domain::counterparty make_live(const world& w, const domain::counterparty_change& line) {
    ores::refdata::service::counterparty_service svc(w.ctx);
    messaging::put_counterparty_request req;
    req.change.write = {.id = line.entity_id,
                        .short_code = line.short_code,
                        .full_name = line.full_name,
                        .transliterated_name = line.transliterated_name,
                        .party_type = line.party_type,
                        .parent_counterparty_id = line.parent_counterparty_id,
                        .status = line.status,
                        .image_id = line.image_id};
    req.change.precondition = creates();
    req.intent = {"system.test", "A proposal test"};
    const auto response = svc.put_counterparty(req);
    REQUIRE(response.result.outcome == ores::utility::domain::outcome::ok);
    return *svc.get_counterparty(line.entity_id);
}

/** The line that rewrites a live counterparty with another full name. */
domain::counterparty_change rename(const domain::counterparty& live, const std::string& name) {
    domain::counterparty_change line;
    line.entity_id = live.id;
    line.short_code = live.short_code;
    line.full_name = name;
    line.transliterated_name = live.transliterated_name;
    line.party_type = live.party_type;
    line.parent_counterparty_id = live.parent_counterparty_id;
    line.status = live.status;
    line.image_id = live.image_id;
    line.operation = "put";
    line.base_version = live.version;
    line.change_reason_code = "system.test";
    return line;
}

counterparty_lines whole_request(ores::testing::scoped_database_helper& h, const world& w) {
    counterparty_lines lines;
    lines.counterparties.push_back(new_counterparty_line(h));
    const auto owner = lines.counterparties.front().entity_id;
    lines.identifiers.push_back(new_identifier_line(h, owner));
    lines.links.push_back(new_link_line(h, w.party_id, owner));
    return lines;
}

bool exists(const world& w, const boost::uuids::uuid& id) {
    return ores::refdata::service::counterparty_service(w.ctx).get_counterparty(id).has_value();
}

}

TEST_CASE("a_preview_accepts_a_new_counterparty_and_names_the_part_that_decides_it", tags) {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    counterparty_proposal_service svc(w.ctx);
    counterparty_lines lines;
    lines.counterparties.push_back(new_counterparty_line(h));

    const auto preview = svc.preview(lines);

    REQUIRE(preview.lines.size() == 1);
    CHECK(preview.lines[0].refusal.empty());
    CHECK(preview.lines[0].entity_type == "counterparty");
    CHECK_FALSE(preview.refused());
    CHECK(preview.part_codes == std::vector<std::string>{"operations"});
    CHECK(preview.lines[0].part_codes == std::vector<std::string>{"operations"});
}

TEST_CASE("a_preview_of_every_kind_numbers_the_lines_across_the_kinds_and_writes_nothing", tags) {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    counterparty_proposal_service svc(w.ctx);
    const auto lines = whole_request(h, w);

    const auto preview = svc.preview(lines);

    REQUIRE(preview.lines.size() == 3);
    CHECK_FALSE(preview.refused());
    CHECK(preview.lines[0].line_no == 1);
    CHECK(preview.lines[0].entity_type == "counterparty");
    CHECK(preview.lines[1].line_no == 2);
    CHECK(preview.lines[1].entity_type == "counterparty_identifier");
    CHECK(preview.lines[2].line_no == 3);
    CHECK(preview.lines[2].entity_type == "party_counterparty");
    // The identifier and the link name a counterparty that the first line made, so
    // the lines run in one transaction in the order of their kinds.
    CHECK_FALSE(exists(w, lines.counterparties.front().entity_id));
}

TEST_CASE("a_refused_line_does_not_stop_the_next_line_being_checked", tags) {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    counterparty_proposal_service svc(w.ctx);
    auto lines = whole_request(h, w);
    lines.identifiers.front().id_scheme = "NOT_A_SCHEME";

    const auto preview = svc.preview(lines);

    REQUIRE(preview.lines.size() == 3);
    CHECK(preview.lines[0].refusal.empty());
    CHECK_FALSE(preview.lines[1].refusal.empty());
    CHECK(preview.lines[2].refusal.empty());
    CHECK(preview.refused());
}

TEST_CASE("a_preview_names_the_columns_a_put_changes", tags) {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    counterparty_proposal_service svc(w.ctx);
    const auto live = make_live(w, new_counterparty_line(h));
    counterparty_lines lines;
    lines.counterparties.push_back(rename(live, "A new full name"));

    const auto preview = svc.preview(lines);

    REQUIRE(preview.lines.size() == 1);
    CHECK(preview.lines[0].refusal.empty());
    CHECK(preview.lines[0].columns == std::vector<std::string>{"full_name"});
}

TEST_CASE("a_raise_writes_one_request_with_every_line_in_the_order_of_the_kinds", tags) {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    counterparty_proposal_service svc(w.ctx);
    const auto lines = whole_request(h, w);

    const auto proposal = svc.raise(lines, "Onboard the clearing house");

    REQUIRE(proposal.request);
    CHECK(proposal.request->kind_code == "refdata.counterparty_change");
    CHECK(proposal.request->state_code == "waiting");
    const auto id = boost::uuids::to_string(proposal.request->id);
    const auto deltas = svc.deltas(id);
    REQUIRE(deltas.size() == 3);
    CHECK(deltas[0].entity_type == "counterparty");
    CHECK(deltas[1].entity_type == "counterparty_identifier");
    CHECK(deltas[2].entity_type == "party_counterparty");
    CHECK_FALSE(exists(w, lines.counterparties.front().entity_id));
}

TEST_CASE("a_raise_is_refused_when_a_line_is_refused_or_nothing_is_asked_or_said", tags) {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    counterparty_proposal_service svc(w.ctx);

    CHECK_FALSE(svc.raise({}, "Why").request);

    auto lines = whole_request(h, w);
    CHECK_FALSE(svc.raise(lines, "").request);

    lines.identifiers.front().id_scheme = "NOT_A_SCHEME";
    const auto refused = svc.raise(lines, "Why");
    CHECK_FALSE(refused.request);
    CHECK(refused.preview.refused());
}

TEST_CASE("an_apply_writes_every_line_once_in_order_and_marks_each_applied", tags) {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    counterparty_proposal_service svc(w.ctx);
    const auto lines = whole_request(h, w);
    const auto proposal = svc.raise(lines, "Onboard the clearing house");
    REQUIRE(proposal.request);
    approve_every_part(w, h, *proposal.request);
    const auto id = boost::uuids::to_string(proposal.request->id);

    const auto first = svc.apply(id);

    CHECK(first.applied);
    CHECK(first.lines_applied == 3);
    const auto owner = lines.counterparties.front().entity_id;
    CHECK(exists(w, owner));
    CHECK(ores::refdata::service::counterparty_identifier_service(w.ctx)
              .get_counterparty_identifier(lines.identifiers.front().entity_id)
              .has_value());
    messaging::get_party_counterparty_request link;
    link.key = {.party_id = w.party_id, .counterparty_id = owner};
    CHECK(ores::refdata::service::party_counterparty_service(w.ctx)
              .get_party_counterparty(link)
              .party_counterparty.has_value());
    for (const auto& d : svc.deltas(id))
        CHECK(d.applied);

    // A second apply finds every line applied and writes nothing.
    const auto again = svc.apply(id);
    CHECK(again.applied);
    CHECK(again.lines_applied == 0);
}

TEST_CASE("an_apply_refused_on_a_late_line_writes_nothing_and_moves_the_request", tags) {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    counterparty_proposal_service svc(w.ctx);
    const auto live = make_live(w, new_counterparty_line(h));
    counterparty_lines lines;
    lines.counterparties.push_back(new_counterparty_line(h));
    lines.counterparties.push_back(rename(live, "Renamed while it waited"));
    const auto proposal = svc.raise(lines, "Onboard and rename");
    REQUIRE(proposal.request);
    approve_every_part(w, h, *proposal.request);

    // The counterparty moves between the approval and the apply, so the second
    // line claims a version that is no longer current.
    auto moved =
        rename(*ores::refdata::service::counterparty_service(w.ctx).get_counterparty(live.id),
               "Renamed by someone else");
    {
        messaging::put_counterparty_request req;
        req.change.write = {.id = moved.entity_id,
                            .short_code = moved.short_code,
                            .full_name = moved.full_name,
                            .transliterated_name = moved.transliterated_name,
                            .party_type = moved.party_type,
                            .parent_counterparty_id = moved.parent_counterparty_id,
                            .status = moved.status,
                            .image_id = moved.image_id};
        req.change.precondition = {precondition_kind::must_match_version,
                                   static_cast<std::uint32_t>(moved.base_version)};
        req.intent = {"system.test", "Someone else"};
        REQUIRE(ores::refdata::service::counterparty_service(w.ctx)
                    .put_counterparty(req)
                    .result.outcome == ores::utility::domain::outcome::ok);
    }

    const auto id = boost::uuids::to_string(proposal.request->id);
    const auto result = svc.apply(id);

    CHECK_FALSE(result.applied);
    CHECK_FALSE(result.transient);
    CHECK(result.failed_line == 2);
    // The first line was a good one, and it is not written either.
    CHECK_FALSE(exists(w, lines.counterparties.front().entity_id));
    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    CHECK(lifecycle.request(id)->state_code == "apply_failed");
}

TEST_CASE("a_delta_shows_a_put_against_the_row_the_maker_read_even_after_it_applies", tags) {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    counterparty_proposal_service svc(w.ctx);
    const auto live = make_live(w, new_counterparty_line(h));
    counterparty_lines lines;
    lines.counterparties.push_back(rename(live, "A new full name"));
    const auto proposal = svc.raise(lines, "Rename it");
    REQUIRE(proposal.request);
    approve_every_part(w, h, *proposal.request);
    const auto id = boost::uuids::to_string(proposal.request->id);
    REQUIRE(svc.apply(id).applied);

    const auto deltas = svc.deltas(id);

    REQUIRE(deltas.size() == 1);
    CHECK(deltas[0].applied);
    const auto name = std::ranges::find(
        deltas[0].fields, "full_name", &ores::refdata::service::field_delta::name);
    REQUIRE(name != deltas[0].fields.end());
    CHECK(name->changed);
    CHECK(name->before == live.full_name);
    CHECK(name->after == "A new full name");
    const auto code = std::ranges::find(
        deltas[0].fields, "short_code", &ores::refdata::service::field_delta::name);
    REQUIRE(code != deltas[0].fields.end());
    CHECK_FALSE(code->changed);
}

TEST_CASE("a_recheck_runs_the_stored_lines_against_the_rows_as_they_are_now", tags) {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    counterparty_proposal_service svc(w.ctx);
    const auto live = make_live(w, new_counterparty_line(h));
    counterparty_lines lines;
    lines.counterparties.push_back(rename(live, "A new full name"));
    const auto proposal = svc.raise(lines, "Rename it");
    REQUIRE(proposal.request);
    const auto id = boost::uuids::to_string(proposal.request->id);
    CHECK_FALSE(svc.recheck(id).refused());

    auto stale = rename(live, "Someone else's name");
    messaging::put_counterparty_request req;
    req.change.write = {.id = stale.entity_id,
                        .short_code = stale.short_code,
                        .full_name = stale.full_name,
                        .transliterated_name = stale.transliterated_name,
                        .party_type = stale.party_type,
                        .parent_counterparty_id = stale.parent_counterparty_id,
                        .status = stale.status,
                        .image_id = stale.image_id};
    req.change.precondition = {precondition_kind::must_match_version,
                               static_cast<std::uint32_t>(stale.base_version)};
    req.intent = {"system.test", "Someone else"};
    REQUIRE(
        ores::refdata::service::counterparty_service(w.ctx).put_counterparty(req).result.outcome ==
        ores::utility::domain::outcome::ok);

    CHECK(svc.recheck(id).refused());
}

TEST_CASE("a_delete_line_for_a_live_link_previews_and_names_operations", tags) {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    counterparty_proposal_service svc(w.ctx);
    const auto live = make_live(w, new_counterparty_line(h));
    messaging::put_party_counterparty_request make;
    make.change.write = {.party_id = w.party_id, .counterparty_id = live.id};
    make.change.precondition = creates();
    make.intent = {"system.test", "A link"};
    REQUIRE(ores::refdata::service::party_counterparty_service(w.ctx)
                .put_party_counterparty(make)
                .result.outcome == ores::utility::domain::outcome::ok);
    counterparty_lines lines;
    auto removal = new_link_line(h, w.party_id, live.id);
    removal.operation = "delete";
    lines.links.push_back(removal);

    const auto preview = svc.preview(lines);

    REQUIRE(preview.lines.size() == 1);
    CHECK(preview.lines[0].refusal.empty());
    CHECK(preview.lines[0].operation == "delete");
    CHECK(preview.part_codes == std::vector<std::string>{"operations"});
}

TEST_CASE("a_request_that_removes_a_counterparty_and_its_link_removes_the_link_first", tags) {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    counterparty_proposal_service svc(w.ctx);
    const auto live = make_live(w, new_counterparty_line(h));
    messaging::put_party_counterparty_request make;
    make.change.write = {.party_id = w.party_id, .counterparty_id = live.id};
    make.change.precondition = creates();
    make.intent = {"system.test", "A link"};
    REQUIRE(ores::refdata::service::party_counterparty_service(w.ctx)
                .put_party_counterparty(make)
                .result.outcome == ores::utility::domain::outcome::ok);
    counterparty_lines lines;
    auto gone = rename(live, live.full_name);
    gone.operation = "delete";
    lines.counterparties.push_back(gone);
    auto removal = new_link_line(h, w.party_id, live.id);
    removal.operation = "delete";
    lines.links.push_back(removal);

    const auto preview = svc.preview(lines);

    // The counterparty is line 1 and the link line 2, as the maker gave them,
    // although the link is removed first.
    REQUIRE(preview.lines.size() == 2);
    CHECK(preview.lines[0].line_no == 1);
    CHECK(preview.lines[0].entity_type == "counterparty");
    CHECK(preview.lines[1].line_no == 2);
    CHECK(preview.lines[1].entity_type == "party_counterparty");
    CHECK_FALSE(preview.refused());
}

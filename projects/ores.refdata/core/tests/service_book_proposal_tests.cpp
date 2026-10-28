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
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.inbox.core/repository/approval_policy_repository.hpp"
#include "ores.inbox.core/service/approval_lifecycle.hpp"
#include "ores.refdata.api/generators/book_change_generator.hpp"
#include "ores.refdata.api/generators/currency_generator.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.api/generators/portfolio_generator.hpp"
#include "ores.refdata.core/repository/book_change_repository.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.refdata.core/repository/portfolio_repository.hpp"
#include "ores.refdata.core/service/book_service.hpp"
#include "ores.refdata.service/service/book_proposal.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

using ores::refdata::domain::book_change;
using ores::refdata::service::book_proposal_service;
using namespace ores::refdata::tests;

}

TEST_CASE("a_preview_accepts_a_valid_new_book_and_names_every_part_that_decides_it",
          "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);

    const auto preview = svc.preview({new_book_line(h, w)});

    REQUIRE(preview.lines.size() == 1);
    CHECK(preview.lines[0].refusal.empty());
    CHECK_FALSE(preview.refused());
    const std::vector<std::string> expected{"controller", "finance", "market_risk", "operations"};
    CHECK(preview.part_codes == expected);
}

TEST_CASE("an_opening_book_asks_the_controller_and_a_change_to_a_live_book_does_not",
          "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);

    const auto opening = svc.raise({new_book_line(h, w)}, "Open the desk");
    REQUIRE(opening.request);
    std::vector<std::string> parts;
    for (const auto& part : lifecycle.parts_of(boost::uuids::to_string(opening.request->id)))
        parts.push_back(part.code);
    const std::vector<std::string> expected{"controller", "finance", "market_risk", "operations"};
    CHECK(parts == expected);
    // The controller holds the first answer order, so it is the only part open.
    const auto open = lifecycle.open_parts_of(boost::uuids::to_string(opening.request->id));
    REQUIRE(open.size() == 1);
    CHECK(open.front().code == "controller");

    const auto created = new_book_line(h, w);
    const auto live = make_live(w, created);
    auto edit = edit_line(live, created);
    edit.cost_center = live.cost_center == "CC-1" ? "CC-2" : "CC-1";

    const auto change = svc.raise({edit}, "Move the cost centre");
    REQUIRE(change.request);
    std::vector<std::string> change_parts;
    for (const auto& part : lifecycle.parts_of(boost::uuids::to_string(change.request->id)))
        change_parts.push_back(part.code);
    CHECK(std::ranges::find(change_parts, "controller") == change_parts.end());
}

TEST_CASE("a_preview_writes_nothing", "[book_proposal]") {

    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    const auto line = new_book_line(h, w);
    const auto before = pending_count(w);

    const auto preview = svc.preview({line});
    REQUIRE_FALSE(preview.refused());

    ores::refdata::service::book_service books(w.ctx);
    CHECK_FALSE(books.get_book(line.entity_id).has_value());
    CHECK(pending_count(w) == before);
}

TEST_CASE("a_refused_line_does_not_stop_the_next_line_being_checked", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    auto bad = new_book_line(h, w);
    bad.functional_currency = "QQQ";
    const auto good = new_book_line(h, w);

    const auto preview = svc.preview({bad, good});

    REQUIRE(preview.lines.size() == 2);
    CHECK_FALSE(preview.lines[0].refusal.empty());
    CHECK(preview.lines[1].refusal.empty());
    CHECK(preview.refused());
}

TEST_CASE("a_preview_refuses_a_line_that_read_a_version_that_is_no_longer_current",
          "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    const auto created = new_book_line(h, w);
    const auto live = make_live(w, created);

    auto stale = edit_line(live, created);
    stale.base_version = live.version + 5;
    stale.description = "edited";

    const auto preview = svc.preview({stale});

    REQUIRE(preview.lines.size() == 1);
    CHECK_FALSE(preview.lines[0].refusal.empty());
}

TEST_CASE("a_change_to_one_column_names_only_the_part_of_that_column", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    const auto created = new_book_line(h, w);
    const auto live = make_live(w, created);

    auto edit = edit_line(live, created);
    edit.book_status = live.book_status == "Active" ? "Frozen" : "Active";

    const auto preview = svc.preview({edit});

    REQUIRE(preview.lines.size() == 1);
    CHECK(preview.lines[0].refusal.empty());
    CHECK(preview.lines[0].columns == std::vector<std::string>{"book_status"});
    CHECK(preview.lines[0].part_codes == std::vector<std::string>{"operations"});
    CHECK(preview.part_codes == std::vector<std::string>{"operations"});
}

TEST_CASE("a_raise_writes_the_request_its_parts_and_its_lines_together", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    const auto line = new_book_line(h, w);

    const auto proposal = svc.raise({line}, "Open the desk");

    REQUIRE(proposal.request);
    CHECK(proposal.request->state_code == "waiting");
    CHECK(proposal.request->kind_code == "refdata.book_change");

    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    const auto id = boost::uuids::to_string(proposal.request->id);
    const auto parts = lifecycle.parts_of(id);
    // An opening asks the controller first and then the three functions that
    // own the columns it writes.
    CHECK(parts.size() == 4);
    CHECK(parts.front().code == "controller");

    const auto rows = ores::refdata::repository::book_change_repository{}.read_latest(w.ctx);
    const auto mine = std::ranges::count_if(
        rows, [&](const auto& r) { return r.request_id == proposal.request->id; });
    CHECK(mine == 1);

    ores::refdata::service::book_service books(w.ctx);
    CHECK_FALSE(books.get_book(line.entity_id).has_value());
}

TEST_CASE("a_raise_with_a_refused_line_raises_nothing", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    auto bad = new_book_line(h, w);
    bad.functional_currency = "QQQ";
    const auto before = pending_count(w);

    const auto proposal = svc.raise({bad}, "Open the desk");

    CHECK_FALSE(proposal.request.has_value());
    CHECK(proposal.preview.refused());
    CHECK(pending_count(w) == before);
}

TEST_CASE("a_change_the_policy_does_not_gate_needs_no_request", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    const auto created = new_book_line(h, w);
    const auto live = make_live(w, created);

    auto edit = edit_line(live, created);
    edit.description = "Only the words change";

    const auto proposal = svc.raise({edit}, "Reword");

    CHECK_FALSE(proposal.request.has_value());
    CHECK(proposal.preview.part_codes.empty());
}

TEST_CASE("two_lines_of_one_request_cannot_share_a_line_number", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    const auto proposal = svc.raise({new_book_line(h, w)}, "Open the desk");
    REQUIRE(proposal.request);

    static boost::uuids::random_generator gen;
    auto second = new_book_line(h, w);
    second.tenant_id = w.ctx.tenant_id();
    second.id = gen();
    second.request_id = proposal.request->id;
    second.line_no = 1;
    second.modified_by = w.ctx.actor();
    second.performed_by = w.ctx.actor();

    ores::refdata::repository::book_change_repository repo;
    CHECK_THROWS(repo.write(w.ctx, second));
}

TEST_CASE("a_line_for_a_request_that_does_not_exist_is_refused", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    static boost::uuids::random_generator gen;
    auto line = new_book_line(h, w);
    line.tenant_id = w.ctx.tenant_id();
    line.id = gen();
    line.request_id = gen();
    line.line_no = 1;
    line.modified_by = h.db_user();
    line.performed_by = h.db_user();

    ores::refdata::repository::book_change_repository repo;
    CHECK_THROWS(repo.write(w.ctx, line));
}

TEST_CASE("a_raise_with_no_lines_or_no_reason_raises_nothing", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);

    const auto none = svc.raise({}, "Open the desk");
    CHECK_FALSE(none.request.has_value());
    CHECK_FALSE(none.message.empty());

    const auto unexplained = svc.raise({new_book_line(h, w)}, "");
    CHECK_FALSE(unexplained.request.has_value());
    CHECK_FALSE(unexplained.message.empty());
}

TEST_CASE("a_line_with_an_unknown_operation_is_refused", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    auto line = new_book_line(h, w);
    line.operation = "x'); drop table ores_refdata_books_tbl; --";

    const auto preview = svc.preview({line});

    REQUIRE(preview.lines.size() == 1);
    CHECK_FALSE(preview.lines[0].refusal.empty());
    ores::refdata::service::book_service books(w.ctx);
    CHECK_FALSE(books.get_book(line.entity_id).has_value());
}

TEST_CASE("a_part_a_tenant_adds_to_an_act_is_named_with_the_system_parts", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    const auto created = new_book_line(h, w);
    const auto live = make_live(w, created);

    ores::inbox::domain::approval_policy extra;
    extra.tenant_id = w.ctx.tenant_id();
    extra.code = "book.put.description.tenant";
    extra.name = "Book description by the tenant";
    extra.description = "The tenant asks the controller to see a description change";
    extra.entity_type = "book";
    extra.operation = "put";
    extra.field_name = "description";
    extra.part_code = "controller";
    extra.display_order = 900;
    extra.modified_by = w.ctx.actor();
    extra.change_reason_code = "system.test";
    ores::inbox::repository::approval_policy_repository policies;
    policies.write(w.ctx, extra);
    // The tenant is shared by every case of a run, so the row is removed
    // however the case ends, or it would add a part to the other cases.
    struct remove_on_exit {
        ores::inbox::repository::approval_policy_repository& repo;
        const ores::database::context& ctx;
        std::string code;
        ~remove_on_exit() {
            try {
                repo.remove(ctx, code);
            } catch (...) {
            }
        }
    } cleanup{policies, w.ctx, extra.code};

    auto edit = edit_line(live, created);
    edit.description = "Only the words change";

    const auto preview = book_proposal_service(w.ctx).preview({edit});

    CHECK(preview.part_codes == std::vector<std::string>{"controller"});
}

TEST_CASE("an_apply_writes_every_line_once_and_marks_each_applied", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    const auto line = new_book_line(h, w);
    const auto proposal = svc.raise({line}, "Open the desk");
    REQUIRE(proposal.request);
    approve_every_part(w, h, *proposal.request);
    const auto id = boost::uuids::to_string(proposal.request->id);

    const auto first = svc.apply(id);

    CHECK(first.applied);
    CHECK(first.lines_applied == 1);
    ores::refdata::service::book_service books(w.ctx);
    const auto written = books.get_book(line.entity_id);
    REQUIRE(written.has_value());
    CHECK(written->name == line.name);
    const auto rows = ores::refdata::repository::book_change_repository{}.read_latest_by_request_id(
        w.ctx, id, 0, 10);
    REQUIRE(rows.size() == 1);
    CHECK(rows.front().applied);

    const auto again = svc.apply(id);
    CHECK(again.applied);
    CHECK(again.lines_applied == 0);
    CHECK(books.get_book(line.entity_id)->version == written->version);
}

TEST_CASE("a_refused_line_rolls_the_whole_apply_back_and_fails_the_request", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);

    const auto existing = new_book_line(h, w);
    const auto live = make_live(w, existing);
    auto edit = edit_line(live, existing);
    edit.description = "Reworded";
    edit.book_status = live.book_status == "Active" ? "Frozen" : "Active";
    const auto added = new_book_line(h, w);

    const auto proposal = svc.raise({added, edit}, "Open one desk, change another");
    REQUIRE(proposal.request);
    approve_every_part(w, h, *proposal.request);

    // Someone else changes the book between the raise and the apply, so the
    // second line now claims a version that is no longer current.
    ores::refdata::service::book_service books(w.ctx);
    auto moved = *books.get_book(live.id);
    moved.description = "Changed by someone else";
    moved.change_reason_code = "system.test";
    write_live(w, moved);

    const auto id = boost::uuids::to_string(proposal.request->id);
    const auto result = svc.apply(id);

    CHECK_FALSE(result.applied);
    CHECK_FALSE(result.transient);
    CHECK(result.failed_line == 2);
    CHECK_FALSE(result.refusal.empty());
    CHECK_FALSE(books.get_book(added.entity_id).has_value());
    const auto rows = ores::refdata::repository::book_change_repository{}.read_latest_by_request_id(
        w.ctx, id, 0, 10);
    CHECK(std::ranges::none_of(rows, [](const auto& r) { return r.applied; }));
    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    const auto failed = lifecycle.request(id);
    CHECK(failed->state_code == "apply_failed");
    CHECK(failed->change_commentary.starts_with("Line 2:"));
}

TEST_CASE("a_request_that_is_not_approved_is_not_applied", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    const auto line = new_book_line(h, w);
    const auto proposal = svc.raise({line}, "Open the desk");
    REQUIRE(proposal.request);

    const auto result = svc.apply(boost::uuids::to_string(proposal.request->id));

    CHECK_FALSE(result.applied);
    CHECK_FALSE(result.refusal.empty());
    ores::refdata::service::book_service books(w.ctx);
    CHECK_FALSE(books.get_book(line.entity_id).has_value());
    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    CHECK(lifecycle.request(boost::uuids::to_string(proposal.request->id))->state_code ==
          "waiting");
}

TEST_CASE("a_request_that_failed_to_apply_is_not_applied_again", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    const auto line = new_book_line(h, w);
    const auto proposal = svc.raise({line}, "Open the desk");
    REQUIRE(proposal.request);
    approve_every_part(w, h, *proposal.request);
    const auto id = boost::uuids::to_string(proposal.request->id);
    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    REQUIRE(lifecycle.fail_apply(id, "Could not apply").outcome == "ok");

    const auto result = svc.apply(id);

    CHECK_FALSE(result.applied);
    CHECK_FALSE(result.transient);
    ores::refdata::service::book_service books(w.ctx);
    CHECK_FALSE(books.get_book(line.entity_id).has_value());
}

TEST_CASE("a_request_over_the_line_limit_is_not_raised", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    std::vector<book_change> lines;
    for (std::size_t i = 0; i <= book_proposal_service::max_lines; ++i)
        lines.push_back(new_book_line(h, w));

    const auto proposal = svc.raise(lines, "Too many");

    CHECK_FALSE(proposal.request.has_value());
    CHECK_FALSE(proposal.message.empty());
}

TEST_CASE("a_recheck_reads_the_stored_lines_against_the_live_book", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    const auto existing = new_book_line(h, w);
    const auto live = make_live(w, existing);
    auto edit = edit_line(live, existing);
    edit.book_status = live.book_status == "Active" ? "Frozen" : "Active";
    const auto proposal = svc.raise({edit}, "Change the status");
    REQUIRE(proposal.request);
    const auto id = boost::uuids::to_string(proposal.request->id);
    CHECK_FALSE(svc.recheck(id).refused());

    ores::refdata::service::book_service books(w.ctx);
    auto moved = *books.get_book(live.id);
    moved.description = "Changed by someone else";
    moved.change_reason_code = "system.test";
    write_live(w, moved);

    CHECK(svc.recheck(id).refused());
}

TEST_CASE("only_the_database_refusing_a_write_is_a_refusal_and_not_the_infrastructure",
          "[book_proposal]") {
    using ores::refdata::service::is_infrastructure_failure;
    // The client cannot reach the server: no label from the server at all.
    CHECK(is_infrastructure_failure("server closed the connection unexpectedly"));
    CHECK(is_infrastructure_failure("Cannot commit the transaction"));
    // The server ends the connection.
    CHECK(is_infrastructure_failure("FATAL:  terminating connection due to administrator command"));
    // The server answers, so it is a refusal, whatever the text goes on to say.
    CHECK_FALSE(
        is_infrastructure_failure("Executing INSERT failed: ERROR:  Invalid currency: QQQ"));
    CHECK_FALSE(is_infrastructure_failure(
        "Executing INSERT failed: ERROR:  Invalid name: lost connection timed out"));
}

TEST_CASE("the_parties_of_a_request_are_found_without_a_party_scope", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    const auto proposal = svc.raise({new_book_line(h, w)}, "Open the desk");
    REQUIRE(proposal.request);
    const auto id = boost::uuids::to_string(proposal.request->id);

    // The apply workflow runs for the tenant and has no party, so it cannot read
    // the lines. The function reads past the party policy for the parties alone.
    const auto tenant_only = h.context().with_tenant(h.tenant_id(), "");
    auto lg = ores::logging::make_logger("ores.refdata.tests");
    const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
        tenant_only,
        "select p::text from ores_refdata_book_change_parties_fn($1::uuid) as p",
        {id},
        lg,
        "Reading the parties of a request");

    REQUIRE(rows.size() == 1);
    CHECK(rows.front().front() == boost::uuids::to_string(w.party_id));
    const auto visible =
        ores::refdata::repository::book_change_repository{}.read_latest_by_request_id(
            tenant_only, id, 0, 10);
    CHECK(visible.empty());
}

TEST_CASE("another_tenant_is_told_no_parties_for_a_request_it_does_not_own", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    const auto proposal = svc.raise({new_book_line(h, w)}, "Open the desk");
    REQUIRE(proposal.request);
    const auto id = boost::uuids::to_string(proposal.request->id);

    auto lg = ores::logging::make_logger("ores.refdata.tests");
    const auto other = h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");
    const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
        other,
        "select p::text from ores_refdata_book_change_parties_fn($1::uuid) as p",
        {id},
        lg,
        "Reading the parties of another tenant's request");

    CHECK(rows.empty());
}

TEST_CASE("a_requests_deltas_list_each_column_beside_the_live_book", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    const auto created = new_book_line(h, w);
    const auto live = make_live(w, created);
    auto edit = edit_line(live, created);
    edit.book_status = live.book_status == "Active" ? "Frozen" : "Active";
    const auto proposal = svc.raise({edit}, "Change the status");
    REQUIRE(proposal.request);

    const auto deltas = svc.deltas(boost::uuids::to_string(proposal.request->id));

    REQUIRE(deltas.size() == 1);
    CHECK(deltas[0].operation == "put");
    CHECK(deltas[0].base_version == live.version);
    const auto status = std::ranges::find(
        deltas[0].fields, "book_status", &ores::refdata::service::field_delta::name);
    REQUIRE(status != deltas[0].fields.end());
    CHECK(status->changed);
    CHECK(status->before == live.book_status);
    CHECK(status->after == edit.book_status);
    const auto description = std::ranges::find(
        deltas[0].fields, "description", &ores::refdata::service::field_delta::name);
    REQUIRE(description != deltas[0].fields.end());
    CHECK_FALSE(description->changed);
}

TEST_CASE("a_line_that_creates_a_book_has_no_before", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    const auto line = new_book_line(h, w);
    const auto proposal = svc.raise({line}, "Open the desk");
    REQUIRE(proposal.request);

    const auto deltas = svc.deltas(boost::uuids::to_string(proposal.request->id));

    REQUIRE(deltas.size() == 1);
    REQUIRE_FALSE(deltas[0].fields.empty());
    for (const auto& f : deltas[0].fields) {
        CHECK(f.before.empty());
        CHECK(f.changed);
    }
}

TEST_CASE("a_line_that_has_applied_still_shows_what_it_changed", "[book_proposal]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    const auto created = new_book_line(h, w);
    const auto live = make_live(w, created);
    auto edit = edit_line(live, created);
    edit.book_status = live.book_status == "Active" ? "Frozen" : "Active";
    const auto proposal = svc.raise({edit}, "Change the status");
    REQUIRE(proposal.request);
    approve_every_part(w, h, *proposal.request);
    const auto id = boost::uuids::to_string(proposal.request->id);
    REQUIRE(svc.apply(id).applied);

    const auto deltas = svc.deltas(id);

    REQUIRE(deltas.size() == 1);
    CHECK(deltas[0].applied);
    const auto status = std::ranges::find(
        deltas[0].fields, "book_status", &ores::refdata::service::field_delta::name);
    REQUIRE(status != deltas[0].fields.end());
    CHECK(status->changed);
    CHECK(status->before == live.book_status);
    CHECK(status->after == edit.book_status);
}

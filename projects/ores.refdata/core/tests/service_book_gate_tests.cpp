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
#include "ores.database/repository/unit_of_work.hpp"
#include "ores.inbox.core/repository/approval_policy_repository.hpp"
#include "ores.refdata.core/service/book_service.hpp"
#include "ores.refdata.service/service/book_proposal.hpp"
#include "ores.service/messaging/approval_gate.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>

// The approval policy gates the book. A write that does not carry out a line of
// an approved request is refused with approval_required, and nothing is
// written. These cases call the service the handlers call, as an ordinary
// caller does, so they say what a script or another client would meet.

namespace {

using namespace ores::refdata::tests;
using ores::refdata::service::book_proposal_service;
using ores::refdata::service::book_service;
using ores::utility::domain::outcome;
using ores::utility::domain::precondition;
using ores::utility::domain::precondition_kind;

ores::refdata::messaging::book_write write_of(const ores::refdata::domain::book_change& l) {
    return {.id = l.entity_id,
            .party_id = l.party_id,
            .name = l.name,
            .description = l.description,
            .parent_portfolio_id = l.parent_portfolio_id,
            .owner_unit_id = l.owner_unit_id,
            .functional_currency = l.functional_currency,
            .gl_account_ref = l.gl_account_ref,
            .cost_center = l.cost_center,
            .book_status = l.book_status,
            .regulatory_book_type = l.regulatory_book_type,
            .book_purpose_type = l.book_purpose_type,
            .ledger_feed_type = l.ledger_feed_type,
            .is_sweepable = l.is_sweepable,
            .rates_centre_code = l.rates_centre_code,
            .sandbox_id = l.sandbox_id};
}

ores::refdata::messaging::put_book_request put_of(const ores::refdata::domain::book_change& l,
                                                  precondition claim) {
    ores::refdata::messaging::put_book_request req;
    req.change.write = write_of(l);
    req.change.precondition = claim;
    req.intent = {"system.test", "A gate test"};
    return req;
}

precondition creates() {
    return {precondition_kind::must_not_exist, std::nullopt};
}

precondition reads(int version) {
    return {precondition_kind::must_match_version, static_cast<std::uint32_t>(version)};
}

/**
 * @brief Removes a tenant policy row however the case ends.
 *
 * The tenant is shared by every case of a run, so a row left behind would gate
 * the other cases.
 */
struct policy_row {
    ores::inbox::repository::approval_policy_repository repo;
    ores::database::context ctx;
    std::string code;

    policy_row(const world& w, std::string c, std::string operation)
        : ctx(w.ctx)
        , code(std::move(c)) {
        ores::inbox::domain::approval_policy row;
        row.tenant_id = w.ctx.tenant_id();
        row.code = code;
        row.name = "Gate test " + code;
        row.description = "The tenant gates the act for a test";
        row.entity_type = "book";
        row.operation = std::move(operation);
        row.part_code = "operations";
        row.display_order = 901;
        row.modified_by = w.ctx.actor();
        row.change_reason_code = "system.test";
        repo.write(ctx, row);
    }

    ~policy_row() {
        try {
            repo.remove(ctx, code);
        } catch (...) {
        }
    }
};

}

TEST_CASE("a_gated_put_with_no_request_writes_nothing_and_answers_approval_required",
          "[book_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_service svc(w.ctx);
    const auto line = new_book_line(h, w);

    const auto response = svc.put_book(put_of(line, creates()));

    CHECK(response.result.outcome == outcome::denied);
    CHECK(response.result.code == "approval_required");
    CHECK_FALSE(svc.get_book(line.entity_id).has_value());
}

TEST_CASE("a_change_to_a_gated_column_is_refused_and_the_live_row_is_left_as_it_was",
          "[book_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_service svc(w.ctx);
    const auto created = new_book_line(h, w);
    const auto live = make_live(w, created);
    auto edit = edit_line(live, created);
    edit.book_status = live.book_status == "Active" ? "Frozen" : "Active";

    const auto response = svc.put_book(put_of(edit, reads(live.version)));

    CHECK(response.result.code == "approval_required");
    const auto after = svc.get_book(live.id);
    REQUIRE(after.has_value());
    CHECK(after->book_status == live.book_status);
    CHECK(after->version == live.version);
}

TEST_CASE("a_change_to_a_column_no_policy_names_is_written_directly", "[book_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_service svc(w.ctx);
    const auto created = new_book_line(h, w);
    const auto live = make_live(w, created);
    auto edit = edit_line(live, created);
    edit.description = "Only the words change";

    const auto response = svc.put_book(put_of(edit, reads(live.version)));

    CHECK(response.result.outcome == outcome::ok);
    CHECK(svc.get_book(live.id)->description == "Only the words change");
}

TEST_CASE("a_removal_is_written_directly_when_the_policy_names_no_part_for_it", "[book_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_service svc(w.ctx);
    const auto live = make_live(w, new_book_line(h, w));
    ores::refdata::messaging::delete_book_request req;
    req.removal.key.name = live.name;
    req.removal.precondition = reads(live.version);
    req.intent = {"system.test", "A gate test"};

    const auto response = svc.delete_book(req);

    CHECK(response.result.outcome == outcome::ok);
    CHECK_FALSE(svc.get_book(live.id).has_value());
}

TEST_CASE("a_removal_the_policy_gates_is_refused_and_the_row_stays", "[book_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_service svc(w.ctx);
    const auto live = make_live(w, new_book_line(h, w));
    const policy_row gate(w, "book.delete.gate_test", "delete");
    ores::refdata::messaging::delete_book_request req;
    req.removal.key.name = live.name;
    req.removal.precondition = reads(live.version);
    req.intent = {"system.test", "A gate test"};

    const auto response = svc.delete_book(req);

    CHECK(response.result.code == "approval_required");
    CHECK(svc.get_book(live.id).has_value());
}

TEST_CASE("a_batch_removal_the_policy_gates_is_refused_and_the_rows_stay", "[book_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_service svc(w.ctx);
    const auto live = make_live(w, new_book_line(h, w));
    const policy_row gate(w, "book.delete.gate_test", "delete");
    ores::refdata::messaging::delete_many_books_request req;
    ores::refdata::messaging::book_removal removal;
    removal.key.name = live.name;
    req.removals.push_back(removal);
    req.intent = {"system.test", "A gate test"};

    const auto response = svc.delete_many_books(req);

    CHECK(response.result.code == "approval_required");
    CHECK(svc.get_book(live.id).has_value());
}

TEST_CASE("an_approved_request_opens_the_gate_for_the_line_it_holds", "[book_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    const auto line = new_book_line(h, w);
    const auto proposal = book_proposal_service(w.ctx).raise({line}, "Open the desk");
    REQUIRE(proposal.request);
    approve_every_part(w, h, *proposal.request);
    book_service svc(w.ctx);
    svc.carry_out_request(boost::uuids::to_string(proposal.request->id));

    const auto response = svc.put_book(put_of(line, creates()));

    CHECK(response.result.outcome == outcome::ok);
    CHECK(svc.get_book(line.entity_id).has_value());
}

TEST_CASE("a_request_that_is_not_yet_approved_opens_nothing", "[book_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    const auto line = new_book_line(h, w);
    const auto proposal = book_proposal_service(w.ctx).raise({line}, "Open the desk");
    REQUIRE(proposal.request);
    book_service svc(w.ctx);
    svc.carry_out_request(boost::uuids::to_string(proposal.request->id));

    const auto response = svc.put_book(put_of(line, creates()));

    CHECK(response.result.code == "approval_required");
    CHECK_FALSE(svc.get_book(line.entity_id).has_value());
}

TEST_CASE("an_approved_request_opens_no_write_it_does_not_hold", "[book_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    const auto held = new_book_line(h, w);
    const auto other = new_book_line(h, w);
    const auto proposal = book_proposal_service(w.ctx).raise({held}, "Open one desk");
    REQUIRE(proposal.request);
    approve_every_part(w, h, *proposal.request);
    book_service svc(w.ctx);
    svc.carry_out_request(boost::uuids::to_string(proposal.request->id));

    const auto response = svc.put_book(put_of(other, creates()));

    CHECK(response.result.code == "approval_required");
    CHECK_FALSE(svc.get_book(other.entity_id).has_value());
}

TEST_CASE("an_approved_request_opens_no_write_that_claims_another_version", "[book_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    const auto created = new_book_line(h, w);
    const auto live = make_live(w, created);
    auto edit = edit_line(live, created);
    edit.book_status = live.book_status == "Active" ? "Frozen" : "Active";
    const auto proposal = book_proposal_service(w.ctx).raise({edit}, "Change the status");
    REQUIRE(proposal.request);
    approve_every_part(w, h, *proposal.request);
    book_service svc(w.ctx);
    svc.carry_out_request(boost::uuids::to_string(proposal.request->id));

    const auto response = svc.put_book(put_of(edit, reads(live.version + 1)));

    CHECK(response.result.code == "approval_required");
}

TEST_CASE("a_line_opens_the_gate_once", "[book_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    const auto line = new_book_line(h, w);
    book_proposal_service proposals(w.ctx);
    const auto proposal = proposals.raise({line}, "Open the desk");
    REQUIRE(proposal.request);
    approve_every_part(w, h, *proposal.request);
    const auto id = boost::uuids::to_string(proposal.request->id);
    REQUIRE(proposals.apply(id).applied);
    book_service svc(w.ctx);
    svc.carry_out_request(id);
    auto again = edit_line(*svc.get_book(line.entity_id), line);
    again.base_version = 0;

    const auto response = svc.put_book(put_of(again, creates()));

    CHECK_FALSE(response.result.outcome == outcome::ok);
}

TEST_CASE("a_rehearsal_is_let_through_the_gate_and_leaves_nothing_behind", "[book_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    const auto line = new_book_line(h, w);
    {
        // The transaction is never committed, as a preview's is not.
        ores::database::repository::unit_of_work uow(w.ctx);
        book_service rehearsal(uow.ctx());
        rehearsal.rehearse();

        const auto response = rehearsal.put_book(put_of(line, creates()));

        CHECK(response.result.outcome == outcome::ok);
    }

    CHECK_FALSE(book_service(w.ctx).get_book(line.entity_id).has_value());
}

TEST_CASE("the_in_process_save_and_remove_methods_are_refused_for_a_gated_entity", "[book_gate]") {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_service svc(w.ctx);
    const auto live = make_live(w, new_book_line(h, w));

    CHECK_THROWS_AS(svc.save_book(live), std::logic_error);
    CHECK_THROWS_AS(svc.save_books({live}), std::logic_error);
    CHECK_THROWS_AS(svc.delete_book(live.id), std::logic_error);
    CHECK_THROWS_AS(svc.delete_books({boost::uuids::to_string(live.id)}), std::logic_error);
    CHECK(svc.get_book(live.id).has_value());
}

TEST_CASE("the_policy_answers_free_required_or_approved_and_reads_a_bad_request_id_as_none",
          "[book_gate]") {
    using ores::service::messaging::gate_verdict;
    using ores::service::messaging::gate_verdict_of;
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    const auto line = new_book_line(h, w);
    const auto proposal = book_proposal_service(w.ctx).raise({line}, "Open the desk");
    REQUIRE(proposal.request);
    const auto id = boost::uuids::to_string(proposal.request->id);

    CHECK(gate_verdict_of(w.ctx, "book", "put", {"description"}, "") == gate_verdict::free);
    CHECK(gate_verdict_of(w.ctx, "book", "put", {"cost_center"}, "") == gate_verdict::required);
    CHECK(gate_verdict_of(w.ctx, "book", "put", {"cost_center"}, "not-a-uuid") ==
          gate_verdict::required);
    CHECK(gate_verdict_of(w.ctx, "book", "put", {"cost_center"}, id) == gate_verdict::required);
    approve_every_part(w, h, *proposal.request);
    CHECK(gate_verdict_of(w.ctx, "book", "put", {"cost_center"}, id) == gate_verdict::approved);
    CHECK(gate_verdict_of(w.ctx, "book", "put", {"description"}, id) == gate_verdict::free);
}

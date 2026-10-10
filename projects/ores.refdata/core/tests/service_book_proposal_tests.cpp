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
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
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
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

const std::string tags("[book_proposal]");

using ores::refdata::domain::book_change;
using ores::refdata::service::book_proposal_service;

/**
 * @brief The rows a valid book points at, and the context that may write it.
 */
struct world {
    std::string currency;
    boost::uuids::uuid party_id;
    boost::uuids::uuid portfolio_id;
    ores::database::context ctx;
};

world seed_world(ores::testing::scoped_database_helper& h) {
    auto gctx = ores::testing::make_generation_context(h);
    auto& base = h.context();

    // The maker is an account of the tenant, and the context acts as it, so
    // the raise finds the account it raises for.
    auto maker = ores::iam::generators::generate_synthetic_account(gctx);
    maker.change_reason_code = "system.test";
    ores::iam::repository::account_repository{}.write(base, maker);

    auto currency = ores::refdata::generators::generate_synthetic_currency(gctx);
    currency.change_reason_code = "system.test";
    ores::refdata::repository::currency_repository{}.write(base, currency);

    auto sentinel = ores::refdata::generators::generate_synthetic_currency(gctx);
    sentinel.iso_code = "X-0";
    sentinel.change_reason_code = "system.test";
    ores::refdata::repository::currency_repository{}.write(base, {sentinel});

    auto attach_to_root = [&](auto& party) {
        for (const auto& e : ores::refdata::repository::party_repository{}.read_latest(base)) {
            if (e.tenant_id == party.tenant_id) {
                party.parent_party_id = e.id;
                break;
            }
        }
    };
    auto party = ores::refdata::generators::generate_synthetic_party(gctx);
    party.change_reason_code = "system.test";
    attach_to_root(party);
    ores::refdata::repository::party_repository{}.write(base, party);

    auto portfolio_party = ores::refdata::generators::generate_synthetic_party(gctx);
    portfolio_party.change_reason_code = "system.test";
    attach_to_root(portfolio_party);
    ores::refdata::repository::party_repository{}.write(base, portfolio_party);

    auto portfolio = ores::refdata::generators::generate_synthetic_portfolio(gctx);
    portfolio.party_id = portfolio_party.id;
    portfolio.change_reason_code = "system.test";
    ores::refdata::repository::portfolio_repository{}.write(base, portfolio);

    return {.currency = currency.iso_code,
            .party_id = party.id,
            .portfolio_id = portfolio.id,
            .ctx = base.with_party(base.tenant_id(), party.id, {party.id}, maker.username)};
}

/**
 * @brief A line that creates a valid book.
 */
book_change new_book_line(ores::testing::scoped_database_helper& h, const world& w) {
    static boost::uuids::random_generator gen;
    auto gctx = ores::testing::make_generation_context(h);
    auto line = ores::refdata::generators::generate_synthetic_book_change(gctx);
    line.entity_id = gen();
    line.party_id = w.party_id;
    line.parent_portfolio_id = w.portfolio_id;
    line.functional_currency = w.currency;
    line.owner_unit_id = std::nullopt;
    line.sandbox_id = std::nullopt;
    line.operation = "put";
    line.base_version = 0;
    line.change_reason_code = "system.test";
    return line;
}

/**
 * @brief The line that rewrites a live book as it stands, for the maker to
 * edit one column of.
 */
book_change edit_line(const ores::refdata::domain::book& live, book_change like) {
    like.entity_id = live.id;
    like.party_id = live.party_id;
    like.name = live.name;
    like.description = live.description;
    like.parent_portfolio_id = live.parent_portfolio_id;
    like.owner_unit_id = live.owner_unit_id;
    like.functional_currency = live.functional_currency;
    like.gl_account_ref = live.gl_account_ref;
    like.cost_center = live.cost_center;
    like.book_status = live.book_status;
    like.regulatory_book_type = live.regulatory_book_type;
    like.book_purpose_type = live.book_purpose_type;
    like.ledger_feed_type = live.ledger_feed_type;
    like.is_sweepable = live.is_sweepable;
    like.rates_centre_code = live.rates_centre_code;
    like.sandbox_id = live.sandbox_id;
    like.operation = "put";
    like.base_version = live.version;
    return like;
}

/**
 * @brief Writes the line's book to the live table, as an unapproved write would.
 */
ores::refdata::domain::book make_live(const world& w, const book_change& line) {
    ores::refdata::service::book_service svc(w.ctx);
    ores::refdata::domain::book b;
    b.id = line.entity_id;
    b.party_id = line.party_id;
    b.name = line.name;
    b.description = line.description;
    b.parent_portfolio_id = line.parent_portfolio_id;
    b.functional_currency = line.functional_currency;
    b.gl_account_ref = line.gl_account_ref;
    b.cost_center = line.cost_center;
    b.book_status = line.book_status;
    b.regulatory_book_type = line.regulatory_book_type;
    b.book_purpose_type = line.book_purpose_type;
    b.ledger_feed_type = line.ledger_feed_type;
    b.is_sweepable = line.is_sweepable;
    b.rates_centre_code = line.rates_centre_code;
    b.change_reason_code = "system.test";
    svc.save_book(b);
    return *svc.get_book(b.id);
}

std::size_t pending_count(const world& w) {
    return ores::refdata::repository::book_change_repository{}.read_latest(w.ctx).size();
}

}

TEST_CASE("a_preview_accepts_a_valid_new_book_and_names_every_part_that_decides_it", tags) {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);

    const auto preview = svc.preview({new_book_line(h, w)});

    REQUIRE(preview.lines.size() == 1);
    CHECK(preview.lines[0].refusal.empty());
    CHECK_FALSE(preview.refused());
    const std::vector<std::string> expected{"finance", "market_risk", "operations"};
    CHECK(preview.part_codes == expected);
}

TEST_CASE("a_preview_writes_nothing", tags) {
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

TEST_CASE("a_refused_line_does_not_stop_the_next_line_being_checked", tags) {
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

TEST_CASE("a_preview_refuses_a_line_that_read_a_version_that_is_no_longer_current", tags) {
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

TEST_CASE("a_change_to_one_column_names_only_the_part_of_that_column", tags) {
    ores::testing::scoped_database_helper h;
    const auto w = seed_world(h);
    book_proposal_service svc(w.ctx);
    const auto created = new_book_line(h, w);
    const auto live = make_live(w, created);

    auto edit = edit_line(live, created);
    edit.book_status = live.book_status == "Active" ? "Inactive" : "Active";

    const auto preview = svc.preview({edit});

    REQUIRE(preview.lines.size() == 1);
    CHECK(preview.lines[0].columns == std::vector<std::string>{"book_status"});
    CHECK(preview.part_codes == std::vector<std::string>{"operations"});
}

TEST_CASE("a_raise_writes_the_request_its_parts_and_its_lines_together", tags) {
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
    CHECK(parts.size() == 3);

    const auto rows = ores::refdata::repository::book_change_repository{}.read_latest(w.ctx);
    const auto mine = std::ranges::count_if(
        rows, [&](const auto& r) { return r.request_id == proposal.request->id; });
    CHECK(mine == 1);

    ores::refdata::service::book_service books(w.ctx);
    CHECK_FALSE(books.get_book(line.entity_id).has_value());
}

TEST_CASE("a_raise_with_a_refused_line_raises_nothing", tags) {
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

TEST_CASE("a_change_the_policy_does_not_gate_needs_no_request", tags) {
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

TEST_CASE("two_lines_of_one_request_cannot_share_a_line_number", tags) {
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

TEST_CASE("a_line_for_a_request_that_does_not_exist_is_refused", tags) {
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

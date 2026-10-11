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
#ifndef ORES_REFDATA_CORE_TESTS_BOOK_PROPOSAL_FIXTURES_HPP
#define ORES_REFDATA_CORE_TESTS_BOOK_PROPOSAL_FIXTURES_HPP

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

namespace ores::refdata::tests {

using ores::refdata::domain::book_change;

/**
 * @brief The rows a valid book points at, and the context that may write it.
 */
struct world {
    std::string currency;
    boost::uuids::uuid party_id;
    boost::uuids::uuid portfolio_id;
    ores::database::context ctx;
};

inline world seed_world(ores::testing::scoped_database_helper& h) {
    auto gctx = ores::testing::make_generation_context(h);
    auto& base = h.context();

    // The maker is an account of the tenant, and the context acts as it, so
    // the raise finds the account it raises for.
    auto maker = ores::iam::generators::generate_synthetic_account(gctx);
    // A person's account written into the shared test tenant would end its
    // bootstrap, and later tests that write as the unit test would be refused.
    maker.account_type = "service";
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
inline book_change new_book_line(ores::testing::scoped_database_helper& h, const world& w) {
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
inline book_change edit_line(const ores::refdata::domain::book& live, book_change like) {
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
inline ores::refdata::domain::book make_live(const world& w, const book_change& line) {
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

inline std::size_t pending_count(const world& w) {
    return ores::refdata::repository::book_change_repository{}.read_latest(w.ctx).size();
}

/**
 * @brief Approves every part of a request, one person to a part, as the
 * deciders would.
 */
inline void approve_every_part(const world& w,
                               ores::testing::scoped_database_helper& h,
                               const ores::inbox::domain::approval_request& raised) {
    ores::inbox::service::approval_lifecycle lifecycle(w.ctx);
    const auto id = boost::uuids::to_string(raised.id);
    auto gctx = ores::testing::make_generation_context(h);
    int version = raised.version;
    while (true) {
        const auto open = lifecycle.open_parts_of(id);
        if (open.empty())
            break;
        auto decider = ores::iam::generators::generate_synthetic_account(gctx);
        decider.account_type = "service";
        decider.change_reason_code = "system.test";
        ores::iam::repository::account_repository{}.write(h.context(), decider);
        const auto r = lifecycle.decide(id, version, "approve", decider.id, "", open.front().code);
        REQUIRE(r.outcome == "ok");
        version = r.version;
    }
    REQUIRE(lifecycle.request(id)->state_code == "approved");
}

}

#endif

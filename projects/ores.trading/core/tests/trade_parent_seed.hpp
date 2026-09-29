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
#ifndef ORES_TRADING_TESTS_TRADE_PARENT_SEED_HPP
#define ORES_TRADING_TESTS_TRADE_PARENT_SEED_HPP

#include "ores.dq.core/repository/fsm_state_repository.hpp"
#include "ores.refdata.api/generators/book_generator.hpp"
#include "ores.refdata.api/generators/currency_generator.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.api/generators/portfolio_generator.hpp"
#include "ores.refdata.core/repository/book_repository.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.refdata.core/repository/portfolio_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>

namespace ores::trading::tests {

/**
 * @brief Writes one trade, with the reference data its insert trigger reads.
 *
 * An instrument's key is the trade it belongs to, so a test that writes an
 * instrument row has to state a trade that exists. The trade's insert trigger
 * validates its book and derives the party and the portfolio from it, so the
 * party, a currency, the portfolio and the book are written first, in that
 * order.
 *
 * @return The id of the trade that was written.
 */
inline boost::uuids::uuid write_parent_trade(ores::testing::database_helper& h) {
    namespace refdata = ores::refdata;
    auto gen = ores::testing::make_generation_context(h);

    auto party = refdata::generators::generate_synthetic_party(gen);
    party.change_reason_code = "system.test";
    refdata::repository::party_repository party_repo;
    for (const auto& existing : party_repo.read_latest(h.context())) {
        if (existing.tenant_id == party.tenant_id) {
            party.parent_party_id = existing.id;
            break;
        }
    }
    party_repo.write(h.context(), party);
    auto ctx = h.context().with_party(h.tenant_id(), party.id, {party.id}, h.db_user());

    auto currency = refdata::generators::generate_synthetic_currency(gen);
    currency.change_reason_code = "system.test";
    refdata::repository::currency_repository currency_repo;
    currency_repo.write(ctx, currency);

    auto portfolio = refdata::generators::generate_synthetic_portfolio(gen);
    portfolio.change_reason_code = "system.test";
    portfolio.party_id = party.id;
    // The generator emits the X-0 sentinel, which the insert trigger refuses
    // unless the sentinel is seeded; the currency above serves instead.
    portfolio.aggregation_ccy = currency.iso_code;
    refdata::repository::portfolio_repository portfolio_repo;
    portfolio_repo.write(ctx, portfolio);

    auto book = refdata::generators::generate_synthetic_book(gen);
    book.change_reason_code = "system.test";
    book.party_id = party.id;
    book.functional_currency = currency.iso_code;
    book.parent_portfolio_id = portfolio.id;
    refdata::repository::book_repository book_repo;
    book_repo.write(ctx, book);

    auto tr = generators::generate_synthetic_trade(gen);
    tr.audit.change_reason_code = "system.test";
    tr.identity.party_id = party.id;
    tr.parties.book_id = book.id;
    // The status is system-tenant reference data, so the row references the
    // seeded catalogue rather than adding a state of its own.
    ores::dq::repository::fsm_state_repository status_repo;
    const auto statuses = status_repo.read_latest(
        ctx.with_tenant(ores::utility::uuid::tenant_id::system(), h.db_user()));
    if (!statuses.empty())
        tr.classification.status_id = statuses.front().id;

    repository::trade_repository trade_repo;
    trade_repo.write(ctx, tr);
    return tr.identity.id;
}

}

#endif

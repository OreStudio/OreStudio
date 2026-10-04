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

#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.trading.api/generators/trade_anchor_generator.hpp"
#include "ores.trading.core/repository/trade_anchor_repository.hpp"
#include <boost/uuid/uuid.hpp>

namespace ores::trading::tests {

/**
 * @brief Writes one trade anchor, with the party it belongs to.
 *
 * An instrument's key is the trade it belongs to, so a test that writes an
 * instrument row has to state a trade whose anchor exists. The anchor needs
 * only its party, which is written first.
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

    auto anchor = generators::generate_synthetic_trade_anchor(gen);
    anchor.party_id = party.id;
    repository::trade_anchor_repository().write(ctx, anchor);
    return anchor.id;
}

}

#endif

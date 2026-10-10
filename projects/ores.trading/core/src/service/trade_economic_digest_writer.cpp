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
#include "ores.trading.core/repository/trade_write_observation.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/economic_digest.hpp"
#include "ores.trading.api/domain/trade_economic_digest.hpp"
#include "ores.trading.core/repository/trade_component_queries.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <string>
#include <vector>

namespace ores::trading::service {

using namespace ores::logging;

namespace {

/*
 * A system reason names a write the customer did not make: an import, a
 * migration, a seed. A user reason names an amendment. That is a question a
 * reason can answer honestly, and a different one from "did the value
 * change", which only the digest answers.
 */
bool is_system_reason(const std::string& reason) {
    return reason.rfind("system.", 0) == 0;
}

}

void refresh_trade_economic_digest(context ctx,
                                   const std::string& trade_id,
                                   const std::string& change_reason_code,
                                   logger_t& log) {
    namespace repository = ores::trading::repository;

    const auto found = repository::trade_repository{}.read_latest(ctx, trade_id);
    if (found.empty()) {
        BOOST_LOG_SEV(log, debug) << "No trade " << trade_id << " to digest.";
        return;
    }
    auto trade = found.front();

    /*
     * The economics: the instrument the trade's type routes to and everything
     * beneath it, plus the identifiers and the party roles, which state terms
     * a confirmation carries. A comment, a lifecycle state, the book or
     * portfolio a trade sits in and its structure membership are not terms
     * the customer agreed to, so none of them is folded.
     */
    std::vector<std::string> component_digests;
    const auto append = [&component_digests](const auto& rows) {
        for (const auto& row : rows)
            component_digests.push_back(domain::economic_digest(row));
    };
    append(repository::read_instrument_digests(ctx, trade_id, trade.trade_type));
    append(repository::read_identifiers_by_trade_ids(ctx, {trade_id}));
    append(repository::read_party_roles_by_trade_ids(ctx, {trade_id}));

    const auto digest = domain::trade_economic_digest(trade, component_digests);

    if (!trade.economic_digest.empty() && trade.economic_digest == digest) {
        BOOST_LOG_SEV(log, debug) << "Trade " << trade_id << " digest is unchanged.";
        return;
    }

    /*
     * The version moves only for an amendment. A system reason is an import or
     * a migration, which is not an agreement, so it states the digest and
     * leaves the version where it was.
     */
    trade.economic_digest = digest;
    if (!is_system_reason(change_reason_code))
        ++trade.external_version;
    repository::trade_repository{}.write(ctx, trade);
    BOOST_LOG_SEV(log, debug) << "Trade " << trade_id << " digest refreshed.";
}

}

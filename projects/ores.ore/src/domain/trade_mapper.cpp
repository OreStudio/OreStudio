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
#include "ores.ore/domain/trade_mapper.hpp"

#include <boost/uuid/nil_generator.hpp>
#include "ores.trading/domain/trade_json_io.hpp" // IWYU pragma: keep.

namespace ores::ore::domain {

using namespace ores::logging;

trading::domain::trade trade_mapper::map(const trade& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping ORE XML trade: " << std::string(v.id);

    const auto nil = boost::uuids::nil_uuid();
    trading::domain::trade r;
    r.id = nil;
    r.party_id = nil;
    r.external_id = std::string(v.id);
    r.trade_type = to_string(v.TradeType);

    // Book, portfolio and counterparty require external mapping context.
    r.book_id = nil;
    r.portfolio_id = nil;

    // Extract envelope fields where available.
    if (v.Envelope) {
        if (v.Envelope->CounterParty) {
            // Store the counterparty name as the netting set ID fallback
            // context. The actual counterparty_id UUID must be resolved by the
            // calling code via a mapping dialog or lookup.
        }
        if (v.Envelope->nettingSetGroup) {
            if (v.Envelope->nettingSetGroup->NettingSetId) {
                r.netting_set_id =
                    std::string(*v.Envelope->nettingSetGroup->NettingSetId);
            }
        }
    }

    r.lifecycle_event = "New";
    r.modified_by = "ores";
    r.change_reason_code = "system.external_data_import";
    r.change_commentary = "Imported from ORE XML portfolio";

    BOOST_LOG_SEV(lg(), trace) << "Mapped trade. Result: " << r;
    return r;
}

std::vector<trading::domain::trade> trade_mapper::map(const portfolio& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping ORE XML portfolio. Total trades: "
                               << v.Trade.size();

    std::vector<trading::domain::trade> r;
    r.reserve(v.Trade.size());
    std::ranges::transform(v.Trade, std::back_inserter(r),
        [](const auto& ve) { return map(ve); });

    BOOST_LOG_SEV(lg(), trace) << "Mapped portfolio trades.";
    return r;
}

}

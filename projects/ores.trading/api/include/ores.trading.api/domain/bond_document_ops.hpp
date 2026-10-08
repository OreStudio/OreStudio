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
#ifndef ORES_TRADING_API_DOMAIN_BOND_DOCUMENT_OPS_HPP
#define ORES_TRADING_API_DOMAIN_BOND_DOCUMENT_OPS_HPP

#include "ores.trading.api/domain/bond_instrument_data.hpp"
#include "ores.trading.api/domain/bond_leg_data.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include <boost/uuid/uuid.hpp>

namespace ores::trading::domain {

/**
 * @brief True when a bond leg carries nothing, so a writer can skip it.
 *
 * The schema requires the leg's type and its payer, so a leg a writer emits
 * empty is not a document the reader accepts. The guard belongs beside the
 * members, in one place rather than at each writer.
 */
[[nodiscard]] inline bool is_empty(const bond_leg_data& leg) {
    return !leg.payer && !leg.leg_type && !leg.currency && !leg.payment_convention &&
           !leg.payment_lag && !leg.payment_calendar && !leg.day_counter &&
           !leg.last_period_day_counter && !leg.notional_payment_lag &&
           !leg.strict_notional_dates && !leg.rate && !leg.indexings_from_asset_leg &&
           !leg.settlement && leg.schedule.rules.empty() && leg.schedule.dates.empty() &&
           leg.amortizations.empty() && leg.notionals.empty() && leg.payment_dates.empty() &&
           leg.payment_schedule.rules.empty() && leg.payment_schedule.dates.empty();
}

/**
 * @brief Stamps the trade id and the activity on a bond document and its rows.
 *
 * The bond carrier holds the instrument header and the product's fact rows,
 * and every one of them names the trade and the activity it was written
 * beside, so one call reaches all of them.
 */
inline void stamp_ids(bond_instrument_data& data,
                      boost::uuids::uuid trade_id,
                      boost::uuids::uuid activity_id = {}) {
    stamp_ids(data.instrument, trade_id, activity_id);
    if (data.option) {
        data.option->trade_id = trade_id;
        data.option->trade_activity_id = activity_id;
    }
    if (data.trs) {
        data.trs->trade_id = trade_id;
        data.trs->trade_activity_id = activity_id;
    }
    if (data.repo) {
        data.repo->trade_id = trade_id;
        data.repo->trade_activity_id = activity_id;
    }
    if (data.future) {
        data.future->trade_id = trade_id;
        data.future->trade_activity_id = activity_id;
    }
    if (data.ascot_row) {
        data.ascot_row->trade_id = trade_id;
        data.ascot_row->trade_activity_id = activity_id;
    }
}

}

#endif

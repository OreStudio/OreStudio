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
#include "ores.trading.core/presentation/equity_digital_option_instrument_history_field_mapper.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value>
render_equity_digital_option_instrument_fields(const domain::equity_digital_option_instrument& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back(
        {.name = "Instrument ID", .value = boost::uuids::to_string(v.identity.instrument_id)});
    fields.push_back({.name = "Trade Type Code", .value = v.identity.trade_type_code});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.identity.party_id)});
    fields.push_back({.name = "Trade ID",
                      .value = v.identity.trade_id ? boost::uuids::to_string(*v.identity.trade_id) :
                                                     std::string{}});
    fields.push_back({.name = "Underlying Name", .value = v.underlying_name});
    fields.push_back({.name = "Currency", .value = v.currency});
    fields.push_back({.name = "Notional", .value = std::to_string(v.notional)});
    fields.push_back({.name = "Option Type", .value = v.option_type});
    fields.push_back(
        {.name = "Strike", .value = v.strike ? std::to_string(*v.strike) : std::string{}});
    fields.push_back({.name = "Barrier Level",
                      .value = v.barrier_level ? std::to_string(*v.barrier_level) : std::string{}});
    fields.push_back({.name = "Barrier Type", .value = v.barrier_type});
    fields.push_back({.name = "Expiry Date", .value = v.expiry_date});
    fields.push_back({.name = "Long Short", .value = v.long_short});
    fields.push_back({.name = "Payout Amount",
                      .value = v.payout_amount ? std::to_string(*v.payout_amount) : std::string{}});
    fields.push_back({.name = "Description", .value = v.description});
    fields.push_back({.name = "Modified By", .value = v.audit.modified_by});
    fields.push_back({.name = "Performed By", .value = v.audit.performed_by});
    fields.push_back({.name = "Change Reason Code", .value = v.audit.change_reason_code});
    fields.push_back({.name = "Change Commentary", .value = v.audit.change_commentary});
    fields.push_back(
        {.name = "Recorded At",
         .value = ores::platform::time::datetime::to_iso8601_utc(v.audit.recorded_at)});

    return fields;
}

}

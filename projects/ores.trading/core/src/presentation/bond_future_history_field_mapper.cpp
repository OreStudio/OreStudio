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
#include "ores.trading.core/presentation/bond_future_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value>
render_bond_future_fields(const domain::bond_future& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "Instrument ID", .value = boost::uuids::to_string(v.instrument_id)});
    fields.push_back({.name = "Contract Name", .value = v.contract_name});
    fields.push_back({.name = "Contract Notional", .value = std::to_string(v.contract_notional)});
    fields.push_back({.name = "Long Short", .value = v.long_short});
    fields.push_back({.name = "Currency", .value = v.currency});
    fields.push_back({.name = "Contract Month", .value = v.contract_month});
    fields.push_back({.name = "Deliverable Grade", .value = v.deliverable_grade});
    fields.push_back({.name = "Fair Price", .value = std::to_string(v.fair_price)});
    fields.push_back({.name = "Settlement", .value = v.settlement});
    fields.push_back({.name = "Settlement Dirty", .value = v.settlement_dirty ? "true" : "false"});
    fields.push_back({.name = "Root Date", .value = v.root_date});
    fields.push_back({.name = "Expiry Basis", .value = v.expiry_basis});
    fields.push_back({.name = "Settlement Basis", .value = v.settlement_basis});
    fields.push_back({.name = "Expiry Lag", .value = std::to_string(v.expiry_lag)});
    fields.push_back({.name = "Settlement Lag", .value = std::to_string(v.settlement_lag)});
    fields.push_back({.name = "Last Trading Date", .value = v.last_trading_date});
    fields.push_back({.name = "Last Delivery Date", .value = v.last_delivery_date});
    using ores::history::domain::provenance_fields;
    fields.push_back({.name = provenance_fields::modified_by, .value = v.modified_by});
    fields.push_back({.name = provenance_fields::performed_by, .value = v.performed_by});
    fields.push_back(
        {.name = provenance_fields::change_reason_code, .value = v.change_reason_code});
    fields.push_back({.name = provenance_fields::change_commentary, .value = v.change_commentary});
    fields.push_back({.name = provenance_fields::recorded_at,
                      .value = ores::platform::time::datetime::to_iso8601_utc(v.recorded_at)});

    return fields;
}

}

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
#include "ores.trading.core/presentation/fx_forward_instrument_history_field_mapper.hpp"
#include "ores.history.api/domain/provenance_fields.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::presentation {

std::vector<ores::diff::domain::field_value>
render_fx_forward_instrument_fields(const domain::fx_forward_instrument& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back(
        {.name = "Instrument ID", .value = boost::uuids::to_string(v.identity.instrument_id)});
    fields.push_back({.name = "Trade Type Code", .value = v.identity.trade_type_code});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.identity.party_id)});
    fields.push_back({.name = "Trade ID",
                      .value = v.identity.trade_id ? boost::uuids::to_string(*v.identity.trade_id) :
                                                     std::string{}});
    fields.push_back({.name = "Bought Currency", .value = v.bought_currency});
    fields.push_back({.name = "Bought Amount", .value = std::to_string(v.bought_amount)});
    fields.push_back({.name = "Sold Currency", .value = v.sold_currency});
    fields.push_back({.name = "Sold Amount", .value = std::to_string(v.sold_amount)});
    fields.push_back({.name = "Value Date", .value = v.value_date});
    fields.push_back({.name = "Settlement", .value = v.settlement});
    fields.push_back({.name = "Description", .value = v.description});
    using ores::history::domain::provenance_fields;
    fields.push_back({.name = provenance_fields::modified_by, .value = v.audit.modified_by});
    fields.push_back({.name = provenance_fields::performed_by, .value = v.audit.performed_by});
    fields.push_back(
        {.name = provenance_fields::change_reason_code, .value = v.audit.change_reason_code});
    fields.push_back(
        {.name = provenance_fields::change_commentary, .value = v.audit.change_commentary});
    fields.push_back(
        {.name = provenance_fields::recorded_at,
         .value = ores::platform::time::datetime::to_iso8601_utc(v.audit.recorded_at)});

    return fields;
}

}

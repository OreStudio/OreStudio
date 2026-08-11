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
#include "ores.dq.core/presentation/synthetic_fx_spot_config_history_field_mapper.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::dq::presentation {

std::vector<ores::diff::domain::field_value>
render_synthetic_fx_spot_config_fields(const domain::synthetic_fx_spot_config& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Name", .value = v.name});
    fields.push_back({.name = "Description", .value = v.description.value_or(std::string{})});
    fields.push_back({.name = "Enabled", .value = v.enabled ? "true" : "false"});
    fields.push_back({.name = "Auto Start", .value = v.auto_start ? "true" : "false"});
    fields.push_back({.name = "Base Currency Code", .value = v.base_currency_code});
    fields.push_back({.name = "Quote Currency Code", .value = v.quote_currency_code});
    fields.push_back({.name = "Gmm Initial Price", .value = std::to_string(v.gmm_initial_price)});
    fields.push_back({.name = "Ticks Per Hour", .value = std::to_string(v.ticks_per_hour)});
    fields.push_back({.name = "Process Type", .value = v.process_type});
    fields.push_back({.name = "Price Source", .value = v.price_source});
    fields.push_back({.name = "Vintage Source", .value = v.vintage_source.value_or(std::string{})});
    fields.push_back({.name = "Vintage Date", .value = v.vintage_date.value_or(std::string{})});
    fields.push_back({.name = "Modified By", .value = v.modified_by});
    fields.push_back({.name = "Performed By", .value = v.performed_by});
    fields.push_back({.name = "Change Reason Code", .value = v.change_reason_code});
    fields.push_back({.name = "Change Commentary", .value = v.change_commentary});
    fields.push_back({.name = "Recorded At",
                      .value = ores::platform::time::datetime::to_iso8601_utc(v.recorded_at)});

    return fields;
}

}

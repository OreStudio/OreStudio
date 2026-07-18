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
#include "ores.synthetic.core/presentation/ir_curve_generation_config_history_field_mapper.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::synthetic::presentation {

std::vector<ores::diff::domain::field_value>
render_ir_curve_generation_config_fields(const domain::ir_curve_generation_config& v) {
    using ores::diff::domain::field_value;
    std::vector<field_value> fields;

    fields.push_back({.name = "ID", .value = boost::uuids::to_string(v.id)});
    fields.push_back({.name = "Party ID", .value = boost::uuids::to_string(v.party_id)});
    fields.push_back({.name = "Config ID", .value = boost::uuids::to_string(v.config_id)});
    fields.push_back({.name = "Currency Code", .value = v.currency_code});
    fields.push_back({.name = "Index Name", .value = v.index_name});
    fields.push_back({.name = "Process Type", .value = v.process_type});
    fields.push_back({.name = "Kappa", .value = std::to_string(v.kappa)});
    fields.push_back({.name = "Theta", .value = std::to_string(v.theta)});
    fields.push_back({.name = "Sigma", .value = std::to_string(v.sigma)});
    fields.push_back({.name = "Initial Rate", .value = std::to_string(v.initial_rate)});
    fields.push_back({.name = "Ticks Per Hour", .value = std::to_string(v.ticks_per_hour)});
    fields.push_back({.name = "Enabled", .value = v.enabled ? "true" : "false"});
    fields.push_back(
        {.name = "Fixed Leg Payment Frequency Code", .value = v.fixed_leg_payment_frequency_code});
    fields.push_back(
        {.name = "Folder ID",
         .value = v.folder_id ? boost::uuids::to_string(*v.folder_id) : std::string{}});
    fields.push_back({.name = "Modified By", .value = v.modified_by});
    fields.push_back({.name = "Performed By", .value = v.performed_by});
    fields.push_back({.name = "Change Reason Code", .value = v.change_reason_code});
    fields.push_back({.name = "Change Commentary", .value = v.change_commentary});
    fields.push_back({.name = "Recorded At",
                      .value = ores::platform::time::datetime::to_iso8601_utc(v.recorded_at)});

    return fields;
}

}

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
#include "ores.http.core/routes/ore_value_set_tables.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.reporting.api/messaging/ore_parameter_setting_value_set_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_value_set_catalogue_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_value_set_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_value_set_value_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <string>
#include <vector>

namespace ores::http::routes {

namespace {

namespace forwarding = ores::http::forwarding;
namespace reporting_messaging = ores::reporting::messaging;
namespace json = ores::utility::codec::json;

using ores::http::domain::http_request;
using ores::nats::service::nats_client;

}

value_set_tables read_value_set_tables(const http_request& req, nats_client& session) {
    value_set_tables tables;
    for (const auto& set : forwarding::forward_all(
             req,
             session,
             reporting_messaging::list_ore_parameter_value_sets_request{},
             &reporting_messaging::list_ore_parameter_value_sets_response::sets))
        tables.sets.push_back(
            {set.code, set.source_kind, set.catalogue_code, set.minimum, set.maximum, set.pattern});
    for (const auto& value : forwarding::forward_all(
             req,
             session,
             reporting_messaging::list_ore_parameter_value_set_values_request{},
             &reporting_messaging::list_ore_parameter_value_set_values_response::values))
        tables.values.push_back({value.value_set_code,
                                 value.value,
                                 value.label,
                                 value.display_order,
                                 value.canonical_value});
    for (const auto& catalogue : forwarding::forward_all(
             req,
             session,
             reporting_messaging::list_ore_parameter_value_set_catalogues_request{},
             &reporting_messaging::list_ore_parameter_value_set_catalogues_response::catalogues))
        tables.catalogues.push_back({catalogue.code, catalogue.relation});
    for (const auto& link : forwarding::forward_all(
             req,
             session,
             reporting_messaging::list_ore_parameter_setting_value_sets_request{},
             &reporting_messaging::list_ore_parameter_setting_value_sets_response::
                 setting_value_sets))
        tables.links.push_back({link.scope, link.name, link.value_set_code});
    return tables;
}

std::vector<std::string> value_fields_members(const value_fields& fields) {
    std::vector<std::string> choices;
    choices.reserve(fields.choices.size());
    for (const auto& choice : fields.choices)
        choices.push_back("{\"value\":" + json::write(choice.value) +
                          ",\"label\":" + json::write(choice.label) + "}");
    const auto rendered = ores::http::net::record_routes::array_of(choices);
    return {"\"valueSet\":" + json::write(fields.value_set),
            "\"sourceKind\":" + json::write(fields.source_kind),
            "\"catalogue\":" + json::write(fields.catalogue),
            "\"choices\":" + rendered,
            "\"minimum\":" + json::write(fields.minimum),
            "\"maximum\":" + json::write(fields.maximum),
            "\"pattern\":" + json::write(fields.pattern)};
}

}

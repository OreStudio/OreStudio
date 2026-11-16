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
#include "ores.http/routes/reporting/ore_value_set_rows.hpp"
#include "ores.analytics.api/messaging/sensitivity_template_protocol.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_protocol.hpp"
#include <string>
#include <vector>

namespace ores::http::routes::reporting::value_set_rows {

namespace {

namespace forwarding = ores::http::forwarding;
namespace refdata_messaging = ores::refdata::messaging;
namespace analytics_messaging = ores::analytics::messaging;

using ores::http::domain::http_request;
using ores::http::routes::value_choice;
using ores::http::routes::value_set_error;
using ores::nats::service::nats_client;

}

std::vector<value_choice>
catalogue_choices_of(const http_request& req, nats_client& session, std::string_view relation) {
    if (relation == "ores_refdata_tenor_catalogue_v") {
        std::vector<value_choice> choices;
        for (const auto& tenor :
             forwarding::forward_all(req,
                                     session,
                                     refdata_messaging::list_tenors_request{},
                                     &refdata_messaging::list_tenors_response::tenors))
            choices.push_back({tenor.code, tenor.display_name});
        return choices;
    }
    if (relation == "ores_refdata_currency_catalogue_v") {
        std::vector<value_choice> choices;
        for (const auto& currency :
             forwarding::forward_all(req,
                                     session,
                                     refdata_messaging::list_currencies_request{},
                                     &refdata_messaging::list_currencies_response::currencies))
            choices.push_back({currency.iso_code, currency.name});
        return choices;
    }
    if (relation == "ores_analytics_sensitivity_template_catalogue_v") {
        std::vector<value_choice> choices;
        for (const auto& template_row : forwarding::forward_all(
                 req,
                 session,
                 analytics_messaging::list_sensitivity_templates_request{},
                 &analytics_messaging::list_sensitivity_templates_response::templates))
            choices.push_back({template_row.code, template_row.code});
        return choices;
    }
    throw value_set_error("The value set tables name the catalogue relation \"" +
                          std::string(relation) + "\", which this build does not read.");
}

}

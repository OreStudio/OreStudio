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
#include "ores.http/routes/refdata/refdata_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http/routes/refdata/business_centre_routes.hpp"
#include "ores.http/routes/refdata/business_centre_web_routes.hpp"
#include "ores.http/routes/refdata/business_unit_routes.hpp"
#include "ores.http/routes/refdata/business_unit_type_routes.hpp"
#include "ores.http/routes/refdata/business_unit_type_web_routes.hpp"
#include "ores.http/routes/refdata/business_unit_web_routes.hpp"
#include "ores.http/routes/refdata/calendar_event_routes.hpp"
#include "ores.http/routes/refdata/calendar_event_web_routes.hpp"
#include "ores.http/routes/refdata/calendar_exception_routes.hpp"
#include "ores.http/routes/refdata/calendar_exception_web_routes.hpp"
#include "ores.http/routes/refdata/calendar_routes.hpp"
#include "ores.http/routes/refdata/calendar_rule_routes.hpp"
#include "ores.http/routes/refdata/calendar_rule_web_routes.hpp"
#include "ores.http/routes/refdata/calendar_web_routes.hpp"
#include "ores.http/routes/refdata/classification_routes.hpp"
#include "ores.http/routes/refdata/counterparty_routes.hpp"
#include "ores.http/routes/refdata/counterparty_web_routes.hpp"
#include "ores.http/routes/refdata/country_routes.hpp"
#include "ores.http/routes/refdata/country_web_routes.hpp"
#include "ores.http/routes/refdata/currency_calendar_routes.hpp"
#include "ores.http/routes/refdata/currency_calendar_web_routes.hpp"
#include "ores.http/routes/refdata/currency_country_routes.hpp"
#include "ores.http/routes/refdata/currency_country_web_routes.hpp"
#include "ores.http/routes/refdata/currency_currency_group_routes.hpp"
#include "ores.http/routes/refdata/currency_currency_group_web_routes.hpp"
#include "ores.http/routes/refdata/currency_group_routes.hpp"
#include "ores.http/routes/refdata/currency_group_web_routes.hpp"
#include "ores.http/routes/refdata/currency_pair_convention_calendar_routes.hpp"
#include "ores.http/routes/refdata/currency_pair_convention_calendar_web_routes.hpp"
#include "ores.http/routes/refdata/currency_pair_convention_routes.hpp"
#include "ores.http/routes/refdata/currency_pair_convention_web_routes.hpp"
#include "ores.http/routes/refdata/currency_pair_routes.hpp"
#include "ores.http/routes/refdata/currency_pair_web_routes.hpp"
#include "ores.http/routes/refdata/currency_routes.hpp"
#include "ores.http/routes/refdata/currency_web_routes.hpp"
#include "ores.http/routes/refdata/instrument_code_web_routes.hpp"
#include "ores.http/routes/refdata/ir_curve_bootstrap_config_routes.hpp"
#include "ores.http/routes/refdata/ir_curve_bootstrap_config_web_routes.hpp"
#include "ores.http/routes/refdata/ir_curve_bootstrap_pillar_routes.hpp"
#include "ores.http/routes/refdata/ir_curve_bootstrap_pillar_web_routes.hpp"
#include "ores.http/routes/refdata/party_contact_information_routes.hpp"
#include "ores.http/routes/refdata/party_contact_information_web_routes.hpp"
#include "ores.http/routes/refdata/party_counterparty_routes.hpp"
#include "ores.http/routes/refdata/party_counterparty_web_routes.hpp"
#include "ores.http/routes/refdata/party_country_routes.hpp"
#include "ores.http/routes/refdata/party_country_web_routes.hpp"
#include "ores.http/routes/refdata/party_currency_routes.hpp"
#include "ores.http/routes/refdata/party_currency_web_routes.hpp"
#include "ores.http/routes/refdata/party_id_scheme_routes.hpp"
#include "ores.http/routes/refdata/party_id_scheme_web_routes.hpp"
#include "ores.http/routes/refdata/party_identifier_routes.hpp"
#include "ores.http/routes/refdata/party_identifier_web_routes.hpp"
#include "ores.http/routes/refdata/party_page_routes.hpp"
#include "ores.http/routes/refdata/party_routes.hpp"
#include "ores.http/routes/refdata/party_web_routes.hpp"
#include "ores.http/routes/refdata/payment_frequency_web_routes.hpp"
#include "ores.http/routes/refdata/pick_list_routes.hpp"
#include "ores.http/routes/refdata/tenor_web_routes.hpp"
#include <string>
#include <string_view>

namespace ores::http::routes::refdata {

namespace {

/// The catalogue of the resources the screens name, built from each unit's own entry.
std::string resource_catalogue_body(const ores::http::domain::http_request& req) {
    const std::string entries[] = {
        business_centre_web_routes::catalogue_entry(req),
        business_unit_web_routes::catalogue_entry(req),
        business_unit_type_web_routes::catalogue_entry(req),
        calendar_web_routes::catalogue_entry(req),
        calendar_event_web_routes::catalogue_entry(req),
        calendar_exception_web_routes::catalogue_entry(req),
        calendar_rule_web_routes::catalogue_entry(req),
        counterparty_web_routes::catalogue_entry(req),
        country_web_routes::catalogue_entry(req),
        currency_web_routes::catalogue_entry(req),
        currency_calendar_web_routes::catalogue_entry(req),
        currency_country_web_routes::catalogue_entry(req),
        currency_currency_group_web_routes::catalogue_entry(req),
        currency_group_web_routes::catalogue_entry(req),
        currency_pair_web_routes::catalogue_entry(req),
        currency_pair_convention_web_routes::catalogue_entry(req),
        currency_pair_convention_calendar_web_routes::catalogue_entry(req),
        ir_curve_bootstrap_config_web_routes::catalogue_entry(req),
        ir_curve_bootstrap_pillar_web_routes::catalogue_entry(req),
        party_web_routes::catalogue_entry(req),
        party_contact_information_web_routes::catalogue_entry(req),
        party_counterparty_web_routes::catalogue_entry(req),
        party_country_web_routes::catalogue_entry(req),
        party_currency_web_routes::catalogue_entry(req),
        party_id_scheme_web_routes::catalogue_entry(req),
        party_identifier_web_routes::catalogue_entry(req),
    };
    std::string out = "{\"resources\":[";
    for (std::size_t i = 0; i < std::size(entries); ++i) {
        if (i != 0)
            out += ",";
        out += entries[i];
    }
    return out + "]}";
}

}

void refdata_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    ores::nats::service::nats_client& session) {

    business_centre_routes::register_routes(router, registry, session);
    business_unit_routes::register_routes(router, registry, session);
    business_unit_type_routes::register_routes(router, registry, session);
    calendar_routes::register_routes(router, registry, session);
    calendar_event_routes::register_routes(router, registry, session);
    calendar_exception_routes::register_routes(router, registry, session);
    calendar_rule_routes::register_routes(router, registry, session);
    counterparty_routes::register_routes(router, registry, session);
    country_routes::register_routes(router, registry, session);
    currency_routes::register_routes(router, registry, session);
    currency_calendar_routes::register_routes(router, registry, session);
    currency_country_routes::register_routes(router, registry, session);
    currency_currency_group_routes::register_routes(router, registry, session);
    currency_group_routes::register_routes(router, registry, session);
    currency_pair_routes::register_routes(router, registry, session);
    currency_pair_convention_routes::register_routes(router, registry, session);
    currency_pair_convention_calendar_routes::register_routes(router, registry, session);
    ir_curve_bootstrap_config_routes::register_routes(router, registry, session);
    ir_curve_bootstrap_pillar_routes::register_routes(router, registry, session);
    party_routes::register_routes(router, registry, session);
    party_contact_information_routes::register_routes(router, registry, session);
    party_counterparty_routes::register_routes(router, registry, session);
    party_country_routes::register_routes(router, registry, session);
    party_currency_routes::register_routes(router, registry, session);
    party_id_scheme_routes::register_routes(router, registry, session);
    party_identifier_routes::register_routes(router, registry, session);

    // The record routes the screens name each resource by.
    business_centre_web_routes::register_routes(router, registry, session);
    business_unit_web_routes::register_routes(router, registry, session);
    business_unit_type_web_routes::register_routes(router, registry, session);
    calendar_web_routes::register_routes(router, registry, session);
    calendar_event_web_routes::register_routes(router, registry, session);
    calendar_exception_web_routes::register_routes(router, registry, session);
    calendar_rule_web_routes::register_routes(router, registry, session);
    counterparty_web_routes::register_routes(router, registry, session);
    country_web_routes::register_routes(router, registry, session);
    currency_web_routes::register_routes(router, registry, session);
    currency_calendar_web_routes::register_routes(router, registry, session);
    currency_country_web_routes::register_routes(router, registry, session);
    currency_currency_group_web_routes::register_routes(router, registry, session);
    currency_group_web_routes::register_routes(router, registry, session);
    currency_pair_web_routes::register_routes(router, registry, session);
    currency_pair_convention_web_routes::register_routes(router, registry, session);
    currency_pair_convention_calendar_web_routes::register_routes(router, registry, session);
    ir_curve_bootstrap_config_web_routes::register_routes(router, registry, session);
    ir_curve_bootstrap_pillar_web_routes::register_routes(router, registry, session);
    party_web_routes::register_routes(router, registry, session);
    party_contact_information_web_routes::register_routes(router, registry, session);
    party_counterparty_web_routes::register_routes(router, registry, session);
    party_country_web_routes::register_routes(router, registry, session);
    party_currency_web_routes::register_routes(router, registry, session);
    party_id_scheme_web_routes::register_routes(router, registry, session);
    party_identifier_web_routes::register_routes(router, registry, session);

    // The classification screen's code lists, its catalogue and the label catalogue.
    classification_routes::register_routes(router, registry, session);

    // The option lists the reference data forms offer, one read per form.
    pick_list_routes::register_routes(router, registry, session);
    party_page_routes::register_routes(router, registry, session);

    // The reference data lookups the synthetic screens read, under /api/synthetic.
    tenor_web_routes::register_routes(router, registry, session);
    instrument_code_web_routes::register_routes(router, registry, session);
    payment_frequency_web_routes::register_routes(router, registry, session);

    // The screens read which resources they may name, and what each states,
    // from one catalogue rather than from a list of their own.
    auto catalogue =
        router->get("/api/refdata")
            .summary("List the reference data resources a screen may name")
            .description("Each resource's key, key fields, search, sortable "
                         "fields, events and permissions.")
            .tags({"refdata"})
            .auth_required()
            .handler([](const ores::http::domain::http_request& req)
                         -> boost::asio::awaitable<ores::http::domain::http_response> {
                co_return ores::http::domain::http_response::json(resource_catalogue_body(req));
            });
    const auto catalogue_built = catalogue.build();
    router->add_route(catalogue_built);
    registry->register_route(catalogue_built);
}

}

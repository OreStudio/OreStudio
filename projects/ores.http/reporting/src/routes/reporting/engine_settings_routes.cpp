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
#include "ores.http/routes/reporting/engine_settings_routes.hpp"
#include "ores.analytics.api/messaging/sensitivity_template_protocol.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/domain/http_status.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.core/routes/ore_value_sets.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/reporting/engine_settings_rows.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_setting_value_set_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_value_set_catalogue_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_value_set_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_value_set_value_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <map>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::reporting {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace reporting_messaging = ores::reporting::messaging;
namespace refdata_messaging = ores::refdata::messaging;
namespace analytics_messaging = ores::analytics::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::routes::value_choice;
using ores::http::routes::value_fields;
using ores::http::routes::value_set_error;
using ores::http::routes::value_set_reader;
using ores::http::routes::value_set_tables;
using ores::nats::service::nats_client;

/// The schema and the scope whose parameters the documentation page states.
constexpr std::string_view ore_schema_code = "ore.xsd";
constexpr std::string_view setup_scope_code = "Setup";

/// The four value set tables as the rows the reader takes.
value_set_tables value_set_tables_of(const http_request& req, nats_client& session) {
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

/**
 * @brief The choices of one catalogue relation, read from the collection that
 * holds it.
 *
 * A relation names a view in another component, so the gateway states which
 * relations it reads. The three the value set data names are read here; any
 * other is refused with a sentence that names it, rather than drawn as a list
 * that is empty.
 */
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

/// One setting with what it means and which values it accepts.
std::string setting_json_of(const engine_setting_row& setting,
                            const value_fields& fields,
                            const ores::reporting::domain::ore_parameter* documented) {
    const auto of_parameter =
        [documented](std::string ores::reporting::domain::ore_parameter::* field) {
            return documented == nullptr ? std::string() : documented->*field;
        };
    std::vector<std::string> choices;
    choices.reserve(fields.choices.size());
    for (const auto& choice : fields.choices)
        choices.push_back("{\"value\":" + json::write(choice.value) +
                          ",\"label\":" + json::write(choice.label) + "}");

    return "{\"column\":" + json::write(setting.column) + ",\"ore\":" + json::write(setting.ore) +
           ",\"changesNumbers\":" + (setting.changes_numbers ? "true" : "false") +
           ",\"valueSet\":" + json::write(fields.value_set) +
           ",\"sourceKind\":" + json::write(fields.source_kind) +
           ",\"catalogue\":" + json::write(fields.catalogue) +
           ",\"choices\":" + rr::array_of(choices) + ",\"minimum\":" + json::write(fields.minimum) +
           ",\"maximum\":" + json::write(fields.maximum) +
           ",\"pattern\":" + json::write(fields.pattern) +
           ",\"documented\":" + (documented != nullptr ? "true" : "false") + ",\"label\":" +
           json::write(of_parameter(&ores::reporting::domain::ore_parameter::label)) +
           ",\"help\":" + json::write(of_parameter(&ores::reporting::domain::ore_parameter::help)) +
           ",\"description\":" +
           json::write(of_parameter(&ores::reporting::domain::ore_parameter::description)) +
           ",\"defaultText\":" +
           json::write(of_parameter(&ores::reporting::domain::ore_parameter::default_text)) +
           ",\"format\":" +
           json::write(of_parameter(&ores::reporting::domain::ore_parameter::format)) +
           ",\"introducedIn\":" +
           json::write(of_parameter(&ores::reporting::domain::ore_parameter::introduced_in)) +
           ",\"removedIn\":" +
           json::write(of_parameter(&ores::reporting::domain::ore_parameter::removed_in)) +
           ",\"xpath\":" +
           json::write(of_parameter(&ores::reporting::domain::ore_parameter::xpath)) + "}";
}

/// The settings of each group, with what each means and which values it accepts.
std::string documentation_of(const http_request& req, nats_client& session) {
    const auto groups = engine_settings_rows::read_groups(req, session);
    const auto parameters =
        forwarding::forward_all(req,
                                session,
                                reporting_messaging::list_ore_parameters_request{},
                                &reporting_messaging::list_ore_parameters_response::parameters);
    std::map<std::string, const ores::reporting::domain::ore_parameter*> documented;
    for (const auto& parameter : parameters) {
        if (parameter.schema_name == ore_schema_code && parameter.scope == setup_scope_code)
            documented.emplace(parameter.name, &parameter);
    }

    const value_set_reader reader(value_set_tables_of(req, session),
                                  [&req, &session](std::string_view relation) {
                                      return catalogue_choices_of(req, session, relation);
                                  });

    std::vector<std::string> group_json;
    group_json.reserve(groups.size());
    for (const auto& group : groups) {
        std::vector<std::string> setting_json;
        setting_json.reserve(group.settings.size());
        for (const auto& setting : group.settings) {
            const auto found = documented.find(setting.ore);
            setting_json.push_back(
                setting_json_of(setting,
                                reader.fields_of(setup_scope_code, setting.ore),
                                found == documented.end() ? nullptr : found->second));
        }
        group_json.push_back("{\"id\":" + json::write(group.code) +
                             ",\"changesNumbers\":" + (group.changes_numbers ? "true" : "false") +
                             ",\"settings\":" + rr::array_of(setting_json) + "}");
    }
    return "{\"groups\":" + rr::array_of(group_json) + "}";
}

}

void engine_settings_routes::register_documentation_route(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    ores::nats::service::nats_client& session) {
    using forwarding::guarded;
    const auto built =
        router->get("/api/reporting/engine-settings/documentation")
            .summary("What each engine setting means and which values it accepts")
            .description("The groups of settings the data states, with each setting's "
                         "documentation and the value set that states what it may hold, so "
                         "the screen draws the engine's own rule and not a second copy of it.")
            .tags({"reporting"})
            .auth_required()
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_ore_parameters_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    try {
                        return http_response::json(documentation_of(req, session));
                    } catch (const value_set_error& e) {
                        rr::refuse(http_status::internal_server_error, "internal-error", e.what());
                    }
                });
            })
            .build();
    router->add_route(built);
    registry->register_route(built);
}
}

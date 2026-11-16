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
#include "ores.http/routes/reporting/pricing_vocabulary.hpp"
#include "ores.analytics.api/messaging/pricing_engine_type_protocol.hpp"
#include "ores.analytics.api/messaging/pricing_method_protocol.hpp"
#include "ores.analytics.api/messaging/pricing_model_engine_pair_protocol.hpp"
#include "ores.analytics.api/messaging/pricing_model_protocol.hpp"
#include "ores.analytics.api/messaging/pricing_pair_setting_protocol.hpp"
#include "ores.analytics.api/messaging/sensitivity_asset_class_protocol.hpp"
#include "ores.analytics.api/messaging/sensitivity_method_protocol.hpp"
#include "ores.analytics.api/messaging/sensitivity_template_protocol.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.core/routes/ore_value_sets.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/reporting/json_shape.hpp"
#include "ores.http.core/routes/ore_value_set_tables.hpp"
#include "ores.http/routes/reporting/ore_value_set_rows.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/messaging/instrument_code_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <algorithm>
#include <map>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::reporting {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace analytics_messaging = ores::analytics::messaging;
namespace refdata_messaging = ores::refdata::messaging;
namespace reporting_messaging = ores::reporting::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::routes::value_set_reader;
using ores::reporting::domain::ore_parameter;
using ores::nats::service::nats_client;
using shape::member;
using shape::object;

auto& pricing_vocabulary_lg() {
    static auto instance = ores::logging::make_logger("ores.http.reporting.pricing_vocabulary");
    return instance;
}

/// The schema whose documentation the vocabulary states.
constexpr std::string_view guide_schema_code = "ore.xsd";

/// The units and the codes of the tenors a period setting offers.
constexpr std::string_view period_units[]{"DAY", "WEEK", "MONTH", "YEAR"};

/// The documentation of one scope, subtype and name, or nothing.
using documentation = std::map<std::string, const ore_parameter*>;

std::string
documentation_key(std::string_view scope, std::string_view subtype, std::string_view name) {
    return std::string(scope) + "|" + std::string(subtype) + "|" + std::string(name);
}

const ore_parameter* documented_of(const documentation& documented,
                                   std::string_view scope,
                                   std::string_view subtype,
                                   std::string_view name) {
    const auto found = documented.find(documentation_key(scope, subtype, name));
    return found == documented.end() ? nullptr : found->second;
}

/// One field of a documentation row, or nothing when the guide never lists it.
std::string documented_field(const ore_parameter* doc, std::string ore_parameter::*field) {
    return doc == nullptr ? std::string() : doc->*field;
}

/// The eight members a documented setting states about itself.
void append_documentation(std::vector<std::string>& members, const ore_parameter* doc) {
    members.push_back(member("label", json::write(documented_field(doc, &ore_parameter::label))));
    members.push_back(member("help", json::write(documented_field(doc, &ore_parameter::help))));
    members.push_back(
        member("description", json::write(documented_field(doc, &ore_parameter::description))));
    members.push_back(
        member("defaultText", json::write(documented_field(doc, &ore_parameter::default_text))));
    members.push_back(member("format", json::write(documented_field(doc, &ore_parameter::format))));
    members.push_back(
        member("introducedIn", json::write(documented_field(doc, &ore_parameter::introduced_in))));
    members.push_back(
        member("removedIn", json::write(documented_field(doc, &ore_parameter::removed_in))));
    members.push_back(member("xpath", json::write(documented_field(doc, &ore_parameter::xpath))));
}

/// The five members a pairs row states about itself.
void append_pair_documentation(std::vector<std::string>& members, const ore_parameter* doc) {
    members.push_back(member("label", json::write(documented_field(doc, &ore_parameter::label))));
    members.push_back(member("help", json::write(documented_field(doc, &ore_parameter::help))));
    members.push_back(
        member("description", json::write(documented_field(doc, &ore_parameter::description))));
    members.push_back(
        member("defaultText", json::write(documented_field(doc, &ore_parameter::default_text))));
    members.push_back(member("xpath", json::write(documented_field(doc, &ore_parameter::xpath))));
}

/// A period a time setting offers: a count of days, weeks, months or years.
bool is_period(std::string_view unit, std::string_view code) {
    if (std::ranges::find(period_units, unit) == std::end(period_units))
        return false;
    if (code.size() < 2)
        return false;
    const auto unit_of_code = code.back();
    if (unit_of_code != 'D' && unit_of_code != 'W' && unit_of_code != 'M' && unit_of_code != 'Y')
        return false;
    return std::ranges::all_of(
        code.begin(), code.end() - 1, [](char c) { return c >= '0' && c <= '9'; });
}

/// The collections the editor's controls are drawn from, read once.
struct vocabulary_rows final {
    std::vector<ores::analytics::domain::pricing_engine_type> types;
    std::vector<ores::analytics::domain::pricing_model_engine_pair> pairs;
    std::vector<ores::analytics::domain::pricing_pair_setting> settings;
    std::vector<ores::analytics::domain::pricing_model> models;
    std::vector<ores::analytics::domain::pricing_method> methods;
    std::vector<ores::analytics::domain::sensitivity_template> templates;
    std::vector<ores::analytics::domain::sensitivity_method> template_methods;
    std::vector<ores::analytics::domain::sensitivity_asset_class> template_classes;
    std::vector<ores::refdata::domain::tenor> tenors;
    std::vector<ores::refdata::domain::instrument_code> instruments;
    std::vector<ore_parameter> parameters;
};

vocabulary_rows read_rows(const http_request& req, nats_client& session) {
    vocabulary_rows rows;
    rows.types =
        forwarding::forward_all(req,
                                session,
                                analytics_messaging::list_pricing_engine_types_request{},
                                &analytics_messaging::list_pricing_engine_types_response::types);
    rows.pairs = forwarding::forward_all(
        req,
        session,
        analytics_messaging::list_pricing_model_engine_pairs_request{},
        &analytics_messaging::list_pricing_model_engine_pairs_response::pairs);
    rows.settings = forwarding::forward_all(
        req,
        session,
        analytics_messaging::list_pricing_pair_settings_request{},
        &analytics_messaging::list_pricing_pair_settings_response::settings);
    rows.models =
        forwarding::forward_all(req,
                                session,
                                analytics_messaging::list_pricing_models_request{},
                                &analytics_messaging::list_pricing_models_response::models);
    rows.methods =
        forwarding::forward_all(req,
                                session,
                                analytics_messaging::list_pricing_methods_request{},
                                &analytics_messaging::list_pricing_methods_response::methods);
    rows.templates = forwarding::forward_all(
        req,
        session,
        analytics_messaging::list_sensitivity_templates_request{},
        &analytics_messaging::list_sensitivity_templates_response::templates);
    rows.template_methods =
        forwarding::forward_all(req,
                                session,
                                analytics_messaging::list_sensitivity_methods_request{},
                                &analytics_messaging::list_sensitivity_methods_response::methods);
    rows.template_classes = forwarding::forward_all(
        req,
        session,
        analytics_messaging::list_sensitivity_asset_classes_request{},
        &analytics_messaging::list_sensitivity_asset_classes_response::asset_classes);
    rows.tenors = forwarding::forward_all(req,
                                          session,
                                          refdata_messaging::list_tenors_request{},
                                          &refdata_messaging::list_tenors_response::tenors);
    rows.instruments =
        forwarding::forward_all(req,
                                session,
                                refdata_messaging::list_instrument_codes_request{},
                                &refdata_messaging::list_instrument_codes_response::instruments);
    rows.parameters =
        forwarding::forward_all(req,
                                session,
                                reporting_messaging::list_ore_parameters_request{},
                                &reporting_messaging::list_ore_parameters_response::parameters);
    return rows;
}

/// The documentation rows the guide states, by the scope, subtype and name they document.
documentation documented_rows(const std::vector<ore_parameter>& parameters) {
    documentation documented;
    for (const auto& parameter : parameters)
        if (parameter.schema_name == guide_schema_code)
            documented.emplace(
                documentation_key(parameter.scope, parameter.subtype, parameter.name), &parameter);
    return documented;
}

/// The value fields of one setting, as the value set tables state them.
std::vector<std::string> vocabulary_values_of(const value_set_reader& reader,
                                              std::string_view scope,
                                              std::string_view name) {
    return ores::http::routes::value_fields_members(reader.fields_of(scope, name));
}

/// Every value set a setting links to, keyed by the scope and name the tables use.
std::string value_sets_json(const value_set_reader& reader,
                            const ores::http::routes::value_set_tables& tables) {
    std::vector<std::string> members;
    members.reserve(tables.links.size());
    for (const auto& link : tables.links)
        members.push_back(member(link.scope + "|" + link.name,
                                 object(vocabulary_values_of(reader, link.scope, link.name))));
    return object(members);
}

std::string vocabulary_json(const http_request& req, nats_client& session) {
    const auto rows = read_rows(req, session);
    const auto documented = documented_rows(rows.parameters);
    const auto tables = ores::http::routes::read_value_set_tables(req, session);
    const auto reader = value_set_reader(tables, [&req, &session](std::string_view relation) {
        return value_set_rows::catalogue_choices_of(req, session, relation);
    });

    std::map<std::string, std::string> asset_class_of;
    for (const auto& instrument : rows.instruments)
        asset_class_of[instrument.code] = instrument.asset_class.value_or(std::string());
    std::map<std::string, std::string> method_name_of;
    for (const auto& method : rows.template_methods)
        method_name_of[method.code] = method.name;
    std::map<std::string, std::string> class_name_of;
    for (const auto& asset_class : rows.template_classes)
        class_name_of[asset_class.asset_class_code] = asset_class.name;

    std::vector<std::string> types;
    types.reserve(rows.types.size());
    for (const auto& type : rows.types) {
        const auto found = asset_class_of.find(type.instrument_type_code);
        const auto doc = documented_of(documented, "Types", "", type.code);
        types.push_back(object(
            {member("code", json::write(type.code)),
             member("description", json::write(type.description)),
             member("instrumentTypeCode", json::write(type.instrument_type_code)),
             member("assetClass",
                    json::write(found == asset_class_of.end() ? std::string() : found->second)),
             member("documentation",
                    json::write(documented_field(doc, &ore_parameter::description)))}));
    }

    std::vector<std::string> pairs;
    pairs.reserve(rows.pairs.size());
    for (const auto& pair : rows.pairs) {
        const auto doc = documented_of(
            documented, "Pairs", pair.pricing_engine_type_code, pair.model + "|" + pair.engine);
        auto members =
            std::vector<std::string>{member("typeCode", json::write(pair.pricing_engine_type_code)),
                                     member("model", json::write(pair.model)),
                                     member("engine", json::write(pair.engine)),
                                     member("usual", pair.is_usual ? "true" : "false")};
        append_pair_documentation(members, doc);
        pairs.push_back(object(members));
    }

    std::vector<std::string> settings;
    settings.reserve(rows.settings.size());
    for (const auto& setting : rows.settings) {
        const auto doc =
            documented_of(documented,
                          setting.scope == "model" ? "ModelParameters" : "EngineParameters",
                          setting.pricing_engine_type_code,
                          setting.name);
        auto members = std::vector<std::string>{
            member("typeCode", json::write(setting.pricing_engine_type_code)),
            member("model", json::write(setting.model)),
            member("engine", json::write(setting.engine)),
            member("scope", json::write(setting.scope)),
            member("name", json::write(setting.name)),
            member("usual", json::write(setting.usual_value))};
        const auto values = vocabulary_values_of(reader, setting.scope, setting.name);
        members.insert(members.end(), values.begin(), values.end());
        members.push_back(member("required", setting.is_required ? "true" : "false"));
        members.push_back(member("documented", doc != nullptr ? "true" : "false"));
        append_documentation(members, doc);
        settings.push_back(object(members));
    }
    // A setting the code reads but the guide never lists has no pair of its own.
    // Its row stands with no type, so it is never a choice for a pair and only
    // ever answers a lookup by name.
    for (const auto& parameter : rows.parameters) {
        if (parameter.schema_name != guide_schema_code || !parameter.subtype.empty())
            continue;
        if (parameter.scope != "ModelParameters" && parameter.scope != "EngineParameters")
            continue;
        const auto scope =
            parameter.scope == "ModelParameters" ? std::string("model") : std::string("engine");
        auto members = std::vector<std::string>{member("typeCode", json::write(std::string())),
                                                member("model", json::write(std::string())),
                                                member("engine", json::write(std::string())),
                                                member("scope", json::write(scope)),
                                                member("name", json::write(parameter.name)),
                                                member("usual", json::write(std::string()))};
        const auto values = vocabulary_values_of(reader, parameter.scope, parameter.name);
        members.insert(members.end(), values.begin(), values.end());
        members.push_back(member("required", "false"));
        members.push_back(member("documented", "true"));
        append_documentation(members, &parameter);
        settings.push_back(object(members));
    }

    std::vector<const ore_parameter*> global_rows;
    for (const auto& parameter : rows.parameters)
        if (parameter.schema_name == guide_schema_code && parameter.scope == "GlobalParameters")
            global_rows.push_back(&parameter);
    std::ranges::sort(global_rows, {}, [](const ore_parameter* row) { return row->name; });

    std::vector<std::string> globals;
    globals.reserve(global_rows.size());
    for (const auto* parameter : global_rows) {
        auto members = std::vector<std::string>{
            member("name", json::write(parameter->name)),
            member("domain", json::write(parameter->ore_parameter_value_domain_code))};
        const auto values = vocabulary_values_of(reader, "GlobalParameters", parameter->name);
        members.insert(members.end(), values.begin(), values.end());
        append_documentation(members, parameter);
        globals.push_back(object(members));
    }

    const auto model_rows = [&documented](const auto& collection, std::string_view scope) {
        std::vector<std::string> out;
        out.reserve(collection.size());
        for (const auto& row : collection) {
            const auto doc = documented_of(documented, scope, "", row.code);
            out.push_back(object(
                {member("code", json::write(row.code)),
                 member("description", json::write(documented_field(doc, &ore_parameter::help))),
                 member("detail", json::write(documented_field(doc, &ore_parameter::description))),
                 member("source", json::write(row.source))}));
        }
        return out;
    };
    const auto models = model_rows(rows.models, "Models");
    const auto methods = model_rows(rows.methods, "Engines");

    std::vector<std::string> templates;
    templates.reserve(rows.templates.size());
    for (const auto& row : rows.templates) {
        const auto asset_class = class_name_of.find(row.asset_class_code);
        const auto method = method_name_of.find(row.method_code);
        templates.push_back(object(
            {member("code", json::write(row.code)),
             member("methodCode", json::write(row.method_code)),
             member("assetClassCode", json::write(row.asset_class_code)),
             member("assetClass",
                    json::write(asset_class == class_name_of.end() ? std::string() :
                                                                     asset_class->second)),
             member("method",
                    json::write(method == method_name_of.end() ? row.method_code : method->second)),
             member("description", json::write(row.description))}));
    }

    std::vector<ores::refdata::domain::tenor> periods;
    for (const auto& tenor : rows.tenors)
        if (is_period(tenor.unit, tenor.code))
            periods.push_back(tenor);
    std::ranges::stable_sort(periods, {}, &ores::refdata::domain::tenor::sort_order);
    std::vector<std::string> tenors;
    tenors.reserve(periods.size());
    for (const auto& tenor : periods)
        tenors.push_back(object({member("code", json::write(tenor.code)),
                                 member("displayName", json::write(tenor.display_name))}));

    BOOST_LOG_SEV(pricing_vocabulary_lg(), ores::logging::debug)
        << "Stating the vocabulary: " << types.size() << " types, " << pairs.size() << " pairs, "
        << settings.size() << " settings, " << globals.size() << " globals, " << models.size()
        << " models, " << methods.size() << " methods, " << templates.size() << " templates, "
        << tenors.size() << " tenors and " << tables.links.size() << " value sets.";

    return object({member("valueSets", value_sets_json(reader, tables)),
                   member("types", rr::array_of(types)),
                   member("pairs", rr::array_of(pairs)),
                   member("settings", rr::array_of(settings)),
                   member("globals", rr::array_of(globals)),
                   member("models", rr::array_of(models)),
                   member("methods", rr::array_of(methods)),
                   member("templates", rr::array_of(templates)),
                   member("tenors", rr::array_of(tenors))});
}

}

void pricing_vocabulary::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"reporting"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/reporting/pricing-engines/vocabulary")
            .summary("The lookups a pricing engine editor draws its controls from")
            .description("The product types, the model and engine pairs, the settings each pair "
                         "carries, the globals, the models, the methods, the sensitivity "
                         "templates and the tenors, each with what the ORE guide says about it "
                         "and the value set that states which values it accepts.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<analytics_messaging::list_pricing_engine_types_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    return http_response::json(vocabulary_json(req, session));
                });
            }));
}

}

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
#include "ores.reporting.api/messaging/engine_settings_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_setting_value_set_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_value_set_catalogue_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_value_set_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_value_set_value_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <functional>
#include <map>
#include <optional>
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

/**
 * @brief The members of one JSON object, joined.
 *
 * A page here is a dozen small objects, so the members are listed and joined
 * rather than built one string at a time.
 */
std::string object_of(const std::vector<std::string>& members) {
    std::string out = "{";
    for (std::size_t i = 0; i < members.size(); ++i) {
        if (i != 0)
            out += ',';
        out += members[i];
    }
    return out + '}';
}

/// One member of a JSON object, its value already rendered.
std::string member_of(std::string_view name, std::string_view value) {
    return "\"" + std::string(name) + "\":" + std::string(value);
}

/// The values one part states, by the columns the group's settings name.
std::string values_of(const engine_group_row& group,
                      const std::map<std::string, std::string>& columns) {
    std::vector<std::string> members;
    members.reserve(group.settings.size());
    for (const auto& setting : group.settings) {
        const auto found = columns.find(setting.column);
        members.push_back(member_of(setting.column,
                                    found == columns.end() ? std::string_view("null") :
                                                             std::string_view(found->second)));
    }
    return object_of(members);
}

/// One part with the values of its group.
std::string part_json_of(const engine_part_row& part, const engine_group_row& group) {
    return object_of({member_of("id", json::write(part.id)),
                      member_of("version", std::to_string(part.version)),
                      member_of("values", values_of(group, part.columns)),
                      member_of("modifiedBy", json::write(part.modified_by)),
                      member_of("recordedAt", json::write(part.recorded_at)),
                      member_of("changeReason", json::write(part.change_reason)),
                      member_of("changeCommentary", json::write(part.change_commentary))});
}

/// One configuration as the page of settings states it.
std::string row_json_of(const ores::reporting::domain::engine_settings& row,
                        const std::vector<engine_group_row>& groups,
                        const engine_parts& parts,
                        const std::map<std::string, engine_party_name>& parties) {
    const auto id = boost::uuids::to_string(row.id);
    const auto party_id = boost::uuids::to_string(row.party_id);
    const auto party = parties.find(party_id);
    const auto party_name = party == parties.end() ? std::string() : party->second.name;
    const auto party_code = party == parties.end() ? std::string() : party->second.short_code;

    std::size_t groups_set = 0;
    std::string threads = "null";
    for (const auto& group : groups) {
        const auto of_group = parts.find(group.code);
        if (of_group == parts.end())
            continue;
        const auto part = of_group->second.find(id);
        if (part == of_group->second.end())
            continue;
        ++groups_set;
        if (group.code != "execution")
            continue;
        const auto column = part->second.columns.find("n_threads");
        if (column != part->second.columns.end())
            threads = column->second;
    }

    return object_of(
        {member_of("id", json::write(id)),
         member_of("partyId", json::write(party_id)),
         member_of("partyName", json::write(party_name)),
         member_of("partyCode", json::write(party_code)),
         member_of("version", std::to_string(row.version)),
         member_of("name", json::write(row.name)),
         member_of("description", json::write(row.description)),
         member_of("configurationId", json::write(boost::uuids::to_string(row.configuration_id))),
         member_of("groupsSet", std::to_string(groups_set)),
         member_of("groups", std::to_string(groups.size())),
         member_of("threads", threads),
         member_of("modifiedBy", json::write(row.modified_by)),
         member_of("recordedAt", json::write(row.recorded_at))});
}

/// One configuration with its groups and the reports that bind it.
std::string document_json_of(const ores::reporting::domain::engine_settings& row,
                             const std::vector<engine_group_row>& groups,
                             const engine_parts& parts,
                             const std::map<std::string, engine_party_name>& parties,
                             const std::optional<std::vector<engine_bound_report>>& reports) {
    const auto id = boost::uuids::to_string(row.id);
    const auto party_id = boost::uuids::to_string(row.party_id);
    const auto party = parties.find(party_id);
    const auto party_name = party == parties.end() ? std::string() : party->second.name;
    const auto party_code = party == parties.end() ? std::string() : party->second.short_code;

    std::vector<std::string> group_members;
    group_members.reserve(groups.size());
    for (const auto& group : groups) {
        const engine_part_row* found = nullptr;
        const auto of_group = parts.find(group.code);
        if (of_group != parts.end()) {
            const auto part = of_group->second.find(id);
            if (part != of_group->second.end())
                found = &part->second;
        }
        const auto part_text = found == nullptr ? std::string("null") : part_json_of(*found, group);
        group_members.push_back(member_of(group.code, part_text));
    }

    std::string reports_json = "null";
    if (reports) {
        std::vector<std::string> report_rows;
        report_rows.reserve(reports->size());
        for (const auto& report : *reports)
            report_rows.push_back(object_of({member_of("id", json::write(report.id)),
                                             member_of("name", json::write(report.name))}));
        reports_json = rr::array_of(report_rows);
    }

    return object_of(
        {member_of("id", json::write(id)),
         member_of("partyId", json::write(party_id)),
         member_of("partyName", json::write(party_name)),
         member_of("partyCode", json::write(party_code)),
         member_of("version", std::to_string(row.version)),
         member_of("name", json::write(row.name)),
         member_of("description", json::write(row.description)),
         member_of("configurationId", json::write(boost::uuids::to_string(row.configuration_id))),
         member_of("modifiedBy", json::write(row.modified_by)),
         member_of("recordedAt", json::write(row.recorded_at)),
         member_of("changeReason", json::write(row.change_reason_code)),
         member_of("changeCommentary", json::write(row.change_commentary)),
         member_of("groups", object_of(group_members)),
         member_of("reports", reports_json)});
}

/// One version of a configuration, or of one of its parts, as the history states it.
struct history_entry final {
    std::string kind;
    int version = 0;
    std::string modified_by;
    std::chrono::system_clock::time_point recorded_at;
    std::string change_reason;
    std::string change_commentary;
    std::string values;
};

/// The history, newest first.
std::string history_json_of(std::vector<history_entry> entries) {
    std::ranges::stable_sort(entries, std::greater<>{}, &history_entry::recorded_at);
    std::vector<std::string> rows;
    rows.reserve(entries.size());
    for (const auto& entry : entries)
        rows.push_back(
            object_of({member_of("kind", json::write(entry.kind)),
                       member_of("version", std::to_string(entry.version)),
                       member_of("modifiedBy", json::write(entry.modified_by)),
                       member_of("recordedAt", json::write(entry.recorded_at)),
                       member_of("changeReason", json::write(entry.change_reason)),
                       member_of("changeCommentary", json::write(entry.change_commentary)),
                       member_of("values", entry.values)}));
    return object_of({member_of("entries", rr::array_of(rows))});
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

void engine_settings_routes::register_read_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    ores::nats::service::nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"reporting"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/reporting/engine-settings")
            .summary("The engine settings a party holds")
            .description("One row per configuration: the party that holds it, the version, "
                         "how many groups hold a part of the whole, the threads the execution "
                         "part states, and who changed it when.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_engine_settings_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto groups = engine_settings_rows::read_groups(req, session);
                    const auto rows = engine_settings_rows::read_engine_settings(req, session);
                    const auto parties = engine_settings_rows::read_party_names(req, session);
                    const auto parts = engine_settings_rows::read_parts(req, session, {});
                    std::vector<std::string> state;
                    state.reserve(rows.size());
                    for (const auto& row : rows)
                        state.push_back(row_json_of(row, groups, parts, parties));
                    return http_response::json(object_of({member_of("rows", rr::array_of(state))}));
                });
            }));

    add(router->get("/api/reporting/engine-settings/{id}")
            .summary("One configuration with its groups and their parts")
            .description("The configuration, each group with the values its part holds or null "
                         "when the group has no row, and the reports that bind it.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_engine_settings_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto raw = req.get_path_param("id");
                    if (!rr::is_uuid(raw))
                        rr::invalid("Engine settings are named by their id.");
                    const auto id = boost::uuids::string_generator()(raw);
                    const auto groups = engine_settings_rows::read_groups(req, session);
                    const auto row = engine_settings_rows::read_central(req, session, id);
                    const auto parties = engine_settings_rows::read_party_names(req, session);
                    const auto parts = engine_settings_rows::read_parts(req, session, {id});
                    const auto reports = engine_settings_rows::read_bound_reports(
                        req, session, row.configuration_id);
                    return http_response::json(
                        document_json_of(row, groups, parts, parties, reports));
                });
            }));

    add(router->get("/api/reporting/engine-settings/{id}/history")
            .summary("Every version of one configuration and of its parts")
            .description("The central row's versions and every part's versions in one list, "
                         "newest first, each with the values it held and the reason it carries.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::list_engine_settings_versions_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto raw = req.get_path_param("id");
                    if (!rr::is_uuid(raw))
                        rr::invalid("Engine settings are named by their id.");
                    const auto id = boost::uuids::string_generator()(raw);
                    const auto row = engine_settings_rows::read_central(req, session, id);
                    const auto groups = engine_settings_rows::read_groups(req, session);
                    const auto parts = engine_settings_rows::read_parts(req, session, {id});

                    std::vector<history_entry> entries;
                    reporting_messaging::list_engine_settings_versions_request central_ask;
                    central_ask.key.name = row.name;
                    for (const auto& version : forwarding::forward_all(
                             req,
                             session,
                             central_ask,
                             &reporting_messaging::list_engine_settings_versions_response::
                                 versions)) {
                        if (version.id != id)
                            continue;
                        entries.push_back(
                            {std::string("central"),
                             version.version,
                             version.modified_by,
                             version.recorded_at,
                             version.change_reason_code,
                             version.change_commentary,
                             object_of(
                                 {member_of("name", json::write(version.name)),
                                  member_of("description", json::write(version.description))})});
                    }

                    const auto id_text = boost::uuids::to_string(id);
                    for (const auto& group : groups) {
                        const auto of_group = parts.find(group.code);
                        if (of_group == parts.end())
                            continue;
                        const auto part = of_group->second.find(id_text);
                        if (part == of_group->second.end())
                            continue;
                        for (const auto& version : engine_settings_rows::read_part_versions(
                                 req,
                                 session,
                                 group.code,
                                 boost::uuids::string_generator()(part->second.id))) {
                            entries.push_back({group.code,
                                               version.version,
                                               version.modified_by,
                                               version.recorded_at,
                                               version.change_reason,
                                               version.change_commentary,
                                               values_of(group, version.columns)});
                        }
                    }
                    return http_response::json(history_json_of(std::move(entries)));
                });
            }));
}
}

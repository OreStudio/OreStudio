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
#include "ores.http.api/net/json_shape.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.core/routes/ore_value_set_tables.hpp"
#include "ores.http.core/routes/ore_value_sets.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/reporting/engine_settings_rows.hpp"
#include "ores.http/routes/reporting/ore_value_set_rows.hpp"
#include "ores.http/routes/reporting/reporting_configuration_rows.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_protocol.hpp"
#include "ores.reporting.api/messaging/engine_settings_building_protocol.hpp"
#include "ores.reporting.api/messaging/engine_settings_execution_protocol.hpp"
#include "ores.reporting.api/messaging/engine_settings_fixings_protocol.hpp"
#include "ores.reporting.api/messaging/engine_settings_proposal_operations_protocol.hpp"
#include "ores.reporting.api/messaging/engine_settings_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_setting_value_set_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_value_set_catalogue_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_value_set_protocol.hpp"
#include "ores.reporting.api/messaging/ore_parameter_value_set_value_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/random_generator.hpp>
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
using ores::http::net::shape::member;
using ores::http::net::shape::object;

auto& engine_settings_lg() {
    static auto instance = ores::logging::make_logger("ores.http.reporting.engine_settings");
    return instance;
}

/// The reporting configuration type a report binds the settings through.
constexpr std::string_view settings_configuration_type = "engine_settings";

/// The schema and the scope whose parameters the documentation page states.
constexpr std::string_view ore_schema_code = "ore.xsd";
constexpr std::string_view setup_scope_code = "Setup";

/// One setting with what it means and which values it accepts.
std::string setting_json_of(const engine_setting_row& setting,
                            const value_fields& fields,
                            const ores::reporting::domain::ore_parameter* documented) {
    const auto of_parameter =
        [documented](std::string ores::reporting::domain::ore_parameter::*field) {
            return documented == nullptr ? std::string() : documented->*field;
        };
    auto members = std::vector<std::string>{
        member("column", json::write(setting.column)),
        member("ore", json::write(setting.ore)),
        member("changesNumbers", setting.changes_numbers ? "true" : "false")};
    const auto values = ores::http::routes::value_fields_members(fields);
    members.insert(members.end(), values.begin(), values.end());
    members.push_back(member("documented", documented != nullptr ? "true" : "false"));
    members.push_back(
        member("label", json::write(of_parameter(&ores::reporting::domain::ore_parameter::label))));
    members.push_back(
        member("help", json::write(of_parameter(&ores::reporting::domain::ore_parameter::help))));
    members.push_back(
        member("description",
               json::write(of_parameter(&ores::reporting::domain::ore_parameter::description))));
    members.push_back(
        member("defaultText",
               json::write(of_parameter(&ores::reporting::domain::ore_parameter::default_text))));
    members.push_back(member(
        "format", json::write(of_parameter(&ores::reporting::domain::ore_parameter::format))));
    members.push_back(
        member("introducedIn",
               json::write(of_parameter(&ores::reporting::domain::ore_parameter::introduced_in))));
    members.push_back(
        member("removedIn",
               json::write(of_parameter(&ores::reporting::domain::ore_parameter::removed_in))));
    members.push_back(
        member("xpath", json::write(of_parameter(&ores::reporting::domain::ore_parameter::xpath))));
    return object(members);
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

    const value_set_reader reader(ores::http::routes::read_value_set_tables(req, session),
                                  [&req, &session](std::string_view relation) {
                                      return value_set_rows::catalogue_choices_of(
                                          req, session, relation);
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

/// The values one part states, by the columns the group's settings name.
std::string values_of(const engine_group_row& group,
                      const std::map<std::string, std::string>& columns) {
    std::vector<std::string> members;
    members.reserve(group.settings.size());
    for (const auto& setting : group.settings) {
        const auto found = columns.find(setting.column);
        members.push_back(member(setting.column,
                                 found == columns.end() ? std::string_view("null") :
                                                          std::string_view(found->second)));
    }
    return object(members);
}

/// One part with the values of its group.
std::string part_json_of(const engine_part_row& part, const engine_group_row& group) {
    return object({member("id", json::write(part.id)),
                   member("version", std::to_string(part.version)),
                   member("values", values_of(group, part.columns)),
                   member("modifiedBy", json::write(part.modified_by)),
                   member("recordedAt", json::write(part.recorded_at)),
                   member("changeReason", json::write(part.change_reason)),
                   member("changeCommentary", json::write(part.change_commentary))});
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

    return object(
        {member("id", json::write(id)),
         member("partyId", json::write(party_id)),
         member("partyName", json::write(party_name)),
         member("partyCode", json::write(party_code)),
         member("version", std::to_string(row.version)),
         member("name", json::write(row.name)),
         member("description", json::write(row.description)),
         member("configurationId", json::write(boost::uuids::to_string(row.configuration_id))),
         member("groupsSet", std::to_string(groups_set)),
         member("groups", std::to_string(groups.size())),
         member("threads", threads),
         member("modifiedBy", json::write(row.modified_by)),
         member("recordedAt", json::write(row.recorded_at))});
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
        group_members.push_back(member(group.code, part_text));
    }

    std::string reports_json = "null";
    if (reports) {
        std::vector<std::string> report_rows;
        report_rows.reserve(reports->size());
        for (const auto& report : *reports)
            report_rows.push_back(object(
                {member("id", json::write(report.id)), member("name", json::write(report.name))}));
        reports_json = rr::array_of(report_rows);
    }

    return object(
        {member("id", json::write(id)),
         member("partyId", json::write(party_id)),
         member("partyName", json::write(party_name)),
         member("partyCode", json::write(party_code)),
         member("version", std::to_string(row.version)),
         member("name", json::write(row.name)),
         member("description", json::write(row.description)),
         member("configurationId", json::write(boost::uuids::to_string(row.configuration_id))),
         member("modifiedBy", json::write(row.modified_by)),
         member("recordedAt", json::write(row.recorded_at)),
         member("changeReason", json::write(row.change_reason_code)),
         member("changeCommentary", json::write(row.change_commentary)),
         member("groups", object(group_members)),
         member("reports", reports_json)});
}

/// What a screen reads of one configuration, as the answer states it.
http_response document_answer(const http_request& req,
                              nats_client& session,
                              const ores::reporting::domain::engine_settings& row,
                              http_status status) {
    const auto groups = engine_settings_rows::read_groups(req, session);
    const auto parties = engine_settings_rows::read_party_names(req, session);
    const auto parts = engine_settings_rows::read_parts(req, session, {row.id});
    std::optional<std::vector<engine_bound_report>> reports;
    try {
        reports = engine_settings_rows::read_bound_reports(
            req, session, settings_configuration_type, row.configuration_id);
    } catch (const std::exception&) {
        // A binding that cannot be read must not hide the settings, so the page
        // states it as unknown rather than refusing.
        BOOST_LOG_SEV(engine_settings_lg(), ores::logging::debug)
            << "The reports that bind configuration "
            << boost::uuids::to_string(row.configuration_id)
            << " could not be read, so the page states them as unknown.";
        reports = std::nullopt;
    }
    return http_response::json(document_json_of(row, groups, parts, parties, reports), status);
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
        rows.push_back(object({member("kind", json::write(entry.kind)),
                               member("version", std::to_string(entry.version)),
                               member("modifiedBy", json::write(entry.modified_by)),
                               member("recordedAt", json::write(entry.recorded_at)),
                               member("changeReason", json::write(entry.change_reason)),
                               member("changeCommentary", json::write(entry.change_commentary)),
                               member("values", entry.values)}));
    return object({member("entries", rr::array_of(rows))});
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
                    return http_response::json(object({member("rows", rr::array_of(state))}));
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
                    return document_answer(req,
                                           session,
                                           engine_settings_rows::read_central(req, session, id),
                                           http_status::ok);
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
                             object({member("name", json::write(version.name)),
                                     member("description", json::write(version.description))})});
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

/// A create as the form states it.
struct engine_create_body final {
    std::string name;
    std::string description;
    std::optional<std::string> party_id;
    rr::stated_intent intent;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_create_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"name", &engine_create_body::name},
                                            field{"description", &engine_create_body::description},
                                            field{"partyId", &engine_create_body::party_id},
                                            field{"intent", &engine_create_body::intent}};
}

/// An update as the form states it.
struct engine_update_body final {
    std::string name;
    std::string description;
    int version{};
    rr::stated_intent intent;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_update_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"name", &engine_update_body::name},
                                            field{"description", &engine_update_body::description},
                                            field{"version", &engine_update_body::version},
                                            field{"intent", &engine_update_body::intent}};
}

/// A removal as the form states it.
struct engine_remove_body final {
    rr::stated_intent intent;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<engine_remove_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"intent", &engine_remove_body::intent}};
}

namespace {

using ores::utility::domain::change_intent;
using ores::utility::domain::precondition_kind;

constexpr std::string_view engine_settings_owning_component = "ores.reporting";

/// The sentence a body of this unit states when it is not the shape the form sends.
constexpr std::string_view malformed_settings_write =
    "A write names the details it changes and its reason.";

using reporting_configuration_rows::intent_or_none;
using reporting_configuration_rows::remove_configuration;

/// The id a path states.
boost::uuids::uuid settings_id_of(const http_request& req) {
    const auto raw = req.get_path_param("id");
    if (!rr::is_uuid(raw))
        rr::invalid("Engine settings are named by their id.");
    return boost::uuids::string_generator()(raw);
}

/// The group with this code, refused when the data does not hold it.
const engine_group_row& group_of(const std::vector<engine_group_row>& groups,
                                 const std::string& code) {
    const auto found = std::ranges::find(groups, code, &engine_group_row::code);
    if (found == groups.end())
        rr::refuse(
            http_status::not_found, "not-found", "There is no such group of engine settings.");
    return *found;
}

/// Write the central row, at no version for a create or at the version read.
ores::reporting::domain::engine_settings
put_central(const http_request& req,
            nats_client& session,
            const reporting_messaging::engine_settings_write& write,
            std::optional<std::uint32_t> version,
            const change_intent& intent) {
    reporting_messaging::put_engine_settings_request msg;
    msg.change.write = write;
    msg.change.precondition.kind =
        version ? precondition_kind::must_match_version : precondition_kind::must_not_exist;
    msg.change.precondition.version = version;
    msg.intent = intent;
    const auto reply = forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);
    return reply.engine_settings ? *reply.engine_settings :
                                   engine_settings_rows::read_central(req, session, write.id);
}

/// One group's write as the form states it.
struct group_write final {
    std::optional<std::uint32_t> version;
    std::string row_json;
    change_intent intent;
};

/// The values a group write states, as the JSON the part's own codec reads.
group_write group_write_of(const http_request& req,
                           const engine_group_row& group,
                           const boost::uuids::uuid& settings_id,
                           const boost::uuids::uuid& row_id) {
    constexpr std::string_view malformed =
        "A group write names the version it was read at, every value of the group, and its reason.";
    rr::refuse_unknown_members(req.body, {"version", "values", "intent"}, std::string(malformed));
    auto parsed = ores::utility::codec::json_document::parse(req.body);
    if (!parsed)
        rr::invalid(std::string(malformed));
    const auto root = parsed->root();
    const auto values = root.member("values");
    if (!values.is_object())
        rr::invalid(std::string(malformed));

    std::string row_json =
        "{\"id\":" + json::write(boost::uuids::to_string(row_id)) +
        ",\"engine_settings_id\":" + json::write(boost::uuids::to_string(settings_id));
    for (const auto& [name, value] : values.members()) {
        if (std::ranges::find(group.settings, name, &engine_setting_row::column) ==
            group.settings.end())
            rr::invalid("The group " + group.code + " holds no setting called " + name + ".");
        row_json += "," + json::write(name) + ":" + value.write();
    }
    for (const auto& setting : group.settings) {
        if (!values.has(setting.column))
            rr::invalid("A group write names every setting of the group, and " + setting.column +
                        " is missing.");
    }
    row_json += "}";

    const auto version = root.member("version");
    const auto intent = root.member("intent");
    if (!version.exists() || !intent.exists())
        rr::invalid(std::string(malformed));

    group_write out;
    if (version.write() != "null") {
        const auto stated = version.integer();
        if (!stated || *stated < 1)
            rr::invalid("A group write states the version it was read at, or null when the group "
                        "has no row.");
        out.version = static_cast<std::uint32_t>(*stated);
    }
    const auto stated_intent = rr::read_body<rr::stated_intent>(intent.write());
    out.intent = stated_intent ? intent_or_none(*stated_intent) : change_intent{};
    out.row_json = std::move(row_json);
    return out;
}

/// The groups whose part a write reaches, in the words a part travels under.
enum class part_kind { building, execution, fixings };

part_kind kind_of(std::string_view code) {
    if (code == "building")
        return part_kind::building;
    if (code == "execution")
        return part_kind::execution;
    if (code == "fixings")
        return part_kind::fixings;
    rr::invalid("There is no such group of engine settings.");
}

/// Read one group's values as the row its part travels as.
template <typename Row>
Row read_part_row(const std::string& row_json) {
    try {
        return ores::utility::codec::wire_traits<Row>::read_json(row_json);
    } catch (const std::exception&) {
        rr::invalid("A group write states a value in a form the group does not hold.");
    }
}

/// Refuse a group's values when they are not the form its columns hold.
void check_values_of(part_kind kind, const std::string& row_json) {
    switch (kind) {
        case part_kind::building:
            static_cast<void>(
                read_part_row<reporting_messaging::engine_settings_building_write>(row_json));
            return;
        case part_kind::execution:
            static_cast<void>(
                read_part_row<reporting_messaging::engine_settings_execution_write>(row_json));
            return;
        case part_kind::fixings:
            static_cast<void>(
                read_part_row<reporting_messaging::engine_settings_fixings_write>(row_json));
            return;
    }
}

/// Write one group's part, at the version the form read or as a new row.
void write_part(const http_request& req,
                nats_client& session,
                part_kind kind,
                const group_write& body) {
    const auto precondition = [&body] {
        ores::utility::domain::precondition out;
        out.kind = body.version ? precondition_kind::must_match_version :
                                  precondition_kind::must_not_exist;
        out.version = body.version;
        return out;
    };
    switch (kind) {
        case part_kind::building: {
            reporting_messaging::put_engine_settings_building_request msg;
            msg.change.write =
                read_part_row<reporting_messaging::engine_settings_building_write>(body.row_json);
            msg.change.precondition = precondition();
            msg.intent = body.intent;
            rr::require_ok(forwarding::forward(req, session, msg).result);
            return;
        }
        case part_kind::execution: {
            reporting_messaging::put_engine_settings_execution_request msg;
            msg.change.write =
                read_part_row<reporting_messaging::engine_settings_execution_write>(body.row_json);
            msg.change.precondition = precondition();
            msg.intent = body.intent;
            rr::require_ok(forwarding::forward(req, session, msg).result);
            return;
        }
        case part_kind::fixings: {
            reporting_messaging::put_engine_settings_fixings_request msg;
            msg.change.write =
                read_part_row<reporting_messaging::engine_settings_fixings_write>(body.row_json);
            msg.change.precondition = precondition();
            msg.intent = body.intent;
            rr::require_ok(forwarding::forward(req, session, msg).result);
            return;
        }
    }
}

/// Remove one group's part.
void delete_part(const http_request& req,
                 nats_client& session,
                 part_kind kind,
                 const boost::uuids::uuid& part_id,
                 const change_intent& intent) {
    switch (kind) {
        case part_kind::building: {
            reporting_messaging::delete_engine_settings_building_request msg;
            msg.removal.key.id = part_id;
            msg.intent = intent;
            rr::require_ok(forwarding::forward(req, session, msg).result);
            return;
        }
        case part_kind::execution: {
            reporting_messaging::delete_engine_settings_execution_request msg;
            msg.removal.key.id = part_id;
            msg.intent = intent;
            rr::require_ok(forwarding::forward(req, session, msg).result);
            return;
        }
        case part_kind::fixings: {
            reporting_messaging::delete_engine_settings_fixings_request msg;
            msg.removal.key.id = part_id;
            msg.intent = intent;
            rr::require_ok(forwarding::forward(req, session, msg).result);
            return;
        }
    }
}

/// The reason a proposal carries, as the service states it.
std::string reason_of(const change_intent& intent) {
    const auto commentary = rr::trimmed(intent.commentary);
    return commentary.empty() ? intent.reason_code : intent.reason_code + ": " + commentary;
}

/// The text a proposal carries for one value; an empty text is the engine default.
std::string proposal_text_of(const ores::utility::codec::json_value& value) {
    if (!value.exists())
        return {};
    if (const auto text = value.text())
        return std::string(*text);
    if (value.is_number() || value.is_bool())
        return value.write();
    return {};
}

/**
 * @brief Hold a write to a numbers-changing group in an approval request.
 *
 * The service decides whether the group changes numbers and whether a report
 * binds the configuration, and refuses a proposal that is not one, so a caller
 * cannot make a governed change in place by asking for a proposal.
 */
std::string propose_group(const http_request& req,
                          nats_client& session,
                          const engine_group_row& group,
                          const boost::uuids::uuid& settings_id,
                          const group_write& body) {
    auto parsed = ores::utility::codec::json_document::parse(body.row_json);
    if (!parsed)
        rr::invalid("A group write states a value in a form the group does not hold.");
    const auto root = parsed->root();

    reporting_messaging::raise_engine_settings_change_request msg;
    msg.engine_settings_id = settings_id;
    msg.group_code = group.code;
    msg.base_version = static_cast<int>(body.version.value_or(0));
    for (const auto& setting : group.settings) {
        reporting_messaging::engine_settings_value value;
        value.setting_code = group.code + ":" + setting.column;
        value.text_value = proposal_text_of(root.member(setting.column));
        msg.values.push_back(std::move(value));
    }
    msg.reason = reason_of(body.intent);
    const auto reply = forwarding::forward(req, session, msg);
    rr::require_ok(reply.result);
    return reply.request_id;
}

}

void engine_settings_routes::register_write_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    ores::nats::service::nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"reporting"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->post("/api/reporting/engine-settings")
            .summary("Create a configuration and the reporting header a report binds it through")
            .description("The configuration a report binds is written first and the central row "
                         "second, and a refused central write removes it again, so a "
                         "create the service refuses leaves nothing behind.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::put_engine_settings_request>([&] {
                    const auto body = rr::lenient_body_of<engine_create_body>(
                        req,
                        {"name", "description", "partyId", "intent"},
                        std::string(malformed_settings_write));
                    const auto name = rr::trimmed(body.name);
                    if (name.empty() || rr::characters_of(name) > 200 ||
                        rr::characters_of(body.description) > 2000)
                        rr::invalid(std::string(malformed_settings_write));
                    const auto party = reporting_configuration_rows::party_for_new(
                        req, body.party_id, "Choose the party that holds the engine settings.");
                    const auto intent = intent_or_none(body.intent);
                    const auto configuration_id =
                        reporting_configuration_rows::register_configuration(
                            req,
                            session,
                            name,
                            settings_configuration_type,
                            engine_settings_owning_component,
                            intent);
                    const auto id = boost::uuids::random_generator()();
                    try {
                        reporting_messaging::engine_settings_write write;
                        write.id = id;
                        write.party_id = party;
                        write.name = name;
                        write.description = body.description;
                        write.configuration_id = configuration_id;
                        const auto central = put_central(req, session, write, std::nullopt, intent);
                        return document_answer(req, session, central, http_status::created);
                    } catch (...) {
                        remove_configuration(req, session, name, intent);
                        throw;
                    }
                });
            }));

    add(router->put("/api/reporting/engine-settings/{id}")
            .summary("Change the name and the description against the version read")
            .description("The party and the configuration the record fills travel from the record "
                         "just read, so a rename cannot move a configuration to another party.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::put_engine_settings_request>([&] {
                    const auto id = settings_id_of(req);
                    const auto body = rr::lenient_body_of<engine_update_body>(
                        req,
                        {"name", "description", "version", "intent"},
                        std::string(malformed_settings_write));
                    const auto name = rr::trimmed(body.name);
                    if (name.empty() || rr::characters_of(name) > 200 ||
                        rr::characters_of(body.description) > 2000 || body.version < 1)
                        rr::invalid(std::string(malformed_settings_write));
                    const auto central = engine_settings_rows::read_central(req, session, id);
                    reporting_messaging::engine_settings_write write;
                    write.id = id;
                    write.party_id = central.party_id;
                    write.name = name;
                    write.description = body.description;
                    write.configuration_id = central.configuration_id;
                    const auto written = put_central(req,
                                                     session,
                                                     write,
                                                     static_cast<std::uint32_t>(body.version),
                                                     intent_or_none(body.intent));
                    return document_answer(req, session, written, http_status::ok);
                });
            }));

    add(router->put("/api/reporting/engine-settings/{id}/groups/{group}")
            .summary("Write one group, or propose the write")
            .description("A group whose settings change the numbers a run computes, on a "
                         "configuration a report binds, is a proposal: nothing in force moves and "
                         "the answer states the approval. The service decides that it is one.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::put_engine_settings_execution_request>([&] {
                    const auto id = settings_id_of(req);
                    const auto groups = engine_settings_rows::read_groups(req, session);
                    const auto code = req.get_path_param("group");
                    const auto wired = engine_settings_rows::wired_groups();
                    if (std::ranges::find(wired, code) == wired.end())
                        rr::invalid("There is no such group of engine settings.");
                    const auto& group = group_of(groups, code);
                    const auto central = engine_settings_rows::read_central(req, session, id);
                    const auto parts = engine_settings_rows::read_parts(req, session, {id});

                    const engine_part_row* existing = nullptr;
                    const auto of_group = parts.find(group.code);
                    if (of_group != parts.end()) {
                        const auto part = of_group->second.find(boost::uuids::to_string(id));
                        if (part != of_group->second.end())
                            existing = &part->second;
                    }
                    const auto row_id = existing == nullptr ?
                                            boost::uuids::random_generator()() :
                                            boost::uuids::string_generator()(existing->id);
                    const auto body = group_write_of(req, group, id, row_id);

                    if (group.changes_numbers) {
                        // A read that fails is not a read that found no report: the
                        // governed case is the one that must not be got wrong, so a
                        // failure proposes rather than writes.
                        std::optional<std::vector<engine_bound_report>> reports;
                        try {
                            reports = engine_settings_rows::read_bound_reports(
                                req,
                                session,
                                settings_configuration_type,
                                central.configuration_id);
                        } catch (const std::exception&) {
                            reports = std::nullopt;
                        }
                        if (!reports || !reports->empty()) {
                            // A proposal states the values a write would, so they are
                            // read as the row the part's own codec writes before they
                            // travel: a value of the wrong form is refused here.
                            check_values_of(kind_of(group.code), body.row_json);
                            const auto request_id = propose_group(req, session, group, id, body);
                            const auto document =
                                document_answer(req, session, central, http_status::ok);
                            return http_response::json(
                                object({member("document", document.body),
                                        member("proposal",
                                               object({member("requestId",
                                                              json::write(request_id))}))}),
                                http_status::accepted);
                        }
                    }

                    write_part(req, session, kind_of(group.code), body);

                    const auto written = engine_settings_rows::read_central(req, session, id);
                    const auto document = document_answer(req, session, written, http_status::ok);
                    return http_response::json(
                        object({member("document", document.body), member("proposal", "null")}),
                        http_status::ok);
                });
            }));

    add(router->delete_("/api/reporting/engine-settings/{id}")
            .summary("Remove a configuration that no report binds")
            .description("A report that binds the configuration refuses the removal with 409. "
                         "Otherwise the parts go first, then the central row, then the header the "
                         "configuration was registered as.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<reporting_messaging::delete_engine_settings_request>([&] {
                    const auto id = settings_id_of(req);
                    const auto body = rr::lenient_body_of<engine_remove_body>(
                        req, {"intent"}, std::string(malformed_settings_write));
                    const auto intent = intent_or_none(body.intent);
                    const auto central = engine_settings_rows::read_central(req, session, id);
                    const auto reports = engine_settings_rows::read_bound_reports(
                        req, session, settings_configuration_type, central.configuration_id);
                    if (!reports.empty())
                        rr::refuse(http_status::conflict,
                                   "conflict",
                                   "A report binds these engine settings, so they cannot be "
                                   "deleted.");

                    const auto parts = engine_settings_rows::read_parts(req, session, {id});
                    const auto id_text = boost::uuids::to_string(id);
                    for (const auto& [code, of_code] : parts) {
                        const auto part = of_code.find(id_text);
                        if (part == of_code.end())
                            continue;
                        delete_part(req,
                                    session,
                                    kind_of(code),
                                    boost::uuids::string_generator()(part->second.id),
                                    intent);
                    }

                    reporting_messaging::delete_engine_settings_request msg;
                    msg.removal.key.name = central.name;
                    msg.removal.precondition.kind = precondition_kind::any;
                    msg.intent = intent;
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);
                    remove_configuration(req, session, central.name, intent);
                    return rr::no_content();
                });
            }));
}

}

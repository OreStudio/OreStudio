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
#include "ores.http/routes/reporting/engine_settings_rows.hpp"
#include "ores.http.api/domain/http_status.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.reporting.api/messaging/engine_settings_building_protocol.hpp"
#include "ores.reporting.api/messaging/engine_settings_execution_protocol.hpp"
#include "ores.reporting.api/messaging/engine_settings_fixings_protocol.hpp"
#include "ores.reporting.api/messaging/engine_settings_group_protocol.hpp"
#include "ores.reporting.api/messaging/engine_settings_group_setting_protocol.hpp"
#include "ores.reporting.api/messaging/engine_settings_protocol.hpp"
#include "ores.reporting.api/messaging/report_configuration_protocol.hpp"
#include "ores.reporting.api/messaging/report_definition_protocol.hpp"
#include "ores.utility/codec/json_document.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <array>
#include <exception>
#include <map>
#include <utility>
#include <vector>

namespace ores::http::routes::reporting {

namespace {

namespace forwarding = ores::http::forwarding;
namespace reporting_messaging = ores::reporting::messaging;
namespace iam_messaging = ores::iam::messaging;
namespace rr = ores::http::net::record_routes;
using ores::http::domain::http_request;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/**
 * @brief The groups whose part the gateway reads, in the words a part travels
 * under. @c read_parts names the same three tables, because a group is read
 * through the protocol that table owns.
 */
constexpr std::array<std::string_view, 3> wireable_groups{"building", "execution", "fixings"};

/// The reporting configuration type a report binds engine settings through.
constexpr std::string_view engine_settings_configuration_type = "engine_settings";

/**
 * @brief Every member of one row, by name, as the JSON it renders to.
 *
 * The row's own codec names its members, so a value is picked by the name the
 * setting table states without a second table saying it again.
 */
template <typename Row>
std::map<std::string, std::string> columns_of(const Row& row) {
    std::map<std::string, std::string> columns;
    auto parsed = ores::utility::codec::json_document::parse(
        ores::utility::codec::wire_traits<Row>::write_json(row));
    if (!parsed)
        return columns;
    for (const auto& [name, value] : parsed->root().members())
        columns.emplace(name, value.write());
    return columns;
}

template <typename Row>
engine_part_row part_of_row(const Row& row) {
    engine_part_row part;
    part.id = boost::uuids::to_string(row.id);
    part.version = row.version;
    part.modified_by = row.modified_by;
    part.recorded_at = row.recorded_at;
    part.change_reason = row.change_reason_code;
    part.change_commentary = row.change_commentary;
    part.columns = columns_of(row);
    return part;
}

/**
 * @brief Read one group's part table, for the configurations named or for all of
 * them.
 */
template <typename Request, typename Rows>
void fill_parts(const http_request& req,
                nats_client& session,
                const std::vector<boost::uuids::uuid>& ids,
                Rows rows,
                std::string_view code,
                engine_parts& parts) {
    Request ask;
    if (!ids.empty()) {
        ask.filter.emplace();
        ask.filter->engine_settings_id_one_of = ids;
    }
    auto& of_code = parts[std::string(code)];
    for (const auto& row : forwarding::forward_all(req, session, ask, rows))
        of_code.emplace(boost::uuids::to_string(row.engine_settings_id), part_of_row(row));
}

}

namespace engine_settings_rows {

std::span<const std::string_view> wired_groups() {
    return wireable_groups;
}

std::vector<engine_group_row> read_groups(const http_request& req, nats_client& session) {
    auto groups =
        forwarding::forward_all(req,
                                session,
                                reporting_messaging::list_engine_settings_groups_request{},
                                &reporting_messaging::list_engine_settings_groups_response::groups);
    auto settings = forwarding::forward_all(
        req,
        session,
        reporting_messaging::list_engine_settings_group_settings_request{},
        &reporting_messaging::list_engine_settings_group_settings_response::settings);

    std::ranges::stable_sort(
        settings, {}, &ores::reporting::domain::engine_settings_group_setting::position);
    std::map<std::string, std::vector<engine_setting_row>> of_group;
    for (auto& row : settings)
        of_group[row.group_code].push_back(engine_setting_row{
            std::move(row.column_name), std::move(row.ore_name), row.changes_numbers});

    std::ranges::stable_sort(groups, {}, &ores::reporting::domain::engine_settings_group::position);
    std::vector<engine_group_row> rows;
    rows.reserve(groups.size());
    for (auto& group : groups) {
        if (std::ranges::find(wireable_groups, group.code) == wireable_groups.end())
            continue;
        engine_group_row row;
        row.code = std::move(group.code);
        const auto found = of_group.find(row.code);
        if (found != of_group.end())
            row.settings = std::move(found->second);
        row.changes_numbers =
            std::ranges::any_of(row.settings, [](const engine_setting_row& setting) {
                return setting.changes_numbers;
            });
        rows.push_back(std::move(row));
    }
    return rows;
}

engine_parts read_parts(const http_request& req,
                        nats_client& session,
                        const std::vector<boost::uuids::uuid>& ids) {
    engine_parts parts;
    fill_parts<reporting_messaging::list_engine_settings_building_request>(
        req,
        session,
        ids,
        &reporting_messaging::list_engine_settings_building_response::engine_settings_building,
        "building",
        parts);
    fill_parts<reporting_messaging::list_engine_settings_execution_request>(
        req,
        session,
        ids,
        &reporting_messaging::list_engine_settings_execution_response::engine_settings_execution,
        "execution",
        parts);
    fill_parts<reporting_messaging::list_engine_settings_fixings_request>(
        req,
        session,
        ids,
        &reporting_messaging::list_engine_settings_fixings_response::engine_settings_fixings,
        "fixings",
        parts);
    return parts;
}

std::vector<ores::reporting::domain::engine_settings> read_engine_settings(const http_request& req,
                                                                           nats_client& session) {
    return forwarding::forward_all(req,
                                   session,
                                   reporting_messaging::list_engine_settings_request{},
                                   &reporting_messaging::list_engine_settings_response::settings);
}

ores::reporting::domain::engine_settings
read_central(const http_request& req, nats_client& session, const boost::uuids::uuid& id) {
    reporting_messaging::list_engine_settings_request ask;
    ask.limit = 1;
    ask.filter.emplace();
    ask.filter->id_one_of = std::vector<boost::uuids::uuid>{id};
    const auto found = forwarding::forward(req, session, ask);
    rr::require_ok(found.result);
    if (found.settings.empty())
        rr::refuse(http_status::not_found, "not-found", "No engine settings have this id.");
    return found.settings.front();
}

std::map<std::string, engine_party_name> read_party_names(const http_request& req,
                                                          nats_client& session) {
    std::map<std::string, engine_party_name> names;
    try {
        const auto reply =
            forwarding::forward(req, session, iam_messaging::get_my_parties_request{});
        for (const auto& party : reply.parties)
            names.emplace(party.party_id, engine_party_name{party.name, party.short_code});
    } catch (const std::exception&) {
        // A read that fails leaves the names empty: the page still states the
        // configuration it could read.
    }
    return names;
}

std::vector<engine_bound_report> read_bound_reports(const http_request& req,
                                                    nats_client& session,
                                                    const boost::uuids::uuid& configuration_id) {
    {
        reporting_messaging::list_report_configurations_request ask;
        ask.filter.emplace();
        ask.filter->configuration_type_code = std::string(engine_settings_configuration_type);
        ask.filter->configuration_id = configuration_id;
        const auto bindings = forwarding::forward_all(
            req,
            session,
            ask,
            &reporting_messaging::list_report_configurations_response::report_configurations);

        std::vector<boost::uuids::uuid> ids;
        ids.reserve(bindings.size());
        for (const auto& binding : bindings)
            ids.push_back(binding.report_definition_id);
        std::ranges::sort(ids);
        ids.erase(std::unique(ids.begin(), ids.end()), ids.end());
        if (ids.empty())
            return std::vector<engine_bound_report>{};

        reporting_messaging::list_report_definitions_request definitions_ask;
        definitions_ask.filter.emplace();
        definitions_ask.filter->id_one_of = ids;
        const auto definitions = forwarding::forward_all(
            req,
            session,
            definitions_ask,
            &reporting_messaging::list_report_definitions_response::definitions);

        std::vector<engine_bound_report> reports;
        reports.reserve(definitions.size());
        for (const auto& definition : definitions)
            reports.push_back({boost::uuids::to_string(definition.id), definition.name});
        return reports;
    }
}

std::vector<engine_part_row> read_part_versions(const http_request& req,
                                                nats_client& session,
                                                std::string_view group_code,
                                                const boost::uuids::uuid& part_id) {
    std::vector<engine_part_row> versions;
    const auto collect = [&versions](const auto& rows) {
        for (const auto& row : rows)
            versions.push_back(part_of_row(row));
    };
    if (group_code == "building") {
        reporting_messaging::list_engine_settings_building_versions_request ask;
        ask.key.id = part_id;
        collect(forwarding::forward_all(
            req,
            session,
            ask,
            &reporting_messaging::list_engine_settings_building_versions_response::versions));
    } else if (group_code == "execution") {
        reporting_messaging::list_engine_settings_execution_versions_request ask;
        ask.key.id = part_id;
        collect(forwarding::forward_all(
            req,
            session,
            ask,
            &reporting_messaging::list_engine_settings_execution_versions_response::versions));
    } else if (group_code == "fixings") {
        reporting_messaging::list_engine_settings_fixings_versions_request ask;
        ask.key.id = part_id;
        collect(forwarding::forward_all(
            req,
            session,
            ask,
            &reporting_messaging::list_engine_settings_fixings_versions_response::versions));
    }
    return versions;
}

}

}

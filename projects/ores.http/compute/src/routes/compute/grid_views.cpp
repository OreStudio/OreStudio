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
#include "ores.http/routes/compute/grid_views.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.reporting.api/messaging/report_instance_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <cctype>
#include <chrono>
#include <cstdint>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::compute {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace iam_messaging = ores::iam::messaging;
namespace reporting_messaging = ores::reporting::messaging;

using ores::http::domain::http_request;
using ores::nats::service::nats_client;

namespace {

/// The trail a read leaves when a decoration of a page is absent.
auto& views_lg() {
    static auto instance = ores::logging::make_logger("ores.http.compute.grid_views");
    return instance;
}

/// The text as a key, so a reference and an id compare whatever their case.
std::string lowered(std::string_view text) {
    std::string out(text);
    std::ranges::transform(
        out, out.begin(), [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return out;
}

/// The whole number a parameter states, or nothing when it states none.
std::optional<std::uint32_t> whole_of(const std::string& raw, std::string_view sentence) {
    if (raw.empty())
        return std::nullopt;
    std::uint32_t value = 0;
    for (const char c : raw) {
        if (c < '0' || c > '9' || value > 400'000'000)
            rr::invalid(std::string(sentence));
        value = value * 10 + static_cast<std::uint32_t>(c - '0');
    }
    return value;
}

}

namespace grid {

std::optional<std::string> stated_id(const boost::uuids::uuid& id) {
    if (id == boost::uuids::nil_uuid())
        return std::nullopt;
    return rr::as_text(id);
}

std::optional<std::string> stated_id(const std::optional<boost::uuids::uuid>& id) {
    return id ? stated_id(*id) : std::nullopt;
}

boost::uuids::uuid id_of(const http_request& req) {
    const auto raw = req.get_path_param("id");
    if (!rr::is_uuid(raw))
        rr::invalid(std::string(id_refused));
    return boost::uuids::string_generator()(raw);
}

page page_of(const http_request& req, std::string_view sentence) {
    page out;
    out.limit = page_default;
    if (const auto stated = whole_of(req.get_query_param("offset"), sentence))
        out.offset = *stated;
    if (const auto stated = whole_of(req.get_query_param("limit"), sentence)) {
        if (*stated < 1 || *stated > page_max)
            rr::invalid(std::string(sentence));
        out.limit = *stated;
    }
    return out;
}

bool is_system_tenant(const http_request& req) {
    const auto& claims = req.authenticated_user;
    return claims && claims->tenant_id && *claims->tenant_id == rr::system_tenant_id;
}

bool may_read_host(const http_request& req) {
    return is_system_tenant(req);
}

std::map<std::string, std::string> party_names(const http_request& req, nats_client& session) {
    std::map<std::string, std::string> names;
    try {
        const auto reply =
            forwarding::forward(req, session, iam_messaging::get_my_parties_request{});
        rr::require_done(reply.result);
        for (const auto& party : reply.parties)
            names[lowered(party.party_id)] = party.name;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(views_lg(), ores::logging::warn)
            << "The names of the parties were not read: " << e.what();
    }
    return names;
}

std::map<std::string, std::string> report_run_names(const http_request& req,
                                                    nats_client& session,
                                                    const std::vector<std::string>& references) {
    std::vector<boost::uuids::uuid> ids;
    std::set<std::string> seen;
    for (const auto& reference : references) {
        if (!rr::is_uuid(reference))
            continue;
        if (!seen.emplace(lowered(reference)).second)
            continue;
        ids.push_back(boost::uuids::string_generator()(reference));
    }
    if (ids.empty())
        return {};

    std::map<std::string, std::string> names;
    try {
        reporting_messaging::list_report_instances_request msg;
        msg.limit = static_cast<std::uint32_t>(ids.size());
        msg.filter.emplace();
        msg.filter->id_one_of = std::move(ids);
        const auto reply = forwarding::forward(req, session, msg);
        rr::require_done(reply.result);
        for (const auto& run : reply.instances)
            names[lowered(rr::as_text(run.id))] = run.name;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(views_lg(), ores::logging::warn)
            << "The names of the report runs were not read: " << e.what();
    }
    return names;
}

std::string batch_json(const ores::compute::domain::batch& batch,
                       const std::map<std::string, std::string>& runs,
                       const std::map<std::string, std::string>& parties) {
    const auto party = stated_id(batch.party_id);
    std::optional<std::string> party_name;
    if (party) {
        const auto found = parties.find(lowered(*party));
        if (found != parties.end())
            party_name = found->second;
    }
    const auto run = runs.find(lowered(batch.external_ref));
    std::optional<std::string> run_name;
    if (run != runs.end())
        run_name = run->second;

    return "{\"id\":" + json::write(rr::as_text(batch.id)) +
           ",\"external_ref\":" + json::write(batch.external_ref) +
           ",\"report_run_name\":" + json::write(run_name) + ",\"party_id\":" + json::write(party) +
           ",\"party_name\":" + json::write(party_name) +
           ",\"status\":" + json::write(batch.status) +
           ",\"modified_by\":" + json::write(batch.modified_by) +
           ",\"recorded_at\":" + json::write(batch.recorded_at) + "}";
}

std::string workunit_json(const ores::compute::domain::workunit& workunit) {
    std::optional<std::string> config;
    if (!workunit.config_uri.empty())
        config = workunit.config_uri;
    return "{\"id\":" + json::write(rr::as_text(workunit.id)) +
           ",\"batch_id\":" + json::write(rr::as_text(workunit.batch_id)) +
           ",\"app_version_id\":" + json::write(rr::as_text(workunit.app_version_id)) +
           ",\"input_uri\":" + json::write(workunit.input_uri) +
           ",\"config_uri\":" + json::write(config) +
           ",\"priority\":" + json::write(workunit.priority) +
           ",\"target_redundancy\":" + json::write(workunit.target_redundancy) +
           ",\"canonical_result_id\":" + json::write(stated_id(workunit.canonical_result_id)) +
           ",\"modified_by\":" + json::write(workunit.modified_by) +
           ",\"recorded_at\":" + json::write(workunit.recorded_at) + "}";
}

std::string result_json(const ores::compute::domain::result& result, bool host_visible) {
    const auto host = stated_id(result.host_id);
    const auto unknown = [](const std::chrono::system_clock::time_point& stated)
        -> std::optional<std::chrono::system_clock::time_point> {
        if (stated == std::chrono::system_clock::time_point{})
            return std::nullopt;
        return stated;
    };
    std::optional<std::string> output;
    if (!result.output_uri.empty())
        output = result.output_uri;
    std::optional<std::string> error;
    if (!result.error_message.empty())
        error = result.error_message;
    return "{\"id\":" + json::write(rr::as_text(result.id)) +
           ",\"workunit_id\":" + json::write(rr::as_text(result.workunit_id)) +
           ",\"host_id\":" + json::write(host_visible ? host : std::nullopt) +
           ",\"host_hidden\":" + json::write(!host_visible && host.has_value()) +
           ",\"server_state\":" + json::write(result.server_state) +
           ",\"outcome\":" + json::write(result.outcome) +
           ",\"output_uri\":" + json::write(output) + ",\"error_message\":" + json::write(error) +
           ",\"received_at\":" + json::write(unknown(result.received_at)) +
           ",\"started_at\":" + json::write(unknown(result.started_at)) +
           ",\"duration_ms\":" + json::write(result.duration_ms) +
           ",\"modified_by\":" + json::write(result.modified_by) +
           ",\"recorded_at\":" + json::write(result.recorded_at) + "}";
}

std::string
page_json(const std::vector<std::string>& rows, std::uint64_t total, const page& asked) {
    return "{\"read_at\":" + json::write(std::chrono::system_clock::now()) +
           ",\"rows\":" + rr::array_of(rows) + ",\"total\":" + std::to_string(total) +
           ",\"offset\":" + std::to_string(asked.offset) +
           ",\"limit\":" + std::to_string(asked.limit) + "}";
}

}

}

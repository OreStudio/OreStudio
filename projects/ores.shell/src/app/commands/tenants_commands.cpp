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
#include "ores.shell/app/commands/tenants_commands.hpp"

#include <ostream>
#include <functional>
#include <rfl/json.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <cli/cli.h>
#include "ores.platform/time/datetime.hpp"
#include "ores.iam.api/messaging/tenant_protocol.hpp"
#include "ores.iam.api/domain/tenant_table_io.hpp" // IWYU pragma: keep.

namespace ores::shell::app::commands {

using namespace logging;
using service::nats_session;

namespace {

std::string format_time(std::chrono::system_clock::time_point tp) {
    return ores::platform::time::datetime::format_time_point(tp);
}

template<typename Response>
std::optional<Response> do_auth_request(std::ostream& out, nats_session& session,
    const std::string& subject, const std::string& body) {
    try {
        auto reply = session.authenticated_request(subject, body);
        auto data_str = std::string(
            reinterpret_cast<const char*>(reply.data.data()), reply.data.size());
        auto result = rfl::json::read<Response>(data_str);
        if (!result) {
            out << "✗ Failed to parse response" << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        out << "✗ Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

}  // anonymous namespace

void tenants_commands::
register_commands(cli::Menu& root_menu, nats_session& session,
                  pagination_context& /*pagination*/) {
    auto tenants_menu =
        std::make_unique<cli::Menu>("tenants");

    tenants_menu->Insert("get", [&session](std::ostream& out) {
        process_get_tenants(std::ref(out), std::ref(session), false);
    }, "Retrieve active tenants from the server");

    tenants_menu->Insert("get-all", [&session](std::ostream& out) {
        process_get_tenants(std::ref(out), std::ref(session), true);
    }, "Retrieve all tenants including deleted");

    tenants_menu->Insert("add", [&session](std::ostream& out,
            std::string code, std::string name, std::string type,
            std::string hostname, std::string description) {
        process_add_tenant(std::ref(out), std::ref(session),
            std::move(code), std::move(name), std::move(type),
            std::move(hostname), std::move(description));
    }, "Add a tenant (code name type hostname description)");

    tenants_menu->Insert("history", [&session](std::ostream& out,
            std::string tenant_id) {
        process_tenant_history(std::ref(out), std::ref(session),
            std::move(tenant_id));
    }, "Show history for a tenant (tenant_id)");

    tenants_menu->Insert("delete", [&session](std::ostream& out,
            std::string tenant_id) {
        process_delete_tenant(std::ref(out), std::ref(session),
            std::move(tenant_id));
    }, "Delete a tenant (tenant_id)");

    root_menu.Insert(std::move(tenants_menu));
}

void tenants_commands::
process_get_tenants(std::ostream& out, nats_session& session,
    bool include_deleted) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get tenants request. "
                               << "include_deleted=" << include_deleted;

    iam::messaging::get_tenants_request req;
    req.include_deleted = include_deleted;

    auto result = do_auth_request<iam::messaging::get_tenants_response>(
        out, session, "iam.v1.tenants.list", rfl::json::write(req));
    if (!result) return;

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->tenants.size() << " tenants.";
    out << result->tenants << std::endl;
}

void tenants_commands::
process_add_tenant(std::ostream& out, nats_session& session,
    std::string code, std::string name, std::string type,
    std::string hostname, std::string description) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add tenant request for code: "
                               << code;

    // Get modified_by from logged-in user
    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to add a tenant." << std::endl;
        return;
    }
    const auto& modified_by = session.auth().username;

    // Generate a new UUID for the tenant
    boost::uuids::random_generator gen;
    auto new_id = gen();

    auto req = iam::messaging::save_tenant_request::from(iam::domain::tenant{
        .version = 0,
        .id = new_id,
        .code = std::move(code),
        .name = std::move(name),
        .type = std::move(type),
        .description = std::move(description),
        .hostname = std::move(hostname),
        .status = "active",
        .modified_by = modified_by,
        .change_reason_code = "new_record",
        .change_commentary = "Created via shell",
        .recorded_at = std::chrono::system_clock::now()
    });

    auto result = do_auth_request<iam::messaging::save_tenant_response>(
        out, session, "iam.v1.tenants.save", rfl::json::write(req));
    if (!result) return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added tenant with ID: "
                                  << new_id;
        out << "✓ Tenant added successfully!" << std::endl;
        out << "  ID: " << new_id << std::endl;
    } else {
        const auto& msg = result->message.empty() ? "Unknown error" : result->message;
        BOOST_LOG_SEV(lg(), warn) << "Failed to add tenant: " << msg;
        out << "✗ Failed to add tenant: " << msg << std::endl;
    }
}

void tenants_commands::
process_tenant_history(std::ostream& out, nats_session& session,
    std::string tenant_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating tenant history request for: "
                               << tenant_id;

    // Validate UUID format
    try {
        boost::lexical_cast<boost::uuids::uuid>(tenant_id);
    } catch (const boost::bad_lexical_cast&) {
        BOOST_LOG_SEV(lg(), error) << "Invalid tenant ID format: " << tenant_id;
        out << "✗ Invalid tenant ID format. Expected UUID." << std::endl;
        return;
    }

    iam::messaging::get_tenant_history_request req;
    req.id = tenant_id;

    auto result = do_auth_request<iam::messaging::get_tenant_history_response>(
        out, session, "iam.v1.tenants.history", rfl::json::write(req));
    if (!result) return;

    if (!result->success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get tenant history: "
                                  << result->message;
        out << "✗ " << result->message << std::endl;
        return;
    }

    const auto& versions = result->versions;
    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << versions.size() << " history entries.";

    if (versions.empty()) {
        out << "No history found for tenant: " << tenant_id << std::endl;
        return;
    }

    out << "History for tenant " << tenant_id << " ("
        << versions.size() << " versions):" << std::endl;
    out << std::string(80, '-') << std::endl;

    for (const auto& entry : versions) {
        out << "  Version " << entry.version << std::endl;
        out << "    Code: " << entry.code << std::endl;
        out << "    Name: " << entry.name << std::endl;
        out << "    Type: " << entry.type << std::endl;
        out << "    Hostname: " << entry.hostname << std::endl;
        out << "    Status: " << entry.status << std::endl;
        out << "    Recorded: " << format_time(entry.recorded_at)
            << " by " << entry.modified_by << std::endl;
        if (!entry.change_reason_code.empty()) {
            out << "    Reason: " << entry.change_reason_code;
            if (!entry.change_commentary.empty()) {
                out << " - " << entry.change_commentary;
            }
            out << std::endl;
        }
        out << std::endl;
    }
}

void tenants_commands::
process_delete_tenant(std::ostream& out, nats_session& session,
    std::string tenant_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete tenant request for: "
                               << tenant_id;

    if (!session.is_logged_in()) {
        out << "✗ You must be logged in to delete a tenant." << std::endl;
        return;
    }

    // Validate UUID format
    try {
        boost::lexical_cast<boost::uuids::uuid>(tenant_id);
    } catch (const boost::bad_lexical_cast&) {
        BOOST_LOG_SEV(lg(), error) << "Invalid tenant ID format: " << tenant_id;
        out << "✗ Invalid tenant ID format. Expected UUID." << std::endl;
        return;
    }

    iam::messaging::delete_tenant_request req;
    req.ids = {tenant_id};

    auto result = do_auth_request<iam::messaging::delete_tenant_response>(
        out, session, "iam.v1.tenants.delete", rfl::json::write(req));
    if (!result) return;

    if (result->success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted tenant: " << tenant_id;
        out << "✓ Tenant deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete tenant: " << result->message;
        out << "✗ Failed to delete tenant: " << result->message << std::endl;
    }
}

}

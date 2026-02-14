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
#include "ores.comms.shell/app/commands/tenants_commands.hpp"

#include <ostream>
#include <functional>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <cli/cli.h>
#include "ores.platform/time/datetime.hpp"
#include "ores.comms/messaging/message_type.hpp"
#include "ores.database/domain/change_reason_constants.hpp"
#include "ores.iam/messaging/tenant_protocol.hpp"
#include "ores.iam/domain/tenant_table_io.hpp" // IWYU pragma: keep.

namespace ores::comms::shell::app::commands {

using namespace logging;
using comms::messaging::message_type;
using comms::net::client_session;
namespace reason = database::domain::change_reason_constants;

namespace {

std::string format_time(std::chrono::system_clock::time_point tp) {
    return ores::platform::time::datetime::format_time_point(tp);
}

}  // anonymous namespace

void tenants_commands::
register_commands(cli::Menu& root_menu, client_session& session,
                  pagination_context& /*pagination*/) {
    // Note: Tenant protocol doesn't support pagination yet, so pagination_context
    // is unused but passed for API consistency.
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
process_get_tenants(std::ostream& out, client_session& session,
    bool include_deleted) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating get tenants request. "
                               << "include_deleted=" << include_deleted;

    using iam::messaging::get_tenants_request;
    using iam::messaging::get_tenants_response;
    auto result = session.process_authenticated_request<get_tenants_request,
                                                        get_tenants_response,
                                                        message_type::get_tenants_request>
        (get_tenants_request{.include_deleted = include_deleted});

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved "
                              << result->tenants.size() << " tenants.";
    out << result->tenants << std::endl;
}

void tenants_commands::
process_add_tenant(std::ostream& out, client_session& session,
    std::string code, std::string name, std::string type,
    std::string hostname, std::string description) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating add tenant request for code: "
                               << code;

    // Get modified_by from logged-in user
    const auto& session_info = session.session_info();
    if (!session_info) {
        out << "✗ You must be logged in to add a tenant." << std::endl;
        return;
    }
    const auto& modified_by = session_info->username;

    // Generate a new UUID for the tenant
    boost::uuids::random_generator gen;
    auto new_id = gen();

    using iam::messaging::save_tenant_request;
    using iam::messaging::save_tenant_response;
    auto result = session.process_authenticated_request<save_tenant_request,
                                                        save_tenant_response,
                                                        message_type::save_tenant_request>
        (save_tenant_request{
            .tenant = iam::domain::tenant{
                .version = 0,
                .id = new_id,
                .code = std::move(code),
                .name = std::move(name),
                .type = std::move(type),
                .description = std::move(description),
                .hostname = std::move(hostname),
                .status = "active",
                .modified_by = modified_by,
                .change_reason_code = std::string{reason::codes::new_record},
                .change_commentary = "Created via shell",
                .recorded_at = std::chrono::system_clock::now()
            }
        });

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (response.success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully added tenant with ID: "
                                  << new_id;
        out << "✓ Tenant added successfully!" << std::endl;
        out << "  ID: " << new_id << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to add tenant: "
                                  << response.message;
        out << "✗ Failed to add tenant: " << response.message << std::endl;
    }
}

void tenants_commands::
process_tenant_history(std::ostream& out, client_session& session,
    std::string tenant_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating tenant history request for: "
                               << tenant_id;

    boost::uuids::uuid parsed_id;
    try {
        parsed_id = boost::lexical_cast<boost::uuids::uuid>(tenant_id);
    } catch (const boost::bad_lexical_cast&) {
        BOOST_LOG_SEV(lg(), error) << "Invalid tenant ID format: " << tenant_id;
        out << "✗ Invalid tenant ID format. Expected UUID." << std::endl;
        return;
    }

    using iam::messaging::get_tenant_history_request;
    using iam::messaging::get_tenant_history_response;
    auto result = session.process_authenticated_request<get_tenant_history_request,
                                                        get_tenant_history_response,
                                                        message_type::get_tenant_history_request>
        (get_tenant_history_request{.id = parsed_id});

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (!response.success) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get tenant history: "
                                  << response.message;
        out << "✗ " << response.message << std::endl;
        return;
    }

    const auto& versions = response.versions;
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
process_delete_tenant(std::ostream& out, client_session& session,
    std::string tenant_id) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating delete tenant request for: "
                               << tenant_id;

    const auto& session_info = session.session_info();
    if (!session_info) {
        out << "✗ You must be logged in to delete a tenant." << std::endl;
        return;
    }

    boost::uuids::uuid parsed_id;
    try {
        parsed_id = boost::lexical_cast<boost::uuids::uuid>(tenant_id);
    } catch (const boost::bad_lexical_cast&) {
        BOOST_LOG_SEV(lg(), error) << "Invalid tenant ID format: " << tenant_id;
        out << "✗ Invalid tenant ID format. Expected UUID." << std::endl;
        return;
    }

    using iam::messaging::delete_tenant_request;
    using iam::messaging::delete_tenant_response;
    auto result = session.process_authenticated_request<delete_tenant_request,
                                                        delete_tenant_response,
                                                        message_type::delete_tenant_request>
        (delete_tenant_request{.ids = {parsed_id}});

    if (!result) {
        out << "✗ " << comms::net::to_string(result.error()) << std::endl;
        return;
    }

    const auto& response = *result;
    if (response.results.empty()) {
        out << "✗ No results returned from server." << std::endl;
        return;
    }

    const auto& delete_result = response.results[0];
    if (delete_result.success) {
        BOOST_LOG_SEV(lg(), info) << "Successfully deleted tenant: "
                                  << tenant_id;
        out << "✓ Tenant deleted successfully!" << std::endl;
    } else {
        BOOST_LOG_SEV(lg(), warn) << "Failed to delete tenant: "
                                  << delete_result.message;
        out << "✗ Failed to delete tenant: " << delete_result.message << std::endl;
    }
}

}

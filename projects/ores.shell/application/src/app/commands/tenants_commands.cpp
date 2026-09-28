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
#include "ores.iam.api/domain/tenant_table_io.hpp" // IWYU pragma: keep.
#include "ores.iam.api/messaging/tenant_protocol.hpp"
#include "ores.iam.api/messaging/tenant_provisioning_protocol.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cli/cli.h>
#include <functional>
#include <ostream>

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

namespace {

std::string format_time(std::chrono::system_clock::time_point tp) {
    return ores::platform::time::datetime::to_local_display_string(tp);
}

} // anonymous namespace

void tenants_commands::register_commands(cli::Menu& root_menu,
                                         nats_client& session,
                                         pagination_context& /*pagination*/) {
    // The generated tenant unit owns the tenants menu. The two verbs below are
    // provisioning steps the model cannot express, so they join it rather than
    // registering a second tenants menu beside it.
    ores::shell::app::extend_menu(root_menu, "tenants", [&session](cli::Menu& tenants_menu) {
        tenants_menu.Insert(
            "history",
            [&session](std::ostream& out, std::string tenant_id) {
                process_tenant_history(std::ref(out), std::ref(session), std::move(tenant_id));
            },
            "Show history for a tenant (tenant_code)");

        tenants_menu.Insert(
            "complete-provisioning",
            [&session](std::ostream& out) {
                process_complete_provisioning(std::ref(out), std::ref(session));
            },
            "Mark the logged-in tenant's provisioning as complete (clears bootstrap state)");
    });
}

void tenants_commands::process_tenant_history(std::ostream& out,
                                              nats_client& session,
                                              std::string tenant_code) {
    BOOST_LOG_SEV(lg(), debug) << "Initiating tenant history request for: " << tenant_code;

    // A tenant is addressed by its code, which is the key the model declares
    // and the one its subject's operations carry.
    if (tenant_code.empty()) {
        fail(out) << "A tenant code is required." << std::endl;
        return;
    }

    iam::messaging::list_tenant_versions_request req;
    req.key.code = tenant_code;

    auto result = do_auth_request<iam::messaging::list_tenant_versions_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    if (result->result.outcome != ores::utility::domain::outcome::ok) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to get tenant history: " << result->result.message;
        fail(out) << result->result.message << std::endl;
        return;
    }

    const auto& versions = result->versions;
    BOOST_LOG_SEV(lg(), info) << "Successfully retrieved " << versions.size()
                              << " history entries.";

    if (versions.empty()) {
        out << "No history found for tenant: " << tenant_code << std::endl;
        return;
    }

    out << "History for tenant " << tenant_code << " (" << versions.size()
        << " versions):" << std::endl;
    out << std::string(80, '-') << std::endl;

    for (const auto& entry : versions) {
        out << "  Version " << entry.version << std::endl;
        out << "    Code: " << entry.code << std::endl;
        out << "    Name: " << entry.name << std::endl;
        out << "    Type: " << entry.type << std::endl;
        out << "    Hostname: " << entry.hostname << std::endl;
        out << "    Status: " << entry.status << std::endl;
        out << "    Recorded: " << format_time(entry.recorded_at) << " by " << entry.modified_by
            << std::endl;
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


void tenants_commands::process_complete_provisioning(std::ostream& out, nats_client& session) {
    if (!session.is_logged_in()) {
        fail(out) << "Not logged in." << std::endl;
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Completing tenant provisioning.";

    iam::messaging::complete_tenant_provisioning_command req;
    auto result = do_auth_request<iam::messaging::complete_tenant_provisioning_response>(
        out, session, std::string(req.nats_subject), req);
    if (!result)
        return;

    if (!result->success) {
        fail(out) << "Failed to complete tenant provisioning: " << result->message << std::endl;
        return;
    }
    out << "✓ Tenant provisioning completed." << std::endl;
    BOOST_LOG_SEV(lg(), info) << "Tenant provisioning completed.";
}

}

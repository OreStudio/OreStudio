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
#include "ores.shell/app/commands/rbac_commands.hpp"
#include "ores.iam.api/domain/permission_table_io.hpp" // IWYU pragma: keep.
#include "ores.iam.api/domain/role_table_io.hpp"       // IWYU pragma: keep.
#include "ores.iam.api/messaging/authorization_protocol.hpp"
#include "ores.iam.api/messaging/permission_protocol.hpp"
#include "ores.iam.api/messaging/role_protocol.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/request_helpers.hpp"
#include "ores.shell/app/shell_root_menu.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cli/cli.h>
#include <functional>
#include <optional>
#include <ostream>
#include <string_view>

namespace ores::shell::app::commands {

using namespace ores::logging;
using ores::nats::service::nats_client;

void rbac_commands::register_commands(cli::Menu& root_menu,
                                      nats_client& session,
                                      pagination_context& /*pagination*/) {
    // The generated permission unit owns the permissions menu. This unit adds
    // the one verb the model cannot express, so both live at one address and
    // the menu's help lists both.
    ores::shell::app::extend_menu(
        root_menu, "permissions", [&session](cli::Menu& permissions_menu) {
            permissions_menu.Insert(
                "suggest",
                [&session](std::ostream& out, std::string username, std::string identifier) {
                    process_suggest_role_commands(std::ref(out),
                                                  std::ref(session),
                                                  std::move(username),
                                                  std::move(identifier));
                },
                "Generate role assignment commands (username hostname_or_tenant_id)");
        });
}


void rbac_commands::process_suggest_role_commands(std::ostream& out,
                                                  nats_client& session,
                                                  std::string username,
                                                  std::string identifier) {
    BOOST_LOG_SEV(lg(), debug) << "Generating role commands for: " << username << "@" << identifier;

    iam::messaging::suggest_role_commands_request req;
    req.username = username;

    // Check if identifier looks like a UUID
    try {
        boost::lexical_cast<boost::uuids::uuid>(identifier);
        // It's a valid UUID, use as tenant_id
        req.tenant_id = identifier;
    } catch (const boost::bad_lexical_cast&) {
        // Not a UUID, treat as hostname
        req.hostname = identifier;
    }

    auto result = do_auth_request<iam::messaging::suggest_role_commands_response>(
        out, session, iam::messaging::suggest_role_commands_request::nats_subject, req);
    if (!result)
        return;

    BOOST_LOG_SEV(lg(), info) << "Generated " << result->commands.size() << " role commands.";

    for (const auto& command : result->commands) {
        out << command << std::endl;
    }
}

}

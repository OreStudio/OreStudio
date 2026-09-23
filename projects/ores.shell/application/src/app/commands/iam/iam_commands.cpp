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
#include "ores.shell/app/commands/iam/iam_commands.hpp"
#include "ores.shell/app/commands/iam/account_commands.hpp"
#include "ores.shell/app/commands/iam/account_contact_information_commands.hpp"
#include "ores.shell/app/commands/iam/account_operations_operations_commands.hpp"
#include "ores.shell/app/commands/iam/account_party_commands.hpp"
#include "ores.shell/app/commands/iam/account_type_commands.hpp"
#include "ores.shell/app/commands/iam/authorization_operations_commands.hpp"
#include "ores.shell/app/commands/iam/bootstrap_operations_commands.hpp"
#include "ores.shell/app/commands/iam/login_info_commands.hpp"
#include "ores.shell/app/commands/iam/permission_commands.hpp"
#include "ores.shell/app/commands/iam/reset_operations_commands.hpp"
#include "ores.shell/app/commands/iam/role_commands.hpp"
#include "ores.shell/app/commands/iam/session_commands.hpp"
#include "ores.shell/app/commands/iam/session_operations_operations_commands.hpp"
#include "ores.shell/app/commands/iam/session_samples_operations_commands.hpp"
#include "ores.shell/app/commands/iam/signup_operations_commands.hpp"
#include "ores.shell/app/commands/iam/tenant_commands.hpp"
#include "ores.shell/app/commands/iam/tenant_provisioning_operations_commands.hpp"
#include "ores.shell/app/commands/iam/tenant_status_commands.hpp"
#include "ores.shell/app/commands/iam/tenant_type_commands.hpp"

namespace ores::shell::app::commands {

using namespace logging;
using ores::nats::service::nats_client;

void iam_commands::register_commands(cli::Menu& root_menu,
                                     nats_client& session,
                                     pagination_context& /*pagination*/) {
    BOOST_LOG_SEV(lg(), debug) << "Registering IAM command surface.";

    login_info_commands::register_commands(root_menu, session);
    // The declared operations first, so an entity's own verbs are registered
    // last and a name the two share resolves to the entity's.
    // The login operations are absent on purpose: they change this client's
    // own session rather than the store, which a unit that sends a request and
    // prints the reply cannot express. accounts_commands owns them.
    bootstrap_operations_commands::register_commands(root_menu, session);
    signup_operations_commands::register_commands(root_menu, session);
    account_commands::register_commands(root_menu, session);
    account_operations_operations_commands::register_commands(root_menu, session);
    authorization_operations_commands::register_commands(root_menu, session);
    session_operations_operations_commands::register_commands(root_menu, session);
    session_samples_operations_commands::register_commands(root_menu, session);
    session_commands::register_commands(root_menu, session);
    reset_operations_commands::register_commands(root_menu, session);
    tenant_provisioning_operations_commands::register_commands(root_menu, session);
    account_contact_information_commands::register_commands(root_menu, session);
    account_party_commands::register_commands(root_menu, session);
    account_type_commands::register_commands(root_menu, session);
    permission_commands::register_commands(root_menu, session);
    role_commands::register_commands(root_menu, session);
    tenant_commands::register_commands(root_menu, session);
    tenant_status_commands::register_commands(root_menu, session);
    tenant_type_commands::register_commands(root_menu, session);
}

}

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
#ifndef ORES_COMMS_SHELL_APP_COMMANDS_TENANTS_COMMANDS_HPP
#define ORES_COMMS_SHELL_APP_COMMANDS_TENANTS_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.comms/net/client_session.hpp"

namespace cli {

class Menu;

}

namespace ores::comms::shell::app::commands {

/**
 * @brief Manages commands related to tenants.
 *
 * Provides CRUD operations for tenant management in multi-tenant systems.
 */
class tenants_commands {
private:
    inline static std::string_view logger_name =
        "ores.comms.shell.app.commands.tenants_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register tenant-related commands.
     *
     * Creates the tenants submenu and adds tenant operations.
     */
    static void register_commands(cli::Menu& root_menu,
        comms::net::client_session& session);

    /**
     * @brief Process a get tenants request.
     *
     * Retrieves all tenants from the server and displays them.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     */
    static void process_get_tenants(std::ostream& out,
        comms::net::client_session& session);

    /**
     * @brief Process an add tenant request.
     *
     * Creates a new tenant with the provided details.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param code Unique tenant code
     * @param name Human-readable tenant name
     * @param type Tenant type classification
     * @param hostname Unique hostname for tenant routing
     * @param description Optional description
     */
    static void process_add_tenant(std::ostream& out,
        comms::net::client_session& session,
        std::string code, std::string name, std::string type,
        std::string hostname, std::string description);

    /**
     * @brief Process a tenant history request.
     *
     * Retrieves version history for a specific tenant.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param tenant_id The tenant UUID to get history for.
     */
    static void process_tenant_history(std::ostream& out,
        comms::net::client_session& session,
        std::string tenant_id);

    /**
     * @brief Process a delete tenant request.
     *
     * Deletes a tenant by ID.
     *
     * @param out Output stream for results
     * @param session Client session for connectivity.
     * @param tenant_id The tenant UUID to delete.
     */
    static void process_delete_tenant(std::ostream& out,
        comms::net::client_session& session,
        std::string tenant_id);
};

}

#endif

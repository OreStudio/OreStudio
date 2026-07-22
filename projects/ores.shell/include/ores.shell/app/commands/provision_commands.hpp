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
#ifndef ORES_SHELL_APP_COMMANDS_PROVISION_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_PROVISION_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <string>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Porcelain provisioning commands.
 *
 * Each command spans one of the GUI provisioning wizards end-to-end,
 * with the wizard's defaults, per-phase progress and failures that
 * abort a loaded script. The wizard pages define capability coverage,
 * not the surface: a single command with flags replaces each wizard.
 */
class provision_commands {
private:
    inline static std::string_view logger_name = "ores.shell.app.commands.provision_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register provisioning commands.
     *
     * Creates the provision submenu.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief The system provisioner flow: provision system <username>
     * <password> <email> --tenant-admin-password <pw> [--tenant-code
     * <c>] [--tenant-name <n>] [--tenant-type <t>] [--tenant-hostname
     * <h>] [--tenant-description <d>] [--tenant-admin <user>]
     * [--tenant-admin-email <email>].
     *
     * Requires bootstrap mode and no login. Creates the initial admin,
     * logs in as it, and provisions the first tenant with its admin
     * account. Defaults reproduce the wizard's single-tenant mode;
     * the session is left logged in as the system admin.
     */
    static void process_system(std::ostream& out,
                               ores::nats::service::nats_client& session,
                               const std::vector<std::string>& args);

    /**
     * @brief The tenant provisioner flow: provision tenant
     * [--bundle <code>] [--source gleif|synthetic] [--root-lei <lei>]
     * [--timeout <s>] [synthetic generation knobs].
     *
     * Run logged in as the tenant admin of a bootstrap-mode tenant.
     * Publishes the selected bundle (default: first available) and
     * waits on its workflow; generates the synthetic organisation
     * when --source synthetic; associates the admin with all
     * Operational parties (non-fatal, as the wizard); clears the
     * bootstrap flag and completes provisioning. Requires logout and
     * re-login afterwards, exactly as the wizard instructs.
     */
    static void process_tenant(std::ostream& out,
                               ores::nats::service::nats_client& session,
                               const std::vector<std::string>& args);

    /**
     * @brief The party provisioner flow: provision party <party>
     * [--dataset-size small|large] [--timeout <s>].
     *
     * <party> is a UUID or exact full name (always explicit; no
     * session party state). Publishes the counterparty dataset, then
     * every party-scoped bundle in party_provisioning_bundle_plan()
     * (risk_management, synthetic_realistic, the curated FX
     * driver-rate bundle) in full, and activates the party. Activation
     * failure is a hard failure — deliberate divergence from the
     * wizard.
     */
    static void process_party(std::ostream& out,
                              ores::nats::service::nats_client& session,
                              const std::vector<std::string>& args);
};

}

#endif

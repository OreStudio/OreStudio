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
#include <optional>
#include <ostream>
#include <string>
#include <string_view>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Porcelain provisioning commands.
 *
 * Thin callers of the same subjects the browser's setup journeys call:
 * =iam.v1.ops.bootstrap_status=, =iam.v1.ops.create_initial_admin=,
 * =iam.v1.ops.provision_tenant= and =iam.v1.ops.provision_party=, each followed
 * through the same progress read the journey's rail renders. A command holds
 * no sequence of its own beyond the order the two clients share, and the
 * starting point it runs from is a flag naming a seeded row rather than a
 * profile this code knows anything about.
 *
 * The commands the wizards' orchestration used to live in are kept as the
 * evidence of what those wizards did only in the recipes that run them; the
 * orchestration itself is the server's.
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
    /// The tenant a provisioning command names, and its first administrator.
    struct tenant_fields {
        std::string code;
        std::string name;
        std::string hostname;
        std::string description;
        std::string admin_username;
        std::string admin_email;
        std::string admin_password;
    };

    /// What a command created, which decides how its ending is worded.
    enum class tenant_creation {
        /// bootstrap setup: the installation and its first tenant.
        system,
        /// provision tenant: one tenant in addition to those that exist.
        additional
    };

    /// How the work behind a tenant's creation ended.
    enum class tenant_setup {
        /// Every run that does the work finished.
        finished,
        /// A run this session cannot read continues the work elsewhere.
        continues_elsewhere
    };

    /**
     * @brief Register provisioning commands.
     *
     * Creates the provision submenu.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief The run a provisioning reply says was started, or nothing.
     *
     * A reply that reports failure names no run to wait on, and neither does
     * one that succeeded without naming an instance: waiting on either would
     * block on a run that does not exist.
     */
    static std::optional<std::string> started_run(bool request_succeeded,
                                                  std::string_view instance_id);

    /**
     * @brief Prints how a tenant's creation ended, and where the tenant stands.
     *
     * A setup this session cannot follow leaves the tenant's own work
     * continuing in another tenant, so the tenant is not ready and the
     * sign-in hint says so rather than claiming work that has not happened.
     */
    static void report_tenant_outcome(std::ostream& out,
                                      const tenant_fields& fields,
                                      tenant_creation created,
                                      tenant_setup setup);

    /**
     * @brief Set an empty installation up: bootstrap setup <username>
     * <password> <email> --tenant-admin-password <pw> [--profile <code>]
     * [--param <name=value>] [--tenant-code <c>] [--tenant-name <n>]
     * [--tenant-hostname <h>] [--tenant-description <d>] [--tenant-admin
     * <user>] [--tenant-admin-email <email>] [--timeout <seconds>].
     *
     * Registered in the bootstrap menu, beside the generated verbs that
     * answer the installation and give it an administrator, because this is
     * the act that brings the installation to life. Requires bootstrap mode
     * and no login. Reads the deployment's own bootstrap answer, creates the
     * initial administrator, signs in as it, and provisions the first tenant
     * with one request, whose run the command then follows. The session is
     * left signed in as the system administrator.
     */
    static void process_setup(std::ostream& out,
                              ores::nats::service::nats_client& session,
                              const std::vector<std::string>& args);

    /**
     * @brief Provision a tenant: provision tenant
     * --tenant-admin-password <pw> [--tenant-code <c>] [--tenant-name <n>]
     * [--tenant-hostname <h>] [--tenant-description <d>] [--tenant-admin
     * <user>] [--tenant-admin-email <email>] [--profile <code>]
     * [--param <name=value>] [--timeout <seconds>].
     *
     * Run signed in as an administrator. Every tenant after the first is
     * created the same way the first one is, so this is the same request
     * =provision system= sends once the installation has an administrator.
     * The command follows the run it starts to its end.
     */
    static void process_tenant(std::ostream& out,
                               ores::nats::service::nats_client& session,
                               const std::vector<std::string>& args);

    /**
     * @brief Provision a party: provision party <party-uuid-or-full-name>
     * [--profile <code>] [--timeout <seconds>].
     *
     * <party> is a UUID or an exact full name, resolved by the server's party
     * read so a person who knows the name and a script that read the
     * identifier reach the same party. The starting point named states the
     * bundles the party's data is published from, and the run the command
     * follows publishes them, activates the party, marks its onboarding
     * complete and joins the signed-in administrator to it.
     */
    static void process_party(std::ostream& out,
                              ores::nats::service::nats_client& session,
                              const std::vector<std::string>& args);
};

}

#endif

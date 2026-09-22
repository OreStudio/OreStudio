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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_shell_command_header.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SHELL_APP_COMMANDS_CALENDAR_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_CALENDAR_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <ostream>
#include <string>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Every verb calendars answer, as one command each.
 *
 * The unit is the entity's own derivation addressed from the REPL, so a verb
 * the model gains appears as a command without an edit here and a verb it
 * loses takes its command with it. What each command asks for follows from its
 * shape: a paged read is addressed by nothing, a write by its write record,
 * and everything else by the key.
 */
class calendar_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.refdata.calendar_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the calendars submenu.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief list [--offset <n>] [--limit <n>] [--order <field>] [--desc]
     */
    static void process_list(std::ostream& out,
                             ores::nats::service::nats_client& session,
                             const std::vector<std::string>& args);

    /**
     * @brief get <code>
     */
    static void process_get(std::ostream& out,
                            ores::nats::service::nats_client& session,
                            const std::vector<std::string>& args);

    /**
     * @brief get-many <code>
     */
    static void process_get_many(std::ostream& out,
                                 ores::nats::service::nats_client& session,
                                 const std::vector<std::string>& args);

    /**
     * @brief add <code> <name> <calendar_type> <country_code> <image_id> <source> <is_editable>
     * <base_calendar_code> <reason> <commentary>
     */
    static void process_add(std::ostream& out,
                            ores::nats::service::nats_client& session,
                            const std::vector<std::string>& args);

    /**
     * @brief set <code> <name> <calendar_type> <country_code> <image_id> <source> <is_editable>
     * <base_calendar_code> <reason> <commentary> [--version <n>]
     */
    static void process_set(std::ostream& out,
                            ores::nats::service::nats_client& session,
                            const std::vector<std::string>& args);

    /**
     * @brief put-many --count <n> <code> <name> <calendar_type> <country_code> <image_id> <source>
     * <is_editable> <base_calendar_code> <reason> <commentary>
     */
    static void process_put_many(std::ostream& out,
                                 ores::nats::service::nats_client& session,
                                 const std::vector<std::string>& args);

    /**
     * @brief delete <code> <reason> <commentary> [--version <n>]
     */
    static void process_delete(std::ostream& out,
                               ores::nats::service::nats_client& session,
                               const std::vector<std::string>& args);

    /**
     * @brief delete-many <code> <reason> <commentary>
     */
    static void process_delete_many(std::ostream& out,
                                    ores::nats::service::nats_client& session,
                                    const std::vector<std::string>& args);

    /**
     * @brief versions <code> [--offset <n>] [--limit <n>] [--order <field>] [--desc]
     */
    static void process_versions(std::ostream& out,
                                 ores::nats::service::nats_client& session,
                                 const std::vector<std::string>& args);

    /**
     * @brief version <code> --version <n>
     */
    static void process_version(std::ostream& out,
                                ores::nats::service::nats_client& session,
                                const std::vector<std::string>& args);
};

}

#endif

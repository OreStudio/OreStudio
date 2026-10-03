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
#ifndef ORES_SHELL_APP_COMMANDS_COMMODITY_CURVE_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_COMMODITY_CURVE_COMMANDS_HPP

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
 * @brief Every verb commodity curves answer, as one command each.
 *
 * The unit is the entity's own derivation addressed from the REPL, so a verb
 * the model gains appears as a command without an edit here and a verb it
 * loses takes its command with it. What each command asks for follows from its
 * shape: a paged read is addressed by nothing, a write by its write record,
 * and everything else by the key.
 */
class commodity_curve_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.refdata.commodity_curve_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the commodity curves submenu.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief list [--offset <n>] [--limit <n>] [--order <field>] [--desc]
     */
    static void process_list(std::ostream& out,
                             ores::nats::service::nats_client& session,
                             const std::vector<std::string>& args);

    /**
     * @brief get <id>
     */
    static void process_get(std::ostream& out,
                            ores::nats::service::nats_client& session,
                            const std::vector<std::string>& args);

    /**
     * @brief get-many <id>
     */
    static void process_get_many(std::ostream& out,
                                 ores::nats::service::nats_client& session,
                                 const std::vector<std::string>& args);

    /**
     * @brief add <curve_definition_id> <currency> <base_price_curve> <base_yield_curve>
     * <yield_curve> <spot_quote> <has_quotes> <day_counter> <interpolation_method> <conventions>
     * <extrapolation> <has_basis_configuration> <basis_base_price_curve>
     * <basis_base_price_conventions> <basis_conventions> <basis_day_counter>
     * <basis_interpolation_method> <basis_add_basis> <basis_month_offset> <basis_average_base>
     * <basis_price_as_historical_fixing> <has_price_segments> <reason> <commentary>
     */
    static void process_add(std::ostream& out,
                            ores::nats::service::nats_client& session,
                            const std::vector<std::string>& args);

    /**
     * @brief set <id> <curve_definition_id> <currency> <base_price_curve> <base_yield_curve>
     * <yield_curve> <spot_quote> <has_quotes> <day_counter> <interpolation_method> <conventions>
     * <extrapolation> <has_basis_configuration> <basis_base_price_curve>
     * <basis_base_price_conventions> <basis_conventions> <basis_day_counter>
     * <basis_interpolation_method> <basis_add_basis> <basis_month_offset> <basis_average_base>
     * <basis_price_as_historical_fixing> <has_price_segments> <reason> <commentary> [--version <n>]
     */
    static void process_set(std::ostream& out,
                            ores::nats::service::nats_client& session,
                            const std::vector<std::string>& args);

    /**
     * @brief put-many --count <n> <id> <curve_definition_id> <currency> <base_price_curve>
     * <base_yield_curve> <yield_curve> <spot_quote> <has_quotes> <day_counter>
     * <interpolation_method> <conventions> <extrapolation> <has_basis_configuration>
     * <basis_base_price_curve> <basis_base_price_conventions> <basis_conventions>
     * <basis_day_counter> <basis_interpolation_method> <basis_add_basis> <basis_month_offset>
     * <basis_average_base> <basis_price_as_historical_fixing> <has_price_segments> <reason>
     * <commentary>
     */
    static void process_put_many(std::ostream& out,
                                 ores::nats::service::nats_client& session,
                                 const std::vector<std::string>& args);

    /**
     * @brief delete <id> <reason> <commentary> [--version <n>]
     */
    static void process_delete(std::ostream& out,
                               ores::nats::service::nats_client& session,
                               const std::vector<std::string>& args);

    /**
     * @brief delete-many <id> <reason> <commentary>
     */
    static void process_delete_many(std::ostream& out,
                                    ores::nats::service::nats_client& session,
                                    const std::vector<std::string>& args);

    /**
     * @brief versions <id> [--offset <n>] [--limit <n>] [--order <field>] [--desc]
     */
    static void process_versions(std::ostream& out,
                                 ores::nats::service::nats_client& session,
                                 const std::vector<std::string>& args);

    /**
     * @brief version <id> --version <n>
     */
    static void process_version(std::ostream& out,
                                ores::nats::service::nats_client& session,
                                const std::vector<std::string>& args);
};

}

#endif

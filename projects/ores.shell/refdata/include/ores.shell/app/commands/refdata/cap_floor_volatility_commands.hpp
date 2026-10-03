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
#ifndef ORES_SHELL_APP_COMMANDS_CAP_FLOOR_VOLATILITY_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_CAP_FLOOR_VOLATILITY_COMMANDS_HPP

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
 * @brief Every verb cap floor volatilities answer, as one command each.
 *
 * The unit is the entity's own derivation addressed from the REPL, so a verb
 * the model gains appears as a command without an edit here and a verb it
 * loses takes its command with it. What each command asks for follows from its
 * shape: a paged read is addressed by nothing, a write by its write record,
 * and everything else by the key.
 */
class cap_floor_volatility_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.refdata.cap_floor_volatility_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the cap floor volatilities submenu.
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
     * @brief add <curve_definition_id> <volatility_type> <output_volatility_type> <model_shift>
     * <output_shift> <extrapolation> <interpolation_method> <include_atm> <day_counter> <calendar>
     * <business_day_convention> <tenors> <strikes> <optional_quotes> <ibor_index> <index>
     * <rate_computation_period> <on_cap_settlement_days> <discount_curve> <atm_tenors>
     * <settlement_days> <interpolate_on> <time_interpolation> <strike_interpolation> <input_type>
     * <quote_includes_index_name> <flat_first_period> <use_effecive_volatility>
     * <use_effective_volatility> <has_proxy_config> <proxy_source_curve_id> <proxy_source_index>
     * <proxy_source_rate_computation_period> <proxy_target_index>
     * <proxy_target_rate_computation_period> <proxy_target_on_cap_settlement_days>
     * <proxy_scaling_factor> <reason> <commentary>
     */
    static void process_add(std::ostream& out,
                            ores::nats::service::nats_client& session,
                            const std::vector<std::string>& args);

    /**
     * @brief set <id> <curve_definition_id> <volatility_type> <output_volatility_type>
     * <model_shift> <output_shift> <extrapolation> <interpolation_method> <include_atm>
     * <day_counter> <calendar> <business_day_convention> <tenors> <strikes> <optional_quotes>
     * <ibor_index> <index> <rate_computation_period> <on_cap_settlement_days> <discount_curve>
     * <atm_tenors> <settlement_days> <interpolate_on> <time_interpolation> <strike_interpolation>
     * <input_type> <quote_includes_index_name> <flat_first_period> <use_effecive_volatility>
     * <use_effective_volatility> <has_proxy_config> <proxy_source_curve_id> <proxy_source_index>
     * <proxy_source_rate_computation_period> <proxy_target_index>
     * <proxy_target_rate_computation_period> <proxy_target_on_cap_settlement_days>
     * <proxy_scaling_factor> <reason> <commentary> [--version <n>]
     */
    static void process_set(std::ostream& out,
                            ores::nats::service::nats_client& session,
                            const std::vector<std::string>& args);

    /**
     * @brief put-many --count <n> <id> <curve_definition_id> <volatility_type>
     * <output_volatility_type> <model_shift> <output_shift> <extrapolation> <interpolation_method>
     * <include_atm> <day_counter> <calendar> <business_day_convention> <tenors> <strikes>
     * <optional_quotes> <ibor_index> <index> <rate_computation_period> <on_cap_settlement_days>
     * <discount_curve> <atm_tenors> <settlement_days> <interpolate_on> <time_interpolation>
     * <strike_interpolation> <input_type> <quote_includes_index_name> <flat_first_period>
     * <use_effecive_volatility> <use_effective_volatility> <has_proxy_config>
     * <proxy_source_curve_id> <proxy_source_index> <proxy_source_rate_computation_period>
     * <proxy_target_index> <proxy_target_rate_computation_period>
     * <proxy_target_on_cap_settlement_days> <proxy_scaling_factor> <reason> <commentary>
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

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
 * Template: cpp_shell_operation_header.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SHELL_APP_COMMANDS_REPORT_OPERATIONS_OPERATIONS_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_REPORT_OPERATIONS_OPERATIONS_COMMANDS_HPP

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
 * @brief The operations report_operations declares that no entity's CRUD verbs state.
 *
 * One command per message the protocol declares with a subject and a response,
 * so the REPL surface and the protocol stay one declaration.
 */
class report_operations_operations_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.reporting.report_operations_operations_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register the report_operations operations on the root menu.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief trigger-report-instance <report_definition_id> <tenant_id> [--job_instance_id <v>]
     */
    static void process_trigger_report_instance(std::ostream& out,
                                                ores::nats::service::nats_client& session,
                                                const std::vector<std::string>& args);

    /**
     * @brief schedule-report-definitions <ids>
     */
    static void process_schedule_report_definitions(std::ostream& out,
                                                    ores::nats::service::nats_client& session,
                                                    const std::vector<std::string>& args);

    /**
     * @brief unschedule-report-definitions <ids>
     */
    static void process_unschedule_report_definitions(std::ostream& out,
                                                      ores::nats::service::nats_client& session,
                                                      const std::vector<std::string>& args);

    /**
     * @brief gather-trades <report_instance_id> <definition_id> <tenant_id> <correlation_id>
     */
    static void process_gather_trades(std::ostream& out,
                                      ores::nats::service::nats_client& session,
                                      const std::vector<std::string>& args);

    /**
     * @brief gather-market-data <report_instance_id> <definition_id> <tenant_id> <correlation_id>
     */
    static void process_gather_market_data(std::ostream& out,
                                           ores::nats::service::nats_client& session,
                                           const std::vector<std::string>& args);

    /**
     * @brief assemble-bundle <report_instance_id> <definition_id> <tenant_id> <correlation_id>
     * <trades_storage_key> <market_data_storage_key> [--trade_count <v>] [--series_count <v>]
     */
    static void process_assemble_bundle(std::ostream& out,
                                        ores::nats::service::nats_client& session,
                                        const std::vector<std::string>& args);

    /**
     * @brief prepare-ore-package <report_instance_id> <bundle_id> <tenant_id> <correlation_id>
     * <trades_storage_key> <market_data_storage_key>
     */
    static void process_prepare_ore_package(std::ostream& out,
                                            ores::nats::service::nats_client& session,
                                            const std::vector<std::string>& args);

    /**
     * @brief submit-compute <report_instance_id> <tenant_id> <correlation_id> <tarball_uris>
     */
    static void process_submit_compute(std::ostream& out,
                                       ores::nats::service::nats_client& session,
                                       const std::vector<std::string>& args);

    /**
     * @brief collect-compute-results <report_instance_id> <tenant_id> <correlation_id> <batch_id>
     */
    static void process_collect_compute_results(std::ostream& out,
                                                ores::nats::service::nats_client& session,
                                                const std::vector<std::string>& args);

    /**
     * @brief finalise-report <report_instance_id> <tenant_id> <correlation_id>
     */
    static void process_finalise_report(std::ostream& out,
                                        ores::nats::service::nats_client& session,
                                        const std::vector<std::string>& args);

    /**
     * @brief fail-report <report_instance_id> <tenant_id> <correlation_id> <error_message>
     */
    static void process_fail_report(std::ostream& out,
                                    ores::nats::service::nats_client& session,
                                    const std::vector<std::string>& args);

    /**
     * @brief resolve-prepared-input <report_instance_id> <tenant_id> <correlation_id>
     * <prepared_input_key>
     */
    static void process_resolve_prepared_input(std::ostream& out,
                                               ores::nats::service::nats_client& session,
                                               const std::vector<std::string>& args);

    /**
     * @brief ignore-compute-results <report_instance_id> <tenant_id> <correlation_id> <batch_id>
     */
    static void process_ignore_compute_results(std::ostream& out,
                                               ores::nats::service::nats_client& session,
                                               const std::vector<std::string>& args);
};

}

#endif

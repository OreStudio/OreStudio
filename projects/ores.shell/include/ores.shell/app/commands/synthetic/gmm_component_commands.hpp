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
#ifndef ORES_SHELL_APP_COMMANDS_SYNTHETIC_GMM_COMPONENT_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_SYNTHETIC_GMM_COMPONENT_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.shell/app/pagination_context.hpp"
#include <string>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Manages commands related to GMM COMPONENT.
 */
class gmm_component_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.synthetic.gmm_component_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register Gmm Component related commands.
     */
    static void register_commands(cli::Menu& root_menu,
                                  ores::nats::service::nats_client& session,
                                  pagination_context& pagination);

    /**
     * @brief Process a get gmm_components request.
     */
    static void process_get_gmm_components(std::ostream& out,
                                           ores::nats::service::nats_client& session,
                                           pagination_context& pagination);

    /**
     * @brief Process an add gmm_component request.
     */
    static void process_add_gmm_component(std::ostream& out,
                                          ores::nats::service::nats_client& session,
                                          std::string s_fx_spot_config_id,
                                          std::string s_component_index,
                                          std::string s_description,
                                          std::string s_mean,
                                          std::string s_stdev,
                                          std::string s_weight,
                                          std::string change_reason_code,
                                          std::string change_commentary);

    /**
     * @brief Process a delete gmm_component request.
     */
    static void process_delete_gmm_component(std::ostream& out,
                                             ores::nats::service::nats_client& session,
                                             std::string id);

    /**
     * @brief Process a get gmm_component history request.
     */
    static void process_get_gmm_component_history(std::ostream& out,
                                                  ores::nats::service::nats_client& session,
                                                  const std::vector<std::string>& args);
};

} // namespace ores::shell::app::commands

#endif

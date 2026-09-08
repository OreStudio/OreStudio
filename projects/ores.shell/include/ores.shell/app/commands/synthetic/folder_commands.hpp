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
#ifndef ORES_SHELL_APP_COMMANDS_SYNTHETIC_FOLDER_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_SYNTHETIC_FOLDER_COMMANDS_HPP

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
 * @brief Manages commands related to FOLDER.
 */
class folder_commands {
private:
    inline static std::string_view logger_name =
        "ores.shell.app.commands.synthetic.folder_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register Folder related commands.
     */
    static void register_commands(cli::Menu& root_menu,
                                  ores::nats::service::nats_client& session,
                                  pagination_context& pagination);

    /**
     * @brief Process a get folders request.
     */
    static void process_get_folders(std::ostream& out,
                                          ores::nats::service::nats_client& session,
                                          pagination_context& pagination);

    /**
     * @brief Process an add folder request.
     */
    static void process_add_folder(std::ostream& out,
                                   ores::nats::service::nats_client& session,
                                      std::string s_name,
                                      std::string s_kind,
                                      std::string s_parent_id,
                                      std::string s_collection_id,
                                      std::string change_reason_code,
                                      std::string change_commentary);

    /**
     * @brief Process a delete folder request.
     */
    static void process_delete_folder(std::ostream& out,
                                      ores::nats::service::nats_client& session,
                                      std::string id);

    /**
     * @brief Process a get folder history request.
     */
    static void process_get_folder_history(std::ostream& out,
                                           ores::nats::service::nats_client& session,
                                           const std::vector<std::string>& args);
};

} // namespace ores::shell::app::commands

#endif

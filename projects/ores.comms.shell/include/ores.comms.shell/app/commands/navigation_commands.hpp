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
#ifndef ORES_COMMS_SHELL_APP_COMMANDS_NAVIGATION_COMMANDS_HPP
#define ORES_COMMS_SHELL_APP_COMMANDS_NAVIGATION_COMMANDS_HPP

#include <iosfwd>
#include "ores.logging/make_logger.hpp"
#include "ores.comms.shell/app/pagination_context.hpp"

namespace cli {

class Menu;

}

namespace ores::comms::shell::app::commands {

/**
 * @brief Manages pagination navigation commands.
 *
 * Provides commands for navigating through paginated result sets:
 * - next: Move to next page
 * - prev: Move to previous page
 * - first: Jump to first page
 * - last: Jump to last page
 * - page-size: Get or set page size
 */
class navigation_commands {
private:
    inline static std::string_view logger_name =
        "ores.comms.shell.app.commands.navigation_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Register navigation commands.
     *
     * Adds pagination navigation commands to the root menu.
     *
     * @param root_menu The root menu to add commands to.
     * @param pagination The pagination context for state management.
     */
    static void register_commands(cli::Menu& root_menu,
                                  pagination_context& pagination);

    /**
     * @brief Move to the next page.
     *
     * Advances the offset by page_size and re-invokes the list command.
     *
     * @param out Output stream for results.
     * @param pagination The pagination context.
     */
    static void process_next(std::ostream& out, pagination_context& pagination);

    /**
     * @brief Move to the previous page.
     *
     * Decrements the offset by page_size and re-invokes the list command.
     *
     * @param out Output stream for results.
     * @param pagination The pagination context.
     */
    static void process_prev(std::ostream& out, pagination_context& pagination);

    /**
     * @brief Jump to the first page.
     *
     * Sets offset to 0 and re-invokes the list command.
     *
     * @param out Output stream for results.
     * @param pagination The pagination context.
     */
    static void process_first(std::ostream& out, pagination_context& pagination);

    /**
     * @brief Jump to the last page.
     *
     * Sets offset to show the last page and re-invokes the list command.
     *
     * @param out Output stream for results.
     * @param pagination The pagination context.
     */
    static void process_last(std::ostream& out, pagination_context& pagination);

    /**
     * @brief Get or set the page size.
     *
     * Without argument, displays the current page size.
     * With argument, sets the page size and resets all entity offsets.
     *
     * @param out Output stream for results.
     * @param pagination The pagination context.
     * @param size Optional new page size (0 means just show current).
     */
    static void process_page_size(std::ostream& out,
                                  pagination_context& pagination,
                                  std::uint32_t size = 0);
};

}

#endif

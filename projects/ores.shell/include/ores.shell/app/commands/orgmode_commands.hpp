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
#ifndef ORES_SHELL_APP_COMMANDS_ORGMODE_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_ORGMODE_COMMANDS_HPP

#include "ores.logging/make_logger.hpp"
#include <iosfwd>
#include <string>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief `doc-show`: a minimal, NATS-free demonstration of
 * `ores.orgmode` from the shell — parse a `.org` file, print its
 * frontmatter/heading summary, and resolve its first outgoing `id:`
 * link (if any) against the repo's org-roam index.
 *
 * Proof that the component works end-to-end outside Qt; not meant to
 * be the final shape of any future doc-browsing UX.
 */
class orgmode_commands {
private:
    inline static std::string_view logger_name = "ores.shell.app.commands.orgmode_commands";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    static void register_commands(cli::Menu& root_menu);

    /**
     * @brief Parse @p path and print a summary, resolving its first
     * outgoing link (depth-first) against the org-roam index found by
     * walking up from the current directory.
     */
    static void process_doc_show(std::ostream& out, const std::string& path);
};

}

#endif

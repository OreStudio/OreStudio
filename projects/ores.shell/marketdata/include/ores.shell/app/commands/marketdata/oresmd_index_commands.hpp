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
#ifndef ORES_SHELL_APP_COMMANDS_MARKETDATA_ORESMD_INDEX_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_MARKETDATA_ORESMD_INDEX_COMMANDS_HPP

#include <ostream>
#include <string>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief Turns ORE index names into oresmd fixing URIs, offline.
 *
 * The two codecs that own the translation live in
 * =ores.marketdata.core= and are otherwise reachable only from C++. This
 * command is the one command-line entry point to them, so the convention
 * extractor can hold a fixed =ore_name<TAB>oresmd_uri= table instead of a
 * Python binding.
 *
 * It is pure: no NATS connection, no session and no database. Its only job
 * is the codec.
 *
 *   marketdata oresmd-index [--in <path>] [--out <path>]
 *
 * One name per line is read from @c --in, or from stdin when @c --in is
 * absent or @c -. Blank lines and lines starting with @c # are skipped.
 * Every name must resolve; a name the codec rejects is named with its line
 * number, the command fails, and no output is written. An empty list
 * resolves to nothing and still succeeds.
 */
class oresmd_index_commands {
public:
    /**
     * @brief Register the =oresmd-index= verb on the marketdata menu.
     *
     * The verb takes no session: it answers from the codec alone, so it
     * works before a login and with no services up.
     */
    static void register_verb(cli::Menu& marketdata_menu);

    /**
     * @brief Run the verb: read ORE index names, write their oresmd URIs.
     */
    static void process(std::ostream& out, const std::vector<std::string>& args);
};

}

#endif

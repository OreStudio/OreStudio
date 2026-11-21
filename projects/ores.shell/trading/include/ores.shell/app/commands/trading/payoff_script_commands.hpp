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
#ifndef ORES_SHELL_APP_COMMANDS_TRADING_PAYOFF_SCRIPT_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_TRADING_PAYOFF_SCRIPT_COMMANDS_HPP

#include <ostream>
#include <string>
#include <vector>

namespace cli {

class Menu;

}

namespace ores::nats::service {

class nats_client;

}

namespace ores::shell::app::commands {

/**
 * @brief The script commands that read a payoff script's text alone.
 *
 * They link ORE Studio's copy of ORE's parser and need no session, no server
 * and no database, so they answer before a login and with no service up:
 *
 *   script parse <file>
 *   script check <file> [--variables <file>]
 *   script inputs <file> [<file>...] [--out <path>]
 *   script print <file>
 *
 * A command that finds an error marks the failure, so a batch run of the
 * shell exits with a non-zero status.
 */
class payoff_script_commands {
public:
    /**
     * @brief Register the =script= menu on the root menu.
     *
     * The session is the registrar's common argument; these commands do not
     * use it.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);

    /**
     * @brief Parse a script and report whether it parses, with the syntax
     * error's line and column when it does not.
     */
    static void process_parse(std::ostream& out, const std::vector<std::string>& args);

    /**
     * @brief Print every diagnostic of a script as
     * =FILE:LINE:COLUMN: severity: message [code]=.
     *
     * Without =--variables=, the findings of the script alone: its syntax and
     * its unused declarations. With a variables file, the findings against
     * those variables as well. The variables file holds one variable per line,
     * =name<TAB>type<TAB>values=, the values separated by commas and a list's
     * type ending in =[]=; blank lines and lines starting with =#= are skipped.
     */
    static void process_check(std::ostream& out, const std::vector<std::string>& args);

    /**
     * @brief List the inputs of a script, or of the merge of its variants when
     * several files are given, one per line as
     * =name<TAB>type<TAB>is_array<TAB>rule=.
     *
     * =is_array= is =1= or =0= and the rule is empty where there is none. A
     * backslash, tab, carriage return or line feed in a field is written as
     * =\\\\=, =\\t=, =\\r= or =\\n=, so that every input is one line.
     */
    static void process_inputs(std::ostream& out, const std::vector<std::string>& args);

    /**
     * @brief Print a script in the normal form ORE's converter writes.
     */
    static void process_print(std::ostream& out, const std::vector<std::string>& args);
};

}

#endif

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
#ifndef ORES_SHELL_APP_COMMANDS_STORAGE_RAW_STORAGE_COMMANDS_HPP
#define ORES_SHELL_APP_COMMANDS_STORAGE_RAW_STORAGE_COMMANDS_HPP

#include "ores.nats/service/nats_client.hpp"

namespace cli {

class Menu;

}

namespace ores::shell::app::commands {

/**
 * @brief The raw storage surface: a bucket, a key and a local path.
 *
 * The generated =objects= menu speaks the NATS protocol, so the value of an
 * object travels inside a message and is bounded by the message size. This unit
 * speaks the HTTP interface instead, so a caller that only wants bytes
 * somewhere moves a file and never learns a domain verb.
 *
 *   storage put    <bucket> <key> <local-path>
 *   storage get    <bucket> <key> <local-path>
 *   storage delete <bucket> <key>
 *   storage list   <bucket> [--prefix <v>] [--offset <v>] [--limit <v>]
 */
class raw_storage_commands {
public:
    /**
     * @brief Register the =storage= submenu on the root menu.
     */
    static void register_commands(cli::Menu& root_menu, ores::nats::service::nats_client& session);
};

}

#endif

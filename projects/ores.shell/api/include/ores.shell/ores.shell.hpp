/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. Seethe GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General PublicLicense along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_SHELL_ORES_SHELL_HPP
#define ORES_SHELL_ORES_SHELL_HPP

/**
 * @brief Interactive REPL (Read-Eval-Print Loop) or shell for connecting to the
 * ORE Studio server.
 *
 *  Features include:
 *
 * - Interactive command-line interface with command completion
 * - Connection management: connect, disconnect to ORE Studio server
 * - Currency operations: list and retrieve currency data
 * - Account operations: create accounts, login, list accounts, unlock accounts
 * - Auto-connect and auto-login from configuration file
 * - Boost program_options for CLI argument parsing
 *
 * The module is organised into namespaces: config (configuration parsing),
 * and app (REPL and application hosting).
 */
namespace ores::shell {}

#endif

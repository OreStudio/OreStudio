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
#ifndef ORES_SHELL_ORES_SHELL_HPP
#define ORES_SHELL_ORES_SHELL_HPP

/**
 * @brief The interactive shell: one command tree over every component's
 * service, driven from a terminal or from a scripted session.
 *
 * The shell is a client, not a service. It owns no subject and stores nothing.
 * It renders each component's generated command units into one REPL, adds the
 * hand-written verbs a component's model cannot express, and sends the
 * canonical NATS requests the services answer.
 *
 * shell_root_menu owns every name in the command tree. It claims a name once and
 * refuses a second claim, because cli::Menu answers a duplicated name by first
 * match and a silent winner hides the other unit's verbs from the help.
 * insert_menu is the seam a unit inserts through, and extend_menu is how a
 * hand-written unit contributes verbs to a menu a generated unit owns.
 *
 * The component is split into parts. The application part holds the REPL, the
 * host and the argument and feedback helpers; the api part holds what the
 * application and the per-component parts share; and one part per component
 * holds that component's shell units, generated from its models and rendered
 * here.
 */
namespace ores::shell {}

#endif

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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_COMMS_ANALYSER_HPP
#define ORES_COMMS_ANALYSER_HPP

/**
 * @brief Communication session analyzer tool for ORE Studio.
 *
 * This component provides a command-line tool for analyzing recorded
 * communication sessions from the ores.comms library. Key features include:
 *
 * - Session reading: parse and decode recorded binary protocol sessions
 * - Message inspection: detailed view of frame headers and payloads
 * - Debugging support: aids in troubleshooting protocol issues
 *
 * The module is organized into namespaces: app (application hosting),
 * config (command-line parsing), and domain (session reading logic).
 */
namespace ores::comms::analyser {}

#endif

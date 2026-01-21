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
#ifndef ORES_CONNECTIONS_HPP
#define ORES_CONNECTIONS_HPP

/**
 * @brief Manages server connection bookmarks with hierarchical organization.
 *
 * This component provides a client-side connection manager for storing and
 * organizing server connection details. Key features include:
 *
 * - Hierarchical folder organization for connections
 * - Tag-based categorization for flexible grouping
 * - Encrypted credential storage using AES with master password
 * - SQLite-based local persistence
 *
 * The component is designed to be consumed by multiple clients including
 * the Qt UI, shell commands, and HTTP API.
 */
namespace ores::connections {}

#endif

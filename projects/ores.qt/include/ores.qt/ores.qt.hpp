/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_ORES_QT_HPP
#define ORES_QT_ORES_QT_HPP

/**
 * @brief Qt-based graphical user interface for ORE Studio.
 *
 * Modern desktop application built with Qt 6 providing visual management of
 * ORE data. Key features:
 *
 * - MDI interface: Multiple Document Interface with detachable windows
 * - Currency management: List, create, edit, delete currencies with history tracking
 * - Authentication: Login dialog with secure password handling
 * - Real-time updates: Asynchronous data loading and server synchronization
 * - Entity controllers: Modular controller pattern for managing entity windows
 * - Custom widgets: Specialized delegates, dialogs, and UI components
 * - Background logo: Customizable MDI area background
 * - Connection status: Visual indication of server connectivity
 *
 * The application uses an entity controller pattern where MainWindow delegates
 * entity-specific operations (e.g., CurrencyController) which manage their own
 * MDI windows, dialogs, and data models.
 */
namespace ores::qt { }

#endif

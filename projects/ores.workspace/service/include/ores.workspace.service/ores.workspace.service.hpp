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
#ifndef ORES_WORKSPACE_SERVICE_ORES_WORKSPACE_SERVICE_HPP
#define ORES_WORKSPACE_SERVICE_ORES_WORKSPACE_SERVICE_HPP

/**
 * @brief The process that hosts the workspace component.
 *
 * The service application builds the database context, assembles the
 * PostgreSQL LISTEN/NOTIFY to NATS change-event pipeline through the generated
 * event registrar, and runs the domain service with the component's registrar.
 * The configuration and the host it runs under live beside it here; the
 * handlers it serves are in ores::workspace.
 */
namespace ores::workspace::service {}

#endif

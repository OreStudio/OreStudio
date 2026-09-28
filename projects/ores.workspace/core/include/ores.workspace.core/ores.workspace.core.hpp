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
#ifndef ORES_WORKSPACE_CORE_ORES_WORKSPACE_CORE_HPP
#define ORES_WORKSPACE_CORE_ORES_WORKSPACE_CORE_HPP

/**
 * @brief Named, isolated data contexts and the inheritance chain between them.
 *
 * A workspace is a row in ores_workspaces_tbl, and the data a workspace does
 * not carry is resolved from its parent chain up to the Live workspace. This
 * namespace holds the repository that persists those rows and reads the chain,
 * the service that wraps it, the messaging layer that serves both over NATS,
 * the presentation mapper the history dialog renders through, and the service
 * application that hosts them all. The domain types and the protocol schemas
 * they travel in live in ores::workspace::api; the process entry point lives
 * in ores::workspace::service.
 */
namespace ores::workspace {}

#endif

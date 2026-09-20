/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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

/**
 * The TypeScript twin of `ores::utility::domain::hierarchy_node`.
 *
 * The C++ type is a hand-written utility struct, not a codegen model, so
 * no `ores.ts.domain` facet emits it. The generated protocol modules that
 * carry a hierarchy import this interface instead. Keep the members in
 * step with `projects/ores.utility/include/ores.utility/domain/hierarchy.hpp`.
 */
export interface HierarchyNode {
    id: string;
    name: string;
    parent_id: string | null;
    children: HierarchyNode[];
}

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
 * The TypeScript twin of the storage grant an IAM capability carries.
 *
 * The C++ type is a hand-written struct in
 * `projects/ores.security/include/ores.security/jwt/jwt_claims.hpp`, not a
 * codegen model, so no `ores.ts.domain` facet emits it. The generated
 * storage capability protocol imports this interface instead. Keep the
 * members in step with that header.
 */

/**
 * One grant row: the bucket, the key prefix inside it, and the operation
 * allowed there.
 */
export interface StorageGrant {
    bucket: string;
    key_prefix: string;
    op: string;
}

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
 *
 */

import { symlink } from 'node:fs/promises';
import type { TestContext } from 'vitest';

/** Keep link-security assertions active wherever Windows permits creating links. */
export async function symlinkForTest(
    context: TestContext,
    target: string,
    path: string,
    type: 'file' | 'dir',
): Promise<void> {
    try {
        await symlink(target, path, type);
    } catch (error) {
        const code = (error as NodeJS.ErrnoException).code;
        if (
            process.platform === 'win32' &&
            (code === 'EPERM' || code === 'EACCES' || code === 'ENOSYS')
        ) {
            context.skip(`Windows cannot create test symbolic links (${code}).`);
        }
        throw error;
    }
}

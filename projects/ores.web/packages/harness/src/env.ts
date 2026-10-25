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

import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const REPO_ROOT = resolve(dirname(fileURLToPath(import.meta.url)), '..', '..', '..', '..', '..');

/**
 * The checkout's .env is the authority for where the web listens, as it is
 * for the BFF and for compass open-as. A variable already in the process
 * environment wins, so a run can point at another host.
 */
try {
    process.loadEnvFile(resolve(REPO_ROOT, '.env'));
} catch {
    // A checkout without one falls back to the defaults below.
}

export function setting(name: string): string | undefined {
    const value = process.env[name];
    return value === undefined || value === '' ? undefined : value;
}

/** The address the harness drives, http://localhost:<ORES_WEB_PORT>/ by default. */
export function webUrl(): string {
    return setting('ORES_HARNESS_URL') ?? `http://localhost:${setting('ORES_WEB_PORT') ?? '8080'}/`;
}

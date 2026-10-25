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

import { expect, test as setup } from '@playwright/test';
import { readFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { PASSWORD, TENANT_ADMIN, TENANT_HOSTNAME } from '../src/acme.js';
import { ADMIN_STATE, signIn } from '../src/signIn.js';

/** The shell script that provisions Acme, and so holds the facts acme.ts copies. */
const PROVISIONING = resolve(
    dirname(fileURLToPath(import.meta.url)),
    '../../../../ores.shell/scripts/library/provisioning',
    'how_do_i_provision_the_system_with_acme_corporation_holding_group.ores',
);

/**
 * A password or hostname that drifts from the provisioning script would fail
 * every check as a sign-in error; this names the drift instead.
 */
setup('the Acme facts match the provisioning script', async () => {
    const script = readFileSync(PROVISIONING, 'utf8');
    const word = (s: string) => s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
    expect(script, `${PROVISIONING} sets the tenant administrator password acme.ts holds`).toMatch(
        new RegExp(`--tenant-admin-password ${word(PASSWORD)}(\\s|$)`, 'm'),
    );
    expect(script, `${PROVISIONING} names the tenant hostname acme.ts holds`).toMatch(
        new RegExp(`--tenant-hostname ${word(TENANT_HOSTNAME)}(\\s|$)`, 'm'),
    );
});

/** Signs in once as the tenant administrator; every check reuses the session. */
setup('sign in as the tenant administrator', async ({ page }) => {
    await signIn(page, TENANT_ADMIN);
    await page.context().storageState({ path: ADMIN_STATE });
});

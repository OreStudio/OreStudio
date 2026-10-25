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

import { expect, test } from '../src/signIn.js';

/**
 * The harness reaches the environment: it signs in as a seeded person, opens a
 * record list and a record page. Every later check builds on this.
 */
test('signs in as the tenant administrator and opens the currencies', async ({ asAdmin: page }) => {
    await page.goto('/refdata/currencies');
    await expect(page.getByRole('heading', { level: 1, name: 'Currencies' })).toBeVisible();
    // RS-LIST-03: Refresh, then Add, stand in the header for a person who may write.
    await expect(page.getByRole('button', { name: 'Refresh', exact: true })).toBeVisible();
    await expect(page.getByRole('button', { name: 'Add currency', exact: true })).toBeVisible();
    // RS-LIST-04: the search goes to the server, so a code on a later page is found and
    // the first page's rows are gone.
    const firstRow = page.locator('table tbody tr').first().getByRole('cell').first();
    const firstCode = (await firstRow.innerText()).trim();
    expect(firstCode).not.toBe('EUR');
    await page.getByRole('searchbox').fill('EUR');
    await expect(page.getByRole('cell', { name: firstCode, exact: true })).toHaveCount(0);
    const row = page
        .getByRole('row')
        .filter({ has: page.getByRole('cell', { name: 'EUR', exact: true }) });
    await expect(row).toHaveCount(1);
    // RS-LIST-10: a click on the row opens the record page.
    await row.click();
    await expect(page).toHaveURL(/\/refdata\/currencies\/EUR$/);
    // RS-PAGE-01: the title is the display field.
    await expect(page.getByRole('heading', { level: 1, name: 'Euro' })).toBeVisible();
});

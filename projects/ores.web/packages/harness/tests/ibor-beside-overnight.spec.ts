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

import { expect, test } from '@playwright/test';
import { SUPER_ADMIN } from '../src/acme.js';
import { signIn } from '../src/signIn.js';

/**
 * A currency's IBOR curve beside its overnight curve, against the running
 * services: the coverage lists every curve of a currency with its role, and a
 * projection opens in the curve analytics beside its funding curve, with the
 * panel that states what the pair teaches. Calibration is a system control, so
 * the system administrator reads it.
 */
test.use({ storageState: { cookies: [], origins: [] } });

test.beforeEach(async ({ page }) => {
    await signIn(page, SUPER_ADMIN);
});

test('a projection curve opens beside its funding curve, with what the pair teaches', async ({
    page,
}) => {
    await page.goto('/market-data/calibration/interest_rates');
    const coverage = page.locator('[data-ir-coverage]');
    await expect(coverage).toBeVisible();
    const projection = coverage
        .locator('[data-ir-curve]')
        .filter({ has: page.getByText('Projection', { exact: true }) })
        .first();
    await expect(projection).toBeVisible();
    await expect(projection).toContainText('discounts off');
    await projection.getByRole('link').click();
    await expect(page).toHaveURL(/\/market-data\/curve-analytics\?curve=/);
    const teaches = page.getByRole('region', { name: 'What the two curves teach' });
    await expect(teaches).toBeVisible();
    await expect(teaches).toContainText('discounts');
    await expect(teaches).toContainText('projects');
});

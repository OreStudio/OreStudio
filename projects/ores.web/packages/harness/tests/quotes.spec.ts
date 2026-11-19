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

import { expect, test } from '@playwright/test';

/**
 * The quotes view reads the C++ server's board: the sources on the rail, each one
 * included or left out, and a board for each asset class and shape with each
 * vendor a column. A picked row opens every vendor's quote of it and their hour.
 */
test('the quotes view shows a board per asset class and shape, each vendor a column', async ({
    page,
}) => {
    await page.goto('/market-data/quotes');
    await expect(page.getByRole('heading', { level: 1, name: 'Quotes' })).toBeVisible();
    const rail = page.getByRole('navigation', { name: 'Sources' });
    await expect(rail.getByRole('checkbox').first()).toBeChecked();
    await expect(rail.getByText('bloomberg')).toBeVisible();
    const boards = page.getByRole('tablist', { name: 'Boards' });
    await expect(boards.getByRole('tab', { name: /FX quotes/ })).toBeVisible({ timeout: 30_000 });
    await boards.getByRole('tab', { name: /FX quotes/ }).click();
    const rows = page.locator('table').first().locator('tbody tr');
    await expect.poll(async () => rows.count(), { timeout: 30_000 }).toBeGreaterThan(5);
    await expect(page.locator('table').first().locator('thead')).toContainText('bloomberg');
    await page.screenshot({ path: '../../.runtime/harness/quotes-board.png' });
    await rows.filter({ hasText: 'EUR/USD' }).first().click();
    await expect(page.getByText(/^Mid of \d+$/)).toBeVisible();
    await expect(page.getByRole('img', { name: /The last hour of EUR\/USD/ })).toBeVisible({
        timeout: 15_000,
    });
    await page.screenshot({ path: '../../.runtime/harness/quotes-picked.png' });

    // Leaving a source out takes its column off every board.
    await rail.getByRole('checkbox').first().uncheck();
    await expect(page.locator('table').first().locator('thead')).not.toContainText('bloomberg');
    await rail.getByRole('checkbox').first().check();

    const curves = boards.getByRole('tab', { name: /curves/ });
    if ((await curves.count()) > 0) {
        await curves.first().click();
        await page.locator('thead button').first().click();
        await expect(page.getByRole('img', { name: /by tenor, each vendor/ })).toBeVisible();
        await page.screenshot({ path: '../../.runtime/harness/quotes-curves.png' });
    }
});

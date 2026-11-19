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
 * The quotes view reads the C++ server's board: the sources the session may watch
 * on the rail, the quotes updating in place, and every vendor's quote of a picked
 * row.
 */
test("the quotes view shows the sources, the ticking quotes and a row's all quotes", async ({
    page,
}) => {
    await page.goto('/market-data/quotes');
    await expect(page.getByRole('heading', { level: 1, name: 'Quotes' })).toBeVisible();
    const rail = page.getByRole('navigation', { name: 'Sources' });
    await expect(rail.getByRole('button', { name: /All vendors/ })).toBeVisible();
    await expect(rail.getByRole('button', { name: /bloomberg/ })).toBeVisible();
    const rows = page.locator('table').first().locator('tbody tr');
    await expect.poll(async () => rows.count(), { timeout: 30_000 }).toBeGreaterThan(5);
    // The table pages: at most one page of rows, and the pager says which.
    await expect(page.getByText(/^1–\d+ of \d+$/)).toBeVisible();
    expect(await rows.count()).toBeLessThanOrEqual(50);
    await page.screenshot({ path: '../../.runtime/harness/quotes-all.png' });
    await rows.filter({ hasText: 'fx_spot' }).first().click();
    const allQuotes = page.getByRole('heading', { name: /All quotes/ });
    await expect(allQuotes).toBeVisible();
    // The panel draws each vendor's last hour once the history reads answer.
    await expect(page.locator('section', { has: allQuotes }).locator('svg').first()).toBeVisible({
        timeout: 15_000,
    });
    await page.screenshot({ path: '../../.runtime/harness/quotes-picked.png' });
});

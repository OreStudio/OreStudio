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

import { expect, test, type Page } from '@playwright/test';

const PANEL = 'Market regime';

/** The Market regime tab of the synthetic screen, which holds the whole market. */
async function openRegimeTab(page: Page): Promise<void> {
    await page.goto('/market-data/synthetic?tab=regime');
    await expect(page.getByRole('heading', { level: 1, name: 'Synthetic' })).toBeVisible();
}

test.describe('the market regime tab', () => {
    test('reads the state in force, the multiplier and who holds it', async ({ page }) => {
        await openRegimeTab(page);
        const panel = page.getByRole('region', { name: PANEL });
        await expect(panel).toBeVisible();
        await expect(panel.getByText('Set by', { exact: true })).toBeVisible();
        await expect(panel.getByText(/^×\d/)).toBeVisible();
        // One switch is in force, whichever it is.
        await expect(panel.getByRole('button', { pressed: true })).toHaveCount(1);
    });

    test('forces the state, and hands it back to the chain', async ({ page }) => {
        await openRegimeTab(page);
        const panel = page.getByRole('region', { name: PANEL });
        const active = panel.getByRole('button', { name: 'Active', exact: true });
        const auto = panel.getByRole('button', { name: 'Auto', exact: true });

        await active.click();
        await expect(active).toHaveAttribute('aria-pressed', 'true');
        await expect(panel.getByText('×2')).toBeVisible();
        await expect(
            panel.getByText('A person, and it holds until it is handed back'),
        ).toBeVisible();

        await auto.click();
        await expect(auto).toHaveAttribute('aria-pressed', 'true');
        await expect(panel.getByText('The chain, turning on its own')).toBeVisible();
    });

    test('draws a row for the feeds that run', async ({ page }) => {
        await openRegimeTab(page);
        const panel = page.getByRole('region', { name: PANEL });
        await expect(
            panel.getByRole('img', { name: 'Tick arrivals of every running feed' }),
        ).toBeVisible();
        await expect(panel.getByText('synthetic.bloomberg.eurusd')).toBeVisible();
    });
});

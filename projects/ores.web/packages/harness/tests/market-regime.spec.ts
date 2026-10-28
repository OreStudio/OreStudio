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
import { SUPER_ADMIN } from '../src/acme.js';
import { signIn } from '../src/signIn.js';

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
        // The multiplier is the first factor the panel states; the pace beside it is another.
        await expect(panel.getByText(/^×\d/).first()).toBeVisible();
        // A tenant reads the market and cannot set it: the installation's market is the system's.
        await expect(panel.getByRole('button', { name: 'Active', exact: true })).toHaveCount(0);
        await expect(
            panel.getByText('The system administrator sets the market for the whole installation'),
        ).toBeVisible();
        const settings = panel.getByRole('region', { name: 'What moves the market' });
        await expect(settings.getByText('Market speed', { exact: true })).toBeVisible();
        await expect(settings.getByRole('spinbutton')).toHaveCount(0);
        await expect(settings.getByRole('button', { name: 'Apply' })).toHaveCount(0);
    });

    test('draws a row for the feeds that run', async ({ page }) => {
        await openRegimeTab(page);
        const panel = page.getByRole('region', { name: PANEL });
        await expect(
            panel.getByRole('img', {
                name: 'Tick arrivals of every feed, the feeds that run first',
            }),
        ).toBeVisible();
        await expect(panel.getByText('synthetic.bloomberg.eurusd')).toBeVisible();
    });

    test('shows how fast market time runs and each feed against its law', async ({ page }) => {
        await openRegimeTab(page);
        const panel = page.getByRole('region', { name: PANEL });
        await expect(panel.getByText('Run seed', { exact: true })).toBeVisible();
        await expect(panel.getByRole('img', { name: 'how fast market time runs' })).toBeVisible();
        await expect(
            panel.getByRole('region', { name: 'Each feed against its law' }),
        ).toBeVisible();
        await panel.getByRole('radio', { name: 'Market time τ' }).click();
        await expect(panel.getByRole('radio', { name: 'Market time τ' })).toHaveAttribute(
            'aria-checked',
            'true',
        );
    });
});

test.describe('the market regime tab, as the system administrator', () => {
    test.use({ storageState: { cookies: [], origins: [] } });

    test.beforeEach(async ({ page }) => {
        await signIn(page, SUPER_ADMIN);
    });

    test('offers the switch, with one state in force', async ({ page }) => {
        await openRegimeTab(page);
        const panel = page.getByRole('region', { name: PANEL });
        await expect(panel.getByRole('button', { pressed: true })).toHaveCount(1);
    });

    test('forces the state, and hands it back to the chain', async ({ page }) => {
        await openRegimeTab(page);
        const panel = page.getByRole('region', { name: PANEL });
        const active = panel.getByRole('button', { name: 'Active', exact: true });
        const auto = panel.getByRole('button', { name: 'Auto', exact: true });

        await active.click();
        await expect(active).toHaveAttribute('aria-pressed', 'true');
        await expect(panel.getByText('×2', { exact: true })).toBeVisible();
        await expect(
            panel.getByText('A person, and it holds until it is handed back'),
        ).toBeVisible();

        await auto.click();
        await expect(auto).toHaveAttribute('aria-pressed', 'true');
        await expect(panel.getByText('The chain, turning on its own')).toBeVisible();
    });

    test('refuses heat that would not settle, naming its gain', async ({ page }) => {
        await openRegimeTab(page);
        const settings = page
            .getByRole('region', { name: PANEL })
            .getByRole('region', { name: 'What moves the market' });
        const decay = settings.getByRole('spinbutton', { name: /Heat decay/ });
        const jump = settings.getByRole('spinbutton', { name: /Heat one tick adds/ });
        await jump.fill(String(Number(await decay.inputValue()) * 2));
        await settings.getByRole('button', { name: 'Apply' }).click();
        await expect(settings.getByText(/is 2; it must be below one/)).toBeVisible();
        await settings.getByRole('button', { name: 'Discard' }).click();
    });

    test('sets the market speed, and sets it back', async ({ page }) => {
        await openRegimeTab(page);
        const panel = page.getByRole('region', { name: PANEL });
        const settings = panel.getByRole('region', { name: 'What moves the market' });
        const speed = settings.getByRole('spinbutton', { name: 'Market speed' });
        const before = await speed.inputValue();
        const doubled = String(Number(before) * 2);

        await speed.fill(doubled);
        await settings.getByRole('button', { name: 'Apply' }).click();
        await expect(
            panel.getByText(`${doubled} simulated seconds a unit of market time`),
        ).toBeVisible();

        await speed.fill(before);
        await settings.getByRole('button', { name: 'Apply' }).click();
        await expect(
            panel.getByText(`${before} simulated seconds a unit of market time`),
        ).toBeVisible();
    });
});

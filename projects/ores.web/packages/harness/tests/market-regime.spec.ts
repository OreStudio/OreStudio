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

const MARKET = 'The market';

/** One of the market's own tabs, inside the Synthetic screen's Market tab. */
async function openMarket(page: Page, view = 'now'): Promise<void> {
    await page.goto(`/market-data/synthetic?tab=regime${view === 'now' ? '' : `&view=${view}`}`);
    await expect(page.getByRole('heading', { level: 1, name: 'Synthetic' })).toBeVisible();
    await expect(page.getByRole('region', { name: 'The market now' })).toBeVisible();
}

test.describe('the market, read by a tenant', () => {
    test('states the market in a strip above every tab', async ({ page }) => {
        await openMarket(page);
        const strip = page.getByRole('region', { name: 'The market now' });
        await expect(strip.getByText('Multiplier', { exact: true })).toBeVisible();
        await expect(strip.getByText(/^×\d/).first()).toBeVisible();
        await expect(strip.getByText('Market speed', { exact: true })).toBeVisible();
        const tabs = page.getByRole('tablist', { name: MARKET });
        for (const name of ['Now', 'Settings', 'Arrivals', 'Laws', 'Fit', 'Trace']) {
            await expect(tabs.getByRole('tab', { name: new RegExp(`^${name}`) })).toBeVisible();
        }
    });

    test('reads the state on Now and offers no switch', async ({ page }) => {
        await openMarket(page);
        await expect(page.getByRole('img', { name: 'How fast market time runs' })).toBeVisible();
        const force = page.getByRole('region', { name: 'Force the state' });
        await expect(force.getByText('Set by', { exact: true })).toBeVisible();
        await expect(
            force.getByText('The system administrator sets the market for the whole installation'),
        ).toBeVisible();
        await expect(force.getByRole('button', { name: 'Active', exact: true })).toHaveCount(0);
    });

    test('reads the settings and cannot change them', async ({ page }) => {
        await openMarket(page, 'settings');
        const settings = page.getByRole('region', { name: 'What moves the market' });
        await expect(settings.getByText('Read only.', { exact: false })).toBeVisible();
        await expect(page.getByRole('textbox', { name: 'Market speed' })).toHaveAttribute(
            'readonly',
            '',
        );
        await expect(page.getByRole('button', { name: 'Apply' })).toHaveCount(0);
    });

    test('draws every running feed on Arrivals', async ({ page }) => {
        await openMarket(page, 'arrivals');
        await expect(
            page.getByRole('img', {
                name: 'Tick arrivals of every feed, the feeds that run first',
            }),
        ).toBeVisible();
        await page.getByRole('radio', { name: '30 s' }).click();
        await expect(page.getByText('Last 30 s, clock time')).toBeVisible();
        await page.getByRole('radio', { name: 'Market time τ' }).click();
        await expect(page.getByRole('radio', { name: 'Market time τ' })).toHaveAttribute(
            'aria-checked',
            'true',
        );
    });

    test('reads each feed’s law on Laws and links to the feed', async ({ page }) => {
        await openMarket(page, 'laws');
        const laws = page.getByRole('region', { name: 'Every feed’s law' });
        const link = laws.getByRole('link', { name: 'Arrivals tab →' }).first();
        await expect(link).toHaveAttribute(
            'href',
            /\/market-data\/synthetic\/(fx|ir)\/.+\?tab=arrivals$/,
        );
        await expect(laws.getByRole('textbox')).toHaveCount(0);
    });

    test('checks each feed against its law on Fit, with the loop’s health', async ({ page }) => {
        await openMarket(page, 'fit');
        await expect(page.getByRole('region', { name: 'Each feed against its law' })).toBeVisible();
        await expect(
            page.getByRole('region', { name: 'The loop’s health' }).getByText('Run seed'),
        ).toBeVisible();
    });

    test('lists the loop’s decisions on Trace', async ({ page }) => {
        await openMarket(page, 'trace');
        const trace = page.getByRole('region', { name: 'What the loop decided' });
        await expect(trace.getByText(/decisions match/)).toBeVisible();
    });
});

test.describe('the market, as the system administrator', () => {
    test.use({ storageState: { cookies: [], origins: [] } });

    test.beforeEach(async ({ page }) => {
        await signIn(page, SUPER_ADMIN);
    });

    test('forces the state, and hands it back to the chain', async ({ page }) => {
        await openMarket(page);
        const force = page.getByRole('region', { name: 'Force the state' });
        const active = force.getByRole('button', { name: 'Active', exact: true });
        const auto = force.getByRole('button', { name: 'Auto', exact: true });

        await active.click();
        await expect(active).toHaveAttribute('aria-pressed', 'true');
        await expect(
            page.getByRole('region', { name: 'The market now' }).getByText('forced'),
        ).toBeVisible();

        await auto.click();
        await expect(auto).toHaveAttribute('aria-pressed', 'true');
        await expect(force.getByText('The chain, turning on its own')).toBeVisible();
    });

    test('names the number when heat would not settle, and applies nothing', async ({ page }) => {
        await openMarket(page, 'settings');
        const decay = page.getByRole('textbox', { name: 'Heat decay, a second' });
        const jump = page.getByRole('textbox', { name: 'Heat one tick adds' });
        await jump.fill(String(Number(await decay.inputValue()) * 2));
        await expect(page.getByText('loop gain 2, must be below 1')).toBeVisible();
        await expect(page.getByRole('button', { name: 'Apply' })).toBeDisabled();
        await page.getByRole('button', { name: 'Discard' }).click();
    });

    test('sets the market speed, and sets it back', async ({ page }) => {
        await openMarket(page, 'settings');
        const speed = page.getByRole('textbox', { name: 'Market speed' });
        const strip = page.getByRole('region', { name: 'The market now' });
        const before = await speed.inputValue();
        const doubled = String(Number(before) * 2);

        await speed.fill(doubled);
        await expect(page.getByText(`Market speed ${before} → ${doubled}`)).toBeVisible();
        await page.getByRole('button', { name: 'Apply' }).click();
        await expect(strip.getByText(doubled, { exact: true })).toBeVisible();

        await speed.fill(before);
        await page.getByRole('button', { name: 'Apply' }).click();
        await expect(strip.getByText(before, { exact: true })).toBeVisible();
    });
});

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
import { allScreens, type Screen } from '../src/screens.js';

const AUDIT_COLUMNS = [
    'Version',
    'Modified by',
    'Performed by',
    'Recorded at',
    'Change reason',
    'Commentary',
];

/** The x coordinate of an element, so the order of two controls can be read. */
/** The kinds that carry the standard Import and Export pair (RS-ORE-01); the currencies are the first. */
const ORE_DOCUMENT_KINDS: ReadonlySet<string> = new Set(['currencies']);

async function left(page: Page, name: string | RegExp): Promise<number> {
    const box = await page.getByRole('button', { name }).first().boundingBox();
    expect(box, `the button ${name} has a box`).not.toBeNull();
    return box?.x ?? 0;
}

/**
 * The list of every record kind with a screen, against the list rules of the
 * Record screen standard. A test's title carries the identifier of the rule it
 * checks, so a failure names the rule and the describe names the screen.
 */
for (const screen of allScreens()) {
    test.describe(`${screen.shape} ${screen.key}`, () => {
        test.beforeEach(async ({ page }) => {
            await page.goto(screen.list);
            await expect(page, 'the saved session is still signed in').not.toHaveURL(/sign-in/);
            await expect(page.getByRole('heading', { level: 1 })).toBeVisible();
        });

        test('[RS-LIST-01] the breadcrumb ends at the plural title', async ({ page }) => {
            const title = (await page.getByRole('heading', { level: 1 }).innerText()).trim();
            const crumbs = page.getByRole('navigation', { name: 'Breadcrumb' });
            await expect(crumbs).toBeVisible();
            await expect(crumbs).toContainText(title);
        });

        test('[RS-LIST-02] one sentence says what the records are', async ({ page }) => {
            const lead = page
                .getByRole('heading', { level: 1 })
                .locator('xpath=following-sibling::p[1]');
            await expect(lead).toBeVisible();
            expect((await lead.innerText()).trim().length, 'the lead has words').toBeGreaterThan(
                10,
            );
        });

        test('[RS-LIST-03] Refresh, then Add, stand at the right; Add only for a writer', async ({
            page,
        }) => {
            await expect(page.getByRole('button', { name: 'Refresh', exact: true })).toBeVisible();
            const add = page.getByRole('button', { name: /^Add\b/ });
            // The session is the tenant administrator's, who may write every kind the
            // server lets anyone write.
            await expect(add, 'Add is drawn exactly for a writable kind').toHaveCount(
                screen.writable ? 1 : 0,
            );
            if (screen.writable) {
                expect(await left(page, 'Refresh')).toBeLessThan(await left(page, /^Add\b/));
            }
        });

        test('[RS-ORE-02] a kind with an ORE document draws Import and Export between Refresh and Add', async ({
            page,
        }) => {
            test.skip(!ORE_DOCUMENT_KINDS.has(screen.key), 'the kind has no ORE document');
            const exportButton = page.getByRole('button', { name: 'Export', exact: true });
            const importLink = page.getByRole('link', { name: 'Import', exact: true });
            await expect(exportButton).toBeVisible();
            await expect(importLink, 'Import is drawn for a writer').toHaveCount(
                screen.writable ? 1 : 0,
            );
            const refreshAt = await left(page, 'Refresh');
            const exportAt = (await exportButton.boundingBox())?.x ?? 0;
            expect(refreshAt).toBeLessThan(exportAt);
            if (screen.writable) {
                expect(exportAt).toBeLessThan(await left(page, /^Add\b/));
            }
        });

        test('[RS-ORE-04] Export opens an operation dialog that names the document', async ({
            page,
        }) => {
            test.skip(!ORE_DOCUMENT_KINDS.has(screen.key), 'the kind has no ORE document');
            await page.getByRole('button', { name: 'Export', exact: true }).click();
            const dialog = page.getByRole('dialog');
            await expect(dialog.getByRole('heading', { level: 2 })).toHaveText('Export');
            await expect(dialog.getByText(/\.xml/)).toBeVisible();
            await expect(dialog.getByRole('button', { name: 'Export', exact: true })).toBeVisible();
            await expect(dialog.getByRole('button', { name: 'Cancel', exact: true })).toBeVisible();
        });

        test('[ES-REFRESH-16] the load time stands under Refresh', async ({ page }) => {
            const header = page
                .getByRole('button', { name: 'Refresh', exact: true })
                .locator('xpath=..');
            await expect(header.locator('time')).toBeVisible();
        });

        test('[RS-LIST-04] a search box stands above the table', async ({ page }) => {
            await expect(page.getByRole('searchbox')).toBeVisible();
        });

        test('[RS-LIST-13] the audit columns are hidden by default', async ({ page }) => {
            for (const column of AUDIT_COLUMNS) {
                await expect(
                    page.getByRole('columnheader', { name: column, exact: true }),
                    `the column ${column} is hidden`,
                ).toHaveCount(0);
            }
        });

        test('[RS-LIST-17] the pager shows the count first and starts at 15 rows', async ({
            page,
        }) => {
            test.skip(
                screen.shape === 'classification',
                'RS-EXCEPT-01: a classification list is read whole',
            );
            await expect(page.getByText(/^\d+[–-]\d+ of \d+$/).first()).toBeVisible();
            const size = page
                .getByRole('combobox')
                .filter({ has: page.locator('option[value="15"]') })
                .first();
            await expect(size).toHaveValue('15');
        });

        test('[RS-LIST-10] a row opens the record page on a click', async ({ page }) => {
            const rows = page.locator('table tbody tr');
            await expect(rows.first()).toBeVisible();
            await rows.first().getByRole('cell').first().click();
            await expect(page).toHaveURL(new RegExp(`${screen.list}/[^/?]+`));
            await expect(page.getByRole('heading', { level: 1 })).toBeVisible();
        });
    });
}

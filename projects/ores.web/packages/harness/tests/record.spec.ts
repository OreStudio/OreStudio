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

import { expect, test, type Locator, type Page } from '@playwright/test';
import { allScreens, type Screen } from '../src/screens.js';

/** Opens the first row of a list and returns the record page's title. */
async function openFirstRecord(page: Page, screen: Screen): Promise<string> {
    await page.goto(screen.list);
    await expect(page, 'the saved session is still signed in').not.toHaveURL(/sign-in/);
    const rows = page.locator('table tbody tr');
    await expect(rows.first()).toBeVisible();
    await rows.first().getByRole('cell').first().click();
    const opened = await page
        .waitForURL(new RegExp(`${screen.list}/[^/?]+`), { timeout: 10_000 })
        .then(
            () => true,
            () => false,
        );
    // A read-only classification list opens nothing, and its list check reports
    // RS-LIST-10 once; a record kind that opens nothing is a failure here too.
    test.skip(
        !opened && screen.shape === 'classification' && !screen.writable,
        'RS-LIST-10 fails on this read-only list: a row opens no record page',
    );
    expect(opened, `a row of ${screen.list} opens a record page`).toBe(true);
    const title = page.getByRole('heading', { level: 1 });
    await expect(title).toBeVisible();
    return (await title.innerText()).trim();
}

/** Every label in a scope, as the person reads it. */
async function labels(scope: Locator): Promise<string[]> {
    const texts = await scope.locator('dt, label').allInnerTexts();
    return texts.map((t) => t.trim()).filter((t) => t.length > 0);
}

function sentenceCaseWithNoColon(label: string): boolean {
    const first = label.charAt(0);
    return !label.endsWith(':') && first === first.toUpperCase() && label !== label.toUpperCase();
}

/**
 * The record page of every record kind with a screen, and its Edit dialog
 * where the administrator may write, against the record page and form rules.
 */
for (const screen of allScreens()) {
    test.describe(`${screen.shape} ${screen.key}`, () => {
        let title = '';
        test.beforeEach(async ({ page }) => {
            title = await openFirstRecord(page, screen);
        });

        test('[RS-PAGE-01] the breadcrumb ends at the subject, by its key, and the title is its name', async ({
            page,
        }) => {
            const crumbs = page.getByRole('navigation', { name: 'Breadcrumb' });
            await expect(crumbs).toBeVisible();
            const last = (
                await crumbs
                    .locator('xpath=.//*[normalize-space(text())][last()]')
                    .last()
                    .innerText()
            ).trim();
            const key = decodeURIComponent(new URL(page.url()).pathname.split('/').pop() ?? '');
            expect([key, title], 'the last crumb is the key or the display field').toContain(last);
            expect(title.length, 'the title is the display field').toBeGreaterThan(0);
        });

        test('[RS-PAGE-02] the lead line reads key and version', async ({ page }) => {
            await expect(page.getByText(/· version \d+/).first()).toBeVisible();
        });

        test('[RS-PAGE-09] Details is the first tab and History the last', async ({ page }) => {
            const tabs = page.getByRole('tablist').getByRole('tab');
            const names = (await tabs.allInnerTexts()).map((t) => t.trim());
            expect(names.length, 'the page has tabs').toBeGreaterThan(0);
            expect(names[0]).toBe('Details');
            expect(names[names.length - 1]).toBe('History');
        });

        test('[RS-PAGE-05] a label stands above each value in sentence case with no colon', async ({
            page,
        }) => {
            const found = await labels(page.getByRole('tabpanel').or(page.getByRole('main')));
            expect(found.length, 'the details have labels').toBeGreaterThan(0);
            const wrong = found.filter((l) => !sentenceCaseWithNoColon(l));
            expect(wrong, 'labels in sentence case with no colon').toEqual([]);
        });

        test('[RS-FEEDBACK-02] the access mark says Editable or Read only', async ({ page }) => {
            await expect(page.getByText(/^(Editable|Read only)$/).first()).toBeVisible();
        });

        test('[ES-REFRESH-16] the load time stands under Refresh', async ({ page }) => {
            const header = page
                .getByRole('button', { name: 'Refresh', exact: true })
                .locator('xpath=..');
            await expect(header.locator('time')).toBeVisible();
        });

        test('[RS-PAGE-10] the details end with the last-changed line', async ({ page }) => {
            await expect(page.getByText(/^Last changed/).first()).toBeVisible();
        });

        test('[ES-HIST-18] the History tab is the shared timeline', async ({ page }) => {
            await page.getByRole('tab', { name: 'History' }).click();
            await expect(
                page.getByRole('button', { name: /system activity/ }),
                'the timeline draws the system activity toggle',
            ).toBeVisible();
            // The legacy panel compares two versions side by side; the timeline never does.
            await expect(page.getByText(/^Compare\b/)).toHaveCount(0);
            await expect(page.getByRole('button', { name: 'Only changes' })).toHaveCount(0);
        });

        test.describe('the Edit dialog', () => {
            test.beforeEach(async ({ page }) => {
                // The access mark is drawn with the actions, so once it shows, Edit is
                // there or absent for good.
                await expect(page.getByText(/^(Editable|Read only)$/).first()).toBeVisible();
                const edit = page.getByRole('button', { name: 'Edit', exact: true });
                test.skip((await edit.count()) === 0, 'RS-WRITE-33: the record offers no Edit');
                await edit.first().click();
                await expect(page.getByRole('dialog')).toBeVisible();
            });

            test('[RS-WRITE-20] the dialog is titled Edit and the title of the page', async ({
                page,
            }) => {
                await expect(
                    page.getByRole('dialog').getByRole('heading', { level: 2 }),
                ).toHaveText(`Edit ${title}`);
            });

            test('[RS-WRITE-16] the dialog says when its record was loaded', async ({ page }) => {
                await expect(page.getByRole('dialog').locator('time').first()).toBeVisible();
            });

            test('[RS-WRITE-09] the form is one dialog with no tabs', async ({ page }) => {
                await expect(page.getByRole('dialog').getByRole('tablist')).toHaveCount(0);
            });

            test('[RS-WRITE-12] each label stands in sentence case with no colon', async ({
                page,
            }) => {
                const found = await labels(page.getByRole('dialog'));
                expect(found.length, 'the form has labels').toBeGreaterThan(0);
                expect(found.filter((l) => !sentenceCaseWithNoColon(l))).toEqual([]);
            });

            test('[RS-WRITE-14] the footer holds Cancel, then Save, at the right', async ({
                page,
            }) => {
                const dialog = page.getByRole('dialog');
                const cancel = await dialog
                    .getByRole('button', { name: 'Cancel', exact: true })
                    .boundingBox();
                const save = await dialog
                    .getByRole('button', { name: 'Save', exact: true })
                    .boundingBox();
                expect(cancel, 'Cancel is drawn').not.toBeNull();
                expect(save, 'Save is drawn').not.toBeNull();
                expect(cancel?.x ?? 0).toBeLessThan(save?.x ?? 0);
                await expect(
                    dialog.getByRole('button', { name: 'Delete', exact: true }),
                ).toHaveCount(0);
            });

            test('[RS-WRITE-24] closing with changes asks first', async ({ page }) => {
                const dialog = page.getByRole('dialog');
                // A field that takes words: a number field drops a letter, so typing one changes nothing.
                const field = dialog
                    .locator(
                        'input[type="text"]:not([disabled]):not([inputmode]), input:not([type]):not([disabled]):not([inputmode]), textarea:not([disabled])',
                    )
                    .first();
                await expect(field).toBeVisible();
                await field.fill(`${await field.inputValue()}x`);
                await dialog.getByRole('button', { name: 'Cancel', exact: true }).click();
                await expect(
                    dialog.getByText('You have unsaved changes. Close anyway?'),
                ).toBeVisible();
            });
        });
    });
}

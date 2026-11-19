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

/**
 * The identity shapes screen against the running services: the shared catalogue
 * is listed by family, a row's versions open beside it, and a tenant row is
 * added, shown as the tenant's own, and removed again.
 */

const FAMILY = 'fx / spot';
const FIELD = 'source';

test('the identity shapes are listed by family, with each row history', async ({ page }) => {
    await page.goto('/market-data/identity-shapes');
    await expect(page.getByRole('heading', { level: 1, name: 'Identity shapes' })).toBeVisible();
    const ibor = page.getByRole('region', { name: 'ir / index_fixing / ibor' });
    const name = ibor
        .getByRole('row')
        .filter({ has: page.getByRole('cell', { name: 'index_name' }) });
    await expect(name.getByText('Required', { exact: true })).toBeVisible();
    await expect(name.getByText('Shared', { exact: true })).toBeVisible();
    await name.getByRole('button', { name: 'History' }).click();
    await expect(ibor.getByRole('table', { name: 'History' })).toContainText('system.initial_load');
});

test('a tenant row is added as its own and removed again', async ({ page }) => {
    await page.goto('/market-data/identity-shapes');
    await page.getByRole('button', { name: 'Add a row' }).click();
    const add = page.getByRole('dialog');
    await add.getByLabel('Asset class').fill('fx');
    await add.getByLabel('Series subclass').fill('spot');
    await add.getByLabel('Field', { exact: true }).fill(FIELD);
    await add.getByLabel('Requirement').selectOption('forbidden');
    await add.getByLabel('Why').fill('A harness row: a spot rate names no fixing source.');
    await add.getByRole('button', { name: 'Save' }).click();
    await expect(add).toBeHidden();

    const family = page.getByRole('region', { name: FAMILY });
    const row = family.getByRole('row').filter({ has: page.getByRole('cell', { name: FIELD }) });
    await expect(row.getByText('Own', { exact: true })).toBeVisible();
    await expect(row.getByText('Forbidden', { exact: true })).toBeVisible();

    await row.getByRole('button', { name: 'Remove' }).click();
    const removal = page.getByRole('dialog');
    await removal.getByRole('button', { name: 'Remove' }).click();
    await expect(removal).toBeHidden();
    await expect(family.getByRole('cell', { name: FIELD })).toHaveCount(0);
});

test('the editor reads the rule that gives every fixing its subclass', async ({ page }) => {
    const response = await page.request.get('/api/marketdata/series-classification-rules');
    expect(response.ok()).toBe(true);
    const { rules } = (await response.json()) as {
        rules: { key_kind: string; series_subclass_code: string }[];
    };
    expect(rules.filter((rule) => rule.key_kind === 'index')).toEqual([
        expect.objectContaining({ series_subclass_code: 'index_fixing' }),
    ]);
});

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
 * The identity shapes against the running services: the shapes are a paged list
 * with what each is for, a shape opens on its own page with its entries, and a
 * tenant shape is added with an entry and deleted again.
 */

const CODE = 'credit.yield';

test('the shapes are a paged list, and a shape opens with its entries', async ({ page }) => {
    await page.goto('/market-data/identity-shapes');
    await expect(page.getByRole('heading', { level: 1, name: 'Identity shapes' })).toBeVisible();
    const ibor = page.getByRole('row').filter({ hasText: 'ir.index_fixing.ibor' });
    await expect(ibor).toContainText('interbank offered rate');
    await expect(page.getByText(/^1–\d+ of \d+$/)).toBeVisible();

    await ibor.getByRole('cell').first().click();
    await expect(page).toHaveURL(/\/market-data\/identity-shapes\/ir\.index_fixing\.ibor$/);
    await expect(page.getByText('interbank offered rate', { exact: false }).first()).toBeVisible();

    await page.getByRole('tab', { name: 'Entries' }).click();
    const name = page.getByRole('row').filter({ hasText: 'index_name' });
    await expect(name).toContainText('Required');
    const unit = page.getByRole('row').filter({ hasText: 'unit_ccy' });
    await expect(unit).toContainText('Forbidden');
});

test('a shape is added with an entry, and deleted with its entries', async ({ page }) => {
    await page.goto('/market-data/identity-shapes');
    await page.getByRole('button', { name: 'Add a shape' }).click();
    const add = page.getByRole('dialog');
    await add.getByLabel('Asset class').fill('credit');
    await add.getByLabel('Series subclass').fill('yield');
    await add.getByLabel('What it is for').fill('A harness shape: a credit yield of one issuer.');
    await add.getByRole('button', { name: 'Add', exact: true }).click();
    await expect(add).toBeHidden();
    await expect(page).toHaveURL(new RegExp(`/market-data/identity-shapes/${CODE}$`));

    await page.getByRole('tab', { name: 'Entries' }).click();
    await page.getByRole('button', { name: 'Add', exact: true }).click();
    const entry = page.getByRole('dialog');
    await entry.getByLabel('Field', { exact: true }).fill('unit_ccy');
    await entry.getByLabel('Requirement').selectOption('forbidden');
    await entry.getByLabel('Why').fill('A yield is a proportion, not a price.');
    await entry.getByRole('button', { name: 'Add', exact: true }).click();
    await expect(entry).toBeHidden();
    await expect(page.getByRole('row').filter({ hasText: 'unit_ccy' })).toContainText('Forbidden');

    await page.getByRole('button', { name: 'Delete', exact: true }).first().click();
    const removal = page.getByRole('dialog');
    await removal.getByRole('button', { name: 'Delete', exact: true }).click();
    await expect(page).toHaveURL(/\/market-data\/identity-shapes$/);
    await expect(page.getByRole('row').filter({ hasText: CODE })).toHaveCount(0);
});

test('the editor reads each field a family requires, and the rule that gives every fixing its subclass', async ({
    page,
}) => {
    const fields = await page.request.get('/api/marketdata/series-identity-shape-fields');
    expect(fields.ok()).toBe(true);
    const body = (await fields.json()) as {
        fields: { asset_class: string; index_family: string; field: string; requirement: string }[];
    };
    expect(body.fields).toContainEqual(
        expect.objectContaining({
            index_family: 'ibor',
            field: 'index_name',
            requirement: 'required',
        }),
    );

    const response = await page.request.get('/api/marketdata/series-classification-rules');
    expect(response.ok()).toBe(true);
    const { rules } = (await response.json()) as {
        rules: { key_kind: string; series_subclass_code: string }[];
    };
    expect(rules.filter((rule) => rule.key_kind === 'index')).toEqual([
        expect.objectContaining({ series_subclass_code: 'index_fixing' }),
    ]);
});

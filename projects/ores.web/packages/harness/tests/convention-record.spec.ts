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

const LIST = '/refdata/conventions/tenor-basis-swap';

/**
 * The terms only the long and short form carries. Two terms are named the same
 * on both forms -- the spread flag and the coupon type -- so they are not here:
 * they cannot tell a form apart.
 */
const LONG_SHORT_ONLY = [
    'Long leg index',
    'Long leg payment tenor',
    'Short leg index',
    'Short leg payment tenor',
    'Spread on the short leg',
];

/** The terms only the paying and receiving form carries. */
const PAY_RECEIVE_ONLY = [
    'Pay index',
    'Pay frequency',
    'Receive index',
    'Receive frequency',
    'Spread on rec',
    'Pay is averaged',
    'Rec is averaged',
];

/**
 * Opens the first convention of the list page that draws @p want and none of
 * @p avoid, so the check reads a convention of one form only. A row that holds
 * both forms is passed over rather than counted as a failure: the point of the
 * check is what a one-form convention draws.
 */
async function openOneFormConvention(page: Page, want: string, avoid: readonly string[]): Promise<void> {
    await page.goto(LIST);
    const rows = page.locator('table tbody tr');
    await expect(rows.first()).toBeVisible({ timeout: 20_000 });
    const count = await rows.count();
    for (let index = 0; index < count; index += 1) {
        await rows.nth(index).focus();
        await page.keyboard.press('Enter');
        await expect(page).toHaveURL(/tenor-basis-swap\/[^/?]+/, { timeout: 15_000 });
        const draws = await expect(page.getByText(want, { exact: true }).first())
            .toBeVisible({ timeout: 15_000 })
            .then(
                () => true,
                () => false,
            );
        if (draws) {
            const holdsBoth = await Promise.all(
                avoid.map((label) =>
                    page
                        .getByText(label, { exact: true })
                        .first()
                        .isVisible()
                        .catch(() => false),
                ),
            );
            if (!holdsBoth.some(Boolean)) return;
        }
        await page.goBack();
        await expect(rows.first()).toBeVisible({ timeout: 15_000 });
    }
    throw new Error(
        `No convention on the first list page draws '${want}' without any of ${avoid.join(', ')}.`,
    );
}

/** The term row a label names, as the person reads it. */
function termRow(page: Page, label: string) {
    return page.locator('li').filter({ has: page.getByText(label, { exact: true }) }).first();
}

/**
 * Each named term explains itself, in the text the ORE parameter contract holds
 * for it. A term whose entry is missing draws its label with no explanation,
 * which is what this refuses.
 *
 * @p contains is the phrase the documentation of these particular terms must
 * carry. It is how the long and short form's text is told apart from the older
 * wording, so a term that kept the old entry is caught rather than passed.
 */
async function expectDocumented(
    page: Page,
    labels: readonly string[],
    contains: string | undefined,
): Promise<void> {
    await page.getByRole('button', { name: 'Show all documentation' }).click();
    for (const label of labels) {
        const row = termRow(page, label);
        await expect(row, `${label} is drawn`).toBeVisible();
        const explanation = row.locator('p').first();
        await expect(explanation, `${label} explains itself`).toBeVisible();
        const text = (await explanation.innerText()).trim();
        // A bare label is not documentation: the passage says what the term means.
        expect(text.length, `${label} carries a passage, not a bare label`).toBeGreaterThan(20);
        if (contains !== undefined) {
            expect(text, `${label} carries its own wording`).toContain(contains);
        }
    }
}

test.describe('the convention record page', () => {
    test('draws the long and short form, documented, and none of the other form', async ({
        page,
    }) => {
        await openOneFormConvention(page, 'Long leg index', PAY_RECEIVE_ONLY);

        // The five terms below carry the text written for the long and short form.
        await expectDocumented(page, LONG_SHORT_ONLY, 'long and short form');

        // A convention of this form is not told anything about the paying and
        // receiving form's terms: it has never held them.
        for (const label of PAY_RECEIVE_ONLY) {
            await expect(page.getByText(label, { exact: true })).toHaveCount(0);
        }
    });

    test('draws the paying and receiving form, documented, and none of the other form', async ({
        page,
    }) => {
        await openOneFormConvention(page, 'Pay index', LONG_SHORT_ONLY);

        await expectDocumented(page, PAY_RECEIVE_ONLY, undefined);

        for (const label of LONG_SHORT_ONLY) {
            await expect(page.getByText(label, { exact: true })).toHaveCount(0);
        }
    });
});

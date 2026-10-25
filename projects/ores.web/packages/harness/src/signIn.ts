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

import { test as base, expect, type Page } from '@playwright/test';
import { PASSWORD, TENANT_ADMIN, principal, type Person } from './acme.js';

/**
 * Signs in through the form a person uses, on the two fields the sign-in page
 * marks with autocomplete, and chooses the first party when the account works
 * in more than one. It ends on the landing page of a signed-in session.
 */
export async function signIn(page: Page, person: Person): Promise<void> {
    await page.goto('/sign-in');
    await page.locator('input[autocomplete="username"]').fill(principal(person));
    await page.locator('input[autocomplete="current-password"]').fill(PASSWORD);
    await page.getByRole('button', { name: 'Sign in', exact: true }).click();
    const chooser = page.getByRole('heading', { name: 'Choose a party' });
    const signedIn = page.getByRole('button', { name: 'Your account' });
    await expect(chooser.or(signedIn).first()).toBeVisible({ timeout: 15_000 });
    // The expectation above has resolved, so this read does not wait.
    if (await chooser.isVisible()) {
        const choices = page.getByRole('main').getByRole('button');
        await choices.first().click();
    }
    await expect(page).not.toHaveURL(/sign-in/);
}

/** A test with a page already signed in as the tenant administrator. */
export const test = base.extend<{ asAdmin: Page }>({
    asAdmin: async ({ page }, use) => {
        await signIn(page, TENANT_ADMIN);
        await use(page);
    },
});

export { expect };

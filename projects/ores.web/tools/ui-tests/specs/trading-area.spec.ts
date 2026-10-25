import { expect, test } from '@playwright/test';
import { signIn } from './support/sign-in';

/**
 * The Trading area: the menu names the area, and the area's page offers the
 * trade list as a card. A person reaches the list by the card, not by a menu
 * entry of its own.
 */

test('the menu offers Trading and its page offers the trade list', async ({ page }) => {
    await signIn(page);

    await page.getByRole('link', { name: 'Trading', exact: true }).first().click();
    await expect(page).toHaveURL(/\/trading$/);
    await expect(page.getByRole('heading', { name: 'Trading', level: 1 })).toBeVisible();

    await page
        .getByRole('link', { name: /Trades/ })
        .first()
        .click();
    await expect(page).toHaveURL(/\/trading\/trades$/);
    await expect(page.getByRole('heading', { name: 'Trades', level: 1 })).toBeVisible();
});

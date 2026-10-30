import { expect, test } from '@playwright/test';
import { signIn } from './support/sign-in';

/**
 * The trade list names a counterparty and a book, and each name opens that
 * entity's own page. Both pages are new, so this case is also what proves they
 * exist and carry the trade's own counterparty and book.
 */

test('a trade opens its counterparty, its book, and comes back', async ({ page }) => {
    await signIn(page);
    await page.goto('/trading/blotter');
    await page.getByLabel('Flat').check();
    const row = page.locator('tbody tr').first();
    // The flat list's columns: identifier, structure, what, book, counterparty.
    const counterparty = (await row.locator('td').nth(4).innerText()).trim();
    const book = (await row.locator('td').nth(3).innerText()).trim();

    await row.locator('td').nth(4).getByRole('link').click();
    await expect(page).toHaveURL(/\/refdata\/counterparties\/[0-9a-f-]{36}$/);
    await expect(page.getByRole('heading', { name: counterparty, level: 1 })).toBeVisible();

    await page.goBack();
    // Coming back re-renders the screen, so the flat list is chosen again: the
    // columns shift with it.
    await page.getByLabel('Flat').check();
    await expect(page.locator('tbody tr').first()).toBeVisible();
    await page.locator('tbody tr').first().locator('td').nth(3).getByRole('link').click();
    await expect(page).toHaveURL(/\/refdata\/books\/[0-9a-f-]{36}$/);
    await expect(page.getByRole('heading', { name: book, level: 1 })).toBeVisible();
});

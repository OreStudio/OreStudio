import { expect, test } from '@playwright/test';

/**
 * The trade list names a counterparty and a book, and each name opens that
 * entity's own page. Both pages are new, so this case is also what proves they
 * exist and carry the trade's own counterparty and book.
 */
const USERNAME = process.env['ORES_UI_USERNAME'] ?? 'diane.kay@acme_corporation';
const PASSWORD = process.env['ORES_UI_PASSWORD'] ?? '';

async function signIn(page: import('@playwright/test').Page) {
    await page.goto('/');
    await page.getByLabel(/username/i).fill(USERNAME);
    await page.getByLabel(/password/i).fill(PASSWORD);
    await page.getByRole('button', { name: /sign in/i }).click();
    await expect(page).not.toHaveURL(/\/login$/);
}

test('a trade opens its counterparty, its book, and comes back', async ({ page }) => {
    await signIn(page);
    await page.goto('/trading/trades');
    const row = page.locator('tbody tr').first();
    const counterparty = (await row.locator('td').nth(2).innerText()).trim();
    const book = (await row.locator('td').nth(3).innerText()).trim();

    await row.locator('td').nth(2).getByRole('link').click();
    await expect(page).toHaveURL(/\/refdata\/counterparties\/[0-9a-f-]{36}$/);
    await expect(page.getByRole('heading', { name: counterparty, level: 1 })).toBeVisible();

    await page.goBack();
    await expect(page.locator('tbody tr').first()).toBeVisible();
    await page.locator('tbody tr').first().locator('td').nth(3).getByRole('link').click();
    await expect(page).toHaveURL(/\/refdata\/books\/[0-9a-f-]{36}$/);
    await expect(page.getByRole('heading', { name: book, level: 1 })).toBeVisible();
});

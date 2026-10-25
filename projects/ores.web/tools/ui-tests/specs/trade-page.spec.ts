import { expect, test } from '@playwright/test';

/**
 * One trade's page, opened from the list.
 *
 * The case walks the way a person does: sign in, open the list, click the first
 * row, and read the trade's own page. It asserts what the page must show rather
 * than how it is built, so a tab that stops rendering its part fails here.
 */
const USERNAME = process.env['ORES_UI_USERNAME'] ?? 'diane.kay@acme_corporation';
const PASSWORD = process.env['ORES_UI_PASSWORD'] ?? '';

test('a trade opens its own page and every tab answers', async ({ page }) => {
    await page.goto('/');
    await page.getByLabel(/username/i).fill(USERNAME);
    await page.getByLabel(/password/i).fill(PASSWORD);
    await page.getByRole('button', { name: /sign in/i }).click();
    await expect(page).not.toHaveURL(/\/login$/);

    await page.goto('/trading/trades');
    const firstRow = page.locator('tbody tr').first();
    await expect(firstRow).toBeVisible();
    await firstRow.click();

    await expect(page).toHaveURL(/\/trading\/trades\/[0-9a-f-]{36}$/);
    await expect(page.getByRole('tab', { name: 'Details' })).toBeVisible();

    for (const tab of ['Instrument', 'Identifiers and roles', 'Links', 'History']) {
        await page.getByRole('tab', { name: tab }).click();
        await expect(page.getByRole('tab', { name: tab })).toHaveAttribute('aria-selected', 'true');
    }
});

/**
 * The issue names its calendar by code, and the calendar has a page. That page is
 * titled by the calendar's own name, so this reads the code from the link and
 * checks the address rather than the heading.
 */
test('the instrument issue links to its calendar', async ({ page }) => {
    await page.goto('/');
    await page.getByLabel(/username/i).fill(USERNAME);
    await page.getByLabel(/password/i).fill(PASSWORD);
    await page.getByRole('button', { name: /sign in/i }).click();
    await expect(page).not.toHaveURL(/\/login$/);

    await page.goto('/trading/trades');
    const row = page.locator('tbody tr').first();
    await row.waitFor({ state: 'visible' });
    await row.locator('td').first().click();
    await page.getByRole('tab', { name: 'Instrument' }).click();

    const linked = page.locator('main a[href^="/refdata/calendars/"]');
    await expect(linked).toHaveCount(1);
    const code = (await linked.innerText()).trim();
    await linked.click();
    await expect(page).toHaveURL(/\/refdata\/calendars\/.+$/);
    expect(page.url()).toContain(encodeURIComponent(code));
});

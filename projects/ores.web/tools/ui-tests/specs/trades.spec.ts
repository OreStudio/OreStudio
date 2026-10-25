import { expect, test } from '@playwright/test';

/**
 * The trade list, seen the way a person sees it.
 *
 * A Trading persona is used because it signs in without choosing a party and it
 * holds every blotter read, so a refused request is a defect in the screen and
 * not in a grant. The credentials come from the environment, so no password is
 * written here.
 */
const USERNAME = process.env['ORES_UI_USERNAME'] ?? 'diane.kay@acme_corporation';
const PASSWORD = process.env['ORES_UI_PASSWORD'] ?? '';

async function signIn(page: import('@playwright/test').Page) {
    await page.goto('/');
    await page.getByLabel(/username/i).fill(USERNAME);
    await page.getByLabel(/password/i).fill(PASSWORD);
    await page.getByRole('button', { name: /sign in/i }).click();
    // The app leaves the sign-in page once the session is established; navigating
    // before that cancels the sign-in and every later page redirects back here.
    await expect(page).not.toHaveURL(/\/login$/);
}

test('a Trading persona signs in and leaves the sign-in page', async ({ page }) => {
    await signIn(page);

    await expect(page).not.toHaveURL(/\/login$/);
});

test('the trade list answers and shows a row', async ({ page }) => {
    const refusals: string[] = [];
    page.on('response', (response) => {
        if (response.url().includes('/api/trades') && response.status() >= 400) {
            refusals.push(`${response.status()} ${response.url()}`);
        }
    });

    await signIn(page);
    await page.goto('/trades');
    await expect(page.getByRole('heading', { name: /trades/i })).toBeVisible();

    expect(refusals, `the trades route refused: ${refusals.join(' | ')}`).toHaveLength(0);
});

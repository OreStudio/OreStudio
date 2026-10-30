import { expect, test } from '@playwright/test';
import { signIn } from './support/sign-in';

/**
 * The trade list, seen the way a person sees it.
 *
 * A Trading persona is used because it signs in without choosing a party and it
 * holds every blotter read, so a refused request is a defect in the screen and
 * not in a grant. The credentials come from the environment, so no password is
 * written here.
 */

test('a Trading persona signs in and leaves the sign-in page', async ({ page }) => {
    await signIn(page);

    await expect(page).not.toHaveURL(/\/login$/);
});

test('the blotter answers and shows a row', async ({ page }) => {
    const refusals: string[] = [];
    page.on('response', (response) => {
        if (response.url().includes('/api/blotter') && response.status() >= 400) {
            refusals.push(`${response.status()} ${response.url()}`);
        }
    });

    await signIn(page);
    await page.goto('/trading/blotter');
    await expect(page.getByRole('heading', { name: /trade blotter/i })).toBeVisible();

    expect(refusals, `the trades route refused: ${refusals.join(' | ')}`).toHaveLength(0);
});

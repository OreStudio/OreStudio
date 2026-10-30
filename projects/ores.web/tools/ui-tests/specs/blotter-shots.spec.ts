import { test } from '@playwright/test';
import { signIn } from './support/sign-in';

/** Screenshots of the blotter, for review: not an assertion case. */
test('shots of the blotter', async ({ page }) => {
    await signIn(page);
    await page.goto('/trading/blotter');
    const rail = page.getByRole('navigation', { name: 'rail' });
    await rail.getByRole('button').first().waitFor({ state: 'visible', timeout: 20_000 });
    await page.locator('tbody tr a').first().waitFor({ state: 'visible', timeout: 20_000 });
    await page.screenshot({ path: 'shots/blotter-1-portfolio.png', fullPage: true });

    await rail.getByRole('button').nth(2).click();
    await page.locator('tbody tr a').first().waitFor({ state: 'visible', timeout: 20_000 });
    await page.screenshot({ path: 'shots/blotter-2-book.png', fullPage: true });

    await page.getByLabel('Flat').check();
    await page.getByRole('cell', { name: 'Structure', exact: true }).waitFor({ state: 'visible', timeout: 20_000 });
    await page.locator('tbody tr a').first().waitFor({ state: 'visible', timeout: 20_000 });
    await page.screenshot({ path: 'shots/blotter-3-flat.png', fullPage: true });

    await page.getByLabel('Grouping').selectOption('counterparty');
    await rail.getByRole('button').nth(1).waitFor({ state: 'visible', timeout: 20_000 });
    await page.locator('tbody tr a').first().waitFor({ state: 'visible', timeout: 20_000 });
    await page.screenshot({ path: 'shots/blotter-4-counterparty.png', fullPage: true });
});

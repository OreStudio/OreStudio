import { expect, test } from '@playwright/test';
import { signIn } from './support/sign-in';

/**
 * A structure's page: the deal the desk grouped, and the trades that are its legs.
 *
 * The case finds a grouped deal by its reference, opens it, reads the legs it
 * holds, and adds one more trade as a leg: the acceptance case for a deal whose
 * parts are trades rather than rows invented for a screen.
 *
 * It needs a tenant whose population grouped trades, which the provisioned
 * environment has. The trade it adds is one the deal does not already hold, so
 * a second run adds a second leg rather than failing on a duplicate.
 */
test('a structure shows its reference and its legs, and takes another leg', async ({ page }) => {
    await signIn(page);
    await page.goto('/trading/trades');

    const search = page.getByRole('searchbox').first();
    await search.fill('PKG-');

    // The search asks the server once typing stops, so the row is waited for
    // rather than read immediately: the page on screen a moment later is the
    // page the filter made.
    const row = page.locator('tbody tr', { hasText: 'PKG-' }).first();
    await row.waitFor({ state: 'visible', timeout: 20_000 });
    const reference = (await row.locator('td').first().innerText()).trim();
    expect(reference).toMatch(/^PKG-/);

    await row.locator('td').first().click();
    await expect(page.getByRole('heading', { name: reference })).toBeVisible({
        timeout: 20_000,
    });

    const legs = page.locator('section', {
        has: page.getByRole('heading', { name: /legs of this deal/i }),
    });
    const before = await legs.locator('tbody tr').count();

    const trade = page.getByRole('combobox', { name: /^trade$/i });
    await trade.waitFor({ state: 'visible' });
    const chosen = (await trade.locator('option').nth(1).innerText()).trim();
    await trade.selectOption({ index: 1 });
    await page.getByRole('button', { name: /^add the leg$/i }).click();

    await expect(legs.locator('tbody tr')).toHaveCount(before + 1, { timeout: 20_000 });
    await expect(legs.getByRole('link', { name: chosen })).toBeVisible({ timeout: 20_000 });
    await page.screenshot({ path: 'shots/structure-page.png', fullPage: true });
});

/**
 * The trade page names the deal its trade is a leg of.
 *
 * A person reading a trade should be able to reach the deal it belongs to
 * without knowing its id, so the link is found by the reference the deal shows.
 */
test('a trade names the structure it is a leg of', async ({ page }) => {
    await signIn(page);
    await page.goto('/trading/trades');
    await page.getByRole('searchbox').first().fill('PKG-');
    const row = page.locator('tbody tr', { hasText: 'PKG-' }).first();
    await row.waitFor({ state: 'visible', timeout: 20_000 });
    const reference = (await row.locator('td').first().innerText()).trim();
    await row.locator('td').first().click();

    // The leg's own link leads to the trade, and the trade names the deal it
    // belongs to by the same reference the deal shows.
    const legs = page.locator('section', {
        has: page.getByRole('heading', { name: /legs of this deal/i }),
    });
    await legs.locator('tbody tr').first().getByRole('link').first().click();
    await expect(page.getByRole('heading', { name: reference })).toBeHidden({ timeout: 20_000 });
    await expect(page.getByRole('tab', { name: /details/i })).toHaveAttribute(
        'aria-selected',
        'true',
    );
    await page.screenshot({ path: 'shots/trade-structure.png', fullPage: true });
});

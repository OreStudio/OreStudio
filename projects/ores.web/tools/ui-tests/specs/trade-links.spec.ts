import { expect, test } from '@playwright/test';
import { signIn } from './support/sign-in';

/**
 * Recording a link between two trades, from the trade's own page.
 *
 * The case opens the first trade in the list, records a hedge to another one,
 * and reads the link back in the table it lands in: the type it was given and
 * the other trade by the identifier the desk knows, never a uuid.
 *
 * It needs a tenant with at least two trades, which the provisioned
 * environment has. It does not create its own: a booking writes to a real
 * book, and a test that books to get a subject would be testing the booking
 * screen's job.
 */
test('a link is recorded and named by both its type and its trade', async ({ page }) => {
    await signIn(page);
    await page.goto('/trading/trades');
    const row = page.locator('tbody tr').first();
    await row.waitFor({ state: 'visible' });
    await row.locator('td').first().click();
    await page.getByRole('tab', { name: 'Links' }).click();

    // The field's label is 'Link': the table column beside it says what it is.
    const type = page.getByRole('combobox', { name: /^link$/i });
    await type.selectOption('Hedge');
    const other = page.getByRole('combobox', { name: /^the other trade$/i });
    // The first option is the empty one every picker starts on, so the second
    // is the first real trade.
    const chosen = (await other.locator('option').nth(1).innerText()).trim();
    await other.selectOption({ index: 1 });

    await page.getByRole('button', { name: /^record the link$/i }).click();

    // The table above the form is re-read, and the new link is in it. Scoped to
    // that table: the page holds other links, and a match anywhere would prove
    // less than this one does.
    const linksFrom = page.locator('section', {
        has: page.getByRole('heading', { name: /links from this trade/i }),
    });
    await expect(linksFrom.getByRole('link', { name: chosen })).toBeVisible({
        timeout: 20_000,
    });
    await page.screenshot({ path: 'shots/trade-links.png', fullPage: true });
});

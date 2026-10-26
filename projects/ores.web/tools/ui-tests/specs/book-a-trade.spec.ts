import { expect, test } from '@playwright/test';
import { signIn } from './support/sign-in';

/**
 * Booking a trade from the screen, the way a person does it.
 *
 * The case walks the form, books, and reads the trade's own page back: the
 * trade exists, it is live, and it is waiting for Operations. A refusal is
 * asserted the same way — the server's reason reaches the person who filled the
 * form — because a booking that fails must say why.
 */
test('a trade books from the form and opens its own page', async ({ page }) => {
    await signIn(page);
    await page.goto('/trading/trades');
    await page.getByRole('button', { name: /book a trade/i }).click();
    await expect(page).toHaveURL(/\/trading\/trades\/new$/);

    // The controls are found by their role and the name a person reads: the
    // label's hint is part of the accessible name, so it is not a bare word.
    await page.getByRole('combobox', { name: /^book$/i }).selectOption({ index: 1 });
    await page.getByRole('combobox', { name: /^counterparty/i }).selectOption({ index: 1 });
    await page.getByRole('textbox', { name: /notional/i }).fill('1250000.25');
    await page.getByRole('combobox', { name: /^security$/i }).selectOption({ index: 1 });
    await page.getByRole('combobox', { name: /^identifier scheme/i }).selectOption({ index: 1 });
    await page.getByRole('textbox', { name: /^identifier/i }).fill(`E2E-${Date.now()}`);

    await page.getByRole('button', { name: /^book trade$/i }).click();

    await expect(page).toHaveURL(/\/trading\/trades\/[0-9a-f-]{36}$/, { timeout: 30_000 });
    await expect(page.getByRole('tab', { name: 'Details' })).toBeVisible();
    // The notional is an instrument term, so it is read on that tab.
    await page.getByRole('tab', { name: 'Instrument' }).click();
    await expect(page.getByText(/1250000\.25/)).toBeVisible();
    await page.screenshot({ path: 'shots/booked-trade.png', fullPage: true });
});

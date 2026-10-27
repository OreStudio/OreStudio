import { expect, type Page } from '@playwright/test';

/** The person every case acts as. */
export const USERNAME = process.env['ORES_UI_USERNAME'] ?? 'diane.kay@acme_corporation';
export const PASSWORD = process.env['ORES_UI_PASSWORD'] ?? '';

/**
 * Signs in and waits until the browser has left the sign-in screen.
 *
 * An account that works in more than one party answers the sign-in with the
 * party picker, which stands on the same screen and would otherwise never be
 * left. The picker is chosen from when it appears; an account that works in one
 * party goes straight through.
 *
 * The wait is long on purpose: the first sign-in after a provisioning run
 * answers slowly, because the session's own reads warm up behind it, and a
 * five-second expectation that the app has moved on fails a working login.
 */
export async function signIn(
    page: Page,
    options: { readonly party?: RegExp | string } = {},
): Promise<void> {
    await page.goto('/');
    await page.getByLabel(/username/i).fill(USERNAME);
    await page.getByLabel(/password/i).fill(PASSWORD);
    await page.getByRole('button', { name: /sign in/i }).click();

    const chooser = page.getByRole('heading', { name: /choose a party/i });
    const away = page.waitForURL((url) => !url.pathname.endsWith('/login'), { timeout: 60_000 });
    // A wait that loses the race is not a failure: the other one answered.
    away.catch(() => undefined);
    const answered = await Promise.race([
        away.then(() => 'signed in' as const),
        chooser
            .first()
            .waitFor({ timeout: 60_000 })
            .then(() => 'party' as const)
            .catch(() => 'signed in' as const),
    ]);
    if (answered === 'party') {
        await page
            .getByRole('button', { name: options.party ?? /./ })
            .first()
            .click();
        await page.waitForURL((url) => !url.pathname.endsWith('/login'), { timeout: 60_000 });
    }
    await expect(page).not.toHaveURL(/\/login$/);
}

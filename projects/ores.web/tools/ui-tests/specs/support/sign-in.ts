import { expect, type Page } from '@playwright/test';

/** The person every case acts as. */
export const USERNAME = process.env['ORES_UI_USERNAME'] ?? 'diane.kay@acme_corporation';
export const PASSWORD = process.env['ORES_UI_PASSWORD'] ?? '';

/**
 * Signs in and waits until the browser has left the sign-in screen.
 *
 * The wait is long on purpose: the first sign-in after a provisioning run
 * answers slowly, because the session's own reads warm up behind it, and a
 * five-second expectation that the app has moved on fails a working login.
 */
export async function signIn(page: Page): Promise<void> {
    await page.goto('/');
    await page.getByLabel(/username/i).fill(USERNAME);
    await page.getByLabel(/password/i).fill(PASSWORD);
    await page.getByRole('button', { name: /sign in/i }).click();
    await page.waitForURL((url) => !url.pathname.endsWith('/login'), { timeout: 60_000 });
    await expect(page).not.toHaveURL(/\/login$/);
}

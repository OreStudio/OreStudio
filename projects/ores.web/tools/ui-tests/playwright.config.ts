import { defineConfig, devices } from '@playwright/test';

/**
 * The browser tests of a running ORE Studio web.
 *
 * These are development instruments, not a gate: nothing installs this package
 * in CI, and no pipeline runs it. They exist so a change to the web client can
 * be seen working against a real fleet instead of being inferred.
 *
 * The address comes from ORES_UI_BASE_URL, so the same suite serves any
 * environment's port. The plain tests are serial because they share one session.
 */
export default defineConfig({
    testDir: './specs',
    fullyParallel: false,
    workers: 1,
    retries: 0,
    reporter: [['list']],
    use: {
        baseURL: process.env['ORES_UI_BASE_URL'] ?? 'http://127.0.0.1:20802',
        trace: 'retain-on-failure',
        screenshot: 'only-on-failure',
    },
    projects: [{ name: 'chromium', use: { ...devices['Desktop Chrome'] } }],
});

import { defineConfig, devices } from '@playwright/test';

/**
 * The browser tests, pointed at a browser this sandbox already holds.
 *
 * Playwright 1.56 wants its own Chromium revision, and the home directory is
 * read-only here, so the run cannot install one. The executable below is the
 * revision the checkout's cache already carries; the specs and the projects
 * are the repository's, and only the launch path differs.
 */
const EXECUTABLE =
    '/home/marco/.cache/ms-playwright/chromium_headless_shell-1248/chrome-headless-shell-linux64/chrome-headless-shell';

export default defineConfig({
    testDir: './specs',
    fullyParallel: false,
    workers: 1,
    retries: 0,
    reporter: [['list']],
    use: {
        baseURL: process.env['ORES_UI_BASE_URL'] ?? 'http://127.0.0.1:21002',
        trace: 'retain-on-failure',
        screenshot: 'only-on-failure',
        launchOptions: { executablePath: EXECUTABLE },
    },
    projects: [{ name: 'chromium', use: { ...devices['Desktop Chrome'] } }],
});

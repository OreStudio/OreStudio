/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

/**
 * Drives a real browser against the running stack.
 *
 * The gate the unit tests cannot be: it proves the landing page, the sign-in
 * screen, the session cookie and the accounts table work together in a browser,
 * and it captures screenshots as evidence.
 *
 * Prerequisites, in order:
 *   scripts/dev-stack.sh start
 *   scripts/seed-test-account.sh
 *
 * Run:
 *   npx tsx scripts/verify-browser.ts
 */

import { mkdirSync, readFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { chromium, type Page } from 'playwright';

const SCRIPT_DIR = dirname(fileURLToPath(import.meta.url));
const REPO_ROOT = resolve(SCRIPT_DIR, '..', '..', '..');
const ENV_FILE = resolve(REPO_ROOT, '.env');
const SITE_CONFIG_FILE = resolve(SCRIPT_DIR, '..', 'config', 'environments.json');

// The checkout's environment file is the authority. A variable already in the
// process environment wins, as it does for the BFF started with --env-file.
try {
  process.loadEnvFile(ENV_FILE);
} catch {
  // A checkout without one falls back to the defaults below.
}

const SITE_ENVIRONMENTS = readSiteEnvironments();
const ENVIRONMENT = expectedEnvironment();
// The deployment page lists every environment the site configuration names, so
// the identifier asserted below follows that file rather than one checkout.
const OTHER_ENVIRONMENT_ID =
  SITE_ENVIRONMENTS.find((entry) => entry.id !== ENVIRONMENT.id)?.id ?? ENVIRONMENT.id;
const WEB_PORT = setting('ORES_WEB_PORT') ?? '8080';
const APP_URL = process.env['ORES_WEB_APP_URL'] ?? `http://127.0.0.1:${WEB_PORT}/`;
const USERNAME = process.env['ORES_PRINCIPAL'] ?? 'ores_web_probe';
const PASSWORD = process.env['ORES_PASSWORD'] ?? 'Secure-Password-123';
const SHOT_DIR = '.runtime/screenshots';

interface EnvironmentExpectation {
  readonly id: string;
  readonly displayName: string;
  readonly port: number;
  readonly subjectPrefix: string;
}

/** Reads a variable, treating whitespace as absent. */
function setting(name: string): string | undefined {
  const value = process.env[name];
  return value !== undefined && value.trim().length > 0 ? value : undefined;
}

/** Reads the environments the site configuration names. */
function readSiteEnvironments(): EnvironmentExpectation[] {
  let text: string;
  try {
    text = readFileSync(SITE_CONFIG_FILE, 'utf8');
  } catch {
    return [];
  }
  let parsed: unknown;
  try {
    parsed = JSON.parse(text) as unknown;
  } catch {
    return [];
  }
  if (typeof parsed !== 'object' || parsed === null) {
    return [];
  }
  const environments = (parsed as { readonly environments?: unknown }).environments;
  if (!Array.isArray(environments)) {
    return [];
  }
  const result: EnvironmentExpectation[] = [];
  for (const item of environments) {
    if (typeof item !== 'object' || item === null) {
      continue;
    }
    const record = item as Record<string, unknown>;
    const id = record['id'];
    const displayName = record['displayName'];
    const port = record['port'];
    const subjectPrefix = record['subjectPrefix'];
    if (
      typeof id === 'string' &&
      typeof displayName === 'string' &&
      typeof port === 'number' &&
      typeof subjectPrefix === 'string'
    ) {
      result.push({ id, displayName, port, subjectPrefix });
    }
  }
  return result;
}

/** The environment the BFF serves, from its own variables then the site file. */
function expectedEnvironment(): EnvironmentExpectation {
  const requested = (process.env['ORES_WEB_ENV'] ?? setting('ORES_ENV_NAME') ?? '')
    .trim()
    .replaceAll('-', '_');
  const chosen = SITE_ENVIRONMENTS.find((entry) => entry.id === requested);
  if (chosen !== undefined) {
    return chosen;
  }
  return {
    id: requested,
    displayName: displayNameFor(requested),
    port: Number(setting('ORES_NATS_PORT') ?? '4222'),
    subjectPrefix: setting('ORES_NATS_SUBJECT_PREFIX') ?? '',
  };
}

/** Turns an environment identifier into the display name the file uses. */
function displayNameFor(id: string): string {
  return id
    .split('_')
    .filter((part) => part.length > 0)
    .map((part) => part.charAt(0).toUpperCase() + part.slice(1))
    .join(' ');
}

let failures = 0;

function check(label: string, condition: boolean, detail = ''): void {
  if (!condition) {
    failures += 1;
  }
  console.log(`  [${condition ? 'PASS' : 'FAIL'}] ${label}${detail.length > 0 ? ` (${detail})` : ''}`);
}

async function screenshot(page: Page, name: string): Promise<void> {
  mkdirSync(SHOT_DIR, { recursive: true });
  await page.screenshot({ path: `${SHOT_DIR}/${name}.png`, fullPage: true });
}

async function main(): Promise<number> {
  const browser = await chromium.launch().catch(() => chromium.launch({ channel: 'chrome' }));
  const context = await browser.newContext({ viewport: { width: 1440, height: 1000 } });
  const page = await context.newPage();

  const pageErrors: string[] = [];
  const consoleErrors: string[] = [];
  page.on('pageerror', (error) => pageErrors.push(error.message));
  page.on('console', (message) => {
    if (message.type() === 'error' && !message.text().includes('Failed to load resource')) {
      consoleErrors.push(message.text());
    }
  });

  console.log('\nthe landing page:');
  await page.goto(APP_URL, { waitUntil: 'load' });
  await page.waitForSelector('h1', { timeout: 15_000 });
  const landing = (await page.textContent('body')) ?? '';
  check('a landing page is the entry point', landing.includes('Enterprise-grade risk analytics'));
  check('the hero artwork is shown', await page.isVisible('figure img'));
  check('there is no prose blurb', !landing.includes('What is here'));
  check('no session is open yet', (await context.cookies()).every((c) => c.name !== 'ores_web_session'));
  await screenshot(page, '30-landing');

  console.log('\nthe header and footer:');
  check('the site link is offered', await page.isVisible('a[href="https://orestudio.github.io/OreStudio/"]'));
  check('a sign in button is offered', await page.isVisible('a[href="/login"]'));
  // Before signing in, the deployment is not offered at all.
  check('the Deployment option is not offered', (await page.locator('a[href="/deployment"]').count()) === 0);
  check('the sign up call to action is on the page', await page.isVisible('a[href="/signup"]'));
  // The environment is a small permanent marker rather than a field, so it
  // belongs in the footer beside the copyright.
  const footer = (await page.textContent('footer')) ?? '';
  check('the environment is a footer marker', footer.includes(ENVIRONMENT.displayName), footer.trim());
  check('the footer carries the copyright', footer.includes('©'));
  check('it marks a development environment', footer.includes('development'));

  console.log('\nthe deployment page is behind sign-in:');
  await page.goto(`${APP_URL}deployment`, { waitUntil: 'load' });
  await page.waitForSelector('input[name="username"]', { timeout: 15_000 });
  check('it redirects to the sign-in screen', await page.isVisible('input[name="username"]'));

  console.log('\nnothing about connections is offered:');
  for (const gone of ['Connections', 'Import', 'Export', 'Master password', 'Quick connect']) {
    check(`'${gone}' does not appear in the landing page`, !landing.includes(gone));
  }

  console.log('\nthe sign-in screen:');
  await page.goto(`${APP_URL}login`, { waitUntil: 'load' });
  await page.waitForSelector('input[name="username"]', { timeout: 15_000 });
  const signIn = (await page.textContent('body')) ?? '';
  check('the username field is present', await page.isVisible('input[name="username"]'));
  check(
    'the password field is masked',
    (await page.getAttribute('input[name="password"]', 'type')) === 'password',
  );
  check('the environment is named', signIn.includes(ENVIRONMENT.displayName));
  check('there is no server field', (await page.locator('input[name="server"]').count()) === 0);
  check('there is no namespace field', !signIn.includes('Namespace'));
  check('there is no connection chooser', !signIn.includes('Quick connect'));
  check('there is no master password prompt', !signIn.includes('Master password'));
  check('there is no splash banner', (await page.locator('.signin__banner').count()) === 0);

  // Show password is a toggle acting on the field rather than a checkbox.
  await page.click('button:has-text("Show")');
  check(
    'show password reveals the field',
    (await page.getAttribute('input[name="password"]', 'type')) === 'text',
  );
  await page.click('button:has-text("Hide")');
  await screenshot(page, '31-signin');

  console.log('\nrejected credentials:');
  await page.fill('input[name="username"]', USERNAME);
  await page.fill('input[name="password"]', 'definitely-the-wrong-password');
  await page.click('button[type="submit"]');
  await page.waitForSelector('[role="alert"]', { timeout: 20_000 });
  check(
    'the server message is shown',
    ((await page.textContent('[role="alert"]')) ?? '').trim().length > 0,
  );
  check('no session was established', (await context.cookies()).every((c) => c.name !== 'ores_web_session'));

  console.log('\nsigning in:');
  await page.fill('input[name="password"]', PASSWORD);
  await page.click('button[type="submit"]');
  // The table element mounts while the query is in flight, so wait for a row.
  await page.waitForSelector('tbody tr', { timeout: 25_000 });
  check('the accounts table rendered', await page.isVisible('table'));

  const cookie = (await context.cookies()).find((c) => c.name === 'ores_web_session');
  check('a session cookie was set', cookie !== undefined);
  check('the session cookie is HttpOnly', cookie?.httpOnly === true);

  const rowCount = await page.locator('tbody tr').count();
  check('accounts were loaded', rowCount > 0, `${rowCount} rows`);
  const accounts = (await page.textContent('body')) ?? '';
  check('the signed-in user is shown', accounts.includes(USERNAME));
  check('no credential field leaked into the page', !accounts.toLowerCase().includes('password_hash'));
  await screenshot(page, '32-accounts');

  console.log('\nthe deployment page, once signed in:');
  await page.goto(`${APP_URL}deployment`, { waitUntil: 'load' });
  await page.waitForSelector('h1', { timeout: 15_000 });
  const deployment = (await page.textContent('body')) ?? '';
  check('it names the environment', deployment.includes(ENVIRONMENT.displayName));
  check('it shows where it points', deployment.includes(String(ENVIRONMENT.port)), String(ENVIRONMENT.port));
  check('it shows the namespace', deployment.includes(ENVIRONMENT.subjectPrefix), ENVIRONMENT.subjectPrefix);
  check('it shows the configuration file', deployment.includes('environments.json'));
  // The identifiers are the point of that table, since they are what you type.
  check('it lists the other environments by identifier', deployment.includes(OTHER_ENVIRONMENT_ID));
  await screenshot(page, '33-deployment');


  console.log('\nsigning out:');
  await page.click('button:has-text("Sign out")');
  await page.waitForSelector('a[href="/login"]', { timeout: 15_000 });
  check('the sign in link is back', await page.isVisible('a[href="/login"]'));
  const after = (await context.cookies()).find((c) => c.name === 'ores_web_session');
  check('the session cookie was cleared', after === undefined || after.value === '');

  console.log('\nbrowser console:');
  check('no uncaught exception was thrown', pageErrors.length === 0, pageErrors.slice(0, 3).join(' | '));
  check('no unexpected console error was logged', consoleErrors.length === 0, consoleErrors.slice(0, 3).join(' | '));

  await browser.close();
  console.log(`\n${failures === 0 ? 'ALL CHECKS PASSED' : `${failures} CHECK(S) FAILED`}`);
  console.log(`screenshots: ${SHOT_DIR}/`);
  return failures === 0 ? 0 : 1;
}

main().then(
  (code) => process.exit(code),
  (error: unknown) => {
    console.error('\nverification aborted:', error);
    process.exit(1);
  },
);

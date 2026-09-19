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
 * The country entity, driven in a real browser.
 *
 * Split into a read path and a write path, because they depend on different
 * things. Reading needs the services up. Writing needs those, plus an account
 * that was properly provisioned with roles: a hand-seeded account has no role
 * claims in its token, and the service refuses every write as though the token
 * had expired, which is a misleading thing to debug.
 *
 * The write half therefore only runs when asked for, with --write, so that the
 * read half stays useful on a system that is not bootstrapped.
 *
 *   npx tsx scripts/verify-country.ts
 *   npx tsx scripts/verify-country.ts --write
 */
import { mkdirSync } from 'node:fs';
import { chromium, type Page } from 'playwright';

/*
 * Navigation waits for `load` and not `load`. The signed-in shell holds an
 * open event stream so the network is never quiet again, and a wait for quiet
 * waits forever.
 */
const APP = process.env['ORES_WEB_APP_URL'] ?? 'http://127.0.0.1:21802/';
const USERNAME = process.env['ORES_WEB_USER'] ?? 'ores_web_probe';
const PASSWORD = process.env['ORES_WEB_PASSWORD'] ?? 'Secure-Password-123';
const SHOTS = '.runtime/screenshots';
const WRITE = process.argv.includes('--write');

let passed = 0;
let failed = 0;

function check(label: string, ok: boolean, detail?: string): void {
  if (ok) {
    passed += 1;
    console.log(`  [PASS] ${label}`);
  } else {
    failed += 1;
    console.log(`  [FAIL] ${label}${detail === undefined ? '' : ` — ${detail}`}`);
  }
}

async function shot(page: Page, name: string): Promise<void> {
  await page.screenshot({ path: `${SHOTS}/${name}.png` });
}

async function signIn(page: Page): Promise<void> {
  await page.goto(`${APP}login`, { waitUntil: 'load' });
  await page.fill('input[name="username"]', USERNAME);
  await page.fill('input[name="password"]', PASSWORD);
  await page.click('button[type="submit"]');

  /*
   * An account that works in more than one party chooses one before the
   * interface opens. The accounts that own the data need this, so the script
   * handles it rather than requiring an account that does not.
   */
  await page
    .waitForSelector('button:has-text("Operational")', { timeout: 8_000 })
    .then(async () => {
      await page.locator('button:has-text("Operational")').first().click();
      await page.waitForTimeout(2_500);
    })
    .catch(() => undefined);

  await page.waitForSelector('h1', { timeout: 25_000 });
}

/** Switches the interface language through the control a person would use. */
async function language(page: Page, englishName: string): Promise<void> {
  await page.locator('header button[aria-haspopup="listbox"]').click();
  await page.waitForTimeout(300);
  await page.locator('ul[role="listbox"] button', { hasText: englishName }).click();
  await page.waitForTimeout(800);
}

const browser = await chromium.launch().catch(() => chromium.launch({ channel: 'chrome' }));
const context = await browser.newContext({ viewport: { width: 1440, height: 950 } });
const page = await context.newPage();
mkdirSync(SHOTS, { recursive: true });

const consoleErrors: string[] = [];
page.on('pageerror', (error) => consoleErrors.push(String(error)));

/*
 * A refused request is only an error if it was not expected.
 *
 * Reading the console text is not enough, because the text of a failed resource
 * carries no URL. So the expected refusal is recognised where the URL is known —
 * the session probe before sign-in — and everything else that fails is reported
 * with its URL, which is what makes a failure actionable.
 */
const expectedRefusals = ['/api/session'];
const failedRequests: string[] = [];
page.on('response', (response) => {
  if (response.status() < 400) return;
  const url = response.url();
  if (expectedRefusals.some((path) => url.includes(path))) return;
  failedRequests.push(`${response.status()} ${url}`);
});
page.on('console', (message) => {
  if (message.type() !== 'error') return;
  const text = message.text();
  // Resource failures are reported from the response listener, with the URL.
  if (text.includes('Failed to load resource')) return;
  consoleErrors.push(text);
});

try {
  await signIn(page);
  await page.waitForTimeout(600);
  await shot(page, '70-home');

  console.log('\nthe shell:');
  check('the sidebar lists the components', (await page.locator('aside nav > div').count()) >= 8);

  console.log('\nthe country list:');
  await page.goto(`${APP}refdata/country`, { waitUntil: 'load' });
  await page.waitForSelector('tbody tr', { timeout: 25_000 });
  await page.waitForTimeout(600);

  const rows = await page.locator('tbody tr').count();
  check('records are listed', rows > 0, `${rows} rows`);
  check('the default page size is 25', rows <= 25, `${rows} rows`);

  const heading = ((await page.textContent('h1')) ?? '').trim();
  check('the heading names the collection', heading === 'Countries', heading);

  // Flags are images reached through the BFF, so a broken one is a broken route
  // rather than a missing field.
  const flags = page.locator('tbody img[src^="/api/images/"]');
  const flagCount = await flags.count();
  check('flags are rendered', flagCount > 0, `${flagCount} on this page`);
  if (flagCount > 0) {
    const loaded = await flags.first().evaluate(
      (image) => (image as HTMLImageElement).naturalWidth > 0,
    );
    check('a flag actually loads', loaded);
  }
  await shot(page, '71-country-list');

  console.log('\nsearch and paging:');
  const search = page.locator('input[type="search"]');
  check('there is a search box', (await search.count()) === 1);
  await search.fill('Brazil');
  await page.waitForTimeout(400);
  const filtered = await page.locator('tbody tr').count();
  check('search narrows the list', filtered > 0 && filtered < rows, `${filtered} of ${rows}`);
  await search.fill('');
  await page.waitForTimeout(300);

  console.log('\nthe country detail:');
  await page.locator('tbody tr').first().click();
  await page.waitForSelector('h1', { timeout: 15_000 });
  await page.waitForTimeout(500);
  const detail = ((await page.textContent('h1')) ?? '').trim();
  check('the record opens', detail.length > 0, detail);
  // A field showing its placeholder for a record that has loaded is the bug that
  // is easiest to ship, so it is asserted rather than eyeballed.
  const alpha2 = await page.locator('input#alpha2_code').inputValue();
  check('the form is filled in, not showing placeholders', alpha2.length > 0, `alpha2="${alpha2}"`);

  /*
   * Provenance is asserted on its labels and its values rather than on a
   * particular reason code. The reasons are data, so a check that depends on one
   * of them tests the seed rather than the screen.
   */
  await page.locator('button[role="tab"]', { hasText: /Provenance/i }).click();
  await page.locator('dt').first().waitFor({ timeout: 10_000 });
  const labels = await page.locator('dt').allTextContents();
  const values = await page.locator('dd').allTextContents();
  check(
    'provenance lists the audit fields',
    labels.some((l) => /version/i.test(l)) && labels.some((l) => /modified by/i.test(l)),
    labels.join(' / ').slice(0, 80),
  );
  check(
    'provenance has values rather than blanks',
    values.some((v) => v.trim().length > 0 && !/not recorded/i.test(v)),
    values.join(' / ').slice(0, 80),
  );
  await shot(page, '72-country-detail');

  /*
   * Getting back has to be possible without the browser button.
   *
   * A person who follows a link into a record and cannot find the list again is
   * a person who stops following links, and this is the screen where that
   * happened: the entity was the last crumb and the last crumb was plain text.
   */
  console.log('\ngetting back to the list:');
  await page.locator('nav[aria-label="Breadcrumb"] a', { hasText: /Country/i }).first().click();
  await page.waitForLoadState('load');
  await page.waitForTimeout(500);
  check('a record leads back to the list', page.url().endsWith('/refdata/country'), page.url());

  console.log('\nthe history:\n');
  await page.goto(`${APP}refdata/country/AR/history`, { waitUntil: 'load' });
  await page.waitForTimeout(1200);
  const history = (await page.textContent('body')) ?? '';
  check('the history screen opens', history.length > 0);
  /*
   * Titled by the record, not by the entity's singular. The singular exists to
   * sit inside a sentence and is deliberately lower case, so using it as a
   * heading produced "country AR" — a fragment and an identifier where a person
   * looks to see what they are looking at.
   */
  const historyTitle = ((await page.locator('h1').first().textContent()) ?? '').trim();
  check('the history is titled by the record', historyTitle === 'Argentina', historyTitle);
  await shot(page, '73-country-history');

  // And from the history, back to the record, and on to the list. The record is
  // named rather than identified, so the link reads Argentina and not AR.
  const trail = await page.locator('nav[aria-label="Breadcrumb"]').textContent();
  check('the breadcrumb names the record', /Argentina/.test(trail ?? ''), (trail ?? '').trim());
  await page.locator('nav[aria-label="Breadcrumb"] a', { hasText: /Argentina/ }).first().click();
  await page.waitForLoadState('load');
  await page.waitForTimeout(500);
  check('the history leads back to the record', page.url().endsWith('/refdata/country/AR'), page.url());
  await page.locator('nav[aria-label="Breadcrumb"] a', { hasText: /Country/i }).first().click();
  await page.waitForLoadState('load');
  await page.waitForTimeout(500);
  check('the record leads back to the list again', page.url().endsWith('/refdata/country'), page.url());

  console.log('\nthe three languages:');
  for (const [englishName, expected] of [
    ['Portuguese', 'Países'],
    ['French', 'Pays'],
    ['English', 'Countries'],
  ] as const) {
    await language(page, englishName);
    await page.goto(`${APP}refdata/country`, { waitUntil: 'load' });
    await page.waitForSelector('tbody tr', { timeout: 20_000 });
    const localized = ((await page.textContent('h1')) ?? '').trim();
    check(`${englishName} renders the collection name`, localized === expected, localized);
  }
  await shot(page, '74-country-english');

  if (WRITE) {
    console.log('\nthe write path:');
    // Deliberately last, and gated, because it changes records and because it is
    // the half that needs a provisioned account.
    await page.goto(`${APP}refdata/country/new`, { waitUntil: 'load' });
    await page.waitForTimeout(600);
    const createHeading = ((await page.textContent('h1')) ?? '').trim();
    check('the create screen opens', createHeading.length > 0, createHeading);
    await page.fill('input#alpha2_code', 'XQ');
    await page.fill('input#alpha3_code', 'XQZ');
    await page.fill('input#numeric_code', '998');
    await page.fill('input#name', 'Verification Land');
    await page.fill('input#official_name', 'The Verification Republic');
    await page.locator('button', { hasText: /^Save$/ }).first().click();
    await page.waitForTimeout(800);

    /*
     * The audit prompt. The reason is a combo, and the commit uses whatever it
     * defaulted to, which is the point: a default that has to be chosen anyway
     * is not a default.
     */
    const reasonCombo = page.locator('[role="dialog"] select');
    const optionCount = await reasonCombo.locator('option').count();
    check('saving prompts for a reason', optionCount > 0, `${optionCount} reasons`);
    const defaulted = await reasonCombo.inputValue();
    check('the reason has a default', defaulted.length > 0, defaulted);
    await shot(page, '75-country-create-reason');

    await page.locator('[role="dialog"] button', { hasText: /^Create$/ }).click();
    await page.waitForLoadState('load');
    await page.waitForTimeout(800);

    // The record should now exist and the screen should be showing it.
    const created = ((await page.textContent('h1')) ?? '').trim();
    check('the record is created', created.includes('Verification Land'), created);
    await shot(page, '76-country-created');

    // Amend it, which is a different write path and a different reason set.
    await page.locator('button', { hasText: /^Edit$/ }).first().click();
    await page.waitForTimeout(600);
    await page.fill('input#name', 'Verification Land amended');
    await page.locator('button', { hasText: /^Save$/ }).first().click();
    await page.waitForTimeout(800);
    const amendReasons = page.locator('[role="dialog"] select');
    check('amending prompts for a reason', (await amendReasons.locator('option').count()) > 0);
    await page.locator('[role="dialog"] button', { hasText: /^Save$/ }).click();
    await page.waitForLoadState('load');
    await page.waitForTimeout(800);
    await page
      .locator('h1', { hasText: 'amended' })
      .waitFor({ timeout: 15_000 })
      .catch(() => undefined);
    const amended = ((await page.textContent('h1')) ?? '').trim();
    check('the amendment is saved', amended.includes('amended'), amended);

    // The history should hold both versions.
    await page.goto(`${APP}refdata/country/XQ/history`, { waitUntil: 'load' });
    await page.waitForTimeout(1500);
    const versions = await page.locator('ol li').count();
    check('the history holds both versions', versions >= 2, `${versions} versions`);
    await shot(page, '77-country-history-written');

    // Delete it, so the verification leaves the system as it found it.
    await page.goto(`${APP}refdata/country/XQ/edit`, { waitUntil: 'load' });
    await page.waitForTimeout(1200);
    await page.locator('button[title="Delete"]').first().click();
    await page.waitForTimeout(600);
    await page.locator('[role="dialog"] button', { hasText: /^Delete$/ }).click();
    await page.waitForTimeout(1200);
    await shot(page, '78-delete-reason');
    const deleteReasons = page.locator('[role="dialog"] select');
    const dialogLabel = await page.locator('[role="dialog"]').first().getAttribute('aria-label').catch(() => null);
    check(
      'deleting prompts for a reason',
      (await deleteReasons.locator('option').count()) > 0,
      `dialog="${dialogLabel}"`,
    );
    await page.locator('[role="dialog"] button', { hasText: /^Confirm Delete$/ }).click();
    await page.waitForLoadState('load');
    await page.waitForTimeout(800);

    await page.goto(`${APP}refdata/country`, { waitUntil: 'load' });
    await page.waitForSelector('tbody tr', { timeout: 20_000 });
    const body = (await page.textContent('body')) ?? '';
    check('the record is gone, leaving the system as found', !body.includes('Verification Land'));
    await shot(page, '78-country-deleted');
  } else {
    console.log('\nthe write path: skipped (pass --write to exercise it, which needs a provisioned account)');
  }

  console.log('\nbrowser console:');
  check('no uncaught exception was thrown', consoleErrors.length === 0, consoleErrors.slice(0, 2).join(' | '));
  check('every request succeeded', failedRequests.length === 0, failedRequests.slice(0, 3).join(' | '));
} finally {
  await browser.close();
}

console.log(`\n${passed} passed, ${failed} failed`);
console.log(`screenshots: ${SHOTS}/`);
process.exitCode = failed === 0 ? 0 : 1;

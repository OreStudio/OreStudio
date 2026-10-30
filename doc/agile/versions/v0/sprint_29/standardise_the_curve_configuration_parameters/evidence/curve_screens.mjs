// -*- mode: js; tab-width: 4; indent-tabs-mode: nil -*-
/*
 * Walks the curve configuration screens in a real browser, signs in as the
 * tenant administrator, opens the UK curve configuration and records what the
 * Curves tab draws: the segment cards, their leaves and their quotes.
 *
 *   node build/e2e/curve_screens.mjs
 *
 * Writes screenshots beside this script and prints the text it found.
 */
import { chromium } from 'playwright';
import { mkdirSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = HERE;
const BASE = 'http://localhost:21202';
const USER = 'tenant_admin@acme_corporation';
const PASSWORD = 'Secure-Password-123';

mkdirSync(OUT, { recursive: true });

const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 1600, height: 1200 } });
const failures = [];
page.on('console', (message) => {
    if (message.type() === 'error') failures.push(message.text());
});
page.on('pageerror', (error) => failures.push(String(error)));

async function shot(name, full = false) {
    await page.screenshot({ path: join(OUT, `${name}.png`), fullPage: full });
}

await page.goto(BASE, { waitUntil: 'networkidle' });
await shot('01-signin');

// The sign-in form: the username has no type attribute, the password follows.
await page.locator('input').first().fill(USER);
await page.locator('input[type="password"]').fill(PASSWORD);
await page.click('button[type="submit"]');
await page.waitForLoadState('networkidle');
await shot('02-after-signin');

// A tenant administrator may be asked which party it acts for.
const party = page.getByText('Acme Corporation Plc', { exact: false }).first();
if (await party.count()) {
    const clicked = await party.click({ timeout: 3000 }).then(() => true).catch(() => false);
    if (clicked) {
        await page.waitForLoadState('networkidle');
        await shot('03-party-chosen');
    }
}

// A tenant administrator chooses the party it works as before the shell opens.
await page.waitForTimeout(4000);
const partyButton = page.getByRole('button', { name: /Acme Corporation Plc/ });
if (await partyButton.count()) {
    await partyButton.first().click();
    await page.waitForTimeout(3000);
    await shot('04-party');
}

await page.getByText('Reporting', { exact: true }).first().click();
await page.waitForTimeout(1500);
await shot('05-reporting');
await page.getByText('Curve configurations', { exact: false }).first().click();
await page.waitForTimeout(2000);
await shot('06-configurations');

await page.getByText('UK curve configuration', { exact: false }).first().click();
await page.waitForTimeout(2500);
await shot('07-configuration');

const curvesTab = page.getByRole('tab', { name: 'Curves' });
if (await curvesTab.count()) {
    await curvesTab.click();
    await page.waitForLoadState('networkidle');
}
await shot('08-curves');
// The whole page, so the segment card below the fold is in the picture.
await shot('10-segments', true);

const body = await page.innerText('body');
const wanted = [
    'Its Segments',
    'Simple',
    'Projection curve',
    'IR_SWAP/RATE/GBP',
    'quotes',
];
for (const word of wanted) {
    if (!body.toLowerCase().includes(word.toLowerCase())) {
        failures.push(`the page does not say ${word}`);
    }
}

console.log('--- what the page says, lines holding a segment or a quote ---');
for (const line of body.split('\n')) {
    if (/segment|quote|Simple|Projection|IR_SWAP|without documentation/i.test(line)) {
        console.log(' ', line.trim());
    }
}
console.log('--- console errors ---');
console.log(failures.length === 0 ? ' none' : failures.map((f) => ` ${f}`).join('\n'));
await browser.close();
process.exit(failures.length === 0 ? 0 : 1);

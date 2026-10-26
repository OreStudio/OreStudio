// Drives the currencies' Export and Import end to end on the running web: the export, a wrong file refused,
// a changed file through the four steps, the import, and the record's history after it.
// Usage: node drive_currency_import_export.mjs [web base url] [screenshot folder]; Playwright from projects/ores.web.
import { createRequire } from 'node:module';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
const here = path.dirname(fileURLToPath(import.meta.url));
const repo = path.resolve(here, '../../../../../../..');
const { chromium } = createRequire(path.join(repo, 'projects/ores.web/package.json'))('playwright');
import { mkdirSync, readFileSync, writeFileSync } from 'node:fs';

const base = process.argv[2] || 'http://localhost:20402';
const out = path.resolve(process.argv[3] || path.join(here, 'shots'));
mkdirSync(out, { recursive: true });
const browser = await chromium.launch();
const context = await browser.newContext({ viewport: { width: 1380, height: 900 }, acceptDownloads: true });
const page = await context.newPage();
const errors = [];
page.on('response', (r) => { if (r.status() >= 400) errors.push(`${r.status()} ${r.request().method()} ${r.url()}`); });
page.on('pageerror', (e) => errors.push(`pageerror ${e.message}`));
page.on('console', (m) => { if (m.type() === 'error') errors.push(`console ${m.text().slice(0, 160)}`); });
const shot = (name) => page.screenshot({ path: `${out}/${name}.png`, fullPage: true });

await page.goto(`${base}/sign-in`);
await page.locator('input[autocomplete="username"]').fill('tenant_admin@acme_corporation');
await page.locator('input[autocomplete="current-password"]').fill('Secure-Password-123');
await page.getByRole('button', { name: 'Sign in', exact: true }).click();
await page.waitForTimeout(2500);
if (await page.getByRole('heading', { name: 'Choose a party' }).isVisible()) {
    await page.getByRole('main').getByRole('button').first().click();
    await page.waitForTimeout(1500);
}

// 1. The list header with the pair.
await page.goto(`${base}/refdata/currencies`);
await page.getByRole('button', { name: 'Export', exact: true }).waitFor();
await shot('list');

// 2. Export: the dialog, the press, the download.
await page.getByRole('button', { name: 'Export', exact: true }).click();
await page.getByRole('dialog').waitFor();
await shot('export-dialog');
const [download] = await Promise.all([
    page.waitForEvent('download', { timeout: 30000 }),
    page.getByRole('dialog').getByRole('button', { name: 'Export', exact: true }).click(),
]);
const saved = `${out}/${download.suggestedFilename()}`;
await download.saveAs(saved);
await page.waitForTimeout(500);
await shot('export-done');
const content = readFileSync(saved, 'utf8');
console.log('downloaded', download.suggestedFilename(), content.length, 'bytes; root', content.slice(0, 120).replace(/\s+/g, ' '));
console.log('export notice:', (await page.getByRole('dialog').innerText()).replace(/\s+/g, ' ').slice(0, 200));
await page.getByRole('dialog').getByRole('button', { name: 'Close', exact: true }).last().click();

// A changed copy: EUR's symbol becomes EUR, and GBP is dropped from the file.
const eur = content.match(/<Currency>\s*<Name>Euro<\/Name>[\s\S]*?<\/Currency>/);
if (!eur) throw new Error('no Euro in the export');
const changedEur = eur[0].replace(/<Symbol>[^<]*<\/Symbol>/, '<Symbol>EUR</Symbol>');
let changed = content.replace(eur[0], changedEur);
const gbp = changed.match(/<Currency>(?:(?!<\/Currency>)[\s\S])*?<ISOCode>GBP<\/ISOCode>(?:(?!<\/Currency>)[\s\S])*?<\/Currency>/);
if (gbp) changed = changed.replace(gbp[0], '');
const changedPath = `${out}/currencyconfig_changed.xml`;
writeFileSync(changedPath, changed);
const wrongPath = `${out}/calendaradjustment.xml`;
writeFileSync(wrongPath, '<?xml version="1.0"?>\n<CalendarAdjustments></CalendarAdjustments>\n');

// 3. Import: the wrong file is refused, naming its root.
await page.goto(`${base}/refdata/currencies/import`);
await page.getByRole('heading', { level: 1 }).waitFor();
await shot('import-1');
await page.locator('input[type="file"]').setInputFiles(wrongPath);
await page.getByRole('button', { name: 'Next', exact: true }).click();
await page.waitForTimeout(1500);
await shot('import-1-wrong');
console.log('wrong file:', (await page.locator('main').innerText()).match(/This file is not[^\n]*/)?.[0]);

// 4. Import: the changed file through the four steps.
await page.locator('input[type="file"]').setInputFiles(changedPath);
await page.getByRole('button', { name: 'Next', exact: true }).click();
await page.getByRole('heading', { name: /What the file holds/ }).waitFor({ timeout: 30000 });
await shot('import-2');
console.log('counts:', (await page.locator('main').innerText()).replace(/\s+/g, ' ').match(/\d+ In the file.*?Not in the file/)?.[0]);
await page.getByRole('button', { name: 'Next', exact: true }).click();
await page.getByRole('heading', { name: /What the system does not know/ }).waitFor();
await shot('import-3');
await page.getByRole('button', { name: 'Next', exact: true }).click();
await page.getByRole('heading', { name: /The difference/ }).waitFor();
await shot('import-4');
const rows = await page.locator('main table tbody tr').allInnerTexts();
console.log('difference rows:', rows.map((r) => r.replace(/\s+/g, ' ').slice(0, 90)));
await page.getByRole('button', { name: 'Import', exact: true }).click();
await page.waitForURL(/\/refdata\/currencies$/, { timeout: 30000 });
await page.waitForTimeout(800);
await shot('imported');
console.log('toast:', (await page.locator('body').innerText()).match(/Imported:[^\n]*/)?.[0]);

// 5. The record shows the new version and the history the change.
await page.goto(`${base}/refdata/currencies/EUR?tab=history`);
await page.waitForTimeout(1500);
await shot('eur-history-after');
console.log('eur lead:', (await page.locator('main').innerText()).match(/EUR · version \d+/)?.[0]);
console.log('history mentions symbol:', /Symbol/.test(await page.locator('main').innerText()));
console.log(errors.length ? errors.join('\n') : 'no script errors');
await browser.close();

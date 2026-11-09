// Drives the scenario Verify the currencies screens, one function per step, and prints
// PASS or FAIL with what was seen. Playwright is resolved from projects/ores.web, so the
// script runs from anywhere: node scenario_runner.mjs [web base url].
import { mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { createRequire } from 'node:module';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const here = path.dirname(fileURLToPath(import.meta.url));
const repo = path.resolve(here, '../../../../../../..');
const { chromium } = createRequire(path.join(repo, 'projects/ores.web/package.json'))('playwright');

const base = process.argv[2] || 'http://localhost:20402';
const CODE = 'ZZS';
const PAIR = `${CODE}/USD`;
const out = process.env.SCENARIO_OUT ?? path.join(tmpdir(), 'currencies-scenario');
mkdirSync(out, { recursive: true });
const browser = await chromium.launch();
const ctx = await browser.newContext({ viewport: { width: 1400, height: 1000 }, locale: 'en-GB', acceptDownloads: true });
const page = await ctx.newPage();
const results = [];
const leftovers = [];
const expect = (cond, what) => { if (!cond) throw new Error(what); };
const text = async (loc) => (await loc.innerText()).replace(/\n/g, ' | ');
async function step(name, fn) {
    try { const note = await fn(); results.push([name, 'PASS', note ?? '']); console.log('PASS', name, note ?? ''); }
    catch (e) { results.push([name, 'FAIL', e.message]); console.log('FAIL', name, e.message); await page.screenshot({ path: `${out}/fail-${results.length}.png`, fullPage: true }).catch(() => {}); }
}
async function signIn(user) {
    await page.goto(`${base}/sign-in`);
    await page.locator('input[autocomplete="username"]').fill(user);
    await page.locator('input[autocomplete="current-password"]').fill('Secure-Password-123');
    await page.getByRole('button', { name: 'Sign in', exact: true }).click();
    await page.waitForTimeout(2500);
    if (await page.getByRole('heading', { name: 'Choose a party' }).count()) { await page.getByRole('main').getByRole('button').first().click(); await page.waitForTimeout(1500); }
}
const buttons = async () => page.locator('main').getByRole('button').allInnerTexts();
const dialog = () => page.getByRole('dialog');
const title = async () => dialog().getByRole('heading', { level: 2 }).innerText();
// ES-REFRESH-15: after a save the dialog closes and Refresh turns gold; the person presses it.
const refresh = async () => { await page.getByRole('button', { name: 'Refresh', exact: true }).first().click(); await page.waitForTimeout(2000); };
const select = (scope, label) => scope.locator(`label:has-text("${label}") select`).first();
// A classification select fills from the server; wait until it offers more than the empty choice.
async function optionsOf(sel) {
    for (let i = 0; i < 20; i++) { const options = await sel.locator('option').allInnerTexts(); if (options.length > 1) return options; await page.waitForTimeout(500); }
    return sel.locator('option').allInnerTexts();
}
const reason = async (scope) => { const why = select(scope, 'Why'); if (await why.count()) await why.selectOption({ index: 1 }); };
const linkRows = () => page.locator('main li').filter({ has: page.getByRole('button', { name: /^Remove|^Take .* away/ }) });
/* The shared record picker: a button that opens a list with a search box. */
async function pick(scope, label, words) {
    const sel = select(scope, label);
    if (await sel.count()) { await sel.selectOption({ label: words }); return; }
    const field = scope.locator(`label:has-text("${label}") button`).first();
    for (let attempt = 0; attempt < 2; attempt++) {
        await field.click(); await page.waitForTimeout(400); await page.keyboard.type(words.split(' ')[0]); await page.waitForTimeout(900);
        const option = page.getByRole('option', { name: new RegExp(words.replace(/[()$]/g, '.'), 'i') }).first();
        if (await option.count()) await option.click(); else await page.keyboard.press('Enter');
        await page.waitForTimeout(500);
        if ((await field.innerText()).includes(words.split(' ')[0])) break;
    }
    expect((await field.innerText()).includes(words.split(' ')[0]), `${label} did not take ${words}: ${await field.innerText()}`);
}
/* The link panel: a picker under the list that offers what is not linked yet, then Add (RS-RELATED-02). */
async function addLink(tab, words) {
    await page.getByRole('tab', { name: tab }).click(); await page.waitForTimeout(1200);
    // A link left behind by an earlier run's deleted record is already there; the sheet records the gap.
    if (await linkRows().filter({ hasText: words.split(' ')[0] }).count()) { leftovers.push(`${tab}: ${words}`); return; }
    const picker = page.locator('main').getByRole('button', { name: /Choose one to add/ }).first();
    if (await picker.count()) {
        await picker.click(); await page.waitForTimeout(400); await page.keyboard.type(words.split(' ')[0]); await page.waitForTimeout(900);
        const option = page.getByRole('option', { name: new RegExp(words.split(' ')[0], 'i') }).first();
        if (await option.count()) await option.click(); else await page.keyboard.press('Enter');
    } else {
        const sel = page.locator('main select').first(); const options = await sel.locator('option').allInnerTexts();
        await sel.selectOption({ label: options.find((o) => o.includes(words.split(' ')[0])) ?? words });
    }
    await page.waitForTimeout(400);
    await page.locator('main').getByRole('button', { name: 'Add', exact: true }).first().click(); await page.waitForTimeout(1500); await refresh();
}
const removeOf = (row) => row.getByRole('button', { name: /^Remove|^Take .* away/ }).first();

await step('Sign in as the tenant administrator', async () => {
    await signIn('tenant_admin@acme_corporation');
    expect(await page.getByRole('button', { name: 'Your account' }).count() > 0, 'no Your account button');
});
// Leftovers of earlier runs go first, so the counts and the create step hold.
for (const [url, label] of [[`${base}/refdata/currency-pairs/${encodeURIComponent(PAIR)}`, 'pair'], [`${base}/refdata/currencies/${CODE}`, 'currency']]) {
    await page.goto(url); await page.waitForTimeout(2000);
    if (await page.getByRole('button', { name: 'Delete', exact: true }).count()) {
        await page.getByRole('button', { name: 'Delete', exact: true }).first().click(); await dialog().waitFor(); await reason(dialog());
        await dialog().getByRole('button', { name: 'Delete', exact: true }).click(); await page.waitForTimeout(3000);
        console.log('removed a leftover', label);
    }
}
await step('Open the currencies list', async () => {
    await page.goto(`${base}/refdata/currencies`); await page.locator('table tbody tr').first().waitFor();
    const crumbs = await text(page.getByRole('navigation', { name: 'Breadcrumb' })); expect(/Currencies$/.test(crumbs), `crumbs: ${crumbs}`);
    await page.getByRole('button', { name: 'Add currency' }).waitFor({ timeout: 15000 });
    const b = await buttons(); const links = await page.locator('main').getByRole('link', { name: 'Import', exact: true }).count();
    expect(b[0] === 'Refresh' && b.includes('Export') && b.includes('Add currency') && links === 1, `header: ${b.slice(0, 4)} import links ${links}`);
    expect(await page.locator('main time').count() > 0, 'no load time');
    const heads = await page.locator('table thead th').allInnerTexts(); expect(heads.join(',').includes('ISO code,Name,Symbol,Monetary nature,Market tier'), `columns: ${heads}`);
    const main = await text(page.locator('main')); expect(main.includes('1–15 of 168'), 'pager not 1–15 of 168');
    const firstNature = await page.locator('table tbody tr').first().getByRole('cell').nth(3).innerText();
    await page.getByRole('searchbox').fill('EUR'); await page.waitForTimeout(1500);
    expect(await page.locator('table tbody tr').count() === 1, 'search rows != 1'); expect(page.url().includes('EUR'), 'search not in address');
    await page.screenshot({ path: `${out}/02-list.png`, fullPage: true });
    return `header ${b.slice(0, 3)} + Import link; columns ${heads.slice(0, 5)}; nature reads "${firstNature}"; pager 1–15 of 168; search in the address`;
});
await step('Open the euro', async () => {
    await page.locator('table tbody tr').first().getByRole('cell').first().click(); await page.waitForURL(/currencies\/EUR/); await page.waitForTimeout(1500);
    const t = await text(page.locator('h1')); expect(t.includes('Euro'), `title: ${t}`);
    const b = await buttons(); expect(b.slice(0, 3).join(',') === 'Refresh,Edit,Delete', `buttons: ${b.slice(0, 4)}`);
    const tabs = await page.getByRole('tab').allInnerTexts(); expect(tabs.join(',') === 'Details,Countries,Calendars,Desk groups,History', `tabs: ${tabs}`);
    const main = await text(page.locator('main')); expect(main.includes('Editable') && main.includes('Last changed'), 'mark or last-changed missing');
    const labels = await page.locator('main dt').allInnerTexts(); expect(labels[0] === 'ISO code', `first label: ${labels[0]}`);
    const dayBasis = await page.locator('main dd').nth(labels.indexOf('Day basis')).innerText(); const tier = await page.locator('main dd').nth(labels.indexOf('Market tier')).innerText();
    await page.screenshot({ path: `${out}/03-eur.png`, fullPage: true });
    return `buttons ${b.slice(0, 3)}; tabs ${tabs}; ${labels.length} labels; Day basis "${dayBasis.trim()}", Market tier "${tier.trim()}"`;
});
await step('Add the scenario dollar', async () => {
    await page.goto(`${base}/refdata/currencies`); await page.getByRole('button', { name: 'Add currency' }).waitFor({ timeout: 15000 }); await page.getByRole('button', { name: 'Add currency' }).click(); await dialog().waitFor();
    expect(await title() === 'Add a currency', `title: ${await title()}`);
    const heads = await dialog().locator('h3').allInnerTexts(); expect(heads.join(',') === 'Identity,Display,Market', `groups: ${heads}`);
    const d = dialog();
    for (const [label, value] of [['ISO code', CODE], ['Name', 'Scenario dollar'], ['Numeric code', '999'], ['Symbol', 'Z$'], ['Fraction symbol', 'c'], ['Fractions per unit', '100'], ['Rounding precision', '2'], ['Format', 'Z$#,##0.00'], ['Spot days', '2'], ['Base precedence', '50']]) await d.getByLabel(label, { exact: true }).fill(value);
    for (const [label, words] of [['Rounding type', /Nearest/], ['Monetary nature', /Fiat/], ['Market tier', /Exotic/], ['ORE currency type', /Major/], ['Day basis', /^ACT\/360/]]) {
        const sel = select(d, label); expect(await sel.count() === 1, `${label} is not a select`);
        const options = await optionsOf(sel); const match = options.find((o) => words.test(o)); expect(match !== undefined, `${label} has no option ${words}: ${options.slice(0, 6)}`);
        await sel.selectOption({ label: match });
    }
    const note = await text(d); expect(note.includes('new record'), 'new-record note missing');
    await d.getByRole('button', { name: 'Add', exact: true }).click(); await page.waitForURL(new RegExp(`currencies/${CODE}`), { timeout: 15000 }); await page.waitForTimeout(2000);
    const t = await text(page.locator('h1')); expect(t.includes('Scenario dollar'), `title: ${t}`);
    await page.screenshot({ path: `${out}/04-added.png`, fullPage: true });
    return `groups ${heads}; five selects; opened on ${page.url().split('/').pop()}`;
});
await step('Link a country, a calendar and a desk group', async () => {
    await addLink('Countries', 'Germany'); let main = await text(page.locator('main')); expect(main.includes('Germany'), 'country row missing');
    await addLink('Calendars', 'TARGET'); main = await text(page.locator('main')); expect(main.includes('TARGET'), 'calendar row missing');
    await addLink('Desk groups', 'Scandies'); main = await text(page.locator('main')); expect(main.includes('Scandies'), 'desk group row missing');
    await page.getByRole('tab', { name: 'Countries' }).click(); await page.waitForTimeout(800);
    // A link's Remove acts on the click, with no dialog and no reason asked; the sheet records the difference from the prototype.
    await removeOf(linkRows().filter({ hasText: 'Germany' })).click(); await page.waitForTimeout(1500);
    const asked = await dialog().count(); if (asked) { await reason(dialog()); await dialog().getByRole('button', { name: /^Remove/ }).last().click(); await page.waitForTimeout(1500); }
    await refresh();
    main = await text(page.locator('main')); expect(!main.includes('Germany'), 'country still linked');
    await page.screenshot({ path: `${out}/05-links.png`, fullPage: true });
    return `three links present after Refresh${leftovers.length ? ` (already linked before this run's add, left behind by a deleted record: ${leftovers.join('; ')})` : ''}; the country removed ${asked ? 'through a dialog' : 'on the click, with no dialog and no reason asked'}`;
});
await step('Amend the currency against a stale version', async () => {
    const second = await ctx.newPage(); await second.goto(`${base}/refdata/currencies/${CODE}`); await second.waitForTimeout(2000);
    await second.getByRole('button', { name: 'Edit', exact: true }).click(); await second.getByRole('dialog').waitFor();
    await page.getByRole('tab', { name: 'Details' }).click(); await page.waitForTimeout(500);
    await page.getByRole('button', { name: 'Edit', exact: true }).click(); await dialog().waitFor();
    const t = await title(); expect(t === 'Edit Scenario dollar', `title: ${t}`); expect(await dialog().locator('time').count() === 1, 'no loaded time');
    await dialog().getByLabel('Name', { exact: true }).fill('Scenario dollar, renamed'); await reason(dialog());
    await dialog().getByRole('button', { name: 'Save', exact: true }).click(); await page.waitForTimeout(2500);
    await second.getByRole('dialog').getByLabel('Name', { exact: true }).fill('Scenario dollar, stale'); await reason(second.getByRole('dialog'));
    await second.getByRole('dialog').getByRole('button', { name: 'Save', exact: true }).click(); await second.waitForTimeout(2500);
    const d = await text(second.getByRole('dialog')); expect(d.includes('changed this record after you opened it') && d.includes('Reload'), `stale: ${d.slice(-300)}`);
    await second.screenshot({ path: `${out}/06-stale.png`, fullPage: true });
    await second.getByRole('button', { name: 'Reload' }).click(); await second.waitForTimeout(2500);
    expect(await second.getByRole('dialog').count() === 0, 'dialog still open after Reload');
    const t2 = await text(second.locator('h1')); expect(t2.includes('renamed'), `second tab title: ${t2}`);
    await second.close(); await refresh();
    return `"${t}" with its load time; the stale save refused; Reload closed the dialog`;
});
await step('Read the history and revert', async () => {
    await page.getByRole('tab', { name: 'History' }).click(); await page.waitForTimeout(2500);
    const entries = await page.locator('main li[data-ms]').count(); expect(entries >= 2, `entries: ${entries}`);
    const main = await text(page.locator('main')); expect(main.includes('Hide system activity'), 'toggle missing'); expect(main.includes('not versioned'), 'the links gap box is missing');
    const reverts = await page.getByRole('button', { name: 'Revert' }).count(); expect(reverts === entries - 1, `revert buttons: ${reverts} for ${entries} entries`);
    await page.screenshot({ path: `${out}/07-history.png`, fullPage: true });
    await page.getByRole('button', { name: 'Revert' }).last().click(); await dialog().waitFor(); await page.waitForTimeout(500);
    const d = await text(dialog()); expect(/Revert to version \d+/.test(d), `revert dialog: ${d.slice(0, 120)}`); await reason(dialog());
    await dialog().getByRole('button', { name: 'Revert', exact: true }).click(); await page.waitForTimeout(2000); await refresh();
    const after = await page.locator('main li[data-ms]').count(); expect(after === entries + 1, `entries after revert: ${after}`);
    const t = await text(page.locator('h1')); expect(t.trim().endsWith('Scenario dollar'), `title after revert: ${t}`);
    return `${entries} entries then ${entries + 1}; Revert on every version but the newest; title back to Scenario dollar${entries > 2 ? ' (the history continues an earlier run\'s, a captured gap)' : ''}`;
});
await step('Export the currencies and import a change', async () => {
    await page.goto(`${base}/refdata/currencies`); await page.getByRole('button', { name: 'Export', exact: true }).waitFor();
    await page.getByRole('button', { name: 'Export', exact: true }).click(); await dialog().waitFor();
    expect(await title() === 'Export', `title: ${await title()}`); const d0 = await text(dialog()); expect(d0.includes('currencyconfig.xml') && d0.includes('browser saves'), 'export words');
    const [download] = await Promise.all([page.waitForEvent('download', { timeout: 30000 }), dialog().getByRole('button', { name: 'Export', exact: true }).click()]);
    const saved = `${out}/${download.suggestedFilename()}`; await download.saveAs(saved); await page.waitForTimeout(500);
    const d1 = await text(dialog()); expect(/Saved currencyconfig\.xml, \d+ records/.test(d1), `after: ${d1.slice(-120)}`);
    await page.screenshot({ path: `${out}/08-export.png`, fullPage: true });
    await dialog().getByRole('button', { name: 'Close', exact: true }).last().click();
    const content = readFileSync(saved, 'utf8'); const mine = content.match(new RegExp(`<Currency>(?:(?!</Currency>)[\\s\\S])*?<ISOCode>${CODE}</ISOCode>(?:(?!</Currency>)[\\s\\S])*?</Currency>`)); expect(mine, `${CODE} not in the export`);
    const changed = content.replace(mine[0], mine[0].replace(/<Symbol>[^<]*<\/Symbol>/, '<Symbol>ZS</Symbol>'));
    const changedPath = `${out}/currencyconfig_changed.xml`; writeFileSync(changedPath, changed);
    const wrongPath = `${out}/calendaradjustment.xml`; writeFileSync(wrongPath, '<?xml version="1.0"?>\n<CalendarAdjustments></CalendarAdjustments>\n');
    await page.getByRole('link', { name: 'Import', exact: true }).click(); await page.waitForURL(/currencies\/import/); await page.getByRole('heading', { level: 1 }).waitFor();
    await page.waitForTimeout(800); expect((await page.locator('h1').allInnerTexts()).join(' ').includes('Import currencies'), `import title: ${await page.locator('h1').allInnerTexts()}`);
    await page.locator('input[type="file"]').setInputFiles(wrongPath); await page.getByRole('button', { name: 'Next', exact: true }).click(); await page.waitForTimeout(1500);
    const refused = await text(page.locator('main')); expect(refused.includes('root element is CalendarAdjustments'), 'wrong file not refused by its root');
    await page.locator('input[type="file"]').setInputFiles(changedPath); await page.getByRole('button', { name: 'Next', exact: true }).click();
    await page.getByRole('heading', { name: /What the file holds/ }).waitFor({ timeout: 30000 });
    await page.waitForTimeout(800);
    const holds = await page.locator('main').innerText();
    const found = holds.match(/(\d+)\s*\|?\s*In the file\s*\|?\s*(\d+)\s*\|?\s*Already held\s*\|?\s*(\d+)\s*\|?\s*Changed\s*\|?\s*(\d+)\s*\|?\s*New\s*\|?\s*(\d+)\s*\|?\s*Not in the file/);
    const counts = found ? found.slice(1, 6).join(',') : '';
    expect(counts === '169,168,1,0,0', `counts in file, held, changed, new, not in file: ${counts || holds.replace(/\s+/g, ' ').slice(0, 300)}`);
    await page.getByRole('button', { name: 'Next', exact: true }).click(); await page.getByRole('heading', { name: /What the system does not know/ }).waitFor();
    const unknown = await text(page.locator('main')); expect(unknown.includes('Format, Monetary nature, Market tier'), 'the fields the document lacks are not named');
    // The importer refuses a zero fractions per unit, which the yen and twelve others carry, so every import lists them here (captured).
    const zeroFraction = (unknown.match(/Fractions per unit must be positive/g) ?? []).length;
    expect(!unknown.includes('Values with no row') || zeroFraction > 0, `unknown values other than the zero fraction ones: ${unknown.slice(unknown.indexOf('Values with no row'), unknown.indexOf('Values with no row') + 300)}`);
    await page.screenshot({ path: `${out}/08-import-3.png`, fullPage: true });
    await page.getByRole('button', { name: 'Next', exact: true }).click(); await page.getByRole('heading', { name: /The difference/ }).waitFor();
    const rows = await page.locator('main table tbody tr').allInnerTexts(); expect(rows.length === 1 && /Changed.*ZZS.*Symbol Z\$ → ZS/.test(rows[0].replace(/\s+/g, ' ')), `rows: ${rows}`);
    await reason(page.locator('main')); await page.screenshot({ path: `${out}/08-import-4.png`, fullPage: true });
    await page.getByRole('button', { name: 'Import', exact: true }).click(); await page.waitForURL(/\/refdata\/currencies$/, { timeout: 30000 }); await page.waitForTimeout(800);
    const toast = (await page.locator('body').innerText()).match(/Imported:[^\n]*/)?.[0] ?? ''; expect(toast.startsWith('Imported: 0 added, 1 changed, 0 deleted, 0 kept'), `toast: ${toast}`);
    await page.goto(`${base}/refdata/currencies/${CODE}?tab=history`); await page.waitForTimeout(2500);
    const story = await text(page.locator('main')); expect(story.includes('Symbol'), 'history does not name the Symbol');
    return `${d1.match(/Saved [^|]*/)?.[0]}; wrong file refused by its root; counts right; ${zeroFraction} zero-fraction currencies listed as values with no row; one changed row; "${toast}"; Symbol in the history`;
});
await step('Open a desk group and change its members', async () => {
    await page.goto(`${base}/refdata/desk-groups`); await page.locator('table tbody tr').first().waitFor(); await page.waitForTimeout(800);
    const main0 = await text(page.locator('main')); expect(main0.includes('1–6 of 6'), 'pager not 1–6 of 6');
    await page.locator('table tbody tr', { hasText: 'G11' }).getByRole('cell').first().click(); await page.waitForURL(/desk-groups\/G11/); await page.waitForTimeout(1500);
    const tabs = await page.getByRole('tab').allInnerTexts(); expect(tabs.join(',') === 'Details,Members,History', `tabs: ${tabs}`);
    await page.getByRole('tab', { name: 'Members' }).click(); await page.waitForTimeout(1500);
    const before = await linkRows().count(); expect(before === 11, `members: ${before}`);
    await addLink('Members', 'Scenario dollar'); let main = await text(page.locator('main')); expect(main.includes(CODE), 'ZZS not a member');
    await removeOf(linkRows().filter({ hasText: CODE })).click(); await page.waitForTimeout(1500);
    if (await dialog().count()) { await reason(dialog()); await dialog().getByRole('button', { name: /^Remove/ }).last().click(); await page.waitForTimeout(1500); }
    await refresh();
    main = await text(page.locator('main')); expect(!main.includes(CODE), 'ZZS still a member');
    await page.getByRole('tab', { name: 'History' }).click(); await page.waitForTimeout(2000);
    const story = await text(page.locator('main')); expect(story.includes('Hide system activity') && story.includes('not versioned'), 'group history not the timeline');
    await page.screenshot({ path: `${out}/09-group.png`, fullPage: true });
    return `pager 1–6 of 6; tabs ${tabs}; 11 members, ZZS added and removed; the timeline`;
});
await step('Set up a currency pair', async () => {
    await page.goto(`${base}/refdata/currency-pairs`); await page.locator('table tbody tr').first().waitFor();
    const heads = await page.locator('table thead th').allInnerTexts(); expect(heads.join(',').includes('Pair,Base currency,Quote currency,Classification'), `columns: ${heads}`);
    await page.getByRole('button', { name: 'Add pair' }).waitFor({ timeout: 15000 }); await page.getByRole('button', { name: 'Add pair' }).click(); await dialog().waitFor();
    expect(await title() === 'Add a currency pair', `title: ${await title()}`);
    const groups = await dialog().locator('h3').allInnerTexts(); expect(groups.join(',') === 'The pair,Quoting convention', `groups: ${groups}`);
    expect((await text(dialog())).includes('Pair code: —'), 'code line not empty');
    await pick(dialog(), 'Base currency', 'Scenario dollar'); await pick(dialog(), 'Quote currency', 'Scenario dollar'); await page.waitForTimeout(500);
    let d = await text(dialog()); expect(d.includes('must differ'), 'same legs not refused'); expect(await dialog().getByRole('button', { name: 'Add', exact: true }).isDisabled(), 'Add enabled with equal legs');
    await pick(dialog(), 'Quote currency', 'US Dollar'); await page.waitForTimeout(500);
    d = await text(dialog()); expect(d.includes(`Pair code: ${PAIR}`), `code line: ${d.match(/Pair code: [^|]*/)?.[0]}`);
    await optionsOf(select(dialog(), 'Classification')); await select(dialog(), 'Classification').selectOption({ index: 1 });
    for (const [label, value] of [['Pip factor', '0.0001'], ['Tick size (pips)', '1'], ['Decimal places', '4']]) await dialog().getByLabel(label, { exact: true }).fill(value);
    await optionsOf(select(dialog(), 'Business day convention')); await select(dialog(), 'Business day convention').selectOption({ index: 1 });
    for (const [label, value] of [['Forward dates from spot', 'Yes'], ['End of month', 'No']]) await select(dialog(), label).selectOption({ label: value });
    d = await text(dialog()); expect(d.includes('reads 1.0842'), 'sample rate');
    await page.screenshot({ path: `${out}/10-pair-dialog.png`, fullPage: true });
    await dialog().getByRole('button', { name: 'Add', exact: true }).click(); await page.waitForURL(/currency-pairs\/ZZS/, { timeout: 15000 }); await page.waitForTimeout(2500);
    const t = await text(page.locator('h1')); expect(t.includes(PAIR), `title: ${t}`);
    const last = await page.locator('main').getByText('Last changed').count(); expect(last === 2, `last-changed lines: ${last}`);
    await page.getByRole('tab', { name: 'History' }).click(); await page.waitForTimeout(2500);
    const story = await text(page.locator('main')); expect(story.includes('Convention v1') && story.includes(`${PAIR} v1`), `history: ${story.slice(0, 200)}`);
    await page.screenshot({ path: `${out}/10-pair-history.png`, fullPage: true });
    return `columns ${heads.slice(0, 4)}; groups ${groups}; equal legs refused; code ${PAIR}; two cards; history with Convention v1`;
});
await step("Edit the pair's convention and delete the pair", async () => {
    await page.getByRole('tab', { name: 'Details' }).click(); await page.waitForTimeout(500);
    await page.getByRole('button', { name: 'Edit', exact: true }).click(); await dialog().waitFor();
    const t = await title(); expect(t === `Edit ${PAIR}`, `title: ${t}`);
    expect(await dialog().locator('label:has-text("Base currency") button, label:has-text("Base currency") select, label:has-text("Base currency") input').first().isDisabled(), 'base not fixed');
    await dialog().getByLabel('Decimal places', { exact: true }).fill('5'); await reason(dialog());
    await dialog().getByRole('button', { name: 'Save', exact: true }).click(); await page.waitForTimeout(2500); await refresh();
    const main = await text(page.locator('main')); expect(main.includes('1.08424'), 'sample rate not 1.08424');
    await page.getByRole('tab', { name: 'History' }).click(); await page.waitForTimeout(2500);
    const story = await text(page.locator('main')); expect(story.indexOf('Convention v2') >= 0 && story.indexOf('Convention v2') < story.indexOf(`${PAIR} v1`), 'Convention v2 not newest');
    await page.getByRole('button', { name: 'Delete', exact: true }).first().click(); await dialog().waitFor();
    const dt = await title(); expect(dt === `Delete ${PAIR}?`, `delete title: ${dt}`); expect((await text(dialog())).includes('convention'), 'warning does not name the convention');
    await reason(dialog()); await dialog().getByRole('button', { name: 'Delete', exact: true }).click(); await page.waitForTimeout(4000);
    expect(/\/refdata\/currency-pairs\/?(\?.*)?$/.test(page.url()), `url after delete: ${page.url()}`);
    await page.getByRole('searchbox').fill(CODE); await page.waitForTimeout(1500); expect((await text(page.locator('main'))).includes('No currency pairs match'), 'pair still listed');
    return `"${t}"; base fixed; sample 1.08424; Convention v2 newest; "${dt}"; list without the pair`;
});
await step('Delete the scenario dollar', async () => {
    await page.goto(`${base}/refdata/currencies/${CODE}`); await page.waitForTimeout(2000);
    await page.getByRole('button', { name: 'Delete', exact: true }).first().click(); await dialog().waitFor(); await page.waitForTimeout(500);
    const t = await title(); expect(t === 'Delete Scenario dollar?', `title: ${t}`); expect((await text(dialog())).includes('trades, books or pairs'), 'warning missing');
    await reason(dialog()); await dialog().getByRole('button', { name: 'Delete', exact: true }).click(); await page.waitForTimeout(4000);
    expect(/\/refdata\/currencies\/?(\?.*)?$/.test(page.url()), `url after delete: ${page.url()}`);
    await page.getByRole('searchbox').fill(CODE); await page.waitForTimeout(1500); expect((await text(page.locator('main'))).includes('No currencies match the search'), 'deleted currency still listed');
    await page.goto(`${base}/refdata/currencies/${CODE}`); await page.waitForTimeout(2000);
    const gone = await text(page.locator('main')); expect(gone.includes('no such record'), `page by address: ${gone.slice(0, 200)}`);
    return `"${t}"; list opened; address answers no such record`;
});
await step('Look as a reader', async () => {
    await ctx.clearCookies(); await signIn('adrian.vance@acme_corporation');
    await page.goto(`${base}/refdata/currencies`); await page.locator('table tbody tr').first().waitFor(); await page.waitForTimeout(2500);
    const b = await buttons(); const imports = await page.locator('main').getByRole('link', { name: 'Import', exact: true }).count();
    expect(b.includes('Export'), 'Export not drawn for a reader');
    await page.goto(`${base}/refdata/currencies/EUR`); await page.waitForTimeout(2000); const mark = (await text(page.locator('main'))).includes('Read only') ? 'Read only' : 'Editable';
    await page.screenshot({ path: `${out}/13-reader.png`, fullPage: true });
    return `Export drawn; Import link ${imports}, Add ${b.includes('Add currency') ? 'drawn' : 'not drawn'}; EUR marked ${mark}`;
});
await browser.close();
console.log('\nSUMMARY'); for (const [n, r, w] of results) console.log(`${r}\t${n}\t${w}`);

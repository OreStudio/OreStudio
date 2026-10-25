// Drives the scenario Verify the holiday calendar screens, one function per step, and
// prints PASS or FAIL with what was seen. Run from projects/ores.web so playwright resolves.
import { chromium } from 'playwright';
import { mkdirSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

const base = 'http://localhost:20402';
const CODE = process.env.CODE ?? 'Acme.Scenario';
// Screenshots go where SCENARIO_OUT says, or under the system's temporary folder.
const out = process.env.SCENARIO_OUT ?? join(tmpdir(), 'calendar-scenario');
mkdirSync(out, { recursive: true });
const browser = await chromium.launch();
const ctx = await browser.newContext({ viewport: { width: 1400, height: 1000 }, locale: 'en-GB' });
const page = await ctx.newPage();
const results = [];
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
// ES-REFRESH-15: after a save the dialog closes and Refresh turns gold; the person presses it.
const refresh = async () => { await page.getByRole('button', { name: 'Refresh', exact: true }).click(); await page.waitForTimeout(2000); };

await step('Sign in as the tenant administrator', async () => {
    await signIn('tenant_admin@acme_corporation');
    expect(await page.getByRole('button', { name: 'Your account' }).count() > 0, 'no Your account button');
});
// Leftovers from earlier runs are removed first, so the list counts 61 and the derive step can create the code.
for (let round = 0; round < 6; round++) {
    await page.goto(`${base}/refdata/calendars/${CODE}`); await page.waitForTimeout(2000);
    if (!(await page.getByRole('button', { name: 'Delete', exact: true }).count())) break;
    await page.getByRole('button', { name: 'Delete', exact: true }).first().click(); await page.getByRole('dialog').waitFor();
    await page.getByRole('dialog').getByRole('button', { name: 'Delete', exact: true }).click(); await page.waitForTimeout(3000);
    console.log('removed a leftover', page.url());
}
await step('Open the calendars list', async () => {
    await page.goto(`${base}/refdata/calendars`); await page.locator('table tbody tr').first().waitFor();
    const crumbs = await text(page.getByRole('navigation', { name: 'Breadcrumb' }));
    expect(/Holiday calendars$/.test(crumbs), `crumbs: ${crumbs}`);
    const t0 = Date.now(); await page.getByRole('button', { name: 'Add calendar' }).waitFor({ timeout: 15000 }); const addAfter = Date.now() - t0;
    const b = await buttons(); expect(b[0] === 'Refresh' && b[1] === 'Add calendar', `buttons: ${b.slice(0, 3)}`);
    expect(await page.locator('main time').count() > 0, 'no load time');
    const foot = await text(page.locator('main')); expect(foot.includes('1–15 of 61'), 'pager not 1–15 of 61');
    await page.getByRole('searchbox').fill('TARGET'); await page.waitForTimeout(1500);
    expect(await page.locator('table tbody tr').count() === 1, 'search rows != 1'); expect(page.url().includes('TARGET'), 'search not in address');
    await page.screenshot({ path: `${out}/02-list.png`, fullPage: true });
    return `crumbs, Refresh then Add (Add drawn ${addAfter} ms after the rows), load time, pager 1–15 of 61, search in the address`;
});
await step('Open a QuantLib calendar', async () => {
    await page.locator('table tbody tr').first().getByRole('cell').first().click(); await page.waitForURL(/calendars\/TARGET/); await page.waitForTimeout(1500);
    const b = await buttons(); expect(b.join(',') === 'Refresh,Derive a calendar' || (b[0] === 'Refresh' && b[1] === 'Derive a calendar' && !b.includes('Edit')), `buttons: ${b}`);
    const tabs = await page.getByRole('tab').allInnerTexts(); expect(tabs.join(',') === 'Details,Diary events,History', `tabs: ${tabs}`);
    const main = await text(page.locator('main')); expect(main.includes('Read only') && main.includes('QuantLib supplies'), 'mark or notice missing');
    await page.screenshot({ path: `${out}/03-quantlib.png`, fullPage: true });
    return `buttons ${b}; tabs ${tabs}`;
});
await step('Derive a calendar from TARGET', async () => {
    await page.getByRole('button', { name: 'Derive a calendar' }).click(); await page.getByRole('dialog').waitFor();
    const title = await page.getByRole('dialog').getByRole('heading', { level: 2 }).innerText(); expect(title === 'Derive a calendar from TARGET', `title: ${title}`);
    await page.getByRole('dialog').getByLabel('Code', { exact: true }).fill(CODE);
    await page.getByRole('dialog').getByLabel('Name', { exact: true }).fill('Acme scenario calendar');
    await page.getByRole('dialog').getByRole('button', { name: 'Add', exact: true }).click();
    await page.waitForURL(new RegExp('calendars/' + CODE.replace('.', '\\.')), { timeout: 15000 }); await page.waitForTimeout(2000);
    const b = await buttons(); expect(b.slice(0, 5).join(',') === 'Refresh,Derive a calendar,Rebuild business days,Edit,Delete', `buttons: ${b.slice(0, 6)}`);
    const tabs = await page.getByRole('tab').allInnerTexts(); expect(tabs.join(',') === 'Details,Exceptions,Diary events,Business days,History', `tabs: ${tabs}`);
    const main = await text(page.locator('main')); expect(main.includes('Editable') && main.includes('TARGET, with exceptions'), 'mark or holidays-from missing');
    await page.screenshot({ path: `${out}/04-derived.png`, fullPage: true });
    return `buttons ${b.slice(0, 5)}; tabs ${tabs}`;
});
await step('Add an exception and a diary event', async () => {
    await page.getByRole('tab', { name: 'Exceptions' }).click(); await page.waitForTimeout(800);
    await page.getByRole('button', { name: 'Add', exact: true }).click(); await page.getByRole('dialog').waitFor();
    const t1 = await page.getByRole('dialog').getByRole('heading', { level: 2 }).innerText();
    await page.getByRole('dialog').locator('input[type="date"]').fill('2026-06-12');
    await page.getByRole('dialog').getByLabel('Description', { exact: true }).fill('Office closed');
    await page.getByRole('dialog').getByRole('button', { name: 'Add', exact: true }).click(); await page.waitForTimeout(1500); await refresh();
    let main = await text(page.locator('main')); expect(main.includes('2026-06-12') && main.includes('Office closed'), 'exception row missing');
    await page.getByRole('tab', { name: 'Diary events' }).click(); await page.waitForTimeout(800);
    await page.getByRole('button', { name: 'Add', exact: true }).click(); await page.getByRole('dialog').waitFor();
    const t2 = await page.getByRole('dialog').getByRole('heading', { level: 2 }).innerText();
    await page.getByRole('dialog').locator('input[type="date"]').fill('2026-02-05');
    const sel = page.getByRole('dialog').locator('select').first(); await sel.selectOption({ index: 1 });
    await page.getByRole('dialog').getByLabel('Name', { exact: true }).fill('MPC decision');
    await page.getByRole('dialog').getByRole('button', { name: 'Add', exact: true }).click(); await page.waitForTimeout(1500); await refresh();
    main = await text(page.locator('main')); expect(main.includes('2026-02-05') && main.includes('MPC decision'), 'event row missing');
    await page.screenshot({ path: `${out}/05-events.png`, fullPage: true });
    return `dialogs: "${t1}", "${t2}"`;
});
await step('Edit a part and read its history', async () => {
    await page.getByRole('tab', { name: 'Exceptions' }).click(); await page.waitForTimeout(800);
    await page.locator('main table').getByRole('button', { name: 'Edit', exact: true }).first().click(); await page.getByRole('dialog').waitFor();
    const title = await page.getByRole('dialog').getByRole('heading', { level: 2 }).innerText(); expect(title === 'Edit 2026-06-12', `edit title: ${title}`);
    expect(await page.getByRole('dialog').locator('time').count() === 1, 'no loaded time in the part dialog');
    await page.getByRole('dialog').getByLabel('Description', { exact: true }).fill('Office closed for the move');
    await page.getByRole('dialog').getByRole('button', { name: 'Save', exact: true }).click(); await page.waitForTimeout(1500); await refresh();
    expect((await text(page.locator('main'))).includes('Office closed for the move'), 'row not updated');
    await page.locator('main table').getByRole('button', { name: 'History', exact: true }).first().click(); await page.getByRole('dialog').waitFor(); await page.waitForTimeout(2000);
    const ht = await page.getByRole('dialog').getByRole('heading', { level: 2 }).innerText(); expect(ht === 'History of 2026-06-12', `history title: ${ht}`);
    const d = await text(page.getByRole('dialog')); expect(d.includes('system activity') && !/Compare/.test(d), 'not the timeline');
    expect(await page.getByRole('dialog').locator('li[data-ms]').count() === 2, 'not two entries');
    await page.getByRole('dialog').getByText('Details', { exact: true }).first().click(); await page.waitForTimeout(500);
    await page.screenshot({ path: `${out}/06-part-history.png`, fullPage: true });
    const d2 = await text(page.getByRole('dialog')); expect(d2.includes('Office closed') && d2.includes('Office closed for the move'), 'old and new not shown');
    await page.getByRole('dialog').getByRole('button', { name: 'Close' }).click(); await page.waitForTimeout(400);
    return `"${title}", "${ht}", timeline with two entries, old and new values`;
});
await step('Build the business days and read them', async () => {
    await page.getByRole('tab', { name: 'Business days' }).click(); await page.waitForTimeout(2500);
    let main = await text(page.locator('main')); expect(main.includes('No business days are built for 2026') && main.includes('Rebuild the calendar to build them'), 'empty state wrong');
    await page.getByRole('button', { name: 'Rebuild business days' }).first().click(); await page.getByRole('dialog').waitFor();
    const d = await text(page.getByRole('dialog')); expect(d.includes('Build up to the end of') && d.includes('does not change a day'), 'rebuild dialog words');
    await page.getByRole('dialog').getByRole('button', { name: 'Rebuild', exact: true }).click();
    await page.getByRole('dialog').getByText(/\d+ days written/).waitFor({ timeout: 30000 });
    const d2 = await text(page.getByRole('dialog'));
    expect(await page.getByRole('dialog').getByRole('button', { name: 'Rebuild', exact: true }).isDisabled(), 'Rebuild still enabled');
    await page.getByRole('dialog').getByRole('button', { name: 'Cancel' }).click(); await page.waitForTimeout(2500);
    main = await text(page.locator('main')); expect(/\d+ business days and \d+ weekday holidays in 2026/.test(main), 'no summary');
    expect(main.includes('Office closed for the move') && main.includes('Exception'), 'June exception not shown with its producer');
    expect(main.includes('QuantLib, but the rebuild does not read them'), 'gap warning missing');
    const picker = page.getByLabel('Compare with').first(); await picker.click(); await page.waitForTimeout(400); await page.keyboard.type('TARGET'); await page.waitForTimeout(600); const opt = page.getByRole('option', { name: /TARGET/ }).first(); if (await opt.count()) await opt.click(); else await page.keyboard.press('Enter');
    await page.waitForTimeout(2500); main = await text(page.locator('main'));
    await page.screenshot({ path: `${out}/07-days.png`, fullPage: true });
    expect(main.includes('With TARGET'), 'the union sentence is not shown after choosing TARGET');
    return (d2.match(/\d+ days written/) ?? [''])[0] + '; union sentence shown';
});
await step('Amend the calendar against a stale version', async () => {
    const second = await ctx.newPage(); await second.goto(`${base}/refdata/calendars/${CODE}`); await second.waitForTimeout(2000);
    await second.getByRole('button', { name: 'Edit', exact: true }).click(); await second.getByRole('dialog').waitFor();
    await page.getByRole('tab', { name: 'Details' }).click(); await page.waitForTimeout(500);
    await page.getByRole('button', { name: 'Edit', exact: true }).click(); await page.getByRole('dialog').waitFor();
    const title = await page.getByRole('dialog').getByRole('heading', { level: 2 }).innerText(); expect(title === 'Edit Acme scenario calendar', `title: ${title}`);
    expect(await page.getByRole('dialog').locator('time').count() === 1, 'no loaded time');
    await page.getByRole('dialog').getByLabel('Name', { exact: true }).fill('Acme scenario calendar, renamed');
    await page.getByRole('dialog').getByRole('button', { name: 'Save', exact: true }).click(); await page.waitForTimeout(2500);
    await second.getByRole('dialog').getByLabel('Name', { exact: true }).fill('Acme scenario calendar, stale');
    await second.getByRole('dialog').getByRole('button', { name: 'Save', exact: true }).click(); await second.waitForTimeout(2500);
    const d = await text(second.getByRole('dialog')); expect(d.includes('changed this record after you opened it') && d.includes('Reload'), `stale: ${d.slice(-300)}`);
    await second.screenshot({ path: `${out}/08-stale.png`, fullPage: true });
    await second.getByRole('button', { name: 'Reload' }).click(); await second.waitForTimeout(2500);
    expect(await second.getByRole('dialog').count() === 0, 'dialog still open after Reload');
    const t2 = await text(second.locator('h1')); expect(t2.includes('renamed'), `second tab title: ${t2}`);
    const g0 = Date.now(); let gold = 0; while (Date.now() - g0 < 15000) { gold = await page.locator('.stale-mark').count(); if (gold) break; await page.waitForTimeout(500); } const goldAfter = gold ? `${Date.now() - g0} ms` : 'not within 15 s';
    await page.screenshot({ path: `${out}/08-gold.png`, fullPage: true });
    await page.getByRole('button', { name: 'Refresh', exact: true }).click(); await page.waitForTimeout(2000);
    await second.close();
    return `stale save refused; Reload closed the dialog; Refresh gold in the first tab after its own save: ${goldAfter}`;
});
await step("Read the calendar's history and revert", async () => {
    await page.getByRole('tab', { name: 'History' }).click(); await page.waitForTimeout(2500);
    const entries = await page.locator('main li[data-ms]').count(); expect(entries >= 2, `entries: ${entries}`);
    const main = await text(page.locator('main')); expect(main.includes('Hide system activity'), 'toggle missing');
    const reverts = await page.getByRole('button', { name: 'Revert' }).count(); expect(reverts === entries - 1, `revert buttons: ${reverts} for ${entries} entries`);
    await page.screenshot({ path: `${out}/09-history.png`, fullPage: true });
    await page.getByRole('button', { name: 'Revert' }).click(); await page.getByRole('dialog').waitFor(); await page.waitForTimeout(500);
    const d = await text(page.getByRole('dialog')); expect(d.includes('Revert to version 1'), `revert dialog: ${d.slice(0, 120)}`);
    await page.getByRole('dialog').getByRole('button', { name: 'Revert', exact: true }).click(); await page.waitForTimeout(2000); await refresh();
    const after = await page.locator('main li[data-ms]').count(); expect(after === entries + 1, `entries after revert: ${after}`);
    await page.getByRole('button', { name: 'Refresh', exact: true }).click(); await page.waitForTimeout(2000);
    const t = await text(page.locator('h1')); expect(t.trim().endsWith('Acme scenario calendar'), `title after revert: ${t}`);
    return `${entries} entries then ${entries + 1}; Revert on every version but the newest; title back to the old name`;
});
await step('Delete the calendar', async () => {
    await page.getByRole('tab', { name: 'Details' }).click(); await page.waitForTimeout(500);
    await page.getByRole('button', { name: 'Delete', exact: true }).first().click(); await page.getByRole('dialog').waitFor(); await page.waitForTimeout(500);
    const title = await page.getByRole('dialog').getByRole('heading', { level: 2 }).innerText();
    const d = await text(page.getByRole('dialog')); expect(d.includes('rules, exceptions and events'), 'warning missing');
    await page.getByRole('dialog').getByRole('button', { name: 'Delete', exact: true }).click(); await page.waitForTimeout(4000);
    expect(/\/refdata\/calendars\/?(\?.*)?$/.test(page.url()), `url after delete: ${page.url()}`);
    await page.getByRole('searchbox').fill(CODE.split('.')[0]); await page.waitForTimeout(1500);
    const main = await text(page.locator('main')); expect(main.includes('No holiday calendars match the search'), 'deleted calendar still listed');
    await page.goto(`${base}/refdata/calendars/${CODE}`); await page.waitForTimeout(2000);
    const gone = await text(page.locator('main')); expect(gone.includes('no such record'), `page by address: ${gone.slice(0, 200)}`);
    return `"${title}"; list opened; address answers no such record`;
});
await step('Look as a reader', async () => {
    await ctx.clearCookies(); await signIn('adrian.vance@acme_corporation');
    await page.goto(`${base}/refdata/calendars/TARGET`); await page.waitForTimeout(2000);
    const b = await buttons(); const main = await text(page.locator('main'));
    await page.screenshot({ path: `${out}/11-reader.png`, fullPage: true });
    expect(main.includes('Read only'), 'no Read only mark');
    expect(!b.includes('Edit') && !b.includes('Delete') && !b.includes('Rebuild business days'), `actions drawn for a reader: ${b}`);
    return `buttons ${b.slice(0, 5)}; mark Read only; no Edit, Delete or Rebuild`;
});
await browser.close();
console.log('\nSUMMARY'); for (const [n, r, w] of results) console.log(`${r}\t${n}\t${w}`);

// Render every state of the currencies prototype, report script errors, keep a screenshot of each.
import { chromium } from 'playwright';
import { mkdirSync } from 'node:fs';

const root = 'file:///home/marco/Development/OreStudio/ores_dev_brave_hopper/doc/prototypes/currencies/currencies-desk-groups-and-pairs/index.html';
const out = '/tmp/claude-1000/-home-marco-Development-OreStudio-ores-dev-brave-hopper/0517118c-2fba-4476-8f19-68c47d846811/scratchpad/shots';
mkdirSync(out, { recursive: true });
const states = [
    ['list', '?screen=currencies'],
    ['list-reader', '?screen=currencies&who=reader'],
    ['list-search-none', '?screen=currencies&q=zzz'],
    ['list-add', '?screen=currencies&dialog=add'],
    ['export', '?screen=currencies&dialog=export'],
    ['exported', '?screen=currencies&done=exported'],
    ['eur', '?screen=currency&code=EUR'],
    ['eur-countries', '?screen=currency&code=EUR&tab=countries'],
    ['eur-calendars', '?screen=currency&code=EUR&tab=calendars'],
    ['eur-groups', '?screen=currency&code=EUR&tab=groups'],
    ['eur-history', '?screen=currency&code=EUR&tab=history'],
    ['eur-edit', '?screen=currency&code=EUR&dialog=edit'],
    ['eur-delete', '?screen=currency&code=EUR&dialog=delete'],
    ['eur-add-country', '?screen=currency&code=EUR&tab=countries&dialog=add-link'],
    ['eur-remove-country', '?screen=currency&code=EUR&tab=countries&dialog=remove-link&link=DE'],
    ['eur-revert', '?screen=currency&code=EUR&tab=history&dialog=revert&v=1'],
    ['eur-reader', '?screen=currency&code=EUR&who=reader'],
    ['groups', '?screen=groups'],
    ['g11', '?screen=group&code=G11'],
    ['g11-members', '?screen=group&code=G11&tab=members'],
    ['g11-history', '?screen=group&code=G11&tab=history'],
    ['g11-edit', '?screen=group&code=G11&dialog=edit'],
    ['groups-add', '?screen=groups&dialog=add'],
    ['pairs', '?screen=pairs'],
    ['eurusd', '?screen=pair&code=EUR%2FUSD'],
    ['eurusd-calendars', '?screen=pair&code=EUR%2FUSD&tab=calendars'],
    ['eurusd-history', '?screen=pair&code=EUR%2FUSD&tab=history'],
    ['eurusd-edit', '?screen=pair&code=EUR%2FUSD&dialog=edit'],
    ['pairs-add', '?screen=pairs&dialog=add'],
    ['pairs-add-same', '?screen=pairs&dialog=add&legs=same'],
    ['import-1', '?screen=import&step=1'],
    ['import-1-wrong', '?screen=import&step=1&file=wrong'],
    ['import-2', '?screen=import&step=2&file=ok'],
    ['import-3', '?screen=import&step=3&file=ok'],
    ['import-3-clean', '?screen=import&step=3&file=ok&clean=1'],
    ['import-4', '?screen=import&step=4&file=ok'],
    ['import-reader', '?screen=import&who=reader'],
    ['imported', '?screen=currencies&done=imported'],
    ['guard', '?screen=currency&code=EUR&dialog=edit&guard=1'],
];
const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 1380, height: 900 } });
let failures = 0;
for (const [name, qs] of states) {
    const errors = [];
    page.removeAllListeners('pageerror');
    page.removeAllListeners('console');
    page.on('pageerror', (e) => errors.push(e.message));
    page.on('console', (m) => { if (m.type() === 'error') errors.push(m.text()); });
    await page.goto(root + qs);
    await page.waitForTimeout(100);
    const title = await page.title();
    const text = (await page.locator('main').innerText()).replace(/\s+/g, ' ').slice(0, 90);
    await page.screenshot({ path: `${out}/${name}.png`, fullPage: true });
    if (errors.length) failures++;
    console.log(`${errors.length ? 'FAIL' : 'ok  '} ${name.padEnd(20)} ${title.padEnd(32)} ${text}${errors.length ? '\n     ' + errors.join('\n     ') : ''}`);
}
await browser.close();
console.log(`${states.length} states, ${failures} with errors`);
process.exit(failures ? 1 : 0);

import { chromium } from '@playwright/test';

/*
 * Renders the clickable prototype so a screen can be compared with it, and
 * captures the list and one structure page side by side with the
 * implementation's own captures.
 */
const url =
    'file:///home/marco/Development/OreStudio/ores_dev_jolly_knuth/doc/prototypes/trades/structure-page/index.html';
const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 1740, height: 900 } });
await page.goto(url);
await page.waitForTimeout(600);
await page.screenshot({ path: 'shots/prototype-list.png', fullPage: true });

const row = page.locator('text=PKG-0004').first();
if ((await row.count()) > 0) {
    await row.click();
    await page.waitForTimeout(500);
}
await page.screenshot({ path: 'shots/prototype-page.png', fullPage: true });
await browser.close();

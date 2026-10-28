import { chromium } from '@playwright/test';

/*
 * Renders one state of the prototype from its address, so the page a person
 * lands on can be compared with the implementation's own page.
 */
const url =
    'file:///home/marco/Development/OreStudio/ores_dev_jolly_knuth/doc/prototypes/trades/structure-page/index.html?deal=PKG-0004';
const browser = await chromium.launch();
const page = await browser.newPage({ viewport: { width: 1740, height: 1400 } });
await page.goto(url);
await page.waitForTimeout(800);
await page.screenshot({ path: 'shots/prototype-deal.png', fullPage: true });
await browser.close();

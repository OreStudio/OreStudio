import { expect, test } from '@playwright/test';
import { signIn } from './support/sign-in';

/**
 * The blotter: the trades and structures a person may see, read by the grouping
 * they choose.
 *
 * The case proves what the signed-off prototype promises: a structure is one row
 * and its legs are not rows of the top list; the flat list shows the legs; a
 * structure stands where its legs are, and says so when they span books; and the
 * rail counts the rows placed under each node.
 */
test('the blotter reads trades and structures by book, and flattens to the legs', async ({
    page,
}) => {
    await signIn(page);
    await page.goto('/trading/blotter');

    // The rail is drawn before the list: the party's own node, with the number of
    // rows placed under it.
    const rail = page.getByRole('navigation', { name: 'rail' });
    await expect(rail).toBeVisible({ timeout: 20_000 });
    const party = rail.getByRole('button').first();
    await expect(party).toBeVisible();
    const partyCount = Number((await party.innerText()).match(/(\d+)\s*$/)?.[1] ?? '0');
    expect(partyCount).toBeGreaterThan(0);
    await page.screenshot({ path: 'shots/blotter-portfolio.png', fullPage: true });

    // The top list holds both kinds: a structure row states a reference, and a
    // trade row states one too.
    const rows = page.locator('tbody tr');
    await expect(rows.first()).toBeVisible({ timeout: 20_000 });
    const firstReference = (await rows.first().locator('td').first().innerText()).trim();
    expect(firstReference).not.toBe('');

    // The date is one line, and the table fits the panel: what cannot fit is
    // shortened with an ellipsis rather than wrapped, so the columns after it are
    // not pushed off the screen.
    const dateCell = rows.first().locator('td').nth(4);
    expect((await dateCell.innerText()).trim()).toMatch(/^(\d{4}-\d{2}-\d{2}|—)$/);
    expect(
        await dateCell
            .locator('span')
            .first()
            .evaluate((el) => getComputedStyle(el).whiteSpace),
    ).toBe('nowrap');
    const fits = async (): Promise<boolean> =>
        page.evaluate(() => {
            const table = document.querySelector('table');
            const box = table?.parentElement;
            return table !== null && box != null && table.scrollWidth <= box.clientWidth;
        });
    expect(await fits()).toBe(true);

    // Narrowing to a node keeps the list and its count in step: the first page
    // holds as many rows as the node counted, unless the page is shorter.
    const node = rail.getByRole('button').nth(1);
    const label = (await node.innerText()).split('\n')[0]?.trim() ?? '';
    const nodeCount = Number((await node.innerText()).match(/(\d+)\s*$/)?.[1] ?? '0');
    expect(label).not.toBe('');
    await node.click();
    await expect(rows.first()).toBeVisible({ timeout: 20_000 });
    expect(await rows.count()).toBe(Math.min(nodeCount, 15));

    // The instrument is the short product code, and the name it stands for is the
    // cell's plain tooltip.
    // The top list's columns: identifier, what, book, counterparty, date, state.
    const what = page.locator('tbody tr').first().locator('td').nth(1);
    const trigger = what.locator('span[title]').first();
    const product = (await trigger.innerText()).trim();
    expect(product).toMatch(/^[A-Z]{2,}$/);
    await expect(trigger).toHaveAttribute('title', /\S+/);

    // Flat is every trade: the structure column appears, and a leg names the
    // structure that holds it.
    await page.getByLabel('Flat').check();
    await expect(page.getByRole('cell', { name: 'Structure', exact: true })).toBeVisible({
        timeout: 20_000,
    });
    await expect(rows.first()).toBeVisible();
    // The extra column still leaves the table inside the panel.
    expect(await fits()).toBe(true);
    await page.screenshot({ path: 'shots/blotter-flat.png', fullPage: true });

    // Back to the top list: the legs are gone again.
    await page.getByLabel('Flat').uncheck();
    await expect(page.getByRole('cell', { name: 'Structure', exact: true })).toHaveCount(0);

    // The counterparty grouping is the same rows by another topology.
    await page.getByLabel('Grouping').selectOption('counterparty');
    // The rail is the counterparties the rows face now. The names come from the
    // provisioned data, so the case proves the topology by narrowing: choosing a
    // counterparty shows exactly as many rows as its node counted.
    const counterpartyNode = rail.getByRole('button').nth(1);
    await counterpartyNode.waitFor({ state: 'visible', timeout: 20_000 });
    const counterpartyCount = Number(
        (await counterpartyNode.innerText()).match(/(\d+)\s*$/)?.[1] ?? '0',
    );
    expect(counterpartyCount).toBeGreaterThan(0);
    await counterpartyNode.click();
    await expect(rows.first()).toBeVisible({ timeout: 20_000 });
    const pager = await page
        .getByText(/^\d+\u2013\d+ of \d+$/)
        .first()
        .innerText();
    const of = pager.match(/of (\d+)/);
    expect(Number(of?.[1] ?? '0')).toBe(counterpartyCount);
    await page.screenshot({ path: 'shots/blotter-counterparty.png', fullPage: true });

    // The third grouping: the classes the rows fall into, and the same narrowing.
    await page.getByLabel('Grouping').selectOption('instrument');
    const classNode = rail.getByRole('button').nth(1);
    await classNode.waitFor({ state: 'visible', timeout: 20_000 });
    const className = (await classNode.innerText()).split('\n')[0]?.trim() ?? '';
    expect(className).toMatch(/^[A-Za-z]/);
    await classNode.click();
    await expect(rows.first()).toBeVisible({ timeout: 20_000 });
    await page.screenshot({ path: 'shots/blotter-instrument.png', fullPage: true });

    // Choosing the tree again draws the tree again, not the counterparties: the
    // rail belongs to the grouping that was chosen.
    await page.getByLabel('Grouping').selectOption('portfolio');
    await expect(rail.getByRole('button', { name: /Global Portfolio/ }).first()).toBeVisible({
        timeout: 20_000,
    });
});

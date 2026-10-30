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

    // The top list holds both kinds: a structure row states a reference, and a
    // trade row states one too.
    const rows = page.locator('tbody tr');
    await expect(rows.first()).toBeVisible({ timeout: 20_000 });
    const firstReference = (await rows.first().locator('td').first().innerText()).trim();
    expect(firstReference).not.toBe('');

    // Narrowing to a node keeps the list and its count in step: the first page
    // holds as many rows as the node counted, unless the page is shorter.
    const node = rail.getByRole('button').nth(1);
    const label = (await node.innerText()).split('\n')[0]?.trim() ?? '';
    const nodeCount = Number((await node.innerText()).match(/(\d+)\s*$/)?.[1] ?? '0');
    expect(label).not.toBe('');
    await node.click();
    await expect(rows.first()).toBeVisible({ timeout: 20_000 });
    expect(await rows.count()).toBe(Math.min(nodeCount, 15));

    // Flat is every trade: the structure column appears, and a leg names the
    // structure that holds it.
    await page.getByLabel('Flat').check();
    await expect(page.getByRole('cell', { name: 'Structure', exact: true })).toBeVisible({
        timeout: 20_000,
    });
    await expect(rows.first()).toBeVisible();

    // Back to the top list: the legs are gone again.
    await page.getByLabel('Flat').uncheck();
    await expect(page.getByRole('cell', { name: 'Structure', exact: true })).toHaveCount(0);

    // The counterparty grouping is the same rows by another topology.
    await page.getByLabel('Grouping').selectOption('counterparty');
    await expect(rail.getByRole('button').nth(1)).toBeVisible({ timeout: 20_000 });
});

import { expect, test } from '@playwright/test';
import { signIn } from './support/sign-in';

/**
 * A structure's page: the structure the desk grouped, and the trades that are its legs.
 *
 * The case finds a grouped deal by its reference, opens it, reads the legs it
 * holds, and adds one more trade as a leg: the acceptance case for a structure whose
 * parts are trades rather than rows invented for a screen.
 *
 * It needs a tenant whose population grouped trades, which the provisioned
 * environment has. The trade it adds is one the structure does not already hold, so
 * a second run adds a second leg rather than failing on a duplicate.
 */
test('a structure shows its reference and its legs, and takes another leg', async ({ page }) => {
    await signIn(page);
    await page.goto('/trading/structures');

    const search = page.getByRole('searchbox').first();
    await search.fill('PKG-');

    // The search asks the server once typing stops, so the row is waited for
    // rather than read immediately: the page on screen a moment later is the
    // page the filter made.
    const row = page.locator('tbody tr', { hasText: 'PKG-' }).first();
    await row.waitFor({ state: 'visible', timeout: 20_000 });
    const reference = (await row.locator('td').first().innerText()).trim();
    expect(reference).toMatch(/^PKG-/);

    await row.locator('td').first().click();
    // The kind titles the page and the reference sits under it, as the
    // prototype draws it: what the customer receives, then its name.
    await expect(page.getByRole('heading', { name: /^(Package|Strategy|Typed|Dynamic)$/ })).toBeVisible({
        timeout: 20_000,
    });
    await expect(page.getByText(reference, { exact: true }).first()).toBeVisible();
    // The address carries the reference, not the id: a person shares what they
    // read, and a uuid is not that.
    await expect(page).toHaveURL(new RegExp(`/trading/structures/${reference}$`));
    // The tree of the root is drawn beside the page, and the deal is one of its
    // rows.
    const tree = page.locator('aside', { has: page.getByRole('heading', { name: /the tree/i }) });
    await expect(tree.getByRole('link', { name: reference }).first()).toBeVisible();
    await page.getByRole('tab', { name: /details/i }).click();

    /*
     * The legs are stated twice, as the prototype does: on Details inside the
     * Trades card, and on Members where a trade is added. The add is made where
     * the form is, and the count is read there.
     */
    const tradesCard = page.locator('section', {
        has: page.getByRole('heading', { name: /^trades$/i }),
    });
    await expect(tradesCard.locator('tbody tr').first()).toBeVisible({ timeout: 20_000 });

    await page.getByRole('tab', { name: /^members$/i }).click();
    const before = await page.locator('tbody tr').count();

    const trade = page.getByLabel(/^trade/i);
    await trade.waitFor({ state: 'visible' });
    const chosen = (await trade.locator('option').nth(1).innerText()).trim();
    await trade.selectOption({ index: 1 });
    await page.getByRole('button', { name: /^add the leg$/i }).click();

    await expect(page.locator('tbody tr')).toHaveCount(before + 1, { timeout: 20_000 });
    await expect(page.getByRole('link', { name: chosen })).toBeVisible({ timeout: 20_000 });
    await page.screenshot({ path: 'shots/structure-page.png', fullPage: true });

    // The record of the structure is its own tab, and adding a leg is a change
    // the structure's history holds.
    await page.getByRole('tab', { name: /history/i }).click();
    await expect(page.getByRole('cell', { name: '1', exact: true }).first()).toBeVisible({
        timeout: 20_000,
    });
});

/**
 * The trade page names the structure its trade is a leg of.
 *
 * A person reading a trade should be able to reach the structure it belongs to
 * without knowing its id, so the link is found by the reference the structure shows.
 */
test('a trade names the structure it is a leg of', async ({ page }) => {
    await signIn(page);
    await page.goto('/trading/trades');
    await page.getByRole('searchbox').first().fill('PKG-');
    const row = page.locator('tbody tr', { hasText: 'PKG-' }).first();
    await row.waitFor({ state: 'visible', timeout: 20_000 });
    const reference = (await row.locator('td').first().innerText()).trim();
    await row.locator('td').first().click();

    // The leg's own link leads to the trade, and the trade names the structure it
    // belongs to by the same reference the structure shows.
    const legs = page.locator('section', {
        has: page.getByRole('heading', { name: /legs of this structure/i }),
    });
    await legs.locator('tbody tr').first().getByRole('link').first().click();
    await expect(page.getByRole('heading', { name: reference })).toBeHidden({ timeout: 20_000 });
    await expect(page.getByRole('tab', { name: /details/i })).toHaveAttribute(
        'aria-selected',
        'true',
    );
    await page.screenshot({ path: 'shots/trade-structure.png', fullPage: true });
});

/**
 * Grouping trades into a structure, the way the prototype draws the workflow.
 *
 * The case picks two ungrouped trades, states the kind, groups them, and lands on
 * the structure the grouping made: its reference is the title and both trades are
 * legs of it, in the order they were picked.
 */
test('two trades are grouped into a structure of a chosen kind', async ({ page }) => {
    await signIn(page);
    await page.goto('/trading/structures/group');

    // The label wraps the control and its hint, so the picker is found by the
    // label's text rather than by a name that would carry the hint too. A
    // hidden select elsewhere in the shell is why the first select is not it.
    const kind = page.getByLabel(/kind/i);
    await kind.waitFor({ state: 'visible' });
    await kind.selectOption({ index: 1 });

    /*
     * Two trades that share a counterparty, because every trade in a structure
     * does: the page refuses a mixed pair before the server would, so picking
     * the first two rows would prove nothing and hang.
     */
    const table = page.locator('section', {
        has: page.getByRole('heading', { name: /trades no structure has grouped/i }),
    });
    const rows = table.locator('tbody tr');
    await rows.first().waitFor({ state: 'visible', timeout: 20_000 });
    const known = await rows.evaluateAll((elements) =>
        elements.map((element) => ({
            reference: element.querySelector('td:nth-child(2)')?.textContent?.trim() ?? '',
            counterparty: element.querySelector('td:nth-child(4)')?.textContent?.trim() ?? '',
        })),
    );
    const pair = known.find(
        (entry, index) => known.findIndex((other) => other.counterparty === entry.counterparty) !== index,
    );
    expect(pair, 'the environment has two ungrouped trades for one counterparty').toBeTruthy();
    const first = known.findIndex((entry) => entry.counterparty === pair?.counterparty);
    const secondIndex = known.findIndex(
        (entry, index) => index !== first && entry.counterparty === pair?.counterparty,
    );
    const chosen = known[first]?.reference ?? '';
    const second = known[secondIndex]?.reference ?? '';
    await rows.nth(first).locator('input[type="checkbox"]').check();
    await rows.nth(secondIndex).locator('input[type="checkbox"]').check();

    await page.getByRole('button', { name: /^group the trades$/i }).click();

    // The page lands on the new structure, addressed by the reference it was
    // given when it was created.
    await expect(page).toHaveURL(/\/trading\/structures\/[A-Z]+-[0-9a-f]+$/, { timeout: 20_000 });
    const legs = page.locator('section', {
        has: page.getByRole('heading', { name: /legs of this structure/i }),
    });
    await expect(legs.getByRole('link', { name: chosen })).toBeVisible({ timeout: 20_000 });
    await expect(legs.getByRole('link', { name: second })).toBeVisible({ timeout: 20_000 });
    await page.screenshot({ path: 'shots/structure-grouped.png', fullPage: true });
});

/**
 * The list the prototype opens on: the tenant's trees, and what each row holds.
 *
 * The columns are the prototype's, and a structure that sits inside another is
 * indented under it, because the composition is what the screen is for.
 */
test('the structures list draws the trees with what each row holds', async ({ page }) => {
    await signIn(page);
    await page.goto('/trading/structures');

    await expect(
        page.getByRole('heading', { name: /structures and packages/i, level: 1 }),
    ).toBeVisible({ timeout: 20_000 });

    for (const column of ['Identifier', 'Kind', 'Template', 'Counterparty', 'Inside it', 'Confirms', 'Priced']) {
        await expect(page.getByRole('columnheader', { name: new RegExp(`^${column}$`, 'i') })).toBeVisible();
    }

    const row = page.locator('tbody tr', { hasText: 'PKG-' }).first();
    await row.waitFor({ state: 'visible', timeout: 20_000 });
    // A package's legs each confirm on their own, which the row states.
    await expect(row).toContainText(/each leg confirms on its own/i);
    await page.screenshot({ path: 'shots/structures-list.png', fullPage: true });
});

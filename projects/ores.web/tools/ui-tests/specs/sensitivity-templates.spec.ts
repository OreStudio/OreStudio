import { expect, test, type Page } from '@playwright/test';
import { signIn } from './support/sign-in';

/**
 * The sensitivity templates, driven the way a person drives them.
 *
 * The tenant administrator is used because the screen writes: the seeded keys
 * are the tenant's, and a person who only reads is offered no action to try.
 * A key the run adds carries the run's own stamp, so a run that dies before it
 * cleans up leaves a row a person can see and remove rather than a name that
 * collides with the next run.
 *
 * The screenshots are written under build/output/ui-tests/, which is the build
 * tree and not the source tree.
 */

const SHOTS = '/home/marco/Development/OreStudio/ores_dev_solid_dirac/build/output/ui-tests';
const CODE = `PW_SENS_${Date.now().toString(36).toUpperCase()}`;
const TEMPLATE = `${CODE}_FD`;
const DESCRIPTION = 'A key the browser run writes.';
const CORRECTED = 'A key the browser run corrected.';
const LIST = '/reporting/sensitivity-templates';

/**
 * Removes the keys an earlier broken run of this spec left, so the run starts
 * from the tenant's own 19 and can be repeated.
 */
async function removeLeftovers(page: Page): Promise<void> {
    for (let attempt = 0; attempt < 20; attempt++) {
        if (page.url() !== `http://127.0.0.1:21002${LIST}`) {
            await page.goto(LIST);
        }
        await page.getByRole('searchbox').fill('PW_SENS');
        await expect
            .poll(async () => page.getByRole('cell', { name: /^PW_SENS_/ }).count(), {
                timeout: 30_000,
            })
            .toBeGreaterThan(-1);
        const leftover = page.getByRole('cell', { name: /^PW_SENS_/ });
        if ((await leftover.count()) === 0) {
            await page.getByRole('searchbox').fill('');
            return;
        }
        await leftover.first().click();
        await page.getByRole('button', { name: 'Delete' }).click();
        const dialog = page.getByRole('dialog');
        await dialog.getByLabel('Why').selectOption({ label: 'User/Booking Error' });
        await dialog.getByRole('button', { name: 'Delete', exact: true }).click();
        await expect(page.getByRole('heading', { name: 'Sensitivity templates' })).toBeVisible({
            timeout: 30_000,
        });
    }
}

test('the list shows the seeded templates and the run adds, corrects and removes one', async ({
    page,
}) => {
    test.setTimeout(240_000);
    const refusals: string[] = [];
    page.on('response', (response) => {
        if (
            response.url().includes('/api/analytics/sensitivity-templates') &&
            response.status() >= 400
        ) {
            refusals.push(`${response.status()} ${response.url()}`);
        }
    });

    // The tenant administrator works in several parties; this environment
    // provisions the tenant with a Hong Kong party.
    await signIn(page, { party: /ACME Corporation HK Ltd/ });

    await page.goto(LIST);
    await expect(page.getByRole('heading', { name: 'Sensitivity templates' })).toBeVisible();
    await removeLeftovers(page);
    // The cleanup leaves its own search in the address, and the tenant's own
    // keys are what the run reads next: 19 of them, paged at 15.
    await page.getByRole('searchbox').fill('');
    await expect(page.getByText(/1–15 of 19/)).toBeVisible();
    await expect(page.getByRole('cell', { name: 'COMM_Analytical', exact: true })).toBeVisible();
    await expect(page.getByRole('cell', { name: 'IR_Analytical', exact: true })).toBeVisible();
    await page.screenshot({ path: `${SHOTS}/01-list.png`, fullPage: true });

    // The second page holds the rest of the seeded keys.
    await page.getByRole('button', { name: /Next/ }).click();
    await expect(page.getByText(/16–19 of 19/)).toBeVisible();
    await expect(page.getByRole('cell', { name: 'IR_FD', exact: true })).toBeVisible();
    await expect(page.getByRole('cell', { name: 'SensiTrade', exact: true })).toBeVisible();
    await page.screenshot({ path: `${SHOTS}/02-list-second-page.png`, fullPage: true });

    // A search reaches the server and finds one key by its description.
    await page.getByRole('searchbox').fill('Bermudan');
    await expect(page.getByText(/1–1 of 1/)).toBeVisible();
    await expect(page.getByRole('cell', { name: 'IR_FD', exact: true })).toBeVisible();
    await page.screenshot({ path: `${SHOTS}/03-search.png`, fullPage: true });
    await page.getByRole('searchbox').fill('');
    await expect(page.getByText(/1–15 of 19/)).toBeVisible();

    // Add one key of the tenant's own. Every field is named by the dialog, so
    // each locator is scoped to it rather than to the page behind it.
    await page.getByRole('button', { name: 'New template' }).click();
    const adding = page.getByRole('dialog');
    await expect(adding).toBeVisible();
    await adding.getByLabel('Code').fill(TEMPLATE);
    await adding.getByLabel('Asset class').selectOption('IR');
    await adding.getByLabel('Method').selectOption('Finite difference');
    // The token follows from the method, and is stated rather than typed.
    await expect(adding).toContainText('FD');
    await adding.getByLabel('Description').fill(DESCRIPTION);
    await page.screenshot({ path: `${SHOTS}/04-add-dialog.png`, fullPage: true });
    await adding.getByRole('button', { name: 'Add', exact: true }).click();

    // The new key's page opens, and it states what it does not decide.
    await expect(page.getByRole('heading', { name: TEMPLATE })).toBeVisible({ timeout: 30_000 });
    await expect(page.getByText(/The shift sizes a key implies are set by/)).toBeVisible();
    await expect(page.getByText(DESCRIPTION)).toBeVisible();
    await page.screenshot({ path: `${SHOTS}/05-record.png`, fullPage: true });

    // Correct its description, with a reason and a commentary.
    await page.getByRole('button', { name: 'Edit' }).click();
    const editing = page.getByRole('dialog');
    await expect(editing).toBeVisible();
    await editing.getByLabel('Description').fill(CORRECTED);
    await editing.getByLabel('Why').selectOption({ label: 'User/Booking Error' });
    await editing.getByLabel('Commentary').fill('The run corrected this key.');
    await page.screenshot({ path: `${SHOTS}/06-edit-dialog.png`, fullPage: true });
    await editing.getByRole('button', { name: 'Save' }).click();
    await expect(page.getByText(CORRECTED)).toBeVisible({ timeout: 30_000 });

    // The change is one more version, and the history names it.
    await page.getByRole('tab', { name: 'History' }).click();
    await expect(page.getByText('v2')).toBeVisible({ timeout: 30_000 });
    await expect(page.getByText('The run corrected this key.')).toBeVisible();
    await page.screenshot({ path: `${SHOTS}/07-history.png`, fullPage: true });

    // Remove it, and say why.
    await page.getByRole('button', { name: 'Delete' }).click();
    const removing = page.getByRole('dialog');
    await expect(removing).toContainText('its versions stay in the history');
    await removing.getByLabel('Why').selectOption({ label: 'User/Booking Error' });
    await removing.getByLabel('Commentary').fill('The run is finished with this key.');
    await page.screenshot({ path: `${SHOTS}/08-delete-dialog.png`, fullPage: true });
    await removing.getByRole('button', { name: 'Delete', exact: true }).click();

    // Back on the list, the tenant's own 19 keys, and the key the run made gone.
    await expect(page.getByRole('heading', { name: 'Sensitivity templates' })).toBeVisible({
        timeout: 30_000,
    });
    await expect(page.getByText(/1–15 of 19/)).toBeVisible();
    await expect(page.getByRole('cell', { name: TEMPLATE, exact: true })).toHaveCount(0);
    await page.screenshot({ path: `${SHOTS}/09-list-after-delete.png`, fullPage: true });

    // None of the seeded keys moved: every one still answers, and its words are
    // the words it was seeded with.
    await page.getByRole('searchbox').fill('IR_FD');
    await expect(page.getByRole('cell', { name: 'IR_FD', exact: true })).toBeVisible();
    await expect(page.getByText(/Bermudan swaptions/)).toBeVisible();
    await page.getByRole('cell', { name: 'IR_FD', exact: true }).click();
    await expect(page.getByRole('heading', { name: 'IR_FD' })).toBeVisible();
    await expect(
        page.getByText('Interest rate products priced on a finite difference grid'),
    ).toBeVisible();
    await expect(page.getByText('IR_FD · version 1')).toBeVisible();
    await page.screenshot({ path: `${SHOTS}/10-seeded-key-untouched.png`, fullPage: true });

    expect(refusals, `the templates route refused: ${refusals.join(' | ')}`).toHaveLength(0);
});

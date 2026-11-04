/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { expect, test, type Page } from '@playwright/test';

/**
 * Scheduling one report from its own page, and taking the schedule away again.
 *
 * The check leaves the report it acts on as it found it: a schedule is what
 * makes a report fire, so a check that stopped halfway would leave a demo
 * tenant running a report nobody asked for. The schedule it writes is the
 * builder's first common one, so no clock is involved.
 */

/** The party the reports of this check belong to, which the session must act for. */
const PARTY = 'ACME Corporation US Inc';

/** A party the same account works in, and whose reports the session does not act for. */
const OTHER_PARTY = 'ACME Corporation UK plc';

/**
 * Acts for the party the report belongs to.
 *
 * A schedule is made for the definition's party: its runs act there, under a
 * grant the person consents to in that party. The party a session acts for
 * outlives the browser, so a session already acting for it needs no change.
 */
async function actFor(page: Page, party: string): Promise<void> {
    await page.goto('/');
    if (await page.getByText(`Party: ${party}`).isVisible()) {
        return;
    }
    await page.getByRole('button', { name: 'Your account' }).click();
    await page.getByText('Change party', { exact: true }).click();
    await page.getByRole('dialog').getByRole('button', { name: party, exact: true }).click();
    await expect(page.getByText(`Party: ${party}`)).toBeVisible();
}

/** The first report the list states as on demand, which is a report nothing fires. */
async function openOnDemandReport(page: Page): Promise<void> {
    await page.goto('/reporting/reports');
    const row = page
        .getByRole('row')
        .filter({ has: page.getByRole('cell', { name: 'On demand', exact: true }) })
        .first();
    await expect(row).toBeVisible();
    await row.click();
    await expect(page).toHaveURL(/\/reporting\/reports\/[0-9a-f-]{36}$/);
}

test('schedules an on-demand report from its own page, and unschedules it', async ({ page }) => {
    await actFor(page, PARTY);
    await openOnDemandReport(page);
    const onDemand = page.getByText('On demand', { exact: true }).first();
    await expect(onDemand).toBeVisible();

    // The report holds no schedule, so the page offers the one action that gives
    // it one, and the dialog opens the builder on a common schedule.
    await page.getByRole('button', { name: 'Schedule', exact: true }).click();
    const dialog = page.getByRole('dialog');
    await dialog.getByRole('button', { name: 'Change the schedule' }).click();
    await page
        .getByRole('dialog', { name: 'Schedule' })
        .getByRole('button', { name: 'Save', exact: true })
        .click();
    await dialog.getByRole('button', { name: 'Schedule', exact: true }).click();

    // The outcome is the operation's own count, stated in words and not dressed
    // up as a success of its own.
    await expect(page.getByText(/^Scheduled\./)).toBeVisible();
    await expect(page.getByRole('button', { name: 'Unschedule', exact: true })).toBeVisible();

    // Taking the schedule away is what returns the report to on demand.
    await page.getByRole('button', { name: 'Unschedule', exact: true }).click();
    await expect(page.getByText(/^Unscheduled\./)).toBeVisible();
    await expect(onDemand).toBeVisible();
    await expect(page.getByRole('button', { name: 'Schedule', exact: true })).toBeVisible();
});

test('offers no schedule for a report of a party the session does not act for', async ({
    page,
}) => {
    await actFor(page, PARTY);
    await page.goto('/reporting/reports');
    // The list opens on the party the session acts for, so another party's reports
    // are reached by the filter. Acting for one party schedules only its reports.
    await page.getByRole('combobox', { name: 'Party' }).selectOption({ label: OTHER_PARTY });
    const row = page
        .getByRole('row')
        .filter({ has: page.getByRole('cell', { name: 'On demand' }) })
        .first();
    await expect(row).toBeVisible();
    await row.click();
    await expect(page).toHaveURL(/\/reporting\/reports\/[0-9a-f-]{36}$/);

    await expect(
        page.getByText(new RegExp(`^This report belongs to ${OTHER_PARTY}\\.`)),
    ).toBeVisible();
    await expect(page.getByRole('button', { name: 'Schedule', exact: true })).toBeDisabled();
});

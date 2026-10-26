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

/**
 * The address of every screen that belongs to an area, in one place.
 *
 * A screen is under the address of its area, so the address and the breadcrumb
 * say the same thing. See the Menu structure page for the area of each screen.
 * No screen writes its own address as text: it asks this table, so moving a
 * screen is one change.
 */
export const PATHS = {
    refdata: '/refdata',
    calendars: '/refdata/calendars',
    counterparties: '/refdata/counterparties',
    books: '/refdata/books',
    trading: '/trading',
    trades: '/trading/trades',
    newTrade: '/trading/trades/new',
    parties: '/refdata/parties',
    newParty: '/refdata/parties/new',
    onboardCounterparty: '/refdata/counterparties/onboard',
    bookStructure: '/refdata/books/structure',
    conventions: '/refdata/conventions',
    conventionAuthoring: '/refdata/conventions/define',
    tenors: '/refdata/tenors',
    tenorAuthoring: '/refdata/tenors/author',
    organisation: '/organisation',
    people: '/organisation/people',
    hierarchy: '/organisation/hierarchy',
    roles: '/organisation/roles',
    rescue: '/organisation/rescue',
    operations: '/operations',
    signIns: '/operations/sign-ins',
    marketDataSources: '/operations/market-data-sources',
    synthetic: '/market-data/synthetic',
    subscriptions: '/market-data/subscriptions',
    fxFeed: '/market-data/synthetic/fx',
    irFeed: '/market-data/synthetic/ir',
    reporting: '/reporting',
    reports: '/reporting/reports',
    configurations: '/reporting/configurations',
    pricingEngines: '/reporting/pricing-engines',
    pricingEngineAuthoring: '/reporting/pricing-engines/author',
    engineSettings: '/reporting/engine-settings',
} as const;

/** The address of the documents of one convention kind. */
export function conventionPath(family: string): string {
    return `${PATHS.conventions}/${encodeURIComponent(family)}`;
}

/** The address of one person's page. */
export function personPath(username: string): string {
    return `${PATHS.people}/${encodeURIComponent(username)}`;
}

/** The address of one role's page. */
export function rolePath(roleId: string): string {
    return `${PATHS.roles}/${encodeURIComponent(roleId)}`;
}

/** The node list narrowed to the nodes that run on one machine. */
export function gridMachinePath(machine: string): string {
    return `/operations/grid?tab=nodes&machine=${encodeURIComponent(machine)}`;
}

/** The address of the page of one job: the workunit and the runs of it. */
export function gridJobPath(workunitId: string): string {
    return `/operations/grid/jobs/${encodeURIComponent(workunitId)}`;
}

/** The address of the Jobs tab with a batch open and selected. */
export function gridBatchPath(batchId: string): string {
    const query = new URLSearchParams({ tab: 'jobs', open: batchId, sel: `batch:${batchId}` });
    return `/operations/grid?${query.toString()}`;
}

/** The address of the page of one app version of the compute catalogue. */
export function gridAppVersionPath(appVersionId: string): string {
    return `/operations/grid/app-versions/${encodeURIComponent(appVersionId)}`;
}

/** The address of the page of the report run that dispatched a batch. */
export function gridReportRunPath(reportInstanceId: string): string {
    return `/operations/grid/report-runs/${encodeURIComponent(reportInstanceId)}`;
}

/** The address of the page of one compute node. */
export function gridNodePath(hostId: string): string {
    return `/operations/grid/nodes/${encodeURIComponent(hostId)}`;
}

/**
 * The address of one report definition's page.
 *
 * The definition is named by its id, because its name is not unique: the record
 * is unique on (tenant, name, party), so a person who works in several parties
 * sees several definitions of one name. The id is the primary key, so the
 * address names one record exactly.
 */
/** The page of one trade. */
export function tradePath(id: string): string {
    return `/trading/trades/${encodeURIComponent(id)}`;
}

/** The page of one book. */
export function bookPath(id: string): string {
    return `/refdata/books/${encodeURIComponent(id)}`;
}

/** The page of one counterparty. */
export function counterpartyPath(id: string): string {
    return `/refdata/counterparties/${encodeURIComponent(id)}`;
}

/** The reporting area's page for one run. */
export function reportRunPath(reportInstanceId: string): string {
    return `/reporting/runs/${encodeURIComponent(reportInstanceId)}`;
}

export function reportPath(id: string): string {
    return `${PATHS.reports}/${encodeURIComponent(id)}`;
}

/** The address of one pricing engine configuration's page. */
export function pricingEnginePath(id: string): string {
    return `${PATHS.pricingEngines}/${encodeURIComponent(id)}`;
}

/** The address of one engine settings configuration's page. */
export function engineSettingsPath(id: string): string {
    return `${PATHS.engineSettings}/${encodeURIComponent(id)}`;
}

/** The address of one report configuration's page. */
export function configurationPath(id: string): string {
    return `${PATHS.configurations}/${encodeURIComponent(id)}`;
}

/** The address of the rescue screen for one account. */
export function rescuePath(username: string): string {
    return `${PATHS.rescue}?username=${encodeURIComponent(username)}`;
}

/**
 * The addresses screens had before they moved under their area, and where each
 * one goes now. A saved link and a notification that names an earlier address
 * still arrive. A `:name` in the target is filled from the same name in the
 * earlier address.
 */
export const MOVED: readonly (readonly [from: string, to: string])[] = [
    ['/parties', PATHS.parties],
    ['/parties/new', PATHS.newParty],
    ['/counterparties/onboard', PATHS.onboardCounterparty],
    ['/books/structure', PATHS.bookStructure],
    ['/conventions', PATHS.conventions],
    ['/staff', PATHS.people],
    ['/people', PATHS.people],
    ['/people/:username', `${PATHS.people}/:username`],
    ['/hierarchy', PATHS.hierarchy],
    ['/reporting-lines', PATHS.hierarchy],
    ['/roles', PATHS.roles],
    ['/roles/:roleId', `${PATHS.roles}/:roleId`],
    ['/rescue', PATHS.rescue],
    ['/audit', PATHS.signIns],
    ['/reporting/:name', `${PATHS.reports}/:name`],
];

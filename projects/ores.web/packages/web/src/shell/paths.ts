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
    blotter: '/trading/blotter',
    trades: '/trading/trades',
    newTrade: '/trading/trades/new',
    structures: '/trading/structures',
    groupStructures: '/trading/structures/group',
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
    publishedCurves: '/market-data/published-curves',
    curveBlueprints: '/market-data/curve-blueprints',
    subscriptions: '/market-data/subscriptions',
    officialRates: '/market-data/official-rates',
    fxFeed: '/market-data/synthetic/fx',
    irFeed: '/market-data/synthetic/ir',
    reporting: '/reporting',
    reports: '/reporting/reports',
    configurations: '/reporting/configurations',
    pricingEngines: '/reporting/pricing-engines',
    pricingEngineAuthoring: '/reporting/pricing-engines/author',
    engineSettings: '/reporting/engine-settings',
    curveConfigurations: '/reporting/curve-configurations',
    sensitivityTemplates: '/reporting/sensitivity-templates',
} as const;

/** The address of the documents of one convention kind. */
export function conventionPath(family: string): string {
    return `${PATHS.conventions}/${encodeURIComponent(family)}`;
}

/**
 * The address of one curve blueprint's record page.
 *
 * A blueprint's key is a UUID and it has no name of its own, so the address
 * carries the id and the page names the record by the series it publishes into.
 */
export function curveBlueprintPath(id?: string): string {
    return id === undefined || id === ''
        ? PATHS.curveBlueprints
        : `${PATHS.curveBlueprints}/${encodeURIComponent(id)}`;
}

/** The address of one convention's record page: its id within its family, and the party that holds it. */
export function conventionDocumentPath(family: string, id: string, party?: string): string {
    const query = party === undefined || party === '' ? '' : `?party=${encodeURIComponent(party)}`;
    return `${conventionPath(family)}/${encodeURIComponent(id)}${query}`;
}

/**
 * The address of one published curve's record: the curve it names and the party
 * the set belongs to.
 *
 * The curve is the curve id its output series carries, and the party is the
 * short code a person reads, because the same curve id in two parties is two
 * curves. The tab and the as-of are the page's own parameters, so leaving them
 * out opens the record on its details.
 */
export function publishedCurvePath(curve: string, party?: string): string {
    const query = party === undefined || party === '' ? '' : `?party=${encodeURIComponent(party)}`;
    return `${PATHS.publishedCurves}/${encodeURIComponent(curve)}${query}`;
}

/**
 * The address of one published curve's comparison, at the as-of a request names.
 *
 * This is the door a decider takes from the queue: the record opens on the tab
 * that shows what the candidate changes, and no second screen holds the same
 * comparison.
 */
export function publishedCurveComparisonPath(curve: string, party: string, asOf: string): string {
    const query = new URLSearchParams({ party, tab: 'compare' });
    if (asOf !== '') query.set('asof', asOf);
    return `${PATHS.publishedCurves}/${encodeURIComponent(curve)}?${query.toString()}`;
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

/** The address of one curve configuration's page, by the report configuration it fills. */
export function curveConfigurationPath(configurationId: string): string {
    return `${PATHS.curveConfigurations}/${encodeURIComponent(configurationId)}`;
}

/** The address of one sensitivity template's page. */
export function sensitivityTemplatePath(code: string): string {
    return `${PATHS.sensitivityTemplates}/${encodeURIComponent(code)}`;
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

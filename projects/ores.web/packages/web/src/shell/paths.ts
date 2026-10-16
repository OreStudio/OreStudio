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
    parties: '/refdata/parties',
    newParty: '/refdata/parties/new',
    onboardCounterparty: '/refdata/counterparties/onboard',
    bookStructure: '/refdata/books/structure',
    conventions: '/refdata/conventions',
    tenors: '/refdata/tenors',
    organisation: '/organisation',
    people: '/organisation/people',
    hierarchy: '/organisation/hierarchy',
    roles: '/organisation/roles',
    rescue: '/organisation/rescue',
    operations: '/operations',
    signIns: '/operations/sign-ins',
    synthetic: '/market-data/synthetic',
    fxFeed: '/market-data/synthetic/fx',
    irFeed: '/market-data/synthetic/ir',
    bindings: '/market-data/bindings',
} as const;

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

/** The address of the page of one compute node. */
export function gridNodePath(hostId: string): string {
    return `/operations/grid/nodes/${encodeURIComponent(hostId)}`;
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
];

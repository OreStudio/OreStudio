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

import type { IconName } from '../ui/Icon.js';
import { PATHS } from './paths.js';
import type { MenuItem } from './areas.js';

/**
 * Every card of the application, defined once.
 *
 * A card leads to one screen. An area page draws the cards of its area, and
 * Home draws the same cards as shortcuts, so a screen has the same name, the
 * same line about it and the same glyph wherever it is offered. A screen that
 * needs a read says so here, and the read decides who sees the card.
 */
export interface CardDef extends Pick<MenuItem, 'permission' | 'scope'> {
    readonly titleKey: string;
    readonly bodyKey: string;
    readonly to: string;
    readonly icon: IconName;
    /**
     * The key of the title the screen draws, when it is not the card's own key.
     * A file card names its screen word for word, so a test holds the two
     * texts equal in every language.
     */
    readonly screenTitleKey?: string;
    /**
     * A workflow card starts a guided run of steps and not a screen. Its title
     * is a verb and its object, and it wears its own hue and a tag.
     */
    readonly kind?: 'workflow';
    /**
     * The screen card a workflow acts on. The workflow is listed on that
     * screen, in its Workflows menu, and not on the area page. A workflow with
     * no such screen is drawn in the area's Workflows group.
     */
    readonly on?: string;
}

export const CARDS = {
    parties: {
        titleKey: 'home.tenant.parties',
        bodyKey: 'home.tenant.partiesBody',
        to: PATHS.parties,
        icon: 'party',
        permission: 'refdata::parties:read',
        screenTitleKey: 'parties.title',
    },
    newParty: {
        titleKey: 'home.tenant.newParty',
        bodyKey: 'home.tenant.newPartyBody',
        to: PATHS.newParty,
        icon: 'add',
        permission: 'refdata::parties:write',
        kind: 'workflow',
        on: 'parties',
    },
    partyDetails: {
        titleKey: 'home.tenant.partyDetails',
        bodyKey: 'home.tenant.partyDetailsBody',
        to: PATHS.partyDetails,
        icon: 'party',
        permission: 'refdata::parties:write',
        kind: 'workflow',
        on: 'parties',
    },
    onboardCounterparty: {
        titleKey: 'home.tenant.counterpartyOnboard',
        bodyKey: 'home.tenant.counterpartyOnboardBody',
        to: PATHS.onboardCounterparty,
        icon: 'people',
        permission: 'refdata::counterparties:write',
        kind: 'workflow',
        on: 'parties',
    },
    bookStructure: {
        titleKey: 'home.tenant.bookStructure',
        bodyKey: 'home.tenant.bookStructureBody',
        to: PATHS.bookStructure,
        icon: 'database',
        kind: 'workflow',
    },
    currencies: {
        titleKey: 'refdata.area.screens.currencies',
        bodyKey: 'refdata.area.screens.currenciesBody',
        to: '/refdata/currencies',
        icon: 'currency',
        screenTitleKey: 'refdata.currencies.title',
    },
    currencyPairs: {
        titleKey: 'refdata.area.screens.currencyPairs',
        bodyKey: 'refdata.area.screens.currencyPairsBody',
        to: '/refdata/currency-pairs',
        icon: 'pairs',
        screenTitleKey: 'refdata.pairs.title',
    },
    deskGroups: {
        titleKey: 'refdata.area.screens.deskGroups',
        bodyKey: 'refdata.area.screens.deskGroupsBody',
        to: '/refdata/desk-groups',
        icon: 'desk',
        screenTitleKey: 'refdata.deskGroups.title',
    },
    calendars: {
        titleKey: 'refdata.area.screens.calendars',
        bodyKey: 'refdata.area.screens.calendarsBody',
        to: '/refdata/calendars',
        icon: 'calendar',
        screenTitleKey: 'refdata.calendars.title',
    },
    conventions: {
        titleKey: 'home.tenant.conventions',
        bodyKey: 'home.tenant.conventionsBody',
        to: PATHS.conventions,
        icon: 'document',
        kind: 'workflow',
    },
    classifications: {
        titleKey: 'refdata.classifications.title',
        bodyKey: 'refdata.area.classificationsBody',
        to: '/refdata/classifications',
        icon: 'classification',
    },
    people: {
        titleKey: 'access.hub.staff',
        bodyKey: 'access.hub.staffBody',
        to: PATHS.people,
        icon: 'person',
        permission: ['iam::accounts:read', 'iam::organisation:read'],
        screenTitleKey: 'access.people.title',
    },
    hierarchy: {
        titleKey: 'access.hub.hierarchy',
        bodyKey: 'access.hub.hierarchyBody',
        to: PATHS.hierarchy,
        icon: 'people',
        permission: ['iam::accounts:read', 'iam::organisation:read'],
        screenTitleKey: 'access.hub.hierarchy',
    },
    roles: {
        titleKey: 'home.tenant.roles',
        bodyKey: 'home.tenant.rolesBody',
        to: PATHS.roles,
        icon: 'access',
        permission: 'iam::roles:read',
        screenTitleKey: 'access.roles.title',
    },
    rescue: {
        titleKey: 'home.tenant.rescue',
        bodyKey: 'home.tenant.rescueBody',
        to: PATHS.rescue,
        icon: 'unlock',
        permission: 'iam::accounts:lock',
        screenTitleKey: 'rescue.title',
    },
    services: {
        titleKey: 'operations.screens.services',
        bodyKey: 'operations.screens.servicesBody',
        to: '/operations/services',
        icon: 'server',
        scope: 'installation',
        screenTitleKey: 'operations.services.title',
    },
    grid: {
        titleKey: 'operations.screens.grid',
        bodyKey: 'operations.screens.gridBody',
        to: '/operations/grid',
        icon: 'apps',
        scope: 'installation',
        screenTitleKey: 'operations.grid.title',
    },
    bus: {
        titleKey: 'operations.screens.bus',
        bodyKey: 'operations.screens.busBody',
        to: '/operations/bus',
        icon: 'bus',
        scope: 'installation',
        screenTitleKey: 'operations.bus.title',
    },
    logs: {
        titleKey: 'operations.screens.logs',
        bodyKey: 'operations.screens.logsBody',
        to: '/operations/logs',
        icon: 'log',
        scope: 'installation',
        screenTitleKey: 'operations.logs.title',
    },
    signIns: {
        titleKey: 'home.tenant.audit',
        bodyKey: 'home.tenant.auditBody',
        to: PATHS.signIns,
        icon: 'record',
        permission: ['iam::sessions:read', 'iam::login_info:read'],
        screenTitleKey: 'auditSignIns.title',
    },
    versions: {
        titleKey: 'operations.screens.versions',
        bodyKey: 'operations.screens.versionsBody',
        to: '/operations/versions',
        icon: 'history',
        screenTitleKey: 'operations.versions.title',
    },
    tenants: {
        titleKey: 'home.system.tenants',
        bodyKey: 'home.system.tenantsLead',
        to: '/tenants',
        icon: 'party',
        scope: 'installation',
        screenTitleKey: 'tenants.title',
    },
    myAccess: {
        titleKey: 'home.tenant.access',
        bodyKey: 'home.tenant.accessBody',
        to: '/access',
        icon: 'person',
        screenTitleKey: 'access.mine.title',
    },
    security: {
        titleKey: 'home.tenant.security',
        bodyKey: 'home.tenant.securityBody',
        to: '/security',
        icon: 'locked',
        screenTitleKey: 'security.title',
    },
} as const satisfies Record<string, CardDef>;

export type CardId = keyof typeof CARDS;

/**
 * The cards of each area, in the order the area page draws them: the screens,
 * then the workflows that have no screen to be listed on. The page has no
 * headings. A workflow is told from a screen by its hue and its tag.
 */
export const AREA_CARDS = {
    refdata: [
        'parties',
        'currencies',
        'currencyPairs',
        'deskGroups',
        'calendars',
        'classifications',
        'bookStructure',
        'conventions',
    ],
    organisation: ['people', 'hierarchy', 'roles', 'rescue'],
    operations: ['services', 'grid', 'bus', 'logs', 'signIns', 'versions'],
    tenants: ['tenants'],
} as const satisfies Record<string, readonly CardId[]>;

export type CardArea = keyof typeof AREA_CARDS;

/** The cards of a person's own account, which Home offers beside the areas. */
export const ACCOUNT_CARDS: readonly CardId[] = ['myAccess', 'security'];

/** The definition of one card. */
export function cardOf(id: CardId): CardDef {
    return CARDS[id];
}

/** The area whose page draws this card, or undefined for a card outside the areas. */
export function areaOfCard(id: CardId): CardArea | undefined {
    return (Object.keys(AREA_CARDS) as CardArea[]).find((area) =>
        (AREA_CARDS[area] as readonly CardId[]).includes(id),
    );
}

/**
 * A folder card: the card of an area, which opens that area's page of cards.
 * Its name and its address are the menu entry's, so the menu and Home cannot
 * differ. Home draws one for each entry of the menu except Home.
 */
export interface FolderDef {
    readonly to: string;
    readonly bodyKey: string;
    readonly icon: IconName;
}

export const FOLDERS: readonly FolderDef[] = [
    { to: PATHS.refdata, bodyKey: 'refdata.area.lead', icon: 'database' },
    { to: PATHS.organisation, bodyKey: 'access.hub.lead', icon: 'people' },
    { to: '/requests', bodyKey: 'inbox.queue.lead', icon: 'document' },
    { to: PATHS.operations, bodyKey: 'operations.hub.description', icon: 'server' },
    { to: '/tenants', bodyKey: 'home.system.tenantsLead', icon: 'party' },
];

/** The workflows listed on a screen: those that act on it. */
export function workflowsOn(screen: CardId): readonly CardId[] {
    return (Object.keys(CARDS) as CardId[]).filter((id) => (CARDS[id] as CardDef).on === screen);
}

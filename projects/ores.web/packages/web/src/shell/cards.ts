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
import { DEPLOYMENT_READ } from '@ores/contracts';
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
export interface CardDef extends Pick<MenuItem, 'permission'> {
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
    trades: {
        titleKey: 'trading.trades.title',
        bodyKey: 'trading.trades.lead',
        to: PATHS.trades,
        icon: 'document',
        permission: 'trading::trade_summaries:read',
    },
    structures: {
        titleKey: 'trading.structures.title',
        bodyKey: 'trading.structures.lead',
        to: PATHS.structures,
        icon: 'document',
        permission: 'trading::structures:read',
    },
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
        titleKey: 'refdata.area.screens.conventions',
        bodyKey: 'refdata.area.screens.conventionsBody',
        to: PATHS.conventions,
        icon: 'document',
        screenTitleKey: 'refdata.conventions.title',
    },
    conventionAuthoring: {
        titleKey: 'home.tenant.conventions',
        bodyKey: 'home.tenant.conventionsBody',
        to: PATHS.conventionAuthoring,
        icon: 'document',
        kind: 'workflow',
        on: 'conventions',
    },
    tenors: {
        titleKey: 'refdata.area.screens.tenors',
        bodyKey: 'refdata.area.screens.tenorsBody',
        to: PATHS.tenors,
        icon: 'document',
        screenTitleKey: 'refdata.tenors.title',
    },
    tenorAuthoring: {
        titleKey: 'home.tenant.tenors',
        bodyKey: 'home.tenant.tenorsBody',
        to: PATHS.tenorAuthoring,
        icon: 'document',
        kind: 'workflow',
        on: 'tenors',
    },
    classifications: {
        titleKey: 'refdata.classifications.title',
        bodyKey: 'refdata.area.classificationsBody',
        to: '/refdata/classifications',
        icon: 'classification',
        screenTitleKey: 'refdata.classifications.title',
    },
    synthetic: {
        titleKey: 'marketdata.area.screens.synthetic',
        bodyKey: 'marketdata.area.screens.syntheticBody',
        to: PATHS.synthetic,
        icon: 'chart',
        permission: [
            'synthetic::fx_spot_generation_configs:read',
            'synthetic::ir_curve_generation_configs:read',
        ],
        screenTitleKey: 'marketdata.synthetic.title',
    },
    subscriptions: {
        titleKey: 'marketdata.area.screens.subscriptions',
        bodyKey: 'marketdata.area.screens.subscriptionsBody',
        to: PATHS.subscriptions,
        icon: 'document',
        permission: 'marketdata::subscriptions:read',
        screenTitleKey: 'marketdata.subscriptions.title',
    },
    curveBlueprints: {
        titleKey: 'marketdata.curveBlueprints.title',
        bodyKey: 'marketdata.curveBlueprints.lead',
        to: PATHS.curveBlueprints,
        icon: 'document',
        permission: 'refdata::ir_curve_bootstrap_configs:read',
        screenTitleKey: 'marketdata.curveBlueprints.title',
    },
    officialRates: {
        titleKey: 'marketdata.official.title',
        bodyKey: 'marketdata.official.lead',
        to: PATHS.officialRates,
        icon: 'chart',
        permission: 'marketdata::providers:read',
        screenTitleKey: 'marketdata.official.title',
    },
    publishedCurves: {
        titleKey: 'marketdata.published.title',
        bodyKey: 'marketdata.published.lead',
        to: PATHS.publishedCurves,
        icon: 'chart',
        permission: 'marketdata::curve_snapshots:read',
        screenTitleKey: 'marketdata.published.title',
    },
    curveEvolution: {
        titleKey: 'market.evolution.title',
        bodyKey: 'marketdata.area.screens.evolutionBody',
        to: '/market-data/evolution',
        icon: 'chart',
        permission: 'marketdata::curve_snapshots:read',
        screenTitleKey: 'market.evolution.title',
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
        permission: DEPLOYMENT_READ,
        screenTitleKey: 'operations.services.title',
    },
    grid: {
        titleKey: 'operations.screens.grid',
        bodyKey: 'operations.screens.gridBody',
        to: '/operations/grid',
        icon: 'apps',
        permission: 'compute::grid_samples:read',
        screenTitleKey: 'operations.grid.title',
    },
    bus: {
        titleKey: 'operations.screens.bus',
        bodyKey: 'operations.screens.busBody',
        to: '/operations/bus',
        icon: 'bus',
        permission: DEPLOYMENT_READ,
        screenTitleKey: 'operations.bus.title',
    },
    logs: {
        titleKey: 'operations.screens.logs',
        bodyKey: 'operations.screens.logsBody',
        to: '/operations/logs',
        icon: 'log',
        permission: DEPLOYMENT_READ,
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
    marketDataSources: {
        titleKey: 'operations.screens.sources',
        bodyKey: 'operations.screens.sourcesBody',
        to: PATHS.marketDataSources,
        icon: 'server',
        permission: 'marketdata::market_data_sources:read',
        screenTitleKey: 'operations.sources.title',
    },
    versions: {
        titleKey: 'operations.screens.versions',
        bodyKey: 'operations.screens.versionsBody',
        to: '/operations/versions',
        icon: 'history',
        screenTitleKey: 'operations.versions.title',
    },
    reports: {
        titleKey: 'reporting.area.screens.reports',
        bodyKey: 'reporting.area.screens.reportsBody',
        to: PATHS.reports,
        icon: 'chart',
        permission: 'reporting::report_definitions:read',
        screenTitleKey: 'reporting.title',
    },
    pricingEngines: {
        titleKey: 'reporting.area.screens.pricingEngines',
        bodyKey: 'reporting.area.screens.pricingEnginesBody',
        to: PATHS.pricingEngines,
        icon: 'pairs',
        permission: 'analytics::pricing_model_configs:read',
        screenTitleKey: 'pricingEngines.title',
    },
    sensitivityTemplates: {
        titleKey: 'reporting.area.screens.sensitivityTemplates',
        bodyKey: 'reporting.area.screens.sensitivityTemplatesBody',
        to: PATHS.sensitivityTemplates,
        icon: 'pairs',
        permission: 'analytics::sensitivity_templates:read',
        screenTitleKey: 'sensitivityTemplates.title',
    },
    /*
     * The card asks for the code its own screen is opened by; the list behind it
     * also reads the curve definitions to count the curves of each row. Member
     * holds both, so the door and the screen agree today. A custom role that
     * holds one and not the other sees the card and then the route's refusal,
     * which is the route's answer to give.
     */
    curveConfigurations: {
        titleKey: 'reporting.area.screens.curveConfigurations',
        bodyKey: 'reporting.area.screens.curveConfigurationsBody',
        to: PATHS.curveConfigurations,
        icon: 'chart',
        permission: 'refdata::curve_configurations:read',
        screenTitleKey: 'curveConfiguration.list.title',
    },
    engineSettings: {
        titleKey: 'reporting.area.screens.engineSettings',
        bodyKey: 'reporting.area.screens.engineSettingsBody',
        to: PATHS.engineSettings,
        icon: 'apps',
        permission: 'reporting::engine_settings:read',
        screenTitleKey: 'engineSettings.title',
    },
    pricingEngineAuthoring: {
        titleKey: 'pricingEngines.workflows.author',
        bodyKey: 'pricingEngines.workflows.authorBody',
        to: PATHS.pricingEngineAuthoring,
        icon: 'document',
        kind: 'workflow',
        on: 'pricingEngines',
        permission: 'analytics::pricing_model_configs:write',
    },
    tenants: {
        titleKey: 'home.system.tenants',
        bodyKey: 'home.system.tenantsLead',
        to: '/tenants',
        icon: 'party',
        permission: DEPLOYMENT_READ,
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
        'tenors',
        'conventions',
        'bookStructure',
    ],
    organisation: ['people', 'hierarchy', 'roles', 'rescue'],
    marketdata: [
        'curveBlueprints',
        'synthetic',
        'subscriptions',
        'officialRates',
        'publishedCurves',
        'curveEvolution',
    ],
    operations: ['services', 'grid', 'bus', 'logs', 'signIns', 'versions', 'marketDataSources'],
    reporting: [
        'reports',
        'pricingEngines',
        'sensitivityTemplates',
        'curveConfigurations',
        'conventions',
        'engineSettings',
    ],
    tenants: ['tenants'],
    trading: ['trades', 'structures'],
} as const satisfies Record<string, readonly CardId[]>;

export type CardArea = keyof typeof AREA_CARDS;

/** The address of each area's page, which is also the start of every address inside it. */
export const AREA_PAGE: Record<CardArea, string> = {
    refdata: PATHS.refdata,
    organisation: PATHS.organisation,
    marketdata: '/market-data',
    operations: PATHS.operations,
    reporting: PATHS.reporting,
    tenants: '/tenants',
    trading: PATHS.trading,
};

/** The cards of a person's own account, which Home offers beside the areas. */
export const ACCOUNT_CARDS: readonly CardId[] = ['myAccess', 'security'];

/** The definition of one card. */
export function cardOf(id: CardId): CardDef {
    return CARDS[id];
}

/**
 * The area a card's screen belongs to. A workflow listed on the screen it acts
 * on has no area page of its own, so its area is that screen's. A card of a
 * person themselves has none.
 */
export function screenAreaOf(id: CardId): CardArea | undefined {
    const on = (CARDS[id] as CardDef).on;
    return areaOfCard(id) ?? (on === undefined ? undefined : areaOfCard(on as CardId));
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
    { to: '/market-data', bodyKey: 'home.party.marketdataBody', icon: 'chart' },
    { to: PATHS.trading, bodyKey: 'trading.area.lead', icon: 'document' },
    { to: '/requests', bodyKey: 'inbox.queue.lead', icon: 'document' },
    { to: PATHS.operations, bodyKey: 'operations.hub.description', icon: 'server' },
    { to: PATHS.reporting, bodyKey: 'reporting.area.lead', icon: 'chart' },
    { to: '/development', bodyKey: 'development.lead', icon: 'apps' },
    { to: '/tenants', bodyKey: 'home.system.tenantsLead', icon: 'party' },
];

/** The workflows listed on a screen: those that act on it. */
export function workflowsOn(screen: CardId): readonly CardId[] {
    return (Object.keys(CARDS) as CardId[]).filter((id) => (CARDS[id] as CardDef).on === screen);
}

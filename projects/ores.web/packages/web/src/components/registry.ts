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
 * Every component, and every entity under it.
 *
 * This file is the navigation. The sidebar renders it, the router generates its
 * routes, and the breadcrumbs read it. Nothing else decides where a screen lives,
 * so a screen cannot end up in two places or in a place nobody looks.
 *
 * The structure follows the ORE Studio components rather than the Qt menu bar,
 * because a menu bar hides every destination behind a click and a component's
 * entities are a list to be read rather than a menu to be opened. Where the Qt
 * client filed a component's screens somewhere else — IAM under Operations > User
 * Accounts — the component is a group here instead.
 *
 * Entities marked `planned` are declared so the structure is complete and the
 * scale is visible. Their screens do not exist yet, so the interface says so
 * rather than offering a link that does nothing.
 *
 * Titles and descriptions are translation keys. See `modeling/component_specification.org`.
 */
import type { ComponentDefinition, EntityDefinition, ResolvedEntity } from './types.js';
import type { EntityDescriptor } from '../entity/descriptor.js';
import { tenantTypeDescriptor } from '../generated/iam/web/tenant_type_declaration.js';
import { accountDescriptor } from '../generated/iam/web/account_declaration.js';
import { accountContactInformationDescriptor } from '../generated/iam/web/account_contact_information_declaration.js';
import { accountTypeDescriptor } from '../generated/iam/web/account_type_declaration.js';
import { loginInfoDescriptor } from '../generated/iam/web/login_info_declaration.js';
import { permissionDescriptor } from '../generated/iam/web/permission_declaration.js';
import { roleDescriptor } from '../generated/iam/web/role_declaration.js';
import { sessionDescriptor } from '../generated/iam/web/session_declaration.js';
import { tenantDescriptor } from '../generated/iam/web/tenant_declaration.js';
import { tenantStatusDescriptor } from '../generated/iam/web/tenant_status_declaration.js';
import { countryDescriptor } from '../generated/refdata/web/country_declaration.js';

/** Marks an entity whose screen is not built yet. */
const planned = { planned: true } as const;

/**
 * Declares an entity.
 *
 * The name and description are the identifier and, where given, a catalogue key.
 * Nothing is required beyond the identifier, because most entities are declared
 * here before they are built and inventing a name for each would fill the
 * catalogue with words nobody chose. See labels.ts.
 */
function entity(
  id: string,
  icon: EntityDefinition['icon'],
  extra: Partial<EntityDefinition> = {},
): EntityDefinition {
  return {
    id,
    icon,
    path: id.replace(/([a-z0-9])([A-Z])/g, '$1-$2').toLowerCase(),
    ...extra,
  };
}

/**
 * Declares an entity whose screen comes from a generated declaration.
 *
 * The route segment is the declaration's, because the model carries it and the
 * router reads it from there. Deriving it here means the link the sidebar builds
 * and the route the router registers are one string rather than two that have to
 * be kept in step.
 */
function wired(
  descriptor: EntityDescriptor,
  id: string,
  icon: EntityDefinition['icon'],
): EntityDefinition {
  return { id, icon, path: descriptor.routeSegment, descriptor };
}

/**
 * Fails when no registry entity sits at a wired declaration's route segment.
 *
 * The router builds its paths from the declaration's segment and the sidebar
 * builds its links from the registry's, so a declaration the registry does not
 * mirror at that segment is a link that 404s. The check runs where the routes are
 * built, which is the one point both declarations are known.
 */
export function assertWiredPath(descriptor: EntityDescriptor): void {
  const component = findComponent(descriptor.component);
  const declared = component?.entities.some(
    (candidate) => candidate.path === descriptor.routeSegment,
  );
  if (declared !== true) {
    throw new Error(
      `wired entity ${descriptor.component}/${descriptor.entity} routes at ` +
        `'${descriptor.routeSegment}' but the registry declares no such path`,
    );
  }
}

export const iamComponent: ComponentDefinition = {
  id: 'iam',
  titleKey: 'nav.iam',
  icon: 'peopleTeam',
  path: 'iam',
  entities: [
    wired(accountDescriptor, 'account', 'personAccounts'),
    wired(accountContactInformationDescriptor, 'accountContactInformation', 'peopleTeam'),
    wired(accountTypeDescriptor, 'accountType', 'settings'),
    wired(loginInfoDescriptor, 'loginInfo', 'keyMultiple'),
    wired(permissionDescriptor, 'permission', 'keyMultiple'),
    wired(roleDescriptor, 'role', 'keyMultiple'),
    wired(sessionDescriptor, 'session', 'clock'),
    wired(tenantDescriptor, 'tenant', 'buildingSkyscraper'),
    wired(tenantStatusDescriptor, 'tenantStatus', 'clock'),
    wired(tenantTypeDescriptor, 'tenantType', 'classification'),
    entity('systemSetting', 'settings', planned),
  ],
  shortcuts: [
    {
      id: 'accounts',
      icon: 'personAccounts',
      to: 'account',
    },
    {
      id: 'org-chart',
      titleKey: 'shortcut.orgChart.title',
      descriptionKey: 'shortcut.orgChart.description',
      icon: 'organization',
      to: 'account/org-chart',
      planned: true,
    },
    {
      id: 'onboard-tenant',
      titleKey: 'shortcut.onboardTenant.title',
      descriptionKey: 'shortcut.onboardTenant.description',
      icon: 'wand',
      to: 'tenant/onboard',
      planned: true,
    },
  ],
};

export const refdataComponent: ComponentDefinition = {
  id: 'refdata',
  titleKey: 'nav.refdata',
  icon: 'globe',
  path: 'refdata',
  entities: [
    entity('book', 'book', planned),
    entity('portfolio', 'briefcase', planned),
    entity('businessUnit', 'building', planned),
    entity('party', 'handshake', planned),
    entity('counterparty', 'buildingBank', planned),
    entity('currency', 'currencyDollarEuro', planned),
    wired(countryDescriptor, 'country', 'flag'),
    entity('calendar', 'calendarClock', planned),
    entity('currencyPair', 'arrowSync', planned),
    entity('dayCountFractionType', 'clock', planned),
    entity('paymentFrequency', 'clock', planned),
    entity('businessDayConventionType', 'calendarClock', planned),
  ],
  shortcuts: [
    {
      id: 'parties',
      titleKey: 'shortcut.parties.title',
      descriptionKey: 'shortcut.parties.description',
      icon: 'handshake',
      to: 'party',
      planned: true,
    },
    {
      id: 'currencies',
      titleKey: 'shortcut.currencies.title',
      descriptionKey: 'shortcut.currencies.description',
      icon: 'currencyDollarEuro',
      to: 'currency',
      planned: true,
    },
    {
      id: 'books',
      titleKey: 'shortcut.books.title',
      descriptionKey: 'shortcut.books.description',
      icon: 'book',
      to: 'book',
      planned: true,
    },
  ],
};

export const tradingComponent: ComponentDefinition = {
  id: 'trading',
  titleKey: 'nav.trading',
  icon: 'arrowTrending',
  path: 'trading',
  entities: [
    entity('trade', 'arrowTrending', planned),
    entity('tradeType', 'classification', planned),
    entity('portfolioExplorer', 'folder', planned),
    entity('lifecycleEvent', 'history', planned),
  ],
  shortcuts: [
    {
      id: 'trades',
      titleKey: 'shortcut.trades.title',
      descriptionKey: 'shortcut.trades.description',
      icon: 'arrowTrending',
      to: 'trade',
      planned: true,
    },
    {
      id: 'portfolios',
      titleKey: 'shortcut.portfolios.title',
      descriptionKey: 'shortcut.portfolios.description',
      icon: 'folder',
      to: 'portfolio-explorer',
      planned: true,
    },
  ],
};

export const marketDataComponent: ComponentDefinition = {
  id: 'marketdata',
  titleKey: 'nav.marketdata',
  icon: 'chartMultiple',
  path: 'market-data',
  entities: [
    entity('marketSeries', 'chartMultiple', planned),
    entity('marketObservation', 'table', planned),
    entity('marketFixings', 'documentTable', planned),
    entity('marketFixingDetail', 'documentTable', planned),
  ],
  shortcuts: [
    {
      id: 'series',
      titleKey: 'shortcut.marketSeries.title',
      descriptionKey: 'shortcut.marketSeries.description',
      icon: 'chartMultiple',
      to: 'market-series',
      planned: true,
    },
    {
      id: 'fixings',
      titleKey: 'shortcut.fixings.title',
      descriptionKey: 'shortcut.fixings.description',
      icon: 'documentTable',
      to: 'market-fixings',
      planned: true,
    },
  ],
};

export const reportingComponent: ComponentDefinition = {
  id: 'reporting',
  titleKey: 'nav.reporting',
  icon: 'columnTriple',
  path: 'reporting',
  entities: [
    entity('reportDefinition', 'notepad', planned),
    entity('reportInstance', 'documentTable', planned),
    entity('reportType', 'classification', planned),
    entity('pricingModelConfig', 'code', planned),
    entity('pricingEngineType', 'flashFlow', planned),
  ],
  shortcuts: [
    {
      id: 'report-definitions',
      titleKey: 'shortcut.reportDefinitions.title',
      descriptionKey: 'shortcut.reportDefinitions.description',
      icon: 'notepad',
      to: 'report-definition',
      planned: true,
    },
    {
      id: 'instances',
      titleKey: 'shortcut.reportInstances.title',
      descriptionKey: 'shortcut.reportInstances.description',
      icon: 'documentTable',
      to: 'report-instance',
      planned: true,
    },
  ],
};

export const dataQualityComponent: ComponentDefinition = {
  id: 'dataquality',
  titleKey: 'nav.dataquality',
  icon: 'checkmarkCircle',
  path: 'data-quality',
  entities: [
    entity('dataset', 'documentTable', planned),
    entity('datasetBundle', 'library', planned),
    entity('codingScheme', 'code', planned),
    entity('codeDomain', 'classification', planned),
    entity('badgeDefinition', 'tag', planned),
    entity('artefactType', 'documentTable', planned),
    entity('methodology', 'notepad', planned),
    entity('changeReason', 'history', planned),
  ],
  shortcuts: [
    {
      id: 'dq-catalog',
      titleKey: 'shortcut.catalog.title',
      descriptionKey: 'shortcut.catalog.description',
      icon: 'library',
      to: 'dataset',
      planned: true,
    },
    {
      id: 'schemes',
      titleKey: 'shortcut.codingSchemes.title',
      descriptionKey: 'shortcut.codingSchemes.description',
      icon: 'code',
      to: 'coding-scheme',
      planned: true,
    },
  ],
};

export const computeComponent: ComponentDefinition = {
  id: 'compute',
  titleKey: 'nav.compute',
  icon: 'desktop',
  path: 'compute',
  entities: [
    entity('app', 'apps', planned),
    entity('appVersion', 'tag', planned),
    entity('batch', 'columnTriple', planned),
    entity('workunit', 'tasksApp', planned),
    entity('queueMonitor', 'table', planned),
    entity('concurrencyPolicy', 'settings', planned),
    entity('host', 'serverLink', planned),
  ],
  shortcuts: [
    {
      id: 'dashboard',
      titleKey: 'shortcut.computeDashboard.title',
      descriptionKey: 'shortcut.computeDashboard.description',
      icon: 'desktop',
      to: 'app',
      planned: true,
    },
    {
      id: 'queues',
      titleKey: 'shortcut.queues.title',
      descriptionKey: 'shortcut.queues.description',
      icon: 'table',
      to: 'queue-monitor',
      planned: true,
    },
  ],
};

export const workflowComponent: ComponentDefinition = {
  id: 'workflow',
  titleKey: 'nav.workflow',
  icon: 'flashFlow',
  path: 'workflow',
  entities: [
    entity('workflowDefinition', 'flashFlow', planned),
    entity('workflow', 'play', planned),
    entity('jobDefinition', 'tasksApp', planned),
    entity('jobInstance', 'clock', planned),
  ],
  shortcuts: [
    {
      id: 'workflow-definitions',
      titleKey: 'shortcut.workflowDefinitions.title',
      descriptionKey: 'shortcut.workflowDefinitions.description',
      icon: 'flashFlow',
      to: 'workflow-definition',
      planned: true,
    },
    {
      id: 'scheduler',
      titleKey: 'shortcut.scheduler.title',
      descriptionKey: 'shortcut.scheduler.description',
      icon: 'clock',
      to: 'job-definition',
      planned: true,
    },
  ],
};

export const platformComponent: ComponentDefinition = {
  id: 'platform',
  titleKey: 'nav.platform',
  icon: 'serverLink',
  path: 'platform',
  group: 'platform',
  entities: [
    entity('connectionStatus', 'plugConnectedCheckmark'),
    entity('deployment', 'serverLink'),
  ],
};

/**
 * Every component, in the order the sidebar shows them.
 *
 * The order is the declaration. Nothing sorts it afterwards, which is what the Qt
 * client has to do for Data Quality because its order was implicit in plugin load
 * order.
 */
export const COMPONENTS: readonly ComponentDefinition[] = [
  iamComponent,
  refdataComponent,
  tradingComponent,
  marketDataComponent,
  reportingComponent,
  dataQualityComponent,
  computeComponent,
  workflowComponent,
];

/** Components reached from the sidebar footer rather than the list. */
export const PLATFORM_COMPONENTS: readonly ComponentDefinition[] = [platformComponent];

export function findComponent(id: string | undefined): ComponentDefinition | undefined {
  if (id === undefined) return undefined;
  return [...COMPONENTS, ...PLATFORM_COMPONENTS].find((c) => c.id === id);
}

export function findEntity(componentId: string, path: string): ResolvedEntity | undefined {
  const component = findComponent(componentId);
  const found = component?.entities.find((e) => e.path === path);
  return component !== undefined && found !== undefined ? { component, entity: found } : undefined;
}

export function allEntities(): readonly ResolvedEntity[] {
  return COMPONENTS.flatMap((component) =>
    component.entities.map((entity) => ({ component, entity })),
  );
}

export function entityCount(component: ComponentDefinition): number {
  return component.entities.length;
}

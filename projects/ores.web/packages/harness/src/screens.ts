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

import { CLASSIFICATION_LISTS, REFDATA_RECORDS } from '@ores/wire-protocol';

/**
 * A screen the harness checks: the list of one record kind and the record page
 * a row opens. The kinds come from the registries, so a kind the BFF can reach
 * is checked without a new test. The address of each kind is the one thing the
 * registry does not know, so this table names it, and a registry key the table
 * does not place stops the harness instead of being skipped in silence.
 */
export interface Screen {
    readonly key: string;
    readonly shape: 'record' | 'classification';
    readonly list: string;
}

/**
 * Where each record kind's list lives, or why it has no list of its own: a
 * part or a link is maintained from its parent's panel (RS-RELATED), and a kind
 * in the registry with no screen is a gap the matrix records.
 */
const RECORD_ROUTES: Readonly<Record<string, string | 'part' | 'link' | 'no screen'>> = {
    currencies: '/refdata/currencies',
    'currency-groups': '/refdata/desk-groups',
    'currency-pairs': '/refdata/currency-pairs',
    calendars: '/refdata/calendars',
    countries: 'no screen',
    'business-centres': 'no screen',
    'business-unit-types': 'no screen',
    'party-id-schemes': 'no screen',
    counterparties: 'no screen',
    parties: 'no screen',
    'business-units': 'part',
    'calendar-rules': 'part',
    'calendar-exceptions': 'part',
    'calendar-events': 'part',
    'currency-pair-conventions': 'part',
    'party-identifiers': 'part',
    'party-contacts': 'part',
    'currency-countries': 'link',
    'currency-calendars': 'link',
    'currency-memberships': 'link',
    'pair-calendars': 'link',
    'party-countries': 'link',
    'party-currencies': 'link',
    'party-counterparties': 'link',
};

export function recordScreens(): readonly Screen[] {
    const unplaced = REFDATA_RECORDS.map((r) => r.key).filter((key) => !(key in RECORD_ROUTES));
    if (unplaced.length > 0) {
        throw new Error(`record kinds the harness does not place: ${unplaced.join(', ')}`);
    }
    return REFDATA_RECORDS.flatMap((resource) => {
        const route = RECORD_ROUTES[resource.key];
        return route !== undefined && route.startsWith('/')
            ? [{ key: resource.key, shape: 'record' as const, list: route }]
            : [];
    });
}

/** The classification lists share one screen, under the exception RS-EXCEPT-01 names. */
export function classificationScreens(): readonly Screen[] {
    return CLASSIFICATION_LISTS.map((list) => ({
        key: list.key,
        shape: 'classification' as const,
        list: `/refdata/classifications/${list.key}`,
    }));
}

export function allScreens(): readonly Screen[] {
    return [...recordScreens(), ...classificationScreens()];
}

/** The kinds the registry names that have no screen of their own, for the baseline. */
export function kindsWithoutAScreen(): readonly string[] {
    return Object.entries(RECORD_ROUTES)
        .filter(([, route]) => route === 'no screen')
        .map(([key]) => key);
}

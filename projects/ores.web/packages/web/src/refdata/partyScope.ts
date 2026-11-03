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
 */

import { useQuery } from '@tanstack/react-query';
import { api } from '../api/client.js';
import type { ListFilter } from './RecordList.js';

/** The parties the session's account works in, which are the parties a filter may offer. */
export const MY_PARTIES_QUERY_KEY = ['my-parties'] as const;

/** The id of the column that names the owning party, which a list drops when it has one party. */
export const PARTY_COLUMN_ID = 'party';

/** The id a list's party filter travels under in the address and in the page request. */
export const PARTY_FILTER_ID = 'party';

/** The value the party filter's "all parties" option writes. */
export const ALL_PARTIES = 'all';

/**
 * The party a page is narrowed to, from its filters. The party filter opens on
 * the party the account works in, so "all parties" is a value of its own and
 * reads as no filter here.
 */
export function partyIdOf(filters: Readonly<Record<string, string>>): string | null {
    const stated = filters[PARTY_FILTER_ID];
    return stated === undefined || stated === '' || stated === ALL_PARTIES ? null : stated;
}

/**
 * What a list of party-scoped rows knows about the parties it may show.
 *
 * A person who works in one party sees only that party's rows, so a column that
 * names the owner and a filter by owner would say nothing. Both exist only when
 * the account works in more than one party, which is when the rows can differ.
 */
export interface PartyScope {
    /** True when the account works in more than one party. */
    readonly several: boolean;
    /** The filter that narrows the rows to one party, present exactly when `several` is. */
    readonly filter: ListFilter | undefined;
    readonly pending: boolean;
    readonly error: string | null;
}

/** The scope of the parties the account works in, with the words the filter shows. */
export function usePartyScope(words: { readonly label: string; readonly all: string }): PartyScope {
    const parties = useQuery({ queryKey: MY_PARTIES_QUERY_KEY, queryFn: api.myParties });
    if (parties.isPending) {
        return { several: false, filter: undefined, pending: true, error: null };
    }
    if (parties.isError) {
        return { several: false, filter: undefined, pending: false, error: parties.error.message };
    }
    const several = parties.data.parties.length > 1;
    const current = parties.data.parties.find(
        (party) => party.partyId === parties.data.defaultPartyId,
    );
    return {
        several,
        filter: several
            ? {
                  kind: 'choice',
                  id: PARTY_FILTER_ID,
                  label: words.label,
                  all: words.all,
                  allValue: ALL_PARTIES,
                  ...(current === undefined ? {} : { defaultValue: current.partyId }),
                  choices: parties.data.parties.map((party) => ({
                      value: party.partyId,
                      label: party.name,
                  })),
              }
            : undefined,
        pending: false,
        error: null,
    };
}

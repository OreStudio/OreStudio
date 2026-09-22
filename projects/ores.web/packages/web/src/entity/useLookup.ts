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

import { useQuery } from '@tanstack/react-query';
import { request } from '../api/transport.js';
import type { LookupSource } from '../ui-contract.js';

/** One choice a combo offers: the stored value and the words a person reads. */
export interface LookupOption {
  readonly value: string;
  readonly label: string;
}

/**
 * How many rows a combo reads before it stops being a list to choose from.
 *
 * A field whose values run past this wants a search, not a longer list, and
 * saying so here keeps the fetch bounded rather than growing with the table.
 */
const LOOKUP_LIMIT = 500;

interface LookupPage {
  readonly rows?: readonly Record<string, unknown>[];
}

/**
 * The choices a fetched combo offers, read from the collection its source names.
 *
 * The value and the label are the source's own fields, so a foreign key is
 * chosen by the name a person knows rather than by the UUID it is stored as.
 * That is the whole point of the control: a junction links an account to a
 * party, and neither is something anyone can type.
 *
 * The fetch is keyed by collection rather than by field, so two fields reading
 * one collection share one request, and it is held briefly because the
 * underlying rows change rarely and a form re-renders often.
 */
export function useLookupOptions(
  lookup: LookupSource | undefined,
): readonly LookupOption[] {
  const collection = lookup?.collection ?? '';
  const query = useQuery({
    queryKey: ['lookup', collection],
    enabled: collection.length > 0,
    staleTime: 60_000,
    queryFn: async () =>
      request(
        `/api/${collection}?offset=0&limit=${String(LOOKUP_LIMIT)}`,
        { method: 'GET' },
      ),
  });

  const rows = (query.data as LookupPage | undefined)?.rows ?? [];
  const valueField = lookup?.valueField ?? '';
  const labelField = lookup?.labelField ?? '';
  return rows.map((row) => ({
    value: String(row[valueField] ?? ''),
    label: String(row[labelField] ?? ''),
  }));
}

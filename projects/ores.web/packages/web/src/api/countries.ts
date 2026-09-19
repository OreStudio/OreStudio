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

import { useQuery, type UseQueryResult } from '@tanstack/react-query';
import { request } from './transport.js';
import { wireCountrySchema, type WireCountry } from '@ores/wire-protocol/browser';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import { z } from 'zod';

/**
 * Countries, fetched from the BFF.
 *
 * One file per entity on the browser side too, so the screens share nothing but
 * the transport. The response schema is declared here because the page shape is
 * the BFF's, not the service's: the BFF has already dropped the wire names.
 */
const countrySchema = z.object({
  id: z.string(),
  version: z.int().nonnegative(),
  alpha2Code: z.string(),
  alpha3Code: z.string(),
  numericCode: z.string(),
  name: z.string(),
  officialName: z.string(),
  modifiedBy: z.string(),
  recordedAt: z.string(),
  changeReasonCode: z.string(),
  changeCommentary: z.string(),
  performedBy: z.string(),
  /** The flag image, when the record carries one. */
  imageId: z.string().nullable(),
  /**
   * The record as the service sent it.
   *
   * Validated rather than assumed, because it is what an amend sends back: a
   * field silently lost here is a field silently cleared on the next save.
   */
  wire: wireCountrySchema,
});

export type Country = z.infer<typeof countrySchema>;

/**
 * A country in the shape the declaration names.
 *
 * The table is driven by column metadata whose `name` is the wire field, because
 * that is the key the row holds, so the row is what has to match. Doing this
 * once here means no screen has to translate between the two.
 */
export type CountryRow = Readonly<Record<string, unknown>>;

export function toRow(country: Country): CountryRow {
  return {
    alpha2_code: country.alpha2Code,
    alpha3_code: country.alpha3Code,
    numeric_code: country.numericCode,
    name: country.name,
    official_name: country.officialName,
    version: country.version,
    modified_by: country.modifiedBy,
    recorded_at: country.recordedAt,
    change_reason_code: country.changeReasonCode,
    change_commentary: country.changeCommentary,
    performed_by: country.performedBy,
    // The record's identity, which for this entity is its natural key.
    id: country.alpha2Code,
    image_id: country.imageId,
    /*
     * The record as the service sent it, carried through to the screen.
     *
     * A save replaces the whole record, and the form has no business knowing the
     * fields it does not show. Round-tripping the original is what stops an amend
     * quietly dropping one, and dropping it here is why the detail form was
     * rendering empty for a record that had loaded.
     */
    wire: country.wire,
  };
}

const countryPageSchema = z.object({
  countries: z.array(countrySchema),
  totalCount: z.int().nonnegative(),
});

export interface CountryQuery {
  readonly page: number;
  readonly pageSize: number;
}

/**
 * One page of countries.
 *
 * The query key carries the paging so a page change is a new cache entry rather
 * than a refetch of the same one, which is what makes going back instant.
 */
export function useCountries(query: CountryQuery): UseQueryResult<{
  rows: readonly CountryRow[];
  totalCount: number;
}> {
  return useQuery({
    queryKey: ['countries', query.page, query.pageSize],
    queryFn: async () => {
      const offset = (query.page - 1) * query.pageSize;
      const body = await request(
        `/api/countries?offset=${offset}&limit=${query.pageSize}`,
        { method: 'GET' },
      );
      const page = countryPageSchema.parse(body);
      // Reshaped here rather than in the screen, so the screen is only layout.
      return { rows: page.countries.map(toRow), totalCount: page.totalCount };
    },
    placeholderData: (previous) => previous,
  });
}


/**
 * Creates or amends one country.
 *
 * The record being replaced is sent whole, with the edits applied, because the
 * service replaces rather than patches and there are fields the interface
 * neither shows nor understands. The version carried in it is the optimistic
 * lock.
 */
export function useSaveCountry() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: async (input: {
      readonly data: WireCountry;
      readonly reasonCode: string;
      readonly commentary: string;
    }) => {
      await request('/api/countries', {
        method: 'POST',
        headers: { 'content-type': 'application/json' },
        body: JSON.stringify({
          data: input.data,
          reason: input.reasonCode,
          commentary: input.commentary,
        }),
      });
    },
    // Every page and the history may now be wrong, so both are invalidated
    // rather than patched. A list is cheap to refetch and a stale one is worse.
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: ['countries'] });
      void queryClient.invalidateQueries({ queryKey: ['country-history'] });
    },
  });
}

export function useDeleteCountry() {
  const queryClient = useQueryClient();
  return useMutation({
    mutationFn: async (alpha2Code: string) => {
      await request(`/api/countries/${encodeURIComponent(alpha2Code)}`, { method: 'DELETE' });
    },
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: ['countries'] });
    },
  });
}

/**
 * Every version of one country.
 *
 * Fetched on demand rather than with the record, because most visits to a record
 * are not visits to its history.
 */
const historySchema = z.object({
  versions: z.array(countrySchema),
  message: z.string(),
});

export function useCountryHistory(alpha2Code: string | undefined): UseQueryResult<{
  versions: readonly Country[];
}> {
  return useQuery({
    queryKey: ['country-history', alpha2Code],
    enabled: alpha2Code !== undefined && alpha2Code.length > 0,
    queryFn: async () => {
      const body = await request(`/api/countries/${encodeURIComponent(alpha2Code ?? '')}/history`, {
        method: 'GET',
      });
      return historySchema.parse(body);
    },
  });
}

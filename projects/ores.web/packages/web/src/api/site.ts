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
import { siteStateSchema, type SiteState } from '@ores/contracts';
import { request } from './transport.js';

/**
 * The site's own state.
 *
 * Which environment this deployment serves, and whether the developer surface
 * exists. It is read once and does not change: the environment is fixed when
 * the process starts.
 */
export const siteKey = ['site'] as const;

export function useSiteState(): { readonly site: SiteState | undefined; readonly isLoading: boolean } {
  const query = useQuery({
    queryKey: siteKey,
    queryFn: async () => siteStateSchema.parse(await request('/api/site', { method: 'GET' })),
    staleTime: Number.POSITIVE_INFINITY,
  });
  return { site: query.data, isLoading: query.isPending };
}

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
import { oresmdQueries } from './queries.js';
import { seriesTitle, type TitledIdentity } from './title.js';

/**
 * A function that names a series in words. Until the schema has arrived, or for a
 * type it does not know, it returns the raw identity, so a label is never empty.
 */
export function useSeriesTitle(): (identity: TitledIdentity) => string {
    const schema = useQuery(oresmdQueries.schema());
    return (identity) =>
        (schema.data === undefined ? undefined : seriesTitle(schema.data, identity)) ??
        rawLabel(identity);
}

function rawLabel(identity: TitledIdentity): string {
    const words = [identity.asset, identity.scope, identity.instrument_type, identity.quote_type]
        .filter((word) => word !== undefined)
        .join(' / ');
    if (identity.fields.length === 0) return words;
    return `${words} · ${identity.fields.map((field) => `${field.name}=${field.text}`).join(', ')}`;
}

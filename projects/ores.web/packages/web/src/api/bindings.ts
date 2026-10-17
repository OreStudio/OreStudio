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

import { z } from 'zod';
import { feedBindingActivityListSchema, type FeedBindingActivityList } from '@ores/contracts';
import { request } from './transport.js';

/** One synthetic source the party may bind: the series it would write, and who holds that series. */
export interface BindableSource {
    readonly source_name: string;
    readonly oresmd_uri: string;
    readonly label: string;
    readonly provider_code: string;
    readonly asset_class_code: string;
    readonly enabled: boolean;
    readonly running: boolean;
    /** The running feed that already holds this series, or null when none does. */
    readonly held_by: string | null;
}

const bindableSourcesSchema = z.object({
    sources: z.array(
        z.object({
            source_name: z.string(),
            oresmd_uri: z.string(),
            label: z.string(),
            provider_code: z.string(),
            asset_class_code: z.string(),
            enabled: z.boolean(),
            running: z.boolean(),
            held_by: z.string().nullable(),
        }),
    ),
});

/** What the party's feed bindings have written, and what it may still bind. */
export const bindings = {
    /** For each binding, the series of its source's newest observation and when it was made. */
    async activity(): Promise<FeedBindingActivityList> {
        return feedBindingActivityListSchema.parse(
            await request('/api/bindings/activity', { method: 'GET' }),
        );
    },
    /** The synthetic sources the party holds with no binding, each with the series it would write. */
    async sources(): Promise<readonly BindableSource[]> {
        return bindableSourcesSchema.parse(
            await request('/api/bindings/bindable-sources', { method: 'GET' }),
        ).sources;
    },
};

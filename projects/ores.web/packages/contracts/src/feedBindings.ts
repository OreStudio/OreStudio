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

import { nullableWireTimestampSchema } from '@ores/wire-protocol/time';
import { z } from 'zod';

/**
 * What a feed binding has written, as the bindings screen reads it. A binding names a
 * source and not a series, so the server answers, for each binding, the series of its
 * source's newest observation and the time of that observation. A source that has
 * written nothing has neither, and the server states null for both.
 */
export const feedBindingActivitySchema = z.object({
    source_name: z.string(),
    /** The series of the source's newest observation, or null when it has written nothing. */
    series_id: z.string().nullable(),
    /** When that observation was made, or null when the source has written nothing. */
    last_tick_at: nullableWireTimestampSchema,
});
export type FeedBindingActivity = z.infer<typeof feedBindingActivitySchema>;

export const feedBindingActivityListSchema = z.object({
    activities: z.array(feedBindingActivitySchema),
});
export type FeedBindingActivityList = z.infer<typeof feedBindingActivityListSchema>;

/** Whether a binding's source has written anything, so the screen can say so. */
export function hasWritten(activity: FeedBindingActivity): boolean {
    return activity.last_tick_at !== null;
}

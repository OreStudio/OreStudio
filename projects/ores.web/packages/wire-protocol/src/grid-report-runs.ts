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
import { reportHealthWordSchema } from './reporting.js';
import { nullableWireTimestampSchema, wireTimestampSchema } from './time.js';

/**
 * One report run as the report run page reads it: the run of a report definition
 * that dispatched a compute batch.
 *
 * The batch names the run in its External Ref, and the run does not name the batch,
 * so `batch_id` is found by reading the batches for that reference and is `null`
 * when the session sees none. `health` is the word the reporting service derives
 * from the run's state, and is `null` when the service states none or a word the
 * screens do not know.
 */
export const gridReportRunViewSchema = z.object({
    id: z.string(),
    name: z.string(),
    description: z.string(),
    definition_id: z.string(),
    definition_name: z.string().nullable(),
    party_id: z.string(),
    party_name: z.string().nullable(),
    kind: z.string(),
    state_name: z.string(),
    health: reportHealthWordSchema.nullable(),
    output_message: z.string(),
    started_at: nullableWireTimestampSchema,
    completed_at: nullableWireTimestampSchema,
    batch_id: z.string().nullable(),
    modified_by: z.string(),
    recorded_at: wireTimestampSchema,
});
export type GridReportRunView = z.infer<typeof gridReportRunViewSchema>;

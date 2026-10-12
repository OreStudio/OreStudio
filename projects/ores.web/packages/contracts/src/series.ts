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

/**
 * The values of one series over time, as the series page reads them: the
 * latest observations, oldest first. The BFF reads them through the server's
 * observation list and keeps only what a chart and a table need.
 */

/** The most observations one read returns. A request for more is clamped to it. */
export const MAX_OBSERVATIONS = 1000;
/** The number of observations a read returns when it names none. */
export const DEFAULT_OBSERVATIONS = 500;

export const observationSchema = z.object({
    /** The instant of the observation, as the wire writes it: YYYY-MM-DD HH:MM:SSZ. */
    at: z.string(),
    /** The value, as the text the server stored. */
    value: z.string(),
    source: z.string(),
});
export type Observation = z.infer<typeof observationSchema>;

export const seriesObservationsSchema = z.object({
    observations: z.array(observationSchema),
    /** How many observations the series holds, which may be more than were read. */
    total: z.number(),
});
export type SeriesObservations = z.infer<typeof seriesObservationsSchema>;

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

import {
    seriesObservationsSchema,
    type SeriesObservations,
    type SeriesWindow,
} from '@ores/contracts';
import { request } from './transport.js';

/** The values of one series over time. */
export const series = {
    /**
     * The observations of a series over a window, oldest first, and how many
     * the window holds. The count caps the answer, so a window wider than the
     * cap returns the newest observations of it.
     */
    async observations(
        seriesId: string,
        limit?: number,
        window?: SeriesWindow,
    ): Promise<SeriesObservations> {
        const query = new URLSearchParams();
        if (limit !== undefined) query.set('limit', String(limit));
        if (window !== undefined) query.set('window', window);
        const asked = query.size === 0 ? '' : `?${query.toString()}`;
        return seriesObservationsSchema.parse(
            await request(`/api/series/${encodeURIComponent(seriesId)}/observations${asked}`, {
                method: 'GET',
            }),
        );
    },
};

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
import { useEffect, useState } from 'react';
import { api, type MarketRegime } from '../../api/client.js';
import { multiplierText, RASTER_SECONDS, type StateSample } from './marketRaster.js';

/** How often the market state is read again, in milliseconds. */
export const REGIME_EVERY_MS = 2_000;

/** The one read of the market state on a screen, under the key every view shares. */
export const REGIME_KEY = ['market-regime'] as const;

/**
 * The market-wide arrival factor, as a query.
 *
 * One read under one key, so the market tab that sets it and a feed's Arrivals
 * tab that reads it cannot disagree, and a switch that lands updates both. The
 * tab that switches reads the query so it can show why a read failed; a view
 * that only reads takes the data.
 */
export function useMarketRegimeQuery(): UseQueryResult<MarketRegime> {
    return useQuery({
        queryKey: REGIME_KEY,
        queryFn: () => api.marketRegime(),
        refetchInterval: REGIME_EVERY_MS,
    });
}

/** The market-wide arrival factor in force, or undefined until it is read. */
export function useMarketRegime(): MarketRegime | undefined {
    return useMarketRegimeQuery().data;
}

/** The state's name as a person reads it. */
export function stateName(state: MarketRegime['state'], t: (key: string) => string): string {
    return t(`marketdata.marketRegime.states.${state}`);
}

/** The state and the multiplier as a sentence fragment, for a screen that only reads it. */
export function regimeLabel(regime: MarketRegime, t: (key: string) => string): string {
    return `${stateName(regime.state, t)} ${multiplierText(regime.multiplier)}`;
}

/**
 * The state this screen saw, once per read, trimmed to the raster's window.
 *
 * The service reports the state in force and not its history, so the stretch a
 * state held is the reader's own observation: the band begins when the tab
 * opens, rather than implying a record that is not kept.
 */
export function useStateTrace(
    dataUpdatedAt: number,
    state: MarketRegime['state'] | undefined,
): readonly StateSample[] {
    const [trace, setTrace] = useState<readonly StateSample[]>([]);
    useEffect(() => {
        if (state === undefined) {
            return;
        }
        const at = Date.now();
        setTrace((seen) => [
            ...seen.filter((sample) => at - sample.at <= RASTER_SECONDS * 1000),
            { at, state },
        ]);
    }, [dataUpdatedAt, state]);
    return trace;
}

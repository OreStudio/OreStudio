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
import { api, type MarketRegime } from '../../api/client.js';
import { multiplierText } from './marketRaster.js';

/** How often the market state is read again, in milliseconds. */
export const REGIME_EVERY_MS = 2_000;

/** The one read of the market state on a screen, under the key every view shares. */
export const REGIME_KEY = ['market-regime'] as const;

/**
 * The market-wide arrival factor in force.
 *
 * One read under one key, so the market tab that sets it and a feed's Arrivals
 * tab that reads it cannot disagree, and a switch that lands updates both.
 */
export function useMarketRegime(): MarketRegime | undefined {
    return useQuery({
        queryKey: REGIME_KEY,
        queryFn: () => api.marketRegime(),
        refetchInterval: REGIME_EVERY_MS,
    }).data;
}

/** The state's name as a person reads it, or the service's word when it is one we do not know. */
export function stateName(state: string, t: (key: string) => string): string {
    return state === 'calm' || state === 'active'
        ? t(`marketdata.marketRegime.states.${state}`)
        : state;
}

/** The state and the multiplier as a sentence fragment, for a screen that only reads it. */
export function regimeLabel(regime: MarketRegime, t: (key: string) => string): string {
    return `${stateName(regime.state, t)} ${multiplierText(regime.multiplier)}`;
}

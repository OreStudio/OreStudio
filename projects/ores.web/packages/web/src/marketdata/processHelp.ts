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

/** The engines a feed's process can run. */
export type ProcessType = 'geometric' | 'arithmetic' | 'ornstein_uhlenbeck';

export function processTypeOf(value: unknown): ProcessType {
    return value === 'arithmetic' || value === 'ornstein_uhlenbeck' ? value : 'geometric';
}

/**
 * The deviation each volatility profile stands for, as the Qt editor sets it:
 * multiples of a base of 0.0005 per update.
 */
const BASE_VOLATILITY = 0.0005;

export const PROFILES = {
    flat: 0,
    calm: BASE_VOLATILITY * 0.5,
    normal: BASE_VOLATILITY * 2,
    volatile: BASE_VOLATILITY * 8,
} as const;

export type Profile = keyof typeof PROFILES;

/** The profile a deviation is, or custom when it is none of them. */
export function profileOf(stdev: string): Profile | 'custom' {
    const value = Number(stdev);
    if (stdev.trim() === '' || !Number.isFinite(value)) {
        return 'custom';
    }
    const found = (Object.keys(PROFILES) as Profile[]).find(
        (profile) => Math.abs(PROFILES[profile] - value) < 1e-12,
    );
    return found ?? 'custom';
}

/**
 * How long an Ornstein-Uhlenbeck price takes to halve its distance from the
 * long-run level, in minutes: ln(2)/κ updates at the feed's rate. Undefined
 * for κ of zero or less, which does not revert.
 */
export function halfLifeMinutes(kappa: number, ticksPerHour: number): number | undefined {
    if (!(kappa > 0) || !(ticksPerHour > 0)) {
        return undefined;
    }
    return Math.log(2) / kappa / (ticksPerHour / 60);
}

/**
 * The width of the band an Ornstein-Uhlenbeck price settles into, about
 * σ/√(2κ), or undefined when κ does not revert.
 */
export function bandWidth(kappa: number, sigma: number): number | undefined {
    return kappa > 0 ? sigma / Math.sqrt(2 * kappa) : undefined;
}

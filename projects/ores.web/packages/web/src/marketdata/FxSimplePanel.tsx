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

import { keepPreviousData, useQuery } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { type Gaussian } from './mixture.js';
import { Answer, Presets, differs } from './SimpleControls.js';

/** The questions of the simple view of an FX feed. Every figure is per hour. */
export interface FxSimpleView {
    readonly trend_pct_per_hour: number;
    readonly volatility_pct_per_hour: number;
    readonly jump_share_pct: number;
    readonly half_life_minutes: number;
}

/** The view a feed starts from when it has no components to read. */
export const FX_SIMPLE_START: FxSimpleView = {
    trend_pct_per_hour: 0,
    volatility_pct_per_hour: 0.1,
    jump_share_pct: 0,
    half_life_minutes: 15,
};

const TREND_PRESETS = [
    { key: 'falling', value: -0.05 },
    { key: 'level', value: 0 },
    { key: 'rising', value: 0.05 },
] as const;

const MOVE_PRESETS = [
    { key: 'calm', value: 0.05 },
    { key: 'normal', value: 0.1 },
    { key: 'stressed', value: 0.3 },
] as const;

const JUMP_PRESETS = [
    { key: 'none', value: 0 },
    { key: 'rare', value: 5 },
    { key: 'often', value: 20 },
] as const;

const REVERSION_PRESETS = [
    { key: 'quick', value: 5 },
    { key: 'medium', value: 15 },
    { key: 'slow', value: 60 },
] as const;

/** What the service makes of the mixture on the screen, in the terms of the simple view. */
export interface FxReading {
    readonly view: FxSimpleView | undefined;
    /** Whether the simple view stands for the mixture without loss. */
    readonly exact: boolean;
    /** Why it does not, in the service's words. */
    readonly reason: string;
    readonly loading: boolean;
    /** Whether the answer is for an earlier request, while the current one is asked. */
    readonly stale: boolean;
}

/**
 * The simple view of the mixture on the screen, asked of the service, which
 * knows how each engine reads its numbers.
 */
export function useFxReading(
    engine: string,
    gaussians: readonly Gaussian[] | undefined,
    price: number,
    ticksPerHour: number,
): FxReading {
    const ready = gaussians !== undefined && gaussians.length > 0 && price > 0 && ticksPerHour > 0;
    const request = {
        process_type: engine,
        to_simple: true,
        initial_price: price,
        ticks_per_hour: ticksPerHour,
        gmm_means: (gaussians ?? []).map((one) => one.mean),
        gmm_stdevs: (gaussians ?? []).map((one) => one.stdev),
        gmm_weights: (gaussians ?? []).map((one) => one.weight),
    };
    const reading = useQuery({
        queryKey: ['fx-reading', request],
        queryFn: () => api.convertFxSimple(request),
        enabled: ready,
        placeholderData: keepPreviousData,
        retry: false,
    });
    const data = reading.data;
    return {
        view: !ready || data === undefined ? undefined : data,
        exact: ready && data?.has_simple_form === true,
        reason: data?.has_simple_form === false ? data.message : '',
        loading: ready && reading.isPending,
        stale: reading.isPlaceholderData,
    };
}

/**
 * The price behaviour tab for a person who does not know the mixture: how far
 * the price drifts, how far it moves and how often it jumps in an hour, or, for
 * the mean-reverting engine, how long it takes to come half way back. The
 * service turns the answers into components, and they go into the same draft as
 * the table, so Save, Discard and the previews work as they do there.
 */
export function FxSimplePanel({
    view,
    saved,
    engine,
    price,
    mayWrite,
    onChange,
}: {
    readonly view: FxSimpleView;
    /** The answers the saved mixture makes, when the engine is the saved one; fields that differ are tinted. */
    readonly saved: FxSimpleView | undefined;
    readonly engine: string;
    readonly price: number;
    readonly mayWrite: boolean;
    readonly onChange: (next: FxSimpleView) => void;
}): ReactNode {
    const { t } = useTranslation();
    const reverting = engine === 'ornstein_uhlenbeck';
    const set = (patch: Partial<FxSimpleView>): void => onChange({ ...view, ...patch });
    const options = (
        items: readonly { readonly key: string; readonly value: number }[],
        group: string,
    ) =>
        items.map((item) => ({
            key: item.key,
            value: item.value,
            label: t(`marketdata.fxSimple.presets.${group}.${item.key}`),
        }));
    const move = Number(view.volatility_pct_per_hour.toPrecision(4));
    const trend = Number(view.trend_pct_per_hour.toPrecision(4));
    const jump = Number(view.jump_share_pct.toPrecision(4));
    const halfLife = Number(view.half_life_minutes.toPrecision(4));

    return (
        <div className="space-y-4 rounded-md border border-line p-4">
            <p className="text-sm text-ink-muted">
                {t(reverting ? 'marketdata.fxSimple.leadReverting' : 'marketdata.fxSimple.lead')}
            </p>
            <div className="grid gap-4 md:grid-cols-2">
                {reverting ? (
                    <div className="space-y-2">
                        <Answer
                            label={t('marketdata.fxSimple.halfLife')}
                            hint={t('marketdata.fxSimple.halfLifeHint', { price: String(price) })}
                            unit={t('marketdata.fxSimple.minutes')}
                            value={halfLife}
                            positive
                            disabled={!mayWrite}
                            changed={differs(halfLife, saved?.half_life_minutes)}
                            onCommit={(minutes) => set({ half_life_minutes: minutes })}
                        />
                        <Presets
                            items={options(REVERSION_PRESETS, 'reversion')}
                            current={halfLife}
                            disabled={!mayWrite}
                            onPick={(minutes) => set({ half_life_minutes: minutes })}
                        />
                    </div>
                ) : (
                    <div className="space-y-2">
                        <Answer
                            label={t('marketdata.fxSimple.trend')}
                            hint={t('marketdata.fxSimple.trendHint')}
                            unit={t('marketdata.fxSimple.perHour')}
                            value={trend}
                            positive={false}
                            signed
                            disabled={!mayWrite}
                            changed={differs(trend, saved?.trend_pct_per_hour)}
                            onCommit={(value) => set({ trend_pct_per_hour: value })}
                        />
                        <Presets
                            items={options(TREND_PRESETS, 'trend')}
                            current={trend}
                            disabled={!mayWrite}
                            onPick={(value) => set({ trend_pct_per_hour: value })}
                        />
                    </div>
                )}
                <div className="space-y-2">
                    <Answer
                        label={t('marketdata.fxSimple.move')}
                        hint={t('marketdata.fxSimple.moveHint')}
                        unit={t('marketdata.fxSimple.perHour')}
                        value={move}
                        positive={false}
                        disabled={!mayWrite}
                        changed={differs(move, saved?.volatility_pct_per_hour)}
                        onCommit={(value) => set({ volatility_pct_per_hour: value })}
                    />
                    <Presets
                        items={options(MOVE_PRESETS, 'move')}
                        current={move}
                        disabled={!mayWrite}
                        onPick={(value) => set({ volatility_pct_per_hour: value })}
                    />
                </div>
                {!reverting && (
                    <div className="space-y-2">
                        <Answer
                            label={t('marketdata.fxSimple.jumps')}
                            hint={t('marketdata.fxSimple.jumpsHint')}
                            unit={t('marketdata.fxSimple.ofUpdates')}
                            value={jump}
                            positive={false}
                            disabled={!mayWrite}
                            changed={differs(jump, saved?.jump_share_pct)}
                            onCommit={(value) => set({ jump_share_pct: value })}
                        />
                        <Presets
                            items={options(JUMP_PRESETS, 'jumps')}
                            current={jump}
                            disabled={!mayWrite}
                            onPick={(value) => set({ jump_share_pct: value })}
                        />
                    </div>
                )}
            </div>
            <p className="text-sm text-ink">
                {reverting
                    ? t('marketdata.fxSimple.sentenceReverting', {
                          price: String(price),
                          minutes: String(halfLife),
                          move: String(move),
                      })
                    : t(
                          `marketdata.fxSimple.sentence.${
                              trend > 0 ? 'rising' : trend < 0 ? 'falling' : 'level'
                          }`,
                          { trend: String(Math.abs(trend)), move: String(move) },
                      )}
            </p>
        </div>
    );
}

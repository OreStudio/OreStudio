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
import { api } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { Notice } from '../../ui/Primitives.js';
import { toast } from '../../ui/Toasts.js';
import { Answer, Presets, differs } from './SimpleControls.js';
import {
    HALF_LIFE_PRESETS,
    MOVE_PRESETS,
    leanOf,
    percentText,
    rateOf,
    viewOf,
    type SimpleView,
} from './irSimple.js';

/** What the service makes of the values on the screen, in the terms of the simple view. */
export interface IrReading {
    /** The simple view of the values, exact or the closest one; undefined when there is none. */
    readonly view: SimpleView | undefined;
    /** Whether the view stands for the values without loss. */
    readonly exact: boolean;
    /** Why there is no simple view, in the service's words; empty when there is one. */
    readonly reason: string;
    readonly caution: string;
    readonly loading: boolean;
    /** Whether the answer is for an earlier request, while the current one is asked. */
    readonly stale: boolean;
}

/**
 * The simple view of the engine's values on the screen, asked of the service,
 * which knows how each engine reads its numbers. Both modes read it: Simple
 * shows it, and Advanced notes each parameter with it.
 */
export function useIrReading(
    engine: string,
    values: Readonly<Record<string, number>> | undefined,
): IrReading {
    const reading = useQuery({
        queryKey: ['ir-reading', engine, values],
        queryFn: () =>
            api.convertIrParameters({
                process_type: engine,
                to_simple: true,
                values: values ?? {},
            }),
        enabled: values !== undefined,
        placeholderData: keepPreviousData,
        retry: false,
    });
    const data = reading.data;
    return {
        view: data === undefined ? undefined : viewOf(data.values),
        exact: data?.has_simple_form === true,
        reason: data?.has_simple_form === false ? data.message : '',
        caution: data?.caution ?? '',
        loading: reading.isPending && values !== undefined,
        stale: reading.isPlaceholderData,
    };
}

/**
 * The Process tab for a person who does not know short-rate models: four plain
 * questions and a sentence that says what the answers make of the curve. The
 * service turns the answers into the engine's parameters, and what a person
 * types goes into the same draft as the advanced table, so Save, Discard and
 * the previews work as they do there.
 */
export function IrSimplePanel({
    view,
    saved,
    caution,
    mayWrite,
    engine,
    onChange,
}: {
    readonly view: SimpleView;
    /** The answers the saved values make, when the engine is the saved one; fields that differ are tinted. */
    readonly saved: SimpleView | undefined;
    readonly caution: string;
    readonly mayWrite: boolean;
    readonly engine: string;
    /** Writes the answers into the draft as the engine's parameters. */
    readonly onChange: (next: SimpleView) => void;
}): ReactNode {
    const { t } = useTranslation();
    const lean = leanOf(view);
    const set = (patch: Partial<SimpleView>): void => {
        const next = { ...view, ...patch };
        onChange(next);
        toast(
            t(`marketdata.irSimple.lean.${leanOf(next)}`, {
                start: percentText(next.start_rate),
                longRun: percentText(next.long_run_rate),
            }),
            'info',
            'ir-simple-summary',
        );
    };

    return (
        <div className="space-y-4 rounded-md border border-line p-4">
            <p className="text-sm text-ink-muted">{t('marketdata.irSimple.lead')}</p>
            <div className="grid gap-4 md:grid-cols-2">
                <Answer
                    label={t('marketdata.irSimple.start')}
                    hint={t('marketdata.irSimple.startHint')}
                    unit="%"
                    bps
                    value={Number(percentText(view.start_rate))}
                    positive={engine === 'BLACK_KARASINSKI'}
                    disabled={!mayWrite}
                    changed={differs(view.start_rate, saved?.start_rate)}
                    onCommit={(percent) => set({ start_rate: rateOf(percent) })}
                />
                <Answer
                    label={t('marketdata.irSimple.longRun')}
                    hint={t('marketdata.irSimple.longRunHint')}
                    unit="%"
                    bps
                    value={Number(percentText(view.long_run_rate))}
                    positive={engine === 'BLACK_KARASINSKI' || engine === 'COX_INGERSOLL_ROSS'}
                    disabled={!mayWrite}
                    changed={differs(view.long_run_rate, saved?.long_run_rate)}
                    onCommit={(percent) => set({ long_run_rate: rateOf(percent) })}
                />
                <div className="space-y-2">
                    <Answer
                        label={t('marketdata.irSimple.halfLife')}
                        hint={t('marketdata.irSimple.halfLifeHint')}
                        unit={t('marketdata.irSimple.years')}
                        value={Number(view.half_life_years.toPrecision(4))}
                        positive
                        disabled={!mayWrite}
                        changed={differs(view.half_life_years, saved?.half_life_years)}
                        onCommit={(years) => set({ half_life_years: years })}
                    />
                    <Presets
                        items={HALF_LIFE_PRESETS.map((preset) => ({
                            key: preset.key,
                            value: preset.years,
                            label: t(`marketdata.irSimple.presets.${preset.key}`),
                        }))}
                        current={Number(view.half_life_years.toPrecision(4))}
                        disabled={!mayWrite}
                        onPick={(years) => set({ half_life_years: years })}
                    />
                </div>
                <div className="space-y-2">
                    <Answer
                        label={t('marketdata.irSimple.move')}
                        hint={t('marketdata.irSimple.moveHint')}
                        unit={t('marketdata.irSimple.bp')}
                        value={Number(view.yearly_move_bp.toPrecision(4))}
                        positive={false}
                        disabled={!mayWrite}
                        changed={differs(view.yearly_move_bp, saved?.yearly_move_bp)}
                        onCommit={(bp) => set({ yearly_move_bp: bp })}
                    />
                    <Presets
                        items={MOVE_PRESETS.map((preset) => ({
                            key: preset.key,
                            value: preset.bp,
                            label: t(`marketdata.irSimple.presets.${preset.key}`),
                        }))}
                        current={Number(view.yearly_move_bp.toPrecision(4))}
                        disabled={!mayWrite}
                        onPick={(bp) => set({ yearly_move_bp: bp })}
                    />
                </div>
            </div>
            {caution !== '' && (
                <Notice tone="warn">{t(`marketdata.irSimple.caution.${caution}`)}</Notice>
            )}
        </div>
    );
}

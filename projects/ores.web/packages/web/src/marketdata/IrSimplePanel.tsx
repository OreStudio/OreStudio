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
import { useEffect, useState, type ReactNode } from 'react';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Field, Input, Notice, cx } from '../ui/Primitives.js';
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
    readonly view: SimpleView | undefined;
    /** Why there is no simple view, in the service's words; empty when there is one. */
    readonly reason: string;
    readonly caution: string;
    readonly loading: boolean;
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
        view: data === undefined || !data.has_simple_form ? undefined : viewOf(data.values),
        reason: data?.has_simple_form === false ? data.message : '',
        caution: data?.caution ?? '',
        loading: reading.isPending && values !== undefined,
    };
}

/**
 * One answer of the simple view. The text is the person's until they leave the
 * field or press Enter, so a half-typed number never reaches the service.
 */
function Answer({
    label,
    hint,
    unit,
    value,
    positive,
    disabled,
    onCommit,
}: {
    readonly label: string;
    readonly hint: string;
    readonly unit: string;
    readonly value: number;
    /** Whether zero is refused as well as a negative number. */
    readonly positive: boolean;
    readonly disabled: boolean;
    readonly onCommit: (value: number) => void;
}): ReactNode {
    const { t } = useTranslation();
    const [text, setText] = useState(String(value));
    useEffect(() => setText(String(value)), [value]);
    const parsed = Number(text);
    const wrong =
        text.trim() === '' || !Number.isFinite(parsed) || parsed < 0 || (positive && parsed === 0);
    const commit = (): void => {
        if (!wrong && parsed !== value) {
            onCommit(parsed);
        }
    };
    return (
        <Field
            label={label}
            hint={hint}
            {...(wrong ? { error: t('marketdata.irSimple.number') } : {})}
        >
            <div className="flex items-center gap-2">
                <Input
                    className="max-w-40 font-mono"
                    inputMode="decimal"
                    value={text}
                    disabled={disabled}
                    aria-invalid={wrong}
                    onChange={(event) => setText(event.target.value)}
                    onBlur={commit}
                    onKeyDown={(event) => {
                        if (event.key === 'Enter') {
                            commit();
                        }
                    }}
                />
                <span className="text-sm text-ink-muted">{unit}</span>
            </div>
        </Field>
    );
}

/** A row of ready answers, the one that matches the value marked. */
function Presets({
    items,
    current,
    disabled,
    onPick,
}: {
    readonly items: readonly {
        readonly key: string;
        readonly value: number;
        readonly label: string;
    }[];
    readonly current: number;
    readonly disabled: boolean;
    readonly onPick: (value: number) => void;
}): ReactNode {
    return (
        <div className="flex flex-wrap gap-2">
            {items.map((item) => (
                <button
                    key={item.key}
                    type="button"
                    disabled={disabled}
                    aria-pressed={Math.abs(item.value - current) < 1e-9}
                    className={cx(
                        'rounded-full border px-3 py-0.5 text-xs disabled:opacity-50',
                        Math.abs(item.value - current) < 1e-9
                            ? 'border-accent/60 bg-accent/10 text-ink'
                            : 'border-line text-ink-muted hover:bg-surface-hover',
                    )}
                    onClick={() => onPick(item.value)}
                >
                    {item.label}
                </button>
            ))}
        </div>
    );
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
    caution,
    mayWrite,
    engine,
    onChange,
}: {
    readonly view: SimpleView;
    readonly caution: string;
    readonly mayWrite: boolean;
    readonly engine: string;
    /** Writes the answers into the draft as the engine's parameters. */
    readonly onChange: (next: SimpleView) => void;
}): ReactNode {
    const { t } = useTranslation();
    const lean = leanOf(view);
    const set = (patch: Partial<SimpleView>): void => onChange({ ...view, ...patch });

    return (
        <div className="space-y-4 rounded-md border border-line p-4">
            <p className="text-sm text-ink-muted">{t('marketdata.irSimple.lead')}</p>
            <div className="grid gap-4 md:grid-cols-2">
                <Answer
                    label={t('marketdata.irSimple.start')}
                    hint={t('marketdata.irSimple.startHint')}
                    unit="%"
                    value={Number(percentText(view.start_rate))}
                    positive={engine === 'BLACK_KARASINSKI'}
                    disabled={!mayWrite}
                    onCommit={(percent) => set({ start_rate: rateOf(percent) })}
                />
                <Answer
                    label={t('marketdata.irSimple.longRun')}
                    hint={t('marketdata.irSimple.longRunHint')}
                    unit="%"
                    value={Number(percentText(view.long_run_rate))}
                    positive={engine === 'BLACK_KARASINSKI' || engine === 'COX_INGERSOLL_ROSS'}
                    disabled={!mayWrite}
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
            <p className="text-sm text-ink">
                {t(`marketdata.irSimple.lean.${lean}`, {
                    start: percentText(view.start_rate),
                    longRun: percentText(view.long_run_rate),
                })}
            </p>
            {caution !== '' && (
                <Notice tone="warn">{t(`marketdata.irSimple.caution.${caution}`)}</Notice>
            )}
        </div>
    );
}

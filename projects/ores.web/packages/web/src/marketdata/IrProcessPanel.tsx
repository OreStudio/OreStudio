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
import { useState, type ReactNode } from 'react';
import { api, type RecordRow } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { show, useRecords } from '../refdata/records.js';
import { Button, Dialog, Field, Input, Notice, Select, Tag, cx } from '../ui/Primitives.js';
import {
    parameterLines,
    parameterLimits,
    parameterError,
    parameterPlan,
    type ParameterLine,
} from './irCurve.js';
import { LineChart, SERIES_COLOURS } from './LineChart.js';
import { useSettled } from './MixturePanel.js';
import { SavePartsDialog, type SaveEngine } from './SavePartsDialog.js';

export const NO_ROWS: readonly RecordRow[] = [];

/** The engine a curve runs, named as the Qt editor names it. */
export function engineName(t: (key: string) => string, code: string): string {
    return t(`marketdata.irFeeds.engine.${code}`);
}

/** A cell whose value differs from the saved one, as the FX mixture marks it. */
const CHANGED_CELL = 'bg-warn/10 text-warn';

/** The words that say what a parameter accepts, for its hint and its refusal. */
function limitsText(
    t: (key: string, values?: Record<string, string>) => string,
    min: number | undefined,
    max: number | undefined,
): string {
    const { kind } = parameterLimits(min, max);
    return t(`marketdata.irProcess.limits.${kind}`, {
        min: String(min ?? ''),
        max: String(max ?? ''),
    });
}

/**
 * The Process tab, laid out as the price behaviour tab of an FX feed: a toolbar
 * with the unsaved changes, the engine, Discard and Save; a table of the
 * engine's parameters, read only, where a click opens one in a dialog; then the
 * previews. The values are held in a draft and written together with the engine
 * under one reason, because a change of engine replaces the parameters.
 */
export function IrProcessPanel({
    feed,
    running,
    mayWrite,
    saved,
    entries,
    valuesResource,
    saveEngine,
}: {
    readonly feed: RecordRow;
    readonly running: boolean;
    readonly mayWrite: boolean;
    readonly saved: readonly RecordRow[];
    readonly entries: readonly RecordRow[];
    /** The resource the parameter values are written to. */
    readonly valuesResource: string;
    readonly saveEngine: SaveEngine;
}): ReactNode {
    const { t } = useTranslation();
    const feedId = show(feed['id']);
    const types = useRecords('process-types');
    const definitions = useRecords('parameter-definitions');
    const savedEngine = show(feed['process_type']);
    const [engine, setEngine] = useState(savedEngine);
    const [typed, setTyped] = useState<Readonly<Record<string, string>>>({});
    const [open, setOpen] = useState<ParameterLine | null>(null);
    const [saving, setSaving] = useState(false);
    const lines = parameterLines(definitions.data ?? NO_ROWS, saved, engine);
    const valueOf = (line: ParameterLine): string => typed[show(line.definition['id'])] ?? line.value;
    const plan = parameterPlan(feedId, lines, typed, saved, () => crypto.randomUUID());
    const problems = lines.filter((line) => parameterError(line.definition, valueOf(line)) !== undefined);
    const engineChanged = engine !== savedEngine;
    const dirty = engineChanged || plan.changes.length > 0 || plan.removals.length > 0;
    const counts = {
        added: plan.changes.filter((change) => change.version === null).length,
        changed: plan.changes.filter((change) => change.version !== null).length,
        removed: plan.removals.length,
    };
    const unsaved = counts.added + counts.changed + counts.removed + (engineChanged ? 1 : 0);

    return (
        <div className="space-y-4">
            <section className="space-y-3">
                <p className="text-sm text-ink-muted">{t('marketdata.irProcess.lead')}</p>
                <div className="flex flex-wrap items-center gap-2">
                    {dirty ? (
                        <Tag tone="warn">
                            {t('marketdata.irProcess.unsaved', { count: String(unsaved) })}
                        </Tag>
                    ) : (
                        <span className="text-sm text-ink-faint">
                            {t('marketdata.irProcess.clean')}
                        </span>
                    )}
                    <label className="ml-auto flex items-center gap-2 text-sm">
                        <span className="text-ink-muted">{t('refdata.fields.process_type')}</span>
                        <Select
                            className={engineChanged ? 'border-warn' : ''}
                            value={engine}
                            disabled={!mayWrite}
                            onChange={(event) => {
                                setEngine(event.target.value);
                                setTyped({});
                            }}
                        >
                            {(types.data ?? NO_ROWS).map((row) => (
                                <option key={show(row['code'])} value={show(row['code'])}>
                                    {show(row['name'])}
                                </option>
                            ))}
                        </Select>
                    </label>
                    {mayWrite && (
                        <div className="flex gap-2">
                            <Button
                                variant="ghost"
                                icon="cancel"
                                disabled={!dirty}
                                onClick={() => {
                                    setEngine(savedEngine);
                                    setTyped({});
                                }}
                            >
                                {t('marketdata.irProcess.discard')}
                            </Button>
                            <Button
                                variant="primary"
                                icon="save"
                                disabled={!dirty || problems.length > 0}
                                onClick={() => setSaving(true)}
                            >
                                {t('marketdata.irProcess.save')}
                            </Button>
                        </div>
                    )}
                </div>
                {engineChanged && (
                    <Notice tone="info">
                        {t('marketdata.irProcess.engineChanged', {
                            from: engineName(t, savedEngine),
                            to: engineName(t, engine),
                        })}
                    </Notice>
                )}
                <div className="overflow-x-auto rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                <th className="px-4 py-2 font-medium">
                                    {t('marketdata.irProcess.parameter')}
                                </th>
                                <th className="px-4 py-2 text-right font-medium">
                                    {t('marketdata.irProcess.value')}
                                </th>
                                <th
                                    className="px-4 py-2 text-right font-medium"
                                    title={t('marketdata.irProcess.minHelp')}
                                >
                                    {t('marketdata.irProcess.min')}
                                </th>
                                <th
                                    className="px-4 py-2 text-right font-medium"
                                    title={t('marketdata.irProcess.maxHelp')}
                                >
                                    {t('marketdata.irProcess.max')}
                                </th>
                                <th className="px-4 py-2" />
                            </tr>
                        </thead>
                        <tbody>
                            {lines.length === 0 && (
                                <tr>
                                    <td colSpan={5} className="px-4 py-3 text-ink-muted">
                                        {t('marketdata.irProcess.none', {
                                            engine: engineName(t, engine),
                                        })}
                                    </td>
                                </tr>
                            )}
                            {lines.map((line) => {
                                const id = show(line.definition['id']);
                                const value = valueOf(line);
                                const added = line.saved === undefined;
                                const changed =
                                    !added && Number(value) !== Number(line.saved?.['parameter_value']);
                                const wrong = parameterError(line.definition, value) !== undefined;
                                return (
                                    <tr
                                        key={id}
                                        className={cx(
                                            'border-b border-line-subtle last:border-b-0',
                                            mayWrite && 'cursor-pointer hover:bg-surface-hover',
                                            added && 'bg-up/5',
                                        )}
                                        title={show(line.definition['description'])}
                                        {...(mayWrite ? { onClick: () => setOpen(line) } : {})}
                                    >
                                        <td className="max-w-md px-4 py-2">
                                            <span className="font-medium text-ink">
                                                {show(line.definition['display_name'])}
                                            </span>
                                            {show(line.definition['symbol']) !== '' && (
                                                <span className="text-ink-faint">
                                                    {' '}
                                                    · {show(line.definition['symbol'])}
                                                </span>
                                            )}
                                        </td>
                                        <td
                                            className={cx(
                                                'px-4 py-2 text-right font-mono text-xs',
                                                changed && CHANGED_CELL,
                                                wrong && 'text-down',
                                            )}
                                        >
                                            {value}
                                        </td>
                                        <td className="px-4 py-2 text-right font-mono text-xs text-ink-muted">
                                            {show(line.definition['min_value']) || '—'}
                                        </td>
                                        <td className="px-4 py-2 text-right font-mono text-xs text-ink-muted">
                                            {show(line.definition['max_value']) || '—'}
                                        </td>
                                        <td className="px-4 py-2 text-right whitespace-nowrap">
                                            {added && (
                                                <Tag tone="up" small>
                                                    {t('marketdata.irProcess.marks.added')}
                                                </Tag>
                                            )}
                                            {changed && (
                                                <Tag tone="warn" small>
                                                    {t('marketdata.irProcess.marks.changed')}
                                                </Tag>
                                            )}
                                        </td>
                                    </tr>
                                );
                            })}
                        </tbody>
                    </table>
                </div>
            </section>
            <IrPreview
                feed={feed}
                engine={engine}
                lines={lines}
                typed={typed}
                entries={entries}
                valid={problems.length === 0}
            />
            {open !== null && (
                <ParameterDialog
                    line={open}
                    value={valueOf(open)}
                    onDone={(value) => {
                        setTyped({ ...typed, [show(open.definition['id'])]: value });
                        setOpen(null);
                    }}
                    onClose={() => setOpen(null)}
                />
            )}
            {saving && (
                <SavePartsDialog
                    feedId={feedId}
                    resource={valuesResource}
                    words="irProcess"
                    plan={plan}
                    counts={counts}
                    engine={engineChanged ? { from: savedEngine, to: engine } : undefined}
                    engineLabel={(code) => engineName(t, code)}
                    saveEngine={saveEngine}
                    running={running}
                    onClose={() => {
                        setSaving(false);
                        setTyped({});
                    }}
                />
            )}
        </div>
    );
}

/**
 * One parameter of the engine, edited on its own. Done puts the value back into
 * the table on the screen; nothing is saved until the process is.
 */
function ParameterDialog({
    line,
    value,
    onDone,
    onClose,
}: {
    readonly line: ParameterLine;
    readonly value: string;
    readonly onDone: (value: string) => void;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const [typed, setTyped] = useState(value);
    const { definition } = line;
    const min = typeof definition['min_value'] === 'number' ? definition['min_value'] : undefined;
    const max = typeof definition['max_value'] === 'number' ? definition['max_value'] : undefined;
    const problem = parameterError(definition, typed);

    return (
        <Dialog
            wide
            title={t('marketdata.irProcess.editTitle', { name: show(definition['display_name']) })}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('refdata.records.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        disabled={problem !== undefined}
                        onClick={() => onDone(typed.trim())}
                    >
                        {t('marketdata.mixture.done')}
                    </Button>
                </>
            }
        >
            <div className="space-y-4">
                {show(definition['description']) !== '' && (
                    <p className="text-sm text-ink-muted">{show(definition['description'])}</p>
                )}
                <Field
                    label={
                        show(definition['symbol']) === ''
                            ? show(definition['display_name'])
                            : `${show(definition['display_name'])} (${show(definition['symbol'])})`
                    }
                    hint={limitsText(t, min, max)}
                >
                    <Input
                        className="font-mono"
                        inputMode="decimal"
                        value={typed}
                        aria-invalid={problem !== undefined}
                        onChange={(event) => setTyped(event.target.value)}
                    />
                </Field>
                {problem !== undefined && (
                    <Notice tone="warn">
                        {t(`marketdata.irProcess.problems.${problem}`, {
                            min: String(min ?? ''),
                            max: String(max ?? ''),
                        })}
                    </Notice>
                )}
            </div>
        </Dialog>
    );
}

const MAX_TICKS = 5000;
const MAX_PATHS = 50;

function whole(text: string, low: number, high: number): number | undefined {
    const value = Number(text);
    return Number.isInteger(value) && value >= low && value <= high ? value : undefined;
}

function rate(value: number): string {
    return `${(value * 100).toFixed(3)}%`;
}

/**
 * What the engine and the parameters on the screen imply: the curve they price
 * at the template's tenor entries, and a batch of short-rate paths. The server
 * prices both from the values on the screen, not from the saved feed, so a
 * person judges the values before saving them.
 */
function IrPreview({
    feed,
    engine,
    lines,
    typed,
    entries,
    valid,
}: {
    readonly feed: RecordRow;
    readonly engine: string;
    readonly lines: readonly ParameterLine[];
    readonly typed: Readonly<Record<string, string>>;
    readonly entries: readonly RecordRow[];
    readonly valid: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const [ticks, setTicks] = useState('250');
    const [paths, setPaths] = useState('5');
    const [seed, setSeed] = useState(42);
    const parameters = lines.map((line) => ({
        parameter_name: show(line.definition['parameter_name']),
        parameter_value: Number(typed[show(line.definition['id'])] ?? line.value),
    }));
    const ready = valid && parameters.length > 0;
    const ladder = [...entries]
        .sort((a, b) => Number(a['sequence_index']) - Number(b['sequence_index']))
        .map((row, index) => ({
            sequence_index: index,
            start_tenor_code: show(row['start_tenor_code']),
            end_tenor_code: show(row['end_tenor_code']),
            instrument_code: show(row['instrument_code']),
        }));
    const shapeRequest =
        ready && ladder.length > 0
            ? {
                  process_type: engine,
                  parameters,
                  fixed_leg_payment_frequency_code: show(feed['fixed_leg_payment_frequency_code']),
                  entries: ladder,
              }
            : undefined;
    const numTicks = whole(ticks, 2, MAX_TICKS);
    const numPaths = whole(paths, 1, MAX_PATHS);
    const pathsRequest =
        ready && numTicks !== undefined && numPaths !== undefined
            ? { process_type: engine, parameters, num_ticks: numTicks, num_paths: numPaths, seed }
            : undefined;
    const settledShape = useSettled(shapeRequest);
    const settledPaths = useSettled(pathsRequest);
    const shape = useQuery({
        queryKey: ['preview-ir-shape', settledShape],
        queryFn: () => (settledShape === undefined ? [] : api.previewIrShape(settledShape)),
        enabled: settledShape !== undefined,
        placeholderData: keepPreviousData,
        retry: false,
    });
    const sample = useQuery({
        queryKey: ['simulate-ir', settledPaths],
        queryFn: () => (settledPaths === undefined ? [] : api.simulateIr(settledPaths)),
        enabled: settledPaths !== undefined,
        placeholderData: keepPreviousData,
        retry: false,
    });
    const points = shape.data ?? [];

    return (
        <section className="grid items-start gap-4 xl:grid-cols-2">
            <div className="space-y-2 rounded-md border border-line p-3">
                <h3 className="text-sm font-medium">{t('marketdata.irProcess.shape')}</h3>
                {shapeRequest === undefined ? (
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.irProcess.shapeWaiting')}
                    </p>
                ) : shape.isError ? (
                    <Notice tone="error">{shape.error.message}</Notice>
                ) : points.length < 2 ? (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                ) : (
                    <LineChart
                        label={t('marketdata.irProcess.shape')}
                        format={rate}
                        xFormat={(index) => points[Math.round(index)]?.end_tenor_code ?? ''}
                        series={[
                            {
                                points: points.map((point) => point.rate),
                                colour: SERIES_COLOURS[0] ?? '',
                                strong: true,
                                fill: true,
                            },
                        ]}
                    />
                )}
            </div>
            <div className="space-y-3 rounded-md border border-line p-3">
                <h3 className="text-sm font-medium">{t('marketdata.irProcess.paths')}</h3>
                {pathsRequest === undefined ? (
                    <p className="text-sm text-ink-muted">{t('marketdata.preview.waiting')}</p>
                ) : sample.isError ? (
                    <Notice tone="error">{sample.error.message}</Notice>
                ) : sample.data === undefined || sample.data.length === 0 ? (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                ) : (
                    <LineChart
                        label={t('marketdata.irProcess.paths')}
                        format={rate}
                        xFormat={(tick) => String(tick)}
                        series={sample.data.map((one, i) => ({
                            points: one,
                            colour: SERIES_COLOURS[i % SERIES_COLOURS.length] ?? '',
                        }))}
                    />
                )}
                <div className="grid gap-3 sm:grid-cols-3">
                    <Field label={t('marketdata.preview.ticks')}>
                        <Input
                            inputMode="numeric"
                            value={ticks}
                            onChange={(event) => setTicks(event.target.value)}
                        />
                    </Field>
                    <Field label={t('marketdata.preview.pathCount')}>
                        <Input
                            inputMode="numeric"
                            value={paths}
                            onChange={(event) => setPaths(event.target.value)}
                        />
                    </Field>
                    <Field label={t('marketdata.preview.seed')}>
                        <div className="flex items-center gap-2">
                            <span className="font-mono text-sm">{seed}</span>
                            <Button
                                size="sm"
                                icon="refresh"
                                onClick={() => setSeed(Math.floor(Math.random() * 1_000_000))}
                            >
                                {t('marketdata.preview.reseed')}
                            </Button>
                        </div>
                    </Field>
                </div>
                <p className="text-xs text-ink-faint">
                    {t('marketdata.irProcess.previewNote', { engine: engineName(t, engine) })}
                </p>
            </div>
        </section>
    );
}

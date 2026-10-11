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
import {
    PartsPanel,
    show,
    useRecords,
    type FieldSpec,
    type FieldValues,
} from '../refdata/records.js';
import { syntheticStore } from '../refdata/store.js';
import { Button, Field, Input, Notice, Tag } from '../ui/Primitives.js';
import { FeedPage, FeedsPage } from './FeedScreens.js';
import { feedPath, type FeedKind } from './feedKind.js';
import {
    INDEX_FAMILIES,
    TERM_FAMILIES,
    completeIrFeed,
    curveOf,
    irSeriesUri,
    parameterError,
    parameterLines,
    parameterPlan,
    type ParameterLine,
} from './irCurve.js';
import { LineChart, SERIES_COLOURS } from './LineChart.js';
import { useSettled } from './MixturePanel.js';
import { IrTicksPanel, savedParameters } from './IrTicksPanel.js';
import { SavePartsDialog, type SaveEngine } from './SavePartsDialog.js';

const FEEDS = 'ir-curve-feeds';
export const PARAMETER_VALUES = 'ir-parameter-values';
export const TEMPLATE_ENTRIES = 'ir-template-entries';

const ROLES = ['self_discounting', 'discount', 'projection'];
const PRICE_SOURCES = ['fixed', 'vintage'];

/** Whether the family chosen in the form is an index with a term. */
const isTerm = (values: FieldValues): boolean =>
    TERM_FAMILIES.includes(values['index_family'] ?? '');

/**
 * The curve's own fields, in the words of the Qt IR curve editor: the
 * currency, the index name, the role, the fixed leg frequency and how often
 * a tick is made, then where the starting rate comes from. The source name is
 * derived and may be changed. The engine and its parameters are on the Process
 * tab, where they are saved together.
 */
const IR_FIELDS: readonly FieldSpec[] = [
    {
        field: 'config_id',
        history: 'Config ID',
        kind: {
            kind: 'record',
            resource: 'collections',
            value: 'id',
            label: 'name',
            store: syntheticStore,
        },
        fixed: true,
    },
    {
        field: 'currency_code',
        history: 'Currency Code',
        kind: { kind: 'record', resource: 'currencies', value: 'iso_code', label: 'name' },
        flag: 'currency',
        fixed: true,
    },
    {
        field: 'index_family',
        history: 'Index Family',
        kind: { kind: 'choice', options: INDEX_FAMILIES },
        fixed: true,
    },
    {
        field: 'tenor',
        history: 'Tenor',
        kind: { kind: 'text', max: 20 },
        blank: true,
        when: (values) => isTerm(values) || values['index_family'] === 'sofr',
        help: 'marketdata.irFeeds.help.tenor',
        fixed: true,
    },
    {
        field: 'role',
        history: 'Role',
        kind: { kind: 'choice', options: ROLES },
        help: 'marketdata.irFeeds.help.role',
    },
    {
        field: 'fixed_leg_payment_frequency_code',
        history: 'Fixed Leg Payment Frequency Code',
        kind: {
            kind: 'record',
            resource: 'payment-frequencies',
            value: 'code',
            label: 'code',
            store: syntheticStore,
        },
    },
    {
        field: 'source_name',
        history: 'Source Name',
        kind: { kind: 'text', max: 200 },
        blank: true,
    },
    {
        field: 'description',
        history: 'Description',
        kind: { kind: 'text' },
        blank: true,
    },
    {
        field: 'process_type',
        history: 'Process Type',
        kind: {
            kind: 'record',
            resource: 'process-types',
            value: 'code',
            label: 'name',
            store: syntheticStore,
        },
    },
    {
        field: 'ticks_per_hour',
        history: 'Ticks Per Hour',
        kind: { kind: 'int', min: 1, max: 360000 },
        help: 'marketdata.irFeeds.help.ticks_per_hour',
    },
    {
        field: 'price_source',
        history: 'Price Source',
        kind: { kind: 'choice', options: PRICE_SOURCES },
        labelKey: 'marketdata.irFeeds.priceSource',
        help: 'marketdata.irFeeds.help.price_source',
    },
    {
        field: 'vintage_source',
        history: 'Vintage Source',
        kind: { kind: 'text', max: 200 },
        when: (values) => values['price_source'] === 'vintage',
    },
    {
        field: 'vintage_date',
        history: 'Vintage Date',
        kind: { kind: 'date' },
        when: (values) => values['price_source'] === 'vintage',
    },
    {
        field: 'vintage_series_uri',
        history: 'Vintage Series Uri',
        kind: { kind: 'text', max: 2000 },
        blank: true,
        when: (values) => values['price_source'] === 'vintage',
    },
    {
        field: 'enabled',
        history: 'Enabled',
        kind: { kind: 'bool' },
        help: 'marketdata.irFeeds.help.enabled',
    },
    {
        field: 'auto_start',
        history: 'Auto Start',
        kind: { kind: 'bool' },
        help: 'marketdata.irFeeds.help.auto_start',
    },
];

/** The fields the server needs that the form does not edit. */
const IR_KEPT = ['folder_id'];

/** The fields the edit dialog offers: the engine is changed on the Process tab, with its parameters. */
const IR_EDIT_FIELDS = IR_FIELDS.filter((spec) => spec.field !== 'process_type');

/** The fields a curve feed write carries, as the BFF takes them. */
const IR_WRITE_FIELDS = [
    'id',
    'config_id',
    'currency_code',
    'index_family',
    'tenor',
    'role',
    'process_type',
    'ticks_per_hour',
    'enabled',
    'auto_start',
    'price_source',
    'vintage_source',
    'vintage_date',
    'vintage_series_uri',
    'description',
    'fixed_leg_payment_frequency_code',
    'source_name',
    'folder_id',
] as const;

/** The feed as written back with a change, such as a new engine. */
export function irFeedWrite(
    feed: Readonly<Record<string, unknown>>,
    change: Readonly<Record<string, unknown>>,
): Record<string, unknown> {
    return {
        ...Object.fromEntries(IR_WRITE_FIELDS.map((field) => [field, feed[field] ?? null])),
        ...change,
    };
}

const NEW_IR_FEED: FieldValues = {
    index_family: 'sofr',
    role: 'self_discounting',
    fixed_leg_payment_frequency_code: 'Annual',
    process_type: 'VASICEK',
    ticks_per_hour: '3600',
    price_source: 'fixed',
    vintage_series_uri: '',
    description: '',
    enabled: 'false',
    auto_start: 'false',
};

/** The rules across fields that the table checks, said before the save rather than after it. */
function useIrFeedCheck(): (values: FieldValues) => readonly string[] {
    const { t } = useTranslation();
    return (values) => {
        const family = values['index_family'] ?? '';
        const tenor = values['tenor'] ?? '';
        if (TERM_FAMILIES.includes(family) && tenor === '') {
            return [t('marketdata.irFeeds.tenorNeeded')];
        }
        if (family === 'sofr' && tenor !== '' && tenor !== 'FOMC') {
            return [t('marketdata.irFeeds.tenorOnlyFomc')];
        }
        return [];
    };
}

/** The template entries of a curve, whose fields the page offers. */
const ENTRY_FIELDS: readonly FieldSpec[] = [
    { field: 'sequence_index', history: 'Sequence Index', kind: { kind: 'int', min: 0 } },
    {
        field: 'start_tenor_code',
        history: 'Start Tenor Code',
        kind: {
            kind: 'record',
            resource: 'tenors',
            value: 'code',
            label: 'code',
            store: syntheticStore,
        },
    },
    {
        field: 'end_tenor_code',
        history: 'End Tenor Code',
        kind: {
            kind: 'record',
            resource: 'tenors',
            value: 'code',
            label: 'code',
            store: syntheticStore,
        },
    },
    {
        field: 'instrument_code',
        history: 'Instrument Code',
        kind: {
            kind: 'record',
            resource: 'instrument-codes',
            value: 'code',
            label: 'code',
            store: syntheticStore,
        },
    },
];

/** A parameter value's fields, for its revert; the Process tab edits the rest. */
const PARAMETER_FIELDS: readonly FieldSpec[] = [
    {
        field: 'parameter_value',
        history: 'Parameter Value',
        kind: { kind: 'decimal' },
    },
];

/** The engine a curve runs, named as the Qt editor names it. */
function engineName(t: (key: string) => string, code: string): string {
    return t(`marketdata.irFeeds.engine.${code}`);
}

const NO_ROWS: readonly RecordRow[] = [];

/**
 * The Process tab: the engine and the parameters it reads, in the words of the
 * Qt editor. The values are typed here and saved together with the engine, under
 * one reason, because a change of engine replaces the parameters.
 */
function IrProcessPanel({
    feed,
    running,
    mayWrite,
    saved,
    entries,
}: {
    readonly feed: RecordRow;
    readonly running: boolean;
    readonly mayWrite: boolean;
    readonly saved: readonly RecordRow[];
    readonly entries: readonly RecordRow[];
}): ReactNode {
    const { t } = useTranslation();
    const feedId = show(feed['id']);
    const types = useRecords('process-types');
    const definitions = useRecords('parameter-definitions');
    const savedEngine = show(feed['process_type']);
    const [engine, setEngine] = useState(savedEngine);
    const [typed, setTyped] = useState<Readonly<Record<string, string>>>({});
    const [saving, setSaving] = useState(false);
    const lines = parameterLines(definitions.data ?? NO_ROWS, saved, engine);
    const plan = parameterPlan(feedId, lines, typed, saved, () => crypto.randomUUID());
    const errors = lines.filter(
        (line) =>
            parameterError(line.definition, typed[show(line.definition['id'])] ?? line.value) !==
            undefined,
    );
    const engineChanged = engine !== savedEngine;
    const changed = engineChanged || plan.changes.length > 0 || plan.removals.length > 0;
    const counts = {
        added: plan.changes.filter((change) => change.version === null).length,
        changed: plan.changes.filter((change) => change.version !== null).length,
        removed: plan.removals.length,
    };
    const saveEngine: SaveEngine = async (code, intent) => {
        await api.saveSynthetic(
            FEEDS,
            [{ write: irFeedWrite(feed, { process_type: code }), version: feed.version }],
            intent,
        );
    };

    return (
        <section className="space-y-4">
            <p className="text-sm text-ink-muted">{t('marketdata.irProcess.lead')}</p>
            <div className="flex flex-wrap items-end gap-4">
                <label className="block">
                    <span className="block text-[10px] tracking-wide text-ink-faint uppercase">
                        {t('marketdata.irProcess.engine')}
                    </span>
                    <select
                        className="rounded-md border border-line bg-surface px-2 py-1.5 text-sm"
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
                    </select>
                </label>
                {mayWrite && (
                    <>
                        <Button
                            variant="primary"
                            icon="save"
                            disabled={!changed || errors.length > 0}
                            onClick={() => setSaving(true)}
                        >
                            {t('marketdata.irProcess.save')}
                        </Button>
                        <Button
                            variant="ghost"
                            disabled={!changed}
                            onClick={() => {
                                setEngine(savedEngine);
                                setTyped({});
                            }}
                        >
                            {t('marketdata.irProcess.discard')}
                        </Button>
                    </>
                )}
                <span className="text-xs text-ink-faint">
                    {changed ? t('marketdata.irProcess.unsaved') : t('marketdata.irProcess.clean')}
                </span>
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
                            <th className="px-4 py-2 font-medium">
                                {t('marketdata.irProcess.value')}
                            </th>
                            <th className="px-4 py-2 font-medium">
                                {t('marketdata.irProcess.bounds')}
                            </th>
                        </tr>
                    </thead>
                    <tbody>
                        {lines.length === 0 && (
                            <tr>
                                <td colSpan={3} className="px-4 py-3 text-ink-muted">
                                    {t('marketdata.irProcess.none', {
                                        engine: engineName(t, engine),
                                    })}
                                </td>
                            </tr>
                        )}
                        {lines.map((line) => {
                            const id = show(line.definition['id']);
                            const value = typed[id] ?? line.value;
                            const problem = parameterError(line.definition, value);
                            const mark =
                                line.saved === undefined
                                    ? ('added' as const)
                                    : Number(value) === Number(line.saved['parameter_value'])
                                      ? undefined
                                      : ('changed' as const);
                            return (
                                <tr
                                    key={id}
                                    className="border-b border-line-subtle last:border-b-0"
                                >
                                    <td
                                        className="px-4 py-2"
                                        title={show(line.definition['description'])}
                                    >
                                        {show(line.definition['display_name'])}
                                        {show(line.definition['symbol']) !== '' && (
                                            <span className="ml-2 text-ink-muted">
                                                ({show(line.definition['symbol'])})
                                            </span>
                                        )}
                                        {mark !== undefined && (
                                            <Tag tone={mark === 'added' ? 'accent' : 'warn'} small>
                                                {t(`marketdata.irProcess.marks.${mark}`)}
                                            </Tag>
                                        )}
                                    </td>
                                    <td className="px-4 py-1.5">
                                        <Input
                                            type="text"
                                            inputMode="decimal"
                                            className="w-40 font-mono"
                                            value={value}
                                            disabled={!mayWrite}
                                            aria-label={show(line.definition['display_name'])}
                                            aria-invalid={problem !== undefined}
                                            onChange={(event) =>
                                                setTyped({ ...typed, [id]: event.target.value })
                                            }
                                        />
                                        {problem !== undefined && (
                                            <span className="ml-2 text-xs text-down">
                                                {t(`marketdata.irProcess.error.${problem}`)}
                                            </span>
                                        )}
                                    </td>
                                    <td className="px-4 py-2 font-mono text-xs text-ink-muted">
                                        {show(line.definition['min_value']) === ''
                                            ? '−∞'
                                            : show(line.definition['min_value'])}
                                        {' … '}
                                        {show(line.definition['max_value']) === ''
                                            ? '∞'
                                            : show(line.definition['max_value'])}
                                    </td>
                                </tr>
                            );
                        })}
                    </tbody>
                </table>
            </div>
            <IrPreview
                feed={feed}
                engine={engine}
                lines={lines}
                typed={typed}
                entries={entries}
                valid={errors.length === 0}
            />
            {saving && (
                <SavePartsDialog
                    feedId={feedId}
                    resource={PARAMETER_VALUES}
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
        </section>
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

/** The Curve Template tab: the tenor entries the curve is priced at, as parts of the feed. */
function IrTemplatePanel({
    feed,
    entries,
}: {
    readonly feed: RecordRow;
    readonly entries: readonly RecordRow[];
}): ReactNode {
    const { t } = useTranslation();
    return (
        <PartsPanel
            resource={TEMPLATE_ENTRIES}
            entityType="ores.synthetic.ir_curve_template_entry"
            specs={ENTRY_FIELDS}
            parentField="ir_curve_config_id"
            parent={show(feed['id'])}
            rows={entries}
            error={undefined}
            columns={[
                {
                    header: t('refdata.fields.sequence_index'),
                    cell: (row) => show(row['sequence_index']),
                    mono: true,
                },
                {
                    header: t('refdata.fields.start_tenor_code'),
                    cell: (row) => show(row['start_tenor_code']),
                    mono: true,
                },
                {
                    header: t('refdata.fields.end_tenor_code'),
                    cell: (row) => show(row['end_tenor_code']),
                    mono: true,
                },
                {
                    header: t('refdata.fields.instrument_code'),
                    cell: (row) => show(row['instrument_code']),
                    mono: true,
                },
            ]}
            sort={(row) => show(row['sequence_index']).padStart(6, '0')}
            initial={{ sequence_index: String(entries.length) }}
            lead={t('marketdata.irTemplate.lead')}
            titles={{
                add: t('marketdata.irTemplate.add'),
                one: t('marketdata.irTemplate.one'),
                remove: t('marketdata.irTemplate.remove'),
            }}
        />
    );
}

/** The Ticks tab: the last curves of the feed, with the saved process beside them. */
function IrTicksTab({
    feed,
    running,
    values,
    entries,
}: {
    readonly feed: RecordRow;
    readonly running: boolean | undefined;
    readonly values: readonly RecordRow[];
    readonly entries: readonly RecordRow[];
}): ReactNode {
    const definitions = useRecords('parameter-definitions');
    return (
        <IrTicksPanel
            feed={feed}
            running={running}
            entries={entries}
            parameters={savedParameters(
                definitions.data ?? NO_ROWS,
                values,
                show(feed['process_type']),
            )}
        />
    );
}

/** The IR curve feeds, as the shared feed screens draw them. */
const IR_FEED_KIND: FeedKind = {
    resource: FEEDS,
    entity: 'ores.synthetic.ir_curve_generation_config',
    historySubject: 'ir_feed',
    path: '/market-data/ir-feeds',
    words: 'irFeeds',
    fields: IR_FIELDS,
    editFields: IR_EDIT_FIELDS,
    kept: IR_KEPT,
    initial: NEW_IR_FEED,
    complete: completeIrFeed,
    useCheck: useIrFeedCheck,
    nameOf: curveOf,
    detailOf: (feed, t) => engineName(t, show(feed['process_type'])),
    columns: (t) => [
        {
            id: 'curve',
            header: t('marketdata.irFeeds.curve'),
            cell: (row) => curveOf(row),
            mono: true,
            sort: 'currency_code',
            flag: { source: 'currency', code: (row) => show(row['currency_code']) },
        },
        {
            id: 'role',
            header: t('refdata.fields.role'),
            cell: (row) => t(`refdata.choices.role.${show(row['role'])}`),
            sort: 'role',
        },
        {
            id: 'source_name',
            header: t('refdata.fields.source_name'),
            cell: (row) => show(row['source_name']),
            mono: true,
            sort: 'source_name',
        },
        {
            id: 'process_type',
            header: t('refdata.fields.process_type'),
            cell: (row) => engineName(t, show(row['process_type'])),
        },
        {
            id: 'ticks_per_hour',
            header: t('refdata.fields.ticks_per_hour'),
            cell: (row) => show(row['ticks_per_hour']),
            numeric: true,
            sort: 'ticks_per_hour',
        },
    ],
    extraDetails: (feed, t) => [[t('refdata.fields.oresmd_uri'), irSeriesUri(feed)]],
    parts: [
        {
            resource: PARAMETER_VALUES,
            entity: 'ores.synthetic.ir_curve_generation_config_process_parameter_value',
            specs: PARAMETER_FIELDS,
            keep: ['id', 'config_id', 'parameter_definition_id'],
        },
        {
            resource: TEMPLATE_ENTRIES,
            entity: 'ores.synthetic.ir_curve_template_entry',
            specs: ENTRY_FIELDS,
            keep: ['id', 'ir_curve_config_id'],
        },
    ],
    tabs: [
        {
            name: 'process',
            render: ({ feed, running, mayWrite, parts }) => (
                <IrProcessPanel
                    feed={feed}
                    running={running === true}
                    mayWrite={mayWrite}
                    saved={parts[0] ?? NO_ROWS}
                    entries={parts[1] ?? NO_ROWS}
                />
            ),
        },
        {
            name: 'template',
            render: ({ feed, parts }) => (
                <IrTemplatePanel feed={feed} entries={parts[1] ?? NO_ROWS} />
            ),
        },
        {
            name: 'ticks',
            render: ({ feed, running, parts }) => (
                <IrTicksTab
                    feed={feed}
                    running={running}
                    values={parts[0] ?? NO_ROWS}
                    entries={parts[1] ?? NO_ROWS}
                />
            ),
        },
    ],
    afterAdd: 'process',
};

export function irFeedPath(id?: string): string {
    return feedPath(IR_FEED_KIND, id);
}

/** The IR curve feeds of the party: one page at a time, searched and sorted on the server. */
export function IrFeedsPage(): ReactNode {
    return <FeedsPage kind={IR_FEED_KIND} />;
}

/** One curve feed: its fields, the engine and parameters, the curve template, and its history. */
export function IrFeedPage(): ReactNode {
    return <FeedPage kind={IR_FEED_KIND} />;
}

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

import { keepPreviousData, useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useEffect, useState, type ReactNode } from 'react';
import type { HistoryVersion } from '@ores/wire-protocol/browser';
import { api, type RecordRow } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Dialog, Field, Input, Notice, Select } from '../ui/Primitives.js';
import { HistoryPanel } from '../refdata/HistoryPanel.js';
import {
    NEW_RECORD_REASON,
    RemoveRecordDialog,
    RevertDialog,
    show,
    useRecordPermissions,
    useRecords,
    type FieldSpec,
} from '../refdata/records.js';
import { ReasonFields, useReason } from '../refdata/shared.js';
import { LineChart, SERIES_COLOURS } from './LineChart.js';
import {
    PROFILES,
    bandWidth,
    halfLifeMinutes,
    processTypeOf,
    profileOf,
    type Profile,
} from './processHelp.js';
import {
    densityOf,
    draftOf,
    gaussiansOf,
    newComponent,
    problemsOf,
    savePlan,
    weightSum,
    type DraftComponent,
} from './mixture.js';

export const COMPONENTS = 'gmm-components';

/** A component's fields, for its history and its revert; the mixture table edits the rest. */
const COMPONENT_FIELDS: readonly FieldSpec[] = [
    {
        field: 'component_index',
        history: 'Component Index',
        kind: { kind: 'int', min: 0 },
    },
    { field: 'description', history: 'Description', kind: { kind: 'text' }, blank: true },
    { field: 'mean', history: 'Mean', kind: { kind: 'decimal' } },
    { field: 'stdev', history: 'Stdev', kind: { kind: 'decimal' } },
    { field: 'weight', history: 'Weight', kind: { kind: 'decimal' } },
];

/** How many ticks and paths a preview draws, as the server bounds them. */
const MAX_TICKS = 5000;
const MAX_PATHS = 50;

/** How long the preview waits after the last keystroke before it asks the server. */
const PREVIEW_PAUSE_MS = 300;

function useSettled<Value>(value: Value): Value {
    const [settled, setSettled] = useState(value);
    useEffect(() => {
        const timer = setTimeout(() => setSettled(value), PREVIEW_PAUSE_MS);
        return () => clearTimeout(timer);
    }, [value]);
    return settled;
}

function whole(value: string, low: number, high: number): number | undefined {
    if (!/^[0-9]+$/.test(value.trim())) {
        return undefined;
    }
    const parsed = Number.parseInt(value, 10);
    return parsed < low || parsed > high ? undefined : parsed;
}

function figure(value: number): string {
    return Math.abs(value) >= 1000 || (value !== 0 && Math.abs(value) < 0.001)
        ? value.toExponential(2)
        : value.toFixed(4);
}

/**
 * A feed's mixture: the Gaussians its process draws from, edited as one set
 * beside what they imply. The person changes the table and reads the return
 * distribution and a batch of sample paths drawn from the values on the screen;
 * nothing reaches the feed until Save writes the set in one call.
 */
export function MixturePanel({ feed }: { readonly feed: RecordRow }): ReactNode {
    const { t } = useTranslation();
    const feedId = show(feed['id']);
    const rows = useRecords(COMPONENTS, feedId);
    if (rows.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (rows.isError) {
        return <Notice tone="error">{rows.error.message}</Notice>;
    }
    return <MixtureEditor key={rows.dataUpdatedAt} feed={feed} rows={rows.data} />;
}

function MixtureEditor({
    feed,
    rows,
}: {
    readonly feed: RecordRow;
    readonly rows: readonly RecordRow[];
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const may = useRecordPermissions(COMPONENTS);
    const feedId = show(feed['id']);
    const saved = draftOf(rows);
    const [draft, setDraft] = useState<readonly DraftComponent[]>(saved);
    const [commentary, setCommentary] = useState('');
    const [open, setOpen] = useState<{
        readonly row: RecordRow;
        readonly view: 'history' | 'remove';
    } | null>(null);
    const [reverting, setReverting] = useState<{
        readonly row: RecordRow;
        readonly version: HistoryVersion;
    } | null>(null);
    const plan = savePlan(feedId, saved, draft);
    const dirty = plan.changes.length > 0 || draft.length !== saved.length;
    const onlyNew = plan.changes.every((change) => change.version === null);
    const reason = useReason('amend', true);
    const missing = !onlyNew && reason.needsCommentary && commentary.trim() === '';
    const problems = problemsOf(draft);
    const gaussians = gaussiansOf(draft);
    const sum = gaussians === undefined ? undefined : weightSum(gaussians);
    const processType = processTypeOf(feed['process_type']);
    const ou = processType === 'ornstein_uhlenbeck';
    const columnHelp = (field: 'mean' | 'stdev' | 'weight'): string =>
        t(`marketdata.process.columns.${processType}.${field}`);

    const edit = (index: number, change: Partial<DraftComponent>): void =>
        setDraft(
            draft.map((component, i) => (i === index ? { ...component, ...change } : component)),
        );
    const move = (index: number, by: number): void => {
        const next = [...draft];
        const [moved] = next.splice(index, 1);
        if (moved !== undefined) {
            next.splice(index + by, 0, moved);
            setDraft(next);
        }
    };
    const rowOf = (id: string): RecordRow | undefined => rows.find((row) => show(row['id']) === id);

    const save = useMutation({
        mutationFn: () =>
            api.saveSynthetic(
                COMPONENTS,
                plan.changes,
                onlyNew
                    ? { reasonCode: NEW_RECORD_REASON, commentary: '' }
                    : { reasonCode: reason.code, commentary: commentary.trim() },
            ),
        onSuccess: async () => {
            setCommentary('');
            await queries.invalidateQueries({ queryKey: ['records', COMPONENTS] });
            await queries.invalidateQueries({ queryKey: ['history'] });
        },
    });

    return (
        <div className="grid gap-4 xl:grid-cols-2">
            <section className="space-y-3">
                <p className="text-sm text-ink-muted">{t('marketdata.mixture.lead')}</p>
                <Notice tone={processType === 'arithmetic' ? 'warn' : 'info'}>
                    {t(`marketdata.process.about.${processType}`)}
                </Notice>
                <div className="overflow-x-auto rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                <th className="px-3 py-2 font-medium">#</th>
                                <th className="px-3 py-2 font-medium">
                                    {t('refdata.fields.description')}
                                </th>
                                <th
                                    className="px-3 py-2 font-medium"
                                    title={t('marketdata.process.profileHelp')}
                                >
                                    {t('marketdata.process.profile')}
                                </th>
                                <th className="px-3 py-2 font-medium" title={columnHelp('mean')}>
                                    {t('refdata.fields.mean')}
                                </th>
                                <th className="px-3 py-2 font-medium" title={columnHelp('stdev')}>
                                    {t('refdata.fields.stdev')}
                                </th>
                                <th className="px-3 py-2 font-medium" title={columnHelp('weight')}>
                                    {ou
                                        ? t('marketdata.process.kappa')
                                        : t('refdata.fields.weight')}
                                </th>
                                <th className="px-3 py-2" />
                            </tr>
                        </thead>
                        <tbody>
                            {draft.length === 0 && (
                                <tr>
                                    <td colSpan={7} className="px-3 py-3 text-ink-muted">
                                        {t('marketdata.mixture.none')}
                                    </td>
                                </tr>
                            )}
                            {draft.map((component, index) => {
                                const row = rowOf(component.id);
                                return (
                                    <tr
                                        key={component.id}
                                        className="border-b border-line-subtle last:border-b-0"
                                        title={t(`marketdata.process.row.${processType}`)}
                                    >
                                        <td className="px-3 py-1.5 font-mono text-xs text-ink-muted">
                                            {index}
                                        </td>
                                        <td className="px-3 py-1.5">
                                            <Input
                                                aria-label={t('refdata.fields.description')}
                                                value={component.description}
                                                readOnly={!may.write}
                                                maxLength={2000}
                                                onChange={(event) =>
                                                    edit(index, { description: event.target.value })
                                                }
                                            />
                                        </td>
                                        <td className="px-3 py-1.5">
                                            <Select
                                                aria-label={t('marketdata.process.profile')}
                                                value={profileOf(component.stdev)}
                                                disabled={!may.write}
                                                onChange={(event) => {
                                                    const chosen = event.target.value;
                                                    if (chosen in PROFILES) {
                                                        edit(index, {
                                                            stdev: String(
                                                                PROFILES[chosen as Profile],
                                                            ),
                                                        });
                                                    }
                                                }}
                                            >
                                                {(
                                                    ['custom', ...Object.keys(PROFILES)] as const
                                                ).map((profile) => (
                                                    <option key={profile} value={profile}>
                                                        {t(
                                                            `marketdata.process.profiles.${profile}`,
                                                        )}
                                                    </option>
                                                ))}
                                            </Select>
                                        </td>
                                        {(['mean', 'stdev', 'weight'] as const).map((field) => (
                                            <td key={field} className="px-3 py-1.5">
                                                <Input
                                                    aria-label={t(`refdata.fields.${field}`)}
                                                    className="w-24 text-right font-mono"
                                                    inputMode="decimal"
                                                    value={component[field]}
                                                    readOnly={!may.write}
                                                    onChange={(event) =>
                                                        edit(index, { [field]: event.target.value })
                                                    }
                                                />
                                            </td>
                                        ))}
                                        <td className="px-3 py-1.5 text-right whitespace-nowrap">
                                            {may.write && index > 0 && (
                                                <Button
                                                    size="sm"
                                                    variant="ghost"
                                                    onClick={() => move(index, -1)}
                                                >
                                                    {t('marketdata.mixture.up')}
                                                </Button>
                                            )}
                                            {row !== undefined && (
                                                <Button
                                                    size="sm"
                                                    variant="ghost"
                                                    icon="history"
                                                    onClick={() =>
                                                        setOpen({ row, view: 'history' })
                                                    }
                                                >
                                                    {t('refdata.records.tabs.history')}
                                                </Button>
                                            )}
                                            {row === undefined && may.write && (
                                                <Button
                                                    size="sm"
                                                    variant="ghost"
                                                    icon="cancel"
                                                    onClick={() =>
                                                        setDraft(
                                                            draft.filter((_, i) => i !== index),
                                                        )
                                                    }
                                                >
                                                    {t('marketdata.mixture.discardRow')}
                                                </Button>
                                            )}
                                            {row !== undefined && may.remove && !dirty && (
                                                <Button
                                                    size="sm"
                                                    variant="ghost"
                                                    icon="delete"
                                                    onClick={() => setOpen({ row, view: 'remove' })}
                                                >
                                                    {t('refdata.records.delete')}
                                                </Button>
                                            )}
                                        </td>
                                    </tr>
                                );
                            })}
                        </tbody>
                    </table>
                </div>
                {may.write && (
                    <Button
                        icon="add"
                        onClick={() => setDraft([...draft, newComponent(crypto.randomUUID())])}
                    >
                        {t('marketdata.mixture.add')}
                    </Button>
                )}
                {ou ? (
                    <MeanReversion feed={feed} first={gaussians?.[0]} />
                ) : (
                    <p className="text-sm">
                        {sum === undefined
                            ? t('marketdata.mixture.sumPending')
                            : t('marketdata.mixture.sum', { sum: sum.toFixed(4) })}
                    </p>
                )}
                {problems.map((problem) => (
                    <Notice key={problem} tone="warn">
                        {t(`marketdata.mixture.problems.${problem}`)}
                    </Notice>
                ))}
                {dirty && may.write && (
                    <div className="space-y-3 rounded-md border border-line p-3">
                        <p className="text-sm text-ink-muted">
                            {t('marketdata.mixture.unsaved', {
                                count: String(plan.changes.length),
                            })}
                        </p>
                        {onlyNew ? (
                            <p className="text-xs text-ink-faint">
                                {t('refdata.classifications.newRecordNote')}
                            </p>
                        ) : (
                            <ReasonFields
                                reason={reason}
                                commentary={commentary}
                                onCommentary={setCommentary}
                                missing={missing}
                            />
                        )}
                        {save.isError && <Notice tone="error">{save.error.message}</Notice>}
                        <div className="flex gap-2">
                            <Button
                                variant="primary"
                                icon="save"
                                pending={save.isPending}
                                disabled={
                                    problems.length > 0 ||
                                    missing ||
                                    (!onlyNew && reason.code === '')
                                }
                                onClick={() => save.mutate()}
                            >
                                {t('marketdata.mixture.save')}
                            </Button>
                            <Button
                                variant="ghost"
                                icon="cancel"
                                onClick={() => {
                                    setDraft(saved);
                                    save.reset();
                                }}
                            >
                                {t('marketdata.mixture.discard')}
                            </Button>
                        </div>
                    </div>
                )}
                {dirty && saved.length > 0 && (
                    <p className="text-xs text-ink-faint">{t('marketdata.mixture.removeAfter')}</p>
                )}
            </section>
            <Preview feed={feed} draft={draft} />
            {open?.view === 'history' && (
                <Dialog
                    title={t('marketdata.mixture.componentTitle')}
                    onClose={() => setOpen(null)}
                    wide
                >
                    <HistoryPanel
                        entityType="ores.synthetic.gmm_component"
                        entityId={show(open.row['id'])}
                        {...(may.write
                            ? {
                                  onRevert: (version: HistoryVersion) => {
                                      setReverting({ row: open.row, version });
                                      setOpen(null);
                                  },
                              }
                            : {})}
                    />
                </Dialog>
            )}
            {open?.view === 'remove' && (
                <RemoveRecordDialog
                    resource={COMPONENTS}
                    title={t('marketdata.mixture.removeTitle')}
                    warning={t('refdata.records.deletePartWarning')}
                    recordKey={{ id: show(open.row['id']) }}
                    version={open.row.version}
                    onClose={() => setOpen(null)}
                    onRemoved={() => setOpen(null)}
                />
            )}
            {reverting !== null && (
                <RevertDialog
                    resource={COMPONENTS}
                    specs={COMPONENT_FIELDS}
                    row={reverting.row}
                    version={reverting.version}
                    keep={['id', 'fx_spot_config_id']}
                    onClose={() => setReverting(null)}
                />
            )}
        </div>
    );
}

/**
 * What an Ornstein-Uhlenbeck feed's numbers mean in time and in price: the
 * engine reads the first component only, its weight as κ and its deviation as
 * σ, and reverts toward the initial price.
 */
function MeanReversion({
    feed,
    first,
}: {
    readonly feed: RecordRow;
    readonly first: { readonly stdev: number; readonly weight: number } | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const minutes =
        first === undefined
            ? undefined
            : halfLifeMinutes(first.weight, Number(feed['ticks_per_hour']));
    const band = first === undefined ? undefined : bandWidth(first.weight, first.stdev);
    return (
        <div className="space-y-1 text-sm">
            <p>
                {minutes === undefined
                    ? t('marketdata.process.noReversion')
                    : t('marketdata.process.halfLife', {
                          minutes: minutes.toFixed(1),
                          days: (minutes / (24 * 60)).toFixed(2),
                      })}
            </p>
            {band !== undefined && (
                <p className="text-ink-muted">
                    {t('marketdata.process.band', { band: band.toPrecision(3) })}
                </p>
            )}
            <p className="text-xs text-ink-faint">{t('marketdata.process.firstOnly')}</p>
        </div>
    );
}

/**
 * What the mixture on the screen implies: its return distribution, drawn here,
 * and a batch of sample paths the server draws from the same values. Neither
 * reads the saved feed, so the person judges the values before saving them.
 */
function Preview({
    feed,
    draft,
}: {
    readonly feed: RecordRow;
    readonly draft: readonly DraftComponent[];
}): ReactNode {
    const { t } = useTranslation();
    const processType = show(feed['process_type']);
    const mixing = processType !== 'ornstein_uhlenbeck';
    const savedPrice = Number(feed['gmm_initial_price']);
    const [price, setPrice] = useState(savedPrice > 0 ? String(savedPrice) : '1');
    const [ticks, setTicks] = useState('250');
    const [paths, setPaths] = useState('5');
    const [seed, setSeed] = useState(42);
    const gaussians = gaussiansOf(draft);
    const drawable = gaussians !== undefined && problemsOf(draft).length === 0;
    const initialPrice = Number(price);
    const numTicks = whole(ticks, 2, MAX_TICKS);
    const numPaths = whole(paths, 1, MAX_PATHS);
    const request =
        drawable &&
        Number.isFinite(initialPrice) &&
        initialPrice > 0 &&
        numTicks !== undefined &&
        numPaths !== undefined
            ? {
                  gmm_means: gaussians.map((gaussian) => gaussian.mean),
                  gmm_stdevs: gaussians.map((gaussian) => gaussian.stdev),
                  gmm_weights: gaussians.map((gaussian) => gaussian.weight),
                  process_type: processType,
                  initial_price: initialPrice,
                  num_ticks: numTicks,
                  num_paths: numPaths,
                  seed,
              }
            : undefined;
    const settled = useSettled(request);
    const sample = useQuery({
        queryKey: ['simulate-fx', settled],
        queryFn: () => (settled === undefined ? [] : api.simulateFx(settled)),
        enabled: settled !== undefined,
        placeholderData: keepPreviousData,
        retry: false,
    });
    const density = drawable && mixing ? densityOf(gaussians) : undefined;

    return (
        <section className="space-y-4">
            <div className="space-y-2 rounded-md border border-line p-3">
                <h3 className="text-sm font-medium">{t('marketdata.preview.distribution')}</h3>
                {!mixing ? (
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.preview.meanReverting')}
                    </p>
                ) : density === undefined || density.xs.length === 0 ? (
                    <p className="text-sm text-ink-muted">{t('marketdata.preview.nothing')}</p>
                ) : (
                    <>
                        <LineChart
                            label={t('marketdata.preview.distribution')}
                            xs={density.xs}
                            format={figure}
                            xFormat={figure}
                            series={[
                                ...density.parts.map((points, i) => ({
                                    points,
                                    colour: SERIES_COLOURS[(i + 1) % SERIES_COLOURS.length] ?? '',
                                    dashed: true,
                                })),
                                {
                                    points: density.mixture,
                                    colour: 'var(--color-ink)',
                                    strong: true,
                                },
                            ]}
                        />
                        <p className="text-xs text-ink-faint">{t('marketdata.preview.density')}</p>
                    </>
                )}
            </div>
            <div className="space-y-3 rounded-md border border-line p-3">
                <h3 className="text-sm font-medium">{t('marketdata.preview.paths')}</h3>
                <div className="grid gap-3 sm:grid-cols-4">
                    <Field label={t('marketdata.preview.initialPrice')}>
                        <Input
                            inputMode="decimal"
                            value={price}
                            onChange={(event) => setPrice(event.target.value)}
                        />
                    </Field>
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
                {request === undefined ? (
                    <p className="text-sm text-ink-muted">{t('marketdata.preview.waiting')}</p>
                ) : sample.isError ? (
                    <Notice tone="error">{sample.error.message}</Notice>
                ) : sample.data === undefined || sample.data.length === 0 ? (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                ) : (
                    <LineChart
                        label={t('marketdata.preview.paths')}
                        format={figure}
                        xFormat={(tick) => String(tick)}
                        series={sample.data.map((points, i) => ({
                            points,
                            colour: SERIES_COLOURS[i % SERIES_COLOURS.length] ?? '',
                        }))}
                    />
                )}
                <p className="text-xs text-ink-faint">
                    {t('marketdata.preview.unsaved', {
                        process: t(`refdata.choices.process_type.${processType}`),
                    })}
                </p>
            </div>
        </section>
    );
}

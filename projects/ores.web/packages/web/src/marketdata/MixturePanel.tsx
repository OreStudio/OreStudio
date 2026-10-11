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
import { api, type RecordRow } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Field, Input, Notice, cx } from '../ui/Primitives.js';
import { NEW_RECORD_REASON, show, useRecordPermissions, useRecords } from '../refdata/records.js';
import { ReasonFields, useReason } from '../refdata/shared.js';
import { LineChart, SERIES_COLOURS } from './LineChart.js';
import { ComponentDialog } from './ComponentDialog.js';
import { bandWidth, halfLifeMinutes, processTypeOf, profileOf } from './processHelp.js';
import {
    densityOf,
    describedAs,
    draftOf,
    gaussiansOf,
    newComponent,
    problemsOf,
    savePlan,
    weightSum,
    type DraftComponent,
} from './mixture.js';

export const COMPONENTS = 'gmm-components';

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
        readonly component: DraftComponent;
        readonly index: number;
    } | null>(null);
    const plan = savePlan(feedId, saved, draft);
    const dirty = plan.changes.length > 0 || plan.removals.length > 0;
    const onlyNew =
        plan.removals.length === 0 && plan.changes.every((change) => change.version === null);
    const reason = useReason('amend', true);
    const missing = !onlyNew && reason.needsCommentary && commentary.trim() === '';
    const problems = problemsOf(draft);
    const gaussians = gaussiansOf(draft);
    const sum = gaussians === undefined ? undefined : weightSum(gaussians);
    const processType = processTypeOf(feed['process_type']);
    const ou = processType === 'ornstein_uhlenbeck';
    const columnHelp = (field: 'mean' | 'stdev' | 'weight'): string =>
        t(`marketdata.process.columns.${processType}.${field}`);

    const move = (index: number, by: number): void => {
        const next = [...draft];
        const [moved] = next.splice(index, 1);
        if (moved !== undefined) {
            next.splice(index + by, 0, moved);
            setDraft(next);
        }
    };
    const done = (component: DraftComponent): void => {
        setDraft(
            open !== null && open.index < draft.length
                ? draft.map((one, i) => (i === open.index ? component : one))
                : [...draft, component],
        );
        setOpen(null);
    };

    const save = useMutation({
        mutationFn: async () => {
            const intent = onlyNew
                ? { reasonCode: NEW_RECORD_REASON, commentary: '' }
                : { reasonCode: reason.code, commentary: commentary.trim() };
            if (plan.removals.length > 0) {
                await api.removeSynthetic(
                    COMPONENTS,
                    plan.removals.map((removal) => ({
                        key: removal.key,
                        version: plan.removals.length === 1 ? removal.version : null,
                    })),
                    intent,
                );
            }
            if (plan.changes.length > 0) {
                await api.saveSynthetic(COMPONENTS, plan.changes, intent);
            }
        },
        onSuccess: async () => {
            setCommentary('');
            await queries.invalidateQueries({ queryKey: ['records', COMPONENTS] });
            await queries.invalidateQueries({ queryKey: ['history'] });
        },
    });

    return (
        <div className="space-y-4">
            <section className="space-y-3">
                <p className="text-sm text-ink-muted">{t('marketdata.mixture.lead')}</p>
                <Notice tone={processType === 'arithmetic' ? 'warn' : 'info'}>
                    {t(`marketdata.process.about.${processType}`)}
                </Notice>
                <div className="overflow-x-auto rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                <th className="px-4 py-2 font-medium">#</th>
                                <th className="px-4 py-2 font-medium">
                                    {t('refdata.fields.description')}
                                </th>
                                <th
                                    className="px-4 py-2 font-medium"
                                    title={t('marketdata.process.profileHelp')}
                                >
                                    {t('marketdata.process.profile')}
                                </th>
                                <th
                                    className="px-4 py-2 text-right font-medium"
                                    title={columnHelp('mean')}
                                >
                                    {t('refdata.fields.mean')}
                                </th>
                                <th
                                    className="px-4 py-2 text-right font-medium"
                                    title={columnHelp('stdev')}
                                >
                                    {t('refdata.fields.stdev')}
                                </th>
                                <th
                                    className="px-4 py-2 text-right font-medium"
                                    title={columnHelp('weight')}
                                >
                                    {ou
                                        ? t('marketdata.process.kappa')
                                        : t('refdata.fields.weight')}
                                </th>
                                {may.write && <th className="px-4 py-2" />}
                            </tr>
                        </thead>
                        <tbody>
                            {draft.length === 0 && (
                                <tr>
                                    <td colSpan={7} className="px-4 py-3 text-ink-muted">
                                        {t('marketdata.mixture.none')}
                                    </td>
                                </tr>
                            )}
                            {draft.map((component, index) => {
                                const { name, note } = describedAs(component.description);
                                return (
                                    <tr
                                        key={component.id}
                                        className={cx(
                                            'border-b border-line-subtle last:border-b-0',
                                            may.write && 'cursor-pointer hover:bg-surface-hover',
                                        )}
                                        title={t(`marketdata.process.row.${processType}`)}
                                        {...(may.write
                                            ? { onClick: () => setOpen({ component, index }) }
                                            : {})}
                                    >
                                        <td className="px-4 py-2 font-mono text-xs text-ink-muted">
                                            {index}
                                        </td>
                                        <td className="max-w-md px-4 py-2">
                                            <div className="truncate" title={component.description}>
                                                <span className="font-medium text-ink">
                                                    {name === ''
                                                        ? t('marketdata.ticks.unnamed')
                                                        : name}
                                                </span>
                                                {note !== '' && (
                                                    <span className="text-ink-faint">
                                                        {' '}
                                                        · {note}
                                                    </span>
                                                )}
                                            </div>
                                        </td>
                                        <td className="px-4 py-2 text-ink-muted">
                                            {t(
                                                `marketdata.process.profiles.${profileOf(component.stdev)}`,
                                            )}
                                        </td>
                                        {(['mean', 'stdev', 'weight'] as const).map((field) => (
                                            <td
                                                key={field}
                                                className="px-4 py-2 text-right font-mono text-xs"
                                            >
                                                {component[field]}
                                            </td>
                                        ))}
                                        {may.write && (
                                            <td
                                                className="px-4 py-2 text-right whitespace-nowrap"
                                                onClick={(event) => event.stopPropagation()}
                                            >
                                                <Button
                                                    size="sm"
                                                    variant="ghost"
                                                    disabled={index === 0}
                                                    aria-label={t('marketdata.mixture.upRow', {
                                                        index: String(index),
                                                    })}
                                                    onClick={() => move(index, -1)}
                                                >
                                                    ↑
                                                </Button>
                                                <Button
                                                    size="sm"
                                                    variant="ghost"
                                                    disabled={index === draft.length - 1}
                                                    aria-label={t('marketdata.mixture.downRow', {
                                                        index: String(index),
                                                    })}
                                                    onClick={() => move(index, 1)}
                                                >
                                                    ↓
                                                </Button>
                                                <Button
                                                    size="sm"
                                                    variant="ghost"
                                                    icon="delete"
                                                    onClick={() =>
                                                        setDraft(
                                                            draft.filter((_, i) => i !== index),
                                                        )
                                                    }
                                                >
                                                    {t('refdata.records.delete')}
                                                </Button>
                                            </td>
                                        )}
                                    </tr>
                                );
                            })}
                        </tbody>
                    </table>
                </div>
                {may.write && (
                    <Button
                        icon="add"
                        onClick={() =>
                            setOpen({
                                component: newComponent(crypto.randomUUID()),
                                index: draft.length,
                            })
                        }
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
                                removed: String(plan.removals.length),
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
            </section>
            <Preview feed={feed} draft={draft} />
            {open !== null && (
                <ComponentDialog
                    component={open.component}
                    index={open.index}
                    adding={open.index >= draft.length}
                    processType={processType}
                    onDone={done}
                    onClose={() => setOpen(null)}
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
        <section className="grid items-start gap-4 xl:grid-cols-2">
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
                <p className="text-xs text-ink-faint">
                    {t('marketdata.preview.unsaved', {
                        process: t(`refdata.choices.process_type.${processType}`),
                    })}
                </p>
            </div>
        </section>
    );
}

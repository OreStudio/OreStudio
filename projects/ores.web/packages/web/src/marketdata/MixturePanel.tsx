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

import { keepPreviousData, useMutation, useQuery } from '@tanstack/react-query';
import { useEffect, useRef, useState, type ReactNode } from 'react';
import { api, type RecordRow } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { EditingModeToggle, useEditingMode } from '../ui/EditingMode.js';
import { Button, Field, Input, Notice, Select, Tag, cx } from '../ui/Primitives.js';
import { NEW_RECORD_REASON, show, useRecordPermissions, useRecords } from '../refdata/records.js';
import { LineChart, SERIES_COLOURS } from './LineChart.js';
import { ComponentDialog } from './ComponentDialog.js';
import {
    FX_SIMPLE_START,
    FxSimplePanel,
    useFxReading,
    type FxSimpleView,
} from './FxSimplePanel.js';
import { SavePartsDialog, type SaveEngine } from './SavePartsDialog.js';
import {
    bandWidth,
    halfLifeMinutes,
    processTypeOf,
    profileOf,
    type ProcessType,
} from './processHelp.js';
import {
    changesOf,
    densityOf,
    describedAs,
    draftFromMixture,
    draftOf,
    gaussiansOf,
    newComponent,
    problemsOf,
    removedFrom,
    savePlan,
    sharesOf,
    weightSum,
    type ComponentValue,
    type DraftComponent,
} from './mixture.js';

export const COMPONENTS = 'gmm-components';

/** How many ticks and paths a preview draws, as the server bounds them. */
const MAX_TICKS = 5000;
const MAX_PATHS = 50;

/** How long the preview waits after the last keystroke before it asks the server. */
const PREVIEW_PAUSE_MS = 300;

export function useSettled<Value>(value: Value): Value {
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
export function MixturePanel({
    feed,
    running,
    mayWriteFeed,
    saveEngine,
}: {
    readonly feed: RecordRow;
    readonly running: boolean;
    readonly mayWriteFeed: boolean;
    readonly saveEngine: SaveEngine;
}): ReactNode {
    const { t } = useTranslation();
    const feedId = show(feed['id']);
    const rows = useRecords(COMPONENTS, feedId);
    if (rows.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (rows.isError) {
        return <Notice tone="error">{rows.error.message}</Notice>;
    }
    return (
        <MixtureEditor
            key={`${String(rows.dataUpdatedAt)}-${String(feed.version)}`}
            feed={feed}
            rows={rows.data}
            running={running}
            mayWriteFeed={mayWriteFeed}
            saveEngine={saveEngine}
        />
    );
}

/** A converted number as a person would type it, without the float's trailing noise. */
function typed(value: number | undefined): string {
    return String(Number((value ?? 0).toPrecision(10)));
}

/** A cell the draft changed is tinted, so what is not saved yet is seen at a glance. */
const CHANGED_CELL = 'bg-warn/10 text-warn';

function MixtureEditor({
    feed,
    rows,
    running,
    mayWriteFeed,
    saveEngine,
}: {
    readonly feed: RecordRow;
    readonly rows: readonly RecordRow[];
    readonly running: boolean;
    readonly mayWriteFeed: boolean;
    readonly saveEngine: SaveEngine;
}): ReactNode {
    const { t } = useTranslation();
    const may = useRecordPermissions(COMPONENTS);
    const feedId = show(feed['id']);
    const saved = draftOf(rows);
    const [draft, setDraft] = useState<readonly DraftComponent[]>(saved);
    const [open, setOpen] = useState<{
        readonly component: DraftComponent;
        readonly index: number;
    } | null>(null);
    const [saving, setSaving] = useState(false);
    const savedEngine = processTypeOf(feed['process_type']);
    const [engine, setEngine] = useState<ProcessType>(savedEngine);
    const savedPrice = Number(feed['gmm_initial_price']);
    const fixedPrice = feed['price_source'] === 'fixed' && savedPrice > 0;
    const [previewPrice, setPreviewPrice] = useState(savedPrice > 0 ? String(savedPrice) : '1');
    const [converted, setConverted] = useState<{
        readonly from: ProcessType;
        readonly to: ProcessType;
        readonly price: number;
    } | null>(null);
    const conversion = useMutation({
        mutationFn: async (to: ProcessType) => {
            const numbers = gaussiansOf(draft);
            if (numbers === undefined) {
                throw new Error(t('marketdata.mixture.problems.number'));
            }
            const price = fixedPrice ? savedPrice : Number(previewPrice);
            const out = await api.convertFxMixture({
                gmm_means: numbers.map((one) => one.mean),
                gmm_stdevs: numbers.map((one) => one.stdev),
                gmm_weights: numbers.map((one) => one.weight),
                from_process_type: engine,
                to_process_type: to,
                initial_price: price,
                ticks_per_hour: Number(feed['ticks_per_hour']),
            });
            return { to, price, out };
        },
        onSuccess: ({ to, price, out }) => {
            setDraft(
                draft.slice(0, out.gmm_means.length).map((component, index) => ({
                    ...component,
                    mean: typed(out.gmm_means[index]),
                    stdev: typed(out.gmm_stdevs[index]),
                    weight: typed(out.gmm_weights[index]),
                })),
            );
            setConverted({ from: engine, to, price });
            setEngine(to);
        },
    });
    const [mode, setMode] = useEditingMode();
    const [refusal, setRefusal] = useState('');
    const [explained, setExplained] = useState('');
    const ticksPerHour = Number(feed['ticks_per_hour']);
    const price = fixedPrice ? savedPrice : Number(previewPrice);
    const plan = savePlan(feedId, saved, draft);
    const changes = changesOf(saved, draft);
    const removed = removedFrom(saved, draft);
    const counts = {
        added: changes.filter((change) => change.added).length,
        changed: changes.filter(
            (change) => !change.added && (change.moved || change.fields.size > 0),
        ).length,
        removed: removed.length,
    };
    const engineChanged = engine !== savedEngine;
    const dirty = engineChanged || counts.added + counts.changed + counts.removed > 0;
    const problems = problemsOf(draft);
    const gaussians = gaussiansOf(draft);
    const sum = gaussians === undefined ? undefined : weightSum(gaussians);
    const shares = sharesOf(draft);
    const processType = engine;
    const ou = processType === 'ornstein_uhlenbeck';
    const columnHelp = (field: 'mean' | 'stdev' | 'weight'): string =>
        t(`marketdata.process.columns.${processType}.${field}`);
    const columns = ou ? 7 : 8;
    const reading = useFxReading(engine, gaussians, price, ticksPerHour);
    const savedReading = useFxReading(
        savedEngine,
        engineChanged ? undefined : gaussiansOf(saved),
        price,
        ticksPerHour,
    );
    const panelView: FxSimpleView | undefined =
        mode !== 'simple'
            ? undefined
            : draft.length === 0
              ? FX_SIMPLE_START
              : reading.exact
                ? reading.view
                : undefined;

    const applySimple = async (nextView: FxSimpleView, explain = false): Promise<void> => {
        try {
            const out = await api.convertFxSimple({
                process_type: engine,
                to_simple: false,
                initial_price: price,
                ticks_per_hour: ticksPerHour,
                ...nextView,
            });
            const next = draftFromMixture(
                draft,
                { means: out.gmm_means, stdevs: out.gmm_stdevs, weights: out.gmm_weights },
                ou,
                () => crypto.randomUUID(),
            );
            setDraft(next);
            setRefusal('');
            setExplained(
                explain
                    ? ou
                        ? t('marketdata.fxSimple.switched.reverting', {
                              removed: String(draft.length - next.length),
                          })
                        : t(`marketdata.fxSimple.switched.${next.length > 1 ? 'jumps' : 'none'}`, {
                              from: String(draft.length),
                              to: String(next.length),
                              jump: String(Number(nextView.jump_share_pct.toPrecision(3))),
                          })
                    : '',
            );
        } catch (error) {
            setRefusal(error instanceof Error ? error.message : String(error));
        }
    };

    // A mixture that only approximates the simple view is converted once, and
    // the person is told what changed. The draft is unsaved, so Discard undoes it.
    const tried = useRef('');
    useEffect(() => {
        const key = JSON.stringify([engine, price, gaussians]);
        if (
            mode !== 'simple' ||
            draft.length === 0 ||
            reading.stale ||
            reading.exact ||
            reading.view === undefined ||
            tried.current === key
        ) {
            return;
        }
        tried.current = key;
        void applySimple(reading.view, true);
    });

    const move = (index: number, by: number): void => {
        const next = [...draft];
        const [moved] = next.splice(index, 1);
        if (moved !== undefined) {
            next.splice(index + by, 0, moved);
            setDraft(next);
        }
    };
    const restore = (component: DraftComponent): void => {
        const at = saved.findIndex((one) => one.id === component.id);
        const next = [...draft];
        next.splice(Math.min(at, next.length), 0, component);
        setDraft(next);
    };
    const done = (component: DraftComponent): void => {
        setDraft(
            open !== null && open.index < draft.length
                ? draft.map((one, i) => (i === open.index ? component : one))
                : [...draft, component],
        );
        setOpen(null);
    };

    return (
        <div className="space-y-4">
            <section className="space-y-3">
                <p className="text-sm text-ink-muted">{t('marketdata.mixture.lead')}</p>
                <div className="flex flex-wrap items-center gap-2">
                    {dirty ? (
                        <Tag tone="warn">
                            {t('marketdata.mixture.unsaved', {
                                count: String(
                                    counts.added +
                                        counts.changed +
                                        counts.removed +
                                        (engineChanged ? 1 : 0),
                                ),
                            })}
                        </Tag>
                    ) : (
                        <span className="text-sm text-ink-faint">
                            {t('marketdata.mixture.clean')}
                        </span>
                    )}
                    <div className="ml-auto flex flex-wrap items-center gap-2">
                        <EditingModeToggle mode={mode} onChange={setMode} />
                        <label className="flex items-center gap-2 text-sm">
                            <span className="text-ink-muted">
                                {t('refdata.fields.process_type')}
                            </span>
                            <Select
                                className={engineChanged ? 'border-warn' : ''}
                                value={engine}
                                disabled={!mayWriteFeed || !may.write || conversion.isPending}
                                title={t(`marketdata.process.about.${engine}`)}
                                onChange={(event) =>
                                    conversion.mutate(processTypeOf(event.target.value))
                                }
                            >
                                {(['geometric', 'arithmetic', 'ornstein_uhlenbeck'] as const).map(
                                    (one) => (
                                        <option key={one} value={one}>
                                            {t(`refdata.choices.process_type.${one}`)}
                                        </option>
                                    ),
                                )}
                            </Select>
                        </label>
                    </div>
                    {may.write && (
                        <div className="flex gap-2">
                            {panelView === undefined && (
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
                            <Button
                                variant="ghost"
                                icon="cancel"
                                disabled={!dirty}
                                onClick={() => {
                                    setDraft(saved);
                                    setEngine(savedEngine);
                                    setConverted(null);
                                    setExplained('');
                                }}
                            >
                                {t('marketdata.mixture.discard')}
                            </Button>
                            <Button
                                variant="primary"
                                icon="save"
                                disabled={!dirty || problems.length > 0}
                                onClick={() => setSaving(true)}
                            >
                                {t('marketdata.mixture.saveButton')}
                            </Button>
                        </div>
                    )}
                </div>
                {conversion.isError && <Notice tone="error">{conversion.error.message}</Notice>}
                {converted !== null && engineChanged && (
                    <Notice tone="info">
                        {t('marketdata.mixture.converted', {
                            from: t(`refdata.choices.process_type.${converted.from}`),
                            to: t(`refdata.choices.process_type.${converted.to}`),
                            price: String(converted.price),
                        })}
                        {!fixedPrice && ` ${t('marketdata.mixture.convertedAtPreview')}`}
                    </Notice>
                )}
                {refusal !== '' && <Notice tone="warn">{refusal}</Notice>}
                {explained !== '' && mode === 'simple' && <Notice tone="info">{explained}</Notice>}
                {panelView !== undefined && draft.length === 0 && (
                    <p className="text-sm text-ink-muted">{t('marketdata.mixture.none')}</p>
                )}
                {panelView !== undefined ? (
                    <FxSimplePanel
                        view={panelView}
                        saved={engineChanged ? undefined : savedReading.view}
                        engine={engine}
                        price={price}
                        mayWrite={may.write && mayWriteFeed}
                        onChange={(next) => void applySimple(next)}
                    />
                ) : (
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
                                    {!ou && (
                                        <th
                                            className="px-4 py-2 text-right font-medium"
                                            title={
                                                sum === undefined
                                                    ? t('marketdata.mixture.sumPending')
                                                    : t('marketdata.mixture.sum', {
                                                          sum: sum.toFixed(4),
                                                      })
                                            }
                                        >
                                            {t('marketdata.mixture.share')}
                                        </th>
                                    )}
                                    <th className="px-4 py-2" />
                                </tr>
                            </thead>
                            <tbody>
                                {draft.length === 0 && removed.length === 0 && (
                                    <tr>
                                        <td colSpan={columns} className="px-4 py-3 text-ink-muted">
                                            {t('marketdata.mixture.none')}
                                        </td>
                                    </tr>
                                )}
                                {draft.map((component, index) => {
                                    const { name, note } = describedAs(component.description);
                                    const change = changes[index];
                                    const changed = (field: ComponentValue): string =>
                                        change?.fields.has(field) === true ? CHANGED_CELL : '';
                                    const share = shares[index];
                                    return (
                                        <tr
                                            key={component.id}
                                            className={cx(
                                                'border-b border-line-subtle last:border-b-0',
                                                may.write &&
                                                    'cursor-pointer hover:bg-surface-hover',
                                                change?.added === true && 'bg-up/5',
                                            )}
                                            title={t(`marketdata.process.row.${processType}`)}
                                            {...(may.write
                                                ? { onClick: () => setOpen({ component, index }) }
                                                : {})}
                                        >
                                            <td className="px-4 py-2 font-mono text-xs text-ink-muted">
                                                {index}
                                            </td>
                                            <td
                                                className={cx(
                                                    'max-w-md px-4 py-2',
                                                    changed('description'),
                                                )}
                                            >
                                                <div
                                                    className="truncate"
                                                    title={component.description}
                                                >
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
                                                    className={cx(
                                                        'px-4 py-2 text-right font-mono text-xs',
                                                        changed(field),
                                                    )}
                                                >
                                                    {component[field]}
                                                </td>
                                            ))}
                                            {!ou && (
                                                <td className="px-4 py-2 text-right font-mono text-xs text-ink-muted">
                                                    {share === undefined
                                                        ? '—'
                                                        : `${(share * 100).toFixed(1)}%`}
                                                </td>
                                            )}
                                            <td
                                                className="px-4 py-2 text-right whitespace-nowrap"
                                                onClick={(event) => event.stopPropagation()}
                                            >
                                                {change?.added === true && (
                                                    <Tag tone="up" small>
                                                        {t('marketdata.mixture.marks.added')}
                                                    </Tag>
                                                )}
                                                {change?.added !== true &&
                                                    (change?.fields.size ?? 0) > 0 && (
                                                        <Tag tone="warn" small>
                                                            {t('marketdata.mixture.marks.changed')}
                                                        </Tag>
                                                    )}
                                                {change?.moved === true && (
                                                    <Tag tone="warn" small>
                                                        {t('marketdata.mixture.marks.moved')}
                                                    </Tag>
                                                )}
                                                {may.write && (
                                                    <>
                                                        <Button
                                                            size="sm"
                                                            variant="ghost"
                                                            disabled={index === 0}
                                                            aria-label={t(
                                                                'marketdata.mixture.upRow',
                                                                {
                                                                    index: String(index),
                                                                },
                                                            )}
                                                            onClick={() => move(index, -1)}
                                                        >
                                                            ↑
                                                        </Button>
                                                        <Button
                                                            size="sm"
                                                            variant="ghost"
                                                            disabled={index === draft.length - 1}
                                                            aria-label={t(
                                                                'marketdata.mixture.downRow',
                                                                { index: String(index) },
                                                            )}
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
                                                                    draft.filter(
                                                                        (_, i) => i !== index,
                                                                    ),
                                                                )
                                                            }
                                                        >
                                                            {t('refdata.records.delete')}
                                                        </Button>
                                                    </>
                                                )}
                                            </td>
                                        </tr>
                                    );
                                })}
                                {removed.map((component) => (
                                    <tr
                                        key={component.id}
                                        className="border-b border-line-subtle bg-down/5 text-ink-faint last:border-b-0"
                                    >
                                        <td className="px-4 py-2" />
                                        <td className="max-w-md px-4 py-2">
                                            <div className="truncate line-through">
                                                {component.description === ''
                                                    ? t('marketdata.ticks.unnamed')
                                                    : component.description}
                                            </div>
                                        </td>
                                        <td className="px-4 py-2" />
                                        {(['mean', 'stdev', 'weight'] as const).map((field) => (
                                            <td
                                                key={field}
                                                className="px-4 py-2 text-right font-mono text-xs line-through"
                                            >
                                                {component[field]}
                                            </td>
                                        ))}
                                        {!ou && <td className="px-4 py-2" />}
                                        <td className="px-4 py-2 text-right whitespace-nowrap">
                                            <Tag tone="down" small>
                                                {t('marketdata.mixture.marks.deleted')}
                                            </Tag>
                                            <Button
                                                size="sm"
                                                variant="ghost"
                                                icon="revert"
                                                onClick={() => restore(component)}
                                            >
                                                {t('marketdata.mixture.undo')}
                                            </Button>
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </div>
                )}
                {panelView === undefined && ou && (
                    <MeanReversion feed={feed} first={gaussians?.[0]} />
                )}
                {problems.map((problem) => (
                    <Notice key={problem} tone="warn">
                        {t(`marketdata.mixture.problems.${problem}`)}
                    </Notice>
                ))}
            </section>
            <Preview
                feed={feed}
                draft={draft}
                processType={engine}
                price={previewPrice}
                onPrice={setPreviewPrice}
            />
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
            {saving && (
                <SavePartsDialog
                    feedId={feedId}
                    resource={COMPONENTS}
                    words="mixture"
                    engineLabel={(code) => t(`refdata.choices.process_type.${code}`)}
                    plan={plan}
                    counts={counts}
                    engine={engineChanged ? { from: savedEngine, to: engine } : undefined}
                    saveEngine={saveEngine}
                    running={running}
                    onClose={() => setSaving(false)}
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
    draft,
    processType,
    price,
    onPrice,
}: {
    readonly feed: RecordRow;
    readonly draft: readonly DraftComponent[];
    readonly processType: ProcessType;
    readonly price: string;
    readonly onPrice: (price: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const mixing = processType !== 'ornstein_uhlenbeck';
    const setPrice = onPrice;
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
        <section className="space-y-3">
            <div className="grid items-start gap-4 xl:grid-cols-2">
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
                                        colour:
                                            SERIES_COLOURS[(i + 1) % SERIES_COLOURS.length] ?? '',
                                        dashed: true,
                                    })),
                                    {
                                        points: density.mixture,
                                        colour: 'var(--color-ink)',
                                        strong: true,
                                    },
                                ]}
                            />
                            <p className="text-xs text-ink-faint">
                                {t('marketdata.preview.density')}
                            </p>
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
            </div>
            {processType === 'arithmetic' && (
                <Notice tone="warn">{t(`marketdata.process.about.${processType}`)}</Notice>
            )}
        </section>
    );
}

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

/**
 * One store the installation keeps, in the two questions a person asks of it.
 *
 * The Details tab answers what the store is and how it moves: its catalogue row
 * and the plot of its size over a horizon the person picks. The Bound tab answers
 * what governs it: the mechanism that removes from it, the last run, and the form
 * that changes the period.
 *
 * The tabs exist because the two questions are asked at different times and the
 * plot needs the width the form was taking. The prototype
 * (doc/prototypes/operations/housekeeping/index.html?variant=A) argued the store's
 * page as one scroll; a plot with axes and a horizon does not fit one.
 */

import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link, useParams } from 'react-router';
import { STORE_RANGES, type StoreBound, type StoreRange } from '@ores/wire-protocol/browser';
import { covers, grantedBy } from '../access/catalogue.js';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { ReasonFields, useReason } from '../refdata/shared.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { PATHS, housekeepingStorePath } from '../shell/paths.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { Button, Detail, Field, Input, Notice, Select, Tag } from '../ui/Primitives.js';
import { PresetBar } from '../ui/Presets.js';
import { RelativeTime, formatDateTime } from '../ui/Time.js';
import { TimeChart } from '../ui/TimeChart.js';
import { toast } from '../ui/Toasts.js';
import { useTabs } from '../ui/Tabs.js';
import { GapPanel, type ScreenGap } from './OperationsParts.js';
import { actionLabel, targetKindName, useHousekeepingStores } from './HousekeepingPage.js';
import {
    RANGE_WORD,
    formatSize,
    measureUnit,
    mechanismOf,
    sizeSeries,
    trendOf,
    verdictOf,
} from './housekeeping.js';

/** The two questions a store answers, in the order they are asked. */
const STORE_TABS = ['details', 'bound'] as const;

/** What the page does not show, and the journey that records why. */
function gapsOf(t: (key: string) => string): readonly ScreenGap[] {
    return [
        {
            title: t('operations.housekeeping.gaps.runsTitle'),
            body: t('operations.housekeeping.gaps.runsBody'),
            journey: 'F6622109-4F52-4DE9-9C06-6B0EAA7C8D2E',
        },
        {
            title: t('operations.housekeeping.gaps.remoteTitle'),
            body: t('operations.housekeeping.gaps.remoteBody'),
            journey: 'F7C1E75E-9F08-437B-80C0-7D50B945CA52',
        },
    ];
}

/** Whether the person may change a bound. The server checks the write again. */
function useMayChangeBound(): boolean {
    const access = useQuery({ queryKey: ['my-access'], queryFn: api.myAccess });
    const granted = grantedBy(access.data?.roles ?? []);
    return covers(granted, 'scheduler::retention_policies:write');
}

/** The verdict of one store, in the colour the list paints it. */
function BoundVerdict({ store }: { readonly store: StoreBound }): ReactNode {
    const { t } = useTranslation();
    const verdict = verdictOf(store);
    return (
        <div className="space-y-1">
            <Tag tone={verdict.verdict === 'unbounded' ? 'down' : 'up'}>
                {t(`operations.housekeeping.verdict.${verdict.verdict}`)}
            </Tag>
            {verdict.reason !== null && (
                <p className="text-sm text-ink-muted">
                    {t(`operations.housekeeping.reason.${verdict.reason}`)}
                </p>
            )}
        </div>
    );
}

/** The store's own record: the catalogue row, as an entity's page states one. */
function StoreRecord({ store }: { readonly store: StoreBound }): ReactNode {
    const { t } = useTranslation();
    const unit = measureUnit(store.measure_kind);
    return (
        <section className="card space-y-4 p-4">
            <h2 className="text-sm font-medium">{t('operations.housekeeping.store.record')}</h2>
            <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-3">
                <Detail label={t('operations.housekeeping.store.fields.code')} value={store.code} />
                <Detail
                    label={t('operations.housekeeping.store.fields.owner')}
                    value={store.owner_component}
                />
                <Detail
                    label={t('operations.housekeeping.store.fields.targetKind')}
                    value={targetKindName(t, store.target_kind)}
                />
                <Detail
                    label={t('operations.housekeeping.store.fields.target')}
                    value={store.target}
                    mono
                />
                <Detail
                    label={t('operations.housekeeping.store.fields.timeColumn')}
                    value={store.time_column ?? t('operations.housekeeping.store.fields.none')}
                    mono
                />
                <Detail
                    label={t('operations.housekeeping.store.fields.measures')}
                    value={t(`operations.housekeeping.units.${unit}`)}
                />
            </dl>
            <p className="text-sm text-ink-muted">{store.description}</p>
        </section>
    );
}

/**
 * What the store holds now, and how it moved over the horizon the person picked.
 *
 * The horizon is asked of the read rather than taken from the clock, so the axis
 * states the window the installation covered. A store that has never been
 * measured says so; one whose horizon holds no measurement says that instead, and
 * names when it was last measured, because "no measurements" and "none in the
 * last day" are different facts.
 */
function StoreSize({ store }: { readonly store: StoreBound }): ReactNode {
    const { t, language } = useTranslation();
    const [range, setRange] = useState<StoreRange>('7d');
    const samples = useQuery({
        queryKey: ['operations', 'housekeeping', 'samples', store.code, range],
        queryFn: () => api.housekeepingSamples(store.code, range),
    });
    const unit = measureUnit(store.measure_kind);
    const points = sizeSeries(samples.data?.samples ?? []);
    const trend = trendOf(samples.data?.samples ?? []);
    const from = samples.data?.from_at ?? null;
    const to = samples.data?.to_at ?? null;

    return (
        <section className="card space-y-4 p-4">
            <header className="flex flex-wrap items-baseline justify-between gap-3">
                <div className="space-y-1">
                    <h2 className="text-sm font-medium">
                        {t('operations.housekeeping.size.title')}
                    </h2>
                    <p className="text-xs text-ink-faint">
                        {t('operations.housekeeping.size.lead')}
                    </p>
                </div>
                <PresetBar
                    label={t('operations.housekeeping.size.rangeLabel')}
                    presets={STORE_RANGES}
                    value={range}
                    labelOf={(preset) =>
                        t(`operations.housekeeping.size.ranges.${RANGE_WORD[preset]}`)
                    }
                    onChange={setRange}
                />
            </header>
            {store.size.sampled_at === null ? (
                <Notice tone="warn">{t('operations.housekeeping.size.noSample')}</Notice>
            ) : (
                <>
                    <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-3">
                        <Detail
                            label={t('operations.housekeeping.size.latest')}
                            value={formatSize(store.size.size_bytes)}
                        />
                        <Detail
                            label={t('operations.housekeeping.size.count')}
                            value={t('operations.housekeeping.measure', {
                                count: store.size.measure_count.toLocaleString(),
                                unit: t(`operations.housekeeping.units.${unit}`),
                            })}
                        />
                        <Detail
                            label={t('operations.housekeeping.size.measuredAt')}
                            value={formatDateTime(store.size.sampled_at, language)}
                        />
                    </dl>
                    {samples.isError && <Notice tone="error">{samples.error.message}</Notice>}
                    {samples.isPending && (
                        <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                    )}
                    {from !== null && to !== null && points.length > 1 && (
                        <TimeChart
                            window={{ from, to }}
                            points={points}
                            format={formatSize}
                            caption={t('operations.housekeeping.size.plotCaption', {
                                name: store.display_name,
                                size: formatSize(store.size.size_bytes),
                            })}
                        />
                    )}
                    {points.length === 1 && (
                        <p className="text-sm text-ink-muted">
                            {t('operations.housekeeping.size.onePoint')}
                        </p>
                    )}
                    {samples.data !== undefined && points.length === 0 && (
                        <p className="text-sm text-ink-muted">
                            {t('operations.housekeeping.size.noneInWindow')}
                        </p>
                    )}
                    <p className="text-xs text-ink-faint">
                        {t('operations.housekeeping.size.plot')}
                    </p>
                    {trend !== null && (
                        <p className="text-sm text-ink-muted">
                            {t('operations.housekeeping.size.trend', {
                                from: formatSize(trend.from),
                                to: formatSize(trend.to),
                                days: trend.days.toFixed(1),
                            })}
                            {trend.perWeek !== null && (
                                <>
                                    {' · '}
                                    {t('operations.housekeeping.size.perWeek', {
                                        rate: formatSize(Math.abs(trend.perWeek)),
                                        direction:
                                            trend.perWeek >= 0
                                                ? t('operations.housekeeping.size.rising')
                                                : t('operations.housekeeping.size.falling'),
                                    })}
                                </>
                            )}
                        </p>
                    )}
                </>
            )}
        </section>
    );
}

/** The bound: what governs the store, what it last did, and the form that changes it. */
function StoreBoundBlock({
    store,
    mayChange,
    onChanged,
}: {
    readonly store: StoreBound;
    readonly mayChange: boolean;
    readonly onChanged: () => Promise<void>;
}): ReactNode {
    const { t } = useTranslation();
    const [editing, setEditing] = useState(false);
    const bound = store.bound;
    const mechanism = mechanismOf(store);
    /*
     * A store the catalogue states no action for cannot be bounded by a period:
     * the calendar dates need a delete by range, and a form that wrote a period
     * against them would delete the days pricing still reads.
     */
    const changeable = mayChange && bound.action !== null;
    return (
        <section className="card space-y-4 p-4">
            <header className="flex flex-wrap items-start justify-between gap-3">
                <h2 className="text-sm font-medium">{t('operations.housekeeping.bound.title')}</h2>
                {changeable && !editing && (
                    <Button size="sm" variant="secondary" onClick={() => setEditing(true)}>
                        {bound.policy_code === null
                            ? t('operations.housekeeping.bound.create')
                            : t('operations.housekeeping.bound.change')}
                    </Button>
                )}
            </header>
            <BoundVerdict store={store} />
            {mechanism !== null && (
                <p className="text-sm text-ink-muted">
                    {t(`operations.housekeeping.mechanisms.${mechanism}`)}
                </p>
            )}
            <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-3">
                <Detail
                    label={t('operations.housekeeping.bound.fields.action')}
                    value={
                        bound.action === null
                            ? t('operations.housekeeping.store.fields.none')
                            : actionLabel(t, bound.action)
                    }
                />
                <Detail
                    label={t('operations.housekeeping.bound.fields.period')}
                    value={
                        bound.retention_days === null
                            ? t('operations.housekeeping.store.fields.none')
                            : t('operations.housekeeping.period', {
                                  days: String(bound.retention_days),
                              })
                    }
                />
                <Detail
                    label={t('operations.housekeeping.bound.fields.cadence')}
                    value={
                        bound.cadence_hours === null
                            ? t('operations.housekeeping.store.fields.none')
                            : t('operations.housekeeping.cadence', {
                                  hours: String(bound.cadence_hours),
                              })
                    }
                />
                <Detail
                    label={t('operations.housekeeping.bound.fields.policy')}
                    value={bound.policy_code ?? t('operations.housekeeping.row.noPolicy')}
                    mono
                />
                <Detail
                    label={t('operations.housekeeping.bound.fields.state')}
                    value={t(
                        bound.policy_code === null
                            ? 'operations.housekeeping.bound.absent'
                            : bound.policy_enabled
                              ? 'operations.housekeeping.bound.on'
                              : 'operations.housekeeping.bound.off',
                    )}
                />
                <Detail
                    label={t('operations.housekeeping.bound.fields.reason')}
                    value={
                        bound.policy_description ?? t('operations.housekeeping.store.fields.none')
                    }
                />
            </dl>
            <div className="space-y-1 text-sm">
                <h3 className="text-xs uppercase tracking-wide text-ink-faint">
                    {t('operations.housekeeping.bound.lastRun')}
                </h3>
                {store.last_run.outcome === null ? (
                    <p className="text-ink-muted">{t('operations.housekeeping.row.neverSwept')}</p>
                ) : (
                    <p className="text-ink-muted">
                        <Tag
                            tone={
                                store.last_run.outcome === 'failed'
                                    ? 'down'
                                    : store.last_run.outcome === 'skipped'
                                      ? 'warn'
                                      : 'neutral'
                            }
                        >
                            {t(`operations.housekeeping.outcomes.${store.last_run.outcome}`)}
                        </Tag>{' '}
                        {store.last_run.ran_at !== null && (
                            <RelativeTime at={store.last_run.ran_at} />
                        )}
                        {' · '}
                        {t('operations.housekeeping.row.removed', {
                            count: store.last_run.removed.toLocaleString(),
                        })}
                        {store.last_run.detail !== null && store.last_run.detail !== '' && (
                            <> · {store.last_run.detail}</>
                        )}
                    </p>
                )}
            </div>
            {editing && changeable && (
                <BoundForm
                    store={store}
                    onClose={() => setEditing(false)}
                    onSaved={async () => {
                        setEditing(false);
                        await onChanged();
                    }}
                />
            )}
            {!mayChange && (
                <p className="text-xs text-ink-faint">
                    {t('operations.housekeeping.bound.readOnly')}
                </p>
            )}
        </section>
    );
}

/** The form that changes a bound, with the reason the change is recorded under. */
function BoundForm({
    store,
    onClose,
    onSaved,
}: {
    readonly store: StoreBound;
    readonly onClose: () => void;
    readonly onSaved: () => Promise<void>;
}): ReactNode {
    const { t } = useTranslation();
    const bound = store.bound;
    // A policy created from nothing applies: the person opens the form to bound a store.
    const [enabled, setEnabled] = useState(
        bound.policy_code === null ? true : bound.policy_enabled,
    );
    const [days, setDays] = useState(String(bound.retention_days ?? 30));
    const [hours, setHours] = useState(String(bound.cadence_hours ?? 24));
    const [commentary, setCommentary] = useState('');
    const period = Number(days);
    const cadence = Number(hours);
    const valid =
        Number.isInteger(period) && period > 0 && Number.isInteger(cadence) && cadence > 0;
    const changed =
        enabled !== bound.policy_enabled ||
        period !== bound.retention_days ||
        cadence !== bound.cadence_hours;
    const reason = useReason('amend', changed);
    const save = useMutation({
        mutationFn: () =>
            api.saveStoreBound(store.code, {
                enabled,
                retention_days: period,
                cadence_hours: cadence,
                reason_code: reason.code,
                commentary,
            }),
        onSuccess: async () => {
            toast(t('operations.housekeeping.bound.saved'), 'info');
            await onSaved();
        },
    });
    return (
        <div className="space-y-3 rounded-md border border-line-subtle p-3">
            <p className="text-sm text-ink-muted">
                {bound.policy_code === null
                    ? t('operations.housekeeping.bound.leadCreate')
                    : t('operations.housekeeping.bound.lead')}
            </p>
            <div className="grid gap-3 sm:grid-cols-3">
                <Field label={t('operations.housekeeping.bound.fields.state')}>
                    <Select
                        value={enabled ? 'on' : 'off'}
                        onChange={(event) => setEnabled(event.target.value === 'on')}
                    >
                        <option value="on">{t('operations.housekeeping.bound.on')}</option>
                        <option value="off">{t('operations.housekeeping.bound.off')}</option>
                    </Select>
                </Field>
                <Field
                    label={t('operations.housekeeping.bound.fields.period')}
                    {...(valid ? {} : { error: t('operations.housekeeping.bound.periodInvalid') })}
                >
                    <Input
                        value={days}
                        inputMode="numeric"
                        onChange={(event) => setDays(event.target.value)}
                    />
                </Field>
                <Field
                    label={t('operations.housekeeping.bound.fields.cadence')}
                    {...(valid ? {} : { error: t('operations.housekeeping.bound.cadenceInvalid') })}
                >
                    <Input
                        value={hours}
                        inputMode="numeric"
                        onChange={(event) => setHours(event.target.value)}
                    />
                </Field>
            </div>
            <ReasonFields
                reason={reason}
                commentary={commentary}
                onCommentary={setCommentary}
                missing={reason.needsCommentary && commentary.trim() === ''}
            />
            <div className="flex items-center gap-2">
                <Button
                    size="sm"
                    variant="primary"
                    disabled={!valid || reason.code === '' || save.isPending}
                    onClick={() => save.mutate()}
                >
                    {save.isPending
                        ? t('operations.housekeeping.bound.saving')
                        : t('operations.housekeeping.bound.save')}
                </Button>
                <Button size="sm" variant="secondary" onClick={onClose}>
                    {t('common.cancel')}
                </Button>
            </div>
            {save.isError && (
                <Notice tone="error">
                    {t('operations.housekeeping.bound.failure', { message: save.error.message })}
                </Notice>
            )}
        </div>
    );
}

export function HousekeepingStorePage(): ReactNode {
    const { t } = useTranslation();
    const { code } = useParams();
    const stores = useHousekeepingStores();
    const queries = useQueryClient();
    const mayChange = useMayChangeBound();
    const { tab, bar } = useTabs({
        label: t('operations.housekeeping.store.tabs.label'),
        tabs: STORE_TABS,
        titleOf: (candidate) => t(`operations.housekeeping.store.tabs.${candidate}`),
    });
    const store = stores.data?.rows.find((candidate) => candidate.code === code);

    const trail = (
        <AreaTrail
            area="operations"
            steps={[
                { label: t('operations.screens.housekeeping'), to: PATHS.housekeeping },
                { label: store?.display_name ?? code ?? '' },
            ]}
        />
    );

    if (stores.isPending || stores.isError) {
        return (
            <ScreenFrame
                trail={trail}
                title={t('operations.housekeeping.store.title')}
                description={t('operations.housekeeping.store.description')}
            >
                {stores.isError ? (
                    <Notice tone="error">{stores.error.message}</Notice>
                ) : (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                )}
            </ScreenFrame>
        );
    }
    if (store === undefined) {
        return (
            <ScreenFrame
                trail={trail}
                title={t('operations.housekeeping.store.title')}
                description={t('operations.housekeeping.store.description')}
            >
                <Notice tone="warn">
                    {t('operations.housekeeping.store.notFound', { code: code ?? '' })}{' '}
                    <Link to={PATHS.housekeeping} className="text-accent hover:text-accent-bright">
                        {t('operations.housekeeping.store.back')}
                    </Link>
                </Notice>
            </ScreenFrame>
        );
    }
    return (
        <ScreenFrame trail={trail} title={store.display_name} description={store.description}>
            <div className="space-y-4">
                <p className="text-xs text-ink-faint">
                    <Link to={PATHS.housekeeping} className="text-accent hover:text-accent-bright">
                        {t('operations.housekeeping.store.back')}
                    </Link>{' '}
                    · {housekeepingStorePath(store.code)}
                </p>
                {bar}
                {tab === 'details' && (
                    <>
                        <StoreRecord store={store} />
                        <StoreSize store={store} />
                    </>
                )}
                {tab === 'bound' && (
                    <StoreBoundBlock
                        store={store}
                        mayChange={mayChange}
                        onChanged={async () => {
                            await queries.invalidateQueries({
                                queryKey: ['operations', 'housekeeping', 'stores'],
                            });
                        }}
                    />
                )}
                <GapPanel gaps={gapsOf(t)} />
            </div>
        </ScreenFrame>
    );
}

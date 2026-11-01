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
 */
import { useEffect, useState, type ReactNode } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Link, useNavigate, useParams, useSearchParams } from 'react-router';
import {
    api,
    type CalibratedFeed,
    type Calibration,
    type CalibrationCandidate,
} from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { OresmdUrl } from '../../oresmd/OresmdUrl.js';
import { ReasonFields, useReason } from '../../refdata/shared.js';
import { AreaTrail } from '../../shell/AreaTrail.js';
import { ScreenFrame } from '../../shell/ScreenFrame.js';
import {
    PATHS,
    calibrationFeedPath,
    calibrationPath,
    calibrationPolicyPath,
} from '../../shell/paths.js';
import { Metric } from '../../ui/Metric.js';
import {
    Button,
    Fact,
    Field,
    Input,
    Notice,
    PageHeader,
    Select,
    Tag,
    cx,
} from '../../ui/Primitives.js';
import { useTabs } from '../../ui/Tabs.js';
import { useFeedStory } from '../synthetic/FeedHistory.js';
import { RecordStoreProvider, syntheticStore } from '../../refdata/store.js';
import { useFeedParts } from '../synthetic/FeedScreens.js';
import { FeedStartMeasures } from '../synthetic/feedStart.js';
import { FX_FEED_KIND, fxFeedPath } from '../synthetic/fxFeeds.js';
import { formatDateTime } from '../../ui/Time.js';
import { CalibrationRun, useCalibrationRun } from './CalibrationRun.js';
import {
    CALIBRATED_CLASSES,
    FILTERS,
    calibrationEntries,
    filterOf,
    moveProvider,
    outcomeOf,
    overrides,
    visibleFeeds,
    type FeedOutcome,
} from './calibration.js';

/**
 * A class's calibration. A screen of one feed names it, so the service resolves that feed
 * alone; the key still starts with the class, so a change to the class refreshes it.
 */
function useCalibration(assetClass: string, configId?: string) {
    return useQuery({
        queryKey:
            configId === undefined
                ? ['calibration', assetClass]
                : ['calibration', assetClass, configId],
        queryFn: () => api.calibration(assetClass, configId),
    });
}

function useAssetClass(): string {
    const { assetClass } = useParams();
    return assetClass ?? CALIBRATED_CLASSES[0];
}

const OUTCOME_TONE = { fixing: 'up', fixed: 'warn', refused: 'down' } as const;

function OutcomeTag({ outcome }: { readonly outcome: FeedOutcome }): ReactNode {
    const { t } = useTranslation();
    return <Tag tone={OUTCOME_TONE[outcome]}>{t(`marketdata.calibration.outcome.${outcome}`)}</Tag>;
}

/** The policy in one line: automatic, or the class's own window, inverse rule, order and fallback. */
function PolicyLine({ calibration }: { readonly calibration: Calibration }): ReactNode {
    const { t } = useTranslation();
    const p = calibration.policy;
    return (
        <p className="text-sm text-ink-muted">
            {t(
                p.from_policy_row
                    ? 'marketdata.calibration.policyLine.custom'
                    : 'marketdata.calibration.policyLine.automatic',
                {
                    window: String(p.window_days),
                    inverse: t(
                        p.allow_inverse
                            ? 'marketdata.calibration.inverse.allowed'
                            : 'marketdata.calibration.inverse.refused',
                    ),
                    order:
                        p.provider_order.length === 0
                            ? t('marketdata.calibration.order.everyProvider')
                            : p.provider_order.join(' → '),
                    fallback: t(`marketdata.calibration.fallback.${p.fallback_code}`),
                },
            )}{' '}
            <Link
                className="text-accent hover:underline"
                to={calibrationPolicyPath(calibration.assetClass)}
            >
                {t('marketdata.calibration.policyLine.open')}
            </Link>
        </p>
    );
}

function Coverage({ calibration }: { readonly calibration: Calibration }): ReactNode {
    const { t } = useTranslation();
    const c = calibration.coverage;
    return (
        <div className="grid grid-cols-2 gap-3 sm:grid-cols-5">
            <Metric value={String(c.total)} label={t('marketdata.calibration.coverage.total')} />
            <Metric
                value={String(c.fromFixing)}
                label={t('marketdata.calibration.coverage.fromFixing')}
                tone="good"
            />
            <Metric
                value={String(c.keptFixed)}
                label={t('marketdata.calibration.coverage.keptFixed')}
                tone={c.keptFixed > 0 ? 'warn' : 'neutral'}
            />
            <Metric
                value={String(c.refused)}
                label={t('marketdata.calibration.coverage.refused')}
                tone={c.refused > 0 ? 'bad' : 'neutral'}
            />
            <Metric
                value={String(c.overridden)}
                label={t('marketdata.calibration.coverage.overridden')}
            />
        </div>
    );
}

function Loading({
    query,
    title,
    trail,
}: {
    readonly query: { readonly isError: boolean; readonly error: Error | null };
    readonly title: string;
    readonly trail: ReactNode;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <ScreenFrame trail={trail} title={title} description={t('marketdata.calibration.lead')}>
            {query.isError ? (
                <Notice tone="error">{query.error?.message}</Notice>
            ) : (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            )}
        </ScreenFrame>
    );
}

/** Every feed of one asset class, with how it starts. Rows open the feed's record. */
export function CalibrationPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const assetClass = useAssetClass();
    const [search, setSearch] = useSearchParams();
    const filter = filterOf(search.get('filter'));
    const calibration = useCalibration(assetClass);
    const client = useQueryClient();
    const run = useCalibrationRun(assetClass, () => {
        void client.invalidateQueries({ queryKey: ['calibration', assetClass] });
        void client.invalidateQueries({ queryKey: ['history'] });
    });
    const title = t('marketdata.calibration.title');
    const trail = <AreaTrail area="marketdata" screen={title} />;
    if (calibration.data === undefined) {
        return <Loading query={calibration} title={title} trail={trail} />;
    }
    const data = calibration.data;

    // Calibration is a system control: the feeds belong to the system tenant.
    if (!data.policyWritable) {
        return (
            <div className="space-y-6">
                <Notice tone="warn">{t('marketdata.calibration.systemOnly')}</Notice>
            </div>
        );
    }
    const shown = visibleFeeds(data.feeds, filter);
    return (
        <div className="space-y-6">
            <div>
                {trail}
                <PageHeader
                    title={title}
                    description={t('marketdata.calibration.lead')}
                    actions={
                        <div className="flex items-end gap-2">
                            <Field label={t('marketdata.calibration.assetClass')}>
                                <Select
                                    value={assetClass}
                                    onChange={(event) =>
                                        void navigate(calibrationPath(event.target.value))
                                    }
                                >
                                    {CALIBRATED_CLASSES.map((c) => (
                                        <option key={c} value={c}>
                                            {t(`marketdata.calibration.classes.${c}`)}
                                        </option>
                                    ))}
                                </Select>
                            </Field>
                            <Button
                                variant="primary"
                                onClick={() => void run.start(data.feeds)}
                                disabled={run.active || data.feeds.length === 0}
                                pending={run.active}
                            >
                                {run.active
                                    ? t('marketdata.calibration.calibrating')
                                    : t('marketdata.calibration.calibrateAll')}
                            </Button>
                        </div>
                    }
                />
                <PolicyLine calibration={data} />
            </div>
            {run.steps !== undefined && (
                <CalibrationRun
                    assetClass={assetClass}
                    steps={run.steps}
                    active={run.active}
                    onClose={run.close}
                />
            )}
            <Coverage calibration={data} />
            <div
                role="radiogroup"
                aria-label={t('marketdata.calibration.filter.label')}
                className="flex gap-1.5"
            >
                {FILTERS.map((f) => (
                    <button
                        key={f}
                        type="button"
                        role="radio"
                        aria-checked={f === filter}
                        onClick={() =>
                            setSearch(f === 'all' ? {} : { filter: f }, { replace: true })
                        }
                        className={
                            f === filter
                                ? 'rounded-chip border border-accent bg-accent-dim px-2 py-0.5 text-xs text-accent'
                                : 'rounded-chip border border-line px-2 py-0.5 text-xs text-ink-muted hover:text-ink'
                        }
                    >
                        {t(`marketdata.calibration.filter.${f}`)}
                    </button>
                ))}
            </div>
            <table className="w-full text-sm">
                <thead className="text-left text-xs text-ink-faint">
                    <tr>
                        <th className="py-2">{t('marketdata.calibration.columns.feed')}</th>
                        <th className="py-2">{t('marketdata.calibration.columns.start')}</th>
                        <th className="py-2">{t('marketdata.calibration.columns.source')}</th>
                        <th className="py-2">{t('marketdata.calibration.columns.state')}</th>
                    </tr>
                </thead>
                <tbody>
                    {shown.length === 0 ? (
                        <tr>
                            <td colSpan={4} className="py-3 text-ink-muted">
                                {t('marketdata.calibration.empty')}
                            </td>
                        </tr>
                    ) : (
                        shown.map((feed) => (
                            <tr key={feed.config_id} className="border-t border-line-subtle">
                                <td className="py-2">
                                    <Link
                                        className="font-medium text-ink hover:text-accent"
                                        to={calibrationFeedPath(assetClass, feed.config_id)}
                                    >
                                        {`${feed.base_currency_code}/${feed.quote_currency_code}`}
                                    </Link>
                                    <div className="font-mono text-xs text-ink-faint">
                                        {feed.source_name}
                                    </div>
                                </td>
                                <td className="py-2 tabular-nums">
                                    {feed.start.from_fixing ? feed.start.value : '—'}
                                </td>
                                <td className="py-2">
                                    {feed.start.from_fixing ? (
                                        <Link
                                            className="text-accent hover:underline"
                                            to={`${PATHS.fixingSeries}?uri=${encodeURIComponent(feed.start.item)}`}
                                            title={feed.start.item}
                                        >
                                            {`${feed.start.provider_code} · ${feed.start.fixing_date ?? ''}`}
                                        </Link>
                                    ) : (
                                        '—'
                                    )}
                                </td>
                                <td className="space-x-1 py-2">
                                    <OutcomeTag outcome={outcomeOf(feed)} />
                                    {overrides(feed) && (
                                        <Tag tone="muted">
                                            {t('marketdata.calibration.overrides')}
                                        </Tag>
                                    )}
                                </td>
                            </tr>
                        ))
                    )}
                </tbody>
            </table>
        </div>
    );
}

const CANDIDATE_TONE = { chosen: 'up', skipped: 'warn', not_reached: 'muted' } as const;

function Candidates({
    feed,
    onPin,
    pinning,
}: {
    readonly feed: CalibratedFeed;
    readonly onPin?: (candidate: CalibrationCandidate) => void;
    readonly pinning: boolean;
}): ReactNode {
    const { t } = useTranslation();
    if (feed.candidates.length === 0) {
        return (
            <p className="text-sm text-ink-muted">
                {t('marketdata.calibration.feed.noCandidates')}
            </p>
        );
    }
    return (
        <table className="w-full text-sm">
            <thead className="text-left text-xs text-ink-faint">
                <tr>
                    <th className="py-2">#</th>
                    <th className="py-2">{t('marketdata.calibration.feed.candidate')}</th>
                    <th className="py-2">{t('marketdata.calibration.feed.readAs')}</th>
                    <th className="py-2">{t('marketdata.calibration.columns.state')}</th>
                    {onPin !== undefined && <th className="py-2" />}
                </tr>
            </thead>
            <tbody>
                {feed.candidates.map((c) => (
                    <tr key={c.item} className="border-t border-line-subtle align-top">
                        <td className="py-2 tabular-nums">{c.rank}</td>
                        <td className="py-2">
                            <span className="font-medium">{c.provider_code}</span>
                            <OresmdUrl uri={c.item} />
                            <div className="text-xs text-ink-muted">{c.reason}</div>
                        </td>
                        <td className="py-2">
                            {c.inverted
                                ? t('marketdata.calibration.feed.inverse')
                                : t('marketdata.calibration.feed.direct')}
                        </td>
                        <td className="py-2">
                            <Tag tone={CANDIDATE_TONE[c.state]}>
                                {t(`marketdata.calibration.candidateState.${c.state}`)}
                            </Tag>
                        </td>
                        {onPin !== undefined && (
                            <td className="py-2 text-right">
                                <Button size="sm" disabled={pinning} onClick={() => onPin(c)}>
                                    {t('marketdata.calibration.feed.pin')}
                                </Button>
                            </td>
                        )}
                    </tr>
                ))}
            </tbody>
        </table>
    );
}

/** A titled card, as every block of the feed page is drawn. */
function Card({
    title,
    aside,
    children,
}: {
    readonly title: string;
    readonly aside?: ReactNode;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <section className="card space-y-3 p-4">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="font-medium">{title}</h2>
                {aside}
            </header>
            {children}
        </section>
    );
}

/** How the feed stands against the class: automatic, inheriting the policy, or overriding it. */
function FeedBanner({
    calibration,
    feed,
}: {
    readonly calibration: Calibration;
    readonly feed: CalibratedFeed;
}): ReactNode {
    const { t } = useTranslation();
    const overridden = overrides(feed);
    const state = overridden
        ? 'override'
        : calibration.policy.from_policy_row
          ? 'inherits'
          : 'automatic';
    const words = overridden
        ? feed.explicit_items
            ? t('marketdata.calibration.feed.overridesItems')
            : t('marketdata.calibration.feed.overridesWindow', {
                  days: String(feed.window_days_override ?? ''),
              })
        : state === 'automatic'
          ? t('marketdata.calibration.feed.banner.automatic')
          : t('marketdata.calibration.feed.banner.inherits', {
                order: calibration.policy.provider_order.join(' → '),
                window: String(calibration.policy.window_days),
            });
    return (
        <div
            className={
                overridden
                    ? 'flex flex-wrap items-center gap-2 rounded-card border border-warn/40 bg-warn/10 p-3 text-sm'
                    : 'flex flex-wrap items-center gap-2 rounded-card border border-line bg-raised p-3 text-sm'
            }
        >
            <Tag tone={overridden ? 'warn' : 'muted'}>
                {t(`marketdata.calibration.feed.badge.${state}`)}
            </Tag>
            <span className="flex-1">{words}</span>
            <Link className="text-accent hover:underline" to={fxFeedPath(feed.config_id)}>
                {t('marketdata.calibration.feed.editWindow')}
            </Link>
        </div>
    );
}

/** Where a run that starts now takes its level from, with the measured volatility. */
function StartCard({ feed }: { readonly feed: CalibratedFeed }): ReactNode {
    const { t } = useTranslation();
    const s = feed.start;
    return (
        <Card title={t('marketdata.calibration.feed.start')}>
            <div className="flex flex-wrap items-start justify-between gap-3">
                <div>
                    <div className="text-3xl font-semibold tabular-nums">
                        {s.from_fixing ? s.value : '—'}
                    </div>
                    <div className="text-sm text-ink-muted">
                        {s.from_fixing
                            ? [s.provider_code, s.fixing_date ?? '', s.received_at ?? '']
                                  .filter((x) => x !== '')
                                  .join(' · ')
                            : s.note}
                    </div>
                </div>
                <div className="flex flex-wrap gap-1">
                    <OutcomeTag outcome={outcomeOf(feed)} />
                    {s.inverted && (
                        <Tag tone="muted">{t('marketdata.calibration.feed.inverse')}</Tag>
                    )}
                    {s.from_fixing && <Tag tone="muted">{s.provider_code}</Tag>}
                </div>
            </div>
            <dl className="grid gap-3 sm:grid-cols-2">
                <div className="sm:col-span-2">
                    <Fact label={t('marketdata.calibration.feed.item')}>
                        {s.from_fixing ? <OresmdUrl uri={s.item} /> : '—'}
                    </Fact>
                </div>
                <Fact
                    label={t('marketdata.calibration.feed.window')}
                    value={
                        s.from_fixing
                            ? t('marketdata.calibration.feed.windowValue', {
                                  used: String(s.used),
                                  days: String(s.window_days),
                              })
                            : ''
                    }
                />
                <Fact label={t('marketdata.calibration.feed.date')} value={s.fixing_date ?? ''} />
            </dl>
            {s.inverted && (
                <p className="text-sm text-ink-muted">
                    {t('marketdata.calibration.feed.invertedNote')}
                </p>
            )}
            {s.from_fixing && s.note !== '' && <p className="text-sm text-ink-muted">{s.note}</p>}
            {!s.from_fixing && (
                <Notice tone="warn">
                    {feed.refused
                        ? t('marketdata.calibration.feed.statedRefusal')
                        : t('marketdata.calibration.feed.statedFallback')}
                </Notice>
            )}
        </Card>
    );
}

const TRAIL_MARK = { chosen: '✓', skipped: '–', not_reached: '·' } as const;
const TRAIL_TONE = {
    chosen: 'text-up',
    skipped: 'text-warn',
    not_reached: 'text-ink-faint',
} as const;

/** Each candidate the resolver read, in rank order, and where the feed starts. */
function Trail({ feed }: { readonly feed: CalibratedFeed }): ReactNode {
    const { t } = useTranslation();
    return (
        <ul className="space-y-1.5 text-sm">
            {feed.candidates.map((c) => (
                <li key={c.item} className="flex gap-2">
                    <span aria-hidden className={cx('w-3 font-semibold', TRAIL_TONE[c.state])}>
                        {TRAIL_MARK[c.state]}
                    </span>
                    <span>
                        <b>{c.provider_code}</b> — {c.reason}
                    </span>
                </li>
            ))}
            <li className="flex gap-2">
                <span
                    aria-hidden
                    className={cx(
                        'w-3 font-semibold',
                        feed.start.from_fixing ? 'text-up' : 'text-warn',
                    )}
                >
                    {feed.start.from_fixing ? '✓' : '–'}
                </span>
                <span>
                    <b>{t('marketdata.calibration.feed.trailStart')}</b> —{' '}
                    {feed.start.from_fixing
                        ? t('marketdata.calibration.feed.trailTakes', {
                              value: feed.start.value,
                              provider: feed.start.provider_code,
                          })
                        : feed.refused
                          ? t('marketdata.calibration.feed.trailRefused')
                          : t('marketdata.calibration.feed.trailKept')}
                </span>
            </li>
        </ul>
    );
}

/** The feed's own facts, and the way to its record. */
function ThisFeed({ feed }: { readonly feed: CalibratedFeed }): ReactNode {
    const { t } = useTranslation();
    return (
        <dl className="grid grid-cols-2 gap-3 text-sm">
            <Fact
                label={t('marketdata.calibration.feed.pair')}
                value={`${feed.base_currency_code}/${feed.quote_currency_code}`}
            />
            <Fact label={t('marketdata.calibration.feed.source')} value={feed.source_name} />
            <Fact
                label={t('marketdata.calibration.feed.policyState')}
                value={t(
                    overrides(feed)
                        ? 'marketdata.calibration.feed.badge.override'
                        : 'marketdata.calibration.feed.badge.inherits',
                )}
            />
            <Fact
                label={t('marketdata.calibration.feed.runs')}
                value={t(
                    feed.running
                        ? 'marketdata.calibration.feed.running'
                        : 'marketdata.calibration.feed.stopped',
                )}
            />
            <div className="col-span-2">
                <Link className="text-accent hover:underline" to={fxFeedPath(feed.config_id)}>
                    {t('marketdata.calibration.feed.openFeed')}
                </Link>
            </div>
        </dl>
    );
}

/** The path from the policy through the candidates to the level the process starts from. */
function FlowDiagram({
    calibration,
    feed,
}: {
    readonly calibration: Calibration;
    readonly feed: CalibratedFeed;
}): ReactNode {
    const { t } = useTranslation();
    const p = calibration.policy;
    const chosen = feed.candidates.find((c) => c.state === 'chosen');
    const node = 'rounded-card border border-line bg-raised p-3';
    return (
        <div className="grid items-center gap-3 md:grid-cols-[1fr_auto_1.4fr_auto_1fr]">
            <div className={node}>
                <p className="text-xs text-ink-faint">{t('marketdata.calibration.flow.policy')}</p>
                <p className="font-mono text-xs">
                    {p.provider_order.length === 0
                        ? t('marketdata.calibration.order.everyProvider')
                        : p.provider_order.join(' → ')}
                </p>
                <p className="text-xs text-ink-muted">
                    {t('marketdata.calibration.feed.windowDays', { days: String(p.window_days) })}
                </p>
            </div>
            <span aria-hidden className="hidden text-ink-faint md:block">
                →
            </span>
            <div className="space-y-1.5">
                <p className="text-xs text-ink-faint">
                    {t('marketdata.calibration.flow.candidates')}
                </p>
                {feed.candidates.length === 0 ? (
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.calibration.feed.noCandidates')}
                    </p>
                ) : (
                    feed.candidates.map((c) => (
                        <div
                            key={c.item}
                            className={cx(
                                'flex items-center gap-2 rounded-chip border px-2 py-1 text-xs',
                                c.state === 'chosen'
                                    ? 'border-up/50 bg-up/10'
                                    : 'border-line text-ink-muted',
                            )}
                        >
                            <span className="tabular-nums">{c.rank}</span>
                            <span className="flex-1">{c.provider_code}</span>
                            <Tag tone={CANDIDATE_TONE[c.state]} small>
                                {t(`marketdata.calibration.candidateState.${c.state}`)}
                            </Tag>
                        </div>
                    ))
                )}
            </div>
            <span aria-hidden className="hidden text-ink-faint md:block">
                →
            </span>
            <div className={node}>
                <p className="text-xs text-ink-faint">
                    {t('marketdata.calibration.flow.resolver')}
                </p>
                <p className="text-2xl font-semibold tabular-nums">
                    {feed.start.from_fixing ? feed.start.value : '—'}
                </p>
                <p className="text-xs text-ink-muted">
                    {chosen === undefined
                        ? feed.refused
                            ? t('marketdata.calibration.flow.refused')
                            : t('marketdata.calibration.flow.fixed')
                        : t('marketdata.calibration.flow.chosen', {
                              provider: chosen.provider_code,
                              value: feed.start.value,
                              date: feed.start.fixing_date ?? '',
                          })}
                </p>
                <p className="text-xs text-ink-muted">
                    {feed.start.hourly_spread == null
                        ? t('marketdata.calibration.flow.mixtureKept')
                        : t('marketdata.calibration.flow.mixtureScaled')}
                </p>
            </div>
        </div>
    );
}

/** The calibrations that changed this feed, read from its history. */
function RecentCalibrations({ configId }: { readonly configId: string }): ReactNode {
    return (
        <RecordStoreProvider store={syntheticStore}>
            <CalibrationLog configId={configId} />
        </RecordStoreProvider>
    );
}

/** The feed's parts are synthetic records, so this reads them under the synthetic store. */
function CalibrationLog({ configId }: { readonly configId: string }): ReactNode {
    const { t, language } = useTranslation();
    const parts = useFeedParts(FX_FEED_KIND, configId);
    const story = useFeedStory({ id: configId, version: 0 }, FX_FEED_KIND.entity, parts, 'fx_feed');
    if (story.error !== null) {
        return <Notice tone="error">{story.error.message}</Notice>;
    }
    if (story.stream === undefined) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    const entries = calibrationEntries(story.stream.events);
    if (entries.length === 0) {
        return (
            <p className="text-sm text-ink-muted">{t('marketdata.calibration.flow.noRecent')}</p>
        );
    }
    return (
        <table className="w-full text-sm">
            <thead className="text-left text-xs text-ink-faint">
                <tr>
                    <th className="py-2">{t('marketdata.calibration.flow.when')}</th>
                    <th className="py-2">{t('marketdata.calibration.flow.what')}</th>
                    <th className="py-2">{t('marketdata.calibration.flow.by')}</th>
                </tr>
            </thead>
            <tbody>
                {entries.map((e) => (
                    <tr
                        key={`${e.at}-${e.commentary}`}
                        className="border-t border-line-subtle align-top"
                    >
                        <td className="whitespace-nowrap py-2 font-mono text-xs text-ink-muted">
                            {formatDateTime(e.at, language)}
                        </td>
                        <td className="py-2">{e.commentary}</td>
                        <td className="py-2 text-xs text-ink-muted">{e.actor}</td>
                    </tr>
                ))}
            </tbody>
        </table>
    );
}

const FEED_TABS = ['record', 'flow'] as const;

/** One feed's calibration: its record (start, candidates, trail, measures) and its flow. */
export function CalibrationFeedPage(): ReactNode {
    const { t } = useTranslation();
    const assetClass = useAssetClass();
    const { configId = '' } = useParams();
    const calibration = useCalibration(assetClass, configId);
    const client = useQueryClient();
    const reason = useReason('amend');
    const [commentary, setCommentary] = useState('');
    const intent = { reasonCode: reason.code, commentary };
    const refresh = () => client.invalidateQueries({ queryKey: ['calibration', assetClass] });
    const pin = useMutation({
        mutationFn: (candidate: CalibrationCandidate) =>
            api.pinCalibrationItem(
                configId,
                {
                    item: candidate.item,
                    providerCode: candidate.provider_code,
                    windowDays: calibration.data?.policy.window_days ?? 90,
                },
                intent,
            ),
        onSuccess: refresh,
    });
    const clear = useMutation({
        mutationFn: () => api.clearCalibrationPin(configId, intent),
        onSuccess: refresh,
    });
    const calibrate = useMutation({
        mutationFn: () => api.runCalibration(assetClass, configId),
        onSuccess: () => {
            void refresh();
            void client.invalidateQueries({ queryKey: ['history'] });
        },
    });
    const { tab, bar } = useTabs({
        label: t('marketdata.calibration.feed.tabs'),
        tabs: FEED_TABS,
        titleOf: (id) => t(`marketdata.calibration.feed.tab.${id}`),
    });
    const listTitle = t('marketdata.calibration.title');
    const trail = (
        <AreaTrail
            area="marketdata"
            steps={[{ label: listTitle, to: calibrationPath(assetClass) }]}
            screen={t('marketdata.calibration.feed.title')}
        />
    );
    if (calibration.data === undefined) {
        return <Loading query={calibration} title={listTitle} trail={trail} />;
    }
    const data = calibration.data;

    // Calibration is a system control: the feeds belong to the system tenant.
    if (!data.policyWritable) {
        return (
            <div className="space-y-6">
                <Notice tone="warn">{t('marketdata.calibration.systemOnly')}</Notice>
            </div>
        );
    }
    const feed = data.feeds.find((f) => f.config_id === configId);
    if (feed === undefined) {
        return (
            <ScreenFrame
                trail={trail}
                title={listTitle}
                description={t('marketdata.calibration.lead')}
            >
                <Notice tone="warn">{t('marketdata.calibration.feed.missing')}</Notice>
            </ScreenFrame>
        );
    }
    const missingCommentary = reason.needsCommentary && commentary.trim() === '';
    const failure = pin.error ?? clear.error ?? calibrate.error;
    const done = calibrate.data;
    return (
        <div className="space-y-6">
            <div>
                {trail}
                <PageHeader
                    title={`${feed.base_currency_code}/${feed.quote_currency_code}`}
                    description={feed.source_name}
                    subdued
                    actions={
                        <div className="flex flex-wrap items-center gap-3">
                            <Link
                                className="text-sm text-accent hover:underline"
                                to={calibrationPath(assetClass)}
                            >
                                {t('marketdata.calibration.feed.back')}
                            </Link>
                            <Button
                                variant="primary"
                                pending={calibrate.isPending}
                                onClick={() => calibrate.mutate()}
                            >
                                {t('marketdata.calibration.feed.calibrate')}
                            </Button>
                        </div>
                    }
                />
            </div>
            {done !== undefined && (
                <Notice
                    tone={
                        done.outcome === 'failed' || done.outcome === 'refused'
                            ? 'error'
                            : done.outcome === 'kept'
                              ? 'warn'
                              : 'success'
                    }
                >
                    {done.outcome_message}
                    {done.outcome === 'updated' && done.running
                        ? ` ${t('marketdata.calibration.run.nextStart')}`
                        : ''}
                </Notice>
            )}
            <FeedBanner calibration={data} feed={feed} />
            {bar}
            {tab === 'record' ? (
                <div className="grid gap-4 lg:grid-cols-[3fr_2fr]">
                    <div className="space-y-4">
                        <StartCard feed={feed} />
                        <Card
                            title={t('marketdata.calibration.feed.candidates')}
                            aside={
                                <span className="text-xs text-ink-faint">
                                    {t('marketdata.calibration.feed.windowDays', {
                                        days: String(
                                            feed.window_days_override ?? data.policy.window_days,
                                        ),
                                    })}
                                </span>
                            }
                        >
                            <Candidates
                                feed={feed}
                                pinning={pin.isPending || missingCommentary}
                                onPin={(c) => pin.mutate(c)}
                            />
                            <ReasonFields
                                reason={reason}
                                commentary={commentary}
                                onCommentary={setCommentary}
                                missing={missingCommentary}
                            />
                            {feed.explicit_items && (
                                <Button
                                    disabled={missingCommentary}
                                    pending={clear.isPending}
                                    onClick={() => clear.mutate()}
                                >
                                    {t('marketdata.calibration.feed.clearPin')}
                                </Button>
                            )}
                        </Card>
                    </div>
                    <div className="space-y-4">
                        <Card title={t('marketdata.calibration.feed.trail')}>
                            <Trail feed={feed} />
                        </Card>
                        <Card title={t('marketdata.calibration.feed.measures')}>
                            {feed.start.from_fixing ? (
                                <FeedStartMeasures start={feed.start} />
                            ) : (
                                <p className="text-sm text-ink-muted">
                                    {t('marketdata.calibration.feed.noMeasures')}
                                </p>
                            )}
                        </Card>
                        <Card title={t('marketdata.calibration.feed.thisFeed')}>
                            <ThisFeed feed={feed} />
                        </Card>
                    </div>
                </div>
            ) : (
                <div className="grid gap-4 lg:grid-cols-[3fr_2fr]">
                    <div className="space-y-4">
                        <Card
                            title={t('marketdata.calibration.flow.title', {
                                pair: `${feed.base_currency_code}/${feed.quote_currency_code}`,
                            })}
                        >
                            <FlowDiagram calibration={data} feed={feed} />
                        </Card>
                        <Card title={t('marketdata.calibration.feed.trail')}>
                            <Trail feed={feed} />
                        </Card>
                    </div>
                    <div className="space-y-4">
                        <Card title={t('marketdata.calibration.flow.inherits')}>
                            <PolicyLine calibration={data} />
                        </Card>
                        <Card title={t('marketdata.calibration.flow.recent')}>
                            <RecentCalibrations configId={configId} />
                        </Card>
                    </div>
                </div>
            )}
            {failure !== null && <Notice tone="error">{failure.message}</Notice>}
        </div>
    );
}

/** A class's policy: the window, the inverse rule, the provider order and the fallback. */
export function CalibrationPolicyPage(): ReactNode {
    const { t } = useTranslation();
    const assetClass = useAssetClass();
    const calibration = useCalibration(assetClass);
    const providers = useQuery({ queryKey: ['public-rates'], queryFn: () => api.publicRates() });
    const client = useQueryClient();
    const reason = useReason('amend');
    const [commentary, setCommentary] = useState('');
    const [draft, setDraft] = useState<
        | {
              readonly windowDays: string;
              readonly allowInverse: boolean;
              readonly fallbackCode: 'keep_fixed' | 'refuse';
              readonly providerOrder: readonly string[];
          }
        | undefined
    >(undefined);
    const intent = { reasonCode: reason.code, commentary };
    const refresh = () => client.invalidateQueries({ queryKey: ['calibration', assetClass] });
    const save = useMutation({
        mutationFn: (version: number | null) =>
            api.saveCalibrationPolicy(
                assetClass,
                {
                    version,
                    windowDays: Number(draft?.windowDays),
                    allowInverse: draft?.allowInverse ?? true,
                    fallbackCode: draft?.fallbackCode ?? 'keep_fixed',
                    providerOrder: draft?.providerOrder ?? [],
                },
                intent,
            ),
        onSuccess: async () => {
            setDraft(undefined);
            await refresh();
        },
    });
    const reset = useMutation({
        mutationFn: () => api.resetCalibrationPolicy(assetClass, intent),
        onSuccess: async () => {
            setDraft(undefined);
            await refresh();
        },
    });
    const listTitle = t('marketdata.calibration.title');
    const trail = (
        <AreaTrail
            area="marketdata"
            steps={[{ label: listTitle, to: calibrationPath(assetClass) }]}
            screen={t('marketdata.calibration.policy.title')}
        />
    );
    if (calibration.data === undefined) {
        return <Loading query={calibration} title={listTitle} trail={trail} />;
    }
    const data = calibration.data;

    // Calibration is a system control: the feeds belong to the system tenant.
    if (!data.policyWritable) {
        return (
            <div className="space-y-6">
                <Notice tone="warn">{t('marketdata.calibration.systemOnly')}</Notice>
            </div>
        );
    }
    const p = data.policy;
    const shown = draft ?? {
        windowDays: String(p.window_days),
        allowInverse: p.allow_inverse,
        fallbackCode: p.fallback_code === 'refuse' ? ('refuse' as const) : ('keep_fixed' as const),
        providerOrder: p.provider_order,
    };
    const edit = (change: Partial<typeof shown>) => setDraft({ ...shown, ...change });
    const window = Number(shown.windowDays);
    const windowValid = Number.isInteger(window) && window >= 2 && window <= 180;
    const missingCommentary = reason.needsCommentary && commentary.trim() === '';
    const writable = data.policyWritable;
    const unused = (providers.data ?? []).filter((pr) => !shown.providerOrder.includes(pr.code));
    const failure = save.error ?? reset.error;
    return (
        <div className="space-y-6">
            <div>
                {trail}
                <PageHeader
                    title={t('marketdata.calibration.policy.title')}
                    description={t(`marketdata.calibration.classes.${assetClass}`)}
                    actions={
                        <Link
                            className="text-sm text-accent hover:underline"
                            to={calibrationPath(assetClass)}
                        >
                            {t('marketdata.calibration.feed.back')}
                        </Link>
                    }
                />
                <Notice tone="warn">{t('marketdata.calibration.policy.affectsEvery')}</Notice>
                <p className="text-sm text-ink-muted">
                    {p.from_policy_row
                        ? t('marketdata.calibration.policy.custom')
                        : t('marketdata.calibration.policy.automatic')}
                </p>
                {!writable && (
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.calibration.policy.systemOnly')}
                    </p>
                )}
            </div>
            <section className="card space-y-4 p-4">
                <div className="grid gap-4 sm:grid-cols-3">
                    <Field
                        label={t('marketdata.calibration.policy.window')}
                        {...(windowValid
                            ? {}
                            : { error: t('marketdata.calibration.policy.windowRange') })}
                    >
                        <Input
                            type="number"
                            min={2}
                            max={180}
                            disabled={!writable}
                            value={shown.windowDays}
                            onChange={(event) => edit({ windowDays: event.target.value })}
                        />
                    </Field>
                    <Field label={t('marketdata.calibration.policy.inverse')}>
                        <Select
                            disabled={!writable}
                            value={shown.allowInverse ? 'yes' : 'no'}
                            onChange={(event) =>
                                edit({ allowInverse: event.target.value === 'yes' })
                            }
                        >
                            <option value="yes">
                                {t('marketdata.calibration.inverse.allowed')}
                            </option>
                            <option value="no">
                                {t('marketdata.calibration.inverse.refused')}
                            </option>
                        </Select>
                    </Field>
                    <Field label={t('marketdata.calibration.policy.fallback')}>
                        <Select
                            disabled={!writable}
                            value={shown.fallbackCode}
                            onChange={(event) =>
                                edit({
                                    fallbackCode:
                                        event.target.value === 'refuse' ? 'refuse' : 'keep_fixed',
                                })
                            }
                        >
                            <option value="keep_fixed">
                                {t('marketdata.calibration.fallback.keep_fixed')}
                            </option>
                            <option value="refuse">
                                {t('marketdata.calibration.fallback.refuse')}
                            </option>
                        </Select>
                    </Field>
                </div>
                <div className="space-y-2">
                    <p className="text-sm font-medium">
                        {t('marketdata.calibration.policy.order')}
                    </p>
                    {shown.providerOrder.length === 0 ? (
                        <p className="text-sm text-ink-muted">
                            {t('marketdata.calibration.order.everyProvider')}
                        </p>
                    ) : (
                        <ol className="space-y-1">
                            {shown.providerOrder.map((code, index) => (
                                <li key={code} className="flex items-center gap-2 text-sm">
                                    <span className="w-5 tabular-nums text-ink-faint">
                                        {index + 1}
                                    </span>
                                    <span className="font-mono">{code}</span>
                                    {writable && (
                                        <>
                                            <Button
                                                size="sm"
                                                aria-label={t('marketdata.calibration.policy.up', {
                                                    provider: code,
                                                })}
                                                onClick={() =>
                                                    edit({
                                                        providerOrder: moveProvider(
                                                            shown.providerOrder,
                                                            code,
                                                            -1,
                                                        ),
                                                    })
                                                }
                                            >
                                                ↑
                                            </Button>
                                            <Button
                                                size="sm"
                                                aria-label={t(
                                                    'marketdata.calibration.policy.down',
                                                    { provider: code },
                                                )}
                                                onClick={() =>
                                                    edit({
                                                        providerOrder: moveProvider(
                                                            shown.providerOrder,
                                                            code,
                                                            1,
                                                        ),
                                                    })
                                                }
                                            >
                                                ↓
                                            </Button>
                                            <Button
                                                size="sm"
                                                onClick={() =>
                                                    edit({
                                                        providerOrder: shown.providerOrder.filter(
                                                            (c) => c !== code,
                                                        ),
                                                    })
                                                }
                                            >
                                                {t('marketdata.calibration.policy.remove')}
                                            </Button>
                                        </>
                                    )}
                                </li>
                            ))}
                        </ol>
                    )}
                    {writable && unused.length > 0 && (
                        <Field label={t('marketdata.calibration.policy.add')}>
                            <Select
                                value=""
                                onChange={(event) =>
                                    event.target.value !== '' &&
                                    edit({
                                        providerOrder: [...shown.providerOrder, event.target.value],
                                    })
                                }
                            >
                                <option value="">
                                    {t('marketdata.calibration.policy.addPrompt')}
                                </option>
                                {unused.map((pr) => (
                                    <option key={pr.code} value={pr.code}>
                                        {pr.name}
                                    </option>
                                ))}
                            </Select>
                        </Field>
                    )}
                </div>
                {writable && (
                    <>
                        <ReasonFields
                            reason={reason}
                            commentary={commentary}
                            onCommentary={setCommentary}
                            missing={missingCommentary}
                        />
                        <div className="flex flex-wrap gap-2">
                            <Button
                                variant="primary"
                                disabled={draft === undefined || !windowValid || missingCommentary}
                                pending={save.isPending}
                                onClick={() => save.mutate(p.version)}
                            >
                                {t('marketdata.calibration.policy.save')}
                            </Button>
                            {p.from_policy_row && (
                                <Button
                                    disabled={missingCommentary}
                                    pending={reset.isPending}
                                    onClick={() => reset.mutate()}
                                >
                                    {t('marketdata.calibration.policy.reset')}
                                </Button>
                            )}
                        </div>
                    </>
                )}
                {failure !== null && <Notice tone="error">{failure.message}</Notice>}
            </section>
            <section className="space-y-3">
                <h2 className="font-medium">{t('marketdata.calibration.policy.coverage')}</h2>
                <Coverage calibration={data} />
            </section>
            <section className="space-y-3">
                <h2 className="font-medium">{t('marketdata.calibration.policy.record')}</h2>
                <table className="w-full text-sm">
                    <thead className="text-left text-xs text-ink-faint">
                        <tr>
                            <th className="py-2">{t('marketdata.calibration.columns.feed')}</th>
                            <th className="py-2">{t('marketdata.calibration.columns.source')}</th>
                            <th className="py-2">{t('marketdata.calibration.columns.state')}</th>
                        </tr>
                    </thead>
                    <tbody>
                        {data.feeds.map((feed) => (
                            <tr key={feed.config_id} className="border-t border-line-subtle">
                                <td className="py-2">
                                    <Link
                                        className="hover:text-accent"
                                        to={calibrationFeedPath(assetClass, feed.config_id)}
                                    >{`${feed.base_currency_code}/${feed.quote_currency_code}`}</Link>
                                </td>
                                <td className="py-2">
                                    {feed.start.from_fixing ? feed.start.item : feed.start.note}
                                </td>
                                <td className="py-2">
                                    <OutcomeTag outcome={outcomeOf(feed)} />
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </section>
        </div>
    );
}

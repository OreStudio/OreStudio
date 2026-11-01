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
} from '../../ui/Primitives.js';
import { useTabs } from '../../ui/Tabs.js';
import { FeedStartMeasures } from '../synthetic/feedStart.js';
import { fxFeedPath } from '../synthetic/fxFeeds.js';
import {
    CALIBRATED_CLASSES,
    FILTERS,
    filterOf,
    moveProvider,
    outcomeOf,
    overrides,
    visibleFeeds,
    type FeedOutcome,
} from './calibration.js';

function useCalibration(assetClass: string) {
    return useQuery({
        queryKey: ['calibration', assetClass],
        queryFn: () => api.calibration(assetClass),
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
    const { t, language } = useTranslation();
    const navigate = useNavigate();
    const assetClass = useAssetClass();
    const [search, setSearch] = useSearchParams();
    const filter = filterOf(search.get('filter'));
    const calibration = useCalibration(assetClass);
    const [calibrating, setCalibrating] = useState(false);
    const [recorded, setRecorded] = useState(false);
    const [elapsed, setElapsed] = useState(0);
    useEffect(() => {
        if (!calibrating) return undefined;
        const started = Date.now();
        const timer = window.setInterval(() => setElapsed(Math.floor((Date.now() - started) / 1000)), 250);
        return () => window.clearInterval(timer);
    }, [calibrating]);
    const calibrate = () => {
        setCalibrating(true);
        setRecorded(false);
        setElapsed(0);
        void calibration.refetch().then(() => {
            setRecorded(true);
            window.setTimeout(() => setCalibrating(false), 900);
        });
    };
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
                                onClick={calibrate}
                                disabled={calibration.isFetching}
                                pending={calibration.isFetching}
                            >
                                {calibration.isFetching
                                    ? t('marketdata.calibration.calibrating')
                                    : t('marketdata.calibration.calibrateAll')}
                            </Button>
                        </div>
                    }
                />
                <PolicyLine calibration={data} />
                {calibrating && (
                    <div className="fixed inset-0 z-20 grid place-items-center bg-black/50">
                        <div className="w-full max-w-md space-y-4 rounded-card border border-line bg-raised p-6">
                            <h2 className="text-base font-semibold">
                                {t('marketdata.calibration.progress.title')}
                            </h2>
                            <ol className="space-y-2 text-sm">
                                <li className="flex items-center gap-2">
                                    <span className="text-up">✓</span>
                                    {t('marketdata.calibration.progress.policy')}
                                </li>
                                <li className="flex items-center gap-2">
                                    {recorded ? (
                                        <span className="text-up">✓</span>
                                    ) : (
                                        <span className="inline-block h-3 w-3 animate-spin rounded-full border border-line-strong border-t-transparent" />
                                    )}
                                    {t('marketdata.calibration.progress.feeds', {
                                        seconds: String(elapsed),
                                    })}
                                </li>
                                <li className="flex items-center gap-2">
                                    {recorded ? (
                                        <span className="text-up">✓</span>
                                    ) : (
                                        <span className="text-ink-faint">·</span>
                                    )}
                                    {t('marketdata.calibration.progress.record')}
                                </li>
                            </ol>
                            <p className="text-xs text-ink-faint">
                                {t('marketdata.calibration.progress.note')}
                            </p>
                        </div>
                    </div>
                )}
                {calibration.dataUpdatedAt > 0 && (
                    <p className="mt-1 text-xs text-ink-faint">
                        {t('marketdata.calibration.calibratedAt', {
                            when: new Intl.DateTimeFormat(language, {
                                dateStyle: 'medium',
                                timeStyle: 'short',
                            }).format(new Date(calibration.dataUpdatedAt)),
                        })}
                    </p>
                )}
            </div>
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

/** The path from the policy through the candidates to the level the process starts from. */
function Flow({
    calibration,
    feed,
}: {
    readonly calibration: Calibration;
    readonly feed: CalibratedFeed;
}): ReactNode {
    const { t } = useTranslation();
    const chosen = feed.candidates.find((c) => c.state === 'chosen');
    return (
        <ol className="space-y-3 text-sm">
            <li>
                <p className="font-medium">{t('marketdata.calibration.flow.policy')}</p>
                <PolicyLine calibration={calibration} />
            </li>
            <li>
                <p className="font-medium">{t('marketdata.calibration.flow.candidates')}</p>
                <p className="text-ink-muted">
                    {feed.explicit_items
                        ? t('marketdata.calibration.flow.explicit', {
                              count: String(feed.candidates.length),
                          })
                        : t('marketdata.calibration.flow.derived', {
                              count: String(feed.candidates.length),
                          })}
                </p>
            </li>
            <li>
                <p className="font-medium">{t('marketdata.calibration.flow.resolver')}</p>
                <p className="text-ink-muted">
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
            </li>
            <li>
                <p className="font-medium">{t('marketdata.calibration.flow.process')}</p>
                <p className="text-ink-muted">
                    {feed.start.hourly_spread == null
                        ? t('marketdata.calibration.flow.mixtureKept')
                        : t('marketdata.calibration.flow.mixtureScaled')}
                </p>
            </li>
        </ol>
    );
}

const FEED_TABS = ['record', 'flow', 'events'] as const;

/** What the last calibration did to one feed, with the time it ran. */
function CalibrationEvents({
    feed,
    at,
}: {
    readonly feed: CalibratedFeed;
    readonly at: number;
}): ReactNode {
    const { t, language } = useTranslation();
    const when = new Intl.DateTimeFormat(language, {
        dateStyle: 'medium',
        timeStyle: 'short',
    }).format(new Date(at));
    const events: string[] = [];
    if (feed.start.from_fixing) {
        events.push(
            t('marketdata.calibration.events.resolved', {
                provider: feed.start.provider_code,
                item: feed.start.item,
                value: feed.start.value,
                date: feed.start.fixing_date ?? '',
            }),
        );
    } else if (feed.refused) {
        events.push(t('marketdata.calibration.events.refused'));
    } else {
        events.push(t('marketdata.calibration.events.kept'));
    }
    if (feed.explicit_items) events.push(t('marketdata.calibration.events.pinned'));
    if (feed.window_days_override != null) {
        events.push(
            t('marketdata.calibration.events.window', {
                days: String(feed.window_days_override),
            }),
        );
    }
    return (
        <section className="card space-y-3 p-4">
            <h2 className="font-medium">{t('marketdata.calibration.events.title')}</h2>
            <table className="w-full text-sm">
                <thead className="text-left text-xs text-ink-faint">
                    <tr>
                        <th className="py-2">{t('marketdata.calibration.events.time')}</th>
                        <th className="py-2">{t('marketdata.calibration.events.event')}</th>
                    </tr>
                </thead>
                <tbody>
                    {events.map((event) => (
                        <tr key={event} className="border-t border-line-subtle">
                            <td className="py-2 font-mono text-xs text-ink-muted">{when}</td>
                            <td className="py-2">{event}</td>
                        </tr>
                    ))}
                </tbody>
            </table>
            <p className="text-xs text-ink-faint">{t('marketdata.calibration.events.note')}</p>
        </section>
    );
}

/** One feed's calibration: its record (start, candidates, measures, exceptions) and its flow. */
export function CalibrationFeedPage(): ReactNode {
    const { t } = useTranslation();
    const assetClass = useAssetClass();
    const { configId = '' } = useParams();
    const calibration = useCalibration(assetClass);
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
    const failure = pin.error ?? clear.error;
    return (
        <div className="space-y-6">
            <div>
                {trail}
                <PageHeader
                    title={`${feed.base_currency_code}/${feed.quote_currency_code}`}
                    description={feed.source_name}
                    subdued
                    actions={
                        <Link
                            className="text-sm text-accent hover:underline"
                            to={calibrationPath(assetClass)}
                        >
                            {t('marketdata.calibration.feed.back')}
                        </Link>
                    }
                />
            </div>
            {bar}
            {tab === 'events' ? (
                <CalibrationEvents feed={feed} at={calibration.dataUpdatedAt} />
            ) : tab === 'record' ? (
                <div className="space-y-6">
                    <section className="card space-y-3 p-4">
                        <h2 className="font-medium">{t('marketdata.calibration.feed.start')}</h2>
                        <dl className="grid grid-cols-2 gap-3 sm:grid-cols-4">
                            <Fact label={t('marketdata.calibration.columns.state')}>
                                <OutcomeTag outcome={outcomeOf(feed)} />
                            </Fact>
                            <Fact
                                label={t('marketdata.calibration.feed.level')}
                                value={feed.start.from_fixing ? feed.start.value : ''}
                            />
                            <Fact
                                label={t('marketdata.calibration.feed.provider')}
                                value={feed.start.provider_code}
                            />
                            <Fact
                                label={t('marketdata.calibration.feed.date')}
                                value={feed.start.fixing_date ?? ''}
                            />
                        </dl>
                        {feed.start.inverted && (
                            <p className="text-sm text-ink-muted">
                                {t('marketdata.calibration.feed.invertedNote')}
                            </p>
                        )}
                        {feed.start.note !== '' && (
                            <p className="text-sm text-ink-muted">{feed.start.note}</p>
                        )}
                        {feed.start.from_fixing && <FeedStartMeasures start={feed.start} />}
                    </section>
                    <section className="card space-y-3 p-4">
                        <h2 className="font-medium">
                            {t('marketdata.calibration.feed.candidates')}
                        </h2>
                        <Candidates
                            feed={feed}
                            pinning={pin.isPending || missingCommentary}
                            onPin={(c) => pin.mutate(c)}
                        />
                    </section>
                    <section className="card space-y-3 p-4">
                        <h2 className="font-medium">
                            {t('marketdata.calibration.feed.exceptions')}
                        </h2>
                        {overrides(feed) ? (
                            <Notice tone="warn">
                                {feed.explicit_items
                                    ? t('marketdata.calibration.feed.overridesItems')
                                    : t('marketdata.calibration.feed.overridesWindow', {
                                          days: String(feed.window_days_override ?? ''),
                                      })}
                            </Notice>
                        ) : (
                            <p className="text-sm text-ink-muted">
                                {t('marketdata.calibration.feed.followsPolicy')}
                            </p>
                        )}
                        <ReasonFields
                            reason={reason}
                            commentary={commentary}
                            onCommentary={setCommentary}
                            missing={missingCommentary}
                        />
                        <div className="flex flex-wrap gap-2">
                            {feed.explicit_items && (
                                <Button
                                    disabled={missingCommentary}
                                    pending={clear.isPending}
                                    onClick={() => clear.mutate()}
                                >
                                    {t('marketdata.calibration.feed.clearPin')}
                                </Button>
                            )}
                            <Link
                                className="text-sm text-accent hover:underline"
                                to={fxFeedPath(feed.config_id)}
                            >
                                {t('marketdata.calibration.feed.editWindow')}
                            </Link>
                            <Link
                                className="text-sm text-accent hover:underline"
                                to={calibrationPolicyPath(assetClass)}
                            >
                                {t('marketdata.calibration.feed.toPolicy')}
                            </Link>
                        </div>
                        {failure !== null && <Notice tone="error">{failure.message}</Notice>}
                    </section>
                </div>
            ) : (
                <section className="card p-4">
                    <Flow calibration={data} feed={feed} />
                </section>
            )}
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

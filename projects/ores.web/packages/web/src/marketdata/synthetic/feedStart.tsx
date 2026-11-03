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

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import type { FeedStart, RecordRow } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { OresmdUrl } from '../../oresmd/OresmdUrl.js';
import { show } from '../../refdata/records.js';
import { calibrationFeedPath } from '../../shell/paths.js';
import { Card, MeasureTile } from '../../ui/Card.js';
import { Fact, Notice, Tag } from '../../ui/Primitives.js';
import { formatDateTime } from '../../ui/Time.js';
import { ParameterTable, garchText } from '../calibration/VarianceCalibration.js';

/** A run's level in the pair's market convention: an inverted item is read as one over it. */
export function levelOf(start: FeedStart): string {
    const raw = Number(start.value);
    if (!start.inverted || !Number.isFinite(raw) || raw === 0) {
        return start.value;
    }
    return (1 / raw).toPrecision(6);
}

export const percent = (value: number): string => `${(value * 100).toFixed(2)}%`;

type Variance = NonNullable<FeedStart['variance']>;

/**
 * One feed's run on one tab, as fact cards: the level it started at and where that came
 * from, what the fixing window measured, the daily variance it carries, and the calibration
 * that variance was approved as. A stopped feed shows what its config names.
 */
export function FeedRunTab({
    feed,
    start,
}: {
    readonly feed: RecordRow;
    readonly start: FeedStart | null | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const garch = show(feed['variance_model']) === 'gjr_garch';
    const running = start !== null && start !== undefined;
    return (
        <div className="space-y-4" data-feed-run>
            {!running && <Notice tone="info">{t('marketdata.fxFeeds.run.notRunning')}</Notice>}
            <div className="grid gap-4 lg:grid-cols-2">
                <div className="space-y-4">
                    {running && (
                        <LevelCard start={start} configured={show(feed['gmm_initial_price'])} />
                    )}
                    {running && start.from_fixing && <WindowCard start={start} />}
                </div>
                <div className="space-y-4">
                    {show(feed['variance_model']) !== '' && (
                        <VarianceCard garch={garch} variance={running ? start.variance : null} />
                    )}
                    {garch && (
                        <CalibrationCard feed={feed} variance={running ? start.variance : null} />
                    )}
                </div>
            </div>
        </div>
    );
}

function LevelCard({
    start,
    configured,
}: {
    readonly start: FeedStart;
    readonly configured: string;
}): ReactNode {
    const { t, language } = useTranslation();
    return (
        <Card
            title={t('marketdata.fxFeeds.run.level')}
            aside={
                <Tag tone={start.from_fixing ? 'up' : 'warn'} small>
                    {t(
                        start.from_fixing
                            ? 'marketdata.fxFeeds.run.fromFixing'
                            : 'marketdata.fxFeeds.run.configured',
                    )}
                </Tag>
            }
        >
            <dl className="grid gap-4 sm:grid-cols-3">
                <Fact label={t('marketdata.fxFeeds.run.levelValue')}>
                    <span className="font-mono text-base font-semibold tabular-nums">
                        {start.from_fixing ? levelOf(start) : configured || '—'}
                    </span>
                </Fact>
                {start.from_fixing && (
                    <>
                        <Fact label={t('marketdata.fxFeeds.run.fixing')} value={start.value} />
                        <Fact
                            label={t('marketdata.fxFeeds.run.orientation')}
                            value={t(
                                start.inverted
                                    ? 'marketdata.fxFeeds.run.inverted'
                                    : 'marketdata.fxFeeds.run.direct',
                            )}
                        />
                        <Fact
                            label={t('marketdata.fxFeeds.run.provider')}
                            value={start.provider_code}
                        />
                        <Fact
                            label={t('marketdata.fxFeeds.run.fixingDate')}
                            value={start.fixing_date ?? ''}
                        />
                        <Fact
                            label={t('marketdata.fxFeeds.run.received')}
                            value={
                                start.received_at === null || start.received_at === undefined
                                    ? ''
                                    : formatDateTime(start.received_at, language)
                            }
                        />
                    </>
                )}
            </dl>
            {start.from_fixing && start.item !== '' && (
                <Fact label={t('marketdata.fxFeeds.run.item')}>
                    <OresmdUrl uri={start.item} />
                </Fact>
            )}
            {start.note !== '' && <p className="text-xs text-ink-muted">{start.note}</p>}
        </Card>
    );
}

/**
 * What the window's fixings measured. Only the volatility changes the calibration today,
 * since it scales the mixture; the rest are recorded for a later policy.
 */
function WindowCard({ start }: { readonly start: FeedStart }): ReactNode {
    const { t } = useTranslation();
    const recorded: { label: string; value: string }[] = [];
    if (start.annual_drift != null) {
        recorded.push({
            label: t('marketdata.fxFeeds.run.drift'),
            value: percent(start.annual_drift),
        });
    }
    if (start.min_return != null && start.max_return != null) {
        recorded.push({
            label: t('marketdata.fxFeeds.run.range'),
            value: `${percent(start.min_return)} / ${percent(start.max_return)}`,
        });
    }
    if (start.age_days != null) {
        recorded.push({
            label: t('marketdata.fxFeeds.run.age'),
            value: t('marketdata.fxFeeds.run.days', { days: String(start.age_days) }),
        });
    }
    if (start.jumps != null) {
        recorded.push({ label: t('marketdata.fxFeeds.run.jumps'), value: String(start.jumps) });
    }
    for (const other of start.dispersion ?? []) {
        recorded.push({
            label: t('marketdata.fxFeeds.run.dispersion', { provider: other.provider_code }),
            value: `${other.basis_points.toFixed(1)} bp`,
        });
    }
    return (
        <Card title={t('marketdata.fxFeeds.run.window')}>
            <div className="grid gap-2 sm:grid-cols-3">
                <MeasureTile
                    label={t('marketdata.fxFeeds.run.fixings')}
                    value={String(start.used)}
                    sub={t('marketdata.fxFeeds.run.days', { days: String(start.window_days) })}
                />
                {start.daily_spread != null && (
                    <MeasureTile
                        label={t('marketdata.fxFeeds.run.dailyVolatility')}
                        value={percent(start.daily_spread)}
                        sub={t('marketdata.fxFeeds.run.used')}
                    />
                )}
                {start.hourly_spread != null && (
                    <MeasureTile
                        label={t('marketdata.fxFeeds.run.hourlyVolatility')}
                        value={percent(start.hourly_spread)}
                        sub={t('marketdata.fxFeeds.run.used')}
                    />
                )}
                {recorded.map((one) => (
                    <MeasureTile
                        key={one.label}
                        label={one.label}
                        value={one.value}
                        sub={t('marketdata.fxFeeds.run.recorded')}
                    />
                ))}
            </div>
        </Card>
    );
}

function VarianceCard({
    garch,
    variance,
}: {
    readonly garch: boolean;
    readonly variance: Variance | null | undefined;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <Card
            title={t('marketdata.fxFeeds.run.variance')}
            aside={
                <Tag tone="muted" small>
                    {t(
                        garch
                            ? 'marketdata.fxFeeds.run.model.gjr_garch'
                            : 'marketdata.fxFeeds.run.model.constant',
                    )}
                </Tag>
            }
        >
            {!garch ? (
                <p className="text-sm text-ink-muted">{t('marketdata.fxFeeds.run.constant')}</p>
            ) : variance === null || variance === undefined ? (
                <p className="text-sm text-ink-muted">{t('marketdata.fxFeeds.run.noShape')}</p>
            ) : (
                <>
                    <ParameterTable
                        rows={[
                            {
                                label: t('marketdata.calibration.variance.alpha'),
                                value: garchText(variance.alpha),
                            },
                            {
                                label: t('marketdata.calibration.variance.gamma'),
                                value: garchText(variance.gamma),
                            },
                            {
                                label: t('marketdata.calibration.variance.beta'),
                                value: garchText(variance.beta),
                            },
                            {
                                label: t('marketdata.calibration.variance.persistence'),
                                value: garchText(variance.persistence),
                            },
                            ...(variance.fitted
                                ? [
                                      {
                                          label: t('marketdata.calibration.variance.startingRatio'),
                                          value: variance.initial_ratio.toFixed(2),
                                      },
                                  ]
                                : []),
                        ]}
                    />
                    <dl className="grid gap-4 sm:grid-cols-2">
                        <Fact
                            label={t('marketdata.fxFeeds.run.shape')}
                            value={
                                variance.fitted
                                    ? t('marketdata.fxFeeds.run.fitted', {
                                          returns: String(variance.returns),
                                      })
                                    : t('marketdata.calibration.variance.method.authored')
                            }
                        />
                    </dl>
                    {variance.note !== '' && (
                        <p className="text-xs text-ink-muted">{variance.note}</p>
                    )}
                </>
            )}
        </Card>
    );
}

/**
 * The calibration the run took its variance from, as a link to its page on the
 * calibration screen. When the config names another calibration, a review approved a
 * newer one after the run started, and a restart runs it.
 */
function CalibrationCard({
    feed,
    variance,
}: {
    readonly feed: RecordRow;
    readonly variance: Variance | null | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const configured = show(feed['variance_calibration_id']);
    const used = variance?.calibration_id ?? '';
    const to = `${calibrationFeedPath(show(feed['asset_class_code']) || 'fx', show(feed['id']))}?tab=variance`;
    const link = (id: string): ReactNode =>
        id === '' ? (
            '—'
        ) : (
            <Link className="font-mono text-xs text-accent hover:underline" to={to}>
                {id}
            </Link>
        );
    return (
        <Card title={t('marketdata.fxFeeds.run.calibration')}>
            <dl className="grid gap-4 sm:grid-cols-2">
                {variance !== null && variance !== undefined && (
                    <>
                        <Fact label={t('marketdata.fxFeeds.run.calibrationRun')}>{link(used)}</Fact>
                        <Fact
                            label={t('marketdata.fxFeeds.run.version')}
                            value={used === '' ? '' : String(variance.calibration_version)}
                        />
                    </>
                )}
                {used !== configured && (
                    <Fact label={t('marketdata.fxFeeds.run.calibrationConfigured')}>
                        {link(configured)}
                    </Fact>
                )}
            </dl>
            {variance !== null && variance !== undefined && used !== configured && (
                <Notice tone="warn">{t('marketdata.fxFeeds.run.restartToApply')}</Notice>
            )}
            <Link className="text-sm text-accent hover:underline" to={to}>
                {t('marketdata.fxFeeds.run.openCalibration')}
            </Link>
        </Card>
    );
}

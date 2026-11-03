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
import type { FeedStart } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { OresmdUrl } from '../../oresmd/OresmdUrl.js';
import { formatDateTime } from '../../ui/Time.js';

/** A run's level in the pair's market convention: an inverted item is read as one over it. */
export function levelOf(start: FeedStart): string {
    const raw = Number(start.value);
    if (!start.inverted || !Number.isFinite(raw) || raw === 0) {
        return start.value;
    }
    return (1 / raw).toPrecision(6);
}

function Section({
    title,
    children,
}: {
    readonly title: string;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <section className="space-y-2">
            <h3 className="text-sm font-medium text-ink">{title}</h3>
            <div className="space-y-1 text-sm text-ink-muted">{children}</div>
        </section>
    );
}

/**
 * Where a run started, as plain text in sections: the level it took, in the pair's market
 * convention; the fixing that level came from; the window that measured its spread; and the
 * daily variance it runs, with the calibration that variance was approved as. A reader can
 * follow each number to its source.
 */
export function FeedRunPanel({ start }: { readonly start: FeedStart }): ReactNode {
    const { t, language } = useTranslation();
    const received =
        start.received_at === null || start.received_at === undefined
            ? t('marketdata.fxFeeds.start.receivedUnknown')
            : formatDateTime(start.received_at, language);
    const date =
        start.fixing_date === null || start.fixing_date === undefined
            ? t('marketdata.fxFeeds.start.dateUnknown')
            : start.fixing_date;
    return (
        <div className="space-y-6" data-feed-run>
            <Section title={t('marketdata.fxFeeds.run.level')}>
                {start.from_fixing ? (
                    <>
                        <p className="text-base font-medium tabular-nums text-ink">
                            {levelOf(start)}
                        </p>
                        <p>
                            {start.inverted
                                ? t('marketdata.fxFeeds.run.levelInverted', {
                                      provider: start.provider_code,
                                      value: start.value,
                                      date,
                                      received,
                                  })
                                : t('marketdata.fxFeeds.start.fromFixing', {
                                      provider: start.provider_code,
                                      value: start.value,
                                      date,
                                      received,
                                  })}
                        </p>
                    </>
                ) : (
                    <p>{t('marketdata.fxFeeds.start.fixedLevel')}</p>
                )}
                {start.note !== '' && <p>{start.note}</p>}
            </Section>
            {start.from_fixing && start.item !== '' && (
                <Section title={t('marketdata.fxFeeds.run.source')}>
                    <OresmdUrl uri={start.item} />
                </Section>
            )}
            {start.from_fixing && (start.used > 0 || start.daily_spread != null) && (
                <Section title={t('marketdata.fxFeeds.run.window')}>
                    {start.used > 0 && (
                        <p>
                            {t('marketdata.fxFeeds.start.window', {
                                used: String(start.used),
                                days: String(start.window_days),
                            })}
                        </p>
                    )}
                    <FeedStartMeasures start={start} />
                </Section>
            )}
            {start.variance !== null && start.variance !== undefined && (
                <Section title={t('marketdata.fxFeeds.run.variance')}>
                    <FeedStartVariance variance={start.variance} />
                </Section>
            )}
        </div>
    );
}

/** The decimals a GARCH parameter is read to: enough to tell one fit from another. */
const shapeText = (value: number): string => value.toFixed(3);

/**
 * How the run's daily variance started: the GJR-GARCH shape it carries, whether its
 * history fitted that shape or the authored one was kept, and how the first day's
 * variance stands against the long-run level.
 */
export function FeedStartVariance({
    variance,
}: {
    readonly variance: NonNullable<FeedStart['variance']>;
}): ReactNode {
    const { t } = useTranslation();
    const shape = {
        alpha: shapeText(variance.alpha),
        gamma: shapeText(variance.gamma),
        beta: shapeText(variance.beta),
        persistence: shapeText(variance.persistence),
    };
    return (
        <div data-feed-variance className="space-y-1">
            {variance.calibration_id !== '' && (
                <p>
                    {t('marketdata.fxFeeds.start.variance.calibration', {
                        id: variance.calibration_id,
                        version: String(variance.calibration_version),
                    })}
                </p>
            )}
            <p>
                {variance.fitted
                    ? t('marketdata.fxFeeds.start.variance.fitted', {
                          ...shape,
                          returns: String(variance.returns),
                      })
                    : t('marketdata.fxFeeds.start.variance.authored', shape)}
            </p>
            {variance.fitted && (
                <p>
                    {t('marketdata.fxFeeds.start.variance.firstDay', {
                        ratio: variance.initial_ratio.toFixed(2),
                    })}
                </p>
            )}
            {variance.note !== '' && <p>{variance.note}</p>}
        </div>
    );
}

export const percent = (value: number): string => `${(value * 100).toFixed(2)}%`;

/**
 * What the window's fixings did. Only the volatility changes the calibration today,
 * since it scales the mixture; the rest are recorded for a later policy.
 */
export function FeedStartMeasures({ start }: { readonly start: FeedStart }): ReactNode {
    const { t } = useTranslation();
    const recorded: string[] = [];
    if (start.annual_drift != null) {
        recorded.push(
            t('marketdata.fxFeeds.start.measures.drift', { value: percent(start.annual_drift) }),
        );
    }
    if (start.min_return != null && start.max_return != null) {
        recorded.push(
            t('marketdata.fxFeeds.start.measures.range', {
                min: percent(start.min_return),
                max: percent(start.max_return),
            }),
        );
    }
    if (start.age_days != null) {
        recorded.push(t('marketdata.fxFeeds.start.measures.age', { days: String(start.age_days) }));
    }
    if (start.jumps != null) {
        recorded.push(t('marketdata.fxFeeds.start.measures.jumps', { count: String(start.jumps) }));
    }
    for (const other of start.dispersion ?? []) {
        recorded.push(
            t('marketdata.fxFeeds.start.measures.dispersion', {
                provider: other.provider_code,
                bp: other.basis_points.toFixed(1),
            }),
        );
    }
    return (
        <div className="mt-2 space-y-1">
            {start.daily_spread != null && start.hourly_spread != null && (
                <p>
                    {t('marketdata.fxFeeds.start.measures.volatility', {
                        daily: percent(start.daily_spread),
                        hourly: percent(start.hourly_spread),
                    })}
                </p>
            )}
            {recorded.length > 0 && (
                <>
                    <p className="font-medium">{t('marketdata.fxFeeds.start.measures.recorded')}</p>
                    <ul className="list-disc pl-5">
                        {recorded.map((line) => (
                            <li key={line}>{line}</li>
                        ))}
                    </ul>
                </>
            )}
        </div>
    );
}

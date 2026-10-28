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

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import type { FeedRaster, MarketTrace } from '../../../api/client.js';
import { useTranslation } from '../../../i18n/Provider.js';
import { cx } from '../../../ui/Primitives.js';
import { assetClassColour } from '../../assetClassColour.js';
import { processKey, processName } from '../processSummary.js';
import { LAW_PARAM, PROCESS_PARAM } from './FeedsView.js';
import { LAWS } from './LawTag.js';
import { FactRows, MarketCard, MicroLabel } from './MarketCard.js';

/** One process the feeds draw their prices from, and how many feeds draw from it. */
export interface ProcessFamily {
    readonly key: string;
    readonly name: string;
    readonly assetClass: string;
    readonly feeds: number;
}

/** The processes of the running feeds, the most used first; a feed with no process is left out. */
export function processFamilies(
    raster: FeedRaster | undefined,
    t: (key: string) => string,
): readonly ProcessFamily[] {
    const families = new Map<string, ProcessFamily>();
    for (const feed of raster?.feeds ?? []) {
        if (feed.engine === '') {
            continue;
        }
        const key = processKey(feed);
        const was = families.get(key);
        families.set(key, {
            key,
            name: was?.name ?? processName(t, feed),
            assetClass: feed.assetClass,
            feeds: (was?.feeds ?? 0) + 1,
        });
    }
    return [...families.values()].sort(
        (left, right) => right.feeds - left.feeds || left.name.localeCompare(right.name),
    );
}

/**
 * The market's families at a glance: how many feeds draw their ticks from each law, and their
 * prices from each process. Each card opens the Feeds tab narrowed to its feeds, where each
 * feed's own numbers are.
 */
export function FamiliesView({
    trace,
    raster,
    feedsLink,
}: {
    readonly trace: MarketTrace | undefined;
    readonly raster: FeedRaster | undefined;
    /** The address of the Feeds tab narrowed to one law or one process. */
    readonly feedsLink: (param: string, value: string) => string;
}): ReactNode {
    const { t } = useTranslation();
    const feeds = trace?.feeds ?? [];
    const processes = processFamilies(raster, t);
    return (
        <div className="grid gap-4">
            <div className="grid gap-4 md:grid-cols-3">
                {LAWS.map((law) => {
                    const count = feeds.filter((feed) => feed.mode === law).length;
                    return (
                        <MarketCard
                            key={law}
                            title={t(`marketdata.marketRegime.modes.${law}`)}
                            lead={t(`marketdata.market.laws.families.${law}.reads`)}
                            action={
                                <span className="flex items-baseline gap-1.5">
                                    <span className="font-mono text-lg font-bold">{count}</span>
                                    <MicroLabel>{t('marketdata.market.laws.feeds')}</MicroLabel>
                                </span>
                            }
                        >
                            <FactRows
                                rows={[
                                    [
                                        t('marketdata.market.laws.intensity'),
                                        t(`marketdata.market.laws.families.${law}.formula`),
                                    ],
                                    [
                                        t('marketdata.market.laws.stableWhen'),
                                        t(`marketdata.market.laws.families.${law}.stable`),
                                    ],
                                ]}
                            />
                            {count === 0 ? null : (
                                <Link
                                    to={feedsLink(LAW_PARAM, law)}
                                    className="text-xs text-accent hover:underline"
                                >
                                    {t('marketdata.market.families.show')}
                                </Link>
                            )}
                        </MarketCard>
                    );
                })}
            </div>

            <MarketCard
                glyph="laws"
                title={t('marketdata.market.families.processes.title')}
                lead={t('marketdata.market.families.processes.lead')}
            >
                {raster === undefined ? (
                    <p className="text-[13px] text-ink-muted">{t('common.loading')}</p>
                ) : processes.length === 0 ? (
                    <p className="text-[13px] text-ink-faint">{t('marketdata.market.laws.none')}</p>
                ) : (
                    <div className="grid gap-2.5 sm:grid-cols-2 lg:grid-cols-4">
                        {processes.map((family) => (
                            <Link
                                key={family.key}
                                to={feedsLink(PROCESS_PARAM, family.key)}
                                className="grid gap-1 rounded-card border border-line-subtle bg-bg-primary px-3 py-2 hover:border-accent/50"
                            >
                                <span className="flex items-center gap-2">
                                    <span
                                        className={cx(
                                            'size-[7px] shrink-0 rounded-sm',
                                            assetClassColour(family.assetClass).dot,
                                        )}
                                    />
                                    <MicroLabel>{family.name}</MicroLabel>
                                </span>
                                <span className="flex items-baseline gap-1.5">
                                    <span className="font-mono text-lg font-bold">
                                        {family.feeds}
                                    </span>
                                    <MicroLabel>{t('marketdata.market.laws.feeds')}</MicroLabel>
                                </span>
                            </Link>
                        ))}
                    </div>
                )}
            </MarketCard>
        </div>
    );
}

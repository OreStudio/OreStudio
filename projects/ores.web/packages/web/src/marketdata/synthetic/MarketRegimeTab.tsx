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

import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { api, type FeedRaster, type MarketRegime, type RegimeMode } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { Button, Fact, Notice, Tag } from '../../ui/Primitives.js';
import { arrivalPositions, multiplierText, RASTER_SECONDS } from './marketRaster.js';
import { REGIME_EVERY_MS, REGIME_KEY, stateName } from './useMarketRegime.js';

const RASTER_KEY = ['market-regime-raster'] as const;

const MODES: readonly RegimeMode[] = ['auto', 'calm', 'active'];

/** A lane's height in pixels: a row of the raster, and the room a mark takes. */
const LANE_HEIGHT = 18;

/**
 * The market-wide arrival factor: the state every feed's rate is scaled by, the
 * switch that forces it, and the arrivals of every running feed on one raster.
 *
 * The factor belongs to the run, so this is the whole market's control and not a
 * feed's: it sets the state and reads the feeds. It never edits a feed's own
 * clustering, which is that feed's Arrivals tab, so the two views never do the
 * same job.
 */
export function MarketRegimeTab(): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const regime = useQuery({
        queryKey: REGIME_KEY,
        queryFn: () => api.marketRegime(),
        refetchInterval: REGIME_EVERY_MS,
    });
    const raster = useQuery({
        queryKey: RASTER_KEY,
        queryFn: () => api.feedRaster({ seconds: RASTER_SECONDS }),
        refetchInterval: REGIME_EVERY_MS,
    });
    const pin = useMutation({
        mutationFn: (mode: RegimeMode) => api.pinMarketRegime(mode),
        onSuccess: (after: MarketRegime) => {
            queries.setQueryData(REGIME_KEY, after);
        },
    });

    if (regime.data === undefined) {
        return regime.error === null ? (
            <p className="text-sm text-ink-muted">{t('common.loading')}</p>
        ) : (
            <Notice tone="error">{regime.error.message}</Notice>
        );
    }
    return (
        <MarketRegimePanel
            regime={regime.data}
            raster={raster.data}
            now={Date.now()}
            pending={pin.isPending ? pin.variables : undefined}
            error={pin.error === null ? null : pin.error.message}
            onPin={(mode) => pin.mutate(mode)}
        />
    );
}

/**
 * The market's state, the switch and the raster, drawn from what the service
 * says. It is given its data so a test renders the same picture without a
 * browser, and so the connected tab holds no rendering of its own.
 */
export function MarketRegimePanel({
    regime,
    raster,
    now,
    pending,
    error,
    onPin,
}: {
    readonly regime: MarketRegime;
    readonly raster: FeedRaster | undefined;
    readonly now: number;
    readonly pending: RegimeMode | undefined;
    readonly error: string | null;
    readonly onPin: (mode: RegimeMode) => void;
}): ReactNode {
    const { t } = useTranslation();
    const forced = regime.pinned ? regime.state : 'auto';
    return (
        <section className="space-y-5" aria-label={t('marketdata.marketRegime.title')}>
            <p className="text-sm text-ink-muted">{t('marketdata.marketRegime.lead')}</p>

            <dl className="grid gap-4 sm:grid-cols-3">
                <Fact label={t('marketdata.marketRegime.state')}>
                    <Tag tone={regime.state === 'active' ? 'accent' : 'neutral'}>
                        {stateName(regime.state, t)}
                    </Tag>
                </Fact>
                <Fact
                    label={t('marketdata.marketRegime.multiplier')}
                    value={multiplierText(regime.multiplier)}
                />
                <Fact
                    label={t('marketdata.marketRegime.heldBy')}
                    value={
                        regime.pinned
                            ? t('marketdata.marketRegime.pinned')
                            : t('marketdata.marketRegime.chain')
                    }
                />
            </dl>

            {error === null ? null : <Notice tone="error">{error}</Notice>}

            <div className="flex flex-wrap items-center gap-2">
                <span className="text-xs uppercase tracking-wide text-ink-faint">
                    {t('marketdata.marketRegime.force')}
                </span>
                {MODES.map((mode) => (
                    <Button
                        key={mode}
                        variant={forced === mode ? 'primary' : 'secondary'}
                        size="sm"
                        aria-pressed={forced === mode}
                        pending={pending === mode}
                        disabled={pending !== undefined}
                        onClick={() => onPin(mode)}
                    >
                        {t(`marketdata.marketRegime.set.${mode}`)}
                    </Button>
                ))}
            </div>

            <div className="space-y-2">
                <h3 className="text-xs uppercase tracking-wide text-ink-faint">
                    {t('marketdata.marketRegime.raster.title', {
                        seconds: raster?.seconds ?? RASTER_SECONDS,
                    })}
                </h3>
                <MarketRaster
                    feeds={raster?.feeds ?? []}
                    seconds={raster?.seconds ?? RASTER_SECONDS}
                    now={now}
                />
            </div>
        </section>
    );
}

/**
 * One row for each running feed, and a mark for each tick it published.
 *
 * The rows are the feeds the service says run, in the order it names them, so the
 * picture follows the run rather than a list on the screen. It draws times and
 * not prices: what a market-wide factor changes is when ticks arrive.
 *
 * A row is a name and a track, drawn as one because a picture of a thousand
 * marks is not one a person reads on a canvas: the name stays text, and each
 * mark is a place on a track the browser can lay out.
 */
export function MarketRaster({
    feeds,
    seconds,
    now,
}: {
    readonly feeds: FeedRaster['feeds'];
    /** The window the rows were read over, which is what the marks are placed in. */
    readonly seconds: number;
    readonly now: number;
}): ReactNode {
    const { t } = useTranslation();
    if (feeds.length === 0) {
        return <Notice tone="info">{t('marketdata.marketRegime.raster.none')}</Notice>;
    }
    return (
        <div
            role="img"
            aria-label={t('marketdata.marketRegime.raster.label')}
            className="rounded-md border border-line bg-surface-base py-1"
        >
            {feeds.map((feed) => (
                <div
                    key={feed.sourceName}
                    className="flex items-center gap-3 px-2"
                    style={{ height: LANE_HEIGHT }}
                >
                    <span className="w-56 shrink-0 truncate font-mono text-[10px] text-ink-muted">
                        {feed.sourceName}
                    </span>
                    <span className="relative h-2 flex-1 rounded-sm bg-line/40">
                        {arrivalPositions(feed.ticks, seconds, now).map((position, index) => (
                            <span
                                key={index}
                                data-mark
                                className="absolute inset-y-0 w-px bg-accent"
                                style={{ left: `${String(position * 100)}%` }}
                            />
                        ))}
                    </span>
                </div>
            ))}
        </div>
    );
}

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

import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { useSearchParams } from 'react-router';
import {
    api,
    type HistoryResolution,
    type MarketRegime,
    type MarketSettings,
    type RegimeMode,
} from '../../../api/client.js';
import { useTranslation } from '../../../i18n/Provider.js';
import { useSession } from '../../../session/SessionProvider.js';
import { Notice } from '../../../ui/Primitives.js';
import { tabAddress, useTabs } from '../../../ui/Tabs.js';
import { REGIME_KEY, useMarketRegimeQuery, useMarketTrace } from '../useMarketRegime.js';
import { FamiliesView } from './FamiliesView.js';
import { feedPages } from './FeedLink.js';
import { FeedsView, type ArrivalWindow } from './FeedsView.js';
import { FitView } from './FitView.js';
import { FoldCard } from './FoldCard.js';
import { Glyph, type GlyphName } from './MarketCard.js';
import { MarketStrip } from './MarketStrip.js';
import { NowView } from './NowView.js';
import { SettingsView } from './SettingsView.js';
import { TraceView } from './TraceView.js';

/** The market's own tabs, one question each. */
export const VIEWS = ['now', 'settings', 'arrivals', 'feeds', 'fit', 'trace'] as const;
export type MarketViewName = (typeof VIEWS)[number];

const GLYPHS: Readonly<Record<MarketViewName, GlyphName>> = {
    now: 'pace',
    settings: 'settings',
    arrivals: 'arrivals',
    feeds: 'laws',
    fit: 'fit',
    trace: 'log',
};

/** How far back Now's seen ticks go, in seconds: its window, and a little more. */
const RECENT_SECONDS = 150;

/** How often the feed list and its ticks are read again, in milliseconds. */
const RASTER_EVERY_MS = 4_000;

/** The Synthetic screen's tab this one lives in, which a change of the market's tab keeps. */
const OUTER = { tab: 'regime' } as const;

/**
 * The market: one clock for every feed of the installation, as six tabs under one strip of
 * figures. Arrivals reads the market whole; Feeds reads it a feed to a row. A system session forces the state and sets what moves the market; a tenant reads
 * the same tabs.
 */
export function MarketTab(): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [, setSearch] = useSearchParams();
    const [arrivalWindow, setArrivalWindow] = useState<ArrivalWindow>(120);
    const session = useSession();
    // The market is the installation's, so only a system session sets it; a tenant reads it.
    const mayChange =
        session.state.status === 'authenticated' &&
        session.state.session.mode === 'system-administration';
    const regime = useMarketRegimeQuery();
    const trace = useMarketTrace();
    const raster = useQuery({
        queryKey: ['market-regime-raster', arrivalWindow],
        queryFn: () => api.feedRaster({ seconds: arrivalWindow }),
        refetchInterval: RASTER_EVERY_MS,
    });
    const [resolution, setResolution] = useState<HistoryResolution>('second');
    const pin = useMutation({
        mutationFn: (mode: RegimeMode) => api.pinMarketRegime(mode),
        onSuccess: (after: MarketRegime) => queries.setQueryData(REGIME_KEY, after),
    });
    const configure = useMutation({
        mutationFn: (settings: MarketSettings) => api.setMarketSettings(settings),
        onSuccess: (after: MarketRegime) => queries.setQueryData(REGIME_KEY, after),
    });
    const { tab: view, bar } = useTabs({
        label: t('marketdata.market.title'),
        tabs: VIEWS,
        param: 'view',
        keep: OUTER,
        titleOf: (name) => t(`marketdata.market.views.${name}.title`),
        renderTitle: (name, selected) => (
            <span className="flex items-center gap-2">
                <Glyph name={GLYPHS[name as MarketViewName]} />
                {t(`marketdata.market.views.${name}.title`)}
                <span
                    className={
                        selected
                            ? 'hidden text-xs text-accent/70 lg:inline'
                            : 'hidden text-xs text-ink-faint lg:inline'
                    }
                >
                    {t(`marketdata.market.views.${name}.question`)}
                </span>
            </span>
        ),
    });

    // The last couple of minutes for Now's seen ticks; the hour only while the fold shows it.
    const recent = useQuery({
        queryKey: ['market-history', 'recent'],
        queryFn: () => api.marketHistory('second', RECENT_SECONDS),
        refetchInterval: RASTER_EVERY_MS,
        enabled: view === 'now',
    });
    const folded = useQuery({
        queryKey: ['market-history', resolution],
        queryFn: () => api.marketHistory(resolution),
        refetchInterval: RASTER_EVERY_MS,
        enabled: view === 'arrivals',
    });

    if (regime.data === undefined) {
        return regime.error === null ? (
            <p className="text-sm text-ink-muted">{t('common.loading')}</p>
        ) : (
            <Notice tone="error">{regime.error.message}</Notice>
        );
    }
    const now = Date.now();
    const pages = feedPages(raster.data);
    return (
        <section className="grid gap-4" aria-label={t('marketdata.market.title')}>
            <MarketStrip regime={regime.data} trace={trace.data} />
            {bar}
            {view === 'now' ? (
                <NowView
                    regime={regime.data}
                    trace={trace.data}
                    history={recent.data}
                    now={now}
                    mayPin={mayChange}
                    pending={pin.isPending ? pin.variables : undefined}
                    error={pin.error === null ? null : pin.error.message}
                    onPin={(mode) => pin.mutate(mode)}
                    pages={pages}
                    onOpenTrace={() =>
                        setSearch(tabAddress(VIEWS, 'trace', 'view', OUTER), { replace: true })
                    }
                />
            ) : null}
            {view === 'settings' ? (
                <SettingsView
                    regime={regime.data}
                    mayChange={mayChange}
                    pending={configure.isPending}
                    error={configure.error === null ? null : configure.error.message}
                    onApply={(settings) => configure.mutate(settings)}
                />
            ) : null}
            {view === 'arrivals' ? (
                <FoldCard
                    history={folded.data}
                    resolution={resolution}
                    onResolution={setResolution}
                    now={now}
                />
            ) : null}
            {view === 'arrivals' ? (
                <FamiliesView
                    trace={trace.data}
                    raster={raster.data}
                    feedsLink={(param, value) =>
                        `?${new URLSearchParams({
                            ...tabAddress(VIEWS, 'feeds', 'view', OUTER),
                            [param]: value,
                        }).toString()}`
                    }
                />
            ) : null}
            {view === 'feeds' ? (
                <FeedsView
                    raster={raster.data}
                    trace={trace.data}
                    now={now}
                    window={arrivalWindow}
                    onWindow={setArrivalWindow}
                    pages={pages}
                />
            ) : null}
            {view === 'fit' ? (
                <FitView regime={regime.data} trace={trace.data} pages={pages} />
            ) : null}
            {view === 'trace' ? <TraceView trace={trace.data} pages={pages} /> : null}
        </section>
    );
}

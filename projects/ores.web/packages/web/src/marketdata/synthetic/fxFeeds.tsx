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
import { api } from '../../api/client.js';
import { PATHS } from '../../shell/paths.js';
import { useTranslation } from '../../i18n/Provider.js';
import { OresmdUrl } from '../../oresmd/OresmdUrl.js';
import { show, type FieldSpec, type FieldValues } from '../../refdata/records.js';
import { syntheticStore } from '../../refdata/store.js';
import { FeedPage } from './FeedScreens.js';
import { feedPath, type FeedKind } from './feedKind.js';
import { COMPONENTS, MixturePanel } from './MixturePanel.js';
import { ArrivalsTab } from './ArrivalsPanel.js';
import { TicksPanel } from './TicksPanel.js';

const FEEDS = 'fx-feeds';

const PROCESS_TYPES = ['geometric', 'arithmetic', 'ornstein_uhlenbeck'];
const ARRIVAL_MODES = ['periodic', 'poisson', 'hawkes'] as const;

const PRICE_SOURCES = ['fixed', 'vintage'];

/**
 * The feed's own fields. The ORE key is derived from the pair and is not asked
 * for; the initial price is asked for only when the price is fixed, and the
 * vintage only when the price comes from one.
 */
const FEED_FIELDS: readonly FieldSpec[] = [
    {
        field: 'config_id',
        history: 'Config ID',
        kind: {
            kind: 'record',
            resource: 'collections',
            value: 'id',
            label: 'name',
            store: syntheticStore,
        },
        fixed: true,
    },
    {
        field: 'base_currency_code',
        history: 'Base Currency Code',
        kind: { kind: 'record', resource: 'currencies', value: 'iso_code', label: 'name' },
        flag: 'currency',
        fixed: true,
    },
    {
        field: 'quote_currency_code',
        history: 'Quote Currency Code',
        kind: { kind: 'record', resource: 'currencies', value: 'iso_code', label: 'name' },
        flag: 'currency',
        fixed: true,
    },
    {
        field: 'source_name',
        history: 'Source Name',
        kind: { kind: 'text', max: 200 },
        blank: true,
    },
    {
        field: 'process_type',
        history: 'Process Type',
        kind: { kind: 'choice', options: PROCESS_TYPES },
        help: 'marketdata.process.about',
    },
    {
        field: 'ticks_per_hour',
        history: 'Ticks Per Hour',
        kind: { kind: 'int', min: 1, max: 360000 },
        help: 'marketdata.fxFeeds.help.ticks_per_hour',
    },
    {
        field: 'arrival_mode',
        history: 'Arrival Mode',
        kind: { kind: 'choice', options: ARRIVAL_MODES },
        help: 'marketdata.fxFeeds.help.arrival_mode',
    },
    {
        field: 'branching_ratio',
        history: 'Branching Ratio',
        kind: { kind: 'decimal', below: 1 },
        help: 'marketdata.fxFeeds.help.branching_ratio',
    },
    {
        field: 'decay',
        history: 'Decay',
        kind: { kind: 'decimal', positive: true },
        help: 'marketdata.fxFeeds.help.decay',
    },
    {
        field: 'sensitivity',
        history: 'Sensitivity',
        kind: { kind: 'decimal', positive: true },
        help: 'marketdata.fxFeeds.help.sensitivity',
    },
    {
        field: 'price_source',
        history: 'Price Source',
        kind: { kind: 'choice', options: PRICE_SOURCES },
        help: 'marketdata.fxFeeds.help.price_source',
    },
    {
        field: 'gmm_initial_price',
        history: 'Gmm Initial Price',
        kind: { kind: 'decimal', positive: true },
        when: (values) => values['price_source'] === 'fixed',
    },
    {
        field: 'vintage_source',
        history: 'Vintage Source',
        kind: { kind: 'text', max: 200 },
        when: (values) => values['price_source'] === 'vintage',
    },
    {
        field: 'vintage_date',
        history: 'Vintage Date',
        kind: { kind: 'date' },
        when: (values) => values['price_source'] === 'vintage',
    },
    {
        field: 'enabled',
        history: 'Enabled',
        kind: { kind: 'bool' },
        help: 'marketdata.fxFeeds.help.enabled',
    },
    {
        field: 'auto_start',
        history: 'Auto Start',
        kind: { kind: 'bool' },
        help: 'marketdata.fxFeeds.help.auto_start',
    },
];

/** The fields the server needs that the form does not edit. */
const FEED_KEPT = ['folder_id'];

/**
 * The fields the edit dialog offers: the engine is changed on the Price
 * behaviour tab, where the mixture is converted with it.
 */
const FEED_EDIT_FIELDS = FEED_FIELDS.filter((spec) => spec.field !== 'process_type');

/** The fields a feed write carries, as the BFF takes them. */
const FEED_WRITE_FIELDS = [
    'id',
    'config_id',
    'base_currency_code',
    'quote_currency_code',
    'source_name',
    'oresmd_uri',
    'price_source',
    'gmm_initial_price',
    'ticks_per_hour',
    'process_type',
    'enabled',
    'auto_start',
    'vintage_source',
    'vintage_date',
    'folder_id',
] as const;

/** The feed as written back with a change, such as a new engine. */
export function feedWrite(
    feed: Readonly<Record<string, unknown>>,
    change: Readonly<Record<string, unknown>>,
): Record<string, unknown> {
    return {
        ...Object.fromEntries(FEED_WRITE_FIELDS.map((field) => [field, feed[field] ?? null])),
        ...change,
    };
}

/** A component's fields, for its revert; the mixture table edits the rest. */
const COMPONENT_FIELDS: readonly FieldSpec[] = [
    { field: 'component_index', history: 'Component Index', kind: { kind: 'int', min: 0 } },
    { field: 'description', history: 'Description', kind: { kind: 'text' }, blank: true },
    { field: 'mean', history: 'Mean', kind: { kind: 'decimal' } },
    { field: 'stdev', history: 'Stdev', kind: { kind: 'decimal' } },
    { field: 'weight', history: 'Weight', kind: { kind: 'decimal' } },
];

const NEW_FEED: FieldValues = {
    process_type: 'geometric',
    ticks_per_hour: '3600',
    price_source: 'fixed',
    gmm_initial_price: '1',
    enabled: 'false',
    auto_start: 'false',
};

export function fxFeedPath(id?: string): string {
    return feedPath(FX_FEED_KIND, id);
}

/** The pair a feed quotes, as the pair screens write it. */
function pairOf(feed: Readonly<Record<string, unknown>>): string {
    return `${show(feed['base_currency_code'])}/${show(feed['quote_currency_code'])}`;
}

/** The synthetic mark every feed's source name starts with, so a simulated feed is never read as a real one. */
export const SYNTHETIC_MARK = 'synthetic.';

/**
 * A feed's source name: the pair under the synthetic mark when none is given,
 * and the given name under the mark when it lacks it. The table refuses a
 * source name without it.
 */
export function syntheticSource(given: string, base: string, quote: string): string {
    const name = given.trim() === '' ? `${base}${quote}`.toLowerCase() : given.trim();
    return name.startsWith(SYNTHETIC_MARK) ? name : `${SYNTHETIC_MARK}${name}`;
}

/**
 * The fields that follow from the others: the oresmd URI from the pair, a source
 * name under the synthetic mark, from the pair when none is given, and the price
 * fields the source does not use emptied, as the table requires.
 */
export function completeFeed(write: Readonly<Record<string, unknown>>): Record<string, unknown> {
    const base = show(write['base_currency_code']);
    const quote = show(write['quote_currency_code']);
    const fixed = write['price_source'] === 'fixed';
    return {
        ...write,
        oresmd_uri: `oresmd://fx/${base}?type=quote&instrument=fx_spot&quote=rate&ccy=${quote}`,
        source_name: syntheticSource(show(write['source_name']), base, quote),
        gmm_initial_price: fixed ? write['gmm_initial_price'] : 0,
        vintage_source: fixed ? '' : write['vintage_source'],
        vintage_date: fixed ? '' : write['vintage_date'],
    };
}

/** The rules across fields that the table checks, said before the save rather than after it. */
function useFeedCheck(): (values: FieldValues) => readonly string[] {
    const { t } = useTranslation();
    return (values) => {
        const base = values['base_currency_code'] ?? '';
        return base !== '' && base === values['quote_currency_code']
            ? [t('marketdata.fxFeeds.samePair')]
            : [];
    };
}

/** The FX price feeds, as the shared feed screens draw them. */
export const FX_FEED_KIND: FeedKind = {
    resource: FEEDS,
    entity: 'ores.synthetic.fx_spot_generation_config',
    historySubject: 'fx_feed',
    path: PATHS.fxFeed,
    words: 'fxFeeds',
    fields: FEED_FIELDS,
    editFields: FEED_EDIT_FIELDS,
    kept: FEED_KEPT,
    initial: NEW_FEED,
    complete: completeFeed,
    useCheck: useFeedCheck,
    nameOf: pairOf,
    flag: 'pair',
    detailOf: (feed, t) => t(`refdata.choices.process_type.${show(feed['process_type'])}`),
    extraDetails: (feed, t) => [
        [
            t('refdata.fields.oresmd_uri'),
            <OresmdUrl uri={show(feed['oresmd_uri'])} />,
            t('marketdata.feeds.targetUri'),
        ],
    ],
    parts: [
        {
            resource: COMPONENTS,
            entity: 'ores.synthetic.gmm_component',
            specs: COMPONENT_FIELDS,
            keep: ['id', 'fx_spot_config_id'],
        },
    ],
    tabs: [
        {
            name: 'behaviour',
            render: ({ feed, running, mayWrite }) => (
                <MixturePanel
                    feed={feed}
                    running={running === true}
                    mayWriteFeed={mayWrite}
                    saveEngine={async (engine, intent) => {
                        await api.saveSynthetic(
                            FEEDS,
                            [
                                {
                                    write: feedWrite(feed, { process_type: engine }),
                                    version: feed.version,
                                },
                            ],
                            intent,
                        );
                    }}
                />
            ),
        },
        {
            name: 'arrivals',
            render: ({ feed, running, mayWrite }) => (
                <ArrivalsTab
                    feed={feed}
                    mayWriteFeed={mayWrite}
                    running={running === true}
                    save={async (model, ratio, decay, sensitivity, intent) => {
                        await api.saveSynthetic(
                            FEEDS,
                            [
                                {
                                    write: feedWrite(feed, {
                                        arrival_mode: model,
                                        branching_ratio: ratio,
                                        decay,
                                        sensitivity,
                                    }),
                                    version: feed.version,
                                },
                            ],
                            intent,
                        );
                    }}
                />
            ),
        },
        {
            name: 'ticks',
            render: ({ feed, running, parts }) => (
                <TicksPanel feed={feed} running={running} mixture={parts[0] ?? []} />
            ),
        },
    ],
    afterAdd: 'behaviour',
};

/** One feed: its fields, the mixture its process draws from with what it implies, and its history. */
export function FxFeedPage(): ReactNode {
    return <FeedPage kind={FX_FEED_KIND} />;
}

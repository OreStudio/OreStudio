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

import { useState, type ReactNode } from 'react';
import { api, type RecordRow } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import {
    PartsPanel,
    RecordDetails,
    RecordDialog,
    show,
    useRecordPermissions,
    useRecords,
    type FieldSpec,
    type FieldValues,
} from '../../refdata/records.js';
import { syntheticStore } from '../../refdata/store.js';
import { PATHS } from '../../shell/paths.js';
import { Notice } from '../../ui/Primitives.js';
import { ArrivalsTab } from './ArrivalsPanel.js';
import { FeedPage } from './FeedScreens.js';
import { feedPath, type FeedKind } from './feedKind.js';
import { SYNTHETIC_MARK } from './fxFeeds.js';

export const VOL_FEEDS = 'fx-vol-surface-feeds';
const EXPIRIES = 'fx-vol-surface-feed-expiries';
const FEED_CONTRIBUTORS = 'fx-vol-surface-feed-contributors';
const CONTRIBUTORS = 'fx-vol-contributors';

const ARRIVAL_MODES = ['periodic', 'poisson', 'hawkes'] as const;

/**
 * The exponent of the SSVI power-law curvature, phi(theta) = eta * theta^-gamma. The
 * generator holds it at one half: there the surface is free of butterfly arbitrage
 * whenever eta stays below the square root of two, which the curvature driver keeps.
 * Any other value makes the conditions depend on each expiry's variance and on rho.
 */
export const SSVI_GAMMA = 0.5;

interface Bounds {
    readonly positive?: boolean;
    readonly above?: number;
    readonly below?: number;
    readonly min?: number;
    readonly max?: number;
}

/** A number a form asks for, inside the bounds the table checks. */
const number = (field: string, history: string, bounds: Bounds = {}): FieldSpec => ({
    field,
    history,
    kind: { kind: 'decimal', ...bounds },
});

/** The same, with its help, for the feed's own fields. */
/** Gamma as a page states it: its value and that it is fixed. */
export function gammaText(t: (key: string) => string): string {
    return `${String(SSVI_GAMMA)}, ${t('marketdata.volFeeds.surface.fixed')}`;
}

const decimal = (field: string, history: string, bounds: Bounds = {}): FieldSpec => ({
    ...number(field, history, bounds),
    help: `marketdata.volFeeds.help.${field}`,
});

/** Where the surface starts and what its skew and curvature revert to. */
const START_FIELDS: readonly FieldSpec[] = [
    decimal('start_skew', 'Start Skew', { above: -1, below: 1 }),
    decimal('start_curvature', 'Start Curvature', { positive: true, below: Math.SQRT2 }),
];

/** How fast the skew and the curvature revert, and how far they move. */
const DYNAMICS_FIELDS: readonly FieldSpec[] = [
    decimal('skew_reversion', 'Skew Reversion', { min: 0 }),
    decimal('skew_volatility', 'Skew Volatility', { min: 0 }),
    decimal('curvature_reversion', 'Curvature Reversion', { min: 0 }),
    decimal('curvature_volatility', 'Curvature Volatility', { min: 0 }),
];

/** How the surface's moves correlate with spot and with themselves. */
const CORRELATION_FIELDS: readonly FieldSpec[] = [
    decimal('spot_variance_correlation', 'Spot Variance Correlation', { above: -1, below: 1 }),
    decimal('spot_skew_correlation', 'Spot Skew Correlation', { above: -1, below: 1 }),
    decimal('segment_correlation', 'Segment Correlation', { min: 0, below: 1 }),
];

export const SURFACE_FIELDS = [...START_FIELDS, ...DYNAMICS_FIELDS, ...CORRELATION_FIELDS];

/** The feed's own fields: its pair and how it ticks, then its surface. */
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
    { field: 'source_name', history: 'Source Name', kind: { kind: 'text', max: 200 }, blank: true },
    {
        field: 'ticks_per_hour',
        history: 'Ticks Per Hour',
        kind: { kind: 'int', min: 1, max: 360000 },
        help: 'marketdata.volFeeds.help.ticks_per_hour',
    },
    {
        field: 'arrival_mode',
        history: 'Arrival Mode',
        kind: { kind: 'choice', options: ARRIVAL_MODES },
        help: 'marketdata.volFeeds.help.arrival_mode',
    },
    decimal('branching_ratio', 'Branching Ratio', { min: 0, below: 1 }),
    decimal('decay', 'Decay', { positive: true }),
    decimal('sensitivity', 'Sensitivity', { positive: true }),
    ...SURFACE_FIELDS,
    {
        field: 'enabled',
        history: 'Enabled',
        kind: { kind: 'bool' },
        help: 'marketdata.volFeeds.help.enabled',
    },
    {
        field: 'auto_start',
        history: 'Auto Start',
        kind: { kind: 'bool' },
        help: 'marketdata.volFeeds.help.auto_start',
    },
];

/** The fields the server needs that the form does not edit. */
const FEED_KEPT = ['folder_id', 'asset_class_code'];

const NEW_FEED: FieldValues = {
    ticks_per_hour: '12',
    arrival_mode: 'periodic',
    branching_ratio: '0',
    decay: '1',
    sensitivity: '1',
    start_skew: '-0.1',
    start_curvature: '0.8',
    skew_reversion: '3',
    skew_volatility: '0.6',
    curvature_reversion: '3',
    curvature_volatility: '0.4',
    spot_variance_correlation: '0.3',
    spot_skew_correlation: '-0.4',
    segment_correlation: '0.9',
    enabled: 'false',
    auto_start: 'false',
};

/** The pair a surface quotes, as the pair screens write it. */
function pairOf(feed: Readonly<Record<string, unknown>>): string {
    return `${show(feed['base_currency_code'])}/${show(feed['quote_currency_code'])}`;
}

/**
 * A surface's source name: `vol.` and the pair under the synthetic mark when none is
 * given, so it never takes the name of the pair's spot feed, and the given name under
 * the mark when it lacks it.
 */
export function volSource(given: string, base: string, quote: string): string {
    const name = given.trim() === '' ? `vol.${base}${quote}`.toLowerCase() : given.trim();
    return name.startsWith(SYNTHETIC_MARK) ? name : `${SYNTHETIC_MARK}${name}`;
}

export function completeVolFeed(write: Readonly<Record<string, unknown>>): Record<string, unknown> {
    return {
        ...write,
        asset_class_code: show(write['asset_class_code']) === '' ? 'fx' : write['asset_class_code'],
        source_name: volSource(
            show(write['source_name']),
            show(write['base_currency_code']),
            show(write['quote_currency_code']),
        ),
    };
}

function useVolFeedCheck(): (values: FieldValues) => readonly string[] {
    const { t } = useTranslation();
    return (values) => {
        const base = values['base_currency_code'] ?? '';
        return base !== '' && base === values['quote_currency_code']
            ? [t('marketdata.volFeeds.samePair')]
            : [];
    };
}

/** The fields of a feed a dialog does not edit, carried over as they stand. */
function keptBeside(specs: readonly FieldSpec[]): readonly string[] {
    return [
        'id',
        ...FEED_FIELDS.filter((spec) => !specs.includes(spec)).map((spec) => spec.field),
        ...FEED_KEPT,
    ];
}

const SURFACE_CARDS = [
    { name: 'start', specs: START_FIELDS },
    { name: 'dynamics', specs: DYNAMICS_FIELDS },
    { name: 'correlations', specs: CORRELATION_FIELDS },
] as const;

/**
 * The Surface tab: the SSVI state the feed evolves, as three cards each edited on its
 * own. Gamma is drawn on the start card and no dialog edits it.
 */
function SurfacePanel({
    feed,
    running,
    mayWrite,
}: {
    readonly feed: RecordRow;
    readonly running: boolean;
    readonly mayWrite: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const check = useVolFeedCheck();
    const [editing, setEditing] = useState<(typeof SURFACE_CARDS)[number] | undefined>();
    return (
        <div className="space-y-4">
            <p className="text-sm text-ink-muted">{t('marketdata.volFeeds.surface.lead')}</p>
            {running && <Notice tone="info">{t('marketdata.volFeeds.surface.running')}</Notice>}
            {SURFACE_CARDS.map((card) => (
                <RecordDetails
                    key={card.name}
                    specs={card.specs}
                    row={feed}
                    heading={{
                        title: t(`marketdata.volFeeds.surface.${card.name}`),
                        onEdit: mayWrite ? () => setEditing(card) : undefined,
                    }}
                    extra={
                        card.name === 'start'
                            ? [
                                  [
                                      t('marketdata.volFeeds.surface.gamma'),
                                      gammaText(t),
                                      t('marketdata.volFeeds.surface.gammaWhy'),
                                  ],
                              ]
                            : []
                    }
                />
            ))}
            {editing !== undefined && (
                <RecordDialog
                    title={t('marketdata.volFeeds.surface.editTitle', {
                        part: t(`marketdata.volFeeds.surface.${editing.name}`),
                        pair: pairOf(feed),
                    })}
                    resource={VOL_FEEDS}
                    specs={editing.specs}
                    row={feed}
                    keep={keptBeside(editing.specs)}
                    complete={completeVolFeed}
                    check={check}
                    onClose={() => setEditing(undefined)}
                />
            )}
        </div>
    );
}

/** How many days a tenor such as 1W or 18M spans, roughly, to order the ladder. */
export function tenorDays(tenor: string): number {
    const match = /^(\d+)([DWMY])$/.exec(tenor.trim().toUpperCase());
    if (match === null) {
        return Number.MAX_SAFE_INTEGER;
    }
    const days = { D: 1, W: 7, M: 30, Y: 365 }[match[2] as 'D' | 'W' | 'M' | 'Y'];
    return Number(match[1]) * days;
}

const EXPIRY_FIELDS: readonly FieldSpec[] = [
    {
        field: 'tenor',
        history: 'Tenor',
        kind: {
            kind: 'record',
            resource: 'tenors',
            value: 'code',
            label: 'code',
            store: syntheticStore,
        },
        fixed: true,
    },
    number('atm_vol', 'Atm Vol', { positive: true }),
    number('variance_reversion', 'Variance Reversion', { min: 0 }),
    number('variance_volatility', 'Variance Volatility', { min: 0 }),
];

const percent = (value: unknown): string => `${(Number(value) * 100).toFixed(2)}%`;

/** The Expiries tab: the ladder the surface is quoted along, as parts of the feed. */
function ExpiriesPanel({
    feed,
    expiries,
}: {
    readonly feed: RecordRow;
    readonly expiries: readonly RecordRow[];
}): ReactNode {
    const { t } = useTranslation();
    return (
        <PartsPanel
            resource={EXPIRIES}
            entityType="ores.synthetic.fx_vol_surface_feed_expiry"
            specs={EXPIRY_FIELDS}
            parentField="feed_id"
            parent={show(feed['id'])}
            rows={expiries}
            error={undefined}
            columns={[
                {
                    header: t('refdata.fields.tenor'),
                    cell: (row) => show(row['tenor']),
                    mono: true,
                },
                {
                    header: t('refdata.fields.atm_vol'),
                    cell: (row) => percent(row['atm_vol']),
                    mono: true,
                },
                {
                    header: t('refdata.fields.variance_reversion'),
                    cell: (row) => show(row['variance_reversion']),
                    mono: true,
                },
                {
                    header: t('refdata.fields.variance_volatility'),
                    cell: (row) => show(row['variance_volatility']),
                    mono: true,
                },
            ]}
            sort={(row) => String(tenorDays(show(row['tenor']))).padStart(16, '0')}
            nameOf={(row) => show(row['tenor'])}
            initial={{ atm_vol: '0.08', variance_reversion: '2', variance_volatility: '0.08' }}
            lead={t('marketdata.volFeeds.expiries.lead')}
            titles={{
                add: t('marketdata.volFeeds.expiries.add'),
                one: t('marketdata.volFeeds.expiries.one'),
                remove: t('marketdata.volFeeds.expiries.remove'),
            }}
        />
    );
}

const LINK_FIELDS: readonly FieldSpec[] = [
    {
        field: 'contributor_id',
        history: 'Contributor ID',
        kind: {
            kind: 'record',
            resource: CONTRIBUTORS,
            value: 'id',
            label: 'provider_code',
            store: syntheticStore,
        },
    },
];

const NOISE_FIELDS: readonly FieldSpec[] = [
    number('noise', 'Noise', { min: 0 }),
    number('wing_widening', 'Wing Widening', { min: 0 }),
    number('expiry_widening', 'Expiry Widening', { min: 0 }),
    number('quote_step', 'Quote Step', { min: 0 }),
    number('missing_probability', 'Missing Probability', { min: 0, max: 1 }),
    number('stale_probability', 'Stale Probability', { min: 0, max: 1 }),
    number('outlier_probability', 'Outlier Probability', { min: 0, max: 1 }),
    number('outlier_size', 'Outlier Size', { min: 0 }),
    number('bias', 'Bias'),
];

/**
 * The Contributors tab: which vendors quote the surface, and the noise each one adds.
 * A noise profile is the contributor's own, shared by every surface it quotes.
 */
function ContributorsPanel({
    feed,
    links,
}: {
    readonly feed: RecordRow;
    readonly links: readonly RecordRow[];
}): ReactNode {
    const { t } = useTranslation();
    const contributors = useRecords(CONTRIBUTORS);
    const may = useRecordPermissions(CONTRIBUTORS);
    const [editing, setEditing] = useState<RecordRow | undefined>();
    const byId = new Map((contributors.data ?? []).map((row) => [show(row['id']), row]));
    const providerOf = (link: RecordRow): string =>
        show(byId.get(show(link['contributor_id']))?.['provider_code']);
    const quoting = links
        .map((link) => byId.get(show(link['contributor_id'])))
        .filter((row): row is RecordRow => row !== undefined)
        .sort((left, right) =>
            show(left['provider_code']).localeCompare(show(right['provider_code'])),
        );
    return (
        <div className="space-y-4">
            <PartsPanel
                resource={FEED_CONTRIBUTORS}
                entityType="ores.synthetic.fx_vol_surface_feed_contributor"
                specs={LINK_FIELDS}
                parentField="feed_id"
                parent={show(feed['id'])}
                rows={links}
                error={undefined}
                columns={[
                    {
                        header: t('refdata.fields.provider_code'),
                        cell: providerOf,
                        mono: true,
                    },
                ]}
                sort={providerOf}
                nameOf={providerOf}
                lead={t('marketdata.volFeeds.contributors.lead')}
                titles={{
                    add: t('marketdata.volFeeds.contributors.add'),
                    one: t('marketdata.volFeeds.contributors.one'),
                    remove: t('marketdata.volFeeds.contributors.remove'),
                }}
            />
            {quoting.map((contributor) => (
                <RecordDetails
                    key={show(contributor['id'])}
                    specs={NOISE_FIELDS}
                    row={contributor}
                    heading={{
                        title: t('marketdata.volFeeds.contributors.noise', {
                            provider: show(contributor['provider_code']),
                        }),
                        onEdit: may.write ? () => setEditing(contributor) : undefined,
                    }}
                />
            ))}
            {editing !== undefined && (
                <RecordDialog
                    title={t('marketdata.volFeeds.contributors.noise', {
                        provider: show(editing['provider_code']),
                    })}
                    resource={CONTRIBUTORS}
                    specs={NOISE_FIELDS}
                    row={editing}
                    keep={['id', 'provider_code']}
                    onClose={() => setEditing(undefined)}
                />
            )}
        </div>
    );
}

/** The synthetic FX volatility surfaces, as the shared feed screens draw them. */
export const VOL_FEED_KIND: FeedKind = {
    resource: VOL_FEEDS,
    entity: 'ores.synthetic.fx_vol_surface_feed',
    historySubject: 'vol_feed',
    path: PATHS.volFeed,
    words: 'volFeeds',
    fields: FEED_FIELDS,
    editFields: FEED_FIELDS,
    kept: FEED_KEPT,
    initial: NEW_FEED,
    complete: completeVolFeed,
    useCheck: useVolFeedCheck,
    nameOf: pairOf,
    flag: 'pair',
    detailOf: (_feed, t) => t('marketdata.volFeeds.engine'),
    extraDetails: (_feed, t) => [
        [
            t('marketdata.volFeeds.surface.gamma'),
            gammaText(t),
            t('marketdata.volFeeds.surface.gammaWhy'),
        ],
    ],
    parts: [
        {
            resource: EXPIRIES,
            entity: 'ores.synthetic.fx_vol_surface_feed_expiry',
            specs: EXPIRY_FIELDS,
            keep: ['id', 'feed_id'],
        },
        {
            resource: FEED_CONTRIBUTORS,
            entity: 'ores.synthetic.fx_vol_surface_feed_contributor',
            specs: LINK_FIELDS,
            keep: ['id', 'feed_id'],
        },
    ],
    tabs: [
        {
            name: 'surface',
            render: ({ feed, running, mayWrite }) => (
                <SurfacePanel feed={feed} running={running === true} mayWrite={mayWrite} />
            ),
        },
        {
            name: 'expiries',
            render: ({ feed, parts }) => <ExpiriesPanel feed={feed} expiries={parts[0] ?? []} />,
        },
        {
            name: 'contributors',
            render: ({ feed, parts }) => <ContributorsPanel feed={feed} links={parts[1] ?? []} />,
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
                            VOL_FEEDS,
                            [
                                {
                                    write: completeVolFeed({
                                        ...Object.fromEntries(
                                            keptBeside([]).map((field) => [
                                                field,
                                                feed[field] ?? null,
                                            ]),
                                        ),
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
    ],
    afterAdd: 'expiries',
};

export function volFeedPath(id?: string): string {
    return feedPath(VOL_FEED_KIND, id);
}

/** One surface feed: its pair and ticks, its SSVI surface, its expiries and contributors, and its history. */
export function VolFeedPage(): ReactNode {
    return <FeedPage kind={VOL_FEED_KIND} />;
}

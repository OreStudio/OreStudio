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
import { Link } from 'react-router';
import { api, type RecordRow } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import {
    PartsPanel,
    RecordDetails,
    RecordDialog,
    labelOf,
    show,
    useRecord,
    useRecordPermissions,
    useRecords,
    type FieldSpec,
    type FieldValues,
} from '../../refdata/records.js';
import { syntheticStore } from '../../refdata/store.js';
import { PATHS } from '../../shell/paths.js';
import { Card } from '../../ui/Card.js';
import { Button, Notice } from '../../ui/Primitives.js';
import { ParameterTable } from '../calibration/VarianceCalibration.js';
import { ArrivalsTab } from './ArrivalsPanel.js';
import { FeedPage } from './FeedScreens.js';
import { feedPath, type FeedKind } from './feedKind.js';
import { FeedStatus } from './feedRuns.js';
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
        field: 'spot_provider_code',
        history: 'Spot Provider Code',
        kind: {
            kind: 'record',
            resource: 'collections',
            value: 'provider_code',
            label: 'name',
            store: syntheticStore,
        },
        help: 'marketdata.volFeeds.help.spot_provider_code',
    },
    {
        field: 'spot_oresmd_uri',
        history: 'Spot Oresmd Uri',
        kind: { kind: 'text', max: 500 },
        blank: true,
        help: 'marketdata.volFeeds.help.spot_oresmd_uri',
    },
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
    const { gamma: _gamma, ...stored } = write;
    return {
        ...stored,
        asset_class_code: show(write['asset_class_code']) === '' ? 'fx' : write['asset_class_code'],
        source_name: volSource(
            show(write['source_name']),
            show(write['base_currency_code']),
            show(write['quote_currency_code']),
        ),
        spot_oresmd_uri:
            show(write['spot_oresmd_uri']).trim() === ''
                ? spotUri(show(write['base_currency_code']), show(write['quote_currency_code']))
                : write['spot_oresmd_uri'],
    };
}

/** The oresmd URI of a pair's spot rate, as the spot feeds of the pair publish it. */
function spotUri(base: string, quote: string): string {
    return `oresmd://fx/${base}?type=quote&instrument=fx_spot&quote=rate&ccy=${quote}`;
}

/** One value a surface reads on every tick, and the feed that publishes it, if one does. */
export interface Upstream {
    readonly role: 'spot' | 'baseCurve' | 'quoteCurve';
    readonly currency: string;
    readonly path: string;
    readonly resource: string;
    readonly row: RecordRow | undefined;
    /** What a matching feed would be, said when none matches. */
    readonly wants: string;
}

/**
 * The feeds a surface reads: the spot its config names by URI and provider, and each
 * currency's discounting curve in the surface's own collection, which is a curve that is not a
 * projection curve and has no index tenor.
 */
export function upstreamsOf(
    feed: Readonly<Record<string, unknown>>,
    spots: readonly RecordRow[],
    collections: readonly RecordRow[],
    curves: readonly RecordRow[],
): readonly Upstream[] {
    const providerOf = new Map(
        collections.map((row) => [show(row['id']), show(row['provider_code'])] as const),
    );
    const uri = show(feed['spot_oresmd_uri']);
    const provider = show(feed['spot_provider_code']);
    const collection = show(feed['config_id']);
    // One URI and provider can name a spot in more than one readable collection; the service
    // reads the one in the surface's own collection first, and so does the screen.
    const spot = spots
        .filter(
            (row) =>
                show(row['oresmd_uri']) === uri &&
                providerOf.get(show(row['config_id'])) === provider,
        )
        .sort(
            (left, right) =>
                Number(show(right['config_id']) === collection) -
                Number(show(left['config_id']) === collection),
        )[0];
    const curveUpstream = (role: 'baseCurve' | 'quoteCurve', currency: string): Upstream => ({
        role,
        currency,
        path: PATHS.irFeed,
        resource: 'ir-curve-feeds',
        row: curves.find(
            (row) =>
                show(row['config_id']) === collection &&
                show(row['currency_code']) === currency &&
                show(row['role']) !== 'projection' &&
                show(row['tenor']) === '',
        ),
        wants: currency,
    });
    return [
        {
            role: 'spot',
            currency: '',
            path: PATHS.fxFeed,
            resource: 'fx-feeds',
            row: spot,
            wants: `${uri} from ${provider}`,
        },
        curveUpstream('baseCurve', show(feed['base_currency_code'])),
        curveUpstream('quoteCurve', show(feed['quote_currency_code'])),
    ];
}

/** Whether one upstream feed runs, as its own record says. */
function UpstreamState({ upstream }: { readonly upstream: Upstream }): ReactNode {
    const record = useRecord(upstream.resource, show(upstream.row?.['id']));
    const running = record.data?.['running'];
    return <FeedStatus running={typeof running === 'boolean' ? running : undefined} />;
}

/**
 * A surface's Calibration tab: the feeds it reads, each linked to its own page, where its
 * start and its calibration are shown. The surface has no calibration of its own to repeat.
 */
function VolUpstreams({
    feed,
    running,
}: {
    readonly feed: RecordRow;
    readonly running: boolean | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const spots = useRecords('fx-feeds');
    const collections = useRecords('collections');
    const curves = useRecords('ir-curve-feeds');
    const upstreams = upstreamsOf(
        feed,
        spots.data ?? [],
        collections.data ?? [],
        curves.data ?? [],
    );
    return (
        <div className="space-y-4">
            {running !== true && (
                <Notice tone="info">{t('marketdata.volFeeds.upstreams.notRunning')}</Notice>
            )}
            <Card title={t('marketdata.volFeeds.upstreams.title')}>
                <p className="text-sm text-ink-muted">{t('marketdata.volFeeds.upstreams.intro')}</p>
                <table className="w-full text-left text-sm">
                    <tbody>
                        {upstreams.map((upstream) => (
                            <tr key={upstream.role} className="border-t border-line">
                                <td className="py-2 pr-4 text-ink-muted">
                                    {t(`marketdata.volFeeds.upstreams.${upstream.role}`, {
                                        currency: upstream.currency,
                                    })}
                                </td>
                                <td className="py-2 pr-4">
                                    {upstream.row === undefined ? (
                                        <span className="text-ink-muted">
                                            {t('marketdata.volFeeds.upstreams.missing', {
                                                wants: upstream.wants,
                                            })}
                                        </span>
                                    ) : (
                                        <Link
                                            to={`${upstream.path}/${encodeURIComponent(show(upstream.row['id']))}`}
                                            className="text-accent hover:underline"
                                        >
                                            {show(upstream.row['source_name'])}
                                        </Link>
                                    )}
                                </td>
                                <td className="py-2">
                                    {upstream.row !== undefined && (
                                        <UpstreamState upstream={upstream} />
                                    )}
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </Card>
        </div>
    );
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
/**
 * The gamma of the SSVI power law, shown in the start card's edit dialog and never written: the
 * generator holds it at one half, the only value its arbitrage bounds hold at.
 */
const GAMMA_SPEC: FieldSpec = {
    field: 'gamma',
    history: 'Gamma',
    kind: { kind: 'decimal' },
    fixed: true,
    labelKey: 'marketdata.volFeeds.surface.gamma',
    help: 'marketdata.volFeeds.surface.gammaWhy',
};

/**
 * The surface's settings as the FX calibration screen draws a run: three cards side by side,
 * each a table of its numbers, each with its own Edit for those numbers alone.
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
    const starts = editing?.name === 'start';
    return (
        <div className="space-y-4">
            <p className="text-sm text-ink-muted">{t('marketdata.volFeeds.surface.lead')}</p>
            {running && <Notice tone="info">{t('marketdata.volFeeds.surface.running')}</Notice>}
            <div className="grid gap-4 lg:grid-cols-3">
                {SURFACE_CARDS.map((card) => (
                    <Card
                        key={card.name}
                        title={t(`marketdata.volFeeds.surface.${card.name}`)}
                        aside={
                            mayWrite ? (
                                <Button
                                    size="sm"
                                    variant="ghost"
                                    icon="edit"
                                    onClick={() => setEditing(card)}
                                >
                                    {t('refdata.records.edit')}
                                </Button>
                            ) : undefined
                        }
                    >
                        <ParameterTable
                            rows={[
                                ...card.specs.map((spec) => ({
                                    label: labelOf(t, spec),
                                    value: show(feed[spec.field]),
                                })),
                                ...(card.name === 'start'
                                    ? [
                                          {
                                              label: t('marketdata.volFeeds.surface.gamma'),
                                              value: String(SSVI_GAMMA),
                                          },
                                      ]
                                    : []),
                            ]}
                        />
                        {card.name === 'start' && (
                            <p className="text-xs text-ink-muted">
                                {t('marketdata.volFeeds.surface.gammaWhy')}
                            </p>
                        )}
                    </Card>
                ))}
            </div>
            {editing !== undefined && (
                <RecordDialog
                    title={t('marketdata.volFeeds.surface.editTitle', {
                        part: t(`marketdata.volFeeds.surface.${editing.name}`),
                        pair: pairOf(feed),
                    })}
                    resource={VOL_FEEDS}
                    specs={starts ? [...editing.specs, GAMMA_SPEC] : editing.specs}
                    row={feed}
                    {...(starts ? { initial: { gamma: String(SSVI_GAMMA) } } : {})}
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

export const NOISE_FIELDS: readonly FieldSpec[] = [
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
/** A contributor's own page, which every surface it quotes links to. */
export function volContributorPath(id: string): string {
    return `${PATHS.volContributor}/${encodeURIComponent(id)}`;
}

/**
 * The vendors that quote the surface, as a list. A row opens the contributor's own page, where
 * its noise profile is read and edited; the profile is shared by every surface it quotes.
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
    const byId = new Map((contributors.data ?? []).map((row) => [show(row['id']), row]));
    const of = (link: RecordRow): RecordRow | undefined => byId.get(show(link['contributor_id']));
    const providerOf = (link: RecordRow): string => show(of(link)?.['provider_code']);
    return (
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
                { header: t('refdata.fields.noise'), cell: (link) => show(of(link)?.['noise']) },
                { header: t('refdata.fields.bias'), cell: (link) => show(of(link)?.['bias']) },
            ]}
            sort={providerOf}
            nameOf={providerOf}
            pathOf={(link) => volContributorPath(show(link['contributor_id']))}
            lead={t('marketdata.volFeeds.contributors.lead')}
            titles={{
                add: t('marketdata.volFeeds.contributors.add'),
                one: t('marketdata.volFeeds.contributors.one'),
                remove: t('marketdata.volFeeds.contributors.remove'),
            }}
        />
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
            String(SSVI_GAMMA),
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
    heldByProvider: true,
    calibration: ({ feed, running }) => <VolUpstreams feed={feed} running={running} />,
};

export function volFeedPath(id?: string): string {
    return feedPath(VOL_FEED_KIND, id);
}

/** One surface feed: its pair and ticks, its SSVI surface, its expiries and contributors, and its history. */
export function VolFeedPage(): ReactNode {
    return <FeedPage kind={VOL_FEED_KIND} />;
}

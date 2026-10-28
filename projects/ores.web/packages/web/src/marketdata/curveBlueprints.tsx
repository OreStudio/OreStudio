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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link, useNavigate, useParams, useSearchParams } from 'react-router';
import type { HistoryVersion } from '@ores/wire-protocol/browser';
import { api, type PublishedCurveSet, type RecordRow } from '../api/client.js';
import { curves } from '../api/curves.js';
import { tenors } from '../api/tenors.js';
import { useTranslation } from '../i18n/Provider.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { curveBlueprintPath, publishedCurvePath } from '../shell/paths.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { useLoadState } from '../events/useLoadState.js';
import { Button, Field, Input, Notice, Tag } from '../ui/Primitives.js';
import { Label } from '../ui/Label.js';
import { RelativeTime } from '../ui/Time.js';
import {
    RecordList,
    useRecordSource,
    type ListFilter,
    type ListSource,
} from '../refdata/RecordList.js';
import { CodedValue, Crumbs, useLabelCatalogue } from '../refdata/shared.js';
import { LineChart } from './LineChart.js';
import {
    PartsPanel,
    RecordDetails,
    RecordDialog,
    RecordGate,
    RecordHeader,
    RemoveRecordDialog,
    RevertDialog,
    show,
    useRecord,
    useRecordPermissions,
    useRecordTabs,
    useRegistry,
    type FieldSpec,
} from '../refdata/records.js';
import { RecordTimeline } from '../refdata/RecordTimeline.js';
import { useRecordStore } from '../refdata/store.js';
import {
    CURVE_FAMILY_ROLES,
    INTERPOLATION_METHODS,
    PILLAR_ROLES,
    TWO_SEGMENT_METHOD,
    buildPoints,
    checkRecipe,
    type CurvePoint,
    findingsOf,
    inMaturityOrder,
    tenorRank,
    unfilledPillars,
} from './curveBlueprintCheck.js';

/** The two record kinds a curve blueprint is made of. */
const CONFIGS = 'ir-curve-bootstrap-configs';
const PILLARS = 'ir-curve-bootstrap-pillars';

/** The entity types the generic history request is read under. */
const CONFIG_ENTITY = 'ores.refdata.ir_curve_bootstrap_config';
const PILLAR_ENTITY = 'ores.refdata.ir_curve_bootstrap_pillar';

/** The most sets one blueprint tab reads: the list is the server's page, not this screen's. */
const SETS_READ = 200;

/** The fields of one pillar: what it covers and what it bootstraps as. */
const PILLAR_FIELDS: readonly FieldSpec[] = [
    {
        field: 'start_tenor_code',
        history: 'Start Tenor Code',
        kind: { kind: 'pick', choices: [] },
        means: { key: 'marketdata.curveBlueprints.fields.startTenorMeans' },
    },
    {
        field: 'end_tenor_code',
        history: 'End Tenor Code',
        kind: { kind: 'pick', choices: [] },
        means: { key: 'marketdata.curveBlueprints.fields.endTenorMeans' },
    },
    {
        field: 'curve_role_code',
        history: 'Curve Role Code',
        kind: { kind: 'classification', list: 'curve-role' },
        means: {
            key: 'marketdata.curveBlueprints.fields.curveRoleMeans',
            values: [...PILLAR_ROLES],
        },
    },
];

/** The fields of a blueprint, in the order the field table reads them. */
function blueprintFields(inputs: {
    readonly tenors: readonly { readonly value: string; readonly label: string }[];
    readonly conventions: readonly { readonly value: string; readonly label: string }[];
    readonly fundings: readonly { readonly value: string; readonly label: string }[];
}): readonly FieldSpec[] {
    return [
        {
            field: 'currency_code',
            history: 'Currency Code',
            kind: { kind: 'record', resource: 'currencies', value: 'iso_code', label: 'name' },
            flag: 'currency',
            means: { key: 'marketdata.curveBlueprints.fields.currencyMeans' },
        },
        {
            field: 'index_tenor',
            history: 'Index Tenor',
            kind: { kind: 'pick', choices: inputs.tenors },
            means: { key: 'marketdata.curveBlueprints.fields.indexTenorMeans' },
        },
        {
            field: 'curve_family_role',
            history: 'Curve Family Role',
            kind: { kind: 'choice', options: [...CURVE_FAMILY_ROLES] },
            means: {
                key: 'marketdata.curveBlueprints.fields.roleMeans',
                values: [...CURVE_FAMILY_ROLES],
            },
        },
        {
            field: 'discount_curve_config_id',
            history: 'Discount Curve Config Id',
            kind: { kind: 'pick', choices: inputs.fundings },
            when: (values) => values['curve_family_role'] === 'PROJECTION',
            optional: true,
            pathOf: (value) => curveBlueprintPath(value),
            means: { key: 'marketdata.curveBlueprints.fields.discountMeans' },
        },
        {
            field: 'interpolation_method',
            history: 'Interpolation Method',
            kind: { kind: 'choice', options: [...INTERPOLATION_METHODS] },
            means: {
                key: 'marketdata.curveBlueprints.fields.interpolationMeans',
                values: [...INTERPOLATION_METHODS],
            },
        },
        {
            field: 'day_count_convention',
            history: 'Day Count Convention',
            kind: { kind: 'classification', list: 'day-count-fraction-type' },
            means: { key: 'marketdata.curveBlueprints.fields.dayCountMeans' },
        },
        {
            field: 'tenor_convention_code',
            history: 'Tenor Convention Code',
            kind: { kind: 'pick', choices: inputs.conventions },
            means: { key: 'marketdata.curveBlueprints.fields.tenorConventionMeans' },
        },
        {
            field: 'split_tenor_code',
            history: 'Split Tenor Code',
            kind: { kind: 'pick', choices: inputs.tenors },
            when: (values) => values['interpolation_method'] === TWO_SEGMENT_METHOD,
            optional: true,
            means: { key: 'marketdata.curveBlueprints.fields.splitTenorMeans' },
        },
    ];
}

/** Every tenor and convention the pickers draw from, ready for use as a choice. */
function useLadderChoices(): {
    readonly tenors: readonly { readonly value: string; readonly label: string }[];
    readonly conventions: readonly { readonly value: string; readonly label: string }[];
} {
    const catalogue = useQuery({ queryKey: ['tenor-catalogue'], queryFn: tenors.catalogue });
    return {
        tenors: (catalogue.data?.tenors ?? []).map((row) => ({
            value: row.code,
            label: row.display_name === '' ? row.code : row.display_name,
        })),
        conventions: (catalogue.data?.conventions ?? []).map((row) => ({
            value: row.code,
            label: row.description === '' ? row.code : row.description,
        })),
    };
}

/**
 * The funding blueprints of one currency, which are the discount curve
 * candidates a projection may name: the server narrows the list by the two
 * members the model made filterable.
 */
function useFundingChoices(currency: string): readonly {
    readonly value: string;
    readonly label: string;
}[] {
    const store = useRecordStore();
    const configs = useQuery({
        queryKey: ['records', CONFIGS, 'funding', currency],
        queryFn: () =>
            store.page(CONFIGS, {
                offset: 0,
                limit: 200,
                search: '',
                sort: '',
                descending: false,
                filters: { currency_code: currency, curve_family_role: 'FUNDING' },
            }),
        enabled: currency !== '',
    });
    const ids = (configs.data?.rows ?? []).map((row) => show(row['id']));
    const uris = useQuery({
        queryKey: ['curve-series-uris', ids.join(',')],
        queryFn: () => curves.seriesUris(ids),
        enabled: ids.length > 0,
    });
    return (configs.data?.rows ?? []).map((row) => {
        const id = show(row['id']);
        return {
            value: id,
            label:
                uris.data?.get(id) ?? `${show(row['currency_code'])} ${show(row['index_tenor'])}`,
        };
    });
}

/** The series URI of each blueprint id asked for, read in one call. */
function useSeriesUris(ids: readonly string[]): ReadonlyMap<string, string> {
    const wanted = ids.filter((id) => id !== '');
    const uris = useQuery({
        queryKey: ['curve-series-uris', wanted.join(',')],
        queryFn: () => curves.seriesUris(wanted),
        enabled: wanted.length > 0,
    });
    return uris.data ?? new Map();
}

/** The sets published from any blueprint, newest first, including the ones that ended. */
function usePublishedSets(): readonly PublishedCurveSet[] {
    const sets = useQuery({
        queryKey: ['curve-sets', 'blueprints'],
        queryFn: () => api.publishedCurves({ includeEnded: true, limit: SETS_READ }),
    });
    return sets.data?.sets ?? [];
}

/** The sets one blueprint fed, newest first. */
function setsOf(
    sets: readonly PublishedCurveSet[],
    configId: string,
): readonly PublishedCurveSet[] {
    return sets
        .filter((set) => set.bootstrap_config_id === configId)
        .sort((left, right) => (right.published_at ?? '').localeCompare(left.published_at ?? ''));
}

/** Whether a set left use, by a withdrawal or by a successor. */
function ended(set: PublishedCurveSet): boolean {
    return set.state_code === 'withdrawn' || set.state_code === 'superseded';
}

/** The pillars of one blueprint, narrowed on the server by the member the model made filterable. */
function usePillars(configId: string) {
    const store = useRecordStore();
    return useQuery({
        queryKey: ['records', PILLARS, 'config', configId],
        queryFn: () =>
            store.page(PILLARS, {
                offset: 0,
                limit: 1000,
                search: '',
                sort: '',
                descending: false,
                filters: { bootstrap_config_id: configId },
            }),
        enabled: configId !== '',
    });
}

/** Where a record names the series a curve is published into, in words. */
function seriesWords(
    t: (key: string, values?: Readonly<Record<string, string>>) => string,
    uri: string | undefined,
): string {
    return uri === undefined || uri === '' ? t('marketdata.curveBlueprints.seriesUnknown') : uri;
}

/**
 * The curve blueprints: the recipes each curve is built from, one page at a
 * time from the server, searched and filtered there.
 *
 * The list reads the series URI of each row and the pillars each holds beside
 * the page, because a blueprint has no name of its own and the two facts are
 * what tell two rows apart. Add is not drawn: no operation mints the output
 * series a blueprint is created with, so a blueprint cannot be created from this
 * screen yet.
 */
export function CurveBlueprintsPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const base = useRecordSource(CONFIGS);
    const labels = useLabelCatalogue();
    const sets = usePublishedSets();
    const store = useRecordStore();
    const pillars = useQuery({
        queryKey: ['records', PILLARS, 'all'],
        queryFn: () => store.records(PILLARS),
    });
    const currencies = useCurrencyChoices();
    const countOf = (configId: string): number =>
        (pillars.data ?? []).filter((row) => show(row['bootstrap_config_id']) === configId).length;
    const filters: readonly ListFilter[] = [
        {
            kind: 'choice',
            id: 'currency_code',
            label: t('marketdata.curveBlueprints.filterCurrency'),
            all: t('marketdata.curveBlueprints.filterAllCurrencies'),
            choices: currencies,
        },
        {
            kind: 'choice',
            id: 'curve_family_role',
            label: t('marketdata.curveBlueprints.filterRole'),
            all: t('marketdata.curveBlueprints.filterAllRoles'),
            choices: CURVE_FAMILY_ROLES.map((role) => ({
                value: role,
                label: t(`marketdata.curveBlueprints.roles.${role}`),
            })),
        },
    ];
    const source: ListSource<RecordRow> = {
        ...base,
        filters,
        read: async (page: Parameters<typeof base.read>[0]) => {
            const answer = await base.read(page);
            const uris = await curves.seriesUris(answer.rows.map((row) => show(row['id'])));
            return {
                ...answer,
                rows: answer.rows.map((row): RecordRow => ({
                    ...row,
                    series_uri: uris.get(show(row['id'])) ?? '',
                })),
            };
        },
    };
    return (
        <RecordList
            source={source}
            title={t('marketdata.curveBlueprints.title')}
            lead={t('marketdata.curveBlueprints.lead')}
            crumbs={[
                ...useAreaRoot('marketdata'),
                { label: t('marketdata.curveBlueprints.title') },
            ]}
            notes={[{ tone: 'info', text: t('marketdata.curveBlueprints.noAdd') }]}
            pathOf={(row) => curveBlueprintPath(show(row['id']))}
            keyOf={(row) => show(row['id'])}
            columns={[
                {
                    id: 'curve',
                    header: t('marketdata.curveBlueprints.columns.curve'),
                    cell: (row) => (
                        <div>
                            <span className="font-mono text-xs">{show(row['series_uri'])}</span>
                            <p className="text-xs text-ink-faint">
                                {stateLine(t, row, countOf(show(row['id'])), sets)}
                            </p>
                        </div>
                    ),
                },
                {
                    id: 'currency_code',
                    header: t('marketdata.curveBlueprints.columns.currency'),
                    cell: (row) => show(row['currency_code']),
                    mono: true,
                    sort: 'currency_code',
                },
                {
                    id: 'curve_family_role',
                    header: t('marketdata.curveBlueprints.columns.role'),
                    cell: (row) => (
                        <CodedValue
                            domain="curve_family_role"
                            code={show(row['curve_family_role'])}
                            catalogue={labels}
                        />
                    ),
                    sort: 'curve_family_role',
                },
                {
                    id: 'index_tenor',
                    header: t('marketdata.curveBlueprints.columns.indexTenor'),
                    cell: (row) => show(row['index_tenor']),
                    mono: true,
                    sort: 'index_tenor',
                },
                {
                    id: 'interpolation_method',
                    header: t('marketdata.curveBlueprints.columns.interpolation'),
                    cell: (row) => (
                        <CodedValue
                            domain="interpolation_method"
                            code={show(row['interpolation_method'])}
                            catalogue={labels}
                        />
                    ),
                    sort: 'interpolation_method',
                },
                {
                    id: 'day_count_convention',
                    header: t('marketdata.curveBlueprints.columns.dayCount'),
                    cell: (row) => (
                        <CodedValue
                            domain="day_count_convention"
                            code={show(row['day_count_convention'])}
                            catalogue={labels}
                        />
                    ),
                    sort: 'day_count_convention',
                },
                {
                    id: 'pillars',
                    header: t('marketdata.curveBlueprints.columns.pillars'),
                    cell: (row) => countOf(show(row['id'])),
                    numeric: true,
                },
                {
                    id: 'version',
                    header: t('refdata.fields.version'),
                    cell: (row) => row.version,
                    numeric: true,
                    mono: true,
                },
                {
                    id: 'recorded_at',
                    header: t('marketdata.curveBlueprints.columns.lastChanged'),
                    cell: (row) => <RelativeTime at={show(row['recorded_at'])} />,
                },
            ]}
        />
    );
}

/**
 * The currencies the party's blueprints are quoted in, which are the list's
 * currency filter.
 *
 * The filter is drawn before any page is read, so its choices cannot come from
 * the page. A blueprint is a recipe per currency and a party holds a handful,
 * so the party's own blueprints are read whole once: a filter over every
 * currency of the tenant would offer a hundred values no blueprint uses.
 */
function useCurrencyChoices(): readonly { readonly value: string; readonly label: string }[] {
    const store = useRecordStore();
    const configs = useQuery({
        queryKey: ['records', CONFIGS, 'currencies'],
        queryFn: () => store.records(CONFIGS),
    });
    const codes = new Set(
        (configs.data ?? []).map((row) => show(row['currency_code'])).filter((code) => code !== ''),
    );
    return [...codes].sort().map((code) => ({ value: code, label: code }));
}

/** The line under a blueprint's code: what it holds and what stands on it. */
function stateLine(
    t: (key: string, values?: Readonly<Record<string, string>>) => string,
    config: RecordRow,
    pillarCount: number,
    sets: readonly PublishedCurveSet[],
): string {
    if (pillarCount === 0) {
        return t('marketdata.curveBlueprints.state.noPillars');
    }
    const fed = setsOf(sets, show(config['id']));
    if (fed.length === 0) {
        return t('marketdata.curveBlueprints.state.neverPublished');
    }
    const newest = fed[0];
    if (newest === undefined || !ended(newest)) {
        return t('marketdata.curveBlueprints.state.published');
    }
    return t('marketdata.curveBlueprints.state.noSetInUse', {
        date: (newest.withdrawn_at ?? newest.superseded_at ?? newest.as_of).slice(0, 10),
    });
}

/** One blueprint: its recipe, its pillars, its check, the sets it fed and its history. */
export function CurveBlueprintPage(): ReactNode {
    const { id = '' } = useParams();
    return (
        <RecordGate resource={CONFIGS} recordKey={id} listPath={curveBlueprintPath()}>
            {(config) => <BlueprintBody key={show(config['id'])} config={config} />}
        </RecordGate>
    );
}

function BlueprintBody({ config }: { readonly config: RecordRow }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const [search] = useSearchParams();
    const queries = useQueryClient();
    const id = show(config['id']);
    const may = useRecordPermissions(CONFIGS);
    const registry = useRegistry();
    const labels = useLabelCatalogue();
    const ladder = useLadderChoices();
    const fundings = useFundingChoices(show(config['currency_code']));
    const series = useSeriesUris([show(config['output_series_id'])]);
    const pillars = usePillars(id);
    const sets = usePublishedSets();
    const reading = useRecord(CONFIGS, id);
    const { tab, bar } = useRecordTabs({
        label: id,
        tabs: ['details', 'pillars', 'check', 'sets', 'history'],
    });
    const [editing, setEditing] = useState(false);
    const [copying, setCopying] = useState(false);
    const [removing, setRemoving] = useState(false);
    const [reverting, setReverting] = useState<HistoryVersion | null>(null);
    const uri = series.get(show(config['output_series_id']));
    const rows = pillars.data?.rows ?? [];
    const fed = setsOf(sets, id);
    const pageKeys = [
        ['records', CONFIGS, 'key', id],
        ['records', PILLARS, 'config', id],
        ['history', CONFIG_ENTITY, id],
        ['curve-sets', 'blueprints'],
    ];
    const load = useLoadState(pageKeys);
    const watches = (registry.data ?? [])
        .filter((entry) => [CONFIGS, PILLARS].includes(entry.key))
        .flatMap((entry) => (entry.events === undefined ? [] : [entry.events]));
    const news = useEntityChanges(watches, load);
    const refresh = (): void => {
        for (const key of pageKeys) {
            void queries.invalidateQueries({ queryKey: key });
        }
    };
    const specs = blueprintFields({
        tenors: ladder.tenors,
        conventions: ladder.conventions,
        fundings,
    });
    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    ...useAreaRoot('marketdata'),
                    { label: t('marketdata.curveBlueprints.title'), to: curveBlueprintPath() },
                    { label: uri ?? id },
                ]}
                title={uri ?? id}
                recordKey={[show(config['currency_code']), show(config['index_tenor'])]
                    .filter((part) => part !== '')
                    .join(' · ')}
                version={config.version}
                detail={t(`marketdata.curveBlueprints.roles.${show(config['curve_family_role'])}`)}
                subdued
                refresh={{ onClick: refresh, load, news }}
                own={
                    may.write ? (
                        <Button icon="copy" onClick={() => setCopying(true)}>
                            {t('marketdata.curveBlueprints.copy')}
                        </Button>
                    ) : undefined
                }
                onEdit={may.write ? () => setEditing(true) : undefined}
                onDelete={may.remove ? () => setRemoving(true) : undefined}
            />
            {bar}
            {tab === 'details' && (
                <div className="space-y-4">
                    <RecordDetails
                        specs={specs}
                        row={config}
                        extra={[
                            [
                                t('marketdata.curveBlueprints.fields.outputSeries'),
                                <Link
                                    className="text-accent hover:text-accent-bright"
                                    to={`/market-data/series?series=${encodeURIComponent(show(config['output_series_id']))}`}
                                >
                                    {seriesWords(t, uri)}
                                </Link>,
                                t('marketdata.curveBlueprints.fields.outputSeriesMeans'),
                            ],
                            [
                                t('marketdata.curveBlueprints.fields.usedBy'),
                                <UsedBy configId={id} />,
                            ],
                            [
                                t('marketdata.curveBlueprints.fields.fed'),
                                fed.length === 0
                                    ? t('marketdata.curveBlueprints.noneYet')
                                    : t('marketdata.curveBlueprints.fields.fedCount', {
                                          count: String(fed.length),
                                      }),
                            ],
                        ]}
                    />
                    {rows.length === 0 && (
                        <Notice tone="info">
                            {t('marketdata.curveBlueprints.noPillarsNotice')}{' '}
                            <button
                                type="button"
                                className="text-accent underline-offset-2 hover:underline"
                                onClick={() => void navigate({ search: '?tab=pillars' })}
                            >
                                {t('marketdata.curveBlueprints.openPillars')}
                            </button>
                        </Notice>
                    )}
                    {fed.length > 0 && fed.every(ended) && (
                        <Notice tone="warn">
                            {t('marketdata.curveBlueprints.allSetsEnded', {
                                date: (fed[0]?.withdrawn_at ?? fed[0]?.superseded_at ?? '').slice(
                                    0,
                                    10,
                                ),
                                account: fed[0]?.withdrawn_by ?? '',
                            })}
                        </Notice>
                    )}
                </div>
            )}
            {tab === 'pillars' && (
                <PartsPanel
                    resource={PILLARS}
                    entityType={PILLAR_ENTITY}
                    specs={pillarFields(ladder.tenors)}
                    parentField="bootstrap_config_id"
                    parent={id}
                    rows={rows}
                    error={pillars.error?.message}
                    lead={t('marketdata.curveBlueprints.pillarsLead')}
                    initial={{ start_tenor_code: 'SPOT', curve_role_code: 'SWAP' }}
                    keep={['party_id']}
                    complete={(write) => ({
                        ...write,
                        sequence_index: sequenceFor(rows, write),
                    })}
                    nameOf={(row) => pillarName(t, row)}
                    loadedAt={pillars.dataUpdatedAt}
                    titles={{
                        add: t('marketdata.curveBlueprints.addPillar'),
                        one: t('marketdata.curveBlueprints.onePillar'),
                        remove: t('marketdata.curveBlueprints.removePillar'),
                    }}
                    columns={[
                        {
                            header: t('marketdata.curveBlueprints.columns.sequence'),
                            cell: (row) => inMaturityOrder(rows).indexOf(row) + 1,
                            mono: true,
                        },
                        {
                            header: t('marketdata.curveBlueprints.columns.startTenor'),
                            cell: (row) => show(row['start_tenor_code']),
                            mono: true,
                        },
                        {
                            header: t('marketdata.curveBlueprints.columns.endTenor'),
                            cell: (row) => show(row['end_tenor_code']),
                            mono: true,
                        },
                        {
                            header: t('marketdata.curveBlueprints.columns.curveRole'),
                            cell: (row) => (
                                <CodedValue
                                    domain="bootstrap_curve_role"
                                    code={show(row['curve_role_code'])}
                                    catalogue={labels}
                                />
                            ),
                        },
                        {
                            header: t('refdata.fields.version'),
                            cell: (row) => row.version,
                            mono: true,
                        },
                        {
                            header: t('marketdata.curveBlueprints.columns.changedBy'),
                            cell: (row) => (
                                <span className="text-xs">
                                    {show(row['modified_by'])}{' '}
                                    <RelativeTime at={show(row['recorded_at'])} />
                                </span>
                            ),
                        },
                    ]}
                    sort={(row) =>
                        `${String(tenorRank(show(row['end_tenor_code']))).padStart(6, '0')}${show(row['end_tenor_code'])}`
                    }
                />
            )}
            {tab === 'check' && (
                <CheckTab
                    config={config}
                    pillars={rows}
                    asOf={search.get('asof') ?? ''}
                    onAsOf={(asOf) => {
                        const next = new URLSearchParams(search);
                        if (asOf === '') {
                            next.delete('asof');
                        } else {
                            next.set('asof', asOf);
                        }
                        void navigate({ search: `?${next.toString()}` });
                    }}
                    onEdit={() => setEditing(true)}
                    onPillars={() => void navigate({ search: '?tab=pillars' })}
                />
            )}
            {tab === 'sets' && <PublishedSetsTab configId={id} sets={fed} />}
            {tab === 'history' && (
                <section className="card space-y-4 p-4">
                    <h2 className="text-sm font-medium">{t('refdata.records.tabs.history')}</h2>
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.curveBlueprints.historyLead')}
                    </p>
                    <RecordTimeline
                        entityType={CONFIG_ENTITY}
                        entityId={id}
                        parts={[
                            {
                                entityType: PILLAR_ENTITY,
                                rows,
                                name: (row) => pillarName(t, row),
                            },
                        ]}
                        {...(may.write ? { onRevert: setReverting } : {})}
                    />
                </section>
            )}
            {editing && (
                <RecordDialog
                    title={t('refdata.records.editTitle', { code: uri ?? id })}
                    resource={CONFIGS}
                    specs={specs}
                    row={config}
                    keep={['output_series_id']}
                    loadedAt={reading.dataUpdatedAt}
                    onClose={() => setEditing(false)}
                />
            )}
            {copying && (
                <RecordDialog
                    title={t('marketdata.curveBlueprints.copyTitle', { code: uri ?? id })}
                    resource={CONFIGS}
                    specs={specs}
                    row={undefined}
                    given={{ id: crypto.randomUUID() }}
                    initial={{
                        interpolation_method: show(config['interpolation_method']),
                        day_count_convention: show(config['day_count_convention']),
                        tenor_convention_code: show(config['tenor_convention_code']),
                    }}
                    note={() => (
                        <Notice tone="info">
                            {t('marketdata.curveBlueprints.copyNote', {
                                count: String(rows.length),
                            })}
                        </Notice>
                    )}
                    onClose={() => setCopying(false)}
                />
            )}
            {removing && (
                <RemoveRecordDialog
                    resource={CONFIGS}
                    title={t('refdata.records.deleteTitle', { code: uri ?? id })}
                    warning={t('marketdata.curveBlueprints.removeWarning', {
                        count: String(rows.length),
                        sets: String(fed.length),
                    })}
                    recordKey={{ id }}
                    version={config.version}
                    before={async (intent) => {
                        for (const row of rows) {
                            await api.removeRecord(PILLARS, {
                                key: { id: show(row['id']) },
                                version: row.version,
                                ...intent,
                            });
                        }
                    }}
                    onClose={() => setRemoving(false)}
                    onRemoved={() => void navigate(curveBlueprintPath())}
                />
            )}
            {reverting !== null && (
                <RevertDialog
                    resource={CONFIGS}
                    specs={specs}
                    row={config}
                    version={reverting}
                    keep={['output_series_id']}
                    onClose={() => setReverting(null)}
                />
            )}
        </div>
    );
}

/** The pillar's fields, with the ladder the pickers draw from. */
function pillarFields(
    ladder: readonly { readonly value: string; readonly label: string }[],
): readonly FieldSpec[] {
    return PILLAR_FIELDS.map((spec) =>
        spec.kind.kind === 'pick' ? { ...spec, kind: { ...spec.kind, choices: ladder } } : spec,
    );
}

/** One pillar in words, for the title of its dialog and its history entry. */
function pillarName(
    t: (key: string, values?: Readonly<Record<string, string>>) => string,
    row: RecordRow,
): string {
    return t('marketdata.curveBlueprints.pillarName', {
        start: show(row['start_tenor_code']),
        end: show(row['end_tenor_code']),
        role: show(row['curve_role_code']),
    });
}

/**
 * The sequence one pillar is written with, derived from its own end tenor.
 *
 * The order is derived and never typed (RS-WRITE-07): a pillar stands where its
 * maturity falls among the others, so its sequence is the count of the pillars
 * that mature before it. The pillar being edited is left out of the count, so
 * an edit that moves a pillar's end tenor moves the pillar and not its
 * neighbours' stored order.
 */
export function sequenceFor(
    pillars: readonly RecordRow[],
    write: Readonly<Record<string, unknown>>,
): number {
    const id = show(write['id']);
    const rank = tenorRank(show(write['end_tenor_code']));
    return pillars.filter(
        (row) => show(row['id']) !== id && tenorRank(show(row['end_tenor_code'])) < rank,
    ).length;
}

/** The projection blueprints that name this one as their discount curve. */
function UsedBy({ configId }: { readonly configId: string }): ReactNode {
    const { t } = useTranslation();
    const store = useRecordStore();
    const users = useQuery({
        queryKey: ['records', CONFIGS, 'used-by', configId],
        queryFn: () =>
            store.page(CONFIGS, {
                offset: 0,
                limit: 200,
                search: '',
                sort: '',
                descending: false,
                filters: { discount_curve_config_id: configId },
            }),
    });
    const ids = (users.data?.rows ?? []).map((row) => show(row['id']));
    const uris = useSeriesUris(ids);
    if (users.isPending) {
        return <>{t('common.loading')}</>;
    }
    if (users.isError) {
        return <>{t('marketdata.curveBlueprints.usedByUnread')}</>;
    }
    if (ids.length === 0) {
        return <>{t('marketdata.curveBlueprints.usedByNone')}</>;
    }
    return (
        <span className="flex flex-wrap gap-2">
            {ids.map((id) => (
                <Link
                    key={id}
                    className="text-accent hover:text-accent-bright"
                    to={curveBlueprintPath(id)}
                >
                    {uris.get(id) ?? id}
                </Link>
            ))}
        </span>
    );
}

/** The sets one blueprint fed, newest first, each opening the published record. */
function PublishedSetsTab({
    configId,
    sets,
}: {
    readonly configId: string;
    readonly sets: readonly PublishedCurveSet[];
}): ReactNode {
    const { t } = useTranslation();
    const labels = useLabelCatalogue();
    return (
        <section className="card space-y-4 p-4">
            <h2 className="text-sm font-medium">{t('marketdata.curveBlueprints.tabs.sets')}</h2>
            <p className="text-sm text-ink-muted">{t('marketdata.curveBlueprints.setsLead')}</p>
            {sets.length === 0 ? (
                <p className="text-sm text-ink-muted">
                    {t('marketdata.curveBlueprints.noneYet')}{' '}
                    {t('marketdata.curveBlueprints.setsNoneLead')}
                </p>
            ) : (
                <div className="overflow-x-auto rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                <th className="px-4 py-2 font-medium">
                                    {t('marketdata.curveBlueprints.columns.asOf')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('marketdata.curveBlueprints.columns.party')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('marketdata.curveBlueprints.columns.publisher')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('marketdata.curveBlueprints.columns.publishedAt')}
                                </th>
                                <th className="px-4 py-2 text-right font-medium">
                                    {t('marketdata.curveBlueprints.columns.points')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('marketdata.curveBlueprints.columns.state')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {sets.map((set) => (
                                <tr
                                    key={set.publish_id}
                                    className="border-b border-line-subtle last:border-b-0"
                                >
                                    <td className="px-4 py-2">
                                        <Link
                                            className="text-accent hover:text-accent-bright"
                                            to={publishedCurvePath(set.curve, set.party)}
                                        >
                                            {set.as_of.slice(0, 10)}
                                        </Link>
                                    </td>
                                    <td className="px-4 py-2">{set.party}</td>
                                    <td className="px-4 py-2">{set.published_by}</td>
                                    <td className="px-4 py-2">
                                        {set.published_at === null ? (
                                            <span className="text-ink-faint">—</span>
                                        ) : (
                                            <RelativeTime at={set.published_at} />
                                        )}
                                    </td>
                                    <td className="px-4 py-2 text-right tabular-nums">
                                        {set.point_count}
                                    </td>
                                    <td className="px-4 py-2">
                                        <CodedValue
                                            domain="curve_publish_state"
                                            code={set.state_code}
                                            catalogue={labels}
                                        />
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            )}
            <p className="text-xs text-ink-faint">{t('marketdata.curveBlueprints.setsNote')}</p>
            <input type="hidden" value={configId} readOnly />
        </section>
    );
}

/** What the test needs to read, and what it answered. */
type CheckRun =
    | { readonly kind: 'idle' }
    | { readonly kind: 'refused'; readonly checks: readonly { readonly id: string }[] }
    | { readonly kind: 'failed'; readonly message: string }
    | {
          readonly kind: 'built';
          readonly points: readonly CurvePoint[];
          readonly unfilled: readonly string[];
      };

/**
 * The Check tab: the recipe's own checks, what the market supplied, the points
 * the build returned and the two lines they make.
 *
 * The six checks are answered from what the screen read, and a recipe that
 * fails one stops before the market is read. The forwards and the findings are
 * the screen's own arithmetic on the points, said as such, because the server
 * answers with the points alone.
 */
function CheckTab({
    config,
    pillars,
    asOf,
    onAsOf,
    onEdit,
    onPillars,
}: {
    readonly config: RecordRow;
    readonly pillars: readonly RecordRow[];
    readonly asOf: string;
    readonly onAsOf: (asOf: string) => void;
    readonly onEdit: () => void;
    readonly onPillars: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const id = show(config['id']);
    const [run, setRun] = useState<CheckRun>({ kind: 'idle' });
    const [defaultInstant] = useState(wireNow);
    const instant = asOf === '' ? defaultInstant : asOf;
    const discountId = show(config['discount_curve_config_id']);
    const discount = useRecord(CONFIGS, discountId);
    const recipe = checkRecipe({
        config,
        pillars,
        discount: discount.data,
    });
    const failing = recipe.filter((check) => !check.passed);
    const runTest = useMutation({
        mutationFn: async () => {
            if (failing.length > 0) {
                return { kind: 'refused', checks: failing } as CheckRun;
            }
            const points = await curves.compute(id, instant);
            const built = buildPoints(pillars, points);
            return {
                kind: 'built',
                points: built,
                unfilled: unfilledPillars(pillars, points),
            } as CheckRun;
        },
        onSuccess: (answer) => setRun(answer),
        onError: (error: Error) => setRun({ kind: 'failed', message: error.message }),
    });
    const tenants = (run.kind === 'built' ? run.points : []).map((point) => point.pillar);
    const findings = run.kind === 'built' ? findingsOf(run.points, run.unfilled) : [];
    const remedy = (check: { readonly remedy: string }): ReactNode => {
        switch (check.remedy) {
            case 'add':
                return (
                    <Button size="sm" variant="ghost" icon="add" onClick={onPillars}>
                        {t('marketdata.curveBlueprints.acts.addPillar')}
                    </Button>
                );
            case 'pillars':
                return (
                    <Button size="sm" variant="ghost" icon="edit" onClick={onPillars}>
                        {t('marketdata.curveBlueprints.acts.openPillars')}
                    </Button>
                );
            default:
                return (
                    <Button size="sm" variant="ghost" icon="edit" onClick={onEdit}>
                        {t('marketdata.curveBlueprints.acts.editBlueprint')}
                    </Button>
                );
        }
    };
    return (
        <section className="card space-y-4 p-4">
            <header className="flex flex-wrap items-end justify-between gap-3">
                <div>
                    <h2 className="text-sm font-medium">
                        {t('marketdata.curveBlueprints.tabs.check')}
                    </h2>
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.curveBlueprints.checkLead')}
                    </p>
                </div>
                <div className="flex items-end gap-2">
                    <Field label={t('marketdata.curveBlueprints.asOf')}>
                        <Input
                            value={instant}
                            maxLength={20}
                            inputMode="text"
                            onChange={(event) => onAsOf(event.target.value.trim())}
                        />
                    </Field>
                    <Button
                        variant="primary"
                        icon="refresh"
                        pending={runTest.isPending}
                        onClick={() => runTest.mutate()}
                    >
                        {t('marketdata.curveBlueprints.runTest')}
                    </Button>
                </div>
            </header>
            <div className="space-y-2">
                <h3 className="text-xs text-ink-faint">
                    {run.kind === 'idle'
                        ? t('marketdata.curveBlueprints.notChecked')
                        : t('marketdata.curveBlueprints.checksTitle')}
                </h3>
                <ul className="divide-y divide-line-subtle">
                    {recipe.map((check) => {
                        const answered = run.kind !== 'idle';
                        return (
                            <li
                                key={check.id}
                                className="flex flex-wrap items-center justify-between gap-2 py-2"
                            >
                                <span className="text-sm">
                                    {t(`marketdata.curveBlueprints.checks.${check.id}`)}
                                </span>
                                <span className="flex items-center gap-2">
                                    <Tag
                                        small
                                        tone={!answered ? 'neutral' : check.passed ? 'up' : 'down'}
                                    >
                                        {!answered
                                            ? t('marketdata.curveBlueprints.notRun')
                                            : check.passed
                                              ? t('marketdata.curveBlueprints.pass')
                                              : t('marketdata.curveBlueprints.fail')}
                                    </Tag>
                                    {answered && !check.passed && remedy(check)}
                                </span>
                            </li>
                        );
                    })}
                </ul>
            </div>
            {run.kind === 'refused' && (
                <Notice tone="warn">
                    {t('marketdata.curveBlueprints.validationFailed', {
                        count: String(run.checks.length),
                    })}
                </Notice>
            )}
            {run.kind === 'failed' && <Notice tone="error">{run.message}</Notice>}
            {run.kind === 'built' && (
                <>
                    <div className="space-y-2">
                        <h3 className="text-xs text-ink-faint">
                            {t('marketdata.curveBlueprints.marketTitle')}
                        </h3>
                        <ul className="flex flex-wrap gap-2">
                            {inMaturityOrder(pillars).map((pillar) => {
                                const code = show(pillar['end_tenor_code']);
                                const filled = !run.unfilled.includes(code);
                                return (
                                    <li
                                        key={code}
                                        className="flex items-center gap-1 rounded border border-line px-2 py-1 text-xs"
                                    >
                                        <span className="font-mono">{code}</span>
                                        <span
                                            className={filled ? 'text-up' : 'font-medium text-warn'}
                                        >
                                            {filled
                                                ? t('marketdata.curveBlueprints.observed')
                                                : t('marketdata.curveBlueprints.noRate')}
                                        </span>
                                    </li>
                                );
                            })}
                        </ul>
                    </div>
                    <div className="space-y-2">
                        <h3 className="text-xs text-ink-faint">
                            {t('marketdata.curveBlueprints.pointsTitle')}
                        </h3>
                        <div className="overflow-x-auto rounded-md border border-line">
                            <table className="w-full text-left text-sm">
                                <thead>
                                    <tr className="border-b border-line text-xs text-ink-muted">
                                        <th className="px-4 py-2 font-medium">
                                            {t('marketdata.curveBlueprints.columns.endTenor')}
                                        </th>
                                        <th className="px-4 py-2 font-medium">
                                            {t('marketdata.curveBlueprints.columns.date')}
                                        </th>
                                        <th className="px-4 py-2 text-right font-medium">
                                            {t('marketdata.curveBlueprints.columns.discountFactor')}
                                        </th>
                                        <th className="px-4 py-2 text-right font-medium">
                                            {t('marketdata.curveBlueprints.columns.forward')}
                                        </th>
                                    </tr>
                                </thead>
                                <tbody>
                                    {run.points.length === 0 && (
                                        <tr>
                                            <td colSpan={4} className="px-4 py-3 text-ink-muted">
                                                {t('marketdata.curveBlueprints.builtNothing')}
                                            </td>
                                        </tr>
                                    )}
                                    {run.points.map((point) => (
                                        <tr
                                            key={point.pillar}
                                            className="border-b border-line-subtle last:border-b-0"
                                        >
                                            <td className="px-4 py-2 font-mono text-xs">
                                                {point.pillar}
                                            </td>
                                            <td className="px-4 py-2 font-mono text-xs">
                                                {point.date}
                                            </td>
                                            <td className="px-4 py-2 text-right font-mono text-xs tabular-nums">
                                                {point.discountFactor.toFixed(6)}
                                            </td>
                                            <td className="px-4 py-2 text-right font-mono text-xs tabular-nums">
                                                {point.forward === null ? (
                                                    <span className="text-ink-faint">—</span>
                                                ) : (
                                                    `${(point.forward * 100).toFixed(3)}%`
                                                )}
                                            </td>
                                        </tr>
                                    ))}
                                </tbody>
                            </table>
                        </div>
                    </div>
                    {run.points.length > 1 && (
                        <div className="grid gap-4 lg:grid-cols-2">
                            <div>
                                <h4 className="text-xs text-ink-faint">
                                    {t('marketdata.curveBlueprints.discountFactors')}
                                </h4>
                                <LineChart
                                    label={t('marketdata.curveBlueprints.discountFactors')}
                                    categories={tenants}
                                    format={(value) => value.toFixed(3)}
                                    series={[
                                        {
                                            points: run.points.map((point) => point.discountFactor),
                                            colour: 'var(--color-accent)',
                                            strong: true,
                                            dots: true,
                                        },
                                    ]}
                                />
                            </div>
                            <div>
                                <h4 className="text-xs text-ink-faint">
                                    {t('marketdata.curveBlueprints.forwards')}
                                </h4>
                                <LineChart
                                    label={t('marketdata.curveBlueprints.forwards')}
                                    categories={tenants}
                                    format={(value) => `${(value * 100).toFixed(2)}%`}
                                    series={[
                                        {
                                            points: run.points.map((point) => point.forward),
                                            colour: 'var(--color-up)',
                                            strong: true,
                                            dots: true,
                                        },
                                    ]}
                                />
                            </div>
                        </div>
                    )}
                    <div className="space-y-1 border-t border-line-subtle pt-3">
                        <h3 className="text-xs text-ink-faint">
                            {t('marketdata.curveBlueprints.findingsTitle')}
                        </h3>
                        {findings.length === 0 ? (
                            <p className="text-sm text-ink-muted">
                                {t('marketdata.curveBlueprints.noFindings')}
                            </p>
                        ) : (
                            <ul className="list-inside list-disc text-sm">
                                {findings.map((finding) => (
                                    <li key={`${finding.kind}-${finding.pillar}`}>
                                        {t(`marketdata.curveBlueprints.findings.${finding.kind}`, {
                                            pillar: finding.pillar,
                                            value:
                                                finding.value === null
                                                    ? ''
                                                    : `${(finding.value * 100).toFixed(3)}%`,
                                        })}
                                    </li>
                                ))}
                            </ul>
                        )}
                        <p className="text-xs text-ink-faint">
                            {t('marketdata.curveBlueprints.derivedNote')}
                        </p>
                    </div>
                </>
            )}
        </section>
    );
}

/** The instant a test reads when the person names none: now, as the wire writes it. */
function wireNow(): string {
    return `${new Date().toISOString().slice(0, 19).replace('T', ' ')}Z`;
}

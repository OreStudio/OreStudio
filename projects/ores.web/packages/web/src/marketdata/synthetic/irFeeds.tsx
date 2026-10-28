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
import { api, type RecordRow } from '../../api/client.js';
import { PATHS } from '../../shell/paths.js';
import { useTranslation } from '../../i18n/Provider.js';
import { OresmdUrl } from '../../oresmd/OresmdUrl.js';
import {
    PartsPanel,
    show,
    useRecords,
    type FieldSpec,
    type FieldValues,
} from '../../refdata/records.js';
import { syntheticStore } from '../../refdata/store.js';
import { FeedPage } from './FeedScreens.js';
import { feedPath, type FeedKind } from './feedKind.js';
import { INDEX_FAMILIES, TERM_FAMILIES, completeIrFeed, curveOf, irSeriesUri } from './irCurve.js';
import { ArrivalsTab } from './ArrivalsPanel.js';
import { IrTicksPanel, savedParameters } from './IrTicksPanel.js';
import { IrProcessPanel, NO_ROWS, engineName } from './IrProcessPanel.js';
import type { SaveEngine } from './SavePartsDialog.js';

const FEEDS = 'ir-curve-feeds';
export const PARAMETER_VALUES = 'ir-parameter-values';
export const TEMPLATE_ENTRIES = 'ir-template-entries';

const ROLES = ['self_discounting', 'discount', 'projection'];
/*
 * Every yield-curve engine precomputes its step from dt, so none of them can
 * follow an arrival that gives ticks of different lengths. The service refuses
 * the pairing when the feed starts; offering only the periodic mode here means a
 * user cannot author it in the first place.
 */
const ARRIVAL_MODES = ['periodic'] as const;

const PRICE_SOURCES = ['fixed', 'vintage'];

/** Whether the family chosen in the form is an index with a term. */
const isTerm = (values: FieldValues): boolean =>
    TERM_FAMILIES.includes(values['index_family'] ?? '');

/**
 * The curve's own fields, in the words of the Qt IR curve editor: the
 * currency, the index name, the role, the fixed leg frequency and how often
 * a tick is made, then where the starting rate comes from. The source name is
 * derived and may be changed. The engine and its parameters are on the Process
 * tab, where they are saved together.
 */
const IR_FIELDS: readonly FieldSpec[] = [
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
        field: 'currency_code',
        history: 'Currency Code',
        kind: { kind: 'record', resource: 'currencies', value: 'iso_code', label: 'name' },
        flag: 'currency',
        fixed: true,
    },
    {
        field: 'index_family',
        history: 'Index Family',
        kind: { kind: 'choice', options: INDEX_FAMILIES },
        fixed: true,
    },
    {
        field: 'tenor',
        history: 'Tenor',
        kind: { kind: 'text', max: 20 },
        blank: true,
        when: (values) => isTerm(values) || values['index_family'] === 'sofr',
        help: 'marketdata.irFeeds.help.tenor',
        fixed: true,
    },
    {
        field: 'role',
        history: 'Role',
        kind: { kind: 'choice', options: ROLES },
        help: 'marketdata.irFeeds.help.role',
    },
    {
        field: 'fixed_leg_payment_frequency_code',
        history: 'Fixed Leg Payment Frequency Code',
        kind: {
            kind: 'record',
            resource: 'payment-frequencies',
            value: 'code',
            label: 'code',
            store: syntheticStore,
        },
    },
    {
        field: 'source_name',
        history: 'Source Name',
        kind: { kind: 'text', max: 200 },
        blank: true,
    },
    {
        field: 'description',
        history: 'Description',
        kind: { kind: 'text' },
        blank: true,
    },
    {
        field: 'process_type',
        history: 'Process Type',
        kind: {
            kind: 'record',
            resource: 'process-types',
            value: 'code',
            label: 'name',
            store: syntheticStore,
        },
    },
    {
        field: 'ticks_per_hour',
        history: 'Ticks Per Hour',
        kind: { kind: 'int', min: 1, max: 360000 },
        help: 'marketdata.irFeeds.help.ticks_per_hour',
    },
    {
        field: 'arrival_mode',
        history: 'Arrival Mode',
        kind: { kind: 'choice', options: ARRIVAL_MODES },
        help: 'marketdata.irFeeds.help.arrival_mode',
    },
    {
        field: 'branching_ratio',
        history: 'Branching Ratio',
        kind: { kind: 'decimal', below: 1 },
        help: 'marketdata.irFeeds.help.branching_ratio',
    },
    {
        field: 'decay',
        history: 'Decay',
        kind: { kind: 'decimal', positive: true },
        help: 'marketdata.irFeeds.help.decay',
    },
    {
        field: 'sensitivity',
        history: 'Sensitivity',
        kind: { kind: 'decimal', positive: true },
        help: 'marketdata.irFeeds.help.sensitivity',
    },
    {
        field: 'price_source',
        history: 'Price Source',
        kind: { kind: 'choice', options: PRICE_SOURCES },
        labelKey: 'marketdata.irFeeds.priceSource',
        help: 'marketdata.irFeeds.help.price_source',
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
        field: 'vintage_series_uri',
        history: 'Vintage Series Uri',
        kind: { kind: 'text', max: 2000 },
        blank: true,
        when: (values) => values['price_source'] === 'vintage',
    },
    {
        field: 'enabled',
        history: 'Enabled',
        kind: { kind: 'bool' },
        help: 'marketdata.irFeeds.help.enabled',
    },
    {
        field: 'auto_start',
        history: 'Auto Start',
        kind: { kind: 'bool' },
        help: 'marketdata.irFeeds.help.auto_start',
    },
];

/** The fields the server needs that the form does not edit. */
const IR_KEPT = ['folder_id'];

/** The fields the edit dialog offers: the engine is changed on the Process tab, with its parameters. */
const IR_EDIT_FIELDS = IR_FIELDS.filter((spec) => spec.field !== 'process_type');

/** The fields a curve feed write carries, as the BFF takes them. */
const IR_WRITE_FIELDS = [
    'id',
    'config_id',
    'currency_code',
    'index_family',
    'tenor',
    'role',
    'process_type',
    'ticks_per_hour',
    'enabled',
    'auto_start',
    'price_source',
    'vintage_source',
    'vintage_date',
    'vintage_series_uri',
    'description',
    'fixed_leg_payment_frequency_code',
    'source_name',
    'folder_id',
] as const;

/** The feed as written back with a change, such as a new engine. */
export function irFeedWrite(
    feed: Readonly<Record<string, unknown>>,
    change: Readonly<Record<string, unknown>>,
): Record<string, unknown> {
    return {
        ...Object.fromEntries(IR_WRITE_FIELDS.map((field) => [field, feed[field] ?? null])),
        ...change,
    };
}

const NEW_IR_FEED: FieldValues = {
    index_family: 'sofr',
    role: 'self_discounting',
    fixed_leg_payment_frequency_code: 'Annual',
    process_type: 'VASICEK',
    ticks_per_hour: '3600',
    price_source: 'fixed',
    vintage_series_uri: '',
    description: '',
    enabled: 'false',
    auto_start: 'false',
};

/** The rules across fields that the table checks, said before the save rather than after it. */
function useIrFeedCheck(): (values: FieldValues) => readonly string[] {
    const { t } = useTranslation();
    return (values) => {
        const family = values['index_family'] ?? '';
        const tenor = values['tenor'] ?? '';
        if (TERM_FAMILIES.includes(family) && tenor === '') {
            return [t('marketdata.irFeeds.tenorNeeded')];
        }
        if (family === 'sofr' && tenor !== '' && tenor !== 'FOMC') {
            return [t('marketdata.irFeeds.tenorOnlyFomc')];
        }
        return [];
    };
}

/** The template entries of a curve, whose fields the page offers. */
const ENTRY_FIELDS: readonly FieldSpec[] = [
    { field: 'sequence_index', history: 'Sequence Index', kind: { kind: 'int', min: 0 } },
    {
        field: 'start_tenor_code',
        history: 'Start Tenor Code',
        kind: {
            kind: 'record',
            resource: 'tenors',
            value: 'code',
            label: 'code',
            store: syntheticStore,
        },
    },
    {
        field: 'end_tenor_code',
        history: 'End Tenor Code',
        kind: {
            kind: 'record',
            resource: 'tenors',
            value: 'code',
            label: 'code',
            store: syntheticStore,
        },
    },
    {
        field: 'instrument_code',
        history: 'Instrument Code',
        kind: {
            kind: 'record',
            resource: 'instrument-codes',
            value: 'code',
            label: 'code',
            store: syntheticStore,
        },
    },
];

/** A parameter value's fields, for its revert; the Process tab edits the rest. */
const PARAMETER_FIELDS: readonly FieldSpec[] = [
    {
        field: 'parameter_value',
        history: 'Parameter Value',
        kind: { kind: 'decimal' },
    },
];

/** Writes the curve's engine, with the reason its parameters are saved under. */
const saveEngine =
    (feed: RecordRow): SaveEngine =>
    async (code, intent) => {
        await api.saveSynthetic(
            FEEDS,
            [{ write: irFeedWrite(feed, { process_type: code }), version: feed.version }],
            intent,
        );
    };

/** The Curve Template tab: the tenor entries the curve is priced at, as parts of the feed. */
function IrTemplatePanel({
    feed,
    entries,
}: {
    readonly feed: RecordRow;
    readonly entries: readonly RecordRow[];
}): ReactNode {
    const { t } = useTranslation();
    return (
        <PartsPanel
            resource={TEMPLATE_ENTRIES}
            entityType="ores.synthetic.ir_curve_template_entry"
            specs={ENTRY_FIELDS}
            parentField="ir_curve_config_id"
            parent={show(feed['id'])}
            rows={entries}
            error={undefined}
            columns={[
                {
                    header: t('refdata.fields.sequence_index'),
                    cell: (row) => show(row['sequence_index']),
                    mono: true,
                },
                {
                    header: t('refdata.fields.start_tenor_code'),
                    cell: (row) => show(row['start_tenor_code']),
                    mono: true,
                },
                {
                    header: t('refdata.fields.end_tenor_code'),
                    cell: (row) => show(row['end_tenor_code']),
                    mono: true,
                },
                {
                    header: t('refdata.fields.instrument_code'),
                    cell: (row) => show(row['instrument_code']),
                    mono: true,
                },
            ]}
            sort={(row) => show(row['sequence_index']).padStart(6, '0')}
            initial={{ sequence_index: String(entries.length) }}
            lead={t('marketdata.irTemplate.lead')}
            titles={{
                add: t('marketdata.irTemplate.add'),
                one: t('marketdata.irTemplate.one'),
                remove: t('marketdata.irTemplate.remove'),
            }}
        />
    );
}

/** The Ticks tab: the last curves of the feed, with the saved process beside them. */
function IrTicksTab({
    feed,
    running,
    values,
    entries,
}: {
    readonly feed: RecordRow;
    readonly running: boolean | undefined;
    readonly values: readonly RecordRow[];
    readonly entries: readonly RecordRow[];
}): ReactNode {
    const definitions = useRecords('parameter-definitions');
    return (
        <IrTicksPanel
            feed={feed}
            running={running}
            entries={entries}
            parameters={savedParameters(
                definitions.data ?? NO_ROWS,
                values,
                show(feed['process_type']),
            )}
        />
    );
}

/** The IR curve feeds, as the shared feed screens draw them. */
export const IR_FEED_KIND: FeedKind = {
    resource: FEEDS,
    entity: 'ores.synthetic.ir_curve_generation_config',
    historySubject: 'ir_feed',
    path: PATHS.irFeed,
    words: 'irFeeds',
    fields: IR_FIELDS,
    editFields: IR_EDIT_FIELDS,
    kept: IR_KEPT,
    initial: NEW_IR_FEED,
    complete: completeIrFeed,
    useCheck: useIrFeedCheck,
    nameOf: curveOf,
    detailOf: (feed, t) => engineName(t, show(feed['process_type'])),
    extraDetails: (feed, t) => [
        [
            t('refdata.fields.oresmd_uri'),
            <OresmdUrl uri={irSeriesUri(feed)} />,
            t('marketdata.feeds.targetUri'),
        ],
    ],
    parts: [
        {
            resource: PARAMETER_VALUES,
            entity: 'ores.synthetic.ir_curve_generation_config_process_parameter_value',
            specs: PARAMETER_FIELDS,
            keep: ['id', 'config_id', 'parameter_definition_id'],
        },
        {
            resource: TEMPLATE_ENTRIES,
            entity: 'ores.synthetic.ir_curve_template_entry',
            specs: ENTRY_FIELDS,
            keep: ['id', 'ir_curve_config_id'],
        },
    ],
    tabs: [
        {
            name: 'process',
            render: ({ feed, running, mayWrite, parts }) => (
                <IrProcessPanel
                    feed={feed}
                    running={running === true}
                    mayWrite={mayWrite}
                    saved={parts[0] ?? NO_ROWS}
                    entries={parts[1] ?? NO_ROWS}
                    valuesResource={PARAMETER_VALUES}
                    saveEngine={saveEngine(feed)}
                />
            ),
        },
        {
            name: 'template',
            render: ({ feed, parts }) => (
                <IrTemplatePanel feed={feed} entries={parts[1] ?? NO_ROWS} />
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
                                    write: irFeedWrite(feed, {
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
                <IrTicksTab
                    feed={feed}
                    running={running}
                    values={parts[0] ?? NO_ROWS}
                    entries={parts[1] ?? NO_ROWS}
                />
            ),
        },
    ],
    afterAdd: 'process',
};

export function irFeedPath(id?: string): string {
    return feedPath(IR_FEED_KIND, id);
}

/** One curve feed: its fields, the engine and parameters, the curve template, and its history. */
export function IrFeedPage(): ReactNode {
    return <FeedPage kind={IR_FEED_KIND} />;
}

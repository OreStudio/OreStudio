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
import { useNavigate, useParams } from 'react-router';
import { api, type RecordRow } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { RecordList, useRecordSource } from '../refdata/RecordList.js';
import {
    RecordDetails,
    RecordDialog,
    RecordGate,
    RecordHeader,
    RemoveRecordDialog,
    show,
    useRecordPermissions,
    useRecordTabs,
    useRecords,
    type FieldSpec,
    type FieldValues,
} from '../refdata/records.js';
import { RecordStoreProvider, syntheticStore } from '../refdata/store.js';
import { Button, Notice } from '../ui/Primitives.js';
import { FeedStatus, RunOutcomeText, useFeedRuns, useRunningFeeds } from './feedRuns.js';
import { FeedHistory } from './FeedHistory.js';
import { COMPONENTS, MixturePanel } from './MixturePanel.js';
import { TicksPanel } from './TicksPanel.js';

const FEEDS = 'fx-feeds';

const PROCESS_TYPES = ['geometric', 'arithmetic', 'ornstein_uhlenbeck'];
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

const NEW_FEED: FieldValues = {
    process_type: 'geometric',
    ticks_per_hour: '3600',
    price_source: 'fixed',
    gmm_initial_price: '1',
    enabled: 'false',
    auto_start: 'false',
};

export function fxFeedPath(id?: string): string {
    return id === undefined
        ? '/market-data/fx-feeds'
        : `/market-data/fx-feeds/${encodeURIComponent(id)}`;
}

/** The pair a feed quotes, as the pair screens write it. */
function pairOf(feed: Readonly<Record<string, unknown>>): string {
    return `${show(feed['base_currency_code'])}/${show(feed['quote_currency_code'])}`;
}

/**
 * The fields that follow from the others: the ORE key from the pair, a source
 * name from the pair when none is given, and the price fields the source does
 * not use emptied, as the table requires.
 */
export function completeFeed(write: Readonly<Record<string, unknown>>): Record<string, unknown> {
    const base = show(write['base_currency_code']);
    const quote = show(write['quote_currency_code']);
    const fixed = write['price_source'] === 'fixed';
    return {
        ...write,
        ore_key: `FX/RATE/${base}/${quote}`,
        source_name:
            show(write['source_name']) === ''
                ? `synthetic.${base}${quote}`.toLowerCase()
                : write['source_name'],
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

/** The FX price feeds of the party: one page at a time, searched and sorted on the server. */
export function FxFeedsPage(): ReactNode {
    return (
        <RecordStoreProvider store={syntheticStore}>
            <FxFeedsList />
        </RecordStoreProvider>
    );
}

function FxFeedsList(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const [adding, setAdding] = useState(false);
    const source = useRecordSource(FEEDS);
    const check = useFeedCheck();
    const may = useRecordPermissions(FEEDS);
    const running = useRunningFeeds();
    const runs = useFeedRuns();
    const [chosen, setChosen] = useState<ReadonlyMap<string, string>>(new Map());
    const choose = (row: RecordRow, on: boolean): void => {
        const next = new Map(chosen);
        if (on) {
            next.set(show(row['id']), show(row['source_name']));
        } else {
            next.delete(show(row['id']));
        }
        setChosen(next);
    };
    const run = (action: 'start' | 'stop'): void => {
        runs.run(
            action,
            [...chosen].map(([id, label]) => ({ id, label })),
        );
        setChosen(new Map());
    };
    return (
        <>
            {runs.outcome !== undefined && (
                <Notice tone={runs.outcome.refused.length > 0 ? 'warn' : 'success'}>
                    <RunOutcomeText outcome={runs.outcome} />
                </Notice>
            )}
            <RecordList
                source={source}
                {...(may.write
                    ? {
                          actions: (
                              <>
                                  <Button
                                      disabled={chosen.size === 0}
                                      pending={runs.pending}
                                      onClick={() => run('start')}
                                  >
                                      {t('marketdata.runs.startChosen', {
                                          count: String(chosen.size),
                                      })}
                                  </Button>
                                  <Button
                                      disabled={chosen.size === 0}
                                      pending={runs.pending}
                                      onClick={() => run('stop')}
                                  >
                                      {t('marketdata.runs.stopChosen', {
                                          count: String(chosen.size),
                                      })}
                                  </Button>
                              </>
                          ),
                      }
                    : {})}
                title={t('marketdata.fxFeeds.title')}
                lead={t('marketdata.fxFeeds.lead')}
                crumbs={[
                    { label: t('marketdata.area.title'), to: '/market-data' },
                    { label: t('marketdata.fxFeeds.title') },
                ]}
                pathOf={(row) => fxFeedPath(show(row['id']))}
                addLabel={t('marketdata.fxFeeds.add')}
                onAdd={() => setAdding(true)}
                columns={[
                    ...(may.write
                        ? [
                              {
                                  id: 'chosen',
                                  header: t('marketdata.runs.choose'),
                                  cell: (row: RecordRow) => (
                                      <input
                                          type="checkbox"
                                          aria-label={t('marketdata.runs.chooseFeed', {
                                              feed: show(row['source_name']),
                                          })}
                                          checked={chosen.has(show(row['id']))}
                                          onChange={(event) => choose(row, event.target.checked)}
                                      />
                                  ),
                              },
                          ]
                        : []),
                    {
                        id: 'status',
                        header: t('marketdata.runs.status'),
                        cell: (row) => (
                            <FeedStatus running={running?.has(show(row['source_name']))} />
                        ),
                    },
                    {
                        id: 'pair',
                        header: t('marketdata.fxFeeds.pair'),
                        cell: (row) => pairOf(row),
                        mono: true,
                        sort: 'base_currency_code',
                        flag: { source: 'pair', code: (row) => pairOf(row) },
                    },
                    {
                        id: 'source_name',
                        header: t('refdata.fields.source_name'),
                        cell: (row) => show(row['source_name']),
                        mono: true,
                        sort: 'source_name',
                    },
                    {
                        id: 'ore_key',
                        header: t('refdata.fields.ore_key'),
                        cell: (row) => show(row['ore_key']),
                        mono: true,
                        sort: 'ore_key',
                    },
                    {
                        id: 'process_type',
                        header: t('refdata.fields.process_type'),
                        cell: (row) =>
                            t(`refdata.choices.process_type.${show(row['process_type'])}`),
                    },
                    {
                        id: 'ticks_per_hour',
                        header: t('refdata.fields.ticks_per_hour'),
                        cell: (row) => show(row['ticks_per_hour']),
                        numeric: true,
                        sort: 'ticks_per_hour',
                    },
                    {
                        id: 'enabled',
                        header: t('refdata.fields.enabled'),
                        cell: (row) =>
                            row['enabled'] === true
                                ? t('refdata.records.yes')
                                : t('refdata.records.no'),
                    },
                    {
                        id: 'auto_start',
                        header: t('refdata.fields.auto_start'),
                        cell: (row) =>
                            row['auto_start'] === true
                                ? t('refdata.records.yes')
                                : t('refdata.records.no'),
                        hidden: true,
                    },
                ]}
            />
            {adding && (
                <RecordDialog
                    title={t('marketdata.fxFeeds.addTitle')}
                    resource={FEEDS}
                    specs={FEED_FIELDS}
                    row={undefined}
                    given={{ id: crypto.randomUUID(), folder_id: null }}
                    initial={NEW_FEED}
                    complete={completeFeed}
                    check={check}
                    onClose={() => setAdding(false)}
                    onSaved={(write) =>
                        void navigate(`${fxFeedPath(show(write['id']))}?tab=behaviour`)
                    }
                />
            )}
        </>
    );
}

/** One feed: its fields, the mixture its process draws from with what it implies, and its history. */
export function FxFeedPage(): ReactNode {
    const { id } = useParams();
    return (
        <RecordStoreProvider store={syntheticStore}>
            <RecordGate resource={FEEDS} recordKey={id ?? ''} listPath={fxFeedPath()}>
                {(feed) => <FxFeedBody key={show(feed['id'])} feed={feed} />}
            </RecordGate>
        </RecordStoreProvider>
    );
}

function FxFeedBody({ feed }: { readonly feed: RecordRow }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const id = show(feed['id']);
    const pair = pairOf(feed);
    const may = useRecordPermissions(FEEDS);
    const mayComponents = useRecordPermissions(COMPONENTS);
    const check = useFeedCheck();
    const mixture = useRecords(COMPONENTS, id);
    const { tab, bar } = useRecordTabs({
        label: pair,
        tabs: ['details', 'behaviour', 'ticks', 'history'],
    });
    const running = useRunningFeeds()?.has(show(feed['source_name']));
    const runs = useFeedRuns();
    const [editing, setEditing] = useState(false);
    const [removing, setRemoving] = useState(false);

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    { label: t('marketdata.area.title'), to: '/market-data' },
                    { label: t('marketdata.fxFeeds.title'), to: fxFeedPath() },
                    { label: pair },
                ]}
                title={show(feed['source_name'])}
                recordKey={pair}
                version={feed.version}
                detail={t(`refdata.choices.process_type.${show(feed['process_type'])}`)}
                flag="pair"
                own={
                    <>
                        <FeedStatus running={running} />
                        {may.write && running !== undefined && (
                            <Button
                                size="sm"
                                pending={runs.pending}
                                onClick={() =>
                                    runs.run(running ? 'stop' : 'start', [
                                        { id, label: show(feed['source_name']) },
                                    ])
                                }
                            >
                                {running ? t('marketdata.runs.stop') : t('marketdata.runs.start')}
                            </Button>
                        )}
                    </>
                }
                onEdit={may.write ? () => setEditing(true) : undefined}
                onDelete={may.remove ? () => setRemoving(true) : undefined}
            />
            {runs.outcome !== undefined && runs.outcome.refused.length > 0 && (
                <Notice tone="warn">
                    <RunOutcomeText outcome={runs.outcome} />
                </Notice>
            )}
            {bar}
            {tab === 'details' && (
                <RecordDetails
                    specs={FEED_FIELDS}
                    row={feed}
                    extra={[[t('refdata.fields.ore_key'), show(feed['ore_key'])]]}
                />
            )}
            {tab === 'behaviour' && <MixturePanel feed={feed} running={running === true} />}
            {tab === 'ticks' && (
                <TicksPanel feed={feed} running={running} mixture={mixture.data ?? []} />
            )}
            {tab === 'history' && (
                <FeedHistory
                    feed={feed}
                    components={mixture.data ?? []}
                    feedSpecs={FEED_FIELDS}
                    feedResource={FEEDS}
                    componentResource={COMPONENTS}
                    complete={completeFeed}
                    keep={['id', ...FEED_KEPT]}
                    mayWrite={{ feed: may.write, components: mayComponents.write }}
                />
            )}
            {editing && (
                <RecordDialog
                    title={t('refdata.records.editTitle', { code: pair })}
                    resource={FEEDS}
                    specs={FEED_FIELDS}
                    row={feed}
                    keep={['id', ...FEED_KEPT]}
                    complete={completeFeed}
                    check={check}
                    onClose={() => setEditing(false)}
                />
            )}
            {removing && (
                <RemoveRecordDialog
                    resource={FEEDS}
                    title={t('refdata.records.deleteTitle', { code: pair })}
                    warning={t('marketdata.fxFeeds.removeWarning')}
                    recordKey={{ id }}
                    version={feed.version}
                    before={async (intent) => {
                        await api.stopFeed(id);
                        const components = mixture.data ?? [];
                        if (components.length > 0) {
                            await api.removeSynthetic(
                                COMPONENTS,
                                components.map((row) => ({
                                    key: show(row['id']),
                                    version: components.length === 1 ? row.version : null,
                                })),
                                intent,
                            );
                        }
                    }}
                    onClose={() => setRemoving(false)}
                    onRemoved={() => void navigate(fxFeedPath())}
                />
            )}
        </div>
    );
}

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

import { useQuery, useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link, useNavigate, useParams } from 'react-router';
import type { HistoryVersion } from '@ores/wire-protocol/browser';
import { api, type RecordRow } from '../api/client.js';
import { ApiFailure } from '../api/transport.js';
import { useTranslation } from '../i18n/Provider.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { Notice } from '../ui/Primitives.js';
import { FlaggedCode } from '../images/flags.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { useLoadState } from '../events/useLoadState.js';
import { currencyPath } from './currencies.js';
import { RecordList, useRecordSource } from './RecordList.js';
import { RecordTimeline } from './RecordTimeline.js';
import {
    ClassifiedValue,
    LinkPanel,
    NEW_RECORD_REASON,
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
    useRecords,
    useRegistry,
    valuesOf,
    writeOf,
    type FieldSpec,
} from './records.js';

const PAIRS = 'currency-pairs';
const CONVENTIONS = 'currency-pair-conventions';
const CALENDARS = 'pair-calendars';
const ENTITY = 'ores.refdata.currency_pair';
const CONVENTION_ENTITY = 'ores.refdata.currency_pair_convention';

const PAIR_FIELDS: readonly FieldSpec[] = [
    {
        field: 'base_currency',
        history: 'Base Currency',
        kind: { kind: 'record', resource: 'currencies', value: 'iso_code', label: 'name' },
        fixed: true,
        flag: 'currency',
        group: 'refdata.pairs.pairGroup',
    },
    {
        field: 'quote_currency',
        history: 'Quote Currency',
        kind: { kind: 'record', resource: 'currencies', value: 'iso_code', label: 'name' },
        fixed: true,
        flag: 'currency',
        group: 'refdata.pairs.pairGroup',
    },
    {
        field: 'classification',
        history: 'Classification',
        kind: { kind: 'classification', list: 'currency-pair-classification' },
        group: 'refdata.pairs.pairGroup',
    },
];

const CONVENTION_TABLE: readonly FieldSpec[] = [
    { field: 'pip_factor', history: 'Pip Factor', kind: { kind: 'decimal' } },
    { field: 'tick_size', history: 'Tick Size', kind: { kind: 'decimal' } },
    { field: 'decimal_places', history: 'Decimal Places', kind: { kind: 'int' } },
    {
        field: 'business_day_convention',
        history: 'Business Day Convention',
        kind: { kind: 'classification', list: 'business-day-convention-type' },
        optional: true,
    },
    { field: 'spot_relative', history: 'Spot Relative', kind: { kind: 'bool' }, optional: true },
    { field: 'end_of_month', history: 'End Of Month', kind: { kind: 'bool' }, optional: true },
];
const CONVENTION_FIELDS: readonly FieldSpec[] = CONVENTION_TABLE.map((spec) => ({
    ...spec,
    group: 'refdata.pairs.convention',
}));

/** The one form of the pair and its convention: two groups, in the field table's order (RS-WRITE-09). */
const PAIR_FORM: readonly FieldSpec[] = [...PAIR_FIELDS, ...CONVENTION_FIELDS];

export function pairPath(code?: string): string {
    return code === undefined
        ? '/refdata/currency-pairs'
        : `/refdata/currency-pairs/${encodeURIComponent(code)}`;
}

/** A rate shown at the pair's display precision, so the person sees what the desk will see. */
function sampleRate(decimals: string): string {
    const places = Number.parseInt(decimals, 10);
    return Number.isNaN(places) ? '' : (1.084235719).toFixed(Math.min(Math.max(places, 0), 10));
}

/** The pairs, one page at a time, searched and sorted on the server. */
export function CurrencyPairsPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const [adding, setAdding] = useState(false);
    const source = useRecordSource(PAIRS);
    return (
        <>
            <RecordList
                source={source}
                title={t('refdata.pairs.title')}
                lead={t('refdata.pairs.lead')}
                crumbs={[...useAreaRoot('refdata'), { label: t('refdata.pairs.title') }]}
                pathOf={(row) => pairPath(show(row['pair_code']))}
                addLabel={t('refdata.pairs.add')}
                onAdd={() => setAdding(true)}
                columns={[
                    {
                        id: 'pair_code',
                        header: t('refdata.fields.pair_code'),
                        cell: (row) => show(row['pair_code']),
                        mono: true,
                        sort: 'pair_code',
                        flag: { source: 'pair', code: (row) => show(row['pair_code']) },
                    },
                    {
                        id: 'base_currency',
                        header: t('refdata.fields.base_currency'),
                        cell: (row) => show(row['base_currency']),
                        mono: true,
                        sort: 'base_currency',
                        flag: { source: 'currency', code: (row) => show(row['base_currency']) },
                    },
                    {
                        id: 'quote_currency',
                        header: t('refdata.fields.quote_currency'),
                        cell: (row) => show(row['quote_currency']),
                        mono: true,
                        sort: 'quote_currency',
                        flag: { source: 'currency', code: (row) => show(row['quote_currency']) },
                    },
                    {
                        id: 'classification',
                        header: t('refdata.fields.classification'),
                        cell: (row) => (
                            <ClassifiedValue
                                list="currency-pair-classification"
                                code={show(row['classification'])}
                            />
                        ),
                        sort: 'classification',
                    },
                ]}
            />
            {adding && (
                <PairDialog
                    pair={undefined}
                    convention={undefined}
                    onClose={() => setAdding(false)}
                    onSaved={(code) => void navigate(pairPath(code))}
                />
            )}
        </>
    );
}

/**
 * The shared record dialog over the pair and its convention: one form of two
 * groups, the pair code derived from the two legs and never asked for
 * (RS-WRITE-07), two equal legs refused before the save (RS-WRITE-08), and the
 * sample rate as a line under the fields (RS-WRITE-12). The convention refers
 * to the pair, so the pair is written first; if the second write fails the
 * dialog says so and stays open, and a pair found without its convention says
 * so on its page.
 */
function PairDialog({
    pair,
    convention,
    loadedAt,
    onClose,
    onSaved,
}: {
    readonly pair: RecordRow | undefined;
    readonly convention: RecordRow | undefined;
    readonly loadedAt?: number | undefined;
    readonly onClose: () => void;
    readonly onSaved?: (code: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const codeOf = (values: Readonly<Record<string, unknown>>): string =>
        pair === undefined
            ? `${show(values['base_currency'])}/${show(values['quote_currency'])}`
            : show(pair['pair_code']);
    return (
        <RecordDialog
            title={
                pair === undefined
                    ? t('refdata.pairs.addTitle')
                    : t('refdata.records.editTitle', { code: show(pair['pair_code']) })
            }
            resource={PAIRS}
            specs={PAIR_FORM}
            row={pair}
            {...(loadedAt !== undefined ? { loadedAt } : {})}
            initial={valuesOf(CONVENTION_FIELDS, convention)}
            complete={(typed) => ({
                pair_code: codeOf(typed),
                ...writeOf(
                    PAIR_FIELDS,
                    Object.fromEntries(
                        Object.entries(typed).map(([field, value]) => [field, show(value)]),
                    ),
                ),
            })}
            check={(values) =>
                values['base_currency'] !== '' &&
                values['base_currency'] === values['quote_currency']
                    ? [t('refdata.pairs.sameLegs')]
                    : []
            }
            note={(values) => (
                <p className="text-sm text-ink-muted">
                    {t('refdata.pairs.codeLine', {
                        code:
                            values['base_currency'] === '' || values['quote_currency'] === ''
                                ? '—'
                                : codeOf(values),
                    })}
                    {' · '}
                    {t('refdata.pairs.sample', {
                        rate: sampleRate(values['decimal_places'] ?? '') || '—',
                    })}
                </p>
            )}
            after={async (write, intent, typed) => {
                const conventionWrite = Object.fromEntries(
                    CONVENTION_FIELDS.map((spec) => [spec.field, typed[spec.field]]),
                );
                await api.saveRecord(CONVENTIONS, {
                    write: {
                        pair_code: show(write['pair_code']),
                        oresmd_uri: convention?.['oresmd_uri'] ?? null,
                        ...conventionWrite,
                    },
                    version: convention?.version ?? null,
                    ...(convention === undefined
                        ? { reasonCode: NEW_RECORD_REASON, commentary: '' }
                        : intent),
                });
            }}
            onClose={onClose}
            onSaved={(write) => onSaved?.(show(write['pair_code']))}
        />
    );
}

/** One pair: the pair and its convention, its settlement calendars, and the history of both. */
export function CurrencyPairPage(): ReactNode {
    const { code } = useParams();
    return (
        <RecordGate resource={PAIRS} recordKey={code ?? ''} listPath={pairPath()}>
            {(pair) => <PairWithConvention key={show(pair['pair_code'])} pair={pair} />}
        </RecordGate>
    );
}

/** Reads the pair's convention; a pair with none yet still opens, and says so. */
function PairWithConvention({ pair }: { readonly pair: RecordRow }): ReactNode {
    const { t } = useTranslation();
    const code = show(pair['pair_code']);
    const convention = useQuery({
        queryKey: ['records', CONVENTIONS, 'key', code],
        queryFn: async () => {
            try {
                return await api.record(CONVENTIONS, code);
            } catch (error) {
                if (error instanceof ApiFailure && error.status === 404) {
                    return null;
                }
                throw error;
            }
        },
    });
    if (convention.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (convention.isError) {
        return <Notice tone="error">{convention.error.message}</Notice>;
    }
    return <PairBody pair={pair} convention={convention.data ?? undefined} />;
}

function PairBody({
    pair,
    convention,
}: {
    readonly pair: RecordRow;
    readonly convention: RecordRow | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const registry = useRegistry();
    const code = show(pair['pair_code']);
    const may = useRecordPermissions(PAIRS);
    const { tab, bar } = useRecordTabs({
        label: code,
        tabs: ['details', 'calendars', 'history'],
    });
    const record = useRecord(PAIRS, code);
    const calendars = useRecords('calendars');
    const [editing, setEditing] = useState(false);
    const [removing, setRemoving] = useState(false);
    const [reverting, setReverting] = useState<HistoryVersion | null>(null);
    // The page never reloads by itself: a change the server tells of turns Refresh gold (ES-REFRESH-15).
    const pageKeys = [
        ['records', PAIRS, 'key', code],
        ['records', CONVENTIONS, 'key', code],
        ['records', CALENDARS],
        ['history', ENTITY, code],
        ['history', CONVENTION_ENTITY, code],
    ];
    const load = useLoadState(pageKeys);
    const watches = (registry.data ?? [])
        .filter((entry) => [PAIRS, CONVENTIONS, CALENDARS].includes(entry.key))
        .flatMap((entry) => (entry.events === undefined ? [] : [entry.events]));
    const news = useEntityChanges(watches, load);
    const refresh = (): void => {
        for (const key of pageKeys) {
            void queries.invalidateQueries({ queryKey: key });
        }
    };

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    ...useAreaRoot('refdata'),
                    { label: t('refdata.pairs.title'), to: pairPath() },
                    { label: code },
                ]}
                title={code}
                recordKey={code}
                version={pair.version}
                flag="pair"
                refresh={{ onClick: refresh, load, news }}
                onEdit={may.write ? () => setEditing(true) : undefined}
                onDelete={may.remove ? () => setRemoving(true) : undefined}
            />
            {bar}
            {tab === 'details' && (
                <div className="space-y-4">
                    <RecordDetails
                        specs={PAIR_FIELDS.filter((spec) => spec.kind.kind !== 'record')}
                        row={pair}
                        extra={(['base_currency', 'quote_currency'] as const).map((leg) => [
                            t(`refdata.fields.${leg}`),
                            <Link
                                className="text-accent hover:underline"
                                to={currencyPath(show(pair[leg]))}
                            >
                                <FlaggedCode source="currency" code={show(pair[leg])} />
                            </Link>,
                        ])}
                    />
                    <h3 className="text-sm font-medium">{t('refdata.pairs.convention')}</h3>
                    {convention === undefined ? (
                        <Notice tone="warn">{t('refdata.pairs.noConvention')}</Notice>
                    ) : (
                        <RecordDetails
                            specs={CONVENTION_FIELDS}
                            row={convention}
                            extra={[
                                [
                                    t('refdata.pairs.sampleRate'),
                                    sampleRate(show(convention['decimal_places'])),
                                ],
                            ]}
                        />
                    )}
                </div>
            )}
            {tab === 'calendars' && (
                <LinkPanel
                    title={t('refdata.pairs.calendars')}
                    junction={CALENDARS}
                    parentField="pair_code"
                    parentValue={code}
                    childField="calendar_code"
                    flag="calendar"
                    choices={(calendars.data ?? []).map((calendar) => ({
                        value: show(calendar['code']),
                        label: show(calendar['name']),
                    }))}
                />
            )}
            {tab === 'history' && (
                <RecordTimeline
                    entityType={ENTITY}
                    entityId={code}
                    parts={[
                        {
                            entityType: CONVENTION_ENTITY,
                            rows: convention === undefined ? [] : [convention],
                            id: (row) => show(row['pair_code']),
                            name: () => t('refdata.pairs.conventionHistory'),
                        },
                    ]}
                    {...(may.write ? { onRevert: setReverting } : {})}
                />
            )}
            {editing && (
                <PairDialog
                    pair={pair}
                    convention={convention}
                    loadedAt={record.dataUpdatedAt}
                    onClose={() => setEditing(false)}
                />
            )}
            {removing && (
                <RemoveRecordDialog
                    resource={PAIRS}
                    title={t('refdata.records.deleteTitle', { code })}
                    warning={t('refdata.pairs.removeWarning')}
                    recordKey={{ pair_code: code }}
                    version={pair.version}
                    before={async (intent) => {
                        if (convention !== undefined) {
                            await api.removeRecord(CONVENTIONS, {
                                key: { pair_code: code },
                                ...intent,
                            });
                        }
                    }}
                    onClose={() => setRemoving(false)}
                    onRemoved={() => void navigate(pairPath())}
                />
            )}
            {reverting !== null && (
                <RevertDialog
                    resource={PAIRS}
                    specs={PAIR_FIELDS}
                    row={pair}
                    version={reverting}
                    keep={['pair_code']}
                    onClose={() => setReverting(null)}
                />
            )}
        </div>
    );
}

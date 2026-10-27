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

import { useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { useNavigate, useParams } from 'react-router';
import type { HistoryVersion } from '@ores/wire-protocol/browser';
import { api, type RecordRow } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { useLoadState } from '../events/useLoadState.js';
import {
    OreExchangeActions,
    OreImportPage,
    type OreDocumentKind,
    type OreImportChoice,
    type OreImportKind,
} from '../ore/OreExchange.js';
import { RecordList, useRecordSource } from './RecordList.js';
import { RecordTimeline } from './RecordTimeline.js';
import {
    ClassifiedValue,
    LinkPanel,
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
    type FieldSpec,
} from './records.js';

const RESOURCE = 'currencies';
const ENTITY = 'ores.refdata.currency';
const LINKS = ['currency-countries', 'currency-calendars', 'currency-memberships'] as const;

/** The ORE document the currencies are: the first kind to carry the standard Import and Export pair. */
const ORE_DOCUMENT: OreDocumentKind = {
    resource: RESOURCE,
    fileName: 'currencyconfig.xml',
    rootElement: 'CurrencyConfig',
    listPath: '/refdata/currencies',
    importPath: '/refdata/currencies/import',
    words: 'refdata.currencies',
};

/** The values ORE's currency configuration takes for a currency's type; the document's own enumeration, not a tenant list. */
const ORE_CURRENCY_TYPES = ['Major', 'Metal', 'Crypto'] as const;

/** The currency fields a person edits, in form order, with the names the history gives them. */
export const CURRENCY_FIELDS: readonly FieldSpec[] = [
    {
        field: 'iso_code',
        history: 'ISO Code',
        group: 'refdata.currencies.fieldGroups.identity',
        kind: { kind: 'text', max: 3 },
        fixed: true,
        flag: 'currency',
    },
    {
        field: 'name',
        history: 'Name',
        group: 'refdata.currencies.fieldGroups.identity',
        kind: { kind: 'text' },
    },
    {
        field: 'numeric_code',
        history: 'Numeric Code',
        group: 'refdata.currencies.fieldGroups.identity',
        kind: { kind: 'text', max: 3 },
    },
    {
        field: 'image_id',
        history: 'Image ID',
        group: 'refdata.currencies.fieldGroups.identity',
        kind: { kind: 'image' },
        optional: true,
    },
    {
        field: 'symbol',
        history: 'Symbol',
        group: 'refdata.currencies.fieldGroups.display',
        kind: { kind: 'text', max: 20 },
    },
    {
        field: 'fraction_symbol',
        history: 'Fraction Symbol',
        group: 'refdata.currencies.fieldGroups.display',
        kind: { kind: 'text', max: 20 },
    },
    {
        field: 'fractions_per_unit',
        history: 'Fractions Per Unit',
        group: 'refdata.currencies.fieldGroups.display',
        kind: { kind: 'int' },
    },
    {
        field: 'rounding_type',
        history: 'Rounding Type',
        group: 'refdata.currencies.fieldGroups.display',
        kind: { kind: 'classification', list: 'rounding-type' },
    },
    {
        field: 'rounding_precision',
        history: 'Rounding Precision',
        group: 'refdata.currencies.fieldGroups.display',
        kind: { kind: 'int' },
    },
    {
        field: 'format',
        history: 'Format',
        group: 'refdata.currencies.fieldGroups.display',
        kind: { kind: 'text', max: 100 },
    },
    {
        field: 'monetary_nature',
        history: 'Monetary Nature',
        group: 'refdata.currencies.fieldGroups.market',
        kind: { kind: 'classification', list: 'monetary-nature' },
    },
    {
        field: 'market_tier',
        history: 'Market Tier',
        group: 'refdata.currencies.fieldGroups.market',
        kind: { kind: 'classification', list: 'currency-market-tier' },
    },
    {
        field: 'ore_currency_type',
        history: 'Ore Currency Type',
        group: 'refdata.currencies.fieldGroups.market',
        kind: { kind: 'choice', options: ORE_CURRENCY_TYPES },
        optional: true,
    },
    {
        field: 'spot_days',
        history: 'Spot Days',
        group: 'refdata.currencies.fieldGroups.market',
        kind: { kind: 'int' },
    },
    {
        field: 'day_basis',
        history: 'Day Basis',
        group: 'refdata.currencies.fieldGroups.market',
        kind: { kind: 'classification', list: 'day-counter' },
    },
    {
        field: 'base_precedence',
        history: 'Base Precedence',
        group: 'refdata.currencies.fieldGroups.market',
        kind: { kind: 'int' },
    },
];

export function currencyPath(code?: string): string {
    return code === undefined
        ? '/refdata/currencies'
        : `/refdata/currencies/${encodeURIComponent(code)}`;
}

/** The tenant's currencies: one page at a time, searched and sorted on the server. */
export function CurrenciesPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const [adding, setAdding] = useState(false);
    const source = useRecordSource(RESOURCE);
    const may = useRecordPermissions(RESOURCE);
    return (
        <>
            <RecordList
                source={source}
                actions={<OreExchangeActions kind={ORE_DOCUMENT} mayWrite={may.write} />}
                title={t('refdata.currencies.title')}
                lead={t('refdata.currencies.lead')}
                crumbs={[...useAreaRoot('refdata'), { label: t('refdata.currencies.title') }]}
                pathOf={(row) => currencyPath(show(row['iso_code']))}
                addLabel={t('refdata.currencies.add')}
                onAdd={() => setAdding(true)}
                columns={[
                    {
                        id: 'iso_code',
                        header: t('refdata.fields.iso_code'),
                        cell: (row) => show(row['iso_code']),
                        mono: true,
                        sort: 'iso_code',
                        flag: { source: 'currency', code: (row) => show(row['iso_code']) },
                    },
                    {
                        id: 'name',
                        header: t('refdata.fields.name'),
                        cell: (row) => show(row['name']),
                        sort: 'name',
                    },
                    {
                        id: 'symbol',
                        header: t('refdata.fields.symbol'),
                        cell: (row) => show(row['symbol']),
                    },
                    {
                        id: 'monetary_nature',
                        header: t('refdata.fields.monetary_nature'),
                        cell: (row) => (
                            <ClassifiedValue
                                list="monetary-nature"
                                code={show(row['monetary_nature'])}
                            />
                        ),
                        sort: 'monetary_nature',
                    },
                    {
                        id: 'market_tier',
                        header: t('refdata.fields.market_tier'),
                        cell: (row) => (
                            <ClassifiedValue
                                list="currency-market-tier"
                                code={show(row['market_tier'])}
                            />
                        ),
                        sort: 'market_tier',
                    },
                ]}
            />
            {adding && (
                <RecordDialog
                    title={t('refdata.currencies.addTitle')}
                    resource={RESOURCE}
                    specs={CURRENCY_FIELDS}
                    row={undefined}
                    onClose={() => setAdding(false)}
                    onSaved={(write) => void navigate(currencyPath(String(write['iso_code'])))}
                />
            )}
        </>
    );
}

/** What the review offers for a currency, the first being the default (RS-ORE-05). */
const CURRENCY_CHOICES: Record<string, readonly string[]> = {
    removed: ['keep', 'delete'],
    changed: ['take', 'keep'],
    added: ['take', 'skip'],
};

/**
 * The Import steps of the currencies, at their own address (RS-ORE-05).
 *
 * The currency document is the whole table, so the target is the tenant's
 * currencies and the steps are four: there is no configuration to choose.
 */
export function CurrencyImportPage(): ReactNode {
    const { t } = useTranslation();
    const choicesOf = (change: string): readonly OreImportChoice[] =>
        (CURRENCY_CHOICES[change] ?? []).map((id) => ({
            id,
            label: t(`refdata.ore.choices.${id}`),
        }));
    const kind: OreImportKind = {
        words: 'refdata.currencies',
        targetKind: 'table',
        rootElement: ORE_DOCUMENT.rootElement,
        listPath: ORE_DOCUMENT.listPath,
        choicesFor: (row) => choicesOf(row.change),
        preview: async (content) => {
            const result = await api.previewOreImport(ORE_DOCUMENT.resource, content);
            return {
                success: result.success,
                message: result.message,
                targetName: '',
                holds: {
                    counts: [
                        {
                            id: 'inFile',
                            label: t('refdata.ore.counts.inFile'),
                            value: result.in_file,
                        },
                        { id: 'held', label: t('refdata.ore.counts.held'), value: result.held },
                        {
                            id: 'changed',
                            label: t('refdata.ore.counts.changed'),
                            value: result.changed,
                        },
                        { id: 'added', label: t('refdata.ore.counts.added'), value: result.added },
                        {
                            id: 'removed',
                            label: t('refdata.ore.counts.removed'),
                            value: result.removed,
                        },
                    ],
                    lines: [],
                    note: t('refdata.ore.countsNote'),
                },
                rows: result.rows.map((row) => ({
                    key: row.iso_code,
                    change: row.change,
                    name: row.iso_code,
                    detail: row.name,
                    what: row.what,
                })),
                issues: result.issues,
                summary: '',
            };
        },
        write: async ({ content, choices, intent }) => {
            const result = await api.importOre(ORE_DOCUMENT.resource, {
                content,
                choices: Object.entries(choices).map(([iso_code, action]) => ({
                    iso_code,
                    action,
                })),
                reasonCode: intent.reason_code,
                commentary: intent.commentary,
            });
            return {
                success: result.success,
                message: result.message,
                errors: result.errors.map((error) => `${error.iso_code}: ${error.message}`),
                summary: t('refdata.ore.imported', {
                    added: String(result.added),
                    changed: String(result.changed),
                    deleted: String(result.deleted),
                    kept: String(result.kept),
                }),
            };
        },
    };
    return <OreImportPage kind={kind} />;
}

/**
 * One currency: its fields, then a tab for each kind of link (countries,
 * calendars and desk groups), then its history. The links keep no versions,
 * so only the currency itself has a history.
 */
export function CurrencyPage(): ReactNode {
    const { code } = useParams();
    return (
        <RecordGate resource={RESOURCE} recordKey={code ?? ''} listPath={currencyPath()}>
            {(row) => <CurrencyBody key={show(row['iso_code'])} row={row} />}
        </RecordGate>
    );
}

function CurrencyBody({ row }: { readonly row: RecordRow }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const registry = useRegistry();
    const code = show(row['iso_code']);
    const name = show(row['name']) || code;
    const may = useRecordPermissions(RESOURCE);
    const { tab, bar } = useRecordTabs({
        label: code,
        tabs: ['details', 'countries', 'calendars', 'groups', 'history'],
    });
    const record = useRecord(RESOURCE, code);
    const countries = useRecords('countries');
    const calendars = useRecords('calendars');
    const groups = useRecords('currency-groups');
    const [editing, setEditing] = useState(false);
    const [removing, setRemoving] = useState(false);
    const [reverting, setReverting] = useState<HistoryVersion | null>(null);
    const choices = (rows: readonly RecordRow[] | undefined, value: string, label: string) =>
        (rows ?? []).map((choice) => ({ value: show(choice[value]), label: show(choice[label]) }));
    // The page never reloads by itself: a change the server tells of turns Refresh gold (ES-REFRESH-15).
    const pageKeys = [
        ['records', RESOURCE, 'key', code],
        ...LINKS.map((junction) => ['records', junction]),
        ['history', ENTITY, code],
    ];
    const load = useLoadState(pageKeys);
    const watches = (registry.data ?? [])
        .filter(
            (entry) => entry.key === RESOURCE || (LINKS as readonly string[]).includes(entry.key),
        )
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
                    { label: t('refdata.currencies.title'), to: currencyPath() },
                    { label: code },
                ]}
                title={name}
                recordKey={code}
                version={row.version}
                flag="currency"
                refresh={{ onClick: refresh, load, news }}
                onEdit={may.write ? () => setEditing(true) : undefined}
                onDelete={may.remove ? () => setRemoving(true) : undefined}
            />
            {bar}
            {tab === 'details' && <RecordDetails specs={CURRENCY_FIELDS} row={row} />}
            {tab === 'countries' && (
                <LinkPanel
                    title={t('refdata.currencies.countries')}
                    junction="currency-countries"
                    parentField="currency_iso_code"
                    parentValue={code}
                    childField="country_alpha2_code"
                    flag="country"
                    choices={choices(countries.data, 'alpha2_code', 'name')}
                />
            )}
            {tab === 'calendars' && (
                <LinkPanel
                    title={t('refdata.currencies.calendars')}
                    junction="currency-calendars"
                    parentField="currency_iso_code"
                    parentValue={code}
                    childField="calendar_code"
                    flag="calendar"
                    choices={choices(calendars.data, 'code', 'name')}
                />
            )}
            {tab === 'groups' && (
                <LinkPanel
                    title={t('refdata.currencies.groups')}
                    junction="currency-memberships"
                    parentField="currency_iso_code"
                    parentValue={code}
                    childField="currency_group_code"
                    choices={choices(groups.data, 'code', 'name')}
                    pathOf={(group) => `/refdata/desk-groups/${encodeURIComponent(group)}`}
                />
            )}
            {tab === 'history' && (
                <RecordTimeline
                    entityType={ENTITY}
                    entityId={code}
                    {...(may.write ? { onRevert: setReverting } : {})}
                />
            )}
            {editing && (
                <RecordDialog
                    title={t('refdata.records.editTitle', { code: name })}
                    resource={RESOURCE}
                    specs={CURRENCY_FIELDS}
                    row={row}
                    loadedAt={record.dataUpdatedAt}
                    onClose={() => setEditing(false)}
                />
            )}
            {removing && (
                <RemoveRecordDialog
                    resource={RESOURCE}
                    title={t('refdata.records.deleteTitle', { code: name })}
                    warning={t('refdata.currencies.removeWarning')}
                    recordKey={{ iso_code: code }}
                    version={row.version}
                    onClose={() => setRemoving(false)}
                    onRemoved={() => void navigate(currencyPath())}
                />
            )}
            {reverting !== null && (
                <RevertDialog
                    resource={RESOURCE}
                    specs={CURRENCY_FIELDS}
                    row={row}
                    version={reverting}
                    onClose={() => setReverting(null)}
                />
            )}
        </div>
    );
}

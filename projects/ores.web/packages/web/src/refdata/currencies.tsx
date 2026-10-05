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
import type { HistoryVersion } from '@ores/wire-protocol/browser';
import type { RecordRow } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Notice } from '../ui/Primitives.js';
import { HistoryPanel } from './HistoryPanel.js';
import { RecordList, useRecordSource } from './RecordList.js';
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
    useRecordPermissions,
    useRecordTabs,
    useRecords,
    type FieldSpec,
} from './records.js';

const RESOURCE = 'currencies';

/** The currency fields a person edits, in form order, with the names the history gives them. */
export const CURRENCY_FIELDS: readonly FieldSpec[] = [
    {
        field: 'iso_code',
        history: 'ISO Code',
        kind: { kind: 'text', max: 3 },
        fixed: true,
        flag: 'currency',
    },
    { field: 'name', history: 'Name', kind: { kind: 'text' } },
    { field: 'numeric_code', history: 'Numeric Code', kind: { kind: 'text', max: 3 } },
    { field: 'symbol', history: 'Symbol', kind: { kind: 'text', max: 20 } },
    { field: 'fraction_symbol', history: 'Fraction Symbol', kind: { kind: 'text', max: 20 } },
    { field: 'fractions_per_unit', history: 'Fractions Per Unit', kind: { kind: 'int' } },
    {
        field: 'rounding_type',
        history: 'Rounding Type',
        kind: { kind: 'classification', list: 'rounding-type' },
    },
    { field: 'rounding_precision', history: 'Rounding Precision', kind: { kind: 'int' } },
    { field: 'format', history: 'Format', kind: { kind: 'text', max: 100 } },
    {
        field: 'monetary_nature',
        history: 'Monetary Nature',
        kind: { kind: 'classification', list: 'monetary-nature' },
    },
    {
        field: 'market_tier',
        history: 'Market Tier',
        kind: { kind: 'classification', list: 'currency-market-tier' },
    },
    {
        field: 'ore_currency_type',
        history: 'Ore Currency Type',
        kind: { kind: 'text', max: 100 },
        optional: true,
    },
    { field: 'spot_days', history: 'Spot Days', kind: { kind: 'int' } },
    { field: 'day_basis', history: 'Day Basis', kind: { kind: 'text', max: 20 } },
    { field: 'base_precedence', history: 'Base Precedence', kind: { kind: 'int' } },
    { field: 'image_id', history: 'Image ID', kind: { kind: 'image' }, optional: true },
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
    return (
        <>
            <RecordList
                source={source}
                title={t('refdata.currencies.title')}
                lead={t('refdata.currencies.lead')}
                crumbs={[
                    { label: t('refdata.area.title'), to: '/refdata' },
                    { label: t('refdata.currencies.title') },
                ]}
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

/**
 * One currency: its fields, then a tab for each kind of link (countries,
 * calendars and desk groups), then its history. The links keep no versions,
 * so only the currency itself has a history.
 */
export function CurrencyPage(): ReactNode {
    const { code } = useParams();
    return (
        <RecordGate resource={RESOURCE} recordKey={code ?? ''} listPath={currencyPath()}>
            {(row) => <CurrencyBody row={row} />}
        </RecordGate>
    );
}

function CurrencyBody({ row }: { readonly row: RecordRow }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const code = show(row['iso_code']);
    const may = useRecordPermissions(RESOURCE);
    const { tab, bar } = useRecordTabs({
        label: code,
        tabs: ['details', 'countries', 'calendars', 'groups', 'history'],
    });
    const countries = useRecords('countries');
    const calendars = useRecords('calendars');
    const groups = useRecords('currency-groups');
    const [editing, setEditing] = useState(false);
    const [removing, setRemoving] = useState(false);
    const [reverting, setReverting] = useState<HistoryVersion | null>(null);
    const choices = (rows: readonly RecordRow[] | undefined, value: string, label: string) =>
        (rows ?? []).map((choice) => ({ value: show(choice[value]), label: show(choice[label]) }));

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    { label: t('refdata.area.title'), to: '/refdata' },
                    { label: t('refdata.currencies.title'), to: currencyPath() },
                    { label: code },
                ]}
                title={show(row['name'])}
                recordKey={code}
                version={row.version}
                flag="currency"
                onEdit={may.write ? () => setEditing(true) : undefined}
                onDelete={may.remove ? () => setRemoving(true) : undefined}
            />
            {bar}
            {tab === 'details' && (
                <div className="space-y-4">
                    <RecordDetails specs={CURRENCY_FIELDS} row={row} />
                    <Notice tone="info">{t('refdata.currencies.oreExport')}</Notice>
                </div>
            )}
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
                <HistoryPanel
                    entityType="ores.refdata.currency"
                    entityId={code}
                    {...(may.write
                        ? { onRevert: (version: HistoryVersion) => setReverting(version) }
                        : {})}
                />
            )}
            {editing && (
                <RecordDialog
                    title={t('refdata.records.editTitle', { code })}
                    resource={RESOURCE}
                    specs={CURRENCY_FIELDS}
                    row={row}
                    onClose={() => setEditing(false)}
                />
            )}
            {removing && (
                <RemoveRecordDialog
                    resource={RESOURCE}
                    title={t('refdata.records.deleteTitle', { code })}
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

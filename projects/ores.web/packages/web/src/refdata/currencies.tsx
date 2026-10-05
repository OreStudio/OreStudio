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
import { Navigate, useNavigate, useParams } from 'react-router';
import type { HistoryVersion } from '@ores/wire-protocol/browser';
import type { RecordRow } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Input, Notice, PageHeader } from '../ui/Primitives.js';
import { HistoryPanel } from './HistoryPanel.js';
import {
    ClassifiedValue,
    LinkPanel,
    RecordDialog,
    RecordDetails,
    RecordTable,
    RemoveRecordDialog,
    RevertDialog,
    useRecordPermissions,
    useRecordTabs,
    show,
    useRecords,
    type FieldSpec,
} from './records.js';
import { Crumbs } from './shared.js';

const RESOURCE = 'currencies';

/** The currency fields a person edits, in form order, with the names the history gives them. */
export const CURRENCY_FIELDS: readonly FieldSpec[] = [
    { field: 'iso_code', history: 'ISO Code', kind: { kind: 'text', max: 3 }, fixed: true },
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
];

/** The fields the server requires that the form does not edit. */
const KEPT = ['image_id'];

export function currencyPath(code?: string): string {
    return code === undefined
        ? '/refdata/currencies'
        : `/refdata/currencies/${encodeURIComponent(code)}`;
}

/** The tenant's currencies, filtered by code or name, with an add for a person who may write. */
export function CurrenciesPage(): ReactNode {
    const { t } = useTranslation();
    const currencies = useRecords(RESOURCE);
    const may = useRecordPermissions(RESOURCE);
    const [filter, setFilter] = useState('');
    const [adding, setAdding] = useState(false);
    const navigate = useNavigate();

    if (currencies.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (currencies.isError) {
        return <Notice tone="error">{currencies.error.message}</Notice>;
    }
    const wanted = filter.trim().toLowerCase();
    const shown = currencies.data.filter(
        (row) =>
            wanted === '' ||
            `${show(row['iso_code'])} ${show(row['name'])}`.toLowerCase().includes(wanted),
    );
    return (
        <div className="space-y-4">
            <div>
                <Crumbs
                    parts={[
                        { label: t('refdata.area.title'), to: '/refdata' },
                        { label: t('refdata.currencies.title') },
                    ]}
                />
                <PageHeader
                    title={t('refdata.currencies.title')}
                    description={t('refdata.currencies.lead')}
                    actions={
                        may.write ? (
                            <Button variant="primary" onClick={() => setAdding(true)}>
                                {t('refdata.currencies.add')}
                            </Button>
                        ) : undefined
                    }
                />
            </div>
            <section className="overflow-hidden rounded-md border border-line">
                <div className="border-b border-line p-3">
                    <Input
                        type="search"
                        value={filter}
                        placeholder={t('refdata.records.filter')}
                        aria-label={t('refdata.records.filter')}
                        onChange={(event) => setFilter(event.target.value)}
                    />
                </div>
                <RecordTable
                    rows={shown}
                    pathOf={(row) => currencyPath(show(row['iso_code']))}
                    empty={t('refdata.records.noMatch')}
                    columns={[
                        {
                            header: t('refdata.fields.iso_code'),
                            cell: (row) => show(row['iso_code']),
                            mono: true,
                        },
                        { header: t('refdata.fields.name'), cell: (row) => show(row['name']) },
                        { header: t('refdata.fields.symbol'), cell: (row) => show(row['symbol']) },
                        {
                            header: t('refdata.fields.monetary_nature'),
                            cell: (row) => (
                                <ClassifiedValue
                                    list="monetary-nature"
                                    code={show(row['monetary_nature'])}
                                />
                            ),
                        },
                        {
                            header: t('refdata.fields.market_tier'),
                            cell: (row) => (
                                <ClassifiedValue
                                    list="currency-market-tier"
                                    code={show(row['market_tier'])}
                                />
                            ),
                        },
                    ]}
                />
                <div className="border-t border-line px-4 py-2 text-xs text-ink-muted">
                    {t('refdata.classifications.shown', {
                        shown: String(shown.length),
                        total: String(currencies.data.length),
                    })}
                </div>
            </section>
            {adding && (
                <RecordDialog
                    title={t('refdata.currencies.addTitle')}
                    resource={RESOURCE}
                    specs={CURRENCY_FIELDS}
                    row={undefined}
                    keep={KEPT}
                    onClose={() => setAdding(false)}
                    onSaved={(write) => void navigate(currencyPath(String(write['iso_code'])))}
                />
            )}
        </div>
    );
}

/**
 * One currency on one screen: the currency, its countries, calendars and desk
 * groups, and its history. The links keep no versions, so only the currency
 * itself has a history.
 */
export function CurrencyPage(): ReactNode {
    const { t } = useTranslation();
    const { code } = useParams();
    const currencies = useRecords(RESOURCE);
    if (currencies.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (currencies.isError) {
        return <Notice tone="error">{currencies.error.message}</Notice>;
    }
    const row = currencies.data.find((candidate) => candidate['iso_code'] === code);
    if (row === undefined) {
        return <Navigate to={currencyPath()} replace />;
    }
    return <CurrencyBody row={row} />;
}

function CurrencyBody({ row }: { readonly row: RecordRow }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const code = show(row['iso_code']);
    const may = useRecordPermissions(RESOURCE);
    const { tab, bar } = useRecordTabs({ label: code, tabs: ['details', 'history'] });
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
            <div>
                <Crumbs
                    parts={[
                        { label: t('refdata.area.title'), to: '/refdata' },
                        { label: t('refdata.currencies.title'), to: currencyPath() },
                        { label: code },
                    ]}
                />
                <PageHeader
                    title={show(row['name'])}
                    description={t('refdata.records.lead', { code, version: String(row.version) })}
                    actions={
                        may.write || may.remove ? (
                            <div className="flex gap-2">
                                {may.write && (
                                    <Button onClick={() => setEditing(true)}>
                                        {t('refdata.records.edit')}
                                    </Button>
                                )}
                                {may.remove && (
                                    <Button variant="danger" onClick={() => setRemoving(true)}>
                                        {t('refdata.records.remove')}
                                    </Button>
                                )}
                            </div>
                        ) : undefined
                    }
                />
            </div>
            {bar}
            {tab === 'details' ? (
                <div className="space-y-4">
                    <RecordDetails specs={CURRENCY_FIELDS} row={row} />
                    <div className="grid gap-4 lg:grid-cols-3">
                        <LinkPanel
                            title={t('refdata.currencies.countries')}
                            junction="currency-countries"
                            parentField="currency_iso_code"
                            parentValue={code}
                            childField="country_alpha2_code"
                            choices={choices(countries.data, 'alpha2_code', 'name')}
                        />
                        <LinkPanel
                            title={t('refdata.currencies.calendars')}
                            junction="currency-calendars"
                            parentField="currency_iso_code"
                            parentValue={code}
                            childField="calendar_code"
                            choices={choices(calendars.data, 'code', 'name')}
                        />
                        <LinkPanel
                            title={t('refdata.currencies.groups')}
                            junction="currency-memberships"
                            parentField="currency_iso_code"
                            parentValue={code}
                            childField="currency_group_code"
                            choices={choices(groups.data, 'code', 'name')}
                            pathOf={(group) => `/refdata/desk-groups/${encodeURIComponent(group)}`}
                        />
                    </div>
                    <Notice tone="info">{t('refdata.currencies.oreExport')}</Notice>
                </div>
            ) : (
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
                    keep={KEPT}
                    onClose={() => setEditing(false)}
                />
            )}
            {removing && (
                <RemoveRecordDialog
                    resource={RESOURCE}
                    title={t('refdata.records.removeTitle', { code })}
                    warning={t('refdata.currencies.removeWarning')}
                    recordKey={{ iso_code: code }}
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
                    keep={KEPT}
                    onClose={() => setReverting(null)}
                />
            )}
        </div>
    );
}

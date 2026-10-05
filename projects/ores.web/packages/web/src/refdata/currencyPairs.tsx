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

import { useMutation, useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link, Navigate, useNavigate, useParams } from 'react-router';
import type { HistoryVersion } from '@ores/wire-protocol/browser';
import { api, type RecordRow } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Dialog, Input, Notice, PageHeader } from '../ui/Primitives.js';
import { currencyPath } from './currencies.js';
import { HistoryPanel } from './HistoryPanel.js';
import {
    ClassifiedValue,
    FieldInput,
    LinkPanel,
    NEW_RECORD_REASON,
    RecordDetails,
    RecordTable,
    RemoveRecordDialog,
    RevertDialog,
    invalidFields,
    useRecordPermissions,
    useRecordTabs,
    show,
    useRecords,
    valuesOf,
    writeOf,
    type FieldSpec,
    type FieldValues,
} from './records.js';
import { Crumbs, ReasonFields, useReason } from './shared.js';

const PAIRS = 'currency-pairs';
const CONVENTIONS = 'currency-pair-conventions';

const PAIR_FIELDS: readonly FieldSpec[] = [
    {
        field: 'base_currency',
        history: 'Base Currency',
        kind: { kind: 'record', resource: 'currencies', value: 'iso_code', label: 'name' },
        fixed: true,
    },
    {
        field: 'quote_currency',
        history: 'Quote Currency',
        kind: { kind: 'record', resource: 'currencies', value: 'iso_code', label: 'name' },
        fixed: true,
    },
    {
        field: 'classification',
        history: 'Classification',
        kind: { kind: 'classification', list: 'currency-pair-classification' },
    },
];

const CONVENTION_FIELDS: readonly FieldSpec[] = [
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

/** The pairs, filtered by code, with their classification and display precision. */
export function CurrencyPairsPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const pairs = useRecords(PAIRS);
    const conventions = useRecords(CONVENTIONS);
    const may = useRecordPermissions(PAIRS);
    const [filter, setFilter] = useState('');
    const [adding, setAdding] = useState(false);

    if (pairs.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (pairs.isError) {
        return <Notice tone="error">{pairs.error.message}</Notice>;
    }
    const conventionOf = (code: string) =>
        conventions.data?.find((row) => row['pair_code'] === code);
    const wanted = filter.trim().toLowerCase();
    const shown = pairs.data.filter(
        (row) => wanted === '' || show(row['pair_code']).toLowerCase().includes(wanted),
    );
    return (
        <div className="space-y-4">
            <div>
                <Crumbs
                    parts={[
                        { label: t('refdata.area.title'), to: '/refdata' },
                        { label: t('refdata.pairs.title') },
                    ]}
                />
                <PageHeader
                    title={t('refdata.pairs.title')}
                    description={t('refdata.pairs.lead')}
                    actions={
                        may.write ? (
                            <Button variant="primary" onClick={() => setAdding(true)}>
                                {t('refdata.pairs.add')}
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
                    pathOf={(row) => pairPath(show(row['pair_code']))}
                    empty={t('refdata.records.noMatch')}
                    columns={[
                        {
                            header: t('refdata.fields.pair_code'),
                            cell: (row) => show(row['pair_code']),
                            mono: true,
                        },
                        {
                            header: t('refdata.fields.base_currency'),
                            cell: (row) => show(row['base_currency']),
                        },
                        {
                            header: t('refdata.fields.quote_currency'),
                            cell: (row) => show(row['quote_currency']),
                        },
                        {
                            header: t('refdata.fields.classification'),
                            cell: (row) => (
                                <ClassifiedValue
                                    list="currency-pair-classification"
                                    code={show(row['classification'])}
                                />
                            ),
                        },
                        {
                            header: t('refdata.fields.decimal_places'),
                            cell: (row) =>
                                show(conventionOf(show(row['pair_code']))?.['decimal_places']) ||
                                '—',
                        },
                    ]}
                />
                <div className="border-t border-line px-4 py-2 text-xs text-ink-muted">
                    {t('refdata.classifications.shown', {
                        shown: String(shown.length),
                        total: String(pairs.data.length),
                    })}
                </div>
            </section>
            {adding && (
                <PairDialog
                    pair={undefined}
                    convention={undefined}
                    onClose={() => setAdding(false)}
                    onSaved={(code) => void navigate(pairPath(code))}
                />
            )}
        </div>
    );
}

/**
 * Adds a pair or corrects one, with its convention, in one form with one save.
 *
 * The convention refers to the pair, so the pair is written first. The two
 * are separate writes; if the second fails, the form says so and stays open,
 * and a pair found later without its convention says so on its page.
 */
function PairDialog({
    pair,
    convention,
    onClose,
    onSaved,
}: {
    readonly pair: RecordRow | undefined;
    readonly convention: RecordRow | undefined;
    readonly onClose: () => void;
    readonly onSaved?: (code: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const reason = useReason('amend');
    const editing = pair !== undefined;
    const [values, setValues] = useState<FieldValues>({
        ...valuesOf(PAIR_FIELDS, pair),
        ...valuesOf(CONVENTION_FIELDS, convention),
    });
    const [commentary, setCommentary] = useState('');
    const base = values['base_currency'] ?? '';
    const quote = values['quote_currency'] ?? '';
    const code = editing ? show(pair['pair_code']) : `${base}/${quote}`;
    const sameLegs = !editing && base !== '' && base === quote;
    const invalid = [
        ...invalidFields(PAIR_FIELDS, values),
        ...invalidFields(CONVENTION_FIELDS, values),
    ];
    const missing = editing && reason.needsCommentary && commentary.trim() === '';
    const save = useMutation({
        mutationFn: async () => {
            const intent = editing
                ? { reasonCode: reason.code, commentary: commentary.trim() }
                : { reasonCode: NEW_RECORD_REASON, commentary: '' };
            await api.saveRecord(PAIRS, {
                write: { pair_code: code, ...writeOf(PAIR_FIELDS, values) },
                version: pair?.version ?? null,
                ...intent,
            });
            await api.saveRecord(CONVENTIONS, {
                write: { pair_code: code, ...writeOf(CONVENTION_FIELDS, values) },
                version: convention?.version ?? null,
                ...(convention === undefined
                    ? { reasonCode: NEW_RECORD_REASON, commentary: '' }
                    : intent),
            });
        },
        onSettled: async () => {
            await queries.invalidateQueries({ queryKey: ['records'] });
            await queries.invalidateQueries({ queryKey: ['history'] });
        },
        onSuccess: () => {
            onSaved?.(code);
            onClose();
        },
    });
    const field = (spec: FieldSpec) => (
        <FieldInput
            key={spec.field}
            spec={spec}
            value={values[spec.field] ?? ''}
            disabled={editing && spec.fixed === true}
            onChange={(value) => setValues({ ...values, [spec.field]: value })}
        />
    );

    return (
        <Dialog
            title={editing ? t('refdata.records.editTitle', { code }) : t('refdata.pairs.addTitle')}
            onClose={onClose}
            wide
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('refdata.records.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        pending={save.isPending}
                        disabled={
                            invalid.length > 0 ||
                            sameLegs ||
                            missing ||
                            (editing && reason.code === '')
                        }
                        onClick={() => save.mutate()}
                    >
                        {editing ? t('refdata.records.save') : t('refdata.records.add')}
                    </Button>
                </>
            }
        >
            <div className="space-y-3">
                <div className="grid gap-3 sm:grid-cols-3">{PAIR_FIELDS.map(field)}</div>
                <p className="text-sm text-ink-muted">
                    {t('refdata.pairs.codeLine', {
                        code: base === '' || quote === '' ? '—' : code,
                    })}
                </p>
                {sameLegs && <Notice tone="warn">{t('refdata.pairs.sameLegs')}</Notice>}
                <h3 className="pt-2 text-sm font-medium">{t('refdata.pairs.convention')}</h3>
                <div className="grid gap-3 sm:grid-cols-3">{CONVENTION_FIELDS.map(field)}</div>
                <p className="text-sm text-ink-muted">
                    {t('refdata.pairs.sample', {
                        rate: sampleRate(values['decimal_places'] ?? '') || '—',
                    })}
                </p>
                {editing ? (
                    <ReasonFields
                        reason={reason}
                        commentary={commentary}
                        onCommentary={setCommentary}
                        missing={missing}
                    />
                ) : (
                    <p className="text-xs text-ink-faint">
                        {t('refdata.classifications.newRecordNote')}
                    </p>
                )}
                {save.isError && <Notice tone="error">{save.error.message}</Notice>}
            </div>
        </Dialog>
    );
}

/** One pair: the pair and its convention, its settlement calendars, and the history of both. */
export function CurrencyPairPage(): ReactNode {
    const { t } = useTranslation();
    const { code } = useParams();
    const pairs = useRecords(PAIRS);
    const conventions = useRecords(CONVENTIONS);
    if (pairs.isPending || conventions.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (pairs.isError) {
        return <Notice tone="error">{pairs.error.message}</Notice>;
    }
    const pair = pairs.data.find((candidate) => candidate['pair_code'] === code);
    if (pair === undefined) {
        return <Navigate to={pairPath()} replace />;
    }
    const convention = conventions.data?.find((candidate) => candidate['pair_code'] === code);
    return <PairBody pair={pair} convention={convention} />;
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
    const code = show(pair['pair_code']);
    const may = useRecordPermissions(PAIRS);
    const { tab, bar } = useRecordTabs({ label: code, tabs: ['details', 'history'] });
    const calendars = useRecords('calendars');
    const [editing, setEditing] = useState(false);
    const [removing, setRemoving] = useState(false);
    const [reverting, setReverting] = useState<{
        readonly kind: 'pair' | 'convention';
        readonly version: HistoryVersion;
    } | null>(null);

    return (
        <div className="space-y-4">
            <div>
                <Crumbs
                    parts={[
                        { label: t('refdata.area.title'), to: '/refdata' },
                        { label: t('refdata.pairs.title'), to: pairPath() },
                        { label: code },
                    ]}
                />
                <PageHeader
                    title={code}
                    description={t('refdata.records.lead', { code, version: String(pair.version) })}
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
                    <RecordDetails
                        specs={PAIR_FIELDS.filter((spec) => spec.kind.kind !== 'record')}
                        row={pair}
                        extra={(['base_currency', 'quote_currency'] as const).map((leg) => [
                            t(`refdata.fields.${leg}`),
                            <Link
                                className="text-accent hover:underline"
                                to={currencyPath(show(pair[leg]))}
                            >
                                {show(pair[leg])}
                            </Link>,
                        ])}
                    />
                    {convention === undefined ? (
                        <Notice tone="warn">{t('refdata.pairs.noConvention')}</Notice>
                    ) : (
                        <RecordDetails
                            specs={CONVENTION_FIELDS}
                            row={convention}
                            extra={[
                                [
                                    t('refdata.pairs.sampleRate'),
                                    sampleRate(show(convention?.['decimal_places'])),
                                ],
                            ]}
                        />
                    )}
                    <LinkPanel
                        title={t('refdata.pairs.calendars')}
                        junction="pair-calendars"
                        parentField="pair_code"
                        parentValue={code}
                        childField="calendar_code"
                        choices={(calendars.data ?? []).map((calendar) => ({
                            value: show(calendar['code']),
                            label: show(calendar['name']),
                        }))}
                    />
                </div>
            ) : (
                <div className="space-y-6">
                    <section className="space-y-2">
                        <h3 className="text-sm font-medium">{t('refdata.pairs.pairHistory')}</h3>
                        <HistoryPanel
                            entityType="ores.refdata.currency_pair"
                            entityId={code}
                            {...(may.write
                                ? {
                                      onRevert: (version: HistoryVersion) =>
                                          setReverting({ kind: 'pair', version }),
                                  }
                                : {})}
                        />
                    </section>
                    {convention !== undefined && (
                        <section className="space-y-2">
                            <h3 className="text-sm font-medium">
                                {t('refdata.pairs.conventionHistory')}
                            </h3>
                            <HistoryPanel
                                entityType="ores.refdata.currency_pair_convention"
                                entityId={code}
                                {...(may.write
                                    ? {
                                          onRevert: (version: HistoryVersion) =>
                                              setReverting({ kind: 'convention', version }),
                                      }
                                    : {})}
                            />
                        </section>
                    )}
                </div>
            )}
            {editing && (
                <PairDialog pair={pair} convention={convention} onClose={() => setEditing(false)} />
            )}
            {removing && (
                <RemoveRecordDialog
                    resource={PAIRS}
                    title={t('refdata.records.removeTitle', { code })}
                    warning={t('refdata.pairs.removeWarning')}
                    recordKey={{ pair_code: code }}
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
            {reverting !== null &&
                (reverting.kind === 'pair' ? (
                    <RevertDialog
                        resource={PAIRS}
                        specs={PAIR_FIELDS}
                        row={pair}
                        version={reverting.version}
                        keep={['pair_code']}
                        onClose={() => setReverting(null)}
                    />
                ) : convention !== undefined ? (
                    <RevertDialog
                        resource={CONVENTIONS}
                        specs={CONVENTION_FIELDS}
                        row={convention}
                        version={reverting.version}
                        keep={['pair_code']}
                        onClose={() => setReverting(null)}
                    />
                ) : null)}
        </div>
    );
}

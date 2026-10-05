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

import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link, useNavigate, useParams } from 'react-router';
import type { HistoryVersion } from '@ores/wire-protocol/browser';
import { api, type RecordRow } from '../api/client.js';
import { ApiFailure } from '../api/transport.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Dialog, Notice } from '../ui/Primitives.js';
import { currencyPath } from './currencies.js';
import { HistoryPanel } from './HistoryPanel.js';
import { RecordList } from './RecordList.js';
import {
    ClassifiedValue,
    FieldInput,
    LeavingFooter,
    LinkPanel,
    NEW_RECORD_REASON,
    RecordDetails,
    RecordGate,
    RecordHeader,
    RemoveRecordDialog,
    RevertDialog,
    invalidFields,
    useCloseGuard,
    useRecordPermissions,
    useRecordTabs,
    show,
    useRecords,
    valuesOf,
    writeOf,
    type FieldSpec,
    type FieldValues,
} from './records.js';
import { ReasonFields, useReason } from './shared.js';

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

/** The pairs, one page at a time, searched and sorted on the server. */
export function CurrencyPairsPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const [adding, setAdding] = useState(false);
    return (
        <>
            <RecordList
                resource={PAIRS}
                title={t('refdata.pairs.title')}
                lead={t('refdata.pairs.lead')}
                crumbs={[
                    { label: t('refdata.area.title'), to: '/refdata' },
                    { label: t('refdata.pairs.title') },
                ]}
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
                    },
                    {
                        id: 'base_currency',
                        header: t('refdata.fields.base_currency'),
                        cell: (row) => show(row['base_currency']),
                        mono: true,
                        sort: 'base_currency',
                    },
                    {
                        id: 'quote_currency',
                        header: t('refdata.fields.quote_currency'),
                        cell: (row) => show(row['quote_currency']),
                        mono: true,
                        sort: 'quote_currency',
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
    const editing = pair !== undefined;
    const initial: FieldValues = {
        ...valuesOf(PAIR_FIELDS, pair),
        ...valuesOf(CONVENTION_FIELDS, convention),
    };
    const [values, setValues] = useState<FieldValues>(initial);
    const [commentary, setCommentary] = useState('');
    const changed = JSON.stringify(values) !== JSON.stringify(initial);
    const reason = useReason('amend', changed);
    const guard = useCloseGuard(changed || commentary.trim() !== '', onClose);
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
            onClose={guard.close}
            wide
            footer={
                guard.leaving ? (
                    <LeavingFooter onStay={guard.stay} onLeave={onClose} />
                ) : (
                    <>
                        <Button variant="ghost" icon="cancel" onClick={guard.close}>
                            {t('refdata.records.cancel')}
                        </Button>
                        <Button
                            variant="primary"
                            icon={editing ? 'save' : 'add'}
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
                )
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
                {guard.leaving && <Notice tone="warn">{t('refdata.records.unsaved')}</Notice>}
                {save.isError && <Notice tone="error">{save.error.message}</Notice>}
            </div>
        </Dialog>
    );
}

/** One pair: the pair and its convention, its settlement calendars, and the history of both. */
export function CurrencyPairPage(): ReactNode {
    const { code } = useParams();
    return (
        <RecordGate resource={PAIRS} recordKey={code ?? ''} listPath={pairPath()}>
            {(pair) => <PairWithConvention pair={pair} />}
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
    const code = show(pair['pair_code']);
    const may = useRecordPermissions(PAIRS);
    const { tab, bar } = useRecordTabs({
        label: code,
        tabs: ['details', 'calendars', 'history'],
    });
    const calendars = useRecords('calendars');
    const [editing, setEditing] = useState(false);
    const [removing, setRemoving] = useState(false);
    const [reverting, setReverting] = useState<{
        readonly kind: 'pair' | 'convention';
        readonly version: HistoryVersion;
    } | null>(null);

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    { label: t('refdata.area.title'), to: '/refdata' },
                    { label: t('refdata.pairs.title'), to: pairPath() },
                    { label: code },
                ]}
                title={code}
                recordKey={code}
                version={pair.version}
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
                </div>
            )}
            {tab === 'calendars' && (
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
            )}
            {tab === 'history' && (
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

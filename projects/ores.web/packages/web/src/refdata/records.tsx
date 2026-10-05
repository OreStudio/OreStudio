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
import { Navigate, useNavigate } from 'react-router';
import type { HistoryVersion } from '@ores/wire-protocol/browser';
import { api, type RecordRow } from '../api/client.js';
import { ApiFailure } from '../api/transport.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Dialog, Field, Input, Notice, PageHeader, Select } from '../ui/Primitives.js';
import { RelativeTime } from '../ui/Time.js';
import { useTabs } from '../ui/Tabs.js';
import { Flag, FlagOf, useFlags, type FlagSource } from '../images/flags.js';
import { ImageField } from '../images/ImageChooser.js';
import { imageUrl } from '../ui/Images.js';
import { RecordPicker, type Choice } from './RecordPicker.js';
import { HistoryPanel, fieldValue } from './HistoryPanel.js';
import { Crumbs, ReasonFields, RowLabel, useLabelCatalogue, useReason } from './shared.js';

/** The reason a new row is written with; no other reason applies to a new record. */
export const NEW_RECORD_REASON = 'system.new_record';

/** The reason a link is taken away with: it removes nothing but the link. */
const UNLINK_REASON = 'common.rectification';

/**
 * How one field of a record is edited and shown.
 *
 * A picker reads its choices from a classification list or another record
 * resource. A fixed field is the record's key: it is entered once and never
 * corrected. An optional field writes null when it is left empty; a blank
 * field writes an empty string. A field with `when` is asked for only while
 * the form's values meet it, and writes null otherwise.
 */
export type FieldKind =
    | { readonly kind: 'text'; readonly max?: number }
    | { readonly kind: 'int'; readonly min?: number; readonly max?: number }
    | { readonly kind: 'decimal' }
    | { readonly kind: 'bool' }
    | { readonly kind: 'image' }
    | { readonly kind: 'date' }
    | { readonly kind: 'choice'; readonly options: readonly string[]; readonly numeric?: boolean }
    | { readonly kind: 'classification'; readonly list: string }
    | {
          readonly kind: 'record';
          readonly resource: string;
          readonly value: string;
          readonly label: string;
      };

export interface FieldSpec {
    readonly field: string;
    readonly history: string;
    readonly kind: FieldKind;
    readonly fixed?: boolean;
    readonly optional?: boolean;
    readonly blank?: boolean;
    /** The field holds a code of this source, drawn with its flag. */
    readonly flag?: FlagSource;
    readonly when?: (values: FieldValues) => boolean;
}

function asked(spec: FieldSpec, values: FieldValues): boolean {
    return spec.when === undefined || spec.when(values);
}

export type FieldValues = Readonly<Record<string, string>>;

/** A field's value as text; an absent value is empty. */
export function show(value: unknown): string {
    return value === null || value === undefined ? '' : String(value);
}

/** The form's values for a row, or blank values for a new one. */
export function valuesOf(specs: readonly FieldSpec[], row: RecordRow | undefined): FieldValues {
    return Object.fromEntries(specs.map((spec) => [spec.field, show(row?.[spec.field])]));
}

/** The values an older version held, read by the names the server's history mapper gives them. */
export function valuesFromHistory(
    specs: readonly FieldSpec[],
    version: HistoryVersion,
): FieldValues {
    return Object.fromEntries(specs.map((spec) => [spec.field, fieldValue(version, spec.history)]));
}

/** A write from the form's values, typed as the server expects each field. */
export function writeOf(specs: readonly FieldSpec[], values: FieldValues): Record<string, unknown> {
    const write: Record<string, unknown> = {};
    for (const spec of specs) {
        const raw = (values[spec.field] ?? '').trim();
        if (!asked(spec, values) || (raw === '' && spec.optional === true)) {
            write[spec.field] = null;
            continue;
        }
        switch (spec.kind.kind) {
            case 'int':
                write[spec.field] = Number.parseInt(raw, 10);
                break;
            case 'choice':
                write[spec.field] = spec.kind.numeric === true ? Number.parseInt(raw, 10) : raw;
                break;
            case 'decimal':
                write[spec.field] = Number.parseFloat(raw);
                break;
            case 'bool':
                write[spec.field] = raw === 'true';
                break;
            default:
                write[spec.field] = raw;
        }
    }
    return write;
}

/**
 * The fields a write carries over from the row unchanged: fields the server
 * requires but the form does not edit, such as an image. A new row has none.
 */
function kept(
    keep: readonly string[] | undefined,
    row: RecordRow | undefined,
): Record<string, unknown> {
    return Object.fromEntries((keep ?? []).map((field) => [field, row?.[field] ?? null]));
}

/** The tabs of a record's page, titled from the record screens' words. */
export function useRecordTabs({
    label,
    tabs,
}: {
    readonly label: string;
    readonly tabs: readonly string[];
}): { readonly tab: string; readonly bar: ReactNode } {
    const { t } = useTranslation();
    return useTabs({ label, tabs, titleOf: (tab) => t(`refdata.records.tabs.${tab}`) });
}

export function useRegistry(): ReturnType<
    typeof useQuery<Awaited<ReturnType<typeof api.refdataRegistry>>>
> {
    return useQuery({ queryKey: ['refdata-registry'], queryFn: api.refdataRegistry });
}

/** One record, named by its key; the page of a record reads only that record. */
export function useRecord(resource: string, key: string) {
    return useQuery({
        queryKey: ['records', resource, 'key', key],
        queryFn: () => api.record(resource, key),
        retry: false,
    });
}

export function useRecords(resource: string, parent?: string) {
    return useQuery({
        queryKey: parent === undefined ? ['records', resource] : ['records', resource, parent],
        queryFn: () => api.records(resource, parent),
    });
}

/**
 * Whether the signed-in person may write and remove rows of one resource.
 * The server checks each write again; this only decides what a screen offers.
 */
export function useRecordPermissions(resource: string): {
    readonly write: boolean;
    readonly remove: boolean;
} {
    const registry = useRegistry();
    const access = useQuery({ queryKey: ['my-access'], queryFn: api.myAccess });
    const entry = registry.data?.find((candidate) => candidate.key === resource);
    const codes = new Set((access.data?.roles ?? []).flatMap((role) => role.permissionCodes));
    const holds = (code: string): boolean =>
        codes.has('*') || codes.has('refdata::*') || codes.has(code);
    if (entry === undefined || !entry.writable) {
        return { write: false, remove: false };
    }
    return { write: holds(entry.writePermission), remove: holds(entry.deletePermission) };
}

/** The choices of a picker: code and the words to show for it. */
function useChoices(kind: FieldKind, flag?: FlagSource): readonly Choice[] {
    const flags = useFlags();
    const classification = useQuery({
        queryKey: ['classifications', kind.kind === 'classification' ? kind.list : ''],
        queryFn: () => api.classificationRows(kind.kind === 'classification' ? kind.list : ''),
        enabled: kind.kind === 'classification',
    });
    const records = useQuery({
        queryKey: ['records', kind.kind === 'record' ? kind.resource : ''],
        queryFn: () => api.records(kind.kind === 'record' ? kind.resource : ''),
        enabled: kind.kind === 'record',
    });
    if (kind.kind === 'classification') {
        return (classification.data ?? []).map((row) => ({
            value: row.code,
            label: row.name === '' ? row.code : `${row.name} (${row.code})`,
        }));
    }
    if (kind.kind === 'record') {
        return (records.data ?? []).map((row) => {
            const value = show(row[kind.value]);
            return {
                value,
                label: `${show(row[kind.label])} (${value})`,
                image: flag === undefined || flag === 'pair' ? null : flags.flag(flag, value),
            };
        });
    }
    return [];
}

export function FieldInput({
    spec,
    value,
    disabled,
    onChange,
    error,
    onBlur,
}: {
    readonly spec: FieldSpec;
    readonly value: string;
    readonly disabled: boolean;
    readonly onChange: (value: string) => void;
    readonly error?: string | undefined;
    readonly onBlur?: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const choices = useChoices(spec.kind, spec.flag);
    const label = t(`refdata.fields.${spec.field}`);
    if (spec.kind.kind === 'image') {
        return <ImageField label={label} imageId={value} disabled={disabled} onChange={onChange} />;
    }
    if (spec.kind.kind === 'record') {
        return (
            <Field label={label} {...(error === undefined ? {} : { error })}>
                <RecordPicker
                    value={value}
                    choices={choices}
                    onChange={onChange}
                    disabled={disabled}
                    optional={spec.optional === true}
                    placeholder={t('refdata.records.linkChoose')}
                    label={label}
                    onBlur={onBlur}
                />
            </Field>
        );
    }
    if (spec.kind.kind === 'classification') {
        return (
            <Field label={label} {...(error === undefined ? {} : { error })}>
                <Select
                    value={value}
                    disabled={disabled}
                    onBlur={onBlur}
                    onChange={(event) => onChange(event.target.value)}
                >
                    {(spec.optional === true || value === '') && <option value="">—</option>}
                    {value !== '' && !choices.some((choice) => choice.value === value) && (
                        <option value={value}>{value}</option>
                    )}
                    {choices.map((choice) => (
                        <option key={choice.value} value={choice.value}>
                            {choice.label}
                        </option>
                    ))}
                </Select>
            </Field>
        );
    }
    if (spec.kind.kind === 'choice') {
        return (
            <Field label={label}>
                <Select
                    value={value}
                    disabled={disabled}
                    onChange={(event) => onChange(event.target.value)}
                >
                    {(spec.optional === true || value === '') && <option value="">—</option>}
                    {spec.kind.options.map((option) => (
                        <option key={option} value={option}>
                            {t(`refdata.choices.${spec.field}.${option}`)}
                        </option>
                    ))}
                </Select>
            </Field>
        );
    }
    if (spec.kind.kind === 'bool') {
        return (
            <Field label={label} {...(error === undefined ? {} : { error })}>
                <Select
                    value={value}
                    disabled={disabled}
                    onBlur={onBlur}
                    onChange={(event) => onChange(event.target.value)}
                >
                    {spec.optional === true && <option value="">—</option>}
                    <option value="true">{t('refdata.records.yes')}</option>
                    <option value="false">{t('refdata.records.no')}</option>
                </Select>
            </Field>
        );
    }
    const numeric = spec.kind.kind === 'int' || spec.kind.kind === 'decimal';
    return (
        <Field label={label} {...(error === undefined ? {} : { error })}>
            <Input
                value={value}
                disabled={disabled}
                onBlur={onBlur}
                type={spec.kind.kind === 'date' ? 'date' : undefined}
                inputMode={numeric ? 'decimal' : undefined}
                maxLength={spec.kind.kind === 'text' ? (spec.kind.max ?? 2000) : 40}
                onChange={(event) => onChange(event.target.value)}
            />
        </Field>
    );
}

/**
 * The values a form refuses: a required field left empty, or a number that is
 * not one or is out of its range. A field the form does not ask for is never refused.
 */
export function invalidFields(specs: readonly FieldSpec[], values: FieldValues): readonly string[] {
    return specs
        .filter((spec) => {
            if (!asked(spec, values)) {
                return false;
            }
            const raw = (values[spec.field] ?? '').trim();
            if (raw === '') {
                return spec.optional !== true && spec.blank !== true;
            }
            if (spec.kind.kind === 'int') {
                const number = Number.parseInt(raw, 10);
                return (
                    !/^-?[0-9]+$/.test(raw) ||
                    number < (spec.kind.min ?? Number.MIN_SAFE_INTEGER) ||
                    number > (spec.kind.max ?? Number.MAX_SAFE_INTEGER)
                );
            }
            if (spec.kind.kind === 'decimal') {
                return Number.isNaN(Number.parseFloat(raw));
            }
            return false;
        })
        .map((spec) => spec.field);
}

/**
 * Asks before a form with changes closes, as the record screen standard
 * requires. `close` asks once; the footer then offers to keep editing or to
 * close anyway.
 */
export function useCloseGuard(
    dirty: boolean,
    onClose: () => void,
): { readonly leaving: boolean; readonly close: () => void; readonly stay: () => void } {
    const [leaving, setLeaving] = useState(false);
    return {
        leaving,
        close: () => (dirty && !leaving ? setLeaving(true) : onClose()),
        stay: () => setLeaving(false),
    };
}

/** The footer a form shows while it asks whether to close with unsaved changes. */
export function LeavingFooter({
    onStay,
    onLeave,
}: {
    readonly onStay: () => void;
    readonly onLeave: () => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <>
            <Button variant="ghost" onClick={onStay}>
                {t('refdata.records.keepEditing')}
            </Button>
            <Button variant="danger" icon="cancel" onClick={onLeave}>
                {t('refdata.records.closeAnyway')}
            </Button>
        </>
    );
}

/**
 * Adds a record, or corrects one against the version read.
 *
 * A new record needs no reason chosen; it is written as a new record. A
 * correction needs one, and some reasons need a commentary too. `given`
 * holds the fields the screen sets rather than the person, such as the parent
 * a row belongs to; `initial` fills a new row's form. `after` runs once the
 * record is written, for a write that belongs with it.
 */
export function RecordDialog({
    title,
    resource,
    specs,
    row,
    keep,
    given,
    initial,
    onClose,
    onSaved,
    after,
}: {
    readonly title: string;
    readonly resource: string;
    readonly specs: readonly FieldSpec[];
    readonly row: RecordRow | undefined;
    readonly keep?: readonly string[];
    readonly given?: Readonly<Record<string, unknown>>;
    readonly initial?: FieldValues;
    readonly onClose: () => void;
    readonly onSaved?: (write: Readonly<Record<string, unknown>>) => void;
    readonly after?: (
        write: Readonly<Record<string, unknown>>,
        intent: { reasonCode: string; commentary: string },
    ) => Promise<void>;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [values, setValues] = useState<FieldValues>({ ...valuesOf(specs, row), ...initial });
    const [commentary, setCommentary] = useState('');
    const [touched, setTouched] = useState<ReadonlySet<string>>(new Set());
    const editing = row !== undefined;
    const changed =
        JSON.stringify(writeOf(specs, values)) !==
        JSON.stringify(writeOf(specs, valuesOf(specs, row)));
    const reason = useReason('amend', changed);
    const missing = editing && reason.needsCommentary && commentary.trim() === '';
    const invalid = invalidFields(specs, values);
    const dirty = editing
        ? changed || commentary.trim() !== ''
        : Object.values(values).some((value) => value !== '');
    const guard = useCloseGuard(dirty, onClose);
    const errorOf = (spec: FieldSpec): string | undefined => {
        if (!touched.has(spec.field) || !invalid.includes(spec.field)) {
            return undefined;
        }
        return (values[spec.field] ?? '').trim() === ''
            ? t('refdata.records.required')
            : t('refdata.records.invalidValue');
    };
    const save = useMutation({
        mutationFn: async () => {
            const write = { ...kept(keep, row), ...given, ...writeOf(specs, values) };
            const intent = editing
                ? { reasonCode: reason.code, commentary: commentary.trim() }
                : { reasonCode: NEW_RECORD_REASON, commentary: '' };
            await api.saveRecord(resource, { write, version: row?.version ?? null, ...intent });
            await after?.(write, intent);
            return write;
        },
        onSuccess: async (write) => {
            await queries.invalidateQueries({ queryKey: ['records'] });
            await queries.invalidateQueries({ queryKey: ['history'] });
            await queries.invalidateQueries({ queryKey: ['image-map'] });
            onSaved?.(write);
            onClose();
        },
    });

    return (
        <Dialog
            title={title}
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
                                invalid.length > 0 || missing || (editing && reason.code === '')
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
                <div className="grid gap-3 sm:grid-cols-2">
                    {specs
                        .filter((spec) => asked(spec, values))
                        .map((spec) => (
                            <FieldInput
                                key={spec.field}
                                spec={spec}
                                value={values[spec.field] ?? ''}
                                disabled={editing && spec.fixed === true}
                                onChange={(value) => setValues({ ...values, [spec.field]: value })}
                                onBlur={() => setTouched(new Set([...touched, spec.field]))}
                                error={errorOf(spec)}
                            />
                        ))}
                </div>
                {guard.leaving && <Notice tone="warn">{t('refdata.records.unsaved')}</Notice>}
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
                {invalid.length > 0 && (
                    <p className="text-xs text-ink-faint">
                        {t('refdata.records.needs', {
                            fields: invalid.map((field) => t(`refdata.fields.${field}`)).join(', '),
                        })}
                    </p>
                )}
                {save.isError && <Notice tone="error">{save.error.message}</Notice>}
            </div>
        </Dialog>
    );
}

/**
 * Who last changed a record, for whom, when and why, as the record screen
 * standard closes the Details tab with it. A record with no versions has none.
 */
export function LastChanged({ row }: { readonly row: RecordRow }): ReactNode {
    const { t } = useTranslation();
    const modified = show(row['modified_by']);
    const performed = show(row['performed_by']);
    const commentary = show(row['change_commentary']);
    if (show(row['recorded_at']) === '') {
        return null;
    }
    return (
        <p className="border-t border-line px-4 py-3 text-xs text-ink-muted">
            <span className="text-ink-faint">{t('refdata.records.lastChanged')}</span>{' '}
            {t('refdata.records.versionLabel', { version: String(row.version) })} ·{' '}
            {modified === performed || performed === ''
                ? modified
                : t('refdata.records.modifiedFor', { modified, performed })}{' '}
            · <RelativeTime at={show(row['recorded_at'])} /> ·{' '}
            <span className="font-mono">{show(row['change_reason_code'])}</span>
            {commentary !== '' && <> · “{commentary}”</>}
        </p>
    );
}

/** A record's fields, laid out as labelled values. Pickers show the label of their choice. */
export function RecordDetails({
    specs,
    row,
    extra,
}: {
    readonly specs: readonly FieldSpec[];
    readonly row: RecordRow;
    readonly extra?: readonly (readonly [string, ReactNode])[];
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="rounded-md border border-line bg-surface-raised">
            <dl className="grid gap-4 p-4 sm:grid-cols-2 lg:grid-cols-4">
                {specs.map((spec) => (
                    <div key={spec.field}>
                        <dt className="text-xs text-ink-faint">
                            {t(`refdata.fields.${spec.field}`)}
                        </dt>
                        <dd className="mt-0.5 text-sm">
                            <DetailValue spec={spec} value={row[spec.field]} />
                        </dd>
                    </div>
                ))}
                {(extra ?? []).map(([label, value]) => (
                    <div key={label}>
                        <dt className="text-xs text-ink-faint">{label}</dt>
                        <dd className="mt-0.5 text-sm">{value}</dd>
                    </div>
                ))}
            </dl>
            <LastChanged row={row} />
        </section>
    );
}

function DetailValue({
    spec,
    value,
}: {
    readonly spec: FieldSpec;
    readonly value: unknown;
}): ReactNode {
    const { t } = useTranslation();
    const text = show(value);
    if (text === '') {
        return <span className="text-ink-faint">—</span>;
    }
    if (spec.kind.kind === 'bool') {
        return <>{text === 'true' ? t('refdata.records.yes') : t('refdata.records.no')}</>;
    }
    if (spec.kind.kind === 'classification') {
        return <ClassifiedValue list={spec.kind.list} code={text} />;
    }
    if (spec.kind.kind === 'image') {
        return <Flag src={imageUrl(text)} size="lg" />;
    }
    if (spec.flag !== undefined) {
        return (
            <span className="inline-flex items-center gap-2">
                <FlagOf source={spec.flag} code={text} />
                {text}
            </span>
        );
    }
    if (spec.kind.kind === 'choice') {
        return <>{t(`refdata.choices.${spec.field}.${text}`)}</>;
    }
    return <>{text}</>;
}

/** A code of a classification list, drawn as its label when the list has labels. */
export function ClassifiedValue({
    list,
    code,
}: {
    readonly list: string;
    readonly code: string;
}): ReactNode {
    const { byCode } = useLabelCatalogue();
    const rows = useQuery({
        queryKey: ['classifications', list],
        queryFn: () => api.classificationRows(list),
    });
    const row = rows.data?.find((candidate) => candidate.code === code);
    if (row?.labelCode != null) {
        return <RowLabel labelCode={row.labelCode} byCode={byCode} />;
    }
    return <>{row?.name !== undefined && row.name !== '' ? row.name : code}</>;
}

/** A table of records; a row opens its own page. */
export function RecordTable({
    rows,
    columns,
    pathOf,
    empty,
}: {
    readonly rows: readonly RecordRow[];
    readonly columns: readonly {
        readonly header: string;
        readonly cell: (row: RecordRow) => ReactNode;
        readonly mono?: boolean;
    }[];
    readonly pathOf: (row: RecordRow) => string;
    readonly empty: string;
}): ReactNode {
    const navigate = useNavigate();
    return (
        <div className="overflow-x-auto">
            <table className="w-full text-left text-sm">
                <thead>
                    <tr className="border-b border-line text-xs text-ink-muted">
                        {columns.map((column) => (
                            <th key={column.header} className="px-4 py-2 font-medium">
                                {column.header}
                            </th>
                        ))}
                    </tr>
                </thead>
                <tbody>
                    {rows.length === 0 && (
                        <tr>
                            <td colSpan={columns.length} className="px-4 py-3 text-ink-muted">
                                {empty}
                            </td>
                        </tr>
                    )}
                    {rows.map((row) => (
                        <tr
                            key={pathOf(row)}
                            className="cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover"
                            onClick={() => void navigate(pathOf(row))}
                        >
                            {columns.map((column) => (
                                <td
                                    key={column.header}
                                    className={
                                        column.mono === true
                                            ? 'px-4 py-2 font-mono text-xs text-ink-muted'
                                            : 'px-4 py-2'
                                    }
                                >
                                    {column.cell(row)}
                                </td>
                            ))}
                        </tr>
                    ))}
                </tbody>
            </table>
        </div>
    );
}

/**
 * The links of one record to another kind, such as a currency's countries.
 *
 * The links are a junction with no versions, so the panel is a plain list:
 * a link is added or taken away, never corrected. A junction that cannot be
 * read by this parent is read whole and filtered here, with `readAll`.
 */
export function LinkPanel({
    title,
    junction,
    parentField,
    parentValue,
    childField,
    choices,
    pathOf,
    readAll,
    flag,
}: {
    readonly title: string;
    readonly junction: string;
    readonly parentField: string;
    readonly parentValue: string;
    readonly childField: string;
    readonly choices: readonly { readonly value: string; readonly label: string }[];
    readonly pathOf?: (child: string) => string;
    readonly readAll?: boolean;
    readonly flag?: Exclude<FlagSource, 'pair'>;
}): ReactNode {
    const flags = useFlags();
    const { t } = useTranslation();
    const queries = useQueryClient();
    const navigate = useNavigate();
    const may = useRecordPermissions(junction);
    const links = useRecords(junction, readAll === true ? undefined : parentValue);
    const rows = (links.data ?? []).filter((row) => show(row[parentField]) === parentValue);
    const [adding, setAdding] = useState('');
    const linked = new Set(rows.map((row) => show(row[childField])));
    const name = (code: string): string =>
        choices.find((choice) => choice.value === code)?.label ?? code;
    const refresh = async (): Promise<void> => {
        await queries.invalidateQueries({ queryKey: ['records', junction] });
    };
    const add = useMutation({
        mutationFn: () =>
            api.saveRecord(junction, {
                write: { [parentField]: parentValue, [childField]: adding },
                version: null,
                reasonCode: NEW_RECORD_REASON,
                commentary: '',
            }),
        onSuccess: async () => {
            setAdding('');
            await refresh();
        },
    });
    const remove = useMutation({
        mutationFn: (child: string) =>
            api.removeRecord(junction, {
                key: { [parentField]: parentValue, [childField]: child },
                reasonCode: UNLINK_REASON,
                commentary: '',
            }),
        onSuccess: refresh,
    });

    return (
        <section className="rounded-md border border-line">
            <h3 className="border-b border-line px-4 py-2 text-sm font-medium">{title}</h3>
            {links.isError && <Notice tone="error">{links.error.message}</Notice>}
            <ul>
                {rows.length === 0 && (
                    <li className="px-4 py-2 text-sm text-ink-muted">
                        {t('refdata.records.noLinks')}
                    </li>
                )}
                {rows.map((row) => {
                    const child = show(row[childField]);
                    return (
                        <li
                            key={child}
                            className="flex items-center gap-2 border-b border-line-subtle px-4 py-1.5 text-sm last:border-b-0"
                        >
                            {flag !== undefined && <FlagOf source={flag} code={child} />}
                            <span className="font-mono text-xs text-ink-muted">{child}</span>
                            {pathOf === undefined ? (
                                <span className="flex-1">{name(child)}</span>
                            ) : (
                                <button
                                    type="button"
                                    className="flex-1 text-left hover:underline"
                                    onClick={() => void navigate(pathOf(child))}
                                >
                                    {name(child)}
                                </button>
                            )}
                            {may.remove && (
                                <Button
                                    size="sm"
                                    variant="ghost"
                                    icon="remove"
                                    aria-label={t('refdata.records.unlink', { name: name(child) })}
                                    pending={remove.isPending && remove.variables === child}
                                    onClick={() => remove.mutate(child)}
                                >
                                    {t('refdata.records.removeLink')}
                                </Button>
                            )}
                        </li>
                    );
                })}
            </ul>
            {may.write && (
                <div className="flex gap-2 border-t border-line p-2">
                    <div className="flex-1">
                        <RecordPicker
                            value={adding}
                            choices={choices
                                .filter((choice) => !linked.has(choice.value))
                                .map((choice) => ({
                                    ...choice,
                                    image:
                                        flag === undefined ? null : flags.flag(flag, choice.value),
                                }))}
                            onChange={setAdding}
                            placeholder={t('refdata.records.linkChoose')}
                            label={t('refdata.records.linkChoose')}
                        />
                    </div>
                    <Button
                        size="sm"
                        icon="add"
                        disabled={adding === ''}
                        pending={add.isPending}
                        onClick={() => add.mutate()}
                    >
                        {t('refdata.records.link')}
                    </Button>
                </div>
            )}
            {(add.isError || remove.isError) && (
                <Notice tone="error">{(add.error ?? remove.error)?.message}</Notice>
            )}
        </section>
    );
}

/**
 * Writes an older version's values back as a new version, against the
 * record's current version, so a change made since is refused rather than
 * lost.
 */
export function RevertDialog({
    resource,
    specs,
    row,
    version,
    keep,
    onClose,
}: {
    readonly resource: string;
    readonly specs: readonly FieldSpec[];
    readonly row: RecordRow;
    readonly version: HistoryVersion;
    readonly keep?: readonly string[];
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const reason = useReason('amend');
    const revert = useMutation({
        mutationFn: () =>
            api.saveRecord(resource, {
                write: { ...kept(keep, row), ...writeOf(specs, valuesFromHistory(specs, version)) },
                version: row.version,
                reasonCode: reason.code,
                commentary: t('refdata.classifications.revertCommentary', {
                    version: String(version.version),
                }),
            }),
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['records'] });
            await queries.invalidateQueries({ queryKey: ['history'] });
            await queries.invalidateQueries({ queryKey: ['image-map'] });
            onClose();
        },
    });
    return (
        <Dialog
            title={t('refdata.records.revertTitle', { version: String(version.version) })}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" icon="cancel" onClick={onClose}>
                        {t('refdata.records.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        icon="revert"
                        pending={revert.isPending}
                        disabled={reason.code === ''}
                        onClick={() => revert.mutate()}
                    >
                        {t('history.revert')}
                    </Button>
                </>
            }
        >
            <div className="space-y-3">
                <p className="text-sm">
                    {t('refdata.records.revertBody', {
                        from: String(row.version),
                        to: String(version.version),
                    })}
                </p>
                <Field label={t('refdata.classifications.reason')}>
                    <Select
                        value={reason.code}
                        onChange={(event) => reason.setCode(event.target.value)}
                    >
                        {reason.reasons.map((choice) => (
                            <option key={choice.code} value={choice.code}>
                                {choice.description}
                            </option>
                        ))}
                    </Select>
                </Field>
                {revert.isError && <Notice tone="error">{revert.error.message}</Notice>}
            </div>
        </Dialog>
    );
}

/** Removes a record against the version read, so a change made since is refused rather than lost. */
export function RemoveRecordDialog({
    resource,
    title,
    warning,
    recordKey,
    version,
    onClose,
    onRemoved,
    before,
}: {
    readonly resource: string;
    readonly title: string;
    readonly warning: string;
    readonly recordKey: Readonly<Record<string, string>>;
    readonly version: number;
    readonly onClose: () => void;
    readonly onRemoved: () => void;
    readonly before?: (intent: { reasonCode: string; commentary: string }) => Promise<void>;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const reason = useReason('delete');
    const [commentary, setCommentary] = useState('');
    const missing = reason.needsCommentary && commentary.trim() === '';
    const remove = useMutation({
        mutationFn: async () => {
            const intent = { reasonCode: reason.code, commentary: commentary.trim() };
            await before?.(intent);
            await api.removeRecord(resource, { key: recordKey, version, ...intent });
        },
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['records'] });
            onRemoved();
        },
    });
    return (
        <Dialog
            title={title}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" icon="cancel" onClick={onClose}>
                        {t('refdata.records.cancel')}
                    </Button>
                    <Button
                        variant="danger"
                        icon="delete"
                        pending={remove.isPending}
                        disabled={reason.code === '' || missing}
                        onClick={() => remove.mutate()}
                    >
                        {t('refdata.records.delete')}
                    </Button>
                </>
            }
        >
            <div className="space-y-3">
                <Notice tone="warn">{warning}</Notice>
                <ReasonFields
                    reason={reason}
                    commentary={commentary}
                    onCommentary={setCommentary}
                    missing={missing}
                />
                {remove.isError && <Notice tone="error">{remove.error.message}</Notice>}
            </div>
        </Dialog>
    );
}

/**
 * Reads one record by its key and draws it, or says it is loading, or says why
 * it failed. A key with no record returns to the list.
 */
export function RecordGate({
    resource,
    recordKey,
    listPath,
    children,
}: {
    readonly resource: string;
    readonly recordKey: string;
    readonly listPath: string;
    readonly children: (row: RecordRow) => ReactNode;
}): ReactNode {
    const { t } = useTranslation();
    const record = useRecord(resource, recordKey);
    if (record.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (record.isError) {
        return record.error instanceof ApiFailure && record.error.status === 404 ? (
            <Navigate to={listPath} replace />
        ) : (
            <Notice tone="error">{record.error.message}</Notice>
        );
    }
    return <>{children(record.data)}</>;
}

/**
 * The head of a record page: the trail, the record's name, its key and
 * version, and its actions in the standard order, the kind's own first, then
 * Edit, then Delete. An action the person may not take is not drawn.
 */
export function RecordHeader({
    crumbs,
    title,
    recordKey,
    version,
    own,
    onEdit,
    onDelete,
    flag,
    mark,
}: {
    readonly crumbs: readonly { readonly label: string; readonly to?: string }[];
    readonly title: string;
    readonly recordKey: string;
    readonly version: number;
    readonly own?: ReactNode;
    readonly onEdit?: (() => void) | undefined;
    readonly onDelete?: (() => void) | undefined;
    /** The record's own flag, drawn before its name. */
    readonly flag?: FlagSource;
    /** A picture drawn before the name when the record has no flag, such as a person's. */
    readonly mark?: ReactNode;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <div>
            <Crumbs parts={crumbs} />
            <PageHeader
                title={title}
                {...(flag !== undefined
                    ? { mark: <FlagOf source={flag} code={recordKey} size="lg" /> }
                    : mark !== undefined
                      ? { mark }
                      : {})}
                description={t('refdata.records.lead', {
                    code: recordKey,
                    version: String(version),
                })}
                actions={
                    <div className="flex gap-2">
                        {own}
                        {onEdit !== undefined && (
                            <Button icon="edit" onClick={onEdit}>
                                {t('refdata.records.edit')}
                            </Button>
                        )}
                        {onDelete !== undefined && (
                            <Button variant="danger" icon="delete" onClick={onDelete}>
                                {t('refdata.records.delete')}
                            </Button>
                        )}
                    </div>
                }
            />
        </div>
    );
}

/**
 * The parts of a record: rows that belong to one parent and keep their own
 * versions, such as a calendar's exceptions. The table lists them in their
 * natural order, each with History, Edit and Delete, and Add sits under it.
 * Every parts panel is this component, as the record screen standard sets it.
 */
export function PartsPanel({
    resource,
    entityType,
    specs,
    parentField,
    parent,
    rows,
    error,
    columns,
    sort,
    keep,
    initial,
    lead,
    titles,
}: {
    readonly resource: string;
    readonly entityType: string;
    readonly specs: readonly FieldSpec[];
    readonly parentField: string;
    readonly parent: string;
    readonly rows: readonly RecordRow[];
    readonly error: string | undefined;
    readonly columns: readonly {
        readonly header: string;
        readonly cell: (row: RecordRow) => ReactNode;
        readonly mono?: boolean;
    }[];
    readonly sort: (row: RecordRow) => string;
    readonly keep?: readonly string[];
    readonly initial?: Readonly<Record<string, string>>;
    readonly lead?: string;
    /** How the dialogs name one part: adding it, opening it, and deleting it. */
    readonly titles: { readonly add: string; readonly one: string; readonly remove: string };
}): ReactNode {
    const { t } = useTranslation();
    const may = useRecordPermissions(resource);
    const [open, setOpen] = useState<{
        readonly row: RecordRow | undefined;
        readonly id: string;
        readonly view: 'edit' | 'history' | 'remove';
    } | null>(null);
    const [reverting, setReverting] = useState<{
        readonly row: RecordRow;
        readonly version: HistoryVersion;
    } | null>(null);
    const kept = ['id', parentField, ...(keep ?? [])];
    const sorted = [...rows].sort((a, b) => sort(a).localeCompare(sort(b)));

    return (
        <section className="space-y-3">
            {lead !== undefined && <p className="text-sm text-ink-muted">{lead}</p>}
            {error !== undefined && <Notice tone="error">{error}</Notice>}
            <div className="overflow-x-auto rounded-md border border-line">
                <table className="w-full text-left text-sm">
                    <thead>
                        <tr className="border-b border-line text-xs text-ink-muted">
                            {columns.map((column) => (
                                <th key={column.header} className="px-4 py-2 font-medium">
                                    {column.header}
                                </th>
                            ))}
                            <th className="px-4 py-2" />
                        </tr>
                    </thead>
                    <tbody>
                        {sorted.length === 0 && (
                            <tr>
                                <td
                                    colSpan={columns.length + 1}
                                    className="px-4 py-3 text-ink-muted"
                                >
                                    {t('refdata.records.noLinks')}
                                </td>
                            </tr>
                        )}
                        {sorted.map((row) => (
                            <tr
                                key={show(row['id'])}
                                className="border-b border-line-subtle last:border-b-0"
                            >
                                {columns.map((column) => (
                                    <td
                                        key={column.header}
                                        className={
                                            column.mono === true
                                                ? 'px-4 py-2 font-mono text-xs text-ink-muted'
                                                : 'px-4 py-2'
                                        }
                                    >
                                        {column.cell(row)}
                                    </td>
                                ))}
                                <td className="px-4 py-1.5 text-right whitespace-nowrap">
                                    <Button
                                        size="sm"
                                        variant="ghost"
                                        icon="history"
                                        onClick={() =>
                                            setOpen({ row, id: show(row['id']), view: 'history' })
                                        }
                                    >
                                        {t('refdata.records.tabs.history')}
                                    </Button>
                                    {may.write && (
                                        <Button
                                            size="sm"
                                            variant="ghost"
                                            icon="edit"
                                            onClick={() =>
                                                setOpen({ row, id: show(row['id']), view: 'edit' })
                                            }
                                        >
                                            {t('refdata.records.edit')}
                                        </Button>
                                    )}
                                    {may.remove && (
                                        <Button
                                            size="sm"
                                            variant="ghost"
                                            icon="delete"
                                            onClick={() =>
                                                setOpen({
                                                    row,
                                                    id: show(row['id']),
                                                    view: 'remove',
                                                })
                                            }
                                        >
                                            {t('refdata.records.delete')}
                                        </Button>
                                    )}
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>
            {may.write && (
                <Button
                    icon="add"
                    onClick={() =>
                        setOpen({ row: undefined, id: crypto.randomUUID(), view: 'edit' })
                    }
                >
                    {t('refdata.records.add')}
                </Button>
            )}
            {open?.view === 'edit' && (
                <RecordDialog
                    title={open.row === undefined ? titles.add : titles.one}
                    resource={resource}
                    specs={specs}
                    row={open.row}
                    keep={kept}
                    given={{ id: open.id, [parentField]: parent }}
                    {...(open.row === undefined && initial !== undefined ? { initial } : {})}
                    onClose={() => setOpen(null)}
                />
            )}
            {open?.view === 'history' && open.row !== undefined && (
                <Dialog title={titles.one} onClose={() => setOpen(null)} wide>
                    <HistoryPanel
                        entityType={entityType}
                        entityId={open.id}
                        {...(may.write
                            ? {
                                  onRevert: (version: HistoryVersion) => {
                                      const row = open.row;
                                      if (row !== undefined) {
                                          setReverting({ row, version });
                                          setOpen(null);
                                      }
                                  },
                              }
                            : {})}
                    />
                </Dialog>
            )}
            {open?.view === 'remove' && open.row !== undefined && (
                <RemoveRecordDialog
                    resource={resource}
                    title={titles.remove}
                    warning={t('refdata.records.deletePartWarning')}
                    recordKey={{ id: open.id }}
                    version={open.row.version}
                    onClose={() => setOpen(null)}
                    onRemoved={() => setOpen(null)}
                />
            )}
            {reverting !== null && (
                <RevertDialog
                    resource={resource}
                    specs={specs}
                    row={reverting.row}
                    version={reverting.version}
                    keep={kept}
                    onClose={() => setReverting(null)}
                />
            )}
        </section>
    );
}

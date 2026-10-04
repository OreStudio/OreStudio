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
import { useEffect, useState, type ReactNode } from 'react';
import { useNavigate, useParams } from 'react-router';
import type {
    ClassificationList,
    ClassificationRow,
    HistoryVersion,
} from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import {
    Button,
    Dialog,
    Field,
    Input,
    Notice,
    PageHeader,
    Select,
    Tag,
    cx,
} from '../ui/Primitives.js';
import { HistoryPanel } from './HistoryPanel.js';

/** The reason a new row is written with; no other reason applies to a new record. */
const NEW_RECORD_REASON = 'system.new_record';

/** The reason a reorder is written with: it changes no row's meaning. */
const REORDER_REASON = 'common.non_material_update';

/** The topics in screen order. A list's topic is one of these. */
const TOPICS = ['currencies', 'calendars', 'parties', 'books', 'products', 'tenors', 'market-data'];

/**
 * The short code lists that classify reference data, maintained on one screen.
 *
 * The lists have no goal of their own, so one plain list with its edit form
 * serves all 28. The person picks the list by topic; the address carries the
 * list, so a picker elsewhere can link straight to the list it reads.
 */
export function ClassificationsPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const { list: listKey } = useParams();
    const lists = useQuery({ queryKey: ['classifications'], queryFn: api.classificationLists });

    if (lists.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (lists.isError) {
        return <Notice tone="error">{lists.error.message}</Notice>;
    }
    const chosen = lists.data.find((list) => list.key === listKey);

    return (
        <div>
            <PageHeader
                title={t('refdata.classifications.title')}
                description={t('refdata.classifications.lead')}
            />
            <div className="grid gap-6 md:grid-cols-[14rem_1fr]">
                <nav aria-label={t('refdata.classifications.title')}>
                    {TOPICS.map((topic) => (
                        <div key={topic} className="mb-3">
                            <h2 className="mb-1 text-xs font-semibold uppercase text-ink-faint">
                                {t(`refdata.classifications.topics.${topic}`)}
                            </h2>
                            <ul>
                                {lists.data
                                    .filter((list) => list.topic === topic)
                                    .map((list) => (
                                        <li key={list.key}>
                                            <button
                                                type="button"
                                                aria-current={
                                                    list.key === listKey ? 'page' : undefined
                                                }
                                                className={cx(
                                                    'w-full rounded px-2 py-1 text-left text-sm hover:bg-surface-hover',
                                                    list.key === listKey &&
                                                        'bg-surface-hover font-medium',
                                                )}
                                                onClick={() =>
                                                    void navigate(
                                                        `/classifications/${encodeURIComponent(list.key)}`,
                                                    )
                                                }
                                            >
                                                {t(`refdata.classifications.lists.${list.key}`)}
                                            </button>
                                        </li>
                                    ))}
                            </ul>
                        </div>
                    ))}
                </nav>
                {chosen === undefined ? (
                    <p className="text-sm text-ink-muted">{t('refdata.classifications.choose')}</p>
                ) : (
                    <ListPanel key={chosen.key} list={chosen} />
                )}
            </div>
        </div>
    );
}

/** What the edit form is doing: nothing, adding a row, or correcting one. */
type Editing =
    | { readonly kind: 'none' }
    | { readonly kind: 'add' }
    | { readonly kind: 'row'; readonly code: string };

function ListPanel({ list }: { readonly list: ClassificationList }): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [editing, setEditing] = useState<Editing>({ kind: 'none' });
    const [removing, setRemoving] = useState<ClassificationRow | null>(null);
    const [reverting, setReverting] = useState<{
        readonly row: ClassificationRow;
        readonly version: HistoryVersion;
    } | null>(null);
    const [order, setOrder] = useState<readonly ClassificationRow[] | null>(null);
    const rows = useQuery({
        queryKey: ['classifications', list.key],
        queryFn: () => api.classificationRows(list.key),
    });
    const ordered = list.shape !== 'plain';
    const reorder = useMutation({
        mutationFn: async (moved: readonly ClassificationRow[]) =>
            api.reorderClassificationRows(list.key, {
                rows: moved.map((row) => ({
                    code: row.code,
                    name: row.name,
                    description: row.description,
                    displayOrder: row.displayOrder ?? 0,
                    version: row.version,
                })),
                reasonCode: REORDER_REASON,
                commentary: '',
            }),
        onSuccess: async () => {
            setOrder(null);
            await queries.invalidateQueries({ queryKey: ['classifications', list.key] });
        },
    });

    if (rows.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (rows.isError) {
        return <Notice tone="error">{rows.error.message}</Notice>;
    }
    const shown = order ?? rows.data;
    const selected =
        editing.kind === 'row' ? rows.data.find((row) => row.code === editing.code) : undefined;

    function move(index: number, by: -1 | 1): void {
        const next = [...shown];
        const other = next[index + by];
        const self = next[index];
        if (other === undefined || self === undefined) {
            return;
        }
        next[index + by] = self;
        next[index] = other;
        setOrder(next);
    }

    function saveOrder(): void {
        const moved = shown
            .map((row, index) => ({ ...row, displayOrder: (index + 1) * 10 }))
            .filter(
                (row) =>
                    row.displayOrder !== rows.data?.find((r) => r.code === row.code)?.displayOrder,
            );
        if (moved.length > 0) {
            reorder.mutate(moved);
        }
    }

    return (
        <section>
            <div className="mb-3 flex flex-wrap items-center gap-2">
                <h2 className="text-lg font-medium">
                    {t(`refdata.classifications.lists.${list.key}`)}
                </h2>
                {!list.editable && (
                    <Tag tone="muted">{t('refdata.classifications.readOnlyTag')}</Tag>
                )}
                {list.editable && (
                    <Button
                        variant="primary"
                        size="sm"
                        className="ml-auto"
                        onClick={() => setEditing({ kind: 'add' })}
                    >
                        {t('refdata.classifications.add')}
                    </Button>
                )}
            </div>
            {!list.editable && (
                <div className="mb-3">
                    <Notice tone="info">{t('refdata.classifications.readOnly')}</Notice>
                </div>
            )}
            {order !== null && (
                <div className="mb-3 flex items-center gap-2">
                    <Notice tone="warn">{t('refdata.classifications.orderChanged')}</Notice>
                    <Button
                        size="sm"
                        variant="primary"
                        pending={reorder.isPending}
                        onClick={saveOrder}
                    >
                        {t('refdata.classifications.saveOrder')}
                    </Button>
                    <Button size="sm" variant="ghost" onClick={() => setOrder(null)}>
                        {t('refdata.classifications.cancel')}
                    </Button>
                </div>
            )}
            {reorder.isError && <Notice tone="error">{reorder.error.message}</Notice>}
            {shown.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('refdata.classifications.empty')}</p>
            ) : (
                <div className="overflow-x-auto rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                <th className="px-4 py-2 font-medium">
                                    {t('refdata.classifications.code')}
                                </th>
                                {list.shape === 'named' && (
                                    <th className="px-4 py-2 font-medium">
                                        {t('refdata.classifications.name')}
                                    </th>
                                )}
                                <th className="px-4 py-2 font-medium">
                                    {t('refdata.classifications.description')}
                                </th>
                                {ordered && (
                                    <th className="px-4 py-2 font-medium">
                                        {t('refdata.classifications.order')}
                                    </th>
                                )}
                                {ordered && list.editable && <th className="px-4 py-2" />}
                            </tr>
                        </thead>
                        <tbody>
                            {shown.map((row, index) => (
                                <tr
                                    key={row.code}
                                    className={cx(
                                        'cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover',
                                        selected?.code === row.code && 'bg-surface-hover',
                                    )}
                                    onClick={() => setEditing({ kind: 'row', code: row.code })}
                                >
                                    <td className="px-4 py-2 font-mono">{row.code}</td>
                                    {list.shape === 'named' && (
                                        <td className="px-4 py-2">{row.name}</td>
                                    )}
                                    <td className="px-4 py-2 text-ink-muted">{row.description}</td>
                                    {ordered && (
                                        <td className="px-4 py-2">{row.displayOrder ?? ''}</td>
                                    )}
                                    {ordered && list.editable && (
                                        <td className="px-2 py-1 text-right whitespace-nowrap">
                                            <Button
                                                size="sm"
                                                variant="ghost"
                                                aria-label={t('refdata.classifications.moveUp')}
                                                disabled={index === 0}
                                                onClick={(event) => {
                                                    event.stopPropagation();
                                                    move(index, -1);
                                                }}
                                            >
                                                ↑
                                            </Button>
                                            <Button
                                                size="sm"
                                                variant="ghost"
                                                aria-label={t('refdata.classifications.moveDown')}
                                                disabled={index === shown.length - 1}
                                                onClick={(event) => {
                                                    event.stopPropagation();
                                                    move(index, 1);
                                                }}
                                            >
                                                ↓
                                            </Button>
                                        </td>
                                    )}
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            )}
            {editing.kind === 'add' && (
                <RowForm list={list} row={null} onDone={() => setEditing({ kind: 'none' })} />
            )}
            {selected !== undefined && (
                <div className="mt-6 grid gap-6 lg:grid-cols-2">
                    {list.editable ? (
                        <RowForm
                            key={`${selected.code}-${String(selected.version)}`}
                            list={list}
                            row={selected}
                            onDone={() => setEditing({ kind: 'none' })}
                            onRemove={() => setRemoving(selected)}
                        />
                    ) : (
                        <div />
                    )}
                    <div>
                        <h3 className="mb-2 text-sm font-medium">
                            {t('refdata.classifications.history')}
                        </h3>
                        <HistoryPanel
                            entityType={list.entityType}
                            entityId={selected.code}
                            {...(list.editable
                                ? {
                                      onRevert: (version: HistoryVersion) =>
                                          setReverting({ row: selected, version }),
                                  }
                                : {})}
                        />
                    </div>
                </div>
            )}
            {removing !== null && (
                <RemoveDialog
                    list={list}
                    row={removing}
                    onClose={() => setRemoving(null)}
                    onRemoved={() => {
                        setRemoving(null);
                        setEditing({ kind: 'none' });
                    }}
                />
            )}
            {reverting !== null && (
                <RevertDialog
                    list={list}
                    row={reverting.row}
                    version={reverting.version}
                    onClose={() => setReverting(null)}
                />
            )}
        </section>
    );
}

/** The reasons a correction or a removal may carry, and the chosen one. */
function useReason(kind: 'amend' | 'delete'): {
    readonly reasons: readonly {
        readonly code: string;
        readonly description: string;
        readonly requiresCommentary: boolean;
    }[];
    readonly code: string;
    readonly setCode: (code: string) => void;
    readonly needsCommentary: boolean;
} {
    const reasons = useQuery({
        queryKey: ['reference-data-reasons', kind],
        queryFn: () => api.referenceDataReasons(kind),
    });
    const [code, setCode] = useState('');
    useEffect(() => {
        if (code === '' && reasons.data !== undefined && reasons.data.length > 0) {
            setCode(reasons.data[0]?.code ?? '');
        }
    }, [code, reasons.data]);
    const list = reasons.data ?? [];
    return {
        reasons: list,
        code,
        setCode,
        needsCommentary: list.find((reason) => reason.code === code)?.requiresCommentary ?? false,
    };
}

/** The values of a row, as the form edits them. */
interface RowValues {
    readonly code: string;
    readonly name: string;
    readonly description: string;
    readonly displayOrder: string;
}

function valuesOf(row: ClassificationRow | null): RowValues {
    return {
        code: row?.code ?? '',
        name: row?.name ?? '',
        description: row?.description ?? '',
        displayOrder: row?.displayOrder === null || row === null ? '' : String(row.displayOrder),
    };
}

function orderOf(list: ClassificationList, text: string): number | null {
    if (list.shape === 'plain') {
        return null;
    }
    const value = Number.parseInt(text, 10);
    return Number.isNaN(value) ? 0 : value;
}

/**
 * Adds a row, or corrects one against the version the screen read.
 *
 * A new row needs no reason to be chosen: it is written as a new record. A
 * correction needs one, and some reasons need a commentary too.
 */
function RowForm({
    list,
    row,
    onDone,
    onRemove,
}: {
    readonly list: ClassificationList;
    readonly row: ClassificationRow | null;
    readonly onDone: () => void;
    readonly onRemove?: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [values, setValues] = useState<RowValues>(valuesOf(row));
    const [commentary, setCommentary] = useState('');
    const reason = useReason('amend');
    const save = useMutation({
        mutationFn: async () => {
            const common = {
                name: values.name.trim(),
                description: values.description.trim(),
                displayOrder: orderOf(list, values.displayOrder),
                commentary: commentary.trim(),
            };
            if (row === null) {
                await api.addClassificationRow(list.key, {
                    ...common,
                    code: values.code.trim(),
                    reasonCode: NEW_RECORD_REASON,
                });
            } else {
                await api.correctClassificationRow(list.key, row.code, {
                    ...common,
                    version: row.version,
                    reasonCode: reason.code,
                });
            }
        },
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['classifications', list.key] });
            await queries.invalidateQueries({ queryKey: ['history', list.entityType] });
            if (row === null) {
                onDone();
            }
        },
    });
    const missingCode = row === null && values.code.trim() === '';
    const missingCommentary = row !== null && reason.needsCommentary && commentary.trim() === '';

    return (
        <form
            className="mt-6 space-y-3 rounded-md border border-line p-4"
            onSubmit={(event) => {
                event.preventDefault();
                save.mutate();
            }}
        >
            <h3 className="text-sm font-medium">
                {row === null ? t('refdata.classifications.add') : row.code}
            </h3>
            {row === null && (
                <Field label={t('refdata.classifications.code')}>
                    <Input
                        value={values.code}
                        maxLength={100}
                        onChange={(event) => setValues({ ...values, code: event.target.value })}
                    />
                </Field>
            )}
            {list.shape === 'named' && (
                <Field label={t('refdata.classifications.name')}>
                    <Input
                        value={values.name}
                        maxLength={2000}
                        onChange={(event) => setValues({ ...values, name: event.target.value })}
                    />
                </Field>
            )}
            <Field label={t('refdata.classifications.description')}>
                <Input
                    value={values.description}
                    maxLength={2000}
                    onChange={(event) => setValues({ ...values, description: event.target.value })}
                />
            </Field>
            {list.shape !== 'plain' && (
                <Field label={t('refdata.classifications.order')}>
                    <Input
                        type="number"
                        min={0}
                        value={values.displayOrder}
                        onChange={(event) =>
                            setValues({ ...values, displayOrder: event.target.value })
                        }
                    />
                </Field>
            )}
            {row !== null && (
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
            )}
            <Field
                label={t('refdata.classifications.commentary')}
                {...(missingCommentary
                    ? { error: t('refdata.classifications.commentaryRequired') }
                    : {})}
            >
                <Input
                    value={commentary}
                    maxLength={2000}
                    onChange={(event) => setCommentary(event.target.value)}
                />
            </Field>
            {save.isError && <Notice tone="error">{save.error.message}</Notice>}
            {save.isSuccess && row !== null && (
                <Notice tone="success">{t('refdata.classifications.saved')}</Notice>
            )}
            <div className="flex gap-2">
                <Button
                    type="submit"
                    variant="primary"
                    pending={save.isPending}
                    disabled={
                        missingCode || missingCommentary || (row !== null && reason.code === '')
                    }
                >
                    {t('refdata.classifications.save')}
                </Button>
                <Button type="button" variant="ghost" onClick={onDone}>
                    {t('refdata.classifications.cancel')}
                </Button>
                {onRemove !== undefined && (
                    <Button type="button" variant="danger" className="ml-auto" onClick={onRemove}>
                        {t('refdata.classifications.remove')}
                    </Button>
                )}
            </div>
        </form>
    );
}

/**
 * Removes a row after a warning: nothing checks whether records still use the
 * code, and a record that does fails its next save.
 */
function RemoveDialog({
    list,
    row,
    onClose,
    onRemoved,
}: {
    readonly list: ClassificationList;
    readonly row: ClassificationRow;
    readonly onClose: () => void;
    readonly onRemoved: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [commentary, setCommentary] = useState('');
    const reason = useReason('delete');
    const remove = useMutation({
        mutationFn: () =>
            api.removeClassificationRow(list.key, row.code, {
                reasonCode: reason.code,
                commentary: commentary.trim(),
            }),
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['classifications', list.key] });
            onRemoved();
        },
    });
    const missingCommentary = reason.needsCommentary && commentary.trim() === '';

    return (
        <Dialog
            title={t('refdata.classifications.removeTitle', { code: row.code })}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('refdata.classifications.cancel')}
                    </Button>
                    <Button
                        variant="danger"
                        pending={remove.isPending}
                        disabled={reason.code === '' || missingCommentary}
                        onClick={() => remove.mutate()}
                    >
                        {t('refdata.classifications.remove')}
                    </Button>
                </>
            }
        >
            <div className="space-y-3">
                <Notice tone="warn">{t('refdata.classifications.removeWarning')}</Notice>
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
                <Field
                    label={t('refdata.classifications.commentary')}
                    {...(missingCommentary
                        ? { error: t('refdata.classifications.commentaryRequired') }
                        : {})}
                >
                    <Input
                        value={commentary}
                        maxLength={2000}
                        onChange={(event) => setCommentary(event.target.value)}
                    />
                </Field>
                {remove.isError && <Notice tone="error">{remove.error.message}</Notice>}
            </div>
        </Dialog>
    );
}

/** The value of one field in a version, by the name the history gives it. */
function fieldOf(version: HistoryVersion, name: string): string {
    return version.fields.find((field) => field.name === name)?.value ?? '';
}

/**
 * Writes an older version's values back as a new version.
 *
 * The values are read from the version's fields by the names the server's
 * history mapper gives them, and written against the row's current version, so
 * a change made since the screen was read is refused rather than lost.
 */
function RevertDialog({
    list,
    row,
    version,
    onClose,
}: {
    readonly list: ClassificationList;
    readonly row: ClassificationRow;
    readonly version: HistoryVersion;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const reason = useReason('amend');
    const revert = useMutation({
        mutationFn: () => {
            const order = Number.parseInt(fieldOf(version, 'Display Order'), 10);
            return api.correctClassificationRow(list.key, row.code, {
                name: fieldOf(version, 'Name'),
                description: fieldOf(version, 'Description'),
                displayOrder: list.shape === 'plain' || Number.isNaN(order) ? null : order,
                version: row.version,
                reasonCode: reason.code,
                commentary: t('refdata.classifications.revertCommentary', {
                    version: String(version.version),
                }),
            });
        },
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['classifications', list.key] });
            await queries.invalidateQueries({ queryKey: ['history', list.entityType] });
            onClose();
        },
    });

    return (
        <Dialog
            title={t('history.revert')}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('refdata.classifications.cancel')}
                    </Button>
                    <Button
                        variant="primary"
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
                    {t('history.revertBody', {
                        name: row.code,
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

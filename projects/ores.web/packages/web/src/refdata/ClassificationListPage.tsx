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
import { Navigate, useNavigate, useParams } from 'react-router';
import type { ClassificationList, ClassificationRow } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Dialog, Field, Input, Notice, PageHeader } from '../ui/Primitives.js';
import {
    Crumbs,
    LabelPicker,
    RowLabel,
    UNMAPPED,
    classificationsPath,
    isLabelled,
    useLabelCatalogue,
    usePermissions,
} from './shared.js';

/** The reason a new row is written with; no other reason applies to a new record. */
const NEW_RECORD_REASON = 'system.new_record';

/** The reason a reorder is written with: it changes no row's meaning. */
const REORDER_REASON = 'common.non_material_update';

/**
 * One classification list: its rows, and the actions on the list.
 *
 * Adding is a dialog and reordering is a mode with its own save, so the table
 * is only ever a table. A row opens on its own page.
 */
export function ClassificationListPage(): ReactNode {
    const { t } = useTranslation();
    const { list: key } = useParams();
    const lists = useQuery({ queryKey: ['classifications'], queryFn: api.classificationLists });

    if (lists.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (lists.isError) {
        return <Notice tone="error">{lists.error.message}</Notice>;
    }
    const list = lists.data.find((candidate) => candidate.key === key);
    if (list === undefined) {
        return <Navigate to={classificationsPath()} replace />;
    }
    return <ListBody key={list.key} list={list} />;
}

function ListBody({ list }: { readonly list: ClassificationList }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const may = usePermissions(list);
    const { byCode, domains } = useLabelCatalogue();
    const [filter, setFilter] = useState('');
    const [order, setOrder] = useState<readonly ClassificationRow[] | null>(null);
    const [adding, setAdding] = useState(false);
    const rows = useQuery({
        queryKey: ['classifications', list.key],
        queryFn: () => api.classificationRows(list.key),
    });
    const reorder = useMutation({
        mutationFn: (moved: readonly ClassificationRow[]) =>
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
    const title = t(`refdata.classifications.lists.${list.key}`);
    const labelled = isLabelled(list, domains);
    const ordered = list.shape !== 'plain';

    if (rows.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (rows.isError) {
        return <Notice tone="error">{rows.error.message}</Notice>;
    }
    const wanted = filter.trim().toLowerCase();
    const shown =
        order ??
        rows.data.filter(
            (row) =>
                wanted === '' ||
                `${row.code} ${row.name} ${row.description}`.toLowerCase().includes(wanted),
        );

    function move(index: number, by: -1 | 1): void {
        if (order === null) {
            return;
        }
        const next = [...order];
        const self = next[index];
        const other = next[index + by];
        if (self === undefined || other === undefined) {
            return;
        }
        next[index] = other;
        next[index + by] = self;
        setOrder(next);
    }

    function saveOrder(): void {
        if (order === null) {
            return;
        }
        const before = new Map(rows.data?.map((row) => [row.code, row.displayOrder]) ?? []);
        const moved = order
            .map((row, index) => ({ ...row, displayOrder: (index + 1) * 10 }))
            .filter((row) => before.get(row.code) !== row.displayOrder);
        if (moved.length === 0) {
            setOrder(null);
            return;
        }
        reorder.mutate(moved);
    }

    return (
        <div className="space-y-4">
            <div>
                <Crumbs
                    parts={[
                        { label: t('refdata.area.title'), to: '/refdata' },
                        { label: t('refdata.classifications.title'), to: classificationsPath() },
                        { label: title },
                    ]}
                />
                <PageHeader
                    title={title}
                    description={t(`refdata.classifications.briefs.${list.key}`)}
                    actions={
                        may.write && order === null ? (
                            <div className="flex gap-2">
                                {ordered && (
                                    <Button onClick={() => setOrder([...rows.data])}>
                                        {t('refdata.classifications.reorder')}
                                    </Button>
                                )}
                                <Button variant="primary" onClick={() => setAdding(true)}>
                                    {t('refdata.classifications.add')}
                                </Button>
                            </div>
                        ) : undefined
                    }
                />
            </div>
            {!list.editable && <Notice tone="info">{t('refdata.classifications.readOnly')}</Notice>}
            {list.editable && !may.write && (
                <Notice tone="info">{t('refdata.classifications.readerOnly')}</Notice>
            )}
            {order !== null && (
                <div className="flex flex-wrap items-center justify-between gap-3 rounded-md border border-warn/40 bg-warn/10 px-4 py-2 text-sm">
                    <span>{t('refdata.classifications.orderLead')}</span>
                    <span className="flex gap-2">
                        <Button size="sm" variant="ghost" onClick={() => setOrder(null)}>
                            {t('refdata.classifications.cancel')}
                        </Button>
                        <Button
                            size="sm"
                            variant="primary"
                            pending={reorder.isPending}
                            onClick={saveOrder}
                        >
                            {t('refdata.classifications.saveOrder')}
                        </Button>
                    </span>
                </div>
            )}
            {reorder.isError && <Notice tone="error">{reorder.error.message}</Notice>}
            <section className="overflow-hidden rounded-md border border-line">
                {order === null && (
                    <div className="border-b border-line p-3">
                        <Input
                            type="search"
                            value={filter}
                            placeholder={t('refdata.classifications.filter')}
                            aria-label={t('refdata.classifications.filter')}
                            onChange={(event) => setFilter(event.target.value)}
                        />
                    </div>
                )}
                <div className="overflow-x-auto">
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
                                {labelled && (
                                    <th className="px-4 py-2 font-medium">
                                        {t('refdata.classifications.label')}
                                    </th>
                                )}
                                <th className="px-4 py-2 font-medium">
                                    {t('refdata.classifications.description')}
                                </th>
                                {ordered && (
                                    <th className="px-4 py-2 text-right font-medium">
                                        {t('refdata.classifications.order')}
                                    </th>
                                )}
                                {order !== null && <th className="px-4 py-2" />}
                            </tr>
                        </thead>
                        <tbody>
                            {shown.length === 0 && (
                                <tr>
                                    <td colSpan={6} className="px-4 py-3 text-ink-muted">
                                        {rows.data.length === 0
                                            ? t('refdata.classifications.empty')
                                            : t('refdata.classifications.noMatch')}
                                    </td>
                                </tr>
                            )}
                            {shown.map((row, index) => (
                                <tr
                                    key={row.code}
                                    className={
                                        order === null
                                            ? 'cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover'
                                            : 'border-b border-line-subtle last:border-b-0'
                                    }
                                    onClick={
                                        order === null
                                            ? () =>
                                                  void navigate(
                                                      classificationsPath(list.key, row.code),
                                                  )
                                            : undefined
                                    }
                                >
                                    <td className="px-4 py-2 font-mono text-xs text-ink-muted">
                                        {row.code}
                                    </td>
                                    {list.shape === 'named' && (
                                        <td className="px-4 py-2">{row.name}</td>
                                    )}
                                    {labelled && (
                                        <td className="px-4 py-2">
                                            <RowLabel labelCode={row.labelCode} byCode={byCode} />
                                        </td>
                                    )}
                                    <td className="px-4 py-2 text-ink-muted">{row.description}</td>
                                    {ordered && (
                                        <td className="px-4 py-2 text-right tabular-nums">
                                            {order === null
                                                ? (row.displayOrder ?? '')
                                                : (index + 1) * 10}
                                        </td>
                                    )}
                                    {order !== null && (
                                        <td className="px-2 py-1 text-right whitespace-nowrap">
                                            <Button
                                                size="sm"
                                                variant="ghost"
                                                aria-label={t('refdata.classifications.moveUp')}
                                                disabled={index === 0}
                                                onClick={() => move(index, -1)}
                                            >
                                                ↑
                                            </Button>
                                            <Button
                                                size="sm"
                                                variant="ghost"
                                                aria-label={t('refdata.classifications.moveDown')}
                                                disabled={index === shown.length - 1}
                                                onClick={() => move(index, 1)}
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
                <div className="flex justify-between border-t border-line px-4 py-2 text-xs text-ink-muted">
                    <span>
                        {t('refdata.classifications.shown', {
                            shown: String(shown.length),
                            total: String(rows.data.length),
                        })}
                    </span>
                    {order === null && <span>{t('refdata.classifications.openHint')}</span>}
                </div>
            </section>
            {adding && (
                <AddDialog
                    list={list}
                    labelled={labelled && may.label}
                    nextOrder={(rows.data.length + 1) * 10}
                    onClose={() => setAdding(false)}
                />
            )}
        </div>
    );
}

/** Adds a row, written as a new record, and its label when one is chosen. */
function AddDialog({
    list,
    labelled,
    nextOrder,
    onClose,
}: {
    readonly list: ClassificationList;
    readonly labelled: boolean;
    readonly nextOrder: number;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const [code, setCode] = useState('');
    const [name, setName] = useState('');
    const [description, setDescription] = useState('');
    const [label, setLabel] = useState(UNMAPPED);
    const add = useMutation({
        mutationFn: async () => {
            const trimmed = code.trim();
            await api.addClassificationRow(list.key, {
                code: trimmed,
                name: name.trim(),
                description: description.trim(),
                displayOrder: list.shape === 'plain' ? null : nextOrder,
                reasonCode: NEW_RECORD_REASON,
                commentary: '',
            });
            if (labelled && label !== UNMAPPED) {
                await api.setClassificationLabel(list.key, trimmed, {
                    badgeCode: label,
                    reasonCode: NEW_RECORD_REASON,
                    commentary: '',
                });
            }
            return trimmed;
        },
        onSuccess: async (added) => {
            await queries.invalidateQueries({ queryKey: ['classifications'] });
            void navigate(classificationsPath(list.key, added));
        },
    });

    return (
        <Dialog
            title={t('refdata.classifications.addTitle', {
                list: t(`refdata.classifications.lists.${list.key}`),
            })}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('refdata.classifications.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        pending={add.isPending}
                        disabled={code.trim() === ''}
                        onClick={() => add.mutate()}
                    >
                        {t('refdata.classifications.add')}
                    </Button>
                </>
            }
        >
            <div className="space-y-3">
                <Field label={t('refdata.classifications.code')}>
                    <Input
                        value={code}
                        maxLength={100}
                        onChange={(event) => setCode(event.target.value)}
                    />
                </Field>
                {list.shape === 'named' && (
                    <Field label={t('refdata.classifications.name')}>
                        <Input
                            value={name}
                            maxLength={2000}
                            onChange={(event) => setName(event.target.value)}
                        />
                    </Field>
                )}
                <Field label={t('refdata.classifications.description')}>
                    <Input
                        value={description}
                        maxLength={2000}
                        onChange={(event) => setDescription(event.target.value)}
                    />
                </Field>
                {labelled && <LabelPicker list={list} value={label} onChange={setLabel} />}
                <p className="text-xs text-ink-faint">
                    {t('refdata.classifications.newRecordNote')}
                </p>
                {add.isError && <Notice tone="error">{add.error.message}</Notice>}
            </div>
        </Dialog>
    );
}

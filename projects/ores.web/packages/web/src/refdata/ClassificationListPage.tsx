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
import { RecordList, type ListSource, type PageRequest } from './RecordList.js';
import { LeavingFooter, useCloseGuard } from './records.js';
import {
    Crumbs,
    LabelPicker,
    RowLabel,
    UNMAPPED,
    classificationsPath,
    isLabelled,
    listSourceKey,
    useLabelCatalogue,
    usePermissions,
} from './shared.js';

/** The reason a new row is written with; no other reason applies to a new record. */
const NEW_RECORD_REASON = 'system.new_record';

/** The reason a reorder is written with: it changes no row's meaning. */
const REORDER_REASON = 'common.non_material_update';

/**
 * One page of a classification list, cut from the whole list. A
 * classification list is short and reordered as a whole, so it is read whole
 * once, as the record screen standard's exception for these lists says; the
 * search, the order and the page are then the list's, not the server's.
 */
export function pageOfList(
    rows: readonly ClassificationRow[],
    page: PageRequest,
): { readonly rows: readonly ClassificationRow[]; readonly total: number } {
    const wanted = page.search.toLowerCase();
    const found = rows.filter(
        (row) =>
            wanted === '' ||
            `${row.code} ${row.name} ${row.description}`.toLowerCase().includes(wanted),
    );
    const value = (row: ClassificationRow): string | number =>
        page.sort === 'display_order'
            ? (row.displayOrder ?? Number.MAX_SAFE_INTEGER)
            : page.sort === 'name'
              ? row.name
              : row.code;
    const sorted =
        page.sort === ''
            ? found
            : [...found].sort((left, right) => {
                  const a = value(left);
                  const b = value(right);
                  const order =
                      typeof a === 'number' && typeof b === 'number'
                          ? a - b
                          : String(a).localeCompare(String(b));
                  return page.descending ? -order : order;
              });
    return { rows: sorted.slice(page.offset, page.offset + page.limit), total: found.length };
}

/**
 * One classification list: the shared record list over its rows, with Reorder
 * as the list's own action. Reordering is a mode with its own save, so the
 * list is only ever a list. A row opens on its own page.
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
    const queries = useQueryClient();
    const may = usePermissions(list);
    const { byCode, domains } = useLabelCatalogue();
    const [order, setOrder] = useState<readonly ClassificationRow[] | null>(null);
    const [adding, setAdding] = useState(false);
    const title = t(`refdata.classifications.lists.${list.key}`);
    const labelled = isLabelled(list, domains);
    const ordered = list.shape !== 'plain';
    const whole = (): readonly ClassificationRow[] =>
        queries.getQueryData<readonly ClassificationRow[]>(['classifications', list.key]) ?? [];

    const source: ListSource<ClassificationRow> = {
        key: listSourceKey(list),
        read: async (page) => {
            const rows = await api.classificationRows(list.key);
            queries.setQueryData(['classifications', list.key], rows);
            return pageOfList(rows, page);
        },
        search: true,
        sortable: [
            'code',
            ...(list.shape === 'named' ? ['name'] : []),
            ...(ordered ? ['display_order'] : []),
        ],
        mayAdd: may.write,
    };

    if (order !== null) {
        return (
            <ReorderPanel
                list={list}
                title={title}
                before={whole()}
                order={order}
                onOrder={setOrder}
                onDone={() => setOrder(null)}
            />
        );
    }

    return (
        <>
            <RecordList
                source={source}
                title={title}
                lead={t(`refdata.classifications.briefs.${list.key}`)}
                crumbs={[
                    { label: t('refdata.area.title'), to: '/refdata' },
                    { label: t('refdata.classifications.title'), to: classificationsPath() },
                    { label: title },
                ]}
                pathOf={(row) => classificationsPath(list.key, row.code)}
                notes={
                    !list.editable
                        ? [{ tone: 'info', text: t('refdata.classifications.readOnly') }]
                        : may.write
                          ? []
                          : [{ tone: 'info', text: t('refdata.classifications.readerOnly') }]
                }
                actions={
                    may.write && ordered ? (
                        <Button onClick={() => setOrder([...whole()])}>
                            {t('refdata.classifications.reorder')}
                        </Button>
                    ) : undefined
                }
                addLabel={t('refdata.classifications.add')}
                onAdd={() => setAdding(true)}
                columns={[
                    {
                        id: 'code',
                        header: t('refdata.classifications.code'),
                        cell: (row) => row.code,
                        mono: true,
                        sort: 'code',
                    },
                    ...(list.shape === 'named'
                        ? [
                              {
                                  id: 'name',
                                  header: t('refdata.classifications.name'),
                                  cell: (row: ClassificationRow) => row.name,
                                  sort: 'name',
                              },
                          ]
                        : []),
                    ...(labelled
                        ? [
                              {
                                  id: 'label',
                                  header: t('refdata.classifications.label'),
                                  cell: (row: ClassificationRow) => (
                                      <RowLabel labelCode={row.labelCode} byCode={byCode} />
                                  ),
                              },
                          ]
                        : []),
                    {
                        id: 'description',
                        header: t('refdata.classifications.description'),
                        cell: (row) => row.description,
                    },
                    ...(ordered
                        ? [
                              {
                                  id: 'display_order',
                                  header: t('refdata.classifications.order'),
                                  cell: (row: ClassificationRow) => String(row.displayOrder ?? ''),
                                  numeric: true,
                                  sort: 'display_order',
                              },
                          ]
                        : []),
                ]}
            />
            {adding && (
                <AddDialog
                    list={list}
                    labelled={labelled && may.label}
                    nextOrder={(whole().length + 1) * 10}
                    onClose={() => setAdding(false)}
                />
            )}
        </>
    );
}

/**
 * The list in reorder mode: every row in its order, moved up and down, and
 * saved as one change that writes only the rows whose order moved.
 */
function ReorderPanel({
    list,
    title,
    before,
    order,
    onOrder,
    onDone,
}: {
    readonly list: ClassificationList;
    readonly title: string;
    readonly before: readonly ClassificationRow[];
    readonly order: readonly ClassificationRow[];
    readonly onOrder: (order: readonly ClassificationRow[]) => void;
    readonly onDone: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
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
            await queries.invalidateQueries({ queryKey: ['classifications', list.key] });
            await queries.invalidateQueries({ queryKey: ['records', listSourceKey(list)] });
            onDone();
        },
    });

    function move(index: number, by: -1 | 1): void {
        const next = [...order];
        const self = next[index];
        const other = next[index + by];
        if (self === undefined || other === undefined) {
            return;
        }
        next[index] = other;
        next[index + by] = self;
        onOrder(next);
    }

    function save(): void {
        const was = new Map(before.map((row) => [row.code, row.displayOrder]));
        const moved = order
            .map((row, index) => ({ ...row, displayOrder: (index + 1) * 10 }))
            .filter((row) => was.get(row.code) !== row.displayOrder);
        if (moved.length === 0) {
            onDone();
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
                    description={t('refdata.classifications.orderLead')}
                    actions={
                        <div className="flex gap-2">
                            <Button variant="ghost" icon="cancel" onClick={onDone}>
                                {t('refdata.records.cancel')}
                            </Button>
                            <Button
                                variant="primary"
                                icon="save"
                                pending={reorder.isPending}
                                onClick={save}
                            >
                                {t('refdata.classifications.saveOrder')}
                            </Button>
                        </div>
                    }
                />
            </div>
            {reorder.isError && <Notice tone="error">{reorder.error.message}</Notice>}
            <section className="overflow-hidden rounded-md border border-line">
                <table className="w-full text-left text-sm">
                    <thead>
                        <tr className="border-b border-line text-xs text-ink-muted">
                            <th className="px-4 py-2 font-medium">
                                {t('refdata.classifications.code')}
                            </th>
                            <th className="px-4 py-2 font-medium">
                                {t('refdata.classifications.description')}
                            </th>
                            <th className="px-4 py-2 text-right font-medium">
                                {t('refdata.classifications.order')}
                            </th>
                            <th className="px-4 py-2" />
                        </tr>
                    </thead>
                    <tbody>
                        {order.map((row, index) => (
                            <tr
                                key={row.code}
                                className="border-b border-line-subtle last:border-b-0"
                            >
                                <td className="px-4 py-2 font-mono text-xs text-ink-muted">
                                    {row.code}
                                </td>
                                <td className="px-4 py-2 text-ink-muted">{row.description}</td>
                                <td className="px-4 py-2 text-right tabular-nums">
                                    {(index + 1) * 10}
                                </td>
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
                                        disabled={index === order.length - 1}
                                        onClick={() => move(index, 1)}
                                    >
                                        ↓
                                    </Button>
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </section>
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
    const guard = useCloseGuard(
        code.trim() !== '' || name.trim() !== '' || description.trim() !== '' || label !== UNMAPPED,
        onClose,
    );
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
            await queries.invalidateQueries({ queryKey: ['records', listSourceKey(list)] });
            void navigate(classificationsPath(list.key, added));
        },
    });

    return (
        <Dialog
            title={t('refdata.classifications.addTitle', {
                list: t(`refdata.classifications.lists.${list.key}`),
            })}
            onClose={guard.close}
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
                            icon="add"
                            pending={add.isPending}
                            disabled={code.trim() === ''}
                            onClick={() => add.mutate()}
                        >
                            {t('refdata.records.add')}
                        </Button>
                    </>
                )
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
                {guard.leaving && <Notice tone="warn">{t('refdata.records.unsaved')}</Notice>}
            </div>
        </Dialog>
    );
}

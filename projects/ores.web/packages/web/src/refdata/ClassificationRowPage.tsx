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
import { Navigate, useNavigate, useParams, useSearchParams } from 'react-router';
import type {
    ClassificationList,
    ClassificationRow,
    HistoryVersion,
} from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Dialog, Field, Input, Notice, PageHeader, Select } from '../ui/Primitives.js';
import { HistoryPanel, fieldValue } from './HistoryPanel.js';
import {
    Crumbs,
    LabelPicker,
    RowLabel,
    UNMAPPED,
    classificationsPath,
    isLabelled,
    useLabelCatalogue,
    usePermissions,
    ReasonFields,
    useReason,
} from './shared.js';

/** The tabs of a row's page, in the order they are drawn. */
const TABS = ['details', 'history'] as const;
type Tab = (typeof TABS)[number];

/**
 * One row of a classification list, on its own page.
 *
 * The address names the list, the row and the tab, so a link opens the same
 * row at the same place again. Editing and removing are dialogs; the history
 * is a tab.
 */
export function ClassificationRowPage(): ReactNode {
    const { t } = useTranslation();
    const { list: key, code } = useParams();
    const lists = useQuery({ queryKey: ['classifications'], queryFn: api.classificationLists });
    const rows = useQuery({
        queryKey: ['classifications', key],
        queryFn: () => api.classificationRows(key ?? ''),
        enabled: key !== undefined,
    });

    if (lists.isPending || rows.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (lists.isError) {
        return <Notice tone="error">{lists.error.message}</Notice>;
    }
    if (rows.isError) {
        return <Notice tone="error">{rows.error.message}</Notice>;
    }
    const list = lists.data.find((candidate) => candidate.key === key);
    if (list === undefined) {
        return <Navigate to={classificationsPath()} replace />;
    }
    const row = rows.data.find((candidate) => candidate.code === code);
    if (row === undefined) {
        return <Navigate to={classificationsPath(list.key)} replace />;
    }
    return <RowBody list={list} row={row} />;
}

function RowBody({
    list,
    row,
}: {
    readonly list: ClassificationList;
    readonly row: ClassificationRow;
}): ReactNode {
    const { t } = useTranslation();
    const [search, setSearch] = useSearchParams();
    const may = usePermissions(list);
    const { byCode, domains } = useLabelCatalogue();
    const [editing, setEditing] = useState(false);
    const [removing, setRemoving] = useState(false);
    const [reverting, setReverting] = useState<HistoryVersion | null>(null);
    const requested = search.get('tab');
    const tab: Tab = TABS.find((candidate) => candidate === requested) ?? 'details';
    const title = t(`refdata.classifications.lists.${list.key}`);
    const labelled = isLabelled(list, domains);
    const canEdit = may.write || (labelled && may.label);

    return (
        <div className="space-y-4">
            <div>
                <Crumbs
                    parts={[
                        { label: t('refdata.area.title'), to: '/refdata' },
                        { label: t('refdata.classifications.title'), to: classificationsPath() },
                        { label: title, to: classificationsPath(list.key) },
                        { label: row.code },
                    ]}
                />
                <PageHeader
                    title={row.name === '' ? row.code : row.name}
                    description={t('refdata.classifications.rowLead', {
                        list: title,
                        code: row.code,
                        version: String(row.version),
                    })}
                    actions={
                        canEdit || may.remove ? (
                            <div className="flex gap-2">
                                {canEdit && (
                                    <Button onClick={() => setEditing(true)}>
                                        {t('refdata.classifications.edit')}
                                    </Button>
                                )}
                                {may.remove && (
                                    <Button variant="danger" onClick={() => setRemoving(true)}>
                                        {t('refdata.classifications.remove')}
                                    </Button>
                                )}
                            </div>
                        ) : undefined
                    }
                />
                {labelled && (
                    <div className="mt-2">
                        <RowLabel labelCode={row.labelCode} byCode={byCode} />
                    </div>
                )}
            </div>
            <div role="tablist" aria-label={row.code} className="flex gap-1 border-b border-line">
                {TABS.map((candidate) => (
                    <button
                        key={candidate}
                        type="button"
                        role="tab"
                        aria-selected={tab === candidate}
                        className={
                            tab === candidate
                                ? 'border-b-2 border-accent px-3 py-2 text-sm text-ink'
                                : 'border-b-2 border-transparent px-3 py-2 text-sm text-ink-muted hover:text-ink'
                        }
                        onClick={() => setSearch(candidate === 'details' ? {} : { tab: candidate })}
                    >
                        {t(`refdata.classifications.tabs.${candidate}`)}
                    </button>
                ))}
            </div>
            {tab === 'details' ? (
                <Details list={list} row={row} labelled={labelled} />
            ) : (
                <HistoryPanel
                    entityType={list.entityType}
                    entityId={row.code}
                    {...(may.write
                        ? { onRevert: (version: HistoryVersion) => setReverting(version) }
                        : {})}
                />
            )}
            {editing && (
                <EditDialog
                    list={list}
                    row={row}
                    writeRow={may.write}
                    writeLabel={labelled && may.label}
                    onClose={() => setEditing(false)}
                />
            )}
            {removing && <RemoveDialog list={list} row={row} onClose={() => setRemoving(false)} />}
            {reverting !== null && (
                <RevertDialog
                    list={list}
                    row={row}
                    version={reverting}
                    onClose={() => setReverting(null)}
                />
            )}
        </div>
    );
}

function Details({
    list,
    row,
    labelled,
}: {
    readonly list: ClassificationList;
    readonly row: ClassificationRow;
    readonly labelled: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const { byCode } = useLabelCatalogue();
    const entries: [string, ReactNode][] = [[t('refdata.classifications.code'), row.code]];
    if (list.shape === 'named') {
        entries.push([t('refdata.classifications.name'), row.name || '—']);
    }
    entries.push([t('refdata.classifications.description'), row.description || '—']);
    if (list.shape !== 'plain') {
        entries.push([t('refdata.classifications.order'), String(row.displayOrder ?? '—')]);
    }
    if (labelled) {
        entries.push([
            t('refdata.classifications.label'),
            <RowLabel labelCode={row.labelCode} byCode={byCode} />,
        ]);
    }
    entries.push([
        t('refdata.classifications.lastChanged'),
        t('refdata.classifications.lastChangedValue', {
            when: row.recordedAt,
            who: row.modifiedBy,
        }),
    ]);
    entries.push([
        t('refdata.classifications.why'),
        row.commentary === '' ? row.reasonCode : `${row.reasonCode} — ${row.commentary}`,
    ]);
    return (
        <section className="rounded-md border border-line bg-surface-raised p-4">
            <dl className="grid gap-4 sm:grid-cols-2 lg:grid-cols-3">
                {entries.map(([label, value]) => (
                    <div key={label}>
                        <dt className="text-xs text-ink-faint">{label}</dt>
                        <dd className="mt-0.5 text-sm">{value}</dd>
                    </div>
                ))}
            </dl>
        </section>
    );
}

/**
 * Corrects a row against the version read, and changes its label.
 *
 * The label lives in the catalogue, so changing only the label writes no new
 * version of the row.
 */
function EditDialog({
    list,
    row,
    writeRow,
    writeLabel,
    onClose,
}: {
    readonly list: ClassificationList;
    readonly row: ClassificationRow;
    readonly writeRow: boolean;
    readonly writeLabel: boolean;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const reason = useReason('amend');
    const [name, setName] = useState(row.name);
    const [description, setDescription] = useState(row.description);
    const [label, setLabel] = useState(row.labelCode ?? UNMAPPED);
    const [commentary, setCommentary] = useState('');
    const rowChanged = name.trim() !== row.name || description.trim() !== row.description;
    const labelChanged = label !== (row.labelCode ?? UNMAPPED);
    const missing = reason.needsCommentary && commentary.trim() === '';
    const save = useMutation({
        mutationFn: async () => {
            const intent = { reasonCode: reason.code, commentary: commentary.trim() };
            if (rowChanged) {
                await api.correctClassificationRow(list.key, row.code, {
                    name: name.trim(),
                    description: description.trim(),
                    displayOrder: row.displayOrder,
                    version: row.version,
                    ...intent,
                });
            }
            if (labelChanged) {
                await api.setClassificationLabel(list.key, row.code, {
                    badgeCode: label === UNMAPPED ? null : label,
                    ...intent,
                });
            }
        },
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['classifications', list.key] });
            await queries.invalidateQueries({ queryKey: ['history', list.entityType, row.code] });
            await queries.invalidateQueries({ queryKey: ['labels'] });
            onClose();
        },
    });

    return (
        <Dialog
            title={t('refdata.classifications.editTitle', { code: row.code })}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('refdata.classifications.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        pending={save.isPending}
                        disabled={(!rowChanged && !labelChanged) || reason.code === '' || missing}
                        onClick={() => save.mutate()}
                    >
                        {t('refdata.classifications.save')}
                    </Button>
                </>
            }
        >
            <div className="space-y-3">
                {writeRow && list.shape === 'named' && (
                    <Field label={t('refdata.classifications.name')}>
                        <Input
                            value={name}
                            maxLength={2000}
                            onChange={(event) => setName(event.target.value)}
                        />
                    </Field>
                )}
                {writeRow && (
                    <Field label={t('refdata.classifications.description')}>
                        <Input
                            value={description}
                            maxLength={2000}
                            onChange={(event) => setDescription(event.target.value)}
                        />
                    </Field>
                )}
                {writeLabel && <LabelPicker list={list} value={label} onChange={setLabel} />}
                <ReasonFields
                    reason={reason}
                    commentary={commentary}
                    onCommentary={setCommentary}
                    missing={missing}
                />
                {save.isError && <Notice tone="error">{save.error.message}</Notice>}
            </div>
        </Dialog>
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
}: {
    readonly list: ClassificationList;
    readonly row: ClassificationRow;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const reason = useReason('delete');
    const [commentary, setCommentary] = useState('');
    const missing = reason.needsCommentary && commentary.trim() === '';
    const remove = useMutation({
        mutationFn: () =>
            api.removeClassificationRow(list.key, row.code, {
                reasonCode: reason.code,
                commentary: commentary.trim(),
            }),
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['classifications'] });
            void navigate(classificationsPath(list.key));
        },
    });

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
                        disabled={reason.code === '' || missing}
                        onClick={() => remove.mutate()}
                    >
                        {t('refdata.classifications.remove')}
                    </Button>
                </>
            }
        >
            <div className="space-y-3">
                <Notice tone="warn">{t('refdata.classifications.removeWarning')}</Notice>
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
            const order = Number.parseInt(fieldValue(version, 'Display Order'), 10);
            return api.correctClassificationRow(list.key, row.code, {
                name: fieldValue(version, 'Name'),
                description: fieldValue(version, 'Description'),
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
            await queries.invalidateQueries({ queryKey: ['history', list.entityType, row.code] });
            onClose();
        },
    });

    return (
        <Dialog
            title={t('refdata.classifications.revertTitle', {
                code: row.code,
                version: String(version.version),
            })}
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

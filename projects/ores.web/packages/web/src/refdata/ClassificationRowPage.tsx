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
import type {
    ClassificationList,
    ClassificationRow,
    HistoryVersion,
} from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Dialog, Field, Input, Notice } from '../ui/Primitives.js';
import { HistoryPanel, fieldValue } from './HistoryPanel.js';
import {
    DeleteDialog,
    LeavingFooter,
    RecordDetails,
    RecordHeader,
    RevertVersionDialog,
    useCloseGuard,
    useRecordTabs,
} from './records.js';
import {
    LabelPicker,
    ReasonFields,
    RowLabel,
    UNMAPPED,
    classificationsPath,
    isLabelled,
    listSourceKey,
    useLabelCatalogue,
    usePermissions,
    useReason,
} from './shared.js';

/**
 * One row of a classification list, on its own page: the shared header, the
 * details and the history as tabs held in the address, and the shared
 * dialogs. A classification list is short and reordered as a whole, so the
 * row is found in the list read once.
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
    const navigate = useNavigate();
    const queries = useQueryClient();
    const may = usePermissions(list);
    const { domains } = useLabelCatalogue();
    const [editing, setEditing] = useState(false);
    const [removing, setRemoving] = useState(false);
    const [reverting, setReverting] = useState<HistoryVersion | null>(null);
    const { tab, bar } = useRecordTabs({ label: row.code, tabs: ['details', 'history'] });
    const title = t(`refdata.classifications.lists.${list.key}`);
    const labelled = isLabelled(list, domains);
    const canEdit = may.write || (labelled && may.label);

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    { label: t('refdata.area.title'), to: '/refdata' },
                    { label: t('refdata.classifications.title'), to: classificationsPath() },
                    { label: title, to: classificationsPath(list.key) },
                    { label: row.code },
                ]}
                title={row.name === '' ? row.code : row.name}
                recordKey={row.code}
                version={row.version}
                access={canEdit}
                onEdit={canEdit ? () => setEditing(true) : undefined}
                onDelete={may.remove ? () => setRemoving(true) : undefined}
            />
            {bar}
            {tab === 'details' && (
                <div className="space-y-4">
                    {!list.editable && (
                        <Notice tone="info">{t('refdata.classifications.readOnly')}</Notice>
                    )}
                    <Details list={list} row={row} labelled={labelled} />
                </div>
            )}
            {tab === 'history' && (
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
            {removing && (
                <DeleteDialog
                    title={t('refdata.records.deleteTitle', { code: row.code })}
                    warning={t('refdata.classifications.removeWarning')}
                    onClose={() => setRemoving(false)}
                    onRemoved={() => void navigate(classificationsPath(list.key))}
                    remove={async (intent) => {
                        await api.removeClassificationRow(list.key, row.code, intent);
                        await queries.invalidateQueries({ queryKey: ['classifications'] });
                        await queries.invalidateQueries({
                            queryKey: ['records', listSourceKey(list)],
                        });
                    }}
                />
            )}
            {reverting !== null && (
                <RevertVersionDialog
                    current={row.version}
                    version={reverting}
                    onClose={() => setReverting(null)}
                    write={async (intent) => {
                        const order = Number.parseInt(fieldValue(reverting, 'Display Order'), 10);
                        await api.correctClassificationRow(list.key, row.code, {
                            name: fieldValue(reverting, 'Name'),
                            description: fieldValue(reverting, 'Description'),
                            displayOrder:
                                list.shape === 'plain' || Number.isNaN(order) ? null : order,
                            version: row.version,
                            ...intent,
                        });
                        await queries.invalidateQueries({
                            queryKey: ['classifications', list.key],
                        });
                        await queries.invalidateQueries({
                            queryKey: ['records', listSourceKey(list)],
                        });
                    }}
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
    const text = (value: string): ReactNode =>
        value === '' ? <span className="text-ink-faint">—</span> : value;
    const entries: [string, ReactNode][] = [
        [t('refdata.classifications.code'), <span className="font-mono text-xs">{row.code}</span>],
    ];
    if (list.shape === 'named') {
        entries.push([t('refdata.classifications.name'), text(row.name)]);
    }
    entries.push([t('refdata.classifications.description'), text(row.description)]);
    if (list.shape !== 'plain') {
        entries.push([t('refdata.classifications.order'), text(String(row.displayOrder ?? ''))]);
    }
    if (labelled) {
        entries.push([
            t('refdata.classifications.label'),
            <RowLabel labelCode={row.labelCode} byCode={byCode} />,
        ]);
    }
    return (
        <RecordDetails
            specs={[]}
            extra={entries}
            row={{
                version: row.version,
                modified_by: row.modifiedBy,
                recorded_at: row.recordedAt,
                change_reason_code: row.reasonCode,
                change_commentary: row.commentary,
            }}
        />
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
    const [name, setName] = useState(row.name);
    const [description, setDescription] = useState(row.description);
    const [label, setLabel] = useState(row.labelCode ?? UNMAPPED);
    const [commentary, setCommentary] = useState('');
    const rowChanged = name.trim() !== row.name || description.trim() !== row.description;
    const labelChanged = label !== (row.labelCode ?? UNMAPPED);
    const reason = useReason('amend', rowChanged || labelChanged);
    const guard = useCloseGuard(rowChanged || labelChanged || commentary.trim() !== '', onClose);
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
            await queries.invalidateQueries({ queryKey: ['records', listSourceKey(list)] });
            await queries.invalidateQueries({ queryKey: ['history', list.entityType, row.code] });
            await queries.invalidateQueries({ queryKey: ['labels'] });
            onClose();
        },
    });

    return (
        <Dialog
            title={t('refdata.records.editTitle', { code: row.code })}
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
                            icon="save"
                            pending={save.isPending}
                            disabled={
                                (!rowChanged && !labelChanged) || reason.code === '' || missing
                            }
                            onClick={() => save.mutate()}
                        >
                            {t('refdata.records.save')}
                        </Button>
                    </>
                )
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
                {guard.leaving && <Notice tone="warn">{t('refdata.records.unsaved')}</Notice>}
            </div>
        </Dialog>
    );
}

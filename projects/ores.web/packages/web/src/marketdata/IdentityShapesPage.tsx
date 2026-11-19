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

import { useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { useNavigate, useParams } from 'react-router';
import type { HistoryVersion } from '@ores/wire-protocol/browser';
import { api, type RecordRow } from '../api/client.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { useLoadState } from '../events/useLoadState.js';
import { useTranslation } from '../i18n/Provider.js';
import { RecordList, useRecordSource } from '../refdata/RecordList.js';
import { RecordTimeline } from '../refdata/RecordTimeline.js';
import {
    PartsPanel,
    RecordDetails,
    RecordDialog,
    RecordGate,
    RecordHeader,
    RemoveRecordDialog,
    RevertDialog,
    show,
    useRecord,
    useRecordPermissions,
    useRecordTabs,
    useRecords,
    useRegistry,
    type FieldSpec,
} from '../refdata/records.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { PATHS } from '../shell/paths.js';

const SHAPES = 'series-identity-shapes';
const ENTRIES = 'series-identity-shape-entries';
const SHAPE_ENTITY = 'ores.marketdata.series_identity_shape';
const ENTRY_ENTITY = 'ores.marketdata.series_identity_shape_entry';

export const REQUIREMENTS = ['required', 'forbidden'] as const;

const labelOf = (field: string): string => `marketdata.identityShapes.fields.${field}`;

/**
 * The family a shape names. Its parts are its key, so they are entered once;
 * the description is what a person corrects.
 */
const SHAPE_FIELDS: readonly FieldSpec[] = [
    {
        field: 'asset_class',
        history: 'Asset Class',
        kind: { kind: 'text', max: 100 },
        fixed: true,
        labelKey: labelOf('asset_class'),
        help: 'marketdata.identityShapes.help.asset_class',
    },
    {
        field: 'series_subclass',
        history: 'Series Subclass',
        kind: { kind: 'text', max: 100 },
        fixed: true,
        labelKey: labelOf('series_subclass'),
        help: 'marketdata.identityShapes.help.series_subclass',
    },
    {
        field: 'index_family',
        history: 'Index Family',
        kind: { kind: 'text', max: 100 },
        fixed: true,
        blank: true,
        labelKey: labelOf('index_family'),
        help: 'marketdata.identityShapes.help.index_family',
    },
    {
        field: 'description',
        history: 'Description',
        kind: { kind: 'text', max: 2000 },
        labelKey: labelOf('description'),
        help: 'marketdata.identityShapes.help.description',
    },
];

const ENTRY_FIELDS: readonly FieldSpec[] = [
    {
        field: 'field',
        history: 'Field',
        kind: { kind: 'text', max: 100 },
        fixed: true,
        labelKey: labelOf('field'),
        help: 'marketdata.identityShapes.help.field',
    },
    {
        field: 'requirement',
        history: 'Requirement',
        kind: { kind: 'choice', options: REQUIREMENTS },
        labelKey: labelOf('requirement'),
        help: 'marketdata.identityShapes.help.requirement',
    },
    {
        field: 'description',
        history: 'Description',
        kind: { kind: 'text', max: 2000 },
        labelKey: labelOf('entry_description'),
    },
];

/** The code of a shape: the parts of its family, joined by dots, the empty index family left out. */
export function codeOf(write: Readonly<Record<string, unknown>>): string {
    return [write['asset_class'], write['series_subclass'], write['index_family']]
        .map(show)
        .filter((part) => part !== '')
        .join('.');
}

export function identityShapePath(code?: string): string {
    return code === undefined
        ? PATHS.identityShapes
        : `${PATHS.identityShapes}/${encodeURIComponent(code)}`;
}

/** The identity shapes: one page at a time, searched and sorted on the server. */
export function IdentityShapesPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const [adding, setAdding] = useState(false);
    const source = useRecordSource(SHAPES);
    return (
        <>
            <RecordList
                source={source}
                title={t('marketdata.identityShapes.title')}
                lead={t('marketdata.identityShapes.lead')}
                crumbs={[
                    ...useAreaRoot('marketdata'),
                    { label: t('marketdata.identityShapes.title') },
                ]}
                pathOf={(row) => identityShapePath(show(row['code']))}
                addLabel={t('marketdata.identityShapes.add')}
                onAdd={() => setAdding(true)}
                columns={[
                    {
                        id: 'code',
                        header: t(labelOf('code')),
                        cell: (row) => show(row['code']),
                        mono: true,
                        sort: 'code',
                    },
                    {
                        id: 'asset_class',
                        header: t(labelOf('asset_class')),
                        cell: (row) => show(row['asset_class']),
                        mono: true,
                        sort: 'asset_class',
                        hidden: true,
                    },
                    {
                        id: 'series_subclass',
                        header: t(labelOf('series_subclass')),
                        cell: (row) => show(row['series_subclass']),
                        mono: true,
                        sort: 'series_subclass',
                        hidden: true,
                    },
                    {
                        id: 'index_family',
                        header: t(labelOf('index_family')),
                        cell: (row) => show(row['index_family']),
                        mono: true,
                        sort: 'index_family',
                        hidden: true,
                    },
                    {
                        id: 'description',
                        header: t(labelOf('description')),
                        cell: (row) => show(row['description']),
                    },
                ]}
            />
            {adding && (
                <RecordDialog
                    title={t('marketdata.identityShapes.addTitle')}
                    resource={SHAPES}
                    specs={SHAPE_FIELDS}
                    row={undefined}
                    complete={(write) => ({ ...write, code: codeOf(write) })}
                    onClose={() => setAdding(false)}
                    onSaved={(write) => void navigate(identityShapePath(codeOf(write)))}
                />
            )}
        </>
    );
}

/** One identity shape: what it is for, the fields it requires and forbids, and its history. */
export function IdentityShapePage(): ReactNode {
    const { code } = useParams();
    return (
        <RecordGate resource={SHAPES} recordKey={code ?? ''} listPath={identityShapePath()}>
            {(shape) => <ShapeBody key={show(shape['code'])} shape={shape} />}
        </RecordGate>
    );
}

function ShapeBody({ shape }: { readonly shape: RecordRow }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const code = show(shape['code']);
    const may = useRecordPermissions(SHAPES);
    const queries = useQueryClient();
    const registry = useRegistry();
    const { tab, bar } = useRecordTabs({ label: code, tabs: ['details', 'entries', 'history'] });
    const record = useRecord(SHAPES, code);
    const entries = useRecords(ENTRIES, code);
    const [editing, setEditing] = useState(false);
    const [removing, setRemoving] = useState(false);
    const [reverting, setReverting] = useState<HistoryVersion | null>(null);
    const pageKeys = [
        ['records', SHAPES, 'key', code],
        ['records', ENTRIES, code],
        ['history', SHAPE_ENTITY, code],
    ];
    const load = useLoadState(pageKeys);
    const watches = (registry.data ?? [])
        .filter((entry) => [SHAPES, ENTRIES].includes(entry.key))
        .flatMap((entry) => (entry.events === undefined ? [] : [entry.events]));
    const news = useEntityChanges(watches, load);
    const refresh = (): void => {
        for (const key of pageKeys) {
            void queries.invalidateQueries({ queryKey: key });
        }
    };
    const rows = entries.data ?? [];

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    ...useAreaRoot('marketdata'),
                    { label: t('marketdata.identityShapes.title'), to: identityShapePath() },
                    { label: code },
                ]}
                title={code}
                recordKey={code}
                version={shape.version}
                refresh={{ onClick: refresh, load, news }}
                onEdit={may.write ? () => setEditing(true) : undefined}
                onDelete={may.remove ? () => setRemoving(true) : undefined}
            />
            {bar}
            {tab === 'details' && <RecordDetails specs={SHAPE_FIELDS} row={shape} />}
            {tab === 'entries' && (
                <PartsPanel
                    resource={ENTRIES}
                    titles={{
                        add: t('marketdata.identityShapes.addEntry'),
                        one: t('marketdata.identityShapes.entry'),
                        remove: t('marketdata.identityShapes.removeEntry'),
                    }}
                    entityType={ENTRY_ENTITY}
                    specs={ENTRY_FIELDS}
                    parentField="shape_code"
                    parent={code}
                    rows={rows}
                    error={entries.error?.message}
                    initial={{ requirement: 'required' }}
                    nameOf={(row) => show(row['field'])}
                    loadedAt={entries.dataUpdatedAt}
                    lead={t('marketdata.identityShapes.entriesLead')}
                    columns={[
                        {
                            header: t(labelOf('field')),
                            cell: (row) => show(row['field']),
                            mono: true,
                        },
                        {
                            header: t(labelOf('requirement')),
                            cell: (row) =>
                                t(`refdata.choices.requirement.${show(row['requirement'])}`),
                        },
                        {
                            header: t(labelOf('entry_description')),
                            cell: (row) => show(row['description']),
                        },
                    ]}
                    sort={(row) => show(row['field'])}
                />
            )}
            {tab === 'history' && (
                <RecordTimeline
                    entityType={SHAPE_ENTITY}
                    entityId={code}
                    parts={[
                        {
                            entityType: ENTRY_ENTITY,
                            rows,
                            name: (row) =>
                                `${t('marketdata.identityShapes.entry')} ${show(row['field'])}`,
                        },
                    ]}
                    {...(may.write ? { onRevert: setReverting } : {})}
                />
            )}
            {editing && (
                <RecordDialog
                    title={t('refdata.records.editTitle', { code })}
                    resource={SHAPES}
                    specs={SHAPE_FIELDS}
                    row={shape}
                    complete={(write) => ({ ...write, code })}
                    loadedAt={record.dataUpdatedAt}
                    onClose={() => setEditing(false)}
                />
            )}
            {reverting !== null && (
                <RevertDialog
                    resource={SHAPES}
                    specs={SHAPE_FIELDS}
                    row={shape}
                    version={reverting}
                    complete={(write) => ({ ...write, code })}
                    onClose={() => setReverting(null)}
                />
            )}
            {removing && (
                <RemoveRecordDialog
                    resource={SHAPES}
                    title={t('refdata.records.deleteTitle', { code })}
                    warning={t('marketdata.identityShapes.removeWarning')}
                    recordKey={{ code }}
                    version={shape.version}
                    before={async (intent) => {
                        for (const row of rows) {
                            await api.removeRecord(ENTRIES, {
                                key: { id: show(row['id']) },
                                ...intent,
                            });
                        }
                    }}
                    onClose={() => setRemoving(false)}
                    onRemoved={() => void navigate(identityShapePath())}
                />
            )}
        </div>
    );
}

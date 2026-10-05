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
import { useNavigate, useParams } from 'react-router';
import type { HistoryVersion } from '@ores/wire-protocol/browser';
import { api, type RecordRow } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { currencyPath } from './currencies.js';
import { HistoryPanel } from './HistoryPanel.js';
import { RecordList, useRecordSource } from './RecordList.js';
import {
    LinkPanel,
    RecordDetails,
    RecordDialog,
    RecordGate,
    RecordHeader,
    RemoveRecordDialog,
    RevertDialog,
    show,
    useRecordPermissions,
    useRecordTabs,
    useRecords,
    type FieldSpec,
} from './records.js';

const RESOURCE = 'currency-groups';
const MEMBERSHIPS = 'currency-memberships';

const GROUP_FIELDS: readonly FieldSpec[] = [
    { field: 'code', history: 'Code', kind: { kind: 'text', max: 100 }, fixed: true },
    { field: 'name', history: 'Name', kind: { kind: 'text' } },
    { field: 'description', history: 'Description', kind: { kind: 'text' }, blank: true },
    { field: 'display_order', history: 'Display Order', kind: { kind: 'int' } },
];

export function deskGroupPath(code?: string): string {
    return code === undefined
        ? '/refdata/desk-groups'
        : `/refdata/desk-groups/${encodeURIComponent(code)}`;
}

/** The desk groups, in their display order, one page at a time. */
export function DeskGroupsPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const [adding, setAdding] = useState(false);
    const source = useRecordSource(RESOURCE);
    return (
        <>
            <RecordList
                source={source}
                title={t('refdata.deskGroups.title')}
                lead={t('refdata.deskGroups.lead')}
                crumbs={[
                    { label: t('refdata.area.title'), to: '/refdata' },
                    { label: t('refdata.deskGroups.title') },
                ]}
                pathOf={(row) => deskGroupPath(show(row['code']))}
                addLabel={t('refdata.deskGroups.add')}
                onAdd={() => setAdding(true)}
                columns={[
                    {
                        id: 'display_order',
                        header: t('refdata.fields.display_order'),
                        cell: (row) => show(row['display_order']),
                        mono: true,
                        numeric: true,
                        sort: 'display_order',
                    },
                    {
                        id: 'code',
                        header: t('refdata.fields.code'),
                        cell: (row) => show(row['code']),
                        mono: true,
                        sort: 'code',
                    },
                    {
                        id: 'name',
                        header: t('refdata.fields.name'),
                        cell: (row) => show(row['name']),
                        sort: 'name',
                    },
                    {
                        id: 'description',
                        header: t('refdata.fields.description'),
                        cell: (row) => show(row['description']),
                        hidden: true,
                    },
                ]}
            />
            {adding && (
                <RecordDialog
                    title={t('refdata.deskGroups.addTitle')}
                    resource={RESOURCE}
                    specs={GROUP_FIELDS}
                    row={undefined}
                    onClose={() => setAdding(false)}
                    onSaved={(write) => void navigate(deskGroupPath(String(write['code'])))}
                />
            )}
        </>
    );
}

/** One desk group: its fields, its members, and its history. */
export function DeskGroupPage(): ReactNode {
    const { code } = useParams();
    return (
        <RecordGate resource={RESOURCE} recordKey={code ?? ''} listPath={deskGroupPath()}>
            {(row) => <DeskGroupBody row={row} />}
        </RecordGate>
    );
}

function DeskGroupBody({ row }: { readonly row: RecordRow }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const code = show(row['code']);
    const may = useRecordPermissions(RESOURCE);
    const { tab, bar } = useRecordTabs({ label: code, tabs: ['details', 'members', 'history'] });
    const currencies = useRecords('currencies');
    const memberships = useRecords(MEMBERSHIPS);
    const [editing, setEditing] = useState(false);
    const [removing, setRemoving] = useState(false);
    const [reverting, setReverting] = useState<HistoryVersion | null>(null);

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    { label: t('refdata.area.title'), to: '/refdata' },
                    { label: t('refdata.deskGroups.title'), to: deskGroupPath() },
                    { label: code },
                ]}
                title={show(row['name'])}
                recordKey={code}
                version={row.version}
                onEdit={may.write ? () => setEditing(true) : undefined}
                onDelete={may.remove ? () => setRemoving(true) : undefined}
            />
            {bar}
            {tab === 'details' && <RecordDetails specs={GROUP_FIELDS} row={row} />}
            {tab === 'members' && (
                <LinkPanel
                    title={t('refdata.deskGroups.members')}
                    junction={MEMBERSHIPS}
                    parentField="currency_group_code"
                    parentValue={code}
                    childField="currency_iso_code"
                    flag="currency"
                    readAll
                    choices={(currencies.data ?? []).map((currency) => ({
                        value: show(currency['iso_code']),
                        label: show(currency['name']),
                    }))}
                    pathOf={(currency) => currencyPath(currency)}
                />
            )}
            {tab === 'history' && (
                <HistoryPanel
                    entityType="ores.refdata.currency_group"
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
                    specs={GROUP_FIELDS}
                    row={row}
                    onClose={() => setEditing(false)}
                />
            )}
            {removing && (
                <RemoveRecordDialog
                    resource={RESOURCE}
                    title={t('refdata.records.deleteTitle', { code })}
                    warning={t('refdata.deskGroups.removeWarning')}
                    recordKey={{ code }}
                    version={row.version}
                    before={async (intent) => {
                        const members = (memberships.data ?? []).filter(
                            (member) => member['currency_group_code'] === code,
                        );
                        for (const member of members) {
                            await api.removeRecord(MEMBERSHIPS, {
                                key: {
                                    currency_iso_code: show(member['currency_iso_code']),
                                    currency_group_code: code,
                                },
                                ...intent,
                            });
                        }
                    }}
                    onClose={() => setRemoving(false)}
                    onRemoved={() => void navigate(deskGroupPath())}
                />
            )}
            {reverting !== null && (
                <RevertDialog
                    resource={RESOURCE}
                    specs={GROUP_FIELDS}
                    row={row}
                    version={reverting}
                    onClose={() => setReverting(null)}
                />
            )}
        </div>
    );
}

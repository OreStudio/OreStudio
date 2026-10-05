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
import { Navigate, useNavigate, useParams } from 'react-router';
import type { HistoryVersion } from '@ores/wire-protocol/browser';
import { api, type RecordRow } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Notice, PageHeader } from '../ui/Primitives.js';
import { currencyPath } from './currencies.js';
import { HistoryPanel } from './HistoryPanel.js';
import {
    LinkPanel,
    RecordDialog,
    RecordDetails,
    RecordTable,
    RemoveRecordDialog,
    RevertDialog,
    useRecordPermissions,
    useRecordTabs,
    show,
    useRecords,
    type FieldSpec,
} from './records.js';
import { Crumbs } from './shared.js';

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

/** The desk groups in display order, each with how many currencies it holds. */
export function DeskGroupsPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const groups = useRecords(RESOURCE);
    const memberships = useRecords(MEMBERSHIPS);
    const may = useRecordPermissions(RESOURCE);
    const [adding, setAdding] = useState(false);

    if (groups.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (groups.isError) {
        return <Notice tone="error">{groups.error.message}</Notice>;
    }
    const members = (code: string): number =>
        (memberships.data ?? []).filter((row) => row['currency_group_code'] === code).length;
    const ordered = [...groups.data].sort(
        (a, b) =>
            Number(a['display_order'] ?? 0) - Number(b['display_order'] ?? 0) ||
            show(a['code']).localeCompare(show(b['code'])),
    );
    return (
        <div className="space-y-4">
            <div>
                <Crumbs
                    parts={[
                        { label: t('refdata.area.title'), to: '/refdata' },
                        { label: t('refdata.deskGroups.title') },
                    ]}
                />
                <PageHeader
                    title={t('refdata.deskGroups.title')}
                    description={t('refdata.deskGroups.lead')}
                    actions={
                        may.write ? (
                            <Button variant="primary" onClick={() => setAdding(true)}>
                                {t('refdata.deskGroups.add')}
                            </Button>
                        ) : undefined
                    }
                />
            </div>
            <section className="overflow-hidden rounded-md border border-line">
                <RecordTable
                    rows={ordered}
                    pathOf={(row) => deskGroupPath(show(row['code']))}
                    empty={t('refdata.deskGroups.empty')}
                    columns={[
                        {
                            header: t('refdata.fields.code'),
                            cell: (row) => show(row['code']),
                            mono: true,
                        },
                        { header: t('refdata.fields.name'), cell: (row) => show(row['name']) },
                        {
                            header: t('refdata.deskGroups.members'),
                            cell: (row) => members(show(row['code'])),
                        },
                        {
                            header: t('refdata.fields.display_order'),
                            cell: (row) => show(row['display_order']),
                        },
                    ]}
                />
            </section>
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
        </div>
    );
}

/** One desk group: its details, its members and its history. */
export function DeskGroupPage(): ReactNode {
    const { t } = useTranslation();
    const { code } = useParams();
    const groups = useRecords(RESOURCE);
    if (groups.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (groups.isError) {
        return <Notice tone="error">{groups.error.message}</Notice>;
    }
    const row = groups.data.find((candidate) => candidate['code'] === code);
    if (row === undefined) {
        return <Navigate to={deskGroupPath()} replace />;
    }
    return <DeskGroupBody row={row} />;
}

function DeskGroupBody({ row }: { readonly row: RecordRow }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const code = show(row['code']);
    const may = useRecordPermissions(RESOURCE);
    const { tab, bar } = useRecordTabs({ label: code, tabs: ['details', 'history'] });
    const currencies = useRecords('currencies');
    const memberships = useRecords(MEMBERSHIPS);
    const [editing, setEditing] = useState(false);
    const [removing, setRemoving] = useState(false);
    const [reverting, setReverting] = useState<HistoryVersion | null>(null);

    return (
        <div className="space-y-4">
            <div>
                <Crumbs
                    parts={[
                        { label: t('refdata.area.title'), to: '/refdata' },
                        { label: t('refdata.deskGroups.title'), to: deskGroupPath() },
                        { label: code },
                    ]}
                />
                <PageHeader
                    title={show(row['name'])}
                    description={t('refdata.records.lead', { code, version: String(row.version) })}
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
                <div className="grid gap-4 lg:grid-cols-2">
                    <RecordDetails specs={GROUP_FIELDS} row={row} />
                    <LinkPanel
                        title={t('refdata.deskGroups.members')}
                        junction={MEMBERSHIPS}
                        parentField="currency_group_code"
                        parentValue={code}
                        childField="currency_iso_code"
                        readAll
                        choices={(currencies.data ?? []).map((currency) => ({
                            value: show(currency['iso_code']),
                            label: show(currency['name']),
                        }))}
                        pathOf={(currency) => currencyPath(currency)}
                    />
                </div>
            ) : (
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
                    title={t('refdata.records.removeTitle', { code })}
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

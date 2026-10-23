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
 */

/**
 * The engine settings a party holds, from
 * doc/prototypes/reporting/see-the-engine-settings-a-party-holds.org.
 *
 * One row per configuration: its name, how many of the three groups it sets,
 * its thread count and who changed it last. The server answers the party's
 * configurations whole, so the list searches and orders them here.
 */

import { useState, type ReactNode } from 'react';
import { Link, useNavigate } from 'react-router';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import { engineSettings, type EngineSummary } from '../api/engineSettings.js';
import { useHolds } from '../access/holds.js';
import { PERMISSION } from '../access/permissions.js';
import { useTranslation } from '../i18n/Provider.js';
import {
    RecordList,
    type ListColumn,
    type ListFilter,
    type ListSource,
    type PageRequest,
} from '../refdata/RecordList.js';
import { NEW_RECORD_REASON, useCloseGuard, LeavingFooter } from '../refdata/records.js';
import { PARTY_FILTER_ID, usePartyScope } from '../refdata/partyScope.js';
import { Crumbs } from '../refdata/shared.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { engineSettingsPath } from '../shell/paths.js';
import { Button, Dialog, Field, Input, Notice, Textarea } from '../ui/Primitives.js';
import { RelativeTime } from '../ui/Time.js';

/** Where the services announce a change to a configuration, so the list can hear one. */
export const ENGINE_SETTINGS_EVENTS = {
    component: 'reporting',
    entity: 'engine_settings',
} as const;

/** The fields a column orders by, which are the ones the rows carry. */
const SORTABLE = ['name', 'partyName', 'groupsSet', 'threads', 'modifiedBy', 'recordedAt'] as const;

type SortField = (typeof SORTABLE)[number];

function isSortField(field: string): field is SortField {
    return (SORTABLE as readonly string[]).includes(field);
}

/** One page of the rows, searched and ordered as the list asked. */
export function pageOf(
    rows: readonly EngineSummary[],
    page: Pick<PageRequest, 'offset' | 'limit' | 'sort' | 'descending' | 'search' | 'filters'>,
): { readonly rows: readonly EngineSummary[]; readonly total: number } {
    const needle = page.search.trim().toLowerCase();
    const party = page.filters[PARTY_FILTER_ID] ?? '';
    const owned = party === '' ? rows : rows.filter((row) => row.partyId === party);
    const found =
        needle === ''
            ? [...owned]
            : owned.filter((row) =>
                  [row.name, row.description, row.partyName, row.partyCode].some((text) =>
                      text.toLowerCase().includes(needle),
                  ),
              );
    const field = isSortField(page.sort) ? page.sort : 'name';
    found.sort((a, b) => {
        const left = a[field];
        const right = b[field];
        const order =
            typeof left === 'string' && typeof right === 'string'
                ? left.localeCompare(right)
                : Number(left ?? -1) - Number(right ?? -1);
        return page.descending ? -order : order;
    });
    return { rows: found.slice(page.offset, page.offset + page.limit), total: found.length };
}

function sourceOf(filter: ListFilter | undefined): ListSource<EngineSummary> {
    return {
        key: 'engine-settings',
        read: async (page) => pageOf(await engineSettings.list(), page),
        search: true,
        sortable: SORTABLE,
        ...(filter === undefined ? {} : { filters: [filter] }),
        mayAdd: false,
        watches: [ENGINE_SETTINGS_EVENTS],
        recordKey: (row) => ({ name: row.name }),
    };
}

function useColumns(showParty: boolean): readonly ListColumn<EngineSummary>[] {
    const { t } = useTranslation();
    const columns: readonly ListColumn<EngineSummary>[] = [
        {
            id: 'name',
            sort: 'name',
            header: t('engineSettings.columns.name'),
            cell: (row) => (
                <Link to={engineSettingsPath(row.id)} className="text-accent">
                    {row.name}
                </Link>
            ),
        },
        {
            id: 'party',
            sort: 'partyName',
            header: t('engineSettings.columns.party'),
            cell: (row) => (row.partyName === '' ? row.partyId : row.partyName),
        },
        {
            id: 'groups',
            sort: 'groupsSet',
            header: t('engineSettings.columns.groups'),
            cell: (row) =>
                t('engineSettings.cell.groups', {
                    set: String(row.groupsSet),
                    count: String(row.groups),
                }),
            mono: true,
            numeric: true,
        },
        {
            id: 'threads',
            sort: 'threads',
            header: t('engineSettings.columns.threads'),
            cell: (row) =>
                row.threads === null ? (
                    <span className="text-ink-faint">{t('engineSettings.cell.default')}</span>
                ) : (
                    String(row.threads)
                ),
            mono: true,
            numeric: true,
        },
        {
            id: 'modifiedBy',
            sort: 'modifiedBy',
            header: t('engineSettings.columns.modifiedBy'),
            cell: (row) => row.modifiedBy,
        },
        {
            id: 'recordedAt',
            sort: 'recordedAt',
            header: t('engineSettings.columns.changed'),
            cell: (row) => <RelativeTime at={row.recordedAt} />,
        },
    ];
    return showParty ? columns : columns.filter((column) => column.id !== 'party');
}

/** Adds a configuration with a name and a description. Its groups are set on its own page. */
function AddDialog({ onClose }: { readonly onClose: () => void }): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const navigate = useNavigate();
    const [name, setName] = useState('');
    const [description, setDescription] = useState('');
    const dirty = name.trim() !== '' || description.trim() !== '';
    const guard = useCloseGuard(dirty, onClose);
    const create = useMutation({
        mutationFn: () =>
            engineSettings.create({
                name: name.trim(),
                description: description.trim(),
                intent: { reason_code: NEW_RECORD_REASON, commentary: '' },
            }),
        onSuccess: async (document) => {
            await queries.invalidateQueries({ queryKey: ['records', 'engine-settings'] });
            onClose();
            void navigate(engineSettingsPath(document.id));
        },
    });
    return (
        <Dialog
            title={t('engineSettings.add.title')}
            name="engine-settings-add"
            onClose={guard.close}
            footer={
                guard.leaving ? (
                    <LeavingFooter onStay={guard.stay} onLeave={onClose} />
                ) : (
                    <>
                        <Button variant="ghost" onClick={guard.close}>
                            {t('common.cancel')}
                        </Button>
                        <Button
                            variant="primary"
                            icon="save"
                            disabled={name.trim() === ''}
                            pending={create.isPending}
                            onClick={() => create.mutate()}
                        >
                            {t('engineSettings.add.create')}
                        </Button>
                    </>
                )
            }
        >
            <div className="space-y-4">
                <p className="text-sm text-ink-muted">{t('engineSettings.add.lead')}</p>
                <Field label={t('engineSettings.fields.name')}>
                    <Input
                        value={name}
                        maxLength={200}
                        onChange={(event) => setName(event.target.value)}
                    />
                </Field>
                <Field label={t('engineSettings.fields.description')}>
                    <Textarea
                        value={description}
                        rows={3}
                        maxLength={2000}
                        onChange={(event) => setDescription(event.target.value)}
                    />
                </Field>
                {create.isError && <Notice tone="error">{create.error.message}</Notice>}
            </div>
        </Dialog>
    );
}

export function EngineSettingsPage(): ReactNode {
    const { t } = useTranslation();
    const holds = useHolds();
    const scope = usePartyScope({
        label: t('engineSettings.columns.party'),
        all: t('engineSettings.allParties'),
    });
    const columns = useColumns(scope.several);
    const source = sourceOf(scope.filter);
    const [adding, setAdding] = useState(false);
    const trail = [...useAreaRoot('reporting'), { label: t('engineSettings.title') }];
    if (scope.pending || scope.error !== null) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={trail} />}
                title={t('engineSettings.title')}
                description={t('engineSettings.lead')}
            >
                {scope.error === null ? (
                    <p className="text-sm text-ink-muted" role="status">
                        {t('common.loading')}
                    </p>
                ) : (
                    <Notice tone="error">{scope.error}</Notice>
                )}
            </ScreenFrame>
        );
    }
    return (
        <>
            <RecordList
                source={source}
                title={t('engineSettings.title')}
                lead={t('engineSettings.lead')}
                crumbs={trail}
                columns={columns}
                pathOf={(row) => engineSettingsPath(row.id)}
                actions={
                    holds(PERMISSION.engineSettingsWrite) ? (
                        <Button icon="add" variant="primary" onClick={() => setAdding(true)}>
                            {t('engineSettings.add.button')}
                        </Button>
                    ) : null
                }
            />
            {adding && <AddDialog onClose={() => setAdding(false)} />}
        </>
    );
}

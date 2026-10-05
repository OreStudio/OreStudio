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

import { keepPreviousData, useQuery } from '@tanstack/react-query';
import { useEffect, useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import {
    SYSTEM_TENANT_ID,
    type TenantStatus,
    type TenantSummary,
    type TenantType,
} from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { RemoveTenantDialog } from './RemoveTenantDialog.js';
import { PaintedValue, SetupCell } from './TenantParts.js';
import { DEFAULT_PAGE_SIZE, Pager, pageBounds } from '../ui/Pager.js';
import { Button, Input, LinkButton, Notice, PageHeader, Select } from '../ui/Primitives.js';

/** How long the search waits after the last keystroke before it asks. */
const SEARCH_PAUSE_MS = 300;

/**
 * The tenants a deployment holds.
 *
 * This is the roster the system administration area opens on: a read-only list
 * of every tenant somebody set up, with the state each one is in. It writes
 * nothing. The destructive operations are their own journey, so a person who
 * only wants to know what exists never has to open one.
 *
 * The deployment's own record, the system tenant, is on this roster like any
 * other, and opening it is how the system administrator reaches the
 * deployment's own data.
 *
 * A deployment that holds no tenant of its own is the state right after the
 * system administrator is created, and it is a normal state rather than an
 * error: the empty roster says so and offers the journey that leaves it.
 */
export function TenantsPage(): ReactNode {
    const { t, plural } = useTranslation();
    const [typed, setTyped] = useState('');
    const [search, setSearch] = useState('');
    const [type, setType] = useState('');
    const [status, setStatus] = useState('');
    const [includeTest, setIncludeTest] = useState(false);
    const [offset, setOffset] = useState(0);

    /*
     * The search asks once the person pauses, not on every keystroke, and a
     * new search starts from the first page: the page the person was on
     * belongs to the old one.
     */
    useEffect(() => {
        const timer = setTimeout(() => {
            setSearch(typed.trim());
            setOffset(0);
        }, SEARCH_PAUSE_MS);
        return () => clearTimeout(timer);
    }, [typed]);

    /*
     * The previous page stays on screen while the next one loads, so the table
     * does not collapse to a loading line between pages.
     */
    const roster = useQuery({
        queryKey: ['tenants', search, type, status, includeTest, offset],
        queryFn: () =>
            api.tenants({ search, type, status, includeTest, offset, limit: DEFAULT_PAGE_SIZE }),
        placeholderData: keepPreviousData,
    });

    /** A filter that changes starts from the first page, as a search does. */
    const refilter = (apply: () => void) => {
        apply();
        setOffset(0);
    };

    /*
     * A page can empty under the person -- a tenant retired while they were on
     * the last page -- and an empty page past the first is not an answer they
     * can use, so the roster goes back to the first page. The total still
     * arrives with an empty page, so the screen never claims there is none.
     */
    const emptyPastFirst =
        roster.data !== undefined && roster.data.tenants.length === 0 && offset > 0;
    useEffect(() => {
        if (emptyPastFirst) {
            setOffset(0);
        }
    }, [emptyPastFirst]);
    /*
     * The statuses are a second read because their words and colours are
     * reference data shared by every screen that shows one. A failure to read
     * them is not a failure to read the roster: the status is still shown, as
     * the code the server sent.
     */
    const statuses = useQuery({ queryKey: ['tenant-statuses'], queryFn: api.tenantStatuses });
    /* The types paint the type column and fill its filter. */
    const types = useQuery({ queryKey: ['tenant-types'], queryFn: api.tenantTypes });
    const typeChoices = types.data ?? [];
    const [removing, setRemoving] = useState<TenantSummary | null>(null);

    if (roster.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }

    if (roster.isError) {
        const reason = roster.error instanceof Error ? roster.error.message : String(roster.error);
        return (
            <div>
                <PageHeader title={t('tenants.title')} description={t('tenants.failed')} />
                <Notice tone="error">{reason}</Notice>
            </div>
        );
    }

    const { tenants, totalCount, setupUnavailable, hiddenTestCount } = roster.data;
    const filtered = search !== '' || type !== '' || status !== '';

    /*
     * The page is the roster. A table inside a card that already carries the
     * title and the count is a box in a box, and the second box only makes the
     * table narrower than the screen it was given.
     *
     * The primary action sits in the header rather than only on the empty
     * state, because a person who has tenants and wants another one is the
     * common case, and the empty state's own button cannot serve them.
     */
    return (
        <div>
            <PageHeader
                title={t('tenants.title')}
                description={t('tenants.description')}
                actions={
                    <LinkButton to="/tenants/new" variant="primary">
                        {t('tenants.add')}
                    </LinkButton>
                }
            />
            {removing !== null && (
                <RemoveTenantDialog
                    tenant={removing}
                    onClose={() => setRemoving(null)}
                    onRemoved={() => setRemoving(null)}
                />
            )}
            {setupUnavailable && (
                <div className="mb-4">
                    <Notice tone="warn">{t('tenants.setupUnavailable')}</Notice>
                </div>
            )}
            {/*
             * A deployment with no tenant at all has nothing to search, so the
             * controls appear once there is something to find. One holding only
             * test tenants still offers the toggle that shows them.
             */}
            {totalCount === 0 && !filtered && !includeTest && hiddenTestCount === 0 ? (
                <EmptyRoster />
            ) : (
                <>
                    <div className="mb-4 flex flex-wrap items-center gap-3">
                        <div className="w-full max-w-sm">
                            <Input
                                type="search"
                                aria-label={t('tenants.search')}
                                placeholder={t('tenants.search')}
                                value={typed}
                                onChange={(event) => setTyped(event.target.value)}
                            />
                        </div>
                        <Select
                            aria-label={t('tenants.filterType')}
                            value={type}
                            onChange={(event) => refilter(() => setType(event.target.value))}
                        >
                            <option value="">{t('tenants.allTypes')}</option>
                            {typeChoices.map((choice) => (
                                <option key={choice.code} value={choice.code}>
                                    {choice.name}
                                </option>
                            ))}
                        </Select>
                        <Select
                            aria-label={t('tenants.filterStatus')}
                            value={status}
                            onChange={(event) => refilter(() => setStatus(event.target.value))}
                        >
                            <option value="">{t('tenants.allStatuses')}</option>
                            {(statuses.data ?? []).map((choice) => (
                                <option key={choice.code} value={choice.code}>
                                    {choice.name}
                                </option>
                            ))}
                        </Select>
                        <label className="flex items-center gap-2 text-sm text-ink-muted">
                            <input
                                type="checkbox"
                                checked={includeTest}
                                onChange={(event) =>
                                    refilter(() => setIncludeTest(event.target.checked))
                                }
                            />
                            {t('tenants.showTest')}
                        </label>
                        {!includeTest && hiddenTestCount > 0 && (
                            <span className="text-xs text-ink-faint">
                                {plural('tenants.hiddenTest', hiddenTestCount)}
                            </span>
                        )}
                    </div>
                    {tenants.length === 0 ? (
                        <p className="text-sm text-ink-muted">
                            {search === ''
                                ? t('tenants.noneFiltered')
                                : t('tenants.noMatch', { search })}
                        </p>
                    ) : (
                        <Roster
                            tenants={tenants}
                            statuses={statuses.data ?? []}
                            types={types.data ?? []}
                            onRemove={setRemoving}
                        />
                    )}
                    <Pager
                        offset={offset}
                        shown={tenants.length}
                        total={totalCount}
                        pageSize={DEFAULT_PAGE_SIZE}
                        showing={plural(
                            'tenants.showing',
                            totalCount,
                            pageBounds(offset, tenants.length),
                        )}
                        onMove={setOffset}
                    />
                </>
            )}
        </div>
    );
}

function Roster({
    tenants,
    statuses,
    types,
    onRemove,
}: {
    readonly tenants: readonly TenantSummary[];
    readonly statuses: readonly TenantStatus[];
    readonly types: readonly TenantType[];
    readonly onRemove: (tenant: TenantSummary) => void;
}): ReactNode {
    const statusByCode = new Map(statuses.map((status) => [status.code, status]));
    const typeByCode = new Map(types.map((type) => [type.code, type]));

    const { t } = useTranslation();

    return (
        <div className="overflow-x-auto">
            <table className="w-full text-left text-sm">
                <thead className="border-b border-line text-[11px] uppercase tracking-wide text-ink-faint">
                    <tr>
                        <th className="py-2 pr-4 font-medium">{t('tenants.code')}</th>
                        <th className="py-2 pr-4 font-medium">{t('tenants.name')}</th>
                        <th className="py-2 pr-4 font-medium">{t('tenants.hostname')}</th>
                        <th className="py-2 pr-4 font-medium">{t('tenants.type')}</th>
                        <th className="py-2 pr-4 font-medium">{t('tenants.status')}</th>
                        <th className="py-2 pr-4 font-medium">{t('tenants.setup')}</th>
                        <th className="py-2 font-medium">
                            <span className="sr-only">{t('tenants.actions')}</span>
                        </th>
                    </tr>
                </thead>
                <tbody>
                    {tenants.map((tenant) => (
                        <tr key={tenant.id} className="border-b border-line-subtle">
                            <td className="py-2.5 pr-4 font-mono text-xs">{tenant.code}</td>
                            <td className="py-2.5 pr-4">
                                <Link
                                    to={`/tenants/${encodeURIComponent(tenant.code)}`}
                                    className="underline-offset-2 hover:underline"
                                >
                                    {tenant.name}
                                </Link>
                            </td>
                            <td className="py-2.5 pr-4 font-mono text-xs">{tenant.hostname}</td>
                            <td className="py-2.5 pr-4">
                                <PaintedValue
                                    value={tenant.type}
                                    known={typeByCode.get(tenant.type)}
                                />
                            </td>
                            <td className="py-2.5 pr-4">
                                <PaintedValue
                                    value={tenant.status}
                                    known={statusByCode.get(tenant.status)}
                                />
                            </td>
                            <td className="py-2.5 pr-4">
                                <SetupCell setup={tenant.setup} />
                            </td>
                            <td className="py-2.5 text-right">
                                <RowActions tenant={tenant} onRemove={onRemove} />
                            </td>
                        </tr>
                    ))}
                </tbody>
            </table>
        </div>
    );
}

/**
 * What a person can do to one tenant, from its row.
 *
 * A native disclosure holds the menu, so it opens without script and a
 * keyboard reaches it as it reaches any other control. Opening the tenant
 * leads to its own screen. Resuming setup is offered when the tenant's
 * provisioning run has not completed. Removing it asks first, and is not
 * offered for the system tenant, which holds the deployment itself.
 */
function RowActions({
    tenant,
    onRemove,
}: {
    readonly tenant: TenantSummary;
    readonly onRemove: (tenant: TenantSummary) => void;
}): ReactNode {
    const { t } = useTranslation();
    const setup = tenant.setup;
    const resumable = setup !== null && setup.status !== 'completed';
    /*
     * The menu sits in the cell's flow, not over the next row: the table's
     * wrapper scrolls sideways, which clips anything positioned outside it,
     * and a roster of one row has no room below for a floating menu.
     */
    return (
        <details
            className="text-left"
            onKeyDown={(event) => {
                if (event.key === 'Escape') event.currentTarget.open = false;
            }}
        >
            <summary
                className="ml-auto w-fit cursor-pointer list-none rounded px-2 text-ink-muted hover:bg-surface-hover focus-visible:outline focus-visible:outline-2 focus-visible:outline-accent"
                aria-label={t('tenants.actionsFor', { name: tenant.name })}
            >
                …
            </summary>
            <ul className="mt-1 w-56 rounded-md border border-line bg-surface-overlay py-1 text-sm shadow-lg">
                {resumable && (
                    <li>
                        <Link
                            to={`/tenants/runs/${encodeURIComponent(setup.instanceId)}`}
                            className="block px-3 py-1.5 hover:bg-surface-hover"
                        >
                            {t('tenants.resumeSetup')}
                        </Link>
                    </li>
                )}
                <li>
                    <Link
                        to={`/tenants/${encodeURIComponent(tenant.code)}`}
                        className="block px-3 py-1.5 hover:bg-surface-hover"
                    >
                        {t('tenants.open')}
                    </Link>
                </li>
                {tenant.id !== SYSTEM_TENANT_ID && (
                    <li>
                        <button
                            type="button"
                            onClick={(event) => {
                                event.currentTarget.closest('details')?.removeAttribute('open');
                                onRemove(tenant);
                            }}
                            className="block w-full px-3 py-1.5 text-left text-down hover:bg-surface-hover"
                        >
                            {t('tenants.removeAction')}
                        </button>
                    </li>
                )}
            </ul>
        </details>
    );
}

/**
 * What a deployment with no tenant of its own reads.
 *
 * It states the state and points at the header's action rather than repeating
 * it: two buttons that do the same thing, one above the other, is a screen
 * asking a question it has already answered.
 */
function EmptyRoster(): ReactNode {
    const { t } = useTranslation();

    return (
        <div className="rounded-md border border-line px-4 py-6">
            <p className="text-sm font-medium">{t('tenants.empty.title')}</p>
            <p className="mt-1 text-sm text-ink-muted">{t('tenants.empty.body')}</p>
        </div>
    );
}

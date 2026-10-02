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
import type { TenantSetup, TenantStatus, TenantSummary } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { Button, Input, LinkButton, Notice, PageHeader } from '../ui/Primitives.js';

/** How many tenants one page of the roster shows. */
export const TENANT_PAGE_SIZE = 25;

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
 * The system tenant is not here because it is not a tenant somebody set up: it
 * is the deployment's own bookkeeping. The server's search leaves it out, so this
 * screen cannot show it by accident.
 *
 * A deployment that holds no tenant of its own is the state right after the
 * system administrator is created, and it is a normal state rather than an
 * error: the empty roster says so and offers the journey that leaves it.
 */
export function TenantsPage(): ReactNode {
    const { t } = useTranslation();
    const [typed, setTyped] = useState('');
    const [search, setSearch] = useState('');
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
        queryKey: ['tenants', search, offset],
        queryFn: () => api.tenants({ search, offset, limit: TENANT_PAGE_SIZE }),
        placeholderData: keepPreviousData,
    });

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

    const { tenants, totalCount, setupUnavailable } = roster.data;

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
                        {t('shell.journey.newTenant')}
                    </LinkButton>
                }
            />
            {setupUnavailable && (
                <div className="mb-4">
                    <Notice tone="warn">{t('tenants.setupUnavailable')}</Notice>
                </div>
            )}
            {/*
             * A deployment with no tenant at all has nothing to search, so the
             * box appears once there is something to find.
             */}
            {totalCount === 0 && search === '' ? (
                <EmptyRoster />
            ) : (
                <>
                    <div className="mb-4 max-w-sm">
                        <Input
                            type="search"
                            aria-label={t('tenants.search')}
                            placeholder={t('tenants.search')}
                            value={typed}
                            onChange={(event) => setTyped(event.target.value)}
                        />
                    </div>
                    {tenants.length === 0 ? (
                        <p className="text-sm text-ink-muted">{t('tenants.noMatch', { search })}</p>
                    ) : (
                        <Roster tenants={tenants} statuses={statuses.data ?? []} />
                    )}
                    <Pager
                        offset={offset}
                        shown={tenants.length}
                        total={totalCount}
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
}: {
    readonly tenants: readonly TenantSummary[];
    readonly statuses: readonly TenantStatus[];
}): ReactNode {
    const byCode = new Map(statuses.map((status) => [status.code, status]));

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
                        <th className="py-2 font-medium">{t('tenants.setup')}</th>
                    </tr>
                </thead>
                <tbody>
                    {tenants.map((tenant) => (
                        <tr key={tenant.id} className="border-b border-line-subtle">
                            <td className="py-2.5 pr-4 font-mono text-xs">{tenant.code}</td>
                            <td className="py-2.5 pr-4">{tenant.name}</td>
                            <td className="py-2.5 pr-4 font-mono text-xs">{tenant.hostname}</td>
                            <td className="py-2.5 pr-4 text-ink-muted">{tenant.type}</td>
                            <td className="py-2.5 pr-4">
                                <StatusBadge
                                    status={tenant.status}
                                    known={byCode.get(tenant.status)}
                                />
                            </td>
                            <td className="py-2.5">
                                <SetupCell setup={tenant.setup} />
                            </td>
                        </tr>
                    ))}
                </tbody>
            </table>
        </div>
    );
}

/**
 * The state a tenant is in, painted by the badge its status row names.
 *
 * The words are the status row's own — `Suspended`, `Terminated` — and the
 * colours, the tooltip and the severity are the badge's. The screen decides
 * neither, so a status reads the same wherever it appears and a deployment's
 * vocabulary is not replaced by the badge catalogue's.
 *
 * A status the deployment does not hold is drawn as the server wrote it, and so
 * is one whose badge has left the catalogue. A tenant in a state nobody has
 * described is exactly the row somebody needs to see, and a screen that
 * swallowed it would be hiding the interesting case.
 */
function StatusBadge({
    status,
    known,
}: {
    readonly status: string;
    readonly known: TenantStatus | undefined;
}): ReactNode {
    const badge = known?.badge ?? undefined;
    if (badge === undefined || badge.backgroundColour === '') {
        return <span className="text-ink-muted">{known?.name ?? status}</span>;
    }
    return (
        <span
            className="inline-block rounded-full px-2 py-0.5 text-[11px] leading-tight"
            style={{ backgroundColor: badge.backgroundColour, color: badge.textColour }}
            title={known?.description === '' ? undefined : known?.description}
        >
            {known?.name ?? status}
        </span>
    );
}

/** The colour each unfinished run state is drawn in. */
const SETUP_TONE: Record<string, string> = {
    in_progress: 'text-accent-bright',
    compensating: 'text-warn',
    failed: 'text-down',
    compensated: 'text-ink-faint',
};

/**
 * Where a tenant's provisioning run has got to, and the way back to it.
 *
 * A completed run says nothing, because the tenant's own status already says
 * the tenant is there. Every other run links to its rail, which is how a person
 * who left the journey returns to it: the run kept working on the server, and
 * a failed one is resumed from that page. A state this screen has no words for
 * is shown as the engine named it, and still links to the run.
 */
function SetupCell({ setup }: { readonly setup: TenantSetup | null }): ReactNode {
    const { t } = useTranslation();
    if (setup === null || setup.status === 'completed') {
        return null;
    }
    const known = setup.status in SETUP_TONE;
    const label = known
        ? t(`tenants.setupState.${setup.status}`, {
              step: setup.currentStepIndex + 1,
              count: setup.stepCount,
          })
        : setup.status;
    return (
        <Link
            to={`/tenants/runs/${encodeURIComponent(setup.instanceId)}`}
            className={`text-xs underline-offset-2 hover:underline ${SETUP_TONE[setup.status] ?? 'text-ink'}`}
            title={setup.error === '' ? undefined : setup.error}
        >
            {label}
        </Link>
    );
}

/**
 * Which rows the roster shows, and the way to the pages either side.
 *
 * The count is the server's, so it is every tenant that matches the search,
 * not the rows on this page.
 */
function Pager({
    offset,
    shown,
    total,
    onMove,
}: {
    readonly offset: number;
    readonly shown: number;
    readonly total: number;
    readonly onMove: (offset: number) => void;
}): ReactNode {
    const { t, plural } = useTranslation();
    const first = shown === 0 ? 0 : offset + 1;
    const last = offset + shown;
    return (
        <div className="mt-4 flex flex-wrap items-center justify-between gap-3 text-sm text-ink-muted">
            <span>{plural('tenants.showing', total, { first, last })}</span>
            <span className="flex gap-2">
                <Button
                    size="sm"
                    disabled={offset === 0}
                    onClick={() => onMove(Math.max(0, offset - TENANT_PAGE_SIZE))}
                >
                    {t('tenants.previous')}
                </Button>
                <Button
                    size="sm"
                    disabled={last >= total}
                    onClick={() => onMove(offset + TENANT_PAGE_SIZE)}
                >
                    {t('tenants.next')}
                </Button>
            </span>
        </div>
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

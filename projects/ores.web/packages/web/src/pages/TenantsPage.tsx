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

import { useQuery } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import type { BadgePresentation, TenantSummary } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { LinkButton, Notice, PageHeader } from '../ui/Primitives.js';

/**
 * The code domain that paints a tenant's status.
 *
 * A tenant row carries the status code and nothing else about how to draw it.
 * The badge catalogue holds that, keyed by the domain, so the screen names the
 * domain rather than a colour.
 */
const TENANT_STATUS_DOMAIN = 'tenant_status';

/**
 * The tenants a deployment holds.
 *
 * This is the roster the system administration area opens on: a read-only list
 * of every tenant somebody set up, with the state each one is in. It writes
 * nothing. The destructive operations are their own journey, so a person who
 * only wants to know what exists never has to open one.
 *
 * The system tenant is not here because it is not a tenant somebody set up: it
 * is the deployment's own bookkeeping. The read drops it, so this screen cannot
 * show it by accident.
 *
 * A deployment that holds no tenant of its own is the state right after the
 * system administrator is created, and it is a normal state rather than an
 * error: the empty roster says so and offers the journey that leaves it.
 */
export function TenantsPage(): ReactNode {
    const { t, plural } = useTranslation();
    const roster = useQuery({ queryKey: ['tenants'], queryFn: api.tenants });
    /*
     * The badges are a second read because they are reference data shared by
     * every screen that shows a tenant status. A failure to read them is not a
     * failure to read the roster: the status is still shown, as the value the
     * server sent.
     */
    const badges = useQuery({
        queryKey: ['badges', TENANT_STATUS_DOMAIN],
        queryFn: () => api.badges(TENANT_STATUS_DOMAIN),
    });

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

    const { tenants, totalCount } = roster.data;

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
                description={plural('tenants.count', totalCount)}
                actions={
                    <LinkButton to="/tenants/new" variant="primary">
                        {t('shell.journey.newTenant')}
                    </LinkButton>
                }
            />
            {tenants.length === 0 ? (
                <EmptyRoster />
            ) : (
                <Roster tenants={tenants} badges={badges.data?.badges ?? {}} />
            )}
        </div>
    );
}

function Roster({
    tenants,
    badges,
}: {
    readonly tenants: readonly TenantSummary[];
    readonly badges: Readonly<Record<string, BadgePresentation>>;
}): ReactNode {
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
                        <th className="py-2 font-medium">{t('tenants.status')}</th>
                    </tr>
                </thead>
                <tbody>
                    {tenants.map((tenant) => (
                        <tr key={tenant.id} className="border-b border-line-subtle">
                            <td className="py-2.5 pr-4 font-mono text-xs">{tenant.code}</td>
                            <td className="py-2.5 pr-4">{tenant.name}</td>
                            <td className="py-2.5 pr-4 font-mono text-xs">{tenant.hostname}</td>
                            <td className="py-2.5 pr-4 text-ink-muted">{tenant.type}</td>
                            <td className="py-2.5">
                                <StatusBadge status={tenant.status} badge={badges[tenant.status]} />
                            </td>
                        </tr>
                    ))}
                </tbody>
            </table>
        </div>
    );
}

/**
 * The state a tenant is in, painted by the badge catalogue.
 *
 * The colours, the label and the words behind the badge are reference data:
 * `ores.dq` holds the badge catalogue, and a mapping says which badge a
 * `tenant_status` value gets. The screen decides none of it, so a status looks
 * the same wherever it is shown.
 *
 * A value with no badge is drawn as the server wrote it rather than hidden. A
 * tenant in a state nobody has mapped is exactly the row somebody needs to see,
 * and a screen that swallowed it would be hiding the interesting case.
 */
function StatusBadge({
    status,
    badge,
}: {
    readonly status: string;
    readonly badge: BadgePresentation | undefined;
}): ReactNode {
    if (badge === undefined || badge.backgroundColour === '') {
        return <span className="text-ink-muted">{status}</span>;
    }
    return (
        <span
            className="inline-block rounded-full px-2 py-0.5 text-[11px] leading-tight"
            style={{ backgroundColor: badge.backgroundColour, color: badge.textColour }}
            title={badge.description === '' ? undefined : badge.description}
        >
            {badge.label}
        </span>
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

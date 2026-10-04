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
import { useState, type ReactNode } from 'react';
import type { TenantParty } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { Notice, PageHeader } from '../ui/Primitives.js';
import { CentreFlag, imageUrl } from '../ui/Images.js';
import { Pager, pageBounds } from '../ui/Pager.js';

/** How many parties one page shows. */
export const PARTY_PAGE_SIZE = 25;

/**
 * The parties of the session's own tenant, a page at a time.
 *
 * The tenant is the session's, so the read names none: row-level security
 * scopes it from the session's token. A system administrator reads a tenant's
 * parties here after entering the tenant. The server pages in key order, and
 * a parent is named when it is on the same page.
 */
export function PartiesPage(): ReactNode {
    const { t, plural } = useTranslation();
    const [offset, setOffset] = useState(0);
    const read = useQuery({
        queryKey: ['parties', offset],
        queryFn: () => api.parties({ offset, limit: PARTY_PAGE_SIZE }),
        placeholderData: keepPreviousData,
    });

    if (read.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }

    if (read.isError) {
        const reason = read.error instanceof Error ? read.error.message : String(read.error);
        return (
            <div>
                <PageHeader title={t('parties.title')} description={t('parties.failed')} />
                <Notice tone="error">{reason}</Notice>
            </div>
        );
    }

    const { parties, totalCount } = read.data;
    return (
        <div>
            <PageHeader title={t('parties.title')} description={t('parties.description')} />
            <div className="overflow-x-auto">
                <table className="w-full text-left text-sm">
                    <thead>
                        <tr className="border-b border-line text-xs text-ink-muted">
                            <th className="py-2 pr-4 font-medium">{t('parties.code')}</th>
                            <th className="py-2 pr-4 font-medium">{t('parties.name')}</th>
                            <th className="py-2 pr-4 font-medium">{t('parties.category')}</th>
                            <th className="py-2 pr-4 font-medium">{t('parties.type')}</th>
                            <th className="py-2 pr-4 font-medium">{t('parties.status')}</th>
                            <th className="py-2 pr-4 font-medium">{t('parties.businessCentre')}</th>
                            <th className="py-2 font-medium">{t('parties.parent')}</th>
                        </tr>
                    </thead>
                    <tbody>
                        {parties.map((party) => (
                            <PartyRow key={party.id} party={party} />
                        ))}
                    </tbody>
                </table>
            </div>
            <Pager
                offset={offset}
                shown={parties.length}
                total={totalCount}
                pageSize={PARTY_PAGE_SIZE}
                showing={plural('parties.showing', totalCount, pageBounds(offset, parties.length))}
                onMove={setOffset}
            />
        </div>
    );
}

function PartyRow({ party }: { readonly party: TenantParty }): ReactNode {
    const { t } = useTranslation();
    return (
        <tr className="border-b border-line-subtle">
            <td className="py-2.5 pr-4 font-mono text-xs">{party.code}</td>
            <td className="py-2.5 pr-4">{party.name}</td>
            <td className="py-2.5 pr-4">{party.category}</td>
            <td className="py-2.5 pr-4">{party.type}</td>
            <td className="py-2.5 pr-4">{party.status}</td>
            <td className="py-2.5 pr-4">
                <CentreFlag
                    code={party.businessCentreCode}
                    src={party.flagImageId === null ? null : imageUrl(party.flagImageId)}
                />
            </td>
            <td className="py-2.5">
                {party.parentId === null ? (
                    ''
                ) : party.parentName !== null ? (
                    party.parentName
                ) : (
                    <span className="text-ink-faint">{t('parties.parentElsewhere')}</span>
                )}
            </td>
        </tr>
    );
}

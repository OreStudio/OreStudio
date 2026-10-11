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

import type { TenantParty } from '@ores/wire-protocol/browser';
import { FlaggedCode } from '../images/flags.js';
import type { ListColumn } from '../refdata/RecordList.js';
import { imageUrl } from '../ui/Images.js';

/**
 * The columns of a list of parties. `tenant` names the tenant whose images
 * the flags come from, when it is not the session's own.
 */
export function partyColumns(
    t: (key: string) => string,
    tenant?: string,
): readonly ListColumn<TenantParty>[] {
    return [
        {
            id: 'code',
            header: t('parties.code'),
            cell: (party) => party.code,
            mono: true,
            sort: 'short_code',
        },
        { id: 'name', header: t('parties.name'), cell: (party) => party.name, sort: 'full_name' },
        {
            id: 'category',
            header: t('parties.category'),
            cell: (party) => party.category,
            sort: 'party_category',
        },
        { id: 'type', header: t('parties.type'), cell: (party) => party.type, sort: 'party_type' },
        {
            id: 'status',
            header: t('parties.status'),
            cell: (party) => party.status,
            sort: 'status',
        },
        {
            id: 'business_centre',
            header: t('parties.businessCentre'),
            sort: 'business_center_code',
            cell: (party) => (
                <FlaggedCode
                    code={party.businessCentreCode}
                    src={party.flagImageId === null ? null : imageUrl(party.flagImageId, tenant)}
                />
            ),
        },
        {
            id: 'parent',
            header: t('parties.parent'),
            cell: (party) =>
                party.parentId === null ? (
                    <span className="text-ink-faint">{t('parties.topOfGroup')}</span>
                ) : party.parentName !== null ? (
                    party.parentName
                ) : (
                    <span className="text-ink-faint">{t('parties.parentElsewhere')}</span>
                ),
        },
    ];
}

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

import type { ReactNode } from 'react';
import { useParams } from 'react-router';
import { Crumbs } from '../refdata/shared.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { api } from '../api/client.js';
import { Avatar, imageUrl } from '../ui/Images.js';
import { PageHeader } from '../ui/Primitives.js';
import { displayName } from './names.js';
import { SignInsPanel } from './SignIns.js';

/**
 * One account of a tenant, opened from the tenant's People tab in system
 * administration: who it is, what kind of account, and its sign-ins.
 *
 * The read runs inside the tenant, or as the session for the system tenant,
 * where the platform's own services sign in. It writes nothing.
 */
export function TenantAccountPage(): ReactNode {
    const { code = '', username = '' } = useParams();

    return (
        <div className="space-y-6">
            <Crumbs
                parts={[
                    ...useAreaRoot('tenants'),
                    { label: code, to: `/tenants/${encodeURIComponent(code)}?tab=people` },
                    { label: username },
                ]}
            />
            <SignInsPanel
                queryKey={['tenant-account-sign-ins', code, username]}
                read={(page) => api.tenantAccountSignIns(code, username, page)}
                header={(account) => {
                    const name = displayName(account, account.username);
                    return (
                        <div className="flex items-center gap-4">
                            <Avatar
                                name={name}
                                size="lg"
                                src={
                                    account.imageId === null
                                        ? null
                                        : imageUrl(account.imageId, code)
                                }
                            />
                            <PageHeader
                                title={name}
                                description={[account.username, account.email, account.jobTitle]
                                    .filter((part) => part !== '')
                                    .join(' · ')}
                            />
                        </div>
                    );
                }}
            />
        </div>
    );
}

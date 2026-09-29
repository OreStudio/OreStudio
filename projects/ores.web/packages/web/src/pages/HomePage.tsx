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
import { useTranslation } from '../i18n/Provider.js';
import { Detail, LinkButton, PageHeader } from '../ui/Primitives.js';

/**
 * Where a signed-in person lands.
 *
 * The journeys are what fill this shell, and they arrive one group at a time;
 * until then the root shows what the session actually holds, which is the
 * tenant and the party the person is working in. A screen that shows real state
 * is readable; a hero that promises features is not.
 */
export interface HomePageProps {
    readonly username: string;
    readonly email: string;
    readonly tenantName: string;
    readonly partyName: string;
}

export function HomePage({ username, email, tenantName, partyName }: HomePageProps): ReactNode {
    const { t } = useTranslation();

    return (
        <div className="card p-6">
            <PageHeader title={t('home.title')} description={t('home.next')} />
            <dl className="grid gap-4 sm:grid-cols-2">
                <Detail label={t('home.username')} value={username} />
                <Detail label={t('home.email')} value={email} />
                <Detail label={t('home.tenant')} value={tenantName} />
                <Detail label={t('home.party')} value={partyName} />
            </dl>
            {/*
             * The way into the journey a signed-in administrator runs: a tenant
             * is added from here until the Tenants page is its permanent home.
             */}
            <div className="mt-6">
                <LinkButton to="/tenants/new">{t('home.newTenant')}</LinkButton>
            </div>
        </div>
    );
}

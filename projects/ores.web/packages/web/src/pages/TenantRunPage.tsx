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

import { useQuery, useQueryClient } from '@tanstack/react-query';
import { useCallback, type ReactNode } from 'react';
import { useParams } from 'react-router';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { RunProgress } from '../journeys/parts.js';
import { useJourneyServer } from '../journeys/server.js';
import { LinkButton, Notice, PageHeader } from '../ui/Primitives.js';

/**
 * A tenant's provisioning run, opened from the roster.
 *
 * This is the way back into a setup journey somebody left. The journey's form
 * is not restored, because nothing it held was written before the run started;
 * what exists is the run, so the page is the journey's provisioning step on its
 * own: the same rail, the same progress read, and the same retry.
 *
 * The tenant's name comes from the roster the person opened this from. A link
 * opened cold reads the roster again, and a run no row names is still shown,
 * because the run is what the person came for.
 */
export function TenantRunPage(): ReactNode {
    const { t } = useTranslation();
    const { instanceId = '' } = useParams();
    const server = useJourneyServer();
    const queries = useQueryClient();
    const roster = useQuery({ queryKey: ['tenants'], queryFn: api.tenants });
    const tenant = roster.data?.tenants.find((row) => row.setup?.instanceId === instanceId);

    /*
     * A run that completes changes the roster's answer for its tenant, so the
     * cached roster is dropped rather than left to say the run is still going.
     */
    const onCompleted = useCallback(() => {
        void queries.invalidateQueries({ queryKey: ['tenants'] });
    }, [queries]);

    const completed = tenant?.setup?.status === 'completed';

    return (
        <div>
            <PageHeader
                title={
                    tenant === undefined
                        ? t('tenants.run.unknownTenant')
                        : t('tenants.run.title', { name: tenant.name })
                }
                description={t('tenants.run.lead')}
                actions={
                    <LinkButton to="/tenants" variant="secondary">
                        {t('tenants.run.back')}
                    </LinkButton>
                }
            />
            {completed && (
                <div className="mb-4">
                    <Notice tone="info">{t('tenants.run.done')}</Notice>
                </div>
            )}
            <div className="card p-6">
                <RunProgress server={server} instanceId={instanceId} onCompleted={onCompleted} />
            </div>
        </div>
    );
}

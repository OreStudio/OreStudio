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
import { Link } from 'react-router';
import type { HeldRole, SessionView } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { ContactTab, IdentityTab } from '../access/PersonForms.js';
import { roleLabel } from '../access/words.js';
import { Detail, Notice, PageHeader } from '../ui/Primitives.js';
import { useTabs } from '../ui/Tabs.js';

/**
 * My profile: only the signed-in person's own account, one part per tab:
 * who I am, how to reach me, and the access I hold. Other people are managed
 * from People, not from here.
 *
 * The journeys are `doc/knowledge/journeys/profile/journey_present_myself.org`
 * and `journey_keep_details_current.org`.
 */
export function ProfilePage({ session }: { readonly session: SessionView }): ReactNode {
    const { t } = useTranslation();
    const access = useQuery({ queryKey: ['my-access'], queryFn: api.myAccess });
    const { tab, bar } = useTabs({
        label: t('profile.title.mine'),
        tabs: ['details', 'contact', 'access'],
        titleOf: (name) => t(`profile.tabs.${name}`),
    });

    return (
        <div className="space-y-4">
            <PageHeader title={t('profile.title.mine')} description={t('profile.lead.mine')} />
            {access.isError && (
                <Notice tone="warn">
                    {t('profile.access.failed', { reason: access.error.message })}
                </Notice>
            )}
            {bar}
            {tab === 'details' && (
                <IdentityTab username={session.username} me fallbackEmail={session.email} />
            )}
            {tab === 'contact' && (
                <ContactTab username={session.username} me fallbackEmail={session.email} />
            )}
            {tab === 'access' && <AccessPanel roles={access.data?.roles ?? []} />}
        </div>
    );
}

function AccessPanel({ roles }: { readonly roles: readonly HeldRole[] }): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-3 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">{t('profile.access.title')}</h2>
                <p className="text-sm text-ink-muted">{t('profile.access.lead')}</p>
            </header>
            <Detail
                label={t('profile.access.roles')}
                value={
                    roles.length === 0
                        ? t('profile.access.none')
                        : roles.map((role) => roleLabel(t, role.name)).join(', ')
                }
            />
            <p className="text-sm text-ink-muted">
                {t('profile.access.protectWhy')}{' '}
                <Link to="/security" className="text-accent-bright hover:underline">
                    {t('profile.access.protect')}
                </Link>
                .
            </p>
            <p className="text-sm text-ink-muted">
                {t('profile.access.knowWhy')}{' '}
                <Link to="/access" className="text-accent-bright hover:underline">
                    {t('profile.access.know')}
                </Link>
                .
            </p>
        </section>
    );
}

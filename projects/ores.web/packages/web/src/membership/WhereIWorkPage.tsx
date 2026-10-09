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

import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import type { MyParty } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { AccountPicture } from '../ui/Images.js';
import { Button, Detail, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { displayName } from '../access/names.js';

/**
 * Where I work: the parties the signed-in person works in.
 *
 * One card per party, with the two things a person does to it — act for it, or
 * make it the default — on the card they change, so nothing depends on a
 * selection made somewhere else on the page.
 *
 * The acting party and the default are different facts and the cards state
 * both. The acting party decides what every screen reads now, and it changes
 * when the person switches; the default decides what quick sign-in picks next
 * time, and it changes when the person sets it.
 */
export function WhereIWorkPage({
    tenantName,
    username,
    email,
    actingPartyId,
    onSwitchParty,
}: {
    readonly tenantName: string;
    readonly username: string;
    readonly email: string;
    /** The party the session is acting for, which the session states. */
    readonly actingPartyId: string;
    /**
     * Re-scopes the open session to another of the person's own parties.
     *
     * Switching is the session's, so the screen is handed the action rather
     * than reaching for the session context.
     */
    readonly onSwitchParty: (partyId: string) => Promise<void>;
}): ReactNode {
    const { t } = useTranslation();
    const queryClient = useQueryClient();
    const [failure, setFailure] = useState('');

    const parties = useQuery({ queryKey: ['my-parties'], queryFn: api.myParties });
    /*
     * The shell reads the signed-in person's own account for the name and
     * picture in its menu, and this is the same answer. A member who may not
     * hold the account read keeps the username the session states, so the query
     * is allowed to fail here rather than being treated as the screen's error.
     */
    const self = useQuery({
        queryKey: ['account', username],
        queryFn: () => api.account(username),
        enabled: username !== '',
        retry: false,
    });

    const afterWrite = async () => {
        setFailure('');
        await queryClient.invalidateQueries({ queryKey: ['my-parties'] });
    };

    const setDefault = useMutation({
        mutationFn: (partyId: string) => api.setMyDefaultParty(partyId),
        onSuccess: afterWrite,
        onError: (error: Error) => setFailure(error.message),
    });

    const switchTo = useMutation({
        mutationFn: (partyId: string) => onSwitchParty(partyId),
        onSuccess: afterWrite,
        onError: (error: Error) => setFailure(error.message),
    });

    if (parties.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (parties.isError) {
        return <Notice tone="error">{parties.error.message}</Notice>;
    }

    const mine = parties.data.parties;
    const storedDefaultId = parties.data.defaultPartyId;
    const acting = mine.find((party) => party.partyId === actingPartyId);
    const busy = setDefault.isPending || switchTo.isPending;
    const name = displayName(self.data, username);

    return (
        <div className="space-y-6">
            <PageHeader
                title={t('membership.where.title')}
                description={t('membership.where.lead', { tenant: tenantName })}
            />

            {failure !== '' && <Notice tone="error">{failure}</Notice>}

            <section className="rounded-md border border-line bg-surface-raised p-4">
                <div className="flex items-center gap-4">
                    <AccountPicture username={username} name={name} size="lg" />
                    <div className="min-w-0">
                        <div className="text-base font-semibold">{name}</div>
                        {self.data != null && self.data.jobTitle !== '' && (
                            <div className="text-sm text-ink-muted">{self.data.jobTitle}</div>
                        )}
                        <div className="text-xs text-ink-faint">{email}</div>
                    </div>
                </div>
            </section>

            <section className="rounded-md border border-line bg-surface-raised p-4">
                <h2 className="text-sm font-semibold">{t('membership.where.acting')}</h2>
                {acting === undefined ? (
                    <p className="mt-2 text-sm text-ink-muted">
                        {t('membership.where.actingUnknown')}
                    </p>
                ) : (
                    <>
                        <div className="mt-2 text-base font-medium">{acting.name}</div>
                        <dl className="mt-3 grid grid-cols-2 gap-4 sm:grid-cols-4">
                            <Detail label={t('membership.where.code')} value={acting.shortCode} mono />
                            <Detail
                                label={t('membership.where.where')}
                                value={acting.businessCenterCode}
                            />
                            <Detail
                                label={t('membership.where.category')}
                                value={acting.partyCategory}
                            />
                        </dl>
                        <p className="mt-3 text-xs text-ink-faint">
                            {t('membership.where.actingNote')}
                        </p>
                    </>
                )}
            </section>

            <section className="space-y-3">
                <h2 className="text-sm font-semibold">{t('membership.where.parties')}</h2>
                {mine.length <= 1 && (
                    <Notice tone="info">{t('membership.where.oneParty')}</Notice>
                )}
                <div className="grid gap-4 md:grid-cols-2 xl:grid-cols-3">
                    {mine.map((party) => (
                        <PartyCard
                            key={party.partyId}
                            party={party}
                            acting={party.partyId === actingPartyId}
                            isDefault={party.partyId === storedDefaultId}
                            busy={busy}
                            onSwitch={() => switchTo.mutate(party.partyId)}
                            onSetDefault={() => setDefault.mutate(party.partyId)}
                            onClearDefault={() => setDefault.mutate('')}
                        />
                    ))}
                </div>
            </section>

            <p className="text-xs text-ink-faint">{t('membership.where.footnote')}</p>
        </div>
    );
}

/** One party, with the two actions a person takes on it. */
function PartyCard({
    party,
    acting,
    isDefault,
    busy,
    onSwitch,
    onSetDefault,
    onClearDefault,
}: {
    readonly party: MyParty;
    readonly acting: boolean;
    readonly isDefault: boolean;
    readonly busy: boolean;
    readonly onSwitch: () => void;
    readonly onSetDefault: () => void;
    readonly onClearDefault: () => void;
}): ReactNode {
    const { t } = useTranslation();
    /*
     * A party the server could not name arrives with empty strings rather than
     * being dropped. The card states that instead of drawing a nameless row
     * that reads as a defect in the person's own membership.
     */
    const named = party.name !== '';

    return (
        <section
            className={`flex flex-col gap-3 rounded-md border p-4 ${
                acting ? 'border-accent/60 bg-surface-raised' : 'border-line bg-surface-raised'
            }`}
        >
            <div className="flex items-start justify-between gap-3">
                <div className="min-w-0">
                    <div className="font-medium">
                        {named ? party.name : t('membership.where.unnamed')}
                    </div>
                    <div className="text-xs text-ink-muted">
                        {party.shortCode !== '' && (
                            <span className="font-mono">{party.shortCode}</span>
                        )}
                        {party.shortCode !== '' && party.businessCenterCode !== '' && ' · '}
                        {party.businessCenterCode}
                    </div>
                </div>
                <div className="flex shrink-0 flex-col items-end gap-1">
                    {acting && <Tag tone="accent">{t('membership.where.actingTag')}</Tag>}
                    {isDefault && <Tag tone="neutral">{t('membership.where.defaultTag')}</Tag>}
                </div>
            </div>
            <div className="mt-auto flex flex-wrap items-center gap-2 border-t border-line-subtle pt-3">
                {acting ? (
                    <span className="text-xs text-ink-faint">
                        {t('membership.where.thisIsIt')}
                    </span>
                ) : (
                    <Button variant="primary" size="sm" disabled={busy} onClick={onSwitch}>
                        {t('membership.where.switch')}
                    </Button>
                )}
                {!isDefault && (
                    <Button variant="ghost" size="sm" disabled={busy} onClick={onSetDefault}>
                        {t('membership.where.setDefault')}
                    </Button>
                )}
                {isDefault && (
                    <Button variant="ghost" size="sm" disabled={busy} onClick={onClearDefault}>
                        {t('membership.where.clearDefault')}
                    </Button>
                )}
            </div>
        </section>
    );
}

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
import type { AccountSignIns } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { ApiFailure } from '../api/transport.js';
import { AccountLockBadge } from '../ui/AccountLockBadge.js';
import { Notice, Tag } from '../ui/Primitives.js';
import { isZeroTimestamp } from '../ui/Time.js';
import { DEFAULT_PAGE_SIZE, Pager, pageBounds } from '../ui/Pager.js';

/**
 * One account's sign-ins, read a page at a time.
 *
 * The read is the caller's: the session's own tenant, or a tenant read from
 * system administration. A caller who may not read sessions is told so in
 * place of the panel, because the rest of the account's page still stands.
 */
export function SignInsPanel({
    queryKey,
    read,
    header,
}: {
    readonly queryKey: readonly unknown[];
    readonly read: (page: {
        readonly offset: number;
        readonly limit: number;
    }) => Promise<AccountSignIns>;
    /** Drawn above the panel from the account the read answered, for a page that has no other read. */
    readonly header?: (account: AccountSignIns['account']) => ReactNode;
}): ReactNode {
    const { t } = useTranslation();
    const [offset, setOffset] = useState(0);
    const signIns = useQuery({
        queryKey: [...queryKey, offset],
        queryFn: () => read({ offset, limit: DEFAULT_PAGE_SIZE }),
        placeholderData: keepPreviousData,
        retry: false,
    });

    if (signIns.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (signIns.isError) {
        const refused = signIns.error instanceof ApiFailure && signIns.error.status === 403;
        return (
            <Notice tone={refused ? 'info' : 'error'}>
                {refused ? t('signIns.notAllowed') : signIns.error.message}
            </Notice>
        );
    }
    return (
        <>
            {header?.(signIns.data.account)}
            <SignInsView signIns={signIns.data} offset={offset} onMove={setOffset} />
        </>
    );
}

function SignInsView({
    signIns,
    offset,
    onMove,
}: {
    readonly signIns: AccountSignIns;
    readonly offset: number;
    readonly onMove: (offset: number) => void;
}): ReactNode {
    const { t, plural } = useTranslation();
    const { account, loginInfo, sessions, totalCount } = signIns;
    const { first, last } = pageBounds(offset, sessions.length);
    const service = account.accountType !== 'user';

    return (
        <section className="space-y-4">
            <h2 className="text-sm font-semibold">{t('signIns.title')}</h2>
            <dl className="grid gap-4 rounded-md border border-line bg-surface-raised p-4 text-sm sm:grid-cols-2 lg:grid-cols-4">
                <div>
                    <dt className="text-xs text-ink-faint">{t('signIns.kind')}</dt>
                    <dd className="mt-0.5">
                        <Tag tone={service ? 'accent' : 'neutral'}>
                            {service ? t('signIns.service') : t('signIns.person')}
                        </Tag>{' '}
                        <span className="font-mono text-xs text-ink-faint">
                            {account.accountType}
                        </span>
                    </dd>
                </div>
                <div>
                    <dt className="text-xs text-ink-faint">{t('signIns.lastSignIn')}</dt>
                    <dd className="mt-0.5">
                        {loginInfo === null || isZeroTimestamp(loginInfo.lastLogin)
                            ? t('signIns.never')
                            : loginInfo.lastLogin}
                    </dd>
                </div>
                <div>
                    <dt className="text-xs text-ink-faint">{t('signIns.failed')}</dt>
                    <dd className="mt-0.5 tabular-nums">{loginInfo?.failedLogins ?? 0}</dd>
                </div>
                <div>
                    <dt className="text-xs text-ink-faint">{t('signIns.state')}</dt>
                    <dd className="mt-0.5 flex flex-wrap gap-1">
                        <AccountLockBadge locked={loginInfo?.locked === true} />
                        {loginInfo?.passwordResetRequired === true && (
                            <Tag tone="warn">{t('signIns.passwordDue')}</Tag>
                        )}
                    </dd>
                </div>
            </dl>

            {totalCount === 0 ? (
                <p className="text-sm text-ink-muted">{t('signIns.noSessions')}</p>
            ) : (
                <div>
                    <div className="overflow-x-auto rounded-md border border-line">
                        <table className="w-full text-left text-sm">
                            <thead>
                                <tr className="border-b border-line text-xs text-ink-muted">
                                    <th className="px-4 py-2 font-medium">
                                        {t('signIns.started')}
                                    </th>
                                    <th className="px-4 py-2 font-medium">{t('signIns.ended')}</th>
                                    <th className="px-4 py-2 font-medium">{t('signIns.client')}</th>
                                    <th className="px-4 py-2 font-medium">
                                        {t('signIns.address')}
                                    </th>
                                    <th className="px-4 py-2 font-medium">
                                        {t('signIns.country')}
                                    </th>
                                    <th className="px-4 py-2 text-right font-medium">
                                        {t('signIns.traffic')}
                                    </th>
                                </tr>
                            </thead>
                            <tbody>
                                {sessions.map((row) => (
                                    <tr
                                        key={row.id}
                                        className="border-b border-line-subtle last:border-b-0"
                                    >
                                        <td className="whitespace-nowrap px-4 py-2">
                                            {isZeroTimestamp(row.startTime) ? '—' : row.startTime}
                                        </td>
                                        <td className="whitespace-nowrap px-4 py-2 text-ink-muted">
                                            {isZeroTimestamp(row.endTime)
                                                ? t('signIns.noEnd')
                                                : row.endTime}
                                        </td>
                                        <td className="px-4 py-2 font-mono text-xs">
                                            {row.clientIdentifier === ''
                                                ? '—'
                                                : row.clientIdentifier}
                                        </td>
                                        <td className="px-4 py-2 font-mono text-xs">
                                            {row.clientIp === '' ? '—' : row.clientIp}
                                        </td>
                                        <td className="px-4 py-2 text-ink-muted">
                                            {row.countryCode === '' ? '—' : row.countryCode}
                                        </td>
                                        <td className="whitespace-nowrap px-4 py-2 text-right text-xs text-ink-faint">
                                            {String(row.bytesSent)} / {String(row.bytesReceived)}
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </div>
                    <Pager
                        offset={offset}
                        shown={sessions.length}
                        total={totalCount}
                        pageSize={DEFAULT_PAGE_SIZE}
                        showing={plural('signIns.showing', totalCount, { first, last })}
                        onMove={onMove}
                    />
                </div>
            )}
        </section>
    );
}

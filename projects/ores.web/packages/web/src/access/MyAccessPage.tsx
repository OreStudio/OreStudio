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
import { useMemo, useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { formatDateTime } from '../ui/Time.js';
import { api } from '../api/client.js';
import { AccountPicture } from '../ui/Images.js';
import { Button, Input, Notice, PageHeader } from '../ui/Primitives.js';
import { areasOf, countCovered, grantedBy, rolesGranting, search } from './catalogue.js';
import { PermissionAreas } from './PermissionAreas.js';
import { roleLabel } from './words.js';
import { AskForRoleDialog } from '../inbox/AskForRoleDialog.js';
import { MyRequests } from '../inbox/MyRequests.js';

/** How many answers "Can I…?" shows at once. */
const ANSWERS = 8;

/**
 * What the signed-in person may do, and which role lets them.
 *
 * The roles come first, with who gave each one and why, because that is the
 * part a person acts on: they know whom to ask. "Can I…?" answers the question
 * a person actually has, and names the role behind a yes. The full picture
 * follows by area; a role that grants everything says so instead of ticking
 * every box.
 */
export function MyAccessPage({ tenantName }: { readonly tenantName: string }): ReactNode {
    const { t, language } = useTranslation();
    const [question, setQuestion] = useState('');
    const [asking, setAsking] = useState(false);
    const access = useQuery({ queryKey: ['my-access'], queryFn: api.myAccess });
    const catalogue = useQuery({ queryKey: ['permissions'], queryFn: api.permissions });
    const areas = useMemo(() => areasOf(catalogue.data ?? []), [catalogue.data]);

    if (access.isPending || catalogue.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (access.isError) {
        return <Notice tone="error">{access.error.message}</Notice>;
    }
    if (catalogue.isError) {
        return <Notice tone="error">{catalogue.error.message}</Notice>;
    }
    const roles = access.data.roles;
    const granted = grantedBy(roles);
    const everything = roles.find((role) => role.permissionCodes.includes('*'));
    const answers = search(catalogue.data, question, ANSWERS);

    return (
        <div className="space-y-6">
            <PageHeader
                title={t('access.mine.title')}
                description={t('access.mine.lead', { tenant: tenantName })}
            />

            <section className="rounded-md border border-line bg-surface-raised">
                <div className="flex items-center justify-between gap-4 px-4 pt-4">
                    <h2 className="text-sm font-semibold">{t('access.mine.roles')}</h2>
                    <Button variant="primary" size="sm" onClick={() => setAsking(true)}>
                        {t('inbox.ask.open')}
                    </Button>
                </div>
                {roles.length === 0 ? (
                    <p className="px-4 py-4 text-sm text-ink-muted">{t('access.mine.noRoles')}</p>
                ) : (
                    <ul>
                        {roles.map((role) => (
                            <li
                                key={role.roleId}
                                className="border-t border-line-subtle px-4 py-3 first:border-t-0"
                            >
                                <div className="text-sm">
                                    <span className="font-medium">{roleLabel(t, role.name)}</span>{' '}
                                    <span className="text-ink-muted">· {role.description}</span>
                                </div>
                                <div className="mt-1 flex items-center gap-1.5 text-xs text-ink-faint">
                                    {t('access.givenBy')}
                                    <AccountPicture
                                        username={role.givenBy}
                                        name={role.givenBy}
                                        size="sm"
                                    />
                                    {role.givenBy}
                                    {role.givenAt !== '' &&
                                        ` · ${formatDateTime(role.givenAt, language)}`}
                                    {role.commentary !== '' && ` · ${role.commentary}`}
                                </div>
                            </li>
                        ))}
                    </ul>
                )}
            </section>

            <MyRequests />

            <section className="space-y-3 rounded-md border border-line bg-surface-raised p-4">
                <h2 className="text-sm font-semibold">{t('access.mine.canI')}</h2>
                <Input
                    type="search"
                    value={question}
                    onChange={(event) => setQuestion(event.target.value)}
                    placeholder={t('access.mine.canIHint')}
                    aria-label={t('access.mine.canI')}
                />
                {question.trim() !== '' && answers.length === 0 && (
                    <p className="text-sm text-ink-muted">{t('access.nothingMatches')}</p>
                )}
                <ul>
                    {answers.map((entry) => {
                        const by = rolesGranting(roles, entry.code);
                        return (
                            <li
                                key={entry.code}
                                className="flex items-start gap-3 border-t border-line-subtle py-2 first:border-t-0"
                            >
                                <span
                                    className={`mt-1.5 h-2 w-2 shrink-0 rounded-full ${by.length > 0 ? 'bg-up' : 'bg-ink-faint'}`}
                                />
                                <div className="min-w-0 flex-1">
                                    <div className="text-sm">{entry.description}</div>
                                    <div className="font-mono text-xs text-ink-faint">
                                        {entry.code}
                                    </div>
                                </div>
                                <div className="text-sm text-ink-muted">
                                    {by.length > 0
                                        ? t('access.mine.yesThrough', {
                                              roles: by
                                                  .map((name) => roleLabel(t, name))
                                                  .join(', '),
                                          })
                                        : t('access.mine.no')}
                                </div>
                            </li>
                        );
                    })}
                </ul>
            </section>

            <section className="space-y-3">
                <div>
                    <h2 className="text-sm font-semibold">{t('access.mine.whatTheyAllow')}</h2>
                    <p className="text-sm text-ink-muted">
                        {everything !== undefined
                            ? t('access.everythingBy', { role: roleLabel(t, everything.name) })
                            : t('access.coveredCount', {
                                  held: String(countCovered(granted, catalogue.data)),
                                  count: String(areas.reduce((sum, area) => sum + area.size, 0)),
                              })}
                    </p>
                </div>
                {everything === undefined && (
                    <PermissionAreas
                        areas={areas}
                        granted={granted}
                        onlyGranted
                        explain={(code) =>
                            rolesGranting(roles, code)
                                .map((name) => roleLabel(t, name))
                                .join(', ')
                        }
                    />
                )}
            </section>

            {asking && <AskForRoleDialog onClose={() => setAsking(false)} />}
        </div>
    );
}

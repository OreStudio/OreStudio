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
import { useMemo, useState, type ReactNode } from 'react';
import { useParams } from 'react-router';
import type { Account, HeldRole, TimelineEvent } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { formatDateTime } from '../ui/Time.js';
import { api } from '../api/client.js';
import { AccountPicture, Avatar, imageUrl } from '../ui/Images.js';
import { Button, Dialog, Field, Input, Notice, Select } from '../ui/Primitives.js';
import { areasOf, grantedBy, rolesGranting } from './catalogue.js';
import { PermissionAreas } from './PermissionAreas.js';
import { SignInsPanel } from './SignIns.js';
import { displayName } from './names.js';
import { roleLabel } from './words.js';
import { ContactTab, IdentityTab, useAccountWrites } from './PersonForms.js';
import { ACCOUNT_ENTITY, PersonRevertDialog, isRevertable, latestVersion } from './PersonRevert.js';
import { Timeline } from '../timeline/Timeline.js';
import { useHolds } from './holds.js';
import { RecordHeader } from '../refdata/records.js';
import { useTabs } from '../ui/Tabs.js';
import { AccountDoors } from '../pages/AccountDoors.js';

/**
 * One person's access: the roles they hold, who gave each one and why, and
 * what those roles let them do.
 *
 * Giving a role asks for a reason, which the grant records. Taking one away
 * closes the grant, so who held it and when stays on record; the server keeps
 * no reason for that, so none is asked. Nobody takes a role away from
 * themselves, and the screen says so before the server would.
 */
export function PersonPage({ me }: { readonly me: string }): ReactNode {
    const { t } = useTranslation();
    const { username = '' } = useParams();
    const account = useQuery({
        queryKey: ['account', username],
        queryFn: () => api.account(username),
        // The page states a refused read itself, so the banner stays quiet.
        meta: { quiet: true },
    });

    if (account.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (account.isError) {
        return <Notice tone="error">{account.error.message}</Notice>;
    }
    if (account.data === null) {
        return <Notice tone="warn">{t('access.person.notFound')}</Notice>;
    }
    return <Person account={account.data} self={account.data.username === me} />;
}

function Person({
    account,
    self,
}: {
    readonly account: Account;
    readonly self: boolean;
}): ReactNode {
    const { t, language } = useTranslation();
    const queries = useQueryClient();
    const [giving, setGiving] = useState(false);
    const [taking, setTaking] = useState<HeldRole | null>(null);
    const [reverting, setReverting] = useState<TimelineEvent | null>(null);
    const writes = useAccountWrites();
    /*
     * Each tab reads something of its own, and each of those reads is its own
     * permission. A member may open a colleague's page to see who they are,
     * which is the account read, and may not read their grants, their address
     * or their sign-ins. A tab the caller cannot read is not offered: showing a
     * tab and then answering it with a refusal teaches them only that something
     * is broken.
     */
    const holds = useHolds();
    const mayReadRoles = holds('iam::roles:read');
    const mayReadContact = holds('iam::account_contact_informations:read');
    const mayReadSignIns = holds('iam::sessions:read');
    /*
     * Your own contact record is read and written through the session, not
     * through your account id, so it needs no permission at all: a person who
     * may read no colleague's address still owns their own, and an empty record
     * is a form waiting to be filled rather than an absence.
     */
    const maySeeContact = mayReadContact || self;
    const access = useQuery({
        queryKey: ['account-access', account.id],
        queryFn: () => api.accountAccess(account.id),
        enabled: mayReadRoles,
    });
    const catalogue = useQuery({ queryKey: ['permissions'], queryFn: api.permissions });
    const story = useQuery({
        queryKey: ['timeline', 'person', account.username],
        queryFn: () => api.timeline('person', account.username),
    });
    const areas = useMemo(() => areasOf(catalogue.data ?? []), [catalogue.data]);
    const name = displayName(account, account.username);
    const refresh = () => {
        void queries.invalidateQueries({ queryKey: ['account-access', account.id] });
        /*
         * A grant taken away is read back as a closed one, which the stream
         * does not carry, so the story is read again rather than patched.
         */
        void queries.invalidateQueries({ queryKey: ['timeline', 'person', account.username] });
    };

    const { tab, bar } = useTabs({
        label: name,
        tabs: [
            'details',
            ...(maySeeContact ? ['contact'] : []),
            ...(mayReadRoles ? ['roles'] : []),
            ...(mayReadSignIns ? ['signIns'] : []),
            'timeline',
        ],
        titleOf: (part) => t(`access.person.tabs.${part}`),
    });
    const roles = access.data?.roles ?? [];
    const everything = roles.find((role) => role.permissionCodes.includes('*'));

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    { label: t('shell.menu.home'), to: '/' },
                    { label: t('access.hub.title'), to: '/organisation' },
                    { label: t('access.people.title'), to: '/staff' },
                    { label: name },
                ]}
                title={name}
                recordKey={account.id}
                version={null}
                subdued
                access={null}
                mark={
                    <Avatar
                        name={name}
                        size="lg"
                        src={account.imageId === null ? null : imageUrl(account.imageId)}
                    />
                }
            />
            {bar}
            {tab === 'details' && (
                <IdentityTab username={account.username} me={self} fallbackEmail={account.email} />
            )}
            {tab === 'contact' && (
                <ContactTab username={account.username} me={self} fallbackEmail={account.email} />
            )}
            {tab === 'signIns' && (
                <SignInsPanel
                    queryKey={['account-sign-ins', account.username]}
                    read={(page) => api.accountSignIns(account.username, page)}
                />
            )}
            {tab === 'timeline' && (
                <>
                    {story.isPending && (
                        <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                    )}
                    {story.isError && <Notice tone="error">{story.error.message}</Notice>}
                    {story.data !== undefined && (
                        <Timeline
                            timeline={story.data}
                            /*
                             * Two entries can be acted on. A grant is taken
                             * away, which closes it and is the write the roles
                             * tab already makes. A version of the account or of
                             * its contact record is written back as a new
                             * version, through the same writes as an edit. The
                             * newest version is the record as it stands, so it
                             * says so rather than offering to revert to itself.
                             */
                            renderActions={(event) => {
                                if (event.kind === 'granted') {
                                    const held = roles.find(
                                        (role) => role.roleId === event.entityId,
                                    );
                                    if (held === undefined) return undefined;
                                    return (
                                        <div className="mt-2 flex justify-end">
                                            <Button size="sm" onClick={() => setTaking(held)}>
                                                {t('access.person.takeAway')}
                                            </Button>
                                        </div>
                                    );
                                }
                                if (!isRevertable(event)) return undefined;
                                const mayWrite =
                                    self ||
                                    (event.entityType === ACCOUNT_ENTITY
                                        ? writes.accounts
                                        : writes.contacts);
                                if (!mayWrite) return undefined;
                                const latest = latestVersion(
                                    story.data?.events ?? [],
                                    event.entityType,
                                );
                                if (event.version >= latest) {
                                    return (
                                        <p className="text-[0.78rem] text-ink-faint">
                                            {t('access.person.currentVersion')}
                                        </p>
                                    );
                                }
                                return (
                                    <div className="mt-2 flex justify-end">
                                        <Button
                                            size="sm"
                                            icon="revert"
                                            onClick={() => setReverting(event)}
                                        >
                                            {t('history.revert')}
                                        </Button>
                                    </div>
                                );
                            }}
                        />
                    )}
                </>
            )}
            {tab === 'roles' && (
                <div className="space-y-4">
                    <div className="flex justify-end">
                        <Button variant="primary" icon="add" onClick={() => setGiving(true)}>
                            {t('access.person.give')}
                        </Button>
                    </div>
                    {access.isError && <Notice tone="error">{access.error.message}</Notice>}
                    <section className="overflow-x-auto rounded-md border border-line">
                        <table className="w-full text-left text-sm">
                            <thead>
                                <tr className="border-b border-line text-xs text-ink-muted">
                                    <th className="px-4 py-2 font-medium">
                                        {t('access.person.role')}
                                    </th>
                                    <th className="px-4 py-2 font-medium">{t('access.givenBy')}</th>
                                    <th className="px-4 py-2 font-medium">
                                        {t('access.person.on')}
                                    </th>
                                    <th className="px-4 py-2 font-medium">
                                        {t('access.person.why')}
                                    </th>
                                    <th className="px-4 py-2" />
                                </tr>
                            </thead>
                            <tbody>
                                {roles.length === 0 && (
                                    <tr>
                                        <td colSpan={5} className="px-4 py-3 text-ink-muted">
                                            {access.isPending
                                                ? t('common.loading')
                                                : t('access.person.noRole')}
                                        </td>
                                    </tr>
                                )}
                                {roles.map((role) => (
                                    <tr
                                        key={role.roleId}
                                        className="border-b border-line-subtle last:border-b-0"
                                    >
                                        <td className="px-4 py-2">
                                            <span className="block font-medium">
                                                {roleLabel(t, role.name)}
                                            </span>
                                            <span className="block text-xs text-ink-faint">
                                                {role.description}
                                            </span>
                                        </td>
                                        <td className="px-4 py-2">
                                            <span className="flex items-center gap-2">
                                                <AccountPicture
                                                    username={role.givenBy}
                                                    name={role.givenBy}
                                                    size="sm"
                                                />
                                                {role.givenBy}
                                            </span>
                                        </td>
                                        <td className="px-4 py-2 text-ink-muted">
                                            {formatDateTime(role.givenAt, language)}
                                        </td>
                                        <td className="px-4 py-2">
                                            <span className="block">{role.reasonCode}</span>
                                            {role.commentary !== '' && (
                                                <span className="block text-xs text-ink-faint">
                                                    {role.commentary}
                                                </span>
                                            )}
                                        </td>
                                        <td className="px-4 py-2 text-right">
                                            <Button
                                                variant="danger"
                                                size="sm"
                                                disabled={self}
                                                title={
                                                    self
                                                        ? t('access.person.notYourself')
                                                        : undefined
                                                }
                                                onClick={() => setTaking(role)}
                                            >
                                                {t('access.person.takeAway')}
                                            </Button>
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </section>
                    <section className="space-y-3">
                        <h2 className="text-sm font-semibold">
                            {t('access.person.whatTheyAllow')}
                        </h2>
                        {everything !== undefined ? (
                            <p className="text-sm text-ink-muted">
                                {t('access.everythingBy', { role: roleLabel(t, everything.name) })}
                            </p>
                        ) : (
                            <PermissionAreas
                                areas={areas}
                                granted={grantedBy(roles)}
                                onlyGranted
                                explain={(code) =>
                                    rolesGranting(roles, code)
                                        .map((roleName) => roleLabel(t, roleName))
                                        .join(', ')
                                }
                            />
                        )}
                    </section>
                </div>
            )}

            {self && <AccountDoors />}

            {giving && (
                <GiveRoleDialog
                    account={account}
                    name={name}
                    held={roles}
                    onClose={() => setGiving(false)}
                    onDone={() => {
                        setGiving(false);
                        void refresh();
                    }}
                />
            )}
            {reverting !== null && (
                <PersonRevertDialog
                    event={reverting}
                    current={latestVersion(story.data?.events ?? [], reverting.entityType)}
                    username={account.username}
                    accountId={account.id}
                    me={self}
                    onClose={() => setReverting(null)}
                />
            )}
            {taking !== null && (
                <TakeAwayDialog
                    account={account}
                    name={name}
                    role={taking}
                    onClose={() => setTaking(null)}
                    onDone={() => {
                        setTaking(null);
                        void refresh();
                    }}
                />
            )}
        </div>
    );
}

function GiveRoleDialog({
    account,
    name,
    held,
    onClose,
    onDone,
}: {
    readonly account: Account;
    readonly name: string;
    readonly held: readonly HeldRole[];
    readonly onClose: () => void;
    readonly onDone: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const roles = useQuery({ queryKey: ['roles'], queryFn: api.roles });
    const reasons = useQuery({ queryKey: ['access-reasons'], queryFn: api.accessReasons });
    const [roleId, setRoleId] = useState('');
    const [reasonCode, setReasonCode] = useState('');
    const [note, setNote] = useState('');
    const give = useMutation({
        mutationFn: () => api.giveRole(account.id, { roleId, reasonCode, note }),
        onSuccess: onDone,
    });
    const offered = (roles.data ?? []).filter((role) => !role.service);
    const holds = new Set(held.map((role) => role.roleId));

    return (
        <Dialog
            title={t('access.person.giveTitle', { name })}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('entity.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        disabled={roleId === '' || reasonCode === '' || give.isPending}
                        onClick={() => give.mutate()}
                    >
                        {t('access.person.give')}
                    </Button>
                </>
            }
        >
            <div className="space-y-4">
                <div className="max-h-72 space-y-2 overflow-y-auto">
                    {offered.map((role) => (
                        <label
                            key={role.id}
                            className={`flex cursor-pointer gap-3 rounded-md border border-line p-2.5 text-sm ${holds.has(role.id) ? 'opacity-50' : 'hover:border-line-strong'}`}
                        >
                            <input
                                type="radio"
                                name="role"
                                value={role.id}
                                disabled={holds.has(role.id)}
                                checked={roleId === role.id}
                                onChange={() => setRoleId(role.id)}
                            />
                            <span>
                                <span className="block font-medium">
                                    {roleLabel(t, role.name)}
                                    {holds.has(role.id) && (
                                        <span className="font-normal text-ink-faint">
                                            {' '}
                                            · {t('access.person.alreadyHeld')}
                                        </span>
                                    )}
                                </span>
                                <span className="block text-ink-muted">{role.description}</span>
                                <span className="block text-xs text-ink-faint">
                                    {role.permissionCodes.includes('*')
                                        ? t('access.lets.everything')
                                        : t('access.lets.count', {
                                              count: String(role.permissionCodes.length),
                                          })}
                                </span>
                            </span>
                        </label>
                    ))}
                </div>
                <div className="grid gap-4 sm:grid-cols-2">
                    <Field label={t('access.person.why')}>
                        <Select
                            value={reasonCode}
                            onChange={(event) => setReasonCode(event.target.value)}
                        >
                            <option value="">{t('access.person.chooseReason')}</option>
                            {(reasons.data ?? []).map((reason) => (
                                <option key={reason.code} value={reason.code}>
                                    {reason.description}
                                </option>
                            ))}
                        </Select>
                    </Field>
                    <Field label={t('access.person.note')}>
                        <Input value={note} onChange={(event) => setNote(event.target.value)} />
                    </Field>
                </div>
                {give.isError && <Notice tone="error">{give.error.message}</Notice>}
            </div>
        </Dialog>
    );
}

function TakeAwayDialog({
    account,
    name,
    role,
    onClose,
    onDone,
}: {
    readonly account: Account;
    readonly name: string;
    readonly role: HeldRole;
    readonly onClose: () => void;
    readonly onDone: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const take = useMutation({
        mutationFn: () => api.takeRoleAway(account.id, role.roleId),
        onSuccess: onDone,
    });
    return (
        <Dialog
            title={t('access.person.takeTitle', { role: roleLabel(t, role.name), name })}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('entity.cancel')}
                    </Button>
                    <Button
                        variant="danger"
                        disabled={take.isPending}
                        onClick={() => take.mutate()}
                    >
                        {t('access.person.takeAway')}
                    </Button>
                </>
            }
        >
            <p className="text-sm text-ink-muted">{t('access.person.takeKept')}</p>
            {take.isError && <Notice tone="error">{take.error.message}</Notice>}
        </Dialog>
    );
}

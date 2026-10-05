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
import { useState, type ChangeEvent, type ReactNode } from 'react';
import { Link } from 'react-router';
import type {
    Account,
    AccountContactInformation,
    HeldRole,
    ImageUploadPolicy,
    SessionView,
} from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { Avatar, imageUrl } from '../ui/Images.js';
import { Button, Detail, Field, Input, Notice, PageHeader, Select } from '../ui/Primitives.js';
import { roleLabel } from '../access/words.js';

/**
 * The profile screen: the three profile journeys as one page with panels.
 *
 * The journeys are `doc/knowledge/journeys/profile/journey_present_myself.org`,
 * `journey_keep_details_current.org` and `journey_change_someones_details.org`,
 * and this is the accepted prototype's variant B: each writable panel carries
 * its own save and its own reason, so a half-finished edit never rides along
 * with the record that is ready. No route sends a pair of writes, and each
 * panel's outcome names only its own record.
 *
 * The person search appears for a holder of a write permission, and each
 * panel's save appears for the permission its operation asks for. The server
 * still refuses a call the permission does not cover, and the panel shows the
 * words it answers with.
 *
 * Two things the platform cannot do are stated rather than hidden. The account
 * read needs `iam::accounts:read`, which a plain member may not hold, so the
 * identity panel offers no fields and no save rather than sending a blind
 * write that would clear them. And nothing holds a reporting-line approval, so
 * the proposal is drawn as a statement beside the line it would replace.
 */

/** One reason a record may be amended for, as the catalogue states it. */
interface AmendReason {
    readonly code: string;
    readonly description: string;
    readonly requiresCommentary: boolean;
}

/** What a panel's last save decided, as the panel draws it. */
interface PanelOutcome {
    readonly ok: boolean;
    readonly message: string;
    readonly fields: readonly {
        readonly field: string;
        readonly code: string;
        readonly message: string;
    }[];
}

/** The wire's field name as the panel's own, so a failure lands under its input. */
function camel(name: string): string {
    return name.replace(/_(\w)/g, (_match, letter: string) => letter.toUpperCase());
}

/** The error prop a field takes, present only when the server named it. */
interface FieldIssue {
    readonly error?: string;
}

/** The failure the server reported for one field, for the input it names. */
function fieldIssue(outcome: PanelOutcome | undefined, name: string): FieldIssue {
    const message = outcome?.fields.find((failure) => camel(failure.field) === name)?.message;
    return message === undefined || message === '' ? {} : { error: message };
}

export function ProfilePage({ session }: { readonly session: SessionView }): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [picked, setPicked] = useState<string | null>(null);
    const me = picked === null || picked === session.username;
    const shownUsername = me ? session.username : (picked ?? session.username);

    const access = useQuery({ queryKey: ['my-access'], queryFn: api.myAccess });
    const granted = access.data?.roles.flatMap((role) => role.permissionCodes) ?? [];
    const everything = granted.includes('*');
    const mayWriteAccounts = everything || granted.includes('iam::accounts:update');
    const mayWriteContacts =
        everything || granted.includes('iam::account_contact_informations:write');
    const mayAdminister = mayWriteAccounts || mayWriteContacts;

    const search = useQuery({
        queryKey: ['accounts'],
        queryFn: api.accounts,
        enabled: mayAdminister,
    });
    const account = useQuery({
        queryKey: ['account', shownUsername],
        queryFn: () => api.account(shownUsername),
    });
    const shownAccountId = account.data?.id ?? '';
    const contactKey = me ? 'me' : shownAccountId;
    const contact = useQuery({
        queryKey: ['contact-information', contactKey],
        queryFn: () =>
            me ? api.myContactInformation() : api.accountContactInformation(shownAccountId),
        enabled: contactKey !== '',
    });
    const reasons = useQuery({ queryKey: ['amend-reasons'], queryFn: api.amendReasons });

    if (access.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }

    const signInEmail = account.data?.email ?? session.email;
    const managerName = resolveManager(
        search.data?.accounts ?? [],
        account.data?.reportsToAccountId ?? null,
    );

    return (
        <div className="space-y-6">
            <PageHeader
                title={me ? t('profile.title.mine') : t('profile.title.other')}
                description={
                    me
                        ? t('profile.lead.mine')
                        : t('profile.lead.other', {
                              signedIn: session.username,
                              person: shownUsername,
                          })
                }
            />
            {access.isError && (
                <Notice tone="warn">
                    {t('profile.access.failed', { reason: access.error.message })}
                </Notice>
            )}
            {mayAdminister && (
                <SearchPanel
                    accounts={search.data?.accounts ?? []}
                    pending={search.isPending}
                    failed={search.isError ? search.error.message : null}
                    picked={picked}
                    onPick={setPicked}
                />
            )}
            <IdentityPanel
                account={account.data ?? null}
                pending={account.isPending}
                refused={account.isError ? account.error.message : null}
                signInEmail={signInEmail}
                managerName={managerName}
                canWrite={me || mayWriteAccounts}
                me={me}
                username={shownUsername}
                reasons={reasons.data ?? []}
                onSaved={() => queries.invalidateQueries({ queryKey: ['account', shownUsername] })}
            />
            <ContactPanel
                contact={contact.data ?? null}
                pending={contact.isPending || account.isPending}
                refused={contact.isError ? contact.error.message : null}
                accountRead={account.data !== null && account.data !== undefined}
                canWrite={me || mayWriteContacts}
                me={me}
                accountId={shownAccountId}
                signInEmail={signInEmail}
                reasons={reasons.data ?? []}
                onSaved={() => queries.invalidateQueries({ queryKey: ['contact-information', contactKey] })}
            />
            {me && <AccessPanel roles={access.data?.roles ?? []} />}
        </div>
    );
}

/** The manager's name, when the tenant list can name the recorded identifier. */
function resolveManager(accounts: readonly Account[], reportsTo: string | null): string | null {
    if (reportsTo === null) return null;
    const manager = accounts.find((row) => row.id === reportsTo);
    if (manager === undefined) return reportsTo;
    return manager.fullName === '' ? manager.username : manager.fullName;
}

/**
 * The person search, which is the administrator's half of the journey.
 *
 * The list is the tenant's accounts, filtered here by username and name
 * because plan item 1 names it as the read the directory already makes. The
 * rows are the accounts themselves, so a person is picked from what the
 * server holds rather than typed in.
 */
function SearchPanel({
    accounts,
    pending,
    failed,
    picked,
    onPick,
}: {
    readonly accounts: readonly Account[];
    readonly pending: boolean;
    readonly failed: string | null;
    readonly picked: string | null;
    readonly onPick: (username: string | null) => void;
}): ReactNode {
    const { t } = useTranslation();
    const [text, setText] = useState('');
    const needle = text.trim().toLowerCase();
    const matches = accounts.filter(
        (row) =>
            needle === '' ||
            row.username.toLowerCase().includes(needle) ||
            row.fullName.toLowerCase().includes(needle),
    );
    const shown = matches.slice(0, 8);

    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('profile.search.title')}</h2>
                {picked !== null && (
                    <Button size="sm" variant="ghost" onClick={() => onPick(null)}>
                        {t('profile.search.mine')}
                    </Button>
                )}
            </header>
            <p className="text-sm text-ink-muted">{t('profile.search.lead')}</p>
            <Input
                type="search"
                value={text}
                onChange={(event) => setText(event.target.value)}
                placeholder={t('profile.search.placeholder')}
                aria-label={t('profile.search.title')}
            />
            {failed !== null && (
                <Notice tone="error">{t('profile.search.failed', { reason: failed })}</Notice>
            )}
            {pending ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : matches.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('profile.search.none')}</p>
            ) : (
                <ul>
                    {shown.map((row) => {
                        const name = row.fullName === '' ? row.username : row.fullName;
                        const current = row.username === picked;
                        return (
                            <li
                                key={row.id}
                                className="border-t border-line-subtle first:border-t-0"
                            >
                                <button
                                    type="button"
                                    aria-current={current ? 'true' : undefined}
                                    onClick={() => onPick(row.username)}
                                    className={`flex w-full items-center gap-3 px-1 py-2 text-left text-sm hover:bg-surface-hover ${current ? 'text-accent-bright' : 'text-ink'}`}
                                >
                                    <Avatar
                                        name={name}
                                        size="sm"
                                        src={row.imageId === null ? null : imageUrl(row.imageId)}
                                    />
                                    <span className="min-w-0">
                                        <span className="block truncate">{name}</span>
                                        <span className="block text-xs text-ink-faint">
                                            {row.username}
                                            {row.jobTitle !== '' && ` · ${row.jobTitle}`}
                                        </span>
                                    </span>
                                </button>
                            </li>
                        );
                    })}
                </ul>
            )}
            {matches.length > shown.length && (
                <p className="text-xs text-ink-faint">{t('profile.search.more')}</p>
            )}
        </section>
    );
}

/**
 * The identity panel: the photo, the read-only fields and the reporting line.
 *
 * A change to the reporting line is drawn as a proposal, and the control that
 * would send it is unavailable with its reason, because nothing in the
 * platform holds an approval today. The value does not move while the
 * proposal waits, and the panel says so rather than inventing a state.
 */
function IdentityPanel({
    account,
    pending,
    refused,
    signInEmail,
    managerName,
    canWrite,
    me,
    username,
    reasons,
    onSaved,
}: {
    readonly account: Account | null;
    readonly pending: boolean;
    readonly refused: string | null;
    readonly signInEmail: string;
    readonly managerName: string | null;
    readonly canWrite: boolean;
    readonly me: boolean;
    readonly username: string;
    readonly reasons: readonly AmendReason[];
    readonly onSaved: () => Promise<unknown>;
}): ReactNode {
    const { t } = useTranslation();
    const [edits, setEdits] = useState<Partial<{ fullName: string; jobTitle: string; imageId: string }>>(
        {},
    );
    const [reasonCode, setReasonCode] = useState('');
    const [commentary, setCommentary] = useState('');
    const [busy, setBusy] = useState(false);
    const [outcome, setOutcome] = useState<PanelOutcome | undefined>(undefined);

    if (pending) {
        return (
            <section className="card p-6">
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            </section>
        );
    }
    if (account === null) {
        return (
            <section className="card space-y-3 p-6">
                <h2 className="text-lg font-medium">{t('profile.identity.title')}</h2>
                {refused === null ? (
                    <Notice tone="warn">{t('profile.identity.missing')}</Notice>
                ) : (
                    <>
                        <Notice tone="error">
                            {t('profile.identity.readRefused', { reason: refused })}
                        </Notice>
                        <p className="text-sm text-ink-muted">{t('profile.identity.readGap')}</p>
                    </>
                )}
            </section>
        );
    }

    const draft = {
        fullName: edits.fullName ?? account.fullName,
        jobTitle: edits.jobTitle ?? account.jobTitle,
        imageId: edits.imageId ?? account.imageId ?? '',
    };
    const chosen = reasons.find((reason) => reason.code === reasonCode);
    const maySave =
        canWrite &&
        reasonCode !== '' &&
        !(chosen?.requiresCommentary === true && commentary.trim() === '');

    const save = async (): Promise<void> => {
        setBusy(true);
        setOutcome(undefined);
        try {
            if (me) {
                const view = await api.saveMyProfile({ ...draft, reasonCode, commentary });
                if (view.result.outcome === 'ok') {
                    setEdits({});
                    setOutcome({
                        ok: true,
                        message:
                            view.account === null
                                ? t('profile.saved.done')
                                : t('profile.saved.version', {
                                      version: String(view.account.version),
                                  }),
                        fields: [],
                    });
                    await onSaved();
                } else {
                    setOutcome({
                        ok: false,
                        message:
                            view.result.code === 'field_not_self_writable'
                                ? t('profile.refused.notYours')
                                : view.result.message,
                        fields: view.result.fields,
                    });
                }
            } else {
                await api.saveAccountProfile(username, { ...draft, reasonCode, commentary });
                setEdits({});
                setOutcome({ ok: true, message: t('profile.saved.done'), fields: [] });
                await onSaved();
            }
        } catch (error) {
            setOutcome({
                ok: false,
                message:
                    error instanceof Error ? error.message : t('profile.saved.failed'),
                fields: [],
            });
        } finally {
            setBusy(false);
        }
    };

    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">{t('profile.identity.title')}</h2>
                <p className="text-sm text-ink-muted">
                    {canWrite
                        ? me
                            ? t('profile.identity.whySelf')
                            : t('profile.identity.whyAdmin')
                        : t('profile.identity.whyReadOnly')}
                </p>
            </header>

            <div className="flex flex-wrap items-start gap-5">
                <div className="flex flex-col items-center gap-2">
                    <Avatar
                        name={draft.fullName === '' ? account.username : draft.fullName}
                        size="lg"
                        src={draft.imageId === '' ? null : imageUrl(draft.imageId)}
                    />
                    {draft.imageId === '' && (
                        <p className="text-xs text-ink-faint">{t('profile.identity.noPhoto')}</p>
                    )}
                    {canWrite && (
                        <PhotoPicker
                            name={draft.fullName === '' ? account.username : draft.fullName}
                            imageId={draft.imageId === '' ? null : draft.imageId}
                            label={
                                draft.imageId === ''
                                    ? t('profile.identity.choosePhoto')
                                    : t('profile.identity.replacePhoto')
                            }
                            onChoose={(imageId) => setEdits({ ...edits, imageId })}
                        />
                    )}
                </div>
                <div className="min-w-0 flex-1">
                    <div className="grid gap-x-6 gap-y-3 sm:grid-cols-2">
                        <div>
                            <Detail
                                label={t('profile.identity.username')}
                                value={account.username}
                            />
                            <p className="mt-1 text-xs text-ink-faint">
                                {t('profile.identity.usernameWhy')}
                            </p>
                        </div>
                        <div>
                            <Detail
                                label={t('profile.identity.signInAddress')}
                                value={signInEmail === '' ? t('account.notSet') : signInEmail}
                            />
                            <p className="mt-1 text-xs text-ink-faint">
                                {t('profile.identity.signInAddressWhy')}
                            </p>
                        </div>
                        <div>
                            <Detail
                                label={t('profile.identity.accountType')}
                                value={account.accountType}
                            />
                            <p className="mt-1 text-xs text-ink-faint">
                                {t('profile.identity.accountTypeWhy')}
                            </p>
                        </div>
                    </div>
                </div>
            </div>

            <div className="grid gap-4 sm:grid-cols-2">
                <Field label={t('profile.identity.fullName')} {...fieldIssue(outcome, 'fullName')}>
                    <Input
                        value={draft.fullName}
                        disabled={!canWrite}
                        onChange={(event) => setEdits({ ...edits, fullName: event.target.value })}
                    />
                </Field>
                <Field label={t('profile.identity.jobTitle')} {...fieldIssue(outcome, 'jobTitle')}>
                    <Input
                        value={draft.jobTitle}
                        disabled={!canWrite}
                        onChange={(event) => setEdits({ ...edits, jobTitle: event.target.value })}
                    />
                </Field>
            </div>

            <div className="space-y-2 border-t border-line-subtle pt-4">
                <h3 className="text-sm font-medium">{t('profile.identity.reporting')}</h3>
                <p className="text-sm">
                    {account.reportsToAccountId === null
                        ? t('profile.identity.noLine')
                        : t('profile.identity.reportsTo', { name: managerName ?? '' })}
                </p>
                {account.reportsToAccountId !== null &&
                    managerName === account.reportsToAccountId && (
                        <p className="text-xs text-ink-faint">
                            {t('profile.identity.reportsToUnknown')}
                        </p>
                    )}
                <div className="flex flex-wrap items-center gap-3">
                    <Button disabled title={t('profile.identity.proposeWhy')}>
                        {t('profile.identity.propose')}
                    </Button>
                    <span className="text-xs text-ink-muted">
                        {t('profile.identity.proposeApprovers')}
                    </span>
                </div>
                <p className="text-xs text-ink-faint">{t('profile.identity.proposeGap')}</p>
            </div>

            {canWrite && (
                <ReasonRow
                    reasons={reasons}
                    reasonCode={reasonCode}
                    commentary={commentary}
                    issue={fieldIssue(outcome, 'changeReasonCode')}
                    onReason={setReasonCode}
                    onCommentary={setCommentary}
                />
            )}
            {outcome !== undefined && (
                <Notice tone={outcome.ok ? 'success' : 'error'}>
                    {outcome.message}
                    {outcome.fields.length > 0 && (
                        <ul className="mt-1 list-disc pl-5">
                            {outcome.fields.map((failure) => (
                                <li key={`${failure.field}:${failure.code}`}>
                                    {failure.field}: {failure.message}
                                </li>
                            ))}
                        </ul>
                    )}
                </Notice>
            )}
            {canWrite && (
                <div className="flex items-center justify-end gap-3">
                    <span className="text-xs text-ink-faint">
                        {t('profile.version', { version: String(account.version) })}
                    </span>
                    <Button variant="primary" disabled={!maySave} pending={busy} onClick={() => void save()}>
                        {t('profile.save.identity')}
                    </Button>
                </div>
            )}
        </section>
    );
}

/**
 * The photo picker: an upload, a preview, and the choice deferred.
 *
 * The rule beside it is the server's own, read from the validator that
 * enforces it. The upload sets no photo: it answers an identifier, and the
 * person confirms before that identifier rides into the panel's save.
 */
function PhotoPicker({
    name,
    imageId,
    label,
    onChoose,
}: {
    readonly name: string;
    readonly imageId: string | null;
    readonly label: string;
    readonly onChoose: (imageId: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const [open, setOpen] = useState(false);
    const [uploaded, setUploaded] = useState<string | null>(null);
    const [uploading, setUploading] = useState(false);
    const [failure, setFailure] = useState<string | null>(null);
    const policy = useQuery({
        queryKey: ['image-upload-policy'],
        queryFn: api.imageUploadPolicy,
        enabled: open,
    });
    const chosen = uploaded ?? imageId;

    const upload = async (event: ChangeEvent<HTMLInputElement>): Promise<void> => {
        const file = event.target.files?.[0];
        if (file === undefined) return;
        setUploading(true);
        setFailure(null);
        try {
            const view = await api.uploadImage(file.type, await base64Of(file));
            if (view.result.outcome === 'ok' && view.imageId !== '') {
                setUploaded(view.imageId);
            } else {
                setFailure(view.result.message === '' ? view.result.code : view.result.message);
            }
        } catch (error) {
            setFailure(error instanceof Error ? error.message : '');
        } finally {
            setUploading(false);
        }
    };

    if (!open) {
        return (
            <Button size="sm" onClick={() => setOpen(true)}>
                {label}
            </Button>
        );
    }

    return (
        <div className="w-72 space-y-3 rounded-md border border-line p-3 text-left">
            <p className="text-sm">{t('profile.photo.pick')}</p>
            {policy.data !== undefined && (
                <p className="text-xs text-ink-faint">{ruleSentence(policy.data, t)}</p>
            )}
            {policy.isError && <Notice tone="error">{policy.error.message}</Notice>}
            <input
                type="file"
                disabled={uploading}
                accept={policy.data?.formats.join(',')}
                onChange={(event) => void upload(event)}
                className="block w-full text-xs text-ink-muted"
            />
            {uploading && <p className="text-xs text-ink-muted">{t('profile.photo.uploading')}</p>}
            {failure !== null && (
                <Notice tone="error">{t('profile.photo.failed', { message: failure })}</Notice>
            )}
            {chosen !== null && (
                <div className="flex items-center gap-2">
                    <Avatar name={name} src={imageUrl(chosen)} size="lg" />
                    <span className="text-xs text-ink-faint">{t('profile.photo.preview')}</span>
                </div>
            )}
            <div className="flex justify-end gap-2">
                <Button
                    size="sm"
                    variant="ghost"
                    onClick={() => {
                        setUploaded(null);
                        setFailure(null);
                        setOpen(false);
                    }}
                >
                    {t('profile.photo.cancel')}
                </Button>
                <Button
                    size="sm"
                    variant="primary"
                    disabled={chosen === null}
                    onClick={() => {
                        if (chosen !== null) onChoose(chosen);
                        setOpen(false);
                        setUploaded(null);
                        setFailure(null);
                    }}
                >
                    {t('profile.photo.use')}
                </Button>
            </div>
        </div>
    );
}

/** The bytes of a file as base64, which is how an upload travels. */
async function base64Of(file: File): Promise<string> {
    const bytes = new Uint8Array(await file.arrayBuffer());
    let binary = '';
    const CHUNK = 0x8000;
    for (let at = 0; at < bytes.length; at += CHUNK) {
        binary += String.fromCharCode(...bytes.subarray(at, at + CHUNK));
    }
    return btoa(binary);
}

/** The rule as the picker states it, from the validator's own numbers. */
function ruleSentence(
    policy: ImageUploadPolicy,
    t: (key: string, values?: Record<string, string>) => string,
): string {
    const formats = policy.formats
        .map((format) => format.replace('image/', '').toUpperCase())
        .join(', ');
    return t('profile.photo.rule', {
        formats,
        size: String(Math.round(policy.maxSizeBytes / (1024 * 1024))),
        width: String(policy.minWidth),
        height: String(policy.minHeight),
    });
}

/**
 * The contact panel: the telephone, the address and the contact address.
 *
 * The contact address is labelled apart from the sign-in address, because a
 * person expects one email field and the journey holds two. This is the
 * record the member owns outright, so the panel works for a member whose
 * account read is refused.
 */
function ContactPanel({
    contact,
    pending,
    refused,
    accountRead,
    canWrite,
    me,
    accountId,
    signInEmail,
    reasons,
    onSaved,
}: {
    readonly contact: AccountContactInformation | null;
    readonly pending: boolean;
    readonly refused: string | null;
    readonly accountRead: boolean;
    readonly canWrite: boolean;
    readonly me: boolean;
    readonly accountId: string;
    readonly signInEmail: string;
    readonly reasons: readonly AmendReason[];
    readonly onSaved: () => Promise<unknown>;
}): ReactNode {
    const { t } = useTranslation();
    const [edits, setEdits] = useState<Partial<ContactDraft>>({});
    const [reasonCode, setReasonCode] = useState('');
    const [commentary, setCommentary] = useState('');
    const [busy, setBusy] = useState(false);
    const [outcome, setOutcome] = useState<PanelOutcome | undefined>(undefined);

    if (pending) {
        return (
            <section className="card p-6">
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            </section>
        );
    }

    const base: ContactDraft = {
        streetLine1: contact?.streetLine1 ?? '',
        streetLine2: contact?.streetLine2 ?? '',
        city: contact?.city ?? '',
        state: contact?.state ?? '',
        countryCode: contact?.countryCode ?? '',
        postalCode: contact?.postalCode ?? '',
        phone: contact?.phone ?? '',
        email: contact?.email ?? '',
        webPage: contact?.webPage ?? '',
    };
    const draft: ContactDraft = {
        streetLine1: edits.streetLine1 ?? base.streetLine1,
        streetLine2: edits.streetLine2 ?? base.streetLine2,
        city: edits.city ?? base.city,
        state: edits.state ?? base.state,
        countryCode: edits.countryCode ?? base.countryCode,
        postalCode: edits.postalCode ?? base.postalCode,
        phone: edits.phone ?? base.phone,
        email: edits.email ?? base.email,
        webPage: edits.webPage ?? base.webPage,
    };
    const chosen = reasons.find((reason) => reason.code === reasonCode);
    const maySave =
        canWrite &&
        reasonCode !== '' &&
        !(chosen?.requiresCommentary === true && commentary.trim() === '');

    const save = async (): Promise<void> => {
        setBusy(true);
        setOutcome(undefined);
        try {
            const view = me
                ? await api.saveMyContactInformation({ ...draft, reasonCode, commentary })
                : await api.saveAccountContactInformation(accountId, {
                      ...draft,
                      version: contact?.version ?? null,
                      reasonCode,
                      commentary,
                  });
            if (view.result.outcome === 'ok') {
                setEdits({});
                setOutcome({
                    ok: true,
                    message:
                        view.contact === null
                            ? t('profile.saved.done')
                            : t('profile.saved.version', {
                                  version: String(view.contact.version),
                              }),
                    fields: [],
                });
                await onSaved();
            } else {
                setOutcome({
                    ok: false,
                    message: !me
                        ? `${view.result.message} ${t('profile.refused.recordChanged')}`
                        : view.result.message,
                    fields: view.result.fields,
                });
                if (!me) await onSaved();
            }
        } catch (error) {
            setOutcome({
                ok: false,
                message: error instanceof Error ? error.message : t('profile.saved.failed'),
                fields: [],
            });
        } finally {
            setBusy(false);
        }
    };

    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">{t('profile.contact.title')}</h2>
                <p className="text-sm text-ink-muted">
                    {canWrite
                        ? me
                            ? t('profile.contact.whySelf')
                            : t('profile.contact.whyAdmin')
                        : t('profile.contact.whyReadOnly')}
                </p>
            </header>

            {refused !== null && <Notice tone="error">{refused}</Notice>}
            {contact === null && refused === null && (
                <p className="text-sm text-ink-muted">{t('profile.contact.noRecord')}</p>
            )}

            <div className="grid gap-4 sm:grid-cols-2">
                <Field label={t('profile.contact.streetLine1')}>
                    <Input
                        value={draft.streetLine1}
                        disabled={!canWrite}
                        onChange={(event) =>
                            setEdits({ ...edits, streetLine1: event.target.value })
                        }
                    />
                </Field>
                <Field label={t('profile.contact.streetLine2')}>
                    <Input
                        value={draft.streetLine2}
                        disabled={!canWrite}
                        onChange={(event) =>
                            setEdits({ ...edits, streetLine2: event.target.value })
                        }
                    />
                </Field>
                <Field label={t('profile.contact.city')}>
                    <Input
                        value={draft.city}
                        disabled={!canWrite}
                        onChange={(event) => setEdits({ ...edits, city: event.target.value })}
                    />
                </Field>
                <Field label={t('profile.contact.state')}>
                    <Input
                        value={draft.state}
                        disabled={!canWrite}
                        onChange={(event) => setEdits({ ...edits, state: event.target.value })}
                    />
                </Field>
                <Field label={t('profile.contact.postalCode')}>
                    <Input
                        value={draft.postalCode}
                        disabled={!canWrite}
                        onChange={(event) =>
                            setEdits({ ...edits, postalCode: event.target.value })
                        }
                    />
                </Field>
                <Field
                    label={t('profile.contact.country')}
                    hint={t('profile.contact.countryHint')}
                    {...fieldIssue(outcome, 'countryCode')}
                >
                    <Input
                        value={draft.countryCode}
                        disabled={!canWrite}
                        onChange={(event) =>
                            setEdits({ ...edits, countryCode: event.target.value })
                        }
                    />
                </Field>
                <Field label={t('profile.contact.phone')} {...fieldIssue(outcome, 'phone')}>
                    <Input
                        value={draft.phone}
                        disabled={!canWrite}
                        onChange={(event) => setEdits({ ...edits, phone: event.target.value })}
                    />
                </Field>
                <Field
                    label={t('profile.contact.email')}
                    hint={
                        accountRead && signInEmail !== ''
                            ? t('profile.contact.emailHint', { email: signInEmail })
                            : t('profile.contact.emailHintUnknown')
                    }
                    {...fieldIssue(outcome, 'email')}
                >
                    <Input
                        value={draft.email}
                        disabled={!canWrite}
                        onChange={(event) => setEdits({ ...edits, email: event.target.value })}
                    />
                </Field>
                <Field label={t('profile.contact.webPage')}>
                    <Input
                        value={draft.webPage}
                        disabled={!canWrite}
                        onChange={(event) => setEdits({ ...edits, webPage: event.target.value })}
                    />
                </Field>
            </div>

            {canWrite && (
                <ReasonRow
                    reasons={reasons}
                    reasonCode={reasonCode}
                    commentary={commentary}
                    issue={fieldIssue(outcome, 'changeReasonCode')}
                    onReason={setReasonCode}
                    onCommentary={setCommentary}
                />
            )}
            {outcome !== undefined && (
                <Notice tone={outcome.ok ? 'success' : 'error'}>
                    {outcome.message}
                    {outcome.fields.length > 0 && (
                        <ul className="mt-1 list-disc pl-5">
                            {outcome.fields.map((failure) => (
                                <li key={`${failure.field}:${failure.code}`}>
                                    {failure.field}: {failure.message}
                                </li>
                            ))}
                        </ul>
                    )}
                </Notice>
            )}
            {canWrite && (
                <div className="flex items-center justify-end gap-3">
                    <span className="text-xs text-ink-faint">
                        {contact === null
                            ? t('profile.contact.noRecordShort')
                            : t('profile.version', { version: String(contact.version) })}
                    </span>
                    <Button variant="primary" disabled={!maySave} pending={busy} onClick={() => void save()}>
                        {t('profile.save.contact')}
                    </Button>
                </div>
            )}
        </section>
    );
}

interface ContactDraft {
    readonly streetLine1: string;
    readonly streetLine2: string;
    readonly city: string;
    readonly state: string;
    readonly countryCode: string;
    readonly postalCode: string;
    readonly phone: string;
    readonly email: string;
    readonly webPage: string;
}

/** The reason each writable panel records with its write. */
function ReasonRow({
    reasons,
    reasonCode,
    commentary,
    issue,
    onReason,
    onCommentary,
}: {
    readonly reasons: readonly AmendReason[];
    readonly reasonCode: string;
    readonly commentary: string;
    readonly issue: FieldIssue;
    readonly onReason: (code: string) => void;
    readonly onCommentary: (text: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const chosen = reasons.find((reason) => reason.code === reasonCode);
    return (
        <div className="grid gap-4 sm:grid-cols-2">
            <Field label={t('profile.save.why')} {...issue}>
                <Select value={reasonCode} onChange={(event) => onReason(event.target.value)}>
                    <option value="">{t('profile.save.chooseReason')}</option>
                    {reasons.map((reason) => (
                        <option key={reason.code} value={reason.code}>
                            {reason.description}
                        </option>
                    ))}
                </Select>
            </Field>
            <Field
                label={t('profile.save.commentary')}
                hint={
                    chosen?.requiresCommentary === true
                        ? t('profile.save.commentaryRequired')
                        : t('profile.save.commentaryHint')
                }
            >
                <Input value={commentary} onChange={(event) => onCommentary(event.target.value)} />
            </Field>
        </div>
    );
}

/**
 * The sign-in and access summary.
 *
 * Read-only, because the journeys that own it are somewhere else: the password
 * and the sign-ins are Protect my account and the roles are Know what I may
 * do. The panel names both rather than copying what they show.
 */
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

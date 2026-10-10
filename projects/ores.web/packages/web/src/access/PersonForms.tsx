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

import { AccessMark } from '../ui/AccessMark.js';
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
import { AccountPicture, Avatar, imageUrl } from '../ui/Images.js';
import { FlagOf, FlaggedCode } from '../images/flags.js';
import { managerCandidates } from '../membership/managers.js';
import { Button, Detail, Dialog, Field, Input, Notice, Select } from '../ui/Primitives.js';
import { displayName } from './names.js';
import { personPath } from './PeoplePage.js';
import { useHolds } from './holds.js';
/**
 * The forms of a person's account: who they are and how to reach them.
 *
 * My profile draws them for the signed-in person, and a person's page under
 * People draws them for someone else. Each panel carries its own save and its
 * own reason, so a half-finished edit never rides along with the record that
 * is ready, and each panel's outcome names only its own record. Each save
 * appears for the permission its operation asks for; the server still refuses
 * a call the permission does not cover, and the panel shows its words.
 *
 * Two things the platform cannot do are stated rather than hidden. The account
 * read needs =iam::accounts:read=, which a plain member may not hold, so the
 * identity panel offers no fields and no save rather than sending a blind
 * write that would clear them. And nothing holds a reporting-line approval, so
 * the proposal is drawn as a statement beside the line it would replace.
 */

/** One reason a record may be amended for, as the catalogue states it. */
export interface AmendReason {
    readonly code: string;
    readonly description: string;
    readonly requiresCommentary: boolean;
}

/** The reason that records a touch: the save changed nothing in the record. */
const TOUCH_REASON = 'common.non_material_update';

/** The reason a change carries unless the person says otherwise. */
const DEFAULT_CHANGE_REASON = 'system.update';

/**
 * The reason a save carries, and the choice the person may make.
 *
 * A save that changed a field cannot be recorded as a touch, and one that
 * changed nothing can only be a touch. The reason defaults to an ordinary
 * change, so a person who has no reason to say otherwise chooses nothing.
 */
export function useReasonChoice(
    reasons: readonly AmendReason[],
    changed: boolean,
): {
    readonly list: readonly AmendReason[];
    readonly code: string;
    readonly setCode: (code: string) => void;
} {
    const [chosen, setCode] = useState('');
    const list = reasons.filter((reason) => (reason.code === TOUCH_REASON) === !changed);
    const code = list.some((reason) => reason.code === chosen)
        ? chosen
        : (list.find((reason) => reason.code === DEFAULT_CHANGE_REASON)?.code ??
          list[0]?.code ??
          '');
    return { list, code, setCode };
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

/** The link that sends mail to an address, present only when there is an address to send to. */
function mailto(address: string): { readonly href?: string } {
    const trimmed = address.trim();
    return trimmed === '' ? {} : { href: `mailto:${trimmed}` };
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

/**
 * The account a reporting line names, when the caller may read the directory.
 *
 * An account states its manager as an identifier, and a reader can do nothing
 * with an identifier: it is resolved to the row it names so the line can be
 * drawn as that person, with their picture and their name, opening their page.
 * A caller who may not read the directory, or a manager who is no longer in it,
 * is shown the recorded identifier rather than a name the screen invented.
 */
function resolveManager(accounts: readonly Account[], reportsTo: string | null): Account | null {
    if (reportsTo === null) return null;
    return accounts.find((row) => row.id === reportsTo) ?? null;
}

/**
 * The person search, which is the administrator's half of the journey.
 *
 * The list is the tenant's accounts, filtered here by username and name
 * because plan item 1 names it as the read the directory already makes. The
 * rows are the accounts themselves, so a person is picked from what the
 * server holds rather than typed in.
 */
export function IdentityPanel({
    account,
    pending,
    refused,
    signInEmail,
    manager,
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
    readonly manager: Account | null;
    readonly canWrite: boolean;
    readonly me: boolean;
    readonly username: string;
    readonly reasons: readonly AmendReason[];
    readonly onSaved: () => Promise<unknown>;
}): ReactNode {
    const { t } = useTranslation();
    const [editing, setEditing] = useState(false);
    const [changingLine, setChangingLine] = useState(false);

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

    const name = displayName(account, account.username);
    return (
        <section className="card space-y-4 p-6">
            <header className="flex items-center justify-between gap-3">
                <div>
                    <h2 className="text-lg font-medium">{t('profile.identity.title')}</h2>
                    <p className="text-xs text-ink-muted">
                        {t('profile.version', { version: String(account.version) })}
                    </p>
                </div>
                <div className="flex items-center gap-2">
                    {!canWrite && <AccessMark canWrite={false} />}
                    {canWrite && (
                        <Button icon="edit" onClick={() => setEditing(true)}>
                            {t('refdata.records.edit')}
                        </Button>
                    )}
                </div>
            </header>

            <div className="flex flex-wrap items-start gap-5">
                <div className="flex flex-col items-center gap-2">
                    <Avatar
                        name={name}
                        size="lg"
                        src={account.imageId === null ? null : imageUrl(account.imageId)}
                    />
                    {account.imageId === null && (
                        <p className="text-xs text-ink-faint">{t('profile.identity.noPhoto')}</p>
                    )}
                </div>
                <dl className="grid min-w-0 flex-1 gap-x-6 gap-y-3 sm:grid-cols-2">
                    <Detail
                        label={t('profile.identity.fullName')}
                        value={account.fullName === '' ? '—' : account.fullName}
                    />
                    <Detail
                        label={t('profile.identity.jobTitle')}
                        value={account.jobTitle === '' ? '—' : account.jobTitle}
                    />
                    <div>
                        <Detail label={t('profile.identity.username')} value={account.username} />
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
                </dl>
            </div>

            <div className="space-y-2 border-t border-line-subtle pt-4">
                <h3 className="text-sm font-medium">{t('profile.identity.reporting')}</h3>
                <div className="flex flex-wrap items-center gap-2">
                    {account.reportsToAccountId === null && (
                        <p className="text-sm">{t('profile.identity.noLine')}</p>
                    )}
                    {account.reportsToAccountId !== null && manager === null && (
                        <p className="text-sm">
                            {t('profile.identity.reportsTo', {
                                name: account.reportsToAccountId,
                            })}
                        </p>
                    )}
                    {manager !== null && (
                        /*
                         * A link rather than plain text: the point of naming
                         * the person is that a reader can go and look at them,
                         * and a reader who cannot see that it is a link will
                         * not try. The job title beside the name says who they
                         * are to this person without opening the page.
                         */
                        <Link
                            to={personPath(manager.username)}
                            className="flex w-fit items-center gap-2 text-sm text-ink"
                        >
                            <AccountPicture
                                username={manager.username}
                                name={displayName(manager, manager.username)}
                                size="sm"
                            />
                            <span className="underline decoration-line-strong underline-offset-2 hover:decoration-accent">
                                {displayName(manager, manager.username)}
                            </span>
                            {manager.jobTitle !== '' && (
                                <span className="text-xs text-ink-muted">{manager.jobTitle}</span>
                            )}
                        </Link>
                    )}
                    {/*
                     * An administrator sets the line: the server allows it to a
                     * holder of iam::accounts:update and refuses a person who
                     * names their own. Everyone else can only propose, and
                     * nothing holds that approval yet.
                     */}
                    {canWrite && !me ? (
                        <Button
                            size="sm"
                            variant="ghost"
                            icon="edit"
                            aria-label={t('profile.identity.changeLine')}
                            title={t('profile.identity.changeLine')}
                            onClick={() => setChangingLine(true)}
                        />
                    ) : (
                        <Button
                            size="sm"
                            variant="ghost"
                            icon="edit"
                            disabled
                            aria-label={t('profile.identity.propose')}
                            title={`${t('profile.identity.propose')}. ${t('profile.identity.proposeWhy')}`}
                        />
                    )}
                </div>
                {account.reportsToAccountId !== null && manager === null && (
                    <p className="text-xs text-ink-faint">
                        {t('profile.identity.reportsToUnknown')}
                    </p>
                )}
                <p className="text-xs text-ink-faint">{t('profile.identity.proposeApprovers')}</p>
                <p className="text-xs text-ink-faint">{t('profile.identity.proposeGap')}</p>
            </div>

            {changingLine && (
                <ReportingLineDialog
                    account={account}
                    reasons={reasons}
                    onClose={() => setChangingLine(false)}
                    onSaved={onSaved}
                />
            )}
            {editing && (
                <IdentityDialog
                    account={account}
                    me={me}
                    username={username}
                    reasons={reasons}
                    onClose={() => setEditing(false)}
                    onSaved={onSaved}
                />
            )}
        </section>
    );
}

/**
 * The identity edit: the fields a person may change, and the reason for the
 * change, which is asked here and nowhere else. A refusal stays in the dialog
 * under the field it names; a save closes it.
 */
function IdentityDialog({
    account,
    me,
    username,
    reasons,
    onClose,
    onSaved,
}: {
    readonly account: Account;
    readonly me: boolean;
    readonly username: string;
    readonly reasons: readonly AmendReason[];
    readonly onClose: () => void;
    readonly onSaved: () => Promise<unknown>;
}): ReactNode {
    const { t } = useTranslation();
    const [edits, setEdits] = useState<
        Partial<{ fullName: string; jobTitle: string; imageId: string }>
    >({});
    const [commentary, setCommentary] = useState('');
    const [busy, setBusy] = useState(false);
    const [outcome, setOutcome] = useState<PanelOutcome | undefined>(undefined);

    const draft = {
        fullName: edits.fullName ?? account.fullName,
        jobTitle: edits.jobTitle ?? account.jobTitle,
        imageId: edits.imageId ?? account.imageId ?? '',
    };
    const changed =
        draft.fullName !== account.fullName ||
        draft.jobTitle !== account.jobTitle ||
        draft.imageId !== (account.imageId ?? '');
    const choice = useReasonChoice(reasons, changed);
    const reasonCode = choice.code;
    const chosen = choice.list.find((reason) => reason.code === reasonCode);
    const maySave =
        reasonCode !== '' && !(chosen?.requiresCommentary === true && commentary.trim() === '');
    const name = displayName({ fullName: draft.fullName }, account.username);

    const save = async (): Promise<void> => {
        setBusy(true);
        setOutcome(undefined);
        try {
            if (me) {
                const view = await api.saveMyProfile({ ...draft, reasonCode, commentary });
                if (view.result.outcome !== 'ok') {
                    setOutcome({
                        ok: false,
                        message:
                            view.result.code === 'field_not_self_writable'
                                ? t('profile.refused.notYours')
                                : view.result.message,
                        fields: view.result.fields,
                    });
                    return;
                }
            } else {
                await api.saveAccountProfile(username, { ...draft, reasonCode, commentary });
            }
            await onSaved();
            onClose();
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
        <Dialog
            title={t('profile.identity.editTitle')}
            onClose={onClose}
            wide
            footer={
                <>
                    <Button variant="ghost" icon="cancel" onClick={onClose}>
                        {t('refdata.records.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        icon="save"
                        disabled={!maySave}
                        pending={busy}
                        onClick={() => void save()}
                    >
                        {t('refdata.records.save')}
                    </Button>
                </>
            }
        >
            <div className="space-y-4">
                <div className="flex flex-wrap items-start gap-5">
                    <PhotoPicker
                        name={name}
                        imageId={draft.imageId === '' ? null : draft.imageId}
                        label={
                            draft.imageId === ''
                                ? t('profile.identity.choosePhoto')
                                : t('profile.identity.replacePhoto')
                        }
                        onChoose={(imageId) => setEdits({ ...edits, imageId })}
                    />
                    <div className="grid min-w-0 flex-1 gap-4 sm:grid-cols-2">
                        <Field
                            label={t('profile.identity.fullName')}
                            {...fieldIssue(outcome, 'fullName')}
                        >
                            <Input
                                value={draft.fullName}
                                onChange={(event) =>
                                    setEdits({ ...edits, fullName: event.target.value })
                                }
                            />
                        </Field>
                        <Field
                            label={t('profile.identity.jobTitle')}
                            {...fieldIssue(outcome, 'jobTitle')}
                        >
                            <Input
                                value={draft.jobTitle}
                                onChange={(event) =>
                                    setEdits({ ...edits, jobTitle: event.target.value })
                                }
                            />
                        </Field>
                    </div>
                </div>
                <ReasonRow
                    reasons={choice.list}
                    reasonCode={reasonCode}
                    commentary={commentary}
                    issue={fieldIssue(outcome, 'changeReasonCode')}
                    onReason={choice.setCode}
                    onCommentary={setCommentary}
                />
                <OutcomeNotice outcome={outcome} />
            </div>
        </Dialog>
    );
}

/**
 * The reporting-line change: who this person reports to, and why.
 *
 * It writes one field and states the version the screen read, so a record that
 * moved since is refused as a conflict rather than overwritten. The people
 * offered exclude the person and everyone under them, because a line to one of
 * them would be a loop.
 */
export function ReportingLineDialog({
    account,
    reasons,
    onClose,
    onSaved,
}: {
    readonly account: Account;
    readonly reasons: readonly AmendReason[];
    readonly onClose: () => void;
    readonly onSaved: () => Promise<unknown>;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const tree = useQuery({ queryKey: ['reporting-tree'], queryFn: () => api.reportingTree() });
    const [managerId, setManagerId] = useState<string | undefined>(undefined);
    const [commentary, setCommentary] = useState('');
    const [busy, setBusy] = useState(false);
    const [outcome, setOutcome] = useState<PanelOutcome | undefined>(undefined);

    const nodes = tree.data?.nodes ?? [];
    const self = nodes.find((node) => node.accountId === account.id);
    const candidates = self === undefined ? [] : managerCandidates(nodes, self);
    const chosenManager = managerId ?? account.reportsToAccountId ?? '';
    const changed = chosenManager !== (account.reportsToAccountId ?? '');
    const choice = useReasonChoice(reasons, changed);
    const reasonCode = choice.code;
    const chosen = choice.list.find((reason) => reason.code === reasonCode);
    const mayClear =
        reasonCode !== '' && !(chosen?.requiresCommentary === true && commentary.trim() === '');
    const maySave =
        changed &&
        reasonCode !== '' &&
        !(chosen?.requiresCommentary === true && commentary.trim() === '');

    const save = async (to: string): Promise<void> => {
        setBusy(true);
        setOutcome(undefined);
        try {
            await api.setReportingLine(account.id, {
                reportsToAccountId: to,
                expectedVersion: String(account.version),
                reasonCode,
                commentary,
            });
            await queries.invalidateQueries({ queryKey: ['reporting-tree'] });
            await queries.invalidateQueries({ queryKey: ['accounts'] });
            await queries.invalidateQueries({ queryKey: ['timeline', 'person', account.username] });
            await onSaved();
            onClose();
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
        <Dialog
            title={t('profile.identity.changeLine')}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" icon="cancel" onClick={onClose}>
                        {t('refdata.records.cancel')}
                    </Button>
                    {account.reportsToAccountId !== null && (
                        <Button
                            variant="danger"
                            icon="remove"
                            disabled={busy || !mayClear}
                            onClick={() => void save('')}
                        >
                            {t('common.clear')}
                        </Button>
                    )}
                    <Button
                        variant="primary"
                        icon="save"
                        disabled={!maySave}
                        pending={busy}
                        onClick={() => void save(chosenManager)}
                    >
                        {t('refdata.records.save')}
                    </Button>
                </>
            }
        >
            <div className="space-y-4">
                {tree.isError && <Notice tone="error">{tree.error.message}</Notice>}
                <Field label={t('membership.reporting.reportsTo')}>
                    <Select
                        value={chosenManager}
                        disabled={tree.isPending}
                        onChange={(event) => setManagerId(event.target.value)}
                    >
                        <option value="">{t('membership.reporting.noManager')}</option>
                        {candidates.map((node) => (
                            <option key={node.accountId} value={node.accountId}>
                                {node.fullName === '' ? node.username : node.fullName}
                                {node.jobTitle === '' ? '' : ` — ${node.jobTitle}`}
                            </option>
                        ))}
                    </Select>
                </Field>
                <ReasonRow
                    reasons={choice.list}
                    reasonCode={reasonCode}
                    commentary={commentary}
                    issue={fieldIssue(outcome, 'changeReasonCode')}
                    onReason={choice.setCode}
                    onCommentary={setCommentary}
                />
                <OutcomeNotice outcome={outcome} />
            </div>
        </Dialog>
    );
}

/** What a failed save says, with the fields the server named. */
function OutcomeNotice({ outcome }: { readonly outcome: PanelOutcome | undefined }): ReactNode {
    if (outcome === undefined) return null;
    return (
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
            <div className="flex flex-col items-center gap-2">
                <button
                    type="button"
                    onClick={() => setOpen(true)}
                    title={label}
                    aria-label={label}
                    className="rounded-full focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent hover:opacity-80"
                >
                    <Avatar
                        name={name}
                        size="lg"
                        src={imageId === null ? null : imageUrl(imageId)}
                    />
                </button>
                <p className="text-xs text-ink-faint">
                    {imageId === null ? t('profile.identity.noPhoto') : label}
                </p>
            </div>
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
    /*
     * The limit is stated as the validator holds it: a whole number of
     * megabytes reads as one, and anything else keeps its tenth. The tenth
     * is taken down, never up, so the picker never states a larger limit
     * than the server enforces.
     */
    const tenths = Math.floor((policy.maxSizeBytes * 10) / (1024 * 1024)) / 10;
    return t('profile.photo.rule', {
        formats,
        size: Number.isInteger(tenths) ? String(tenths) : tenths.toFixed(1),
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
export function ContactPanel({
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
    const [editing, setEditing] = useState(false);

    if (pending) {
        return (
            <section className="card p-6">
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            </section>
        );
    }

    const shown = (value: string | undefined): string =>
        value === undefined || value === '' ? '—' : value;
    return (
        <section className="card space-y-4 p-6">
            <header className="flex items-center justify-between gap-3">
                <div>
                    <h2 className="text-lg font-medium">{t('profile.contact.title')}</h2>
                    <p className="text-xs text-ink-muted">
                        {contact === null
                            ? t('profile.contact.noRecordShort')
                            : t('profile.version', { version: String(contact.version) })}
                    </p>
                </div>
                <div className="flex items-center gap-2">
                    {!canWrite && <AccessMark canWrite={false} />}
                    {canWrite && (
                        <Button icon="edit" onClick={() => setEditing(true)}>
                            {t('refdata.records.edit')}
                        </Button>
                    )}
                </div>
            </header>

            {refused !== null && <Notice tone="error">{refused}</Notice>}
            {contact === null && refused === null && (
                <p className="text-sm text-ink-muted">{t('profile.contact.noRecord')}</p>
            )}

            <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-2">
                <Detail
                    label={t('profile.contact.streetLine1')}
                    value={shown(contact?.streetLine1)}
                />
                <Detail
                    label={t('profile.contact.streetLine2')}
                    value={shown(contact?.streetLine2)}
                />
                <Detail label={t('profile.contact.city')} value={shown(contact?.city)} />
                <Detail label={t('profile.contact.state')} value={shown(contact?.state)} />
                <Detail
                    label={t('profile.contact.postalCode')}
                    value={shown(contact?.postalCode)}
                />
                <div className="min-w-0">
                    <dt className="text-[11px] uppercase tracking-wide text-ink-faint">
                        {t('profile.contact.country')}
                    </dt>
                    <dd className="mt-0.5 text-sm">
                        {contact?.countryCode === undefined || contact.countryCode === '' ? (
                            '—'
                        ) : (
                            <FlaggedCode source="country" code={contact.countryCode} />
                        )}
                    </dd>
                </div>
                <Detail label={t('profile.contact.phone')} value={shown(contact?.phone)} />
                <div>
                    <Detail
                        label={t('profile.contact.email')}
                        value={shown(contact?.email)}
                        {...mailto(contact?.email ?? '')}
                    />
                    <p className="mt-1 text-xs text-ink-faint">
                        {accountRead && signInEmail !== ''
                            ? t('profile.contact.emailHint', { email: signInEmail })
                            : t('profile.contact.emailHintUnknown')}
                    </p>
                </div>
                <Detail label={t('profile.contact.webPage')} value={shown(contact?.webPage)} />
            </dl>

            {editing && (
                <ContactDialog
                    contact={contact}
                    me={me}
                    accountId={accountId}
                    signInEmail={signInEmail}
                    accountRead={accountRead}
                    reasons={reasons}
                    onClose={() => setEditing(false)}
                    onSaved={onSaved}
                />
            )}
        </section>
    );
}

/**
 * The contact edit: the fields of the record, and the reason for the change,
 * which is asked here and nowhere else. A record that moved since the panel
 * was drawn is refused by the server and the panel is read again.
 */
function ContactDialog({
    contact,
    me,
    accountId,
    signInEmail,
    accountRead,
    reasons,
    onClose,
    onSaved,
}: {
    readonly contact: AccountContactInformation | null;
    readonly me: boolean;
    readonly accountId: string;
    readonly signInEmail: string;
    readonly accountRead: boolean;
    readonly reasons: readonly AmendReason[];
    readonly onClose: () => void;
    readonly onSaved: () => Promise<unknown>;
}): ReactNode {
    const { t } = useTranslation();
    const [edits, setEdits] = useState<Partial<ContactDraft>>({});
    const [commentary, setCommentary] = useState('');
    const [busy, setBusy] = useState(false);
    const [outcome, setOutcome] = useState<PanelOutcome | undefined>(undefined);

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
    const draft: ContactDraft = { ...base, ...edits };
    const changed = (Object.keys(base) as (keyof ContactDraft)[]).some(
        (field) => draft[field] !== base[field],
    );
    const choice = useReasonChoice(reasons, changed);
    const reasonCode = choice.code;
    const chosen = choice.list.find((reason) => reason.code === reasonCode);
    const maySave =
        reasonCode !== '' && !(chosen?.requiresCommentary === true && commentary.trim() === '');
    const set =
        (field: keyof ContactDraft) =>
        (event: ChangeEvent<HTMLInputElement>): void =>
            setEdits({ ...edits, [field]: event.target.value });

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
                await onSaved();
                onClose();
                return;
            }
            setOutcome({
                ok: false,
                /*
                 * The sentence about a moved record belongs to the server's
                 * conflict answer alone. Any other refusal -- a permission, a
                 * field it would not take -- travels with its own words and
                 * nothing added.
                 */
                message:
                    !me && view.result.outcome === 'conflict'
                        ? `${view.result.message} ${t('profile.refused.recordChanged')}`
                        : view.result.message,
                fields: view.result.fields,
            });
            if (!me) await onSaved();
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
        <Dialog
            title={t('profile.contact.editTitle')}
            onClose={onClose}
            wide
            footer={
                <>
                    <Button variant="ghost" icon="cancel" onClick={onClose}>
                        {t('refdata.records.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        icon="save"
                        disabled={!maySave}
                        pending={busy}
                        onClick={() => void save()}
                    >
                        {t('refdata.records.save')}
                    </Button>
                </>
            }
        >
            <div className="space-y-4">
                <div className="grid gap-4 sm:grid-cols-2">
                    <Field label={t('profile.contact.streetLine1')}>
                        <Input value={draft.streetLine1} onChange={set('streetLine1')} />
                    </Field>
                    <Field label={t('profile.contact.streetLine2')}>
                        <Input value={draft.streetLine2} onChange={set('streetLine2')} />
                    </Field>
                    <Field label={t('profile.contact.city')}>
                        <Input value={draft.city} onChange={set('city')} />
                    </Field>
                    <Field label={t('profile.contact.state')}>
                        <Input value={draft.state} onChange={set('state')} />
                    </Field>
                    <Field label={t('profile.contact.postalCode')}>
                        <Input value={draft.postalCode} onChange={set('postalCode')} />
                    </Field>
                    <Field
                        label={t('profile.contact.country')}
                        hint={t('profile.contact.countryHint')}
                        {...fieldIssue(outcome, 'countryCode')}
                    >
                        <div className="flex items-center gap-2">
                            <FlagOf source="country" code={draft.countryCode.trim().toUpperCase()} />
                            <Input value={draft.countryCode} onChange={set('countryCode')} />
                        </div>
                    </Field>
                    <Field label={t('profile.contact.phone')} {...fieldIssue(outcome, 'phone')}>
                        <Input value={draft.phone} onChange={set('phone')} />
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
                        <Input value={draft.email} onChange={set('email')} />
                    </Field>
                    <Field label={t('profile.contact.webPage')}>
                        <Input value={draft.webPage} onChange={set('webPage')} />
                    </Field>
                </div>
                <ReasonRow
                    reasons={choice.list}
                    reasonCode={reasonCode}
                    commentary={commentary}
                    issue={fieldIssue(outcome, 'changeReasonCode')}
                    onReason={choice.setCode}
                    onCommentary={setCommentary}
                />
                <OutcomeNotice outcome={outcome} />
            </div>
        </Dialog>
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
                    {reasons.map((reason) => (
                        <option key={reason.code} value={reason.code}>
                            {t(`profile.reason.${reason.code.replace('.', '_')}`)}
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
/** Which account and contact writes the signed-in person holds. */
export function useAccountWrites(): {
    readonly pending: boolean;
    readonly accounts: boolean;
    readonly contacts: boolean;
} {
    const access = useQuery({ queryKey: ['my-access'], queryFn: api.myAccess });
    const granted = access.data?.roles.flatMap((role) => role.permissionCodes) ?? [];
    const everything = granted.includes('*');
    return {
        pending: access.isPending,
        accounts: everything || granted.includes('iam::accounts:update'),
        contacts: everything || granted.includes('iam::account_contact_informations:write'),
    };
}

/**
 * A person's identity: name, picture, job, reporting line and sign-in address,
 * read and saved for the signed-in person (`me`) or for someone else.
 */
export function IdentityTab({
    username,
    me,
    fallbackEmail,
}: {
    readonly username: string;
    readonly me: boolean;
    readonly fallbackEmail: string;
}): ReactNode {
    const queries = useQueryClient();
    const writes = useAccountWrites();
    const account = useQuery({
        queryKey: ['account', username],
        queryFn: () => api.account(username),
        // The panel states a refused read itself, so the banner stays quiet.
        meta: { quiet: true },
    });
    /*
     * The tenant list that names the manager is the administrator's read, so
     * a member who cannot read it is shown the recorded identifier instead.
     */
    /*
     * The directory is a read, not a write: a member may not change anybody's
     * account and may still see who their manager is. Gating this on write was
     * why a member was shown their manager's identifier instead of the person.
     */
    const mayReadDirectory = useHolds()('iam::accounts:read');
    const managers = useQuery({
        queryKey: ['accounts'],
        queryFn: api.accounts,
        enabled: mayReadDirectory,
    });
    const reasons = useQuery({ queryKey: ['amend-reasons'], queryFn: api.amendReasons });
    return (
        <IdentityPanel
            account={account.data ?? null}
            pending={account.isPending}
            refused={account.isError ? account.error.message : null}
            signInEmail={account.data?.email ?? fallbackEmail}
            manager={resolveManager(
                managers.data?.accounts ?? [],
                account.data?.reportsToAccountId ?? null,
            )}
            canWrite={me || writes.accounts}
            me={me}
            username={username}
            reasons={reasons.data ?? []}
            onSaved={() => queries.invalidateQueries({ queryKey: ['account', username] })}
        />
    );
}

/**
 * How to reach a person: telephone, address and contact email, read and saved
 * for the signed-in person (`me`) or for someone else, whose record is found
 * through their account.
 */
export function ContactTab({
    username,
    me,
    fallbackEmail,
}: {
    readonly username: string;
    readonly me: boolean;
    readonly fallbackEmail: string;
}): ReactNode {
    const queries = useQueryClient();
    const writes = useAccountWrites();
    const account = useQuery({
        queryKey: ['account', username],
        queryFn: () => api.account(username),
        // The panel states a refused read itself, so the banner stays quiet.
        meta: { quiet: true },
    });
    const accountId = account.data?.id ?? '';
    const contactKey = me ? 'me' : accountId;
    const contact = useQuery({
        queryKey: ['contact-information', contactKey],
        queryFn: () => (me ? api.myContactInformation() : api.accountContactInformation(accountId)),
        enabled: contactKey !== '',
    });
    const reasons = useQuery({ queryKey: ['amend-reasons'], queryFn: api.amendReasons });
    /*
     * Someone else's contact record is found through their account, so a
     * failed account read leaves no record to ask for; the panel states that
     * failure rather than waiting on a read that cannot start.
     */
    const waiting = contactKey === '';
    return (
        <ContactPanel
            contact={contact.data ?? null}
            pending={waiting ? account.isPending : contact.isPending}
            refused={waiting ? (account.error?.message ?? null) : (contact.error?.message ?? null)}
            accountRead={account.data !== null && account.data !== undefined}
            canWrite={me || writes.contacts}
            me={me}
            accountId={accountId}
            signInEmail={account.data?.email ?? fallbackEmail}
            reasons={reasons.data ?? []}
            onSaved={() =>
                queries.invalidateQueries({ queryKey: ['contact-information', contactKey] })
            }
        />
    );
}

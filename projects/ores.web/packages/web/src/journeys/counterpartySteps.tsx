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

/**
 * The steps a counterparty onboarding runs.
 *
 * The person sees the counterparties the tenant holds, describes the one they
 * are bringing on board, gives it the names it answers to and the people it is
 * reached through, records the agreements it trades under and the collateral
 * those agreements secure, reads the one summary of all of it, and confirms.
 * The confirmer writes the whole graph as one act, and the outcome reports what
 * the store gave it. A history closes the journey for a counterparty already on
 * board, and a refusal keeps what the person typed and names what was left
 * unchanged.
 *
 * The steps are data, which is why this is a function and not a component: the
 * page hands the result to the runtime, and a test walks the same list with no
 * renderer in the way.
 */

import { Fragment, useEffect, useState, type ReactNode } from 'react';
import { Button, Field, Input, Notice, Select, Tag } from '../ui/Primitives.js';
import { HistoryPanel } from '../refdata/HistoryPanel.js';
import {
    NEW_COUNTERPARTY_REASON,
    COUNTERPARTY_ENTITY_TYPE,
    blankAgreement,
    blankSet,
    compositeRequest,
    contactProblems,
    identifierProblems,
    writeCounts,
    type AgreementField,
    type ContactDraft,
    type ContactField,
    type CounterpartyChildren,
    type CounterpartyIntent,
    type CounterpartyRefusal,
    type CsaField,
    type NettingSetDraft,
    type NewCounterparty,
} from './counterpartyState.js';
import type { JourneyStep, StepId } from './runtime.js';
import type { CounterpartyServer, CounterpartyPickLists } from './counterpartyServer.js';
import type { Counterparty } from '@ores/wire-protocol/generated/refdata/domain/counterparty';
import type { CounterpartyBusinessCentre } from '@ores/wire-protocol/generated/refdata/domain/counterparty_business_centre';
import type { Translator } from '../i18n/translate.js';

/** The steps in the order the rail draws them, and the index each one sits at. */
export const COUNTERPARTY_STEP_IDS = [
    'list',
    'identity',
    'identifiers',
    'contacts',
    'agreements',
    'review',
    'outcome',
    'history',
] as const;

export function counterpartyStepIndex(id: StepId): number {
    const index = COUNTERPARTY_STEP_IDS.indexOf(id as (typeof COUNTERPARTY_STEP_IDS)[number]);
    return index < 0 ? 0 : index;
}

export interface CounterpartyStepsInput {
    readonly t: Translator['t'];
    readonly server: CounterpartyServer;
    readonly state: NewCounterparty;
    /** The tenant's own party, which an agreement's fixed tenant side names. */
    readonly partyId: string;
    /** Every list a picker draws from, once the journey has read them. */
    readonly pickLists: CounterpartyPickLists | undefined;
    /** Why the pick lists are absent, stated on the steps that need them. */
    readonly pickFailure: string | undefined;
    /** The counterparties a new one may name as its parent. */
    readonly parents: readonly Counterparty[];
    readonly onMove: (index: number) => void;
    readonly onFinished: () => void;
    readonly onAnother: () => void;
}

/** The reason and the commentary every write carries. */
function newRecordIntent(): CounterpartyIntent {
    return { reason_code: NEW_COUNTERPARTY_REASON, commentary: '' };
}

/**
 * The refusal as a sentence a person can act on.
 *
 * The server states what it refused and why; the screen adds the two things it
 * knows and the server does not: that nothing was written, and that the step
 * kept the answer. The field failures are appended when the server named any.
 */
function refusalText(t: Translator['t'], refusal: CounterpartyRefusal): string {
    const fields = refusal.fields
        .map((failure) => `${failure.field}: ${failure.message}`)
        .join('; ');
    return [
        t('journey.counterparty.refusal.heading'),
        refusal.subject,
        refusal.message,
        fields,
        t('journey.counterparty.refusal.kept'),
    ]
        .filter((part) => part !== '')
        .join(' ');
}

type WriteOutcome = Awaited<ReturnType<CounterpartyServer['writeComposite']>>;

function refusalOf(writer: string, step: string, outcome: WriteOutcome): CounterpartyRefusal {
    return {
        step,
        subject: writer,
        code: outcome.code,
        message: outcome.message,
        fields: outcome.fields,
    };
}

/** A picker's options, drawn from the list the server holds. */
function codesOf(
    rows: readonly { readonly code: string; readonly name: string }[],
): readonly { readonly value: string; readonly label: string }[] {
    return rows.map((row) => ({
        value: row.code,
        label: row.name === '' ? row.code : row.name,
    }));
}

function Picker({
    label,
    hint,
    value,
    options,
    empty,
    onChange,
}: {
    readonly label: string;
    readonly hint?: string;
    readonly value: string;
    readonly options: readonly { readonly value: string; readonly label: string }[];
    readonly empty?: string;
    readonly onChange: (value: string) => void;
}): ReactNode {
    return (
        <Field label={label} {...(hint === undefined ? {} : { hint })}>
            <Select value={value} onChange={(event) => onChange(event.target.value)}>
                {empty !== undefined && <option value="">{empty}</option>}
                {value !== '' && !options.some((option) => option.value === value) && (
                    <option value={value}>{value}</option>
                )}
                {options.map((option) => (
                    <option key={option.value} value={option.value}>
                        {option.label}
                    </option>
                ))}
            </Select>
        </Field>
    );
}

/* ------------------------------------------------------------------ landing */

function CounterpartyList({
    t,
    server,
    onStart,
    onOpen,
}: {
    readonly t: Translator['t'];
    readonly server: CounterpartyServer;
    readonly onStart: () => void;
    readonly onOpen: (row: Counterparty, children: CounterpartyChildren) => void;
}): ReactNode {
    const [query, setQuery] = useState('');
    const [tab, setTab] = useState<'active' | 'closed' | 'all'>('active');
    const [rows, setRows] = useState<readonly Counterparty[]>();
    const [total, setTotal] = useState(0);
    const [children, setChildren] = useState<ReadonlyMap<string, CounterpartyChildren>>(new Map());
    const [failure, setFailure] = useState<string>();

    useEffect(() => {
        let cancelled = false;
        const load = async (): Promise<void> => {
            try {
                const page = await server.listCounterparties({
                    offset: 0,
                    limit: 50,
                    search: query,
                    status: tab,
                });
                if (cancelled) {
                    return;
                }
                setRows(page.rows);
                setTotal(page.total);
                setFailure(undefined);
                const read = await server.childrenOf(page.rows.map((row) => row.id));
                if (!cancelled) {
                    setChildren(new Map(read.map((bundle) => [bundle.counterpartyId, bundle])));
                }
            } catch (error) {
                if (!cancelled) {
                    setFailure(error instanceof Error ? error.message : String(error));
                }
            }
        };
        void load();
        return () => {
            cancelled = true;
        };
    }, [server, query, tab]);

    const centreLabel = (centres: readonly CounterpartyBusinessCentre[]): string =>
        centres.map((centre) => centre.business_centre_code).join(', ');

    return (
        <div className="space-y-5">
            <div className="flex flex-wrap items-center gap-3">
                <div className="min-w-64 flex-1">
                    <Input
                        value={query}
                        placeholder={t('journey.counterparty.list.search')}
                        onChange={(event) => setQuery(event.target.value)}
                    />
                </div>
                <div className="flex gap-1">
                    {(['active', 'closed', 'all'] as const).map((kind) => (
                        <Button
                            key={kind}
                            size="sm"
                            variant={tab === kind ? 'primary' : 'ghost'}
                            onClick={() => setTab(kind)}
                        >
                            {t(`journey.counterparty.list.tabs.${kind}`)}
                        </Button>
                    ))}
                </div>
                <Button variant="primary" onClick={onStart}>
                    {t('journey.counterparty.list.onboard')}
                </Button>
            </div>

            {failure !== undefined && (
                <Notice tone="error">
                    {t('journey.counterparty.list.readFailed', { message: failure })}
                </Notice>
            )}

            <div className="overflow-x-auto rounded-md border border-line">
                <table className="w-full text-left text-sm">
                    <thead>
                        <tr className="border-b border-line text-xs text-ink-muted">
                            {[
                                'code',
                                'name',
                                'type',
                                'status',
                                'centre',
                                'identifiers',
                                'contacts',
                                'version',
                                'modifiedBy',
                            ].map((column) => (
                                <th key={column} className="px-3 py-2 font-medium">
                                    {t(`journey.counterparty.list.columns.${column}`)}
                                </th>
                            ))}
                        </tr>
                    </thead>
                    <tbody>
                        {rows !== undefined && rows.length === 0 && (
                            <tr>
                                <td colSpan={9} className="px-3 py-3 text-ink-muted">
                                    {t('journey.counterparty.list.empty')}
                                </td>
                            </tr>
                        )}
                        {rows?.map((row) => {
                            const bundle = children.get(row.id);
                            return (
                                <tr
                                    key={row.id}
                                    className="cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover"
                                    onClick={() =>
                                        bundle === undefined ? undefined : onOpen(row, bundle)
                                    }
                                >
                                    <td className="px-3 py-2 font-mono text-xs">
                                        {row.short_code}
                                    </td>
                                    <td className="px-3 py-2">{row.full_name}</td>
                                    <td className="px-3 py-2">{row.party_type}</td>
                                    <td className="px-3 py-2">
                                        <Tag tone={row.status === 'Active' ? 'up' : 'muted'}>
                                            {row.status}
                                        </Tag>
                                    </td>
                                    <td className="px-3 py-2 text-xs">
                                        {centreLabel(bundle?.centres ?? [])}
                                    </td>
                                    <td className="px-3 py-2">
                                        {(bundle?.identifiers.length ?? 0) === 0 ? (
                                            <span className="text-xs text-ink-faint">
                                                {t('journey.counterparty.list.noIdentifier')}
                                            </span>
                                        ) : (
                                            <span className="flex flex-wrap gap-1">
                                                {bundle?.identifiers.map((identifier) => (
                                                    <Tag
                                                        key={identifier.id}
                                                        tone={
                                                            identifier.is_authoritative
                                                                ? 'accent'
                                                                : 'neutral'
                                                        }
                                                    >
                                                        {identifier.id_scheme} {identifier.id_value}
                                                    </Tag>
                                                ))}
                                            </span>
                                        )}
                                    </td>
                                    <td className="px-3 py-2 text-xs text-ink-muted">
                                        {t('journey.counterparty.list.contacts', {
                                            count: bundle?.contacts.length ?? 0,
                                        })}
                                    </td>
                                    <td className="px-3 py-2 text-right font-mono text-xs">
                                        {row.version}
                                    </td>
                                    <td className="px-3 py-2 font-mono text-xs text-ink-muted">
                                        {row.modified_by}
                                    </td>
                                </tr>
                            );
                        })}
                    </tbody>
                </table>
            </div>
            {rows !== undefined && (
                <p className="text-xs text-ink-faint">
                    {t('journey.counterparty.list.total', { total, shown: rows.length })}
                </p>
            )}
        </div>
    );
}

/* ----------------------------------------------------------------- identity */

const IDENTITY_FIELDS = [
    'shortCode',
    'fullName',
    'transliteratedName',
    'partyType',
    'status',
    'parentCounterpartyId',
] as const;

type IdentityField = (typeof IDENTITY_FIELDS)[number];

function IdentityStep({
    t,
    state,
    pickLists,
    pickFailure,
    parents,
}: {
    readonly t: Translator['t'];
    readonly state: NewCounterparty;
    readonly pickLists: CounterpartyPickLists | undefined;
    readonly pickFailure: string | undefined;
    readonly parents: readonly Counterparty[];
}): ReactNode {
    const set = (field: IdentityField, value: string): void => state.setIdentity(field, value);
    const centres = pickLists?.businessCentres ?? [];
    return (
        <div className="space-y-5">
            {pickFailure !== undefined && (
                <Notice tone="warn">
                    {t('journey.counterparty.pickFailed', { message: pickFailure })}
                </Notice>
            )}
            <div className="grid gap-4 sm:grid-cols-2">
                <Field
                    label={t('journey.counterparty.identity.shortCode')}
                    hint={t('journey.counterparty.identity.shortCodeHint')}
                >
                    <Input
                        value={state.shortCode}
                        onChange={(event) => set('shortCode', event.target.value)}
                    />
                </Field>
                <Field
                    label={t('journey.counterparty.identity.fullName')}
                    hint={t('journey.counterparty.identity.fullNameHint')}
                >
                    <Input
                        value={state.fullName}
                        onChange={(event) => set('fullName', event.target.value)}
                    />
                </Field>
                <Field
                    label={t('journey.counterparty.identity.transliteratedName')}
                    hint={t('journey.counterparty.identity.transliteratedHint')}
                >
                    <Input
                        value={state.transliteratedName}
                        onChange={(event) => set('transliteratedName', event.target.value)}
                    />
                </Field>
                <Picker
                    label={t('journey.counterparty.identity.partyType')}
                    value={state.partyType}
                    options={codesOf(pickLists?.partyTypes ?? [])}
                    empty={t('journey.counterparty.identity.choose')}
                    onChange={(value) => set('partyType', value)}
                />
                <Picker
                    label={t('journey.counterparty.identity.status')}
                    value={state.status}
                    options={codesOf(pickLists?.partyStatuses ?? [])}
                    empty={t('journey.counterparty.identity.choose')}
                    onChange={(value) => set('status', value)}
                />
                <Picker
                    label={t('journey.counterparty.identity.parent')}
                    hint={t('journey.counterparty.identity.parentHint')}
                    value={state.parentCounterpartyId}
                    options={parents
                        .filter((parent) => parent.id !== state.id)
                        .map((parent) => ({
                            value: parent.id,
                            label: `${parent.full_name} (${parent.short_code})`,
                        }))}
                    empty={t('journey.counterparty.identity.noParent')}
                    onChange={(value) => set('parentCounterpartyId', value)}
                />
            </div>

            <div className="rounded-md border border-line bg-surface-overlay p-4">
                <h3 className="text-sm font-semibold">
                    {t('journey.counterparty.identity.centres')}
                </h3>
                <p className="mt-1 text-xs text-ink-faint">
                    {t('journey.counterparty.identity.centresHint')}
                </p>
                <div className="mt-3 flex flex-wrap gap-3">
                    {centres.map((centre) => {
                        const chosen = state.businessCentreCodes.includes(centre.code);
                        return (
                            <label key={centre.code} className="flex items-center gap-2 text-sm">
                                <input
                                    type="checkbox"
                                    checked={chosen}
                                    onChange={() =>
                                        state.setCentres(
                                            chosen
                                                ? state.businessCentreCodes.filter(
                                                      (code) => code !== centre.code,
                                                  )
                                                : [...state.businessCentreCodes, centre.code],
                                        )
                                    }
                                />
                                <span className="font-mono text-xs">{centre.code}</span>
                                {centre.city_name}
                            </label>
                        );
                    })}
                    {centres.length === 0 && (
                        <span className="text-sm text-ink-faint">
                            {t('journey.counterparty.identity.noCentres')}
                        </span>
                    )}
                </div>
            </div>
        </div>
    );
}

/* -------------------------------------------------------------- identifiers */

function IdentifiersStep({
    t,
    server,
    state,
    pickLists,
}: {
    readonly t: Translator['t'];
    readonly server: CounterpartyServer;
    readonly state: NewCounterparty;
    readonly pickLists: CounterpartyPickLists | undefined;
}): ReactNode {
    const [scheme, setScheme] = useState('');
    const [value, setValue] = useState('');
    const [visibleParties, setVisibleParties] = useState<readonly string[]>();

    useEffect(() => {
        if (!state.opened) {
            return;
        }
        let cancelled = false;
        void server
            .visibilityOf(state.id)
            .then((rows) => {
                if (!cancelled) {
                    setVisibleParties(rows.map((row) => row.party_id));
                }
            })
            .catch(() => {
                if (!cancelled) {
                    setVisibleParties(undefined);
                }
            });
        return () => {
            cancelled = true;
        };
    }, [server, state.opened, state.id]);
    const schemes = codesOf(pickLists?.identifierSchemes ?? []);
    const chosen = scheme === '' ? (schemes[0]?.value ?? '') : scheme;
    const problems = identifierProblems(state);
    const authoritative = state.authoritative;

    return (
        <div className="space-y-5">
            {problems.length > 0 ? (
                <Notice tone="warn">
                    <ul className="ml-4 list-disc">
                        {problems.map((problem) => (
                            <li key={problem}>{problem}</li>
                        ))}
                    </ul>
                </Notice>
            ) : (
                <Notice tone="info">{t('journey.counterparty.identifiers.resolution')}</Notice>
            )}

            <ul className="divide-y divide-line-subtle rounded-md border border-line">
                {state.identifiers.length === 0 && (
                    <li className="px-3 py-2 text-sm text-ink-faint">
                        {t('journey.counterparty.identifiers.empty')}
                    </li>
                )}
                {state.identifiers.map((identifier) => (
                    <li key={identifier.id} className="flex items-center gap-3 px-3 py-2 text-sm">
                        <Tag tone={identifier.authoritative ? 'accent' : 'neutral'}>
                            {identifier.scheme}
                        </Tag>
                        <span className="flex-1 font-mono text-xs">
                            {identifier.value === '' ? (
                                <span className="text-ink-faint">
                                    {t('journey.counterparty.identifiers.noValue')}
                                </span>
                            ) : (
                                identifier.value
                            )}
                        </span>
                        {identifier.authoritative && (
                            <Tag tone="up">
                                {t('journey.counterparty.identifiers.authoritative')}
                            </Tag>
                        )}
                        <Button
                            size="sm"
                            variant="ghost"
                            onClick={() => state.markAuthoritative(identifier.id)}
                        >
                            {t('journey.counterparty.identifiers.mark')}
                        </Button>
                        <Button
                            size="sm"
                            variant="ghost"
                            onClick={() => state.removeIdentifier(identifier.id)}
                        >
                            {t('journey.counterparty.identifiers.remove')}
                        </Button>
                    </li>
                ))}
            </ul>

            <div className="grid gap-4 rounded-md border border-line bg-surface-overlay p-4 sm:grid-cols-2">
                <Picker
                    label={t('journey.counterparty.identifiers.scheme')}
                    value={chosen}
                    options={schemes}
                    onChange={setScheme}
                />
                <Field label={t('journey.counterparty.identifiers.value')}>
                    <Input value={value} onChange={(event) => setValue(event.target.value)} />
                </Field>
                <div className="sm:col-span-2">
                    <Button
                        size="sm"
                        disabled={value.trim() === ''}
                        onClick={() => {
                            state.addIdentifier(chosen, value.trim());
                            setValue('');
                        }}
                    >
                        {t('journey.counterparty.identifiers.add')}
                    </Button>
                </div>
            </div>

            <dl className="grid gap-2 text-sm sm:grid-cols-2">
                <dt className="text-ink-faint">
                    {t('journey.counterparty.identifiers.authoritativeLabel')}
                </dt>
                <dd>
                    {authoritative === undefined
                        ? t('journey.counterparty.identifiers.none')
                        : `${authoritative.scheme} ${authoritative.value}`}
                </dd>
                <dt className="text-ink-faint">
                    {t('journey.counterparty.identifiers.visibleTo')}
                </dt>
                <dd>
                    {!state.opened
                        ? t('journey.counterparty.identifiers.visibleSelf')
                        : visibleParties === undefined
                          ? t('journey.counterparty.identifiers.visibleOpened')
                          : visibleParties.join(', ')}
                </dd>
            </dl>
        </div>
    );
}

/* ----------------------------------------------------------------- contacts */

const CONTACT_FIELDS = [
    'streetLine1',
    'streetLine2',
    'city',
    'state',
    'countryCode',
    'postalCode',
    'phone',
    'email',
    'webPage',
] as const satisfies readonly ContactField[];

function contactSummary(contact: ContactDraft): string {
    const where = [
        contact.streetLine1,
        contact.streetLine2,
        contact.city,
        contact.state,
        contact.countryCode,
        contact.postalCode,
    ]
        .filter((part) => part !== '')
        .join(', ');
    return where !== '' ? where : contact.email;
}

function ContactsStep({
    t,
    state,
    pickLists,
}: {
    readonly t: Translator['t'];
    readonly state: NewCounterparty;
    readonly pickLists: CounterpartyPickLists | undefined;
}): ReactNode {
    const types = codesOf(pickLists?.contactTypes ?? []);
    const [selected, setSelected] = useState('');
    const type = selected === '' ? (types[0]?.value ?? '') : selected;
    const contact = state.contacts.find((candidate) => candidate.contactType === type);
    const problems = contactProblems(state);

    return (
        <div className="space-y-5">
            {problems.length > 0 && (
                <Notice tone="warn">
                    <ul className="ml-4 list-disc">
                        {problems.map((problem) => (
                            <li key={problem}>{problem}</li>
                        ))}
                    </ul>
                </Notice>
            )}

            <ul className="divide-y divide-line-subtle rounded-md border border-line">
                {types.map((candidate) => {
                    const held = state.contacts.find((row) => row.contactType === candidate.value);
                    return (
                        <li
                            key={candidate.value}
                            className="flex cursor-pointer items-center gap-3 px-3 py-2 text-sm hover:bg-surface-hover"
                            onClick={() => setSelected(candidate.value)}
                        >
                            <Tag tone={type === candidate.value ? 'accent' : 'neutral'}>
                                {candidate.label}
                            </Tag>
                            <span className="flex-1 text-xs text-ink-muted">
                                {held === undefined
                                    ? t('journey.counterparty.contacts.notAdded')
                                    : contactSummary(held)}
                            </span>
                            {held === undefined ? (
                                <Button
                                    size="sm"
                                    variant="ghost"
                                    onClick={(event) => {
                                        event.stopPropagation();
                                        state.addContact(candidate.value);
                                        setSelected(candidate.value);
                                    }}
                                >
                                    {t('journey.counterparty.contacts.add')}
                                </Button>
                            ) : (
                                <Button
                                    size="sm"
                                    variant="ghost"
                                    onClick={(event) => {
                                        event.stopPropagation();
                                        state.removeContact(held.id);
                                    }}
                                >
                                    {t('journey.counterparty.contacts.remove')}
                                </Button>
                            )}
                        </li>
                    );
                })}
                {types.length === 0 && (
                    <li className="px-3 py-2 text-sm text-ink-faint">
                        {t('journey.counterparty.contacts.empty')}
                    </li>
                )}
            </ul>

            {contact !== undefined && (
                <div className="grid gap-4 rounded-md border border-line bg-surface-overlay p-4 sm:grid-cols-2">
                    {CONTACT_FIELDS.map((field) => (
                        <Field
                            key={field}
                            label={t(`journey.counterparty.contacts.${field}`)}
                            className={field === 'webPage' ? 'sm:col-span-2' : ''}
                        >
                            <Input
                                value={contact[field]}
                                onChange={(event) =>
                                    state.setContact(contact.id, field, event.target.value)
                                }
                            />
                        </Field>
                    ))}
                </div>
            )}
        </div>
    );
}

/* --------------------------------------------------------------- agreements */

const AGREEMENT_FIELDS = [
    { field: 'agreementNumber', key: 'number' },
    { field: 'agreementType', key: 'type' },
    { field: 'governingLaw', key: 'law' },
    { field: 'description', key: 'description' },
] as const satisfies readonly { readonly field: AgreementField; readonly key: string }[];

const SET_FIELDS = [
    { field: 'code', key: 'code' },
    { field: 'callType', key: 'callType' },
    { field: 'initialMarginType', key: 'initialMarginType' },
    { field: 'riskWeight', key: 'riskWeight' },
    { field: 'description', key: 'description' },
] as const;

const CSA_FIELDS = [
    { field: 'bilateral', key: 'bilateral' },
    { field: 'currency', key: 'currency' },
    { field: 'indexName', key: 'indexName' },
    { field: 'marginPeriodOfRisk', key: 'mpor' },
    { field: 'thresholdPay', key: 'thresholdPay' },
    { field: 'thresholdReceive', key: 'thresholdReceive' },
    { field: 'minimumTransferAmountPay', key: 'mtaPay' },
    { field: 'minimumTransferAmountReceive', key: 'mtaReceive' },
    { field: 'independentAmount', key: 'independentAmount' },
    { field: 'independentAmountType', key: 'independentAmountType' },
    { field: 'callFrequency', key: 'callFrequency' },
    { field: 'postFrequency', key: 'postFrequency' },
    { field: 'spreadReceive', key: 'spreadReceive' },
    { field: 'spreadPay', key: 'spreadPay' },
    { field: 'nonExemptImRegulations', key: 'nonExempt' },
] as const satisfies readonly { readonly field: CsaField; readonly key: string }[];

const CSA_TOGGLES = [
    { field: 'isActive', key: 'isActive' },
    { field: 'applyInitialMargin', key: 'applyInitialMargin' },
    { field: 'calculateImAmount', key: 'calculateImAmount' },
    { field: 'calculateVmAmount', key: 'calculateVmAmount' },
] as const;

function AgreementsStep({
    t,
    state,
    partyId,
    pickLists,
}: {
    readonly t: Translator['t'];
    readonly state: NewCounterparty;
    readonly partyId: string;
    readonly pickLists: CounterpartyPickLists | undefined;
}): ReactNode {
    const [agreementId, setAgreementId] = useState('');
    const [setId, setSetId] = useState('');
    const [draft, setDraft] = useState({
        number: '',
        type: 'ISDA Master Agreement',
        law: 'English law',
    });
    const [newSet, setNewSet] = useState({ code: '', callType: 'Bilateral' });
    const [setName, setSetName] = useState({ scheme: 'ORE', value: '' });
    const [currency, setCurrency] = useState('');

    const agreement =
        state.agreements.find((candidate) => candidate.id === agreementId) ?? state.agreements[0];
    const set = agreement?.sets.find((candidate) => candidate.id === setId) ?? agreement?.sets[0];
    const currencies = (pickLists?.currencies ?? []).map((row) => ({
        value: row.iso_code,
        label: row.name === '' ? row.iso_code : row.name,
    }));

    if (state.agreements.length === 0 || agreement === undefined) {
        return (
            <div className="space-y-5">
                <Notice tone="info">{t('journey.counterparty.agreements.empty')}</Notice>
                {addAgreementForm()}
            </div>
        );
    }

    function addAgreementForm(): ReactNode {
        return (
            <div className="grid gap-4 rounded-md border border-line bg-surface-overlay p-4 sm:grid-cols-2">
                <Field label={t('journey.counterparty.agreements.number')}>
                    <Input
                        value={draft.number}
                        onChange={(event) => setDraft({ ...draft, number: event.target.value })}
                    />
                </Field>
                <Field label={t('journey.counterparty.agreements.type')}>
                    <Input
                        value={draft.type}
                        onChange={(event) => setDraft({ ...draft, type: event.target.value })}
                    />
                </Field>
                <Field label={t('journey.counterparty.agreements.law')}>
                    <Input
                        value={draft.law}
                        onChange={(event) => setDraft({ ...draft, law: event.target.value })}
                    />
                </Field>
                <div className="sm:col-span-2">
                    <Button
                        size="sm"
                        disabled={draft.number.trim() === ''}
                        onClick={() => {
                            const made = blankAgreement(draft.number.trim());
                            state.addAgreement({
                                ...made,
                                agreementType: draft.type,
                                governingLaw: draft.law,
                            });
                            setAgreementId(made.id);
                            setSetId('');
                            setDraft({ ...draft, number: '' });
                        }}
                    >
                        {t('journey.counterparty.agreements.add')}
                    </Button>
                </div>
            </div>
        );
    }

    return (
        <div className="space-y-5">
            <div className="flex flex-wrap gap-2">
                {state.agreements.map((candidate) => (
                    <Button
                        key={candidate.id}
                        size="sm"
                        variant={candidate.id === agreement.id ? 'primary' : 'ghost'}
                        onClick={() => {
                            setAgreementId(candidate.id);
                            setSetId('');
                        }}
                    >
                        {candidate.agreementNumber}
                    </Button>
                ))}
            </div>

            <div className="grid gap-4 rounded-md border border-line bg-surface-overlay p-4 sm:grid-cols-2">
                {AGREEMENT_FIELDS.map((entry) => (
                    <Field
                        key={entry.field}
                        label={t(`journey.counterparty.agreements.${entry.key}`)}
                        className={entry.field === 'description' ? 'sm:col-span-2' : ''}
                    >
                        <Input
                            value={agreement[entry.field]}
                            onChange={(event) =>
                                state.setAgreement(agreement.id, entry.field, event.target.value)
                            }
                        />
                    </Field>
                ))}
                <p className="text-xs text-ink-faint sm:col-span-2">
                    {t('journey.counterparty.agreements.parties', {
                        tenant:
                            partyId === ''
                                ? t('journey.counterparty.agreements.tenantParty')
                                : partyId,
                        counterparty:
                            state.fullName === ''
                                ? t('journey.counterparty.agreements.thisCounterparty')
                                : state.fullName,
                    })}
                </p>
                <div className="sm:col-span-2">
                    <Button
                        size="sm"
                        variant="ghost"
                        onClick={() => {
                            state.removeAgreement(agreement.id);
                            setAgreementId('');
                            setSetId('');
                        }}
                    >
                        {t('journey.counterparty.agreements.remove')}
                    </Button>
                </div>
            </div>

            <h3 className="text-sm font-semibold">{t('journey.counterparty.agreements.sets')}</h3>
            <ul className="space-y-3">
                {agreement.sets.map((candidate) => (
                    <li key={candidate.id} className="rounded-md border border-line p-4">
                        <div className="flex flex-wrap items-center gap-3">
                            <Button
                                size="sm"
                                variant={candidate.id === set?.id ? 'primary' : 'ghost'}
                                onClick={() => setSetId(candidate.id)}
                            >
                                {candidate.code}
                            </Button>
                            <Tag tone={candidate.csa.isActive ? 'up' : 'warn'}>
                                {candidate.csa.isActive
                                    ? t('journey.counterparty.agreements.csaActive')
                                    : t('journey.counterparty.agreements.csaOff')}
                            </Tag>
                            <span className="flex-1 text-xs text-ink-muted">
                                {candidate.identifiers.length === 0
                                    ? t('journey.counterparty.agreements.set.noNames')
                                    : candidate.identifiers
                                          .map(
                                              (identifier) =>
                                                  `${identifier.scheme} ${identifier.value}`,
                                          )
                                          .join(', ')}
                            </span>
                            <Button
                                size="sm"
                                variant="ghost"
                                onClick={() => {
                                    state.removeSet(agreement.id, candidate.id);
                                    setSetId('');
                                }}
                            >
                                {t('journey.counterparty.agreements.set.remove')}
                            </Button>
                        </div>

                        {candidate.id === set?.id && (
                            <div className="mt-4 space-y-5">
                                <div className="grid gap-4 sm:grid-cols-2">
                                    {SET_FIELDS.map((entry) => (
                                        <Field
                                            key={entry.field}
                                            label={t(
                                                `journey.counterparty.agreements.set.${entry.key}`,
                                            )}
                                            className={
                                                entry.field === 'description' ? 'sm:col-span-2' : ''
                                            }
                                        >
                                            <Input
                                                value={candidate[entry.field]}
                                                onChange={(event) =>
                                                    state.setSet(
                                                        agreement.id,
                                                        candidate.id,
                                                        entry.field,
                                                        event.target.value,
                                                    )
                                                }
                                            />
                                        </Field>
                                    ))}
                                </div>

                                <div>
                                    <h4 className="text-sm font-semibold">
                                        {t('journey.counterparty.agreements.set.names')}
                                    </h4>
                                    <ul className="mt-2 divide-y divide-line-subtle rounded-md border border-line">
                                        {candidate.identifiers.map((identifier) => (
                                            <li
                                                key={identifier.id}
                                                className="flex items-center gap-3 px-3 py-2 text-sm"
                                            >
                                                <Tag tone="accent">{identifier.scheme}</Tag>
                                                <span className="flex-1 font-mono text-xs">
                                                    {identifier.value}
                                                </span>
                                                <Button
                                                    size="sm"
                                                    variant="ghost"
                                                    onClick={() =>
                                                        state.removeSetIdentifier(
                                                            agreement.id,
                                                            candidate.id,
                                                            identifier.id,
                                                        )
                                                    }
                                                >
                                                    {t(
                                                        'journey.counterparty.agreements.set.removeName',
                                                    )}
                                                </Button>
                                            </li>
                                        ))}
                                    </ul>
                                    <div className="mt-3 flex flex-wrap items-end gap-3">
                                        <Field
                                            label={t('journey.counterparty.agreements.set.scheme')}
                                        >
                                            <Input
                                                value={setName.scheme}
                                                onChange={(event) =>
                                                    setSetName({
                                                        ...setName,
                                                        scheme: event.target.value,
                                                    })
                                                }
                                            />
                                        </Field>
                                        <Field
                                            label={t('journey.counterparty.agreements.set.value')}
                                        >
                                            <Input
                                                value={setName.value}
                                                onChange={(event) =>
                                                    setSetName({
                                                        ...setName,
                                                        value: event.target.value,
                                                    })
                                                }
                                            />
                                        </Field>
                                        <Button
                                            size="sm"
                                            disabled={setName.value.trim() === ''}
                                            onClick={() => {
                                                state.addSetIdentifier(
                                                    agreement.id,
                                                    candidate.id,
                                                    setName.scheme,
                                                    setName.value.trim(),
                                                );
                                                setSetName({ ...setName, value: '' });
                                            }}
                                        >
                                            {t('journey.counterparty.agreements.set.addName')}
                                        </Button>
                                    </div>
                                </div>

                                <div>
                                    <h4 className="text-sm font-semibold">
                                        {t('journey.counterparty.agreements.csa.title')}
                                    </h4>
                                    <div className="mt-2 grid gap-4 sm:grid-cols-2">
                                        {CSA_FIELDS.map((entry) => (
                                            <Field
                                                key={entry.field}
                                                label={t(
                                                    `journey.counterparty.agreements.csa.${entry.key}`,
                                                )}
                                                className={
                                                    entry.field === 'nonExemptImRegulations'
                                                        ? 'sm:col-span-2'
                                                        : ''
                                                }
                                            >
                                                <Input
                                                    value={candidate.csa[entry.field]}
                                                    onChange={(event) =>
                                                        state.setCsa(
                                                            agreement.id,
                                                            candidate.id,
                                                            entry.field,
                                                            event.target.value,
                                                        )
                                                    }
                                                />
                                            </Field>
                                        ))}
                                    </div>
                                    <div className="mt-3 flex flex-wrap gap-4">
                                        {CSA_TOGGLES.map((toggle) => (
                                            <label
                                                key={toggle.field}
                                                className="flex items-center gap-2 text-sm"
                                            >
                                                <input
                                                    type="checkbox"
                                                    checked={candidate.csa[toggle.field]}
                                                    onChange={(event) =>
                                                        state.toggleCsa(
                                                            agreement.id,
                                                            candidate.id,
                                                            toggle.field,
                                                            event.target.checked,
                                                        )
                                                    }
                                                />
                                                {t(
                                                    `journey.counterparty.agreements.csa.${toggle.key}`,
                                                )}
                                            </label>
                                        ))}
                                    </div>
                                </div>

                                <div>
                                    <h4 className="text-sm font-semibold">
                                        {t('journey.counterparty.agreements.csa.eligible')}
                                    </h4>
                                    <div className="mt-2 flex flex-wrap gap-2">
                                        {candidate.eligible.map((eligible) => (
                                            <span
                                                key={eligible.id}
                                                className="inline-flex items-center gap-2"
                                            >
                                                <Tag>
                                                    {eligible.currencyCode} · {eligible.position}
                                                </Tag>
                                                <Button
                                                    size="sm"
                                                    variant="ghost"
                                                    onClick={() =>
                                                        state.removeEligibleCurrency(
                                                            agreement.id,
                                                            candidate.id,
                                                            eligible.id,
                                                        )
                                                    }
                                                >
                                                    {t('journey.counterparty.agreements.csa.drop')}
                                                </Button>
                                            </span>
                                        ))}
                                    </div>
                                    <div className="mt-3 flex flex-wrap items-end gap-3">
                                        <Picker
                                            label={t(
                                                'journey.counterparty.agreements.csa.currency',
                                            )}
                                            value={currency}
                                            options={currencies}
                                            empty={t('journey.counterparty.agreements.csa.choose')}
                                            onChange={setCurrency}
                                        />
                                        <Button
                                            size="sm"
                                            disabled={currency === ''}
                                            onClick={() => {
                                                state.addEligibleCurrency(
                                                    agreement.id,
                                                    candidate.id,
                                                    currency,
                                                );
                                                setCurrency('');
                                            }}
                                        >
                                            {t('journey.counterparty.agreements.csa.addEligible')}
                                        </Button>
                                    </div>
                                </div>
                            </div>
                        )}
                    </li>
                ))}
                {agreement.sets.length === 0 && (
                    <li className="rounded-md border border-line px-3 py-2 text-sm text-ink-faint">
                        {t('journey.counterparty.agreements.set.none')}
                    </li>
                )}
            </ul>

            <div className="grid gap-4 rounded-md border border-line bg-surface-overlay p-4 sm:grid-cols-2">
                <Field label={t('journey.counterparty.agreements.set.code')}>
                    <Input
                        value={newSet.code}
                        onChange={(event) => setNewSet({ ...newSet, code: event.target.value })}
                    />
                </Field>
                <Field label={t('journey.counterparty.agreements.set.callType')}>
                    <Input
                        value={newSet.callType}
                        onChange={(event) => setNewSet({ ...newSet, callType: event.target.value })}
                    />
                </Field>
                <div className="sm:col-span-2">
                    <Button
                        size="sm"
                        disabled={newSet.code.trim() === ''}
                        onClick={() => {
                            const made: NettingSetDraft = {
                                ...blankSet(newSet.code.trim()),
                                callType: newSet.callType,
                            };
                            state.addSet(agreement.id, made);
                            setSetId(made.id);
                            setNewSet({ ...newSet, code: '' });
                        }}
                    >
                        {t('journey.counterparty.agreements.set.add')}
                    </Button>
                </div>
            </div>

            {addAgreementForm()}
        </div>
    );
}

/* ------------------------------------------------------------------- review */

function summaryRows(
    t: Translator['t'],
    state: NewCounterparty,
): readonly (readonly [string, string])[] {
    const dash = (value: string): string => (value === '' ? '—' : value);
    const sets = state.agreements.flatMap((agreement) => agreement.sets);
    const rows: readonly (readonly [string, string])[] = [
        [t('journey.counterparty.review.rows.shortCode'), dash(state.shortCode)],
        [t('journey.counterparty.review.rows.fullName'), dash(state.fullName)],
        [t('journey.counterparty.review.rows.transliteratedName'), dash(state.transliteratedName)],
        [t('journey.counterparty.review.rows.partyType'), dash(state.partyType)],
        [t('journey.counterparty.review.rows.status'), dash(state.status)],
        [t('journey.counterparty.review.rows.centres'), dash(state.businessCentreCodes.join(', '))],
        [t('journey.counterparty.review.rows.parent'), dash(state.parentCounterpartyId)],
        [
            t('journey.counterparty.review.rows.authoritative'),
            state.authoritative === undefined
                ? t('journey.counterparty.review.none')
                : `${state.authoritative.scheme} ${state.authoritative.value}`,
        ],
        [
            t('journey.counterparty.review.rows.identifiers'),
            dash(state.identifiers.map((i) => `${i.scheme} ${i.value}`).join(', ')),
        ],
        [
            t('journey.counterparty.review.rows.contacts'),
            dash(state.contacts.map((c) => c.contactType).join(', ')),
        ],
        [
            t('journey.counterparty.review.rows.agreements'),
            dash(state.agreements.map((a) => a.agreementNumber).join(', ')),
        ],
        [t('journey.counterparty.review.rows.sets'), dash(sets.map((set) => set.code).join(', '))],
        [
            t('journey.counterparty.review.rows.collateral'),
            dash(
                sets
                    .map(
                        (set) =>
                            `${set.code}: ${
                                set.csa.isActive
                                    ? t('journey.counterparty.agreements.csaActive')
                                    : t('journey.counterparty.agreements.csaOff')
                            } ${set.csa.currency}`,
                    )
                    .join('; '),
            ),
        ],
        [
            t('journey.counterparty.review.rows.visibleTo'),
            t('journey.counterparty.review.visibleSelf'),
        ],
    ];
    return rows;
}

function RefusalPanel({
    t,
    refusal,
    onMove,
}: {
    readonly t: Translator['t'];
    readonly refusal: CounterpartyRefusal;
    readonly onMove: (index: number) => void;
}): ReactNode {
    return (
        <Notice tone="error">
            <p className="font-semibold">{t('journey.counterparty.refusal.title')}</p>
            <p className="mt-2 font-mono text-xs break-words">{refusal.subject}</p>
            <p className="mt-2">{refusal.message}</p>
            {refusal.fields.length > 0 && (
                <ul className="mt-2 ml-4 list-disc">
                    {refusal.fields.map((failure) => (
                        <li key={failure.field}>
                            {failure.field}: {failure.message}
                        </li>
                    ))}
                </ul>
            )}
            <p className="mt-2">{t('journey.counterparty.refusal.unchanged')}</p>
            <div className="mt-3 flex flex-wrap gap-2">
                <Button
                    size="sm"
                    variant="primary"
                    onClick={() => onMove(counterpartyStepIndex(refusal.step))}
                >
                    {t('journey.counterparty.refusal.back', {
                        step: t(`journey.counterparty.step.${refusal.step}`),
                    })}
                </Button>
            </div>
        </Notice>
    );
}

function ReviewStep({
    t,
    state,
    onMove,
}: {
    readonly t: Translator['t'];
    readonly state: NewCounterparty;
    readonly onMove: (index: number) => void;
}): ReactNode {
    return (
        <div className="space-y-5">
            {state.refusal !== undefined && (
                <RefusalPanel t={t} refusal={state.refusal} onMove={onMove} />
            )}
            <dl className="grid gap-2 text-sm sm:grid-cols-2">
                {summaryRows(t, state).map(([label, value]) => (
                    <Fragment key={label}>
                        <dt className="text-ink-faint">{label}</dt>
                        <dd>{value}</dd>
                    </Fragment>
                ))}
            </dl>

            <h3 className="text-sm font-semibold">{t('journey.counterparty.review.writes')}</h3>
            <div className="overflow-x-auto rounded-md border border-line">
                <table className="w-full text-left text-sm">
                    <tbody>
                        {writeCounts(state).map(([subject, count]) => (
                            <tr
                                key={subject}
                                className="border-b border-line-subtle last:border-b-0"
                            >
                                <th className="px-3 py-2 font-mono text-xs font-normal text-ink-muted">
                                    {subject}
                                </th>
                                <td className="px-3 py-2 text-xs">
                                    {t('journey.counterparty.review.rowCount', { count })}
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>
        </div>
    );
}

/* ------------------------------------------------------------------ outcome */

function OutcomeStep({
    t,
    state,
    onMove,
    onAnother,
    onFinished,
}: {
    readonly t: Translator['t'];
    readonly state: NewCounterparty;
    readonly onMove: (index: number) => void;
    readonly onAnother: () => void;
    readonly onFinished: () => void;
}): ReactNode {
    const written = state.written;
    const version = written?.version ?? 1;
    return (
        <div className="space-y-5">
            <Notice tone="success">
                {t('journey.counterparty.outcome.onBoard', {
                    name:
                        state.fullName === ''
                            ? t('journey.counterparty.outcome.it')
                            : state.fullName,
                })}
            </Notice>
            <dl className="grid gap-2 text-sm sm:grid-cols-2">
                <dt className="text-ink-faint">
                    {t('journey.counterparty.review.rows.shortCode')}
                </dt>
                <dd>{state.shortCode}</dd>
                <dt className="text-ink-faint">
                    {t('journey.counterparty.review.rows.identifiers')}
                </dt>
                <dd>
                    {state.identifiers.map((identifier) => (
                        <Tag key={identifier.id} tone={identifier.authoritative ? 'up' : 'neutral'}>
                            {identifier.scheme} {identifier.value}
                        </Tag>
                    ))}
                </dd>
                <dt className="text-ink-faint">{t('journey.counterparty.review.rows.contacts')}</dt>
                <dd>{state.contacts.map((contact) => contact.contactType).join(', ') || '—'}</dd>
                <dt className="text-ink-faint">
                    {t('journey.counterparty.review.rows.agreements')}
                </dt>
                <dd>
                    {state.agreements.map((agreement) => agreement.agreementNumber).join(', ') ||
                        '—'}
                </dd>
                <dt className="text-ink-faint">{t('journey.counterparty.outcome.version')}</dt>
                <dd className="font-mono text-xs">{version}</dd>
            </dl>
            <div className="grid gap-3 sm:grid-cols-3">
                <button
                    type="button"
                    className="card p-4 text-left hover:border-accent"
                    onClick={() => onMove(counterpartyStepIndex('history'))}
                >
                    <span className="font-semibold">
                        {t('journey.counterparty.outcome.history')}
                    </span>
                    <p className="mt-1 text-sm text-ink-muted">
                        {t('journey.counterparty.outcome.historyHint')}
                    </p>
                </button>
                <button
                    type="button"
                    className="card p-4 text-left hover:border-accent"
                    onClick={() => {
                        state.startNew();
                        onAnother();
                    }}
                >
                    <span className="font-semibold">
                        {t('journey.counterparty.outcome.another')}
                    </span>
                    <p className="mt-1 text-sm text-ink-muted">
                        {t('journey.counterparty.outcome.anotherHint')}
                    </p>
                </button>
                <button
                    type="button"
                    className="card p-4 text-left hover:border-accent"
                    onClick={onFinished}
                >
                    <span className="font-semibold">{t('journey.counterparty.outcome.done')}</span>
                    <p className="mt-1 text-sm text-ink-muted">
                        {t('journey.counterparty.outcome.doneHint')}
                    </p>
                </button>
            </div>
        </div>
    );
}

/* ------------------------------------------------------------------ history */

function HistoryStep({
    t,
    state,
}: {
    readonly t: Translator['t'];
    readonly state: NewCounterparty;
}): ReactNode {
    if (!state.opened && state.written === undefined) {
        return <Notice tone="info">{t('journey.counterparty.history.none')}</Notice>;
    }
    return <HistoryPanel entityType={COUNTERPARTY_ENTITY_TYPE} entityId={state.shortCode} />;
}

/* -------------------------------------------------------------- the step list */

export function counterpartySteps(
    input: CounterpartyStepsInput,
): readonly JourneyStep<ReactNode>[] {
    const { t, server, state, partyId, pickLists, pickFailure, parents } = input;
    const identityIssues = identifierProblems(state);
    const contactIssues = contactProblems(state);
    const ready = state.hasIdentity && identityIssues.length === 0 && contactIssues.length === 0;

    const confirm = async (): Promise<void> => {
        state.clearRefusal();
        const outcome = await server.writeComposite(
            compositeRequest(state, partyId, newRecordIntent()),
        );
        if (!outcome.success || outcome.counterparty === undefined) {
            const refusal = refusalOf(
                'refdata.v1.ops.put_counterparty_composite',
                'review',
                outcome,
            );
            state.recordRefusal(refusal);
            throw new Error(refusalText(t, refusal));
        }
        const centres = await server.writeBusinessCentres(
            outcome.counterparty.id,
            state.businessCentreCodes,
            newRecordIntent(),
        );
        if (!centres.success) {
            const refusal = refusalOf(
                'refdata.v1.counterparty_business_centres.put_many',
                'identity',
                centres,
            );
            state.recordRefusal(refusal);
            throw new Error(refusalText(t, refusal));
        }
        if (partyId !== '') {
            const visibility = await server.writeVisibility(
                partyId,
                outcome.counterparty.id,
                newRecordIntent(),
            );
            if (!visibility.success) {
                const refusal = refusalOf(
                    'refdata.v1.party_counterparties.put',
                    'identifiers',
                    visibility,
                );
                state.recordRefusal(refusal);
                throw new Error(refusalText(t, refusal));
            }
        }
        state.recordWritten(outcome.counterparty);
    };

    return [
        {
            id: 'list',
            title: t('journey.counterparty.list.title'),
            lead: t('journey.counterparty.list.lead'),
            body: (
                <CounterpartyList
                    t={t}
                    server={server}
                    onStart={() => {
                        state.startNew();
                        input.onMove(counterpartyStepIndex('identity'));
                    }}
                    onOpen={(row, children) => {
                        state.open(row, children);
                        input.onMove(counterpartyStepIndex('identity'));
                    }}
                />
            ),
        },
        {
            id: 'identity',
            title: t('journey.counterparty.identity.title'),
            lead: t('journey.counterparty.identity.lead'),
            body: (
                <IdentityStep
                    t={t}
                    state={state}
                    pickLists={pickLists}
                    pickFailure={pickFailure}
                    parents={parents}
                />
            ),
            next: { label: t('common.continue'), enabled: state.hasIdentity },
        },
        {
            id: 'identifiers',
            title: t('journey.counterparty.identifiers.title'),
            lead: t('journey.counterparty.identifiers.lead'),
            body: <IdentifiersStep t={t} server={server} state={state} pickLists={pickLists} />,
            next: { label: t('common.continue'), enabled: identityIssues.length === 0 },
        },
        {
            id: 'contacts',
            title: t('journey.counterparty.contacts.title'),
            lead: t('journey.counterparty.contacts.lead'),
            body: <ContactsStep t={t} state={state} pickLists={pickLists} />,
            next: { label: t('common.continue'), enabled: contactIssues.length === 0 },
        },
        {
            id: 'agreements',
            title: t('journey.counterparty.agreements.title'),
            lead: t('journey.counterparty.agreements.lead'),
            body: <AgreementsStep t={t} state={state} partyId={partyId} pickLists={pickLists} />,
            next: { label: t('common.continue'), enabled: true },
        },
        {
            id: 'review',
            title: t('journey.counterparty.review.title'),
            lead: t('journey.counterparty.review.lead'),
            body: <ReviewStep t={t} state={state} onMove={input.onMove} />,
            next: {
                label: t('journey.counterparty.review.confirm'),
                enabled: ready,
                run: confirm,
            },
            final: state.written !== undefined,
        },
        {
            id: 'outcome',
            title: t('journey.counterparty.outcome.title'),
            lead: t('journey.counterparty.outcome.lead'),
            final: true,
            body: (
                <OutcomeStep
                    t={t}
                    state={state}
                    onMove={input.onMove}
                    onAnother={input.onAnother}
                    onFinished={input.onFinished}
                />
            ),
        },
        {
            id: 'history',
            title: t('journey.counterparty.history.title'),
            lead: t('journey.counterparty.history.lead'),
            final: true,
            body: <HistoryStep t={t} state={state} />,
            next: {
                label: t('journey.counterparty.outcome.done'),
                enabled: true,
                run: input.onFinished,
            },
        },
    ];
}

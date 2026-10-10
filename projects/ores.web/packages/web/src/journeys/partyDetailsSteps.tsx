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
 * The steps a party correction runs.
 *
 * The person finds a party, corrects its own record, its identifiers and its
 * contacts, sets the countries and currencies it works in, sets the
 * counterparties it sees and the type of each business unit, reads the one
 * review of every difference, and confirms. The confirm writes the composite
 * first, because a child write bumps the party's version and the party states
 * the version it read, and then the rest, in the order the plan lists them. A
 * history closes the journey, and a refusal keeps what the person changed and
 * names what was left unchanged.
 *
 * The steps are data, which is why this is a function and not a component.
 */

import { useEffect, useState, type ReactNode } from 'react';
import { Button, Field, Input, Notice, Select, Tag } from '../ui/Primitives.js';
import { HistoryPanel } from '../refdata/HistoryPanel.js';
import { Picker, optionsOf } from './pickers.js';
import {
    PARTY_ENTITY_TYPE,
    cardinalityProblems,
    levelBreach,
    typesByLevel,
    writePlan,
    type ContactField,
    type Membership,
    type PartyDetails,
    type PartyRefusal,
} from './partyDetailsState.js';
import type { JourneyStep, StepId } from './runtime.js';
import type {
    PartyDetailsServer,
    PartyPickLists,
    PartySets,
    PartyWriteOutcome,
} from './partyDetailsServer.js';
import type { Party } from '@ores/wire-protocol/generated/refdata/domain/party';
import type { Translator } from '../i18n/translate.js';

/** The steps in the order the rail draws them, and the index each one sits at. */
export const PARTY_STEP_IDS = [
    'list',
    'overview',
    'identifiers',
    'contacts',
    'memberships',
    'structure',
    'review',
    'outcome',
    'history',
] as const;

export function partyStepIndex(id: StepId): number {
    const index = PARTY_STEP_IDS.indexOf(id as (typeof PARTY_STEP_IDS)[number]);
    return index < 0 ? 0 : index;
}

export interface PartyStepsInput {
    readonly t: Translator['t'];
    readonly server: PartyDetailsServer;
    readonly state: PartyDetails;
    /** Every list a picker draws from, once the journey has read them. */
    readonly pickLists: PartyPickLists | undefined;
    /** Why the pick lists are absent, stated on the steps that need them. */
    readonly pickFailure: string | undefined;
    /** The reasons a record may be amended for. */
    readonly reasons: readonly {
        readonly code: string;
        readonly description: string;
        readonly requiresCommentary: boolean;
    }[];
    readonly onMove: (index: number) => void;
    readonly onFinished: () => void;
}

/** What was and was not written before the refusal, stated so a partial write is not a surprise. */
function writtenText(t: Translator['t'], refusal: PartyRefusal): string {
    return refusal.written.length === 0
        ? t('journey.partyDetails.refusal.nothingWritten')
        : t('journey.partyDetails.refusal.partial', { calls: refusal.written.join(', ') });
}

function refusalText(t: Translator['t'], refusal: PartyRefusal): string {
    const fields = refusal.fields
        .map((failure) => `${failure.field}: ${failure.message}`)
        .join('; ');
    return [
        t('journey.partyDetails.refusal.heading'),
        refusal.subject,
        refusal.message,
        fields,
        writtenText(t, refusal),
        t('journey.partyDetails.refusal.kept'),
    ]
        .filter((part) => part !== '')
        .join(' ');
}

function refusalOf(
    subject: string,
    step: string,
    outcome: PartyWriteOutcome,
    written: readonly string[],
): PartyRefusal {
    return {
        written,
        step,
        subject,
        code: outcome.code,
        message: outcome.message,
        fields: outcome.fields,
    };
}

/** Stands above every step once a party is open, so the record stays in view. */
export function partyHeader(t: Translator['t'], state: PartyDetails): ReactNode | undefined {
    if (state.opened === undefined) {
        return undefined;
    }
    return (
        <div className="flex flex-wrap items-baseline justify-between gap-3 text-sm">
            <span className="truncate font-medium">{state.fields.fullName}</span>
            <span className="flex items-center gap-2">
                <span className="font-mono text-xs text-ink-faint">
                    {state.fields.shortCode === ''
                        ? t('journey.partyDetails.noShortCode')
                        : state.fields.shortCode}
                </span>
                <Tag tone="neutral">{state.fields.status}</Tag>
                <span className="text-xs text-ink-faint">v{state.opened.version}</span>
            </span>
        </div>
    );
}

/* ------------------------------------------------------------------ landing */

function PartyList({
    t,
    server,
    onOpen,
}: {
    readonly t: Translator['t'];
    readonly server: PartyDetailsServer;
    readonly onOpen: (party: Party, sets: PartySets) => void;
}): ReactNode {
    const [query, setQuery] = useState('');
    const [rows, setRows] = useState<readonly Party[]>();
    const [total, setTotal] = useState(0);
    const [failure, setFailure] = useState<string>();
    const [opening, setOpening] = useState<string>();

    useEffect(() => {
        let cancelled = false;
        const load = async (): Promise<void> => {
            try {
                const page = await server.listParties({ offset: 0, limit: 50, search: query });
                if (!cancelled) {
                    setRows(page.rows);
                    setTotal(page.total);
                    setFailure(undefined);
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
    }, [server, query]);

    const open = async (party: Party): Promise<void> => {
        setOpening(party.id);
        try {
            onOpen(party, await server.setsOf(party.id));
        } catch (error) {
            setFailure(error instanceof Error ? error.message : String(error));
        } finally {
            setOpening(undefined);
        }
    };

    return (
        <div className="space-y-5">
            <Input
                value={query}
                placeholder={t('journey.partyDetails.list.search')}
                onChange={(event) => setQuery(event.target.value)}
            />
            {failure !== undefined && (
                <Notice tone="error">
                    {t('journey.partyDetails.list.readFailed', { message: failure })}
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
                                'version',
                                'modifiedBy',
                            ].map((column) => (
                                <th key={column} className="px-3 py-2 font-medium">
                                    {t(`journey.partyDetails.list.columns.${column}`)}
                                </th>
                            ))}
                        </tr>
                    </thead>
                    <tbody>
                        {rows?.length === 0 && (
                            <tr>
                                <td colSpan={7} className="px-3 py-3 text-ink-muted">
                                    {t('journey.partyDetails.list.empty')}
                                </td>
                            </tr>
                        )}
                        {rows?.map((row) => (
                            <tr
                                key={row.id}
                                aria-busy={opening === row.id}
                                className="cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover"
                                onClick={() => void open(row)}
                            >
                                <td className="px-3 py-2 font-mono text-xs">{row.short_code}</td>
                                <td className="px-3 py-2">{row.full_name}</td>
                                <td className="px-3 py-2">{row.party_type}</td>
                                <td className="px-3 py-2">
                                    <Tag tone={row.status === 'Active' ? 'up' : 'muted'}>
                                        {row.status}
                                    </Tag>
                                </td>
                                <td className="px-3 py-2 text-xs">{row.business_center_code}</td>
                                <td className="px-3 py-2 text-xs">{row.version}</td>
                                <td className="px-3 py-2 text-xs text-ink-muted">
                                    {row.modified_by}
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>
            {rows !== undefined && (
                <p className="text-xs text-ink-faint">
                    {t('journey.partyDetails.list.total', {
                        shown: String(rows.length),
                        total: String(total),
                    })}
                </p>
            )}
        </div>
    );
}

/* ----------------------------------------------------------------- overview */

function OverviewStep({
    t,
    state,
    pickLists,
    pickFailure,
}: {
    readonly t: Translator['t'];
    readonly state: PartyDetails;
    readonly pickLists: PartyPickLists | undefined;
    readonly pickFailure: string | undefined;
}): ReactNode {
    const opened = state.opened;
    if (opened === undefined) {
        return <Notice tone="info">{t('journey.partyDetails.none')}</Notice>;
    }
    return (
        <div className="space-y-5">
            {pickFailure !== undefined && (
                <Notice tone="warn">
                    {t('journey.partyDetails.pickFailed', { message: pickFailure })}
                </Notice>
            )}
            <Notice tone="info">{t('journey.partyDetails.overview.correction')}</Notice>
            <div className="grid gap-4 md:grid-cols-2">
                <Field label={t('journey.partyDetails.overview.shortCode')}>
                    <Input
                        value={state.fields.shortCode}
                        onChange={(event) => state.setField('shortCode', event.target.value)}
                    />
                </Field>
                <Field label={t('journey.partyDetails.overview.fullName')}>
                    <Input
                        value={state.fields.fullName}
                        onChange={(event) => state.setField('fullName', event.target.value)}
                    />
                </Field>
                <Field label={t('journey.partyDetails.overview.transliteratedName')}>
                    <Input
                        value={state.fields.transliteratedName}
                        onChange={(event) =>
                            state.setField('transliteratedName', event.target.value)
                        }
                    />
                </Field>
                <Picker
                    label={t('journey.partyDetails.overview.partyType')}
                    value={state.fields.partyType}
                    options={optionsOf(pickLists?.partyTypes ?? [])}
                    onChange={(value) => state.setField('partyType', value)}
                />
                <Picker
                    label={t('journey.partyDetails.overview.status')}
                    value={state.fields.status}
                    options={optionsOf(pickLists?.partyStatuses ?? [])}
                    onChange={(value) => state.setField('status', value)}
                />
                <Picker
                    label={t('journey.partyDetails.overview.businessCenter')}
                    value={state.fields.businessCenterCode}
                    options={(pickLists?.businessCentres ?? []).map((centre) => ({
                        value: centre.code,
                        label:
                            centre.description === ''
                                ? centre.code
                                : `${centre.code} ${centre.description}`,
                    }))}
                    onChange={(value) => state.setField('businessCenterCode', value)}
                />
                <Field label={t('journey.partyDetails.overview.parent')}>
                    <Input
                        value={state.fields.parentPartyId}
                        placeholder={t('journey.partyDetails.overview.noParent')}
                        onChange={(event) => state.setField('parentPartyId', event.target.value)}
                    />
                </Field>
                <label className="flex items-center gap-2 self-end pb-2 text-sm">
                    <input
                        type="checkbox"
                        checked={state.fields.isRegistrationDefault}
                        onChange={(event) => state.setDefault(event.target.checked)}
                    />
                    {t('journey.partyDetails.overview.registrationDefault')}
                </label>
            </div>
            <dl className="grid gap-2 text-sm md:grid-cols-2">
                <div>
                    <dt className="text-xs text-ink-faint">
                        {t('journey.partyDetails.overview.codename')}
                    </dt>
                    <dd className="font-mono">{opened.codename}</dd>
                </div>
                <div>
                    <dt className="text-xs text-ink-faint">
                        {t('journey.partyDetails.overview.category')}
                    </dt>
                    <dd>{opened.party_category}</dd>
                </div>
            </dl>
            <p className="text-xs text-ink-faint">{t('journey.partyDetails.overview.readOnly')}</p>
        </div>
    );
}

/* -------------------------------------------------------------- identifiers */

function IdentifiersStep({
    t,
    state,
    pickLists,
}: {
    readonly t: Translator['t'];
    readonly state: PartyDetails;
    readonly pickLists: PartyPickLists | undefined;
}): ReactNode {
    const [scheme, setScheme] = useState('');
    const [value, setValue] = useState('');
    const schemes = pickLists?.identifierSchemes ?? [];
    const limitOf = (code: string): number | null =>
        schemes.find((candidate) => candidate.code === code)?.max_cardinality ?? null;
    const problems = cardinalityProblems(state, limitOf);
    return (
        <div className="space-y-5">
            <div className="grid gap-3 md:grid-cols-2">
                <Notice tone="info">{t('journey.partyDetails.identifiers.nameChange')}</Notice>
                <Notice tone="info">{t('journey.partyDetails.identifiers.idChange')}</Notice>
            </div>
            <ul className="space-y-3">
                {state.identifiers.length === 0 && (
                    <li className="text-sm text-ink-muted">
                        {t('journey.partyDetails.identifiers.none')}
                    </li>
                )}
                {state.identifiers.map((row) => (
                    <li key={row.key} className="card flex flex-wrap items-end gap-3 p-3">
                        <Tag tone="accent">{row.scheme}</Tag>
                        <div className="min-w-48 flex-1">
                            <Field label={t('journey.partyDetails.identifiers.value')}>
                                <Input
                                    value={row.value}
                                    disabled={row.retired}
                                    onChange={(event) =>
                                        state.setIdentifier(row.key, event.target.value)
                                    }
                                />
                            </Field>
                        </div>
                        <div className="min-w-48 flex-1">
                            <Field label={t('journey.partyDetails.identifiers.description')}>
                                <Input
                                    value={row.description}
                                    disabled={row.retired}
                                    onChange={(event) =>
                                        state.setIdentifierDescription(row.key, event.target.value)
                                    }
                                />
                            </Field>
                        </div>
                        <Button
                            size="sm"
                            variant="ghost"
                            onClick={() => state.retireIdentifier(row.key, !row.retired)}
                        >
                            {row.retired
                                ? t('journey.partyDetails.identifiers.undo')
                                : t('journey.partyDetails.identifiers.retire')}
                        </Button>
                    </li>
                ))}
            </ul>
            <div className="card flex flex-wrap items-end gap-3 p-3">
                <div className="min-w-48">
                    <Picker
                        label={t('journey.partyDetails.identifiers.scheme')}
                        value={scheme}
                        options={optionsOf(schemes)}
                        empty={t('journey.partyDetails.identifiers.choose')}
                        onChange={setScheme}
                    />
                </div>
                <div className="min-w-48 flex-1">
                    <Field
                        label={t('journey.partyDetails.identifiers.value')}
                        {...(scheme !== '' && limitOf(scheme) !== null
                            ? {
                                  hint: t('journey.partyDetails.identifiers.cardinality', {
                                      scheme,
                                      max: String(limitOf(scheme)),
                                  }),
                              }
                            : {})}
                    >
                        <Input value={value} onChange={(event) => setValue(event.target.value)} />
                    </Field>
                </div>
                <Button
                    variant="primary"
                    disabled={scheme === '' || value.trim() === ''}
                    onClick={() => {
                        state.addIdentifier(scheme, value.trim(), '');
                        setValue('');
                    }}
                >
                    {t('journey.partyDetails.identifiers.add')}
                </Button>
            </div>
            {problems.length > 0 && (
                <Notice tone="warn">
                    <ul>
                        {problems.map((problem) => (
                            <li key={problem}>{problem}</li>
                        ))}
                    </ul>
                </Notice>
            )}
        </div>
    );
}

/* ----------------------------------------------------------------- contacts */

const CONTACT_FIELD_KEYS: readonly (readonly [ContactField, string])[] = [
    ['streetLine1', 'street1'],
    ['streetLine2', 'street2'],
    ['city', 'city'],
    ['state', 'state'],
    ['countryCode', 'country'],
    ['postalCode', 'postal'],
    ['phone', 'phone'],
    ['email', 'email'],
    ['webPage', 'web'],
];

function ContactsStep({
    t,
    state,
    pickLists,
}: {
    readonly t: Translator['t'];
    readonly state: PartyDetails;
    readonly pickLists: PartyPickLists | undefined;
}): ReactNode {
    const types = pickLists?.contactTypes ?? [];
    const missing = types.filter(
        (type) => !state.contacts.some((row) => row.contactType === type.code),
    );
    return (
        <div className="space-y-5">
            {state.contacts.length === 0 && (
                <p className="text-sm text-ink-muted">{t('journey.partyDetails.contacts.none')}</p>
            )}
            {state.contacts.map((row) => (
                <section key={row.contactType} className="card space-y-3 p-4">
                    <div className="flex items-center justify-between">
                        <h3 className="font-semibold">{row.contactType}</h3>
                        {row.isPrimary ? (
                            <Tag tone="accent">{t('journey.partyDetails.contacts.primary')}</Tag>
                        ) : (
                            <Button
                                size="sm"
                                variant="ghost"
                                onClick={() => state.markPrimary(row.contactType)}
                            >
                                {t('journey.partyDetails.contacts.markPrimary')}
                            </Button>
                        )}
                    </div>
                    <div className="grid gap-3 md:grid-cols-3">
                        {CONTACT_FIELD_KEYS.map(([field, key]) => (
                            <Field key={field} label={t(`journey.partyDetails.contacts.${key}`)}>
                                <Input
                                    value={row[field]}
                                    onChange={(event) =>
                                        state.setContact(row.contactType, field, event.target.value)
                                    }
                                />
                            </Field>
                        ))}
                    </div>
                </section>
            ))}
            {missing.length > 0 && (
                <div className="flex flex-wrap gap-2">
                    {missing.map((type) => (
                        <Button
                            key={type.code}
                            size="sm"
                            onClick={() => state.addContact(type.code)}
                        >
                            {t('journey.partyDetails.contacts.add', { type: type.name })}
                        </Button>
                    ))}
                </div>
            )}
        </div>
    );
}

/* -------------------------------------------------------------- memberships */

function MembershipList({
    t,
    title,
    rows,
    labelOf,
    options,
    onToggle,
}: {
    readonly t: Translator['t'];
    readonly title: string;
    readonly rows: readonly Membership[];
    readonly labelOf: (code: string) => string;
    readonly options: readonly { readonly value: string; readonly label: string }[];
    readonly onToggle: (code: string) => void;
}): ReactNode {
    const [adding, setAdding] = useState('');
    const free = options.filter((option) => !rows.some((row) => row.code === option.value));
    return (
        <section className="space-y-3">
            <h3 className="font-semibold">{title}</h3>
            <ul className="space-y-2">
                {rows.length === 0 && (
                    <li className="text-sm text-ink-muted">
                        {t('journey.partyDetails.memberships.none')}
                    </li>
                )}
                {rows.map((row) => (
                    <li key={row.code} className="card flex items-center justify-between p-3">
                        <span className="text-sm">
                            {labelOf(row.code)}{' '}
                            <Tag tone={row.wanted ? 'up' : 'muted'}>
                                {row.wanted
                                    ? t('journey.partyDetails.memberships.open')
                                    : t('journey.partyDetails.memberships.closing')}
                            </Tag>
                        </span>
                        <Button size="sm" variant="ghost" onClick={() => onToggle(row.code)}>
                            {row.wanted
                                ? t('journey.partyDetails.memberships.close')
                                : t('journey.partyDetails.memberships.reopen')}
                        </Button>
                    </li>
                ))}
            </ul>
            <div className="flex gap-2">
                <Select value={adding} onChange={(event) => setAdding(event.target.value)}>
                    <option value="">{t('journey.partyDetails.memberships.choose')}</option>
                    {free.map((option) => (
                        <option key={option.value} value={option.value}>
                            {option.label}
                        </option>
                    ))}
                </Select>
                <Button
                    disabled={adding === ''}
                    onClick={() => {
                        onToggle(adding);
                        setAdding('');
                    }}
                >
                    {t('journey.partyDetails.memberships.link')}
                </Button>
            </div>
        </section>
    );
}

function MembershipsStep({
    t,
    state,
    pickLists,
}: {
    readonly t: Translator['t'];
    readonly state: PartyDetails;
    readonly pickLists: PartyPickLists | undefined;
}): ReactNode {
    const countries = pickLists?.countries ?? [];
    const currencies = pickLists?.currencies ?? [];
    return (
        <div className="space-y-6">
            <Notice tone="info">{t('journey.partyDetails.memberships.closeNote')}</Notice>
            <MembershipList
                t={t}
                title={t('journey.partyDetails.memberships.countries')}
                rows={state.countries}
                labelOf={(code) =>
                    countries.find((country) => country.alpha2_code === code)?.name ?? code
                }
                options={countries.map((country) => ({
                    value: country.alpha2_code,
                    label: country.name,
                }))}
                onToggle={(code) => state.toggleMembership('countries', code)}
            />
            <MembershipList
                t={t}
                title={t('journey.partyDetails.memberships.currencies')}
                rows={state.currencies}
                labelOf={(code) =>
                    currencies.find((currency) => currency.iso_code === code)?.name ?? code
                }
                options={currencies.map((currency) => ({
                    value: currency.iso_code,
                    label: `${currency.iso_code} ${currency.name}`,
                }))}
                onToggle={(code) => state.toggleMembership('currencies', code)}
            />
            <p className="text-xs text-ink-faint">
                {t('journey.partyDetails.memberships.noHistory')}
            </p>
        </div>
    );
}

/* ---------------------------------------------------------------- structure */

function StructureStep({
    t,
    state,
    pickLists,
}: {
    readonly t: Translator['t'];
    readonly state: PartyDetails;
    readonly pickLists: PartyPickLists | undefined;
}): ReactNode {
    const counterparties = pickLists?.counterparties ?? [];
    const types = typesByLevel(pickLists?.businessUnitTypes ?? []);
    return (
        <div className="space-y-6">
            <MembershipList
                t={t}
                title={t('journey.partyDetails.structure.counterparties')}
                rows={state.counterparties}
                labelOf={(code) =>
                    counterparties.find((counterparty) => counterparty.id === code)?.full_name ??
                    code
                }
                options={counterparties.map((counterparty) => ({
                    value: counterparty.id,
                    label: counterparty.full_name,
                }))}
                onToggle={(code) => state.toggleMembership('counterparties', code)}
            />
            <section className="space-y-3">
                <h3 className="font-semibold">{t('journey.partyDetails.structure.units')}</h3>
                {state.units.length === 0 && (
                    <p className="text-sm text-ink-muted">
                        {t('journey.partyDetails.structure.noUnits')}
                    </p>
                )}
                <ul className="space-y-2">
                    {state.units.map((row) => {
                        const breach = levelBreach(row, state.units, types);
                        return (
                            <li key={row.unit.id} className="card space-y-2 p-3">
                                <div className="flex flex-wrap items-center justify-between gap-3">
                                    <span className="text-sm">
                                        <span className="font-mono text-xs">
                                            {row.unit.unit_code}
                                        </span>{' '}
                                        {row.unit.unit_name}
                                    </span>
                                    <Select
                                        value={row.unitTypeId ?? ''}
                                        onChange={(event) =>
                                            state.setUnitType(
                                                row.unit.id,
                                                event.target.value === ''
                                                    ? null
                                                    : event.target.value,
                                            )
                                        }
                                    >
                                        <option value="">
                                            {t('journey.partyDetails.structure.noType')}
                                        </option>
                                        {types.map((type) => (
                                            <option key={type.id} value={type.id}>
                                                {type.name} ({type.level})
                                            </option>
                                        ))}
                                    </Select>
                                </div>
                                {breach !== undefined && (
                                    <Notice tone="warn">
                                        {t('journey.partyDetails.structure.levelBreach', {
                                            unit: row.unit.unit_code,
                                            level: String(breach.level),
                                            parent: breach.parent.unit.unit_code,
                                            parentLevel: String(breach.parentLevel),
                                        })}
                                    </Notice>
                                )}
                            </li>
                        );
                    })}
                </ul>
            </section>
        </div>
    );
}

/* ------------------------------------------------------------------- review */

function ReviewStep({
    t,
    state,
    reasons,
    onMove,
}: {
    readonly t: Translator['t'];
    readonly state: PartyDetails;
    readonly reasons: PartyStepsInput['reasons'];
    readonly onMove: (index: number) => void;
}): ReactNode {
    const chosen = reasons.find((reason) => reason.code === state.reasonCode);
    return (
        <div className="space-y-5">
            {state.refusal !== undefined && (
                <Notice tone="error">
                    <p className="font-semibold">{t('journey.partyDetails.refusal.heading')}</p>
                    <p className="text-sm">{state.refusal.subject}</p>
                    <p className="text-sm">{state.refusal.message}</p>
                    <ul className="text-sm">
                        {state.refusal.fields.map((failure) => (
                            <li key={`${failure.field}:${failure.code}`}>
                                {failure.field}: {failure.message}
                            </li>
                        ))}
                    </ul>
                    <p className="mt-2 text-sm">{writtenText(t, state.refusal)}</p>
                    <p className="text-sm">{t('journey.partyDetails.refusal.kept')}</p>
                    <Button
                        size="sm"
                        className="mt-2"
                        onClick={() => onMove(partyStepIndex(state.refusal?.step ?? 'overview'))}
                    >
                        {t('journey.partyDetails.refusal.walkBack')}
                    </Button>
                </Notice>
            )}
            {state.changes.length === 0 ? (
                <Notice tone="info">{t('journey.partyDetails.review.nothing')}</Notice>
            ) : (
                <div className="overflow-x-auto rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                {['what', 'before', 'after', 'operation'].map((column) => (
                                    <th key={column} className="px-3 py-2 font-medium">
                                        {t(`journey.partyDetails.review.columns.${column}`)}
                                    </th>
                                ))}
                            </tr>
                        </thead>
                        <tbody>
                            {state.changes.map((change) => (
                                <tr
                                    key={change.id}
                                    className="border-b border-line-subtle last:border-b-0"
                                >
                                    <td className="px-3 py-2">{change.what}</td>
                                    <td className="px-3 py-2 text-ink-muted">{change.before}</td>
                                    <td className="px-3 py-2">{change.after}</td>
                                    <td className="px-3 py-2 font-mono text-xs text-ink-faint">
                                        {change.operation}
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            )}
            <div className="grid gap-4 md:grid-cols-2">
                <Field label={t('journey.partyDetails.review.reason')}>
                    <Select
                        value={state.reasonCode}
                        onChange={(event) => state.setReason(event.target.value)}
                    >
                        {reasons.length === 0 && (
                            <option value={state.reasonCode}>{state.reasonCode}</option>
                        )}
                        {reasons.map((reason) => (
                            <option key={reason.code} value={reason.code}>
                                {reason.description}
                            </option>
                        ))}
                    </Select>
                </Field>
                <Field
                    label={t('journey.partyDetails.review.commentary')}
                    {...(chosen?.requiresCommentary === true
                        ? { hint: t('journey.partyDetails.review.commentaryRequired') }
                        : {})}
                >
                    <Input
                        value={state.commentary}
                        onChange={(event) => state.setCommentary(event.target.value)}
                    />
                </Field>
            </div>
        </div>
    );
}

/* ------------------------------------------------------------------ outcome */

function OutcomeStep({
    t,
    state,
    onMove,
    onFinished,
}: {
    readonly t: Translator['t'];
    readonly state: PartyDetails;
    readonly onMove: (index: number) => void;
    readonly onFinished: () => void;
}): ReactNode {
    if (state.written === undefined) {
        return <Notice tone="info">{t('journey.partyDetails.none')}</Notice>;
    }
    return (
        <div className="space-y-4">
            <Notice tone="success">
                {t('journey.partyDetails.outcome.written', {
                    name: state.written.full_name,
                    version: String(state.written.version),
                })}
            </Notice>
            <div className="grid gap-3 md:grid-cols-2">
                <button
                    type="button"
                    className="card p-4 text-left hover:border-accent"
                    onClick={() => onMove(partyStepIndex('history'))}
                >
                    <span className="font-semibold">
                        {t('journey.partyDetails.outcome.history')}
                    </span>
                </button>
                <button
                    type="button"
                    className="card p-4 text-left hover:border-accent"
                    onClick={onFinished}
                >
                    <span className="font-semibold">{t('journey.partyDetails.outcome.done')}</span>
                </button>
            </div>
        </div>
    );
}

/* ------------------------------------------------------------------ history */

function HistoryStep({
    t,
    server,
    state,
    onMove,
}: {
    readonly t: Translator['t'];
    readonly server: PartyDetailsServer;
    readonly state: PartyDetails;
    readonly onMove: (index: number) => void;
}): ReactNode {
    const [failure, setFailure] = useState<string>();
    const opened = state.opened;
    if (opened === undefined) {
        return <Notice tone="info">{t('journey.partyDetails.history.none')}</Notice>;
    }
    return (
        <div className="space-y-4">
            {failure !== undefined && <Notice tone="error">{failure}</Notice>}
            <p className="text-xs text-ink-faint">{t('journey.partyDetails.history.revertNote')}</p>
            <HistoryPanel
                entityType={PARTY_ENTITY_TYPE}
                entityId={(state.written ?? opened).short_code}
                onRevert={(version) => {
                    void (async () => {
                        try {
                            const current = state.written ?? opened;
                            if (state.written !== undefined) {
                                state.open(current, await server.setsOf(current.id));
                            }
                            const as = await server.compositeAsOf(current.id, version.version);
                            state.revertFields(as.party);
                            setFailure(undefined);
                            onMove(partyStepIndex('review'));
                        } catch (error) {
                            setFailure(error instanceof Error ? error.message : String(error));
                        }
                    })();
                }}
            />
        </div>
    );
}

/* -------------------------------------------------------------- the step list */

export function partySteps(input: PartyStepsInput): readonly JourneyStep<ReactNode>[] {
    const { t, server, state, pickLists, pickFailure, reasons } = input;
    const schemes = pickLists?.identifierSchemes ?? [];
    const problems = cardinalityProblems(
        state,
        (code) => schemes.find((scheme) => scheme.code === code)?.max_cardinality ?? null,
    );
    const types = pickLists?.businessUnitTypes ?? [];
    const breached = state.units.some((row) => levelBreach(row, state.units, types) !== undefined);
    const chosen = reasons.find((reason) => reason.code === state.reasonCode);
    const commentaryMissing = chosen?.requiresCommentary === true && state.commentary.trim() === '';
    const ready =
        state.opened !== undefined &&
        state.changes.length > 0 &&
        problems.length === 0 &&
        !breached &&
        !commentaryMissing &&
        state.fields.shortCode.trim() !== '' &&
        state.fields.fullName.trim() !== '';

    const wrote: string[] = [];

    function fail(subject: string, step: string, outcome: PartyWriteOutcome): never {
        const refusal = refusalOf(subject, step, outcome, [...wrote]);
        state.recordRefusal(refusal);
        throw new Error(refusalText(t, refusal));
    }

    const confirm = async (): Promise<void> => {
        const plan = writePlan(state);
        if (plan === undefined) {
            return;
        }
        state.clearRefusal();
        wrote.length = 0;
        const intent = plan.composite.intent;
        const partyId = plan.composite.party.id;
        const composite = await server.writeComposite(plan.composite);
        if (!composite.success || composite.party === undefined) {
            fail('refdata.v1.ops.put_party_composite', 'overview', composite);
        }
        wrote.push('refdata.v1.ops.put_party_composite');
        for (const retire of plan.retires) {
            const outcome = await server.retireIdentifier(
                partyId,
                retire.value,
                retire.version,
                intent,
            );
            if (!outcome.success) {
                fail('refdata.v1.party_identifiers.delete', 'identifiers', outcome);
            }
            wrote.push(`party_identifiers.delete ${retire.value}`);
        }
        for (const link of plan.links) {
            const outcome = link.open
                ? await server.linkMembership(partyId, link.set, link.code, intent)
                : await server.closeMembership(partyId, link.set, link.code, intent);
            if (!outcome.success) {
                fail(
                    `refdata.v1.party_${link.set}.${link.open ? 'put' : 'delete'}`,
                    link.set === 'counterparties' ? 'structure' : 'memberships',
                    outcome,
                );
            }
            wrote.push(`party_${link.set}.${link.open ? 'put' : 'delete'} ${link.code}`);
        }
        for (const row of plan.units) {
            const outcome = await server.reclassifyUnit(row.unit, row.unitTypeId, intent);
            if (!outcome.success) {
                fail('refdata.v1.business_units.put', 'structure', outcome);
            }
            wrote.push(`business_units.put ${row.unit.unit_code}`);
        }
        state.recordWritten(composite.party);
    };

    return [
        {
            id: 'list',
            title: t('journey.partyDetails.list.title'),
            lead: t('journey.partyDetails.list.lead'),
            body: (
                <PartyList
                    t={t}
                    server={server}
                    onOpen={(party, sets) => {
                        state.open(party, sets);
                        input.onMove(partyStepIndex('overview'));
                    }}
                />
            ),
        },
        {
            id: 'overview',
            title: t('journey.partyDetails.overview.title'),
            lead: t('journey.partyDetails.overview.lead'),
            body: (
                <OverviewStep t={t} state={state} pickLists={pickLists} pickFailure={pickFailure} />
            ),
            next: {
                label: t('common.continue'),
                enabled:
                    state.opened !== undefined &&
                    state.fields.shortCode.trim() !== '' &&
                    state.fields.fullName.trim() !== '',
            },
        },
        {
            id: 'identifiers',
            title: t('journey.partyDetails.identifiers.title'),
            lead: t('journey.partyDetails.identifiers.lead'),
            body: <IdentifiersStep t={t} state={state} pickLists={pickLists} />,
            next: { label: t('common.continue'), enabled: problems.length === 0 },
        },
        {
            id: 'contacts',
            title: t('journey.partyDetails.contacts.title'),
            lead: t('journey.partyDetails.contacts.lead'),
            body: <ContactsStep t={t} state={state} pickLists={pickLists} />,
            next: { label: t('common.continue'), enabled: true },
        },
        {
            id: 'memberships',
            title: t('journey.partyDetails.memberships.title'),
            lead: t('journey.partyDetails.memberships.lead'),
            body: <MembershipsStep t={t} state={state} pickLists={pickLists} />,
            next: { label: t('common.continue'), enabled: true },
        },
        {
            id: 'structure',
            title: t('journey.partyDetails.structure.title'),
            lead: t('journey.partyDetails.structure.lead'),
            body: <StructureStep t={t} state={state} pickLists={pickLists} />,
            next: { label: t('common.continue'), enabled: !breached },
        },
        {
            id: 'review',
            title: t('journey.partyDetails.review.title'),
            lead: t('journey.partyDetails.review.lead'),
            body: <ReviewStep t={t} state={state} reasons={reasons} onMove={input.onMove} />,
            next: {
                label: t('journey.partyDetails.review.confirm'),
                enabled: ready,
                run: confirm,
            },
            final: state.written !== undefined,
        },
        {
            id: 'outcome',
            title: t('journey.partyDetails.outcome.title'),
            lead: t('journey.partyDetails.outcome.lead'),
            final: true,
            body: (
                <OutcomeStep
                    t={t}
                    state={state}
                    onMove={input.onMove}
                    onFinished={input.onFinished}
                />
            ),
        },
        {
            id: 'history',
            title: t('journey.partyDetails.history.title'),
            lead: t('journey.partyDetails.history.lead'),
            body: <HistoryStep t={t} server={server} state={state} onMove={input.onMove} />,
            next: {
                label: t('journey.partyDetails.outcome.done'),
                enabled: true,
                run: input.onFinished,
            },
        },
    ];
}

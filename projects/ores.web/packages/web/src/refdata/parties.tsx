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

import { useState, type ReactNode } from 'react';
import { useParams } from 'react-router';
import type { HistoryVersion, TimelineEvent } from '@ores/wire-protocol/browser';
import { useQuery } from '@tanstack/react-query';
import type { RecordRow } from '../api/client.js';
import { api } from '../api/client.js';
import { useHolds } from '../access/holds.js';
import { actorPathFor } from '../access/PeoplePage.js';
import { useActorPictures, usePeopleDirectory } from '../access/PersonRef.js';
import { Timeline } from '../timeline/Timeline.js';
import { Button, Notice } from '../ui/Primitives.js';
import { useTranslation } from '../i18n/Provider.js';
import { Flag, FlaggedCode } from '../images/flags.js';
import { useAreaParts, useAreaRoot } from '../shell/AreaTrail.js';
import { PATHS } from '../shell/paths.js';
import { WorkflowMenu } from '../shell/WorkflowMenu.js';
import { imageUrl } from '../ui/Images.js';
import { mailtoLink, webLink } from '../ui/links.js';
import { RecordList, useRecordSource } from './RecordList.js';
import {
    ClassifiedValue,
    LinkPanel,
    PartsPanel,
    RecordDetails,
    RecordDialog,
    RecordGate,
    RecordHeader,
    RevertDialog,
    show,
    useRecordPermissions,
    useRecordTabs,
    useRecords,
    type FieldSpec,
} from './records.js';

const PARTIES = 'parties';
const IDENTIFIERS = 'party-identifiers';
const CONTACTS = 'party-contacts';
const UNITS = 'business-units';

/** The address of a party's page. */
export function partyPath(id: string): string {
    return `${PATHS.parties}/${encodeURIComponent(id)}`;
}

/** The party fields a person edits, in form order, with the names the history gives them. */
export const PARTY_FIELDS: readonly FieldSpec[] = [
    { field: 'short_code', history: 'Short Code', kind: { kind: 'text', max: 100 } },
    { field: 'full_name', history: 'Full Name', kind: { kind: 'text' } },
    {
        field: 'transliterated_name',
        history: 'Transliterated Name',
        kind: { kind: 'text' },
        optional: true,
    },
    { field: 'codename', history: 'Codename', kind: { kind: 'text', max: 100 }, fixed: true },
    {
        field: 'party_category',
        history: 'Party Category',
        kind: { kind: 'text', max: 100 },
        fixed: true,
    },
    {
        field: 'party_type',
        history: 'Party Type',
        kind: { kind: 'classification', list: 'party-type' },
    },
    { field: 'status', history: 'Status', kind: { kind: 'classification', list: 'party-status' } },
    {
        field: 'parent_party_id',
        history: 'Parent Party ID',
        kind: { kind: 'record', resource: PARTIES, value: 'id', label: 'full_name' },
        optional: true,
        pathOf: partyPath,
    },
    {
        field: 'business_center_code',
        history: 'Business Center Code',
        kind: { kind: 'record', resource: 'business-centres', value: 'code', label: 'name' },
        flag: 'businessCentre',
    },
    {
        field: 'image_id',
        history: 'Image ID',
        kind: { kind: 'image' },
        optional: true,
        labelKey: 'refdata.parties.logo',
    },
    {
        field: 'is_registration_default',
        history: 'Is Registration Default',
        kind: { kind: 'bool' },
    },
];

const IDENTIFIER_FIELDS: readonly FieldSpec[] = [
    {
        field: 'id_scheme',
        history: 'ID Scheme',
        kind: { kind: 'record', resource: 'party-id-schemes', value: 'code', label: 'name' },
    },
    { field: 'id_value', history: 'ID Value', kind: { kind: 'text', max: 200 } },
    { field: 'description', history: 'Description', kind: { kind: 'text' }, blank: true },
];

const CONTACT_FIELDS: readonly FieldSpec[] = [
    {
        field: 'contact_type',
        history: 'Contact Type',
        kind: { kind: 'classification', list: 'contact-type' },
    },
    { field: 'street_line_1', history: 'Street Line 1', kind: { kind: 'text' }, blank: true },
    { field: 'street_line_2', history: 'Street Line 2', kind: { kind: 'text' }, blank: true },
    { field: 'city', history: 'City', kind: { kind: 'text' }, blank: true },
    { field: 'state', history: 'State', kind: { kind: 'text' }, blank: true },
    { field: 'postal_code', history: 'Postal Code', kind: { kind: 'text', max: 20 }, blank: true },
    {
        field: 'country_code',
        history: 'Country Code',
        kind: { kind: 'text', max: 3 },
        blank: true,
        flag: 'country',
    },
    { field: 'phone', history: 'Phone', kind: { kind: 'text', max: 50 }, blank: true },
    { field: 'email', history: 'Email', kind: { kind: 'text', max: 254 }, blank: true },
    { field: 'web_page', history: 'Web Page', kind: { kind: 'text', max: 500 }, blank: true },
    { field: 'is_primary', history: 'Is Primary', kind: { kind: 'bool' } },
];

const UNIT_FIELDS: readonly FieldSpec[] = [
    { field: 'unit_name', history: 'Unit Name', kind: { kind: 'text' } },
    { field: 'unit_code', history: 'Unit Code', kind: { kind: 'text', max: 100 } },
    {
        field: 'business_centre_code',
        history: 'Business Centre Code',
        kind: { kind: 'record', resource: 'business-centres', value: 'code', label: 'name' },
        flag: 'businessCentre',
        blank: true,
    },
    {
        field: 'unit_type_id',
        history: 'Unit Type ID',
        kind: { kind: 'record', resource: 'business-unit-types', value: 'id', label: 'name' },
        optional: true,
    },
    { field: 'status', history: 'Status', kind: { kind: 'text', max: 50 } },
];

/** The fields a write needs that no form edits, carried over from the record read. */
const PARTY_KEPT = ['id'];

/** An address that opens in the person's mail or browser when it is one, and plain text when it is not. */
function Address({
    text,
    link,
}: {
    readonly text: string;
    readonly link: { readonly href?: string; readonly external?: boolean };
}): ReactNode {
    if (text === '') {
        return <span className="text-ink-faint">—</span>;
    }
    return link.href === undefined ? (
        <>{text}</>
    ) : (
        <a
            href={link.href}
            {...(link.external === true ? { target: '_blank', rel: 'noopener noreferrer' } : {})}
            className="underline decoration-line-strong underline-offset-2 hover:decoration-accent"
        >
            {text}
        </a>
    );
}

/**
 * The tenant's parties: one page at a time, searched and sorted on the server.
 * A party is made of several kinds of record, so a row opens its page. A new
 * party is provisioned by a workflow, listed in the Workflows menu, because
 * provisioning also publishes the party's data and links its accounts.
 */
export function PartiesPage(): ReactNode {
    const { t } = useTranslation();
    const source = { ...useRecordSource(PARTIES), mayAdd: false };
    return (
        <RecordList
            source={source}
            title={t('parties.title')}
            lead={t('parties.description')}
            crumbs={useAreaParts('refdata', t('parties.title'))}
            keyOf={(row) => show(row['id'])}
            pathOf={(row) => partyPath(show(row['id']))}
            actions={<WorkflowMenu on="parties" />}
            columns={[
                {
                    id: 'short_code',
                    header: t('refdata.fields.short_code'),
                    cell: (row) => show(row['short_code']),
                    mono: true,
                    sort: 'short_code',
                },
                {
                    id: 'full_name',
                    header: t('refdata.fields.full_name'),
                    cell: (row) => show(row['full_name']),
                    sort: 'full_name',
                },
                {
                    id: 'party_category',
                    header: t('refdata.fields.party_category'),
                    cell: (row) => show(row['party_category']),
                    sort: 'party_category',
                },
                {
                    id: 'party_type',
                    header: t('refdata.fields.party_type'),
                    cell: (row) => (
                        <ClassifiedValue list="party-type" code={show(row['party_type'])} />
                    ),
                    sort: 'party_type',
                },
                {
                    id: 'status',
                    header: t('refdata.fields.status'),
                    cell: (row) => (
                        <ClassifiedValue list="party-status" code={show(row['status'])} />
                    ),
                    sort: 'status',
                },
                {
                    id: 'business_center_code',
                    header: t('refdata.fields.business_center_code'),
                    cell: (row) => show(row['business_center_code']),
                    sort: 'business_center_code',
                    flag: {
                        source: 'businessCentre',
                        code: (row) => show(row['business_center_code']),
                    },
                },
            ]}
        />
    );
}

/**
 * One party, made of several kinds of record. Each aspect is a tab, and each
 * card or table on it is changed in a dialog. Opening a party reads only that
 * party; the tabs read what they show.
 */
export function PartyPage(): ReactNode {
    const { id } = useParams();
    return (
        <RecordGate resource={PARTIES} recordKey={id ?? ''} listPath={PATHS.parties}>
            {(row) => <PartyBody row={row} />}
        </RecordGate>
    );
}

/** The names a link panel offers, read when the tab that needs them is open and not before. */
function namesOf(
    rows: readonly RecordRow[] | undefined,
    value: string,
    label: string,
): readonly { readonly value: string; readonly label: string }[] {
    return (rows ?? []).map((row) => ({ value: show(row[value]), label: show(row[label]) }));
}

/** The countries and the currencies a party works in. */
function Memberships({ partyId }: { readonly partyId: string }): ReactNode {
    const { t } = useTranslation();
    const countries = useRecords('countries');
    const currencies = useRecords('currencies');
    return (
        <div className="space-y-4">
            <LinkPanel
                title={t('refdata.parties.countries')}
                junction="party-countries"
                parentField="party_id"
                parentValue={partyId}
                childField="country_alpha2_code"
                flag="country"
                choices={namesOf(countries.data, 'alpha2_code', 'name')}
            />
            <LinkPanel
                title={t('refdata.parties.currencies')}
                junction="party-currencies"
                parentField="party_id"
                parentValue={partyId}
                childField="currency_iso_code"
                flag="currency"
                choices={namesOf(currencies.data, 'iso_code', 'name')}
            />
        </div>
    );
}

/** The counterparties a party may trade with. */
function TradingCounterparties({ partyId }: { readonly partyId: string }): ReactNode {
    const { t } = useTranslation();
    const counterparties = useRecords('counterparties');
    return (
        <LinkPanel
            title={t('refdata.parties.counterparties')}
            junction="party-counterparties"
            parentField="party_id"
            parentValue={partyId}
            childField="counterparty_id"
            choices={namesOf(counterparties.data, 'id', 'full_name')}
        />
    );
}

/** The record a party's history entry came from, by name and not by identifier. */
const RECORD_KEYS: Readonly<Record<string, { readonly name: string; readonly field: string }>> = {
    'ores.refdata.party': { name: 'party', field: 'Short Code' },
    'ores.refdata.party_identifier': { name: 'identifier', field: 'ID Value' },
    'ores.refdata.party_contact_information': { name: 'contact', field: 'Contact Type' },
    'ores.refdata.business_unit': { name: 'unit', field: 'Unit Name' },
    'ores.refdata.party_country': { name: 'country', field: 'Country' },
    'ores.refdata.party_currency': { name: 'currency', field: 'Currency' },
    'ores.refdata.party_counterparty': { name: 'counterparty', field: 'Counterparty' },
};

/** A timeline entry as the version the revert dialog names. */
function versionOf(event: TimelineEvent): HistoryVersion {
    return {
        version: event.version,
        modifiedBy: event.actor,
        recordedAt: event.at,
        fields: [...event.fields],
        changes: [],
    };
}

/**
 * The story of a party: one rail for the party, its identifiers, its contacts
 * and its business units, under a chart of the events over time, with the
 * author's photo and link on each entry. A version of the party can be written
 * back by a person who may write it; the newest says it is the current one.
 */
function PartyHistory({
    row,
    onRevert,
}: {
    readonly row: RecordRow;
    readonly onRevert: ((version: HistoryVersion) => void) | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const holds = useHolds();
    const pictureOf = useActorPictures();
    const { isService } = usePeopleDirectory();
    const id = show(row['id']);
    const story = useQuery({
        queryKey: ['timeline', 'party', id],
        queryFn: () => api.timeline('party', id),
    });
    if (story.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (story.isError) {
        return <Notice tone="error">{story.error.message}</Notice>;
    }
    return (
        <Timeline
            timeline={story.data}
            actorPath={actorPathFor(holds('iam::accounts:read'))}
            actorPicture={pictureOf}
            isService={isService}
            hideUnchanged
            recordName={(event) => {
                const record = RECORD_KEYS[event.entityType];
                if (record === undefined) return undefined;
                const named = event.fields.find((field) => field.name === record.field)?.value;
                return [t(`refdata.parties.record.${record.name}`), named]
                    .filter((part) => part !== undefined && part !== '')
                    .join(' ');
            }}
            renderActions={(event) =>
                onRevert !== undefined &&
                event.entityType === 'ores.refdata.party' &&
                event.version < row.version ? (
                    <div className="mt-2 flex justify-end">
                        <Button size="sm" onClick={() => onRevert(versionOf(event))}>
                            {t('history.revertTo', { version: String(event.version) })}
                        </Button>
                    </div>
                ) : undefined
            }
        />
    );
}

function PartyBody({ row }: { readonly row: RecordRow }): ReactNode {
    const { t } = useTranslation();
    const id = show(row['id']);
    const name = show(row['full_name']);
    const may = useRecordPermissions(PARTIES);
    const root = useAreaRoot('refdata');
    const { tab, bar } = useRecordTabs({
        label: name,
        tabs: [
            'details',
            'identifiers',
            'contacts',
            'memberships',
            'centres',
            'counterparties',
            'history',
        ],
    });
    const identifiers = useRecords(IDENTIFIERS, id);
    const contacts = useRecords(CONTACTS, id);
    const units = useRecords(UNITS, id);
    const [editing, setEditing] = useState(false);
    const [reverting, setReverting] = useState<HistoryVersion | null>(null);
    const image = show(row['image_id']);

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    ...root,
                    { label: t('parties.title'), to: PATHS.parties },
                    { label: name },
                ]}
                title={name}
                recordKey={show(row['short_code'])}
                version={null}
                access={null}
                {...(image === '' ? {} : { mark: <Flag src={imageUrl(image)} size="lg" /> })}
            />
            {bar}
            {tab === 'details' && (
                <RecordDetails
                    specs={PARTY_FIELDS}
                    row={row}
                    heading={{
                        title: t('refdata.parties.card'),
                        onEdit: may.write ? () => setEditing(true) : undefined,
                    }}
                />
            )}
            {tab === 'identifiers' && (
                <PartsPanel
                    resource={IDENTIFIERS}
                    titles={{
                        add: t('refdata.parties.addPart.party-identifiers'),
                        one: t('refdata.parties.editPart.party-identifiers'),
                        remove: t('refdata.parties.removePart.party-identifiers'),
                    }}
                    entityType="ores.refdata.party_identifier"
                    historyKey={(row) => show(row['id_value'])}
                    specs={IDENTIFIER_FIELDS}
                    parentField="party_id"
                    parent={id}
                    rows={identifiers.data ?? []}
                    error={identifiers.error?.message}
                    lead={t('refdata.parties.identifiersLead')}
                    columns={[
                        {
                            header: t('refdata.fields.id_scheme'),
                            cell: (row) => show(row['id_scheme']),
                        },
                        {
                            header: t('refdata.fields.id_value'),
                            cell: (row) => show(row['id_value']),
                            mono: true,
                        },
                        {
                            header: t('refdata.fields.description'),
                            cell: (row) => show(row['description']),
                        },
                    ]}
                    sort={(row) => `${show(row['id_scheme'])}${show(row['id_value'])}`}
                />
            )}
            {tab === 'contacts' && (
                <PartsPanel
                    resource={CONTACTS}
                    titles={{
                        add: t('refdata.parties.addPart.party-contacts'),
                        one: t('refdata.parties.editPart.party-contacts'),
                        remove: t('refdata.parties.removePart.party-contacts'),
                    }}
                    entityType="ores.refdata.party_contact_information"
                    historyKey={(row) => show(row['contact_type'])}
                    specs={CONTACT_FIELDS}
                    parentField="party_id"
                    parent={id}
                    rows={contacts.data ?? []}
                    error={contacts.error?.message}
                    initial={{ is_primary: 'false' }}
                    columns={[
                        {
                            header: t('refdata.fields.contact_type'),
                            cell: (row) => (
                                <ClassifiedValue
                                    list="contact-type"
                                    code={show(row['contact_type'])}
                                />
                            ),
                        },
                        {
                            header: t('refdata.parties.address'),
                            cell: (row) =>
                                [
                                    show(row['street_line_1']),
                                    show(row['city']),
                                    show(row['postal_code']),
                                ]
                                    .filter((part) => part !== '')
                                    .join(', '),
                        },
                        {
                            header: t('refdata.fields.country_code'),
                            cell: (row) => (
                                <FlaggedCode source="country" code={show(row['country_code'])} />
                            ),
                        },
                        {
                            header: t('refdata.fields.phone'),
                            cell: (row) => show(row['phone']),
                        },
                        {
                            header: t('refdata.fields.email'),
                            cell: (row) => (
                                <Address
                                    text={show(row['email'])}
                                    link={mailtoLink(show(row['email']))}
                                />
                            ),
                        },
                        {
                            header: t('refdata.fields.web_page'),
                            cell: (row) => (
                                <Address
                                    text={show(row['web_page'])}
                                    link={webLink(show(row['web_page']))}
                                />
                            ),
                        },
                    ]}
                    sort={(row) => show(row['contact_type'])}
                />
            )}
            {tab === 'memberships' && <Memberships partyId={id} />}
            {tab === 'centres' && (
                <PartsPanel
                    resource={UNITS}
                    titles={{
                        add: t('refdata.parties.addPart.business-units'),
                        one: t('refdata.parties.editPart.business-units'),
                        remove: t('refdata.parties.removePart.business-units'),
                    }}
                    entityType="ores.refdata.business_unit"
                    historyKey={(row) => show(row['unit_code'])}
                    specs={UNIT_FIELDS}
                    parentField="party_id"
                    parent={id}
                    keep={['parent_business_unit_id']}
                    rows={units.data ?? []}
                    error={units.error?.message}
                    lead={t('refdata.parties.unitsLead')}
                    columns={[
                        {
                            header: t('refdata.fields.unit_name'),
                            cell: (row) => show(row['unit_name']),
                        },
                        {
                            header: t('refdata.fields.unit_code'),
                            cell: (row) => show(row['unit_code']),
                            mono: true,
                        },
                        {
                            header: t('refdata.fields.business_centre_code'),
                            cell: (row) => (
                                <FlaggedCode
                                    source="businessCentre"
                                    code={show(row['business_centre_code'])}
                                />
                            ),
                        },
                        {
                            header: t('refdata.fields.status'),
                            cell: (row) => show(row['status']),
                        },
                    ]}
                    sort={(row) => show(row['unit_code'])}
                />
            )}
            {tab === 'counterparties' && <TradingCounterparties partyId={id} />}
            {tab === 'history' && (
                <PartyHistory
                    row={row}
                    onRevert={may.write ? (version) => setReverting(version) : undefined}
                />
            )}
            {editing && (
                <RecordDialog
                    title={t('refdata.parties.editTitle', { name })}
                    resource={PARTIES}
                    specs={PARTY_FIELDS}
                    row={row}
                    keep={PARTY_KEPT}
                    onClose={() => setEditing(false)}
                />
            )}
            {reverting !== null && (
                <RevertDialog
                    resource={PARTIES}
                    specs={PARTY_FIELDS}
                    row={row}
                    version={reverting}
                    keep={PARTY_KEPT}
                    onClose={() => setReverting(null)}
                />
            )}
        </div>
    );
}

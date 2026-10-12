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

/*
 * A timeline is one subject's whole story in one stream: every row any
 * component wrote for it, newest first, whether the row was a field change or
 * an act that changed no field.
 *
 * The shape is not a person's, and it is not a request's. A person and a
 * request are two subjects of the same stream, and the difference between them
 * is the producer that fills it, not the stream. So the entry model lives here
 * rather than beside either of them: a second copy would drift, and a screen
 * would then have to know which copy it was reading.
 *
 * Two things make the stream honest about itself. An entry names the row it
 * came from, so a screen can pair the entries of one entity and diff them. And
 * the stream carries its own gaps: a source that has no read, or that refused
 * this caller, is stated as a gap rather than left out, because a story that
 * quietly omits a chapter reads as though the chapter never happened.
 */

import { z } from 'zod';
import type { AuthenticatedCaller } from './account-operations.js';
import { readAccountAccess } from './access.js';
import { readEntityHistory } from './classifications.js';
import { readAccount, readAuthEvents } from './credentials.js';
import { readContactInformation } from './profile-operations.js';
import { listRecords, readRecord, recordResource, type RecordRow } from './records.js';

/** One field of one entry in a timeline, as a screen draws it. */
export const timelineFieldSchema = z.object({
    name: z.string().default(''),
    value: z.string().default(''),
});

export type TimelineField = z.infer<typeof timelineFieldSchema>;

/**
 * One thing that happened to a subject, drawn as a row of its stream.
 *
 * `entityType` and `entityId` name the row the entry came from, so the screen
 * can pair consecutive entries of the same entity and diff them. `kind` is what
 * the entry is, already decided: raising a record, changing it, asking for a
 * role, granting one, signing in. The screen names and tints from it rather
 * than working it out again.
 */
export const timelineEventSchema = z.object({
    entityType: z.string().default(''),
    entityId: z.string().default(''),
    kind: z.string().default(''),
    at: z.string().default(''),
    actor: z.string().default(''),
    version: z.int().nonnegative().default(0),
    reasonCode: z.string().default(''),
    commentary: z.string().default(''),
    fields: z.array(timelineFieldSchema).default([]),
});

export type TimelineEvent = z.infer<typeof timelineEventSchema>;

/**
 * A source the stream could not draw from, and why.
 *
 * A gap is part of the answer rather than a failure of it. A grant that was
 * closed, a junction with no history provider, a window that has aged out: each
 * is a fact about what the product can say, and a reader who is not told it
 * will read the stream as complete.
 */
export const timelineGapSchema = z.object({
    entity: z.string().default(''),
    reason: z.string().default(''),
});

export type TimelineGap = z.infer<typeof timelineGapSchema>;

/** One subject's story: what happened, and what the stream cannot show. */
export const timelineSchema = z.object({
    subject: z.string().default(''),
    id: z.string().default(''),
    events: z.array(timelineEventSchema).default([]),
    gaps: z.array(timelineGapSchema).default([]),
});

export type Timeline = z.infer<typeof timelineSchema>;

/**
 * How late in a second an entry of each kind lands.
 *
 * The timestamps on the wire carry whole seconds, and a record is raised,
 * changed, asked for and told about inside one of them. Ties are therefore the
 * common case rather than the exception, and the order among them is chosen to
 * read as the order the acts happened in: a request is raised, its roles are
 * recorded, notices go out, an answer is given, the role is applied. A person
 * signs in or out at the top of their own second, because the sign-in is what
 * the rest of the second is done under.
 */
const KIND_RANK: Readonly<Record<string, number>> = {
    raised: 10,
    changed: 20,
    asked: 30,
    told: 40,
    decided: 50,
    granted: 60,
    signed_in: 70,
    closed: 80,
    sign_in_failed: 70,
    signed_out: 70,
    refreshed: 70,
    noticed: 70,
};

/** Newest first, and within one second, the later act first. */
export function orderTimeline(events: readonly TimelineEvent[]): TimelineEvent[] {
    return [...events].sort((left, right) => {
        if (left.at !== right.at) return left.at < right.at ? 1 : -1;
        const rank = (KIND_RANK[right.kind] ?? 0) - (KIND_RANK[left.kind] ?? 0);
        if (rank !== 0) return rank;
        if (left.entityId !== right.entityId) return left.entityId < right.entityId ? -1 : 1;
        return right.version - left.version;
    });
}

/**
 * The field names the history layer stamps on every version.
 *
 * They are provenance rather than the record's own state, so a screen shows
 * them as the entry's own facts and keeps them out of the value diff. The names
 * are constants on the server side and travel as ordinary fields.
 */
export const TIMELINE_PROVENANCE_FIELDS: ReadonlySet<string> = new Set([
    'Modified By',
    'Performed By',
    'Change Reason Code',
    'Change Commentary',
    'Recorded At',
]);

const RECORDED_AT_FIELD = 'Recorded At';
const MODIFIED_BY_FIELD = 'Modified By';
const REASON_CODE_FIELD = 'Change Reason Code';
const COMMENTARY_FIELD = 'Change Commentary';

/** One field's value, or the empty string when the version does not carry it. */
export function fieldValue(fields: readonly TimelineField[], name: string): string {
    return fields.find((field) => field.name === name)?.value ?? '';
}

/** The account entity whose versions make up a person's own changes. */
export const ACCOUNT_ENTITY = 'ores.iam.account';

/** The contact record a person owns, whose versions make up their address. */
export const CONTACT_ENTITY = 'ores.iam.account_contact_information';

/** The link between a person and a party they work in. */
export const ACCOUNT_PARTY_ENTITY = 'ores.iam.account_party';

/** One person's grants or refusals of a sign-in. */
export const AUTH_EVENT_ENTITY = 'ores.iam.auth_event';

/** How many sign-ins one person's stream carries. */
const SIGN_IN_PAGE = 200;

/**
 * The key a record's history is read by.
 *
 * A history provider resolves the row by the key its model declares, not by the
 * row's id, and the two differ: an account is declared by its username and a
 * contact record by its mail address. Reading by the id answers with no
 * versions, which reads on the stream as a record that was never written.
 */
const ACCOUNT_KEY = (account: { readonly username: string }): string => account.username;

const CONTACT_KEY = (contact: { readonly email: string }): string => contact.email;

/** What each authentication event type reads as in the stream. */
const SIGN_IN_KINDS: Readonly<Record<string, string>> = {
    login_success: 'signed_in',
    login_failure: 'sign_in_failed',
    logout: 'signed_out',
    token_refresh: 'refreshed',
    account_locked: 'locked',
    account_unlocked: 'unlocked',
};

/**
 * Runs one source of the stream, recording a gap instead of failing the whole.
 *
 * A source that refuses this caller, or that this deployment does not serve, is
 * one chapter missing rather than a broken screen. The account itself is the
 * exception, because nothing can be drawn without it; every other source is
 * read through here.
 */
async function orGap<T>(
    gaps: TimelineGap[],
    entity: string,
    reason: string,
    read: () => Promise<T>,
    missing: T,
): Promise<T> {
    try {
        return await read();
    } catch {
        gaps.push({ entity, reason });
        return missing;
    }
}

/**
 * The entries one versioned record's history makes.
 *
 * Every record counts its versions from its own first one, and the history
 * numbers them newest first, so the version number goes on the entry as it
 * arrived. The reason and the commentary are stamped on the version as fields,
 * which is why they are read back out of them here.
 */
export function recordEvents(
    entityType: string,
    entityId: string,
    versions: readonly {
        readonly version: number;
        readonly modifiedBy: string;
        readonly recordedAt: string;
        readonly fields: readonly TimelineField[];
    }[],
): readonly TimelineEvent[] {
    return versions.map((version) => ({
        entityType,
        entityId,
        kind: version.version === 1 ? 'raised' : 'changed',
        at: version.recordedAt,
        actor: version.modifiedBy,
        version: version.version,
        reasonCode: fieldValue(version.fields, REASON_CODE_FIELD),
        commentary: fieldValue(version.fields, COMMENTARY_FIELD),
        fields: [...version.fields],
    }));
}

/** The name a link's history gives to its own row: both keys, parent first. */
const LINK_ID_FIELD = 'ID';

/** The end of a link's validity, empty while the link is open. */
const CLOSED_AT_FIELD = 'Closed At';

/**
 * The entries a link table's history makes.
 *
 * A link has two keys, so the server reads its history by the parent and answers
 * every version of every link of that parent, each carrying the pair as its
 * @c ID field. The versions are grouped by that pair, and each link is one row
 * of the stream: its opening, each change, and its closing. A link is closed by
 * ending its validity and not by writing a version, so the closing is read off
 * the field the server states, and it has no author: the store records when a
 * link ended, and not who ended it.
 */
export function linkEvents(
    entityType: string,
    versions: readonly {
        readonly version: number;
        readonly modifiedBy: string;
        readonly recordedAt: string;
        readonly fields: readonly TimelineField[];
    }[],
): readonly TimelineEvent[] {
    const links = new Map<string, (typeof versions)[number][]>();
    for (const version of versions) {
        const id = fieldValue(version.fields, LINK_ID_FIELD);
        links.set(id, [...(links.get(id) ?? []), version]);
    }
    return [...links.entries()].flatMap(([id, own]) => {
        const events = recordEvents(entityType, id, own);
        const closings = own
            .filter((version) => fieldValue(version.fields, CLOSED_AT_FIELD) !== '')
            .map(
                (version): TimelineEvent => ({
                    entityType,
                    entityId: id,
                    kind: 'closed',
                    at: fieldValue(version.fields, CLOSED_AT_FIELD),
                    actor: '',
                    version: version.version,
                    reasonCode: '',
                    commentary: '',
                    fields: version.fields.filter(
                        (field) =>
                            !TIMELINE_PROVENANCE_FIELDS.has(field.name) &&
                            field.name !== CLOSED_AT_FIELD,
                    ),
                }),
            );
        return [...events, ...closings];
    });
}

/**
 * One person's whole story, newest first.
 *
 * The account, its contact record, the roles it holds and its sign-ins are four
 * reads over four tables, merged into one stream. The parts are not equally
 * answerable, and the stream says so rather than rounding: a grant that was
 * taken away has no read at all, and the sign-in log keeps a window rather than
 * everything. Each is a gap on the answer.
 *
 * A username nothing holds answers with an empty stream, which is what the
 * person screen answers for the same name.
 */
export async function readPersonTimeline(
    caller: AuthenticatedCaller,
    username: string,
): Promise<Timeline> {
    const account = await readAccount(caller, username);
    if (account === null) {
        return { subject: 'person', id: username, events: [], gaps: [] };
    }

    const gaps: TimelineGap[] = [];

    const [accountVersions, contact, grants, signIns, partyLinks] = await Promise.all([
        orGap(
            gaps,
            ACCOUNT_ENTITY,
            'The account’s own versions need the account read, which this caller does not hold.',
            () => readEntityHistory(caller, ACCOUNT_ENTITY, ACCOUNT_KEY(account)),
            [],
        ),
        orGap(
            gaps,
            CONTACT_ENTITY,
            'This person has no contact record, so the stream carries no address change.',
            () => readContactInformation(caller, account.id),
            null,
        ),
        orGap(
            gaps,
            'ores.iam.account_role',
            'The roles read refused this caller, so the grants this person holds are not shown.',
            () => readAccountAccess(caller, account.id),
            { roles: [] },
        ),
        orGap(
            gaps,
            AUTH_EVENT_ENTITY,
            'The sign-in read refused this caller, so this person’s sign-ins are not shown.',
            () => readAuthEvents(caller, { accountId: account.id, limit: SIGN_IN_PAGE }),
            [],
        ),
        orGap(
            gaps,
            ACCOUNT_PARTY_ENTITY,
            'The parties this person works in could not be read, so opening or closing one is not shown.',
            () => readEntityHistory(caller, ACCOUNT_PARTY_ENTITY, account.id),
            [],
        ),
    ]);

    const contactEvents =
        contact === null || CONTACT_KEY(contact) === ''
            ? []
            : await orGap(
                  gaps,
                  CONTACT_ENTITY,
                  'The contact record’s versions need the contact history read, which this caller does not hold.',
                  () => readEntityHistory(caller, CONTACT_ENTITY, CONTACT_KEY(contact)),
                  [],
              ).then((versions) => recordEvents(CONTACT_ENTITY, contact.id, versions));

    /*
     * A grant is read as the roles held now, with who gave each and when. The
     * grant's own row is versioned and would carry its closure, but no read
     * returns a closed one, so the stream can say that a role was given and
     * cannot say that it was taken away.
     */
    const grantEvents: readonly TimelineEvent[] = grants.roles.map((role) => ({
        entityType: 'ores.iam.account_role',
        entityId: role.roleId,
        kind: 'granted',
        at: role.givenAt,
        actor: role.givenBy,
        version: 1,
        reasonCode: role.reasonCode,
        commentary: role.commentary,
        fields: [
            { name: 'Role', value: role.name },
            { name: 'Description', value: role.description },
        ],
    }));

    const partyLinkEvents = await namedLinks(
        caller,
        linkEvents(ACCOUNT_PARTY_ENTITY, partyLinks),
        'Party',
        'parties',
    );

    const signInEvents: readonly TimelineEvent[] = signIns.map((event) => ({
        entityType: AUTH_EVENT_ENTITY,
        entityId: event.id,
        kind: SIGN_IN_KINDS[event.eventType] ?? 'noticed',
        at: event.eventTime,
        actor: event.username,
        version: 1,
        reasonCode: '',
        commentary: '',
        fields: [
            { name: 'Event', value: event.eventType },
            { name: 'Session', value: event.sessionId },
            { name: 'Party', value: event.partyId },
            { name: 'Detail', value: event.errorDetail },
        ],
    }));

    gaps.push({
        entity: 'ores.iam.account_role',
        reason: 'A grant that was taken away is closed by its end time, and no read returns a closed grant, so only the grants held now are shown.',
    });
    gaps.push({
        entity: AUTH_EVENT_ENTITY,
        reason: `The sign-in log keeps a rolling window rather than everything, so a sign-in older than the window is not shown. This stream reads the most recent ${SIGN_IN_PAGE}.`,
    });

    return {
        subject: 'person',
        id: username,
        events: orderTimeline([
            ...recordEvents(ACCOUNT_ENTITY, account.id, accountVersions),
            ...contactEvents,
            ...partyLinkEvents,
            ...grantEvents,
            ...signInEvents,
        ]),
        gaps,
    };
}

/** The entity types whose versions make up a party's story. */
export const PARTY_ENTITY = 'ores.refdata.party';
export const PARTY_IDENTIFIER_ENTITY = 'ores.refdata.party_identifier';
export const PARTY_CONTACT_ENTITY = 'ores.refdata.party_contact_information';
export const BUSINESS_UNIT_ENTITY = 'ores.refdata.business_unit';

/**
 * The kinds of record that belong to a party, each with the resource that lists
 * them by party and the key its history is read by.
 *
 * A history provider resolves a row by the key its model declares, which is not
 * always the row's id: an identifier by its value, a contact by its type and a
 * unit by its code. The id is what ties a version to its row, so the versions
 * read are kept only when they carry the id of the row asked for.
 */
const PARTY_PARTS: readonly {
    readonly resource: string;
    readonly entityType: string;
    readonly key: (row: RecordRow) => string;
    readonly name: string;
}[] = [
    {
        resource: 'party-identifiers',
        entityType: PARTY_IDENTIFIER_ENTITY,
        key: (row) => String(row['id_value'] ?? ''),
        name: 'identifier',
    },
    {
        resource: 'party-contacts',
        entityType: PARTY_CONTACT_ENTITY,
        key: (row) => String(row['contact_type'] ?? ''),
        name: 'contact',
    },
    {
        resource: 'business-units',
        entityType: BUSINESS_UNIT_ENTITY,
        key: (row) => String(row['unit_code'] ?? ''),
        name: 'business unit',
    },
];

/**
 * Says the row on a link's far side by its name and not by its id.
 *
 * A link records the id of the party or the counterparty it points at, and a
 * reader of the stream cannot read an id. The names are read one row at a time,
 * which is a handful for a party, and a name that cannot be read leaves the id
 * in place: the entry still says which row, only less kindly.
 */
async function namedLinks(
    caller: AuthenticatedCaller,
    events: readonly TimelineEvent[],
    field: string,
    resourceKey: string,
): Promise<readonly TimelineEvent[]> {
    const resource = recordResource(resourceKey);
    if (resource === undefined) return events;
    const ids = [
        ...new Set(events.map((event) => fieldValue(event.fields, field)).filter((v) => v !== '')),
    ];
    const names = new Map<string, string>();
    await Promise.all(
        ids.map(async (id) => {
            try {
                const row = await readRecord(caller, resource, id);
                const name = String(row?.['full_name'] ?? row?.['short_code'] ?? '');
                if (name !== '') names.set(id, name);
            } catch {
                // The id stands in for a name that could not be read.
            }
        }),
    );
    return events.map((event) => ({
        ...event,
        fields: event.fields.map((f) =>
            f.name === field && names.has(f.value) ? { ...f, value: names.get(f.value) ?? '' } : f,
        ),
    }));
}

/** The links a party holds, each read by the party under its own entity type. */
const PARTY_LINKS: readonly {
    readonly entityType: string;
    readonly what: string;
    /** The field that holds an id, and the resource that names it. A code needs none. */
    readonly far?: { readonly field: string; readonly resource: string };
}[] = [
    { entityType: 'ores.refdata.party_country', what: 'countries it works in' },
    { entityType: 'ores.refdata.party_currency', what: 'currencies it works in' },
    {
        entityType: 'ores.refdata.party_counterparty',
        what: 'counterparties it may trade with',
        far: { field: 'Counterparty', resource: 'counterparties' },
    },
];

/** The versions of a history that belong to the row asked for. */
function versionsOfRow<T extends { readonly fields: readonly TimelineField[] }>(
    versions: readonly T[],
    id: string,
): readonly T[] {
    return versions.filter((version) => fieldValue(version.fields, 'ID') === id);
}

/**
 * One party's whole story, newest first.
 *
 * A party is made of several kinds of record, so its story is the party's own
 * versions, merged with those of its identifiers, its contacts and its business
 * units, and with the opening and closing of the countries and currencies it
 * works in and the counterparties it may trade with. A link has two keys, so it
 * is read by the party and answers every link the party ever held.
 *
 * A party nothing holds answers with an empty stream, as the party page does.
 */
export async function readPartyTimeline(
    caller: AuthenticatedCaller,
    partyId: string,
): Promise<Timeline> {
    const parties = recordResource('parties');
    const party = parties === undefined ? undefined : await readRecord(caller, parties, partyId);
    if (party === undefined) {
        return { subject: 'party', id: partyId, events: [], gaps: [] };
    }

    const gaps: TimelineGap[] = [];
    const partyVersions = await orGap(
        gaps,
        PARTY_ENTITY,
        'The party’s own versions need the party read, which this caller does not hold.',
        () => readEntityHistory(caller, PARTY_ENTITY, String(party['short_code'] ?? '')),
        [],
    );

    const partEvents = await Promise.all(
        PARTY_PARTS.map(async (part) => {
            const resource = recordResource(part.resource);
            if (resource === undefined) return [];
            const rows = await orGap(
                gaps,
                part.entityType,
                `The ${part.name}s of this party could not be read, so their versions are not shown.`,
                () => listRecords(caller, resource, partyId),
                [] as readonly RecordRow[],
            );
            const perRow = await Promise.all(
                rows.map(async (row) => {
                    const id = String(row['id'] ?? '');
                    const versions = await orGap(
                        gaps,
                        part.entityType,
                        `The versions of a ${part.name} could not be read, so they are not shown.`,
                        () => readEntityHistory(caller, part.entityType, part.key(row)),
                        [],
                    );
                    const own = versionsOfRow(versions, id);
                    if (own.length === 0 && versions.length > 0) {
                        gaps.push({
                            entity: part.entityType,
                            reason: `The history of a ${part.name} is read by a key that other parties may share, and the server answered with another party’s row, so this one’s versions are not shown.`,
                        });
                    }
                    return recordEvents(part.entityType, id, own);
                }),
            );
            return perRow.flat();
        }),
    );

    const linkStories = await Promise.all(
        PARTY_LINKS.map(async (link) => {
            const events = linkEvents(
                link.entityType,
                await orGap(
                    gaps,
                    link.entityType,
                    `The ${link.what} could not be read, so opening or closing one is not shown.`,
                    () => readEntityHistory(caller, link.entityType, partyId),
                    [],
                ),
            );
            return link.far === undefined
                ? events
                : namedLinks(caller, events, link.far.field, link.far.resource);
        }),
    );

    return {
        subject: 'party',
        id: partyId,
        events: orderTimeline([
            ...recordEvents(PARTY_ENTITY, partyId, versionsOfRow(partyVersions, partyId)),
            ...partEvents.flat(),
            ...linkStories.flat(),
        ]),
        gaps,
    };
}

/** The field names a record's version carries about its own writing. */
export { RECORDED_AT_FIELD, MODIFIED_BY_FIELD };

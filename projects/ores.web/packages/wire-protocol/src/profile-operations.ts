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

import { z } from 'zod';
import type { AuthenticatedCaller } from './account-operations.js';
import type { Account, AccountContactInformation } from './domain.js';
import {
    accountContactInformationSchema,
    accountSchema,
    uuidSchema,
    wireTimestampSchema,
} from './domain.js';
import { OperationFailedError } from './errors.js';
import { subjects as accountSubjects } from './generated/iam/protocol/account_operations_protocol.js';
import { subjects as contactSubjects } from './generated/iam/protocol/account_contact_information_protocol.js';
import { accountWriteReplySchema, decidedResultSchema } from './operations.js';
import type { AccountWriteReply } from './operations.js';

/**
 * The profile write path: the account and the contact record a person owns.
 *
 * Each operation is a thin, typed wrapper over one subject. The self writes
 * name no account, because the session names it, so a caller cannot reach
 * another account's record. The administered writes name the account the
 * screen shows, and the server refuses a caller without the permission.
 *
 * A refusal a panel must draw travels as a reply rather than as a thrown
 * error: the self writes and the contact put answer with the shared result,
 * field failures whole. A read that the server did not decide is thrown.
 */

/** The profile fields a write owns, and the reason for it. */
export const profileWriteSchema = z.object({
    fullName: z.string().default(''),
    jobTitle: z.string().default(''),
    /** The id of an uploaded image, or empty for no picture. */
    imageId: z.string().default(''),
    reasonCode: z.string().default(''),
    commentary: z.string().default(''),
});

export type ProfileWrite = z.infer<typeof profileWriteSchema>;

/** The contact fields a write owns, and the reason for it. */
export const contactWriteSchema = z.object({
    streetLine1: z.string().default(''),
    streetLine2: z.string().default(''),
    city: z.string().default(''),
    state: z.string().default(''),
    countryCode: z.string().default(''),
    postalCode: z.string().default(''),
    phone: z.string().default(''),
    /** The address colleagues use. Not the account's sign-in address. */
    email: z.string().default(''),
    webPage: z.string().default(''),
    reasonCode: z.string().default(''),
    commentary: z.string().default(''),
});

export type ContactWrite = z.infer<typeof contactWriteSchema>;

/**
 * The same contact write with the claim an administrator's panel states.
 *
 * The version is the one the panel read, so a record that moved since the
 * panel was drawn is refused by the store rather than overwritten. A null
 * version says the panel showed no record, which is the claim that no current
 * row exists.
 */
export const claimedContactWriteSchema = contactWriteSchema.extend({
    version: z.int().nonnegative().nullable().default(null),
});

export type ClaimedContactWrite = z.infer<typeof claimedContactWriteSchema>;

/**
 * One contact record, as the server writes it.
 *
 * The tenant id is dropped: a contact record is read inside the caller's
 * tenant, and no screen names the tenant it belongs to.
 */
const wireContactSchema = z.object({
    version: z.int().nonnegative().default(0),
    id: uuidSchema,
    account_id: uuidSchema,
    street_line_1: z.string().default(''),
    street_line_2: z.string().default(''),
    city: z.string().default(''),
    state: z.string().default(''),
    country_code: z.string().default(''),
    postal_code: z.string().default(''),
    phone: z.string().default(''),
    email: z.string().default(''),
    web_page: z.string().default(''),
    modified_by: z.string().default(''),
    change_reason_code: z.string().default(''),
    change_commentary: z.string().default(''),
    performed_by: z.string().default(''),
    recorded_at: wireTimestampSchema,
});

function mapContact(row: z.infer<typeof wireContactSchema>): AccountContactInformation {
    return {
        version: row.version,
        id: row.id,
        accountId: row.account_id,
        streetLine1: row.street_line_1,
        streetLine2: row.street_line_2,
        city: row.city,
        state: row.state,
        countryCode: row.country_code,
        postalCode: row.postal_code,
        phone: row.phone,
        email: row.email,
        webPage: row.web_page,
        modifiedBy: row.modified_by,
        changeReasonCode: row.change_reason_code,
        changeCommentary: row.change_commentary,
        performedBy: row.performed_by,
        recordedAt: row.recorded_at,
    };
}

/**
 * `update_self_account_contact_information_response`, and the administered
 * put's reply: what the write decided, and the record as written.
 *
 * The record is stated only when the outcome is ok. The result keeps its
 * field failures because the panel branches on them.
 */
export const contactWriteReplySchema = z
    .object({
        result: decidedResultSchema,
        account_contact_information: wireContactSchema.nullable().default(null),
    })
    .transform((row) => ({
        result: row.result,
        contact:
            row.account_contact_information === null
                ? null
                : mapContact(row.account_contact_information),
    }));

export type ContactWriteReply = z.infer<typeof contactWriteReplySchema>;

/** `list_by_account_id_account_contact_informations_response`. */
export const contactPageSchema = z
    .object({
        result: decidedResultSchema,
        account_contact_informations: z.array(wireContactSchema).default([]),
        total: z.int().nonnegative().default(0),
    })
    .transform((row) => ({
        result: row.result,
        contacts: row.account_contact_informations.map(mapContact),
        totalCount: row.total,
    }));

export type ContactPage = z.infer<typeof contactPageSchema>;

/*
 * The same answers as the BFF serves them, in the camelCase the browser reads.
 *
 * The wire replies above translate snake_case on the way in, so parsing their
 * own output would fail; these are the shapes the browser parses instead. The
 * BFF serialises them from the wire replies after the helpers translated them.
 */

/** The self account write's answer: the result, and the account as written. */
export const accountWriteViewSchema = z.object({
    result: decidedResultSchema,
    account: accountSchema.nullable().default(null),
});

export type AccountWriteView = z.infer<typeof accountWriteViewSchema>;

/** The contact write's answer: the result, and the record as written. */
export const contactWriteViewSchema = z.object({
    result: decidedResultSchema,
    contact: accountContactInformationSchema.nullable().default(null),
});

export type ContactWriteView = z.infer<typeof contactWriteViewSchema>;

/** The contact read's answer: the record, or nothing when the account has none. */
export const contactViewSchema = z.object({
    contact: accountContactInformationSchema.nullable().default(null),
});

export type ContactView = z.infer<typeof contactViewSchema>;

/**
 * Writes the signed-in person's own profile fields.
 *
 * The three fields a member does not own are sent empty, which the protocol
 * reads as not stated; a stated value would be refused with
 * `field_not_self_writable`. The panel never states one.
 */
export async function updateSelfAccount(
    caller: AuthenticatedCaller,
    input: ProfileWrite,
): Promise<AccountWriteReply> {
    return caller.callAuthenticated(
        accountSubjects.update_self_account_request,
        {
            full_name: input.fullName,
            job_title: input.jobTitle,
            image_id: input.imageId,
            email: '',
            default_party_id: '',
            reports_to_account_id: '',
            change_reason_code: input.reasonCode,
            change_commentary: input.commentary,
        },
        accountWriteReplySchema,
    );
}

/** Writes the signed-in person's own contact record, creating it if absent. */
export async function updateSelfContactInformation(
    caller: AuthenticatedCaller,
    input: ContactWrite,
): Promise<ContactWriteReply> {
    return caller.callAuthenticated(
        accountSubjects.update_self_account_contact_information_request,
        {
            street_line_1: input.streetLine1,
            street_line_2: input.streetLine2,
            city: input.city,
            state: input.state,
            country_code: input.countryCode,
            postal_code: input.postalCode,
            phone: input.phone,
            email: input.email,
            web_page: input.webPage,
            change_reason_code: input.reasonCode,
            change_commentary: input.commentary,
        },
        contactWriteReplySchema,
    );
}

/**
 * Writes one account whole, as an administrator.
 *
 * The subject replaces every field, so the caller passes the account as it
 * was read and the record is composed from it: the fields this screen does
 * not set are echoed, and an empty string would clear them.
 */
export async function updateAccount(
    caller: AuthenticatedCaller,
    account: Account,
    write: ProfileWrite,
): Promise<void> {
    const reply = await caller.callAuthenticated(
        accountSubjects.update_account_request,
        {
            account_id: account.id,
            email: account.email,
            full_name: write.fullName,
            default_party_id: account.defaultPartyId ?? '',
            job_title: write.jobTitle,
            reports_to_account_id: account.reportsToAccountId ?? '',
            image_id: write.imageId,
            change_reason_code: write.reasonCode,
            change_commentary: write.commentary,
        },
        z.object({ success: z.boolean().default(false), message: z.string().default('') }),
    );
    if (!reply.success) {
        throw new OperationFailedError(accountSubjects.update_account_request, reply.message);
    }
}

/**
 * One account's contact record, or nothing when it has none.
 *
 * The read names the account because an administrator reads a colleague's
 * record; the self route passes the session's own account id.
 */
export async function readContactInformation(
    caller: AuthenticatedCaller,
    accountId: string,
): Promise<AccountContactInformation | null> {
    const reply = await caller.callAuthenticated(
        contactSubjects.list_by_account_id_account_contact_informations_request,
        {
            account_id: accountId,
            scope: 'direct',
            offset: 0,
            limit: 1,
            order: { field: '', descending: false },
            filter: null,
        },
        contactPageSchema,
    );
    if (reply.result.outcome !== 'ok') {
        throw new OperationFailedError(
            contactSubjects.list_by_account_id_account_contact_informations_request,
            reply.result.message,
        );
    }
    return reply.contacts[0] ?? null;
}

/**
 * Writes one account's contact record, as an administrator.
 *
 * The claim is built from the version the panel read: a number claims the
 * record still carries that version, and null claims no current record
 * exists. The store decides the claim in the write's own transaction.
 */
export async function putContactInformation(
    caller: AuthenticatedCaller,
    input: {
        readonly accountId: string;
        readonly recordId: string;
        readonly claim: {
            readonly kind: 'must_match_version' | 'must_not_exist';
            readonly version: number | null;
        };
        readonly write: ContactWrite;
    },
): Promise<ContactWriteReply> {
    return caller.callAuthenticated(
        contactSubjects.put_account_contact_information_request,
        {
            change: {
                write: {
                    id: input.recordId,
                    account_id: input.accountId,
                    street_line_1: input.write.streetLine1,
                    street_line_2: input.write.streetLine2,
                    city: input.write.city,
                    state: input.write.state,
                    country_code: input.write.countryCode,
                    postal_code: input.write.postalCode,
                    phone: input.write.phone,
                    email: input.write.email,
                    web_page: input.write.webPage,
                },
                precondition: { kind: input.claim.kind, version: input.claim.version },
            },
            intent: {
                reason_code: input.write.reasonCode,
                commentary: input.write.commentary,
            },
        },
        contactWriteReplySchema,
    );
}

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

import { describe, expect, it } from 'vitest';
import type { AuthenticatedCaller } from './account-operations.js';
import { accountSchema } from './domain.js';
import { OperationFailedError } from './errors.js';
import {
    putContactInformation,
    readContactInformation,
    readMyContactInformation,
    updateAccount,
    updateSelfAccount,
    updateSelfContactInformation,
} from './profile-operations.js';

/**
 * The profile write path, against a caller that records what it was sent,
 * parses the answered message with the schema the helper names, and hands the
 * result back. Parsing the wire answers here is what proves the mappers: the
 * panels read camelCase records, and the server writes snake_case ones.
 */

const ACCOUNT_ID = '11111111-1111-1111-1111-111111111111';
const TENANT_ID = 'ffffffff-ffff-ffff-ffff-ffffffffffff';
const RECORD_ID = '22222222-2222-2222-2222-222222222222';
const MANAGER_ID = '33333333-3333-3333-3333-333333333333';
const IMAGE_ID = '44444444-4444-4444-4444-444444444444';
const RECORDED_AT = '2026-10-05 09:30:00Z';

interface Recorded {
    subject: string;
    body: unknown;
}

function callerAnswering(answer: unknown, sent: Recorded[] = []): AuthenticatedCaller {
    return {
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            sent.push({ subject, body });
            return schema.parse(answer);
        },
    } as unknown as AuthenticatedCaller;
}

const okResult = { outcome: 'ok', code: '', message: '', fields: [] };

const accountRow = {
    version: 7,
    id: ACCOUNT_ID,
    tenant_id: TENANT_ID,
    username: 'ada',
    full_name: 'Ada Lovelace',
    email: 'ada@example.com',
    account_type: 'user',
    job_title: 'Head of Desk',
    reports_to_account_id: MANAGER_ID,
    default_party_id: null,
    image_id: IMAGE_ID,
    modified_by: 'ada',
    change_reason_code: 'common.non_material_update',
    change_commentary: '',
    performed_by: 'ada',
    recorded_at: RECORDED_AT,
};

const contactRow = {
    version: 4,
    id: RECORD_ID,
    account_id: ACCOUNT_ID,
    street_line_1: '1 Panton Street',
    street_line_2: '',
    city: 'London',
    state: '',
    country_code: 'GB',
    postal_code: 'SW1Y 4DL',
    phone: '+44 20 0000 0000',
    email: 'ada@colleagues.example.com',
    web_page: '',
    modified_by: 'ada',
    change_reason_code: 'common.rectification',
    change_commentary: '',
    performed_by: 'ada',
    recorded_at: RECORDED_AT,
};

const profileWrite = {
    fullName: 'Ada Lovelace',
    jobTitle: 'Chief Analyst',
    imageId: IMAGE_ID,
    reasonCode: 'common.non_material_update',
    commentary: 'promotion',
};

const contactWrite = {
    streetLine1: '1 Panton Street',
    streetLine2: '',
    city: 'London',
    state: '',
    countryCode: 'GB',
    postalCode: 'SW1Y 4DL',
    phone: '+44 20 0000 0000',
    email: 'ada@colleagues.example.com',
    webPage: '',
    reasonCode: 'common.rectification',
    commentary: 'moved desk',
};

describe('updateSelfAccount', () => {
    it('names the self subject and states the unowned fields empty', async () => {
        const sent: Recorded[] = [];
        await updateSelfAccount(
            callerAnswering({ result: okResult, account: accountRow }, sent),
            profileWrite,
        );

        expect(sent[0]?.subject).toBe('iam.v1.accounts.update-self');
        expect(sent[0]?.body).toEqual({
            full_name: 'Ada Lovelace',
            job_title: 'Chief Analyst',
            image_id: IMAGE_ID,
            email: '',
            default_party_id: '',
            reports_to_account_id: '',
            change_reason_code: 'common.non_material_update',
            change_commentary: 'promotion',
        });
    });

    it('answers with the account as written', async () => {
        const reply = await updateSelfAccount(
            callerAnswering({ result: okResult, account: accountRow }),
            profileWrite,
        );

        expect(reply.result.outcome).toBe('ok');
        expect(reply.account?.version).toBe(7);
        expect(reply.account?.fullName).toBe('Ada Lovelace');
        expect(reply.account?.reportsToAccountId).toBe(MANAGER_ID);
    });

    it('keeps the field failures a refusal carries', async () => {
        const refusal = {
            result: {
                outcome: 'denied',
                code: 'field_not_self_writable',
                message: 'Only an administrator can change this field.',
                fields: [
                    {
                        field: 'email',
                        code: 'field_not_self_writable',
                        message: 'Only an administrator can change this field.',
                    },
                ],
            },
            account: null,
        };
        const reply = await updateSelfAccount(callerAnswering(refusal), profileWrite);

        expect(reply.result.outcome).toBe('denied');
        expect(reply.result.code).toBe('field_not_self_writable');
        expect(reply.result.fields).toEqual([
            {
                field: 'email',
                code: 'field_not_self_writable',
                message: 'Only an administrator can change this field.',
            },
        ]);
        expect(reply.account).toBeNull();
    });
});

describe('updateSelfContactInformation', () => {
    it('names the self subject and sends the nine fields whole', async () => {
        const sent: Recorded[] = [];
        await updateSelfContactInformation(
            callerAnswering({ result: okResult, account_contact_information: contactRow }, sent),
            contactWrite,
        );

        expect(sent[0]?.subject).toBe('iam.v1.account_contact_informations.update-self');
        expect(sent[0]?.body).toEqual({
            street_line_1: '1 Panton Street',
            street_line_2: '',
            city: 'London',
            state: '',
            country_code: 'GB',
            postal_code: 'SW1Y 4DL',
            phone: '+44 20 0000 0000',
            email: 'ada@colleagues.example.com',
            web_page: '',
            change_reason_code: 'common.rectification',
            change_commentary: 'moved desk',
        });
    });

    it('answers with the record as written, in camelCase', async () => {
        const reply = await updateSelfContactInformation(
            callerAnswering({ result: okResult, account_contact_information: contactRow }),
            contactWrite,
        );

        expect(reply.contact?.version).toBe(4);
        expect(reply.contact?.city).toBe('London');
        expect(reply.contact?.countryCode).toBe('GB');
        expect(reply.contact?.recordedAt).toBe(RECORDED_AT);
    });

    it('answers null for the record when the write was refused', async () => {
        const reply = await updateSelfContactInformation(
            callerAnswering({
                result: {
                    outcome: 'invalid',
                    code: 'invalid_field_value',
                    message: 'The email address is not valid.',
                    fields: [
                        {
                            field: 'email',
                            code: 'invalid_field_value',
                            message: 'Not a valid address.',
                        },
                    ],
                },
                account_contact_information: null,
            }),
            contactWrite,
        );

        expect(reply.result.outcome).toBe('invalid');
        expect(reply.result.fields).toHaveLength(1);
        expect(reply.contact).toBeNull();
    });
});

describe('updateAccount', () => {
    const account = accountSchema.parse({
        version: 7,
        id: ACCOUNT_ID,
        tenantId: TENANT_ID,
        username: 'ada',
        fullName: 'Ada Lovelace',
        email: 'ada@example.com',
        accountType: 'user',
        jobTitle: 'Head of Desk',
        reportsToAccountId: MANAGER_ID,
        defaultPartyId: null,
        imageId: IMAGE_ID,
        modifiedBy: 'ada',
        changeReasonCode: 'common.non_material_update',
        changeCommentary: '',
        performedBy: 'ada',
        recordedAt: RECORDED_AT,
    });

    it('echoes the fields the screen does not set, and blanks a null one', async () => {
        const sent: Recorded[] = [];
        await updateAccount(callerAnswering({ success: true, message: '' }, sent), account, {
            ...profileWrite,
            fullName: 'Ada Byron',
            imageId: '',
        });

        expect(sent[0]?.subject).toBe('iam.v1.accounts.update');
        expect(sent[0]?.body).toEqual({
            account_id: ACCOUNT_ID,
            email: 'ada@example.com',
            full_name: 'Ada Byron',
            default_party_id: '',
            job_title: 'Chief Analyst',
            reports_to_account_id: MANAGER_ID,
            image_id: '',
            change_reason_code: 'common.non_material_update',
            change_commentary: 'promotion',
        });
    });

    it('resolves when the server accepts the write', async () => {
        await expect(
            updateAccount(callerAnswering({ success: true, message: '' }), account, profileWrite),
        ).resolves.toBeUndefined();
    });

    it('raises the server message when the body reports failure', async () => {
        const caller = callerAnswering({
            success: false,
            message: 'reports_to_account_id is not yours',
        });
        await expect(updateAccount(caller, account, profileWrite)).rejects.toThrow(
            OperationFailedError,
        );
        await expect(updateAccount(caller, account, profileWrite)).rejects.toThrow(
            'reports_to_account_id is not yours',
        );
    });
});

describe('readMyContactInformation', () => {
    it('reads the self subject and names no account', async () => {
        const sent: Recorded[] = [];
        const contact = await readMyContactInformation(
            callerAnswering({ result: okResult, account_contact_information: contactRow }, sent),
        );

        expect(sent[0]?.subject).toBe('iam.v1.account_contact_informations.mine');
        expect(sent[0]?.body).toEqual({});
        expect(contact?.id).toBe(RECORD_ID);
        expect(contact?.city).toBe('London');
    });

    it('answers nothing when the person has no record', async () => {
        const contact = await readMyContactInformation(
            callerAnswering({ result: okResult, account_contact_information: null }),
        );

        expect(contact).toBeNull();
    });

    it('raises when the read was not answered ok', async () => {
        const caller = callerAnswering({
            result: { outcome: 'failed', code: 'internal_error', message: 'The read failed.' },
            account_contact_information: null,
        });
        await expect(readMyContactInformation(caller)).rejects.toThrow(OperationFailedError);
    });
});

describe('readContactInformation', () => {
    it('reads the account by id with the direct scope', async () => {
        const sent: Recorded[] = [];
        const contact = await readContactInformation(
            callerAnswering(
                { result: okResult, account_contact_informations: [contactRow], total: 1 },
                sent,
            ),
            ACCOUNT_ID,
        );

        expect(sent[0]?.subject).toBe('iam.v1.account_contact_informations.list_by_account_id');
        expect(sent[0]?.body).toEqual({
            account_id: ACCOUNT_ID,
            scope: 'direct',
            offset: 0,
            limit: 1,
            order: { field: '', descending: false },
            filter: null,
        });
        expect(contact?.id).toBe(RECORD_ID);
        expect(contact?.city).toBe('London');
    });

    it('answers nothing when the account has no record', async () => {
        const contact = await readContactInformation(
            callerAnswering({ result: okResult, account_contact_informations: [], total: 0 }),
            ACCOUNT_ID,
        );

        expect(contact).toBeNull();
    });

    it('raises when the read was not answered ok', async () => {
        const caller = callerAnswering({
            result: { outcome: 'failed', code: 'internal_error', message: 'account not found' },
            account_contact_informations: [],
            total: 0,
        });
        await expect(readContactInformation(caller, ACCOUNT_ID)).rejects.toThrow(
            OperationFailedError,
        );
        await expect(readContactInformation(caller, ACCOUNT_ID)).rejects.toThrow(
            'account not found',
        );
    });
});

describe('putContactInformation', () => {
    it('sends the record whole and claims the version the panel read', async () => {
        const sent: Recorded[] = [];
        await putContactInformation(
            callerAnswering({ result: okResult, account_contact_information: contactRow }, sent),
            {
                accountId: ACCOUNT_ID,
                recordId: RECORD_ID,
                claim: { kind: 'must_match_version', version: 4 },
                write: contactWrite,
            },
        );

        expect(sent[0]?.subject).toBe('iam.v1.account_contact_informations.put');
        expect(sent[0]?.body).toEqual({
            change: {
                write: {
                    id: RECORD_ID,
                    account_id: ACCOUNT_ID,
                    street_line_1: '1 Panton Street',
                    street_line_2: '',
                    city: 'London',
                    state: '',
                    country_code: 'GB',
                    postal_code: 'SW1Y 4DL',
                    phone: '+44 20 0000 0000',
                    email: 'ada@colleagues.example.com',
                    web_page: '',
                },
                precondition: { kind: 'must_match_version', version: 4 },
            },
            intent: {
                reason_code: 'common.rectification',
                commentary: 'moved desk',
            },
        });
    });

    it('claims no record when the panel showed none', async () => {
        const sent: Recorded[] = [];
        await putContactInformation(
            callerAnswering({ result: okResult, account_contact_information: contactRow }, sent),
            {
                accountId: ACCOUNT_ID,
                recordId: RECORD_ID,
                claim: { kind: 'must_not_exist', version: null },
                write: contactWrite,
            },
        );

        const body = sent[0]?.body as {
            change: { precondition: { kind: string; version: number | null } };
        };
        expect(body.change.precondition).toEqual({ kind: 'must_not_exist', version: null });
    });

    it('answers a refused claim rather than raising it', async () => {
        const reply = await putContactInformation(
            callerAnswering({
                result: {
                    outcome: 'failed',
                    code: 'internal_error',
                    message: 'precondition failed: version mismatch',
                    fields: [],
                },
                account_contact_information: null,
            }),
            {
                accountId: ACCOUNT_ID,
                recordId: RECORD_ID,
                claim: { kind: 'must_match_version', version: 3 },
                write: contactWrite,
            },
        );

        expect(reply.result.outcome).toBe('failed');
        expect(reply.result.message).toBe('precondition failed: version mismatch');
        expect(reply.contact).toBeNull();
    });

    it('answers with the record as written when the store accepts the claim', async () => {
        const reply = await putContactInformation(
            callerAnswering({ result: okResult, account_contact_information: contactRow }),
            {
                accountId: ACCOUNT_ID,
                recordId: RECORD_ID,
                claim: { kind: 'must_match_version', version: 4 },
                write: contactWrite,
            },
        );

        expect(reply.result.outcome).toBe('ok');
        expect(reply.contact?.version).toBe(4);
    });
});

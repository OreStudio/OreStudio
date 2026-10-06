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

import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import { isUuid, type OresClient, type SessionMode } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The profile routes: the member's two self writes and their contact read,
 * the administrator's two administered writes, and the picture's upload.
 *
 * A refusal the panel must draw -- a field a member does not own, a record
 * that moved -- answers 200 with the result in the body, so each of those is
 * asserted as an answer rather than as a failed call.
 */

const ACCOUNT_ID = '11111111-1111-1111-1111-111111111111';
const TENANT_ID = '44444444-4444-4444-4444-444444444444';
const RECORD_ID = '22222222-2222-2222-2222-222222222222';
const MANAGER_ID = '33333333-3333-3333-3333-333333333333';
const IMAGE_ID = '55555555-5555-5555-5555-555555555555';
const RECORDED_AT = '2026-10-05 09:30:00Z';

const config: Config = {
    port: 0,
    host: '127.0.0.1',
    logLevel: 'silent',
    session: { ttlSeconds: 3600, cookieSecure: false },
    allowedOrigins: [],
    loginAttemptsPerMinute: 100,
};

function siteConfiguration(): ReturnType<typeof loadSiteConfiguration> {
    const path = resolve(
        dirname(fileURLToPath(import.meta.url)),
        '../../../config/environments.json',
    );
    return loadSiteConfiguration({
        environment: { [SITE_CONFIG_VARIABLE]: path },
        environmentId: 'brave_hopper',
    });
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

/** A server whose broker is a stub that answers each subject as told. */
function buildTestServer(answerFor: (subject: string) => unknown) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const client = {
        enteredTenant: undefined,
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push({ subject, body });
            return schema.parse(answerFor(subject));
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const party = {
        id: '66666666-6666-6666-6666-666666666666',
        name: 'System Party',
        partyCategory: 'System',
        businessCenterCode: '',
    };
    const own = {
        accountId: ACCOUNT_ID,
        tenantId: TENANT_ID,
        tenantName: 'Acme Corporation',
        version: 'v0.0.25 (test)',
        username: 'ada',
        email: 'ada@example.com',
        availableParties: [party],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: '77777777-7777-7777-7777-777777777777',
    };
    const session = sessions.create({
        ...own,
        client,
        session: { ...own, kind: 'active', token: 't', party },
        mode: 'application' as SessionMode,
    });
    const server = buildServer({
        config,
        site: siteConfiguration(),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
    return { server, cookies: { ores_web_session: session.id }, calls };
}

/** The answers a profile screen's routes expect, keyed by subject. */
function profileAnswers(overrides: Record<string, unknown> = {}) {
    const answers: Record<string, unknown> = {
        'iam.v1.accounts.update-self': { result: okResult, account: accountRow },
        'iam.v1.accounts.get': { account: accountRow },
        'iam.v1.accounts.update': { success: true, message: '' },
        'iam.v1.account_contact_informations.update-self': {
            result: okResult,
            account_contact_information: contactRow,
        },
        'iam.v1.account_contact_informations.mine': {
            result: okResult,
            account_contact_information: contactRow,
        },
        'iam.v1.account_contact_informations.list_by_account_id': {
            result: okResult,
            account_contact_informations: [contactRow],
            total: 1,
        },
        'iam.v1.account_contact_informations.put': {
            result: okResult,
            account_contact_information: contactRow,
        },
        'assets.v1.images.upload': { result: okResult, image_id: IMAGE_ID },
        'assets.v1.images.upload-policy': {
            result: okResult,
            formats: ['image/png', 'image/jpeg', 'image/webp'],
            max_size_bytes: 2097152,
            min_width: 128,
            min_height: 128,
        },
    };
    return (subject: string): unknown => overrides[subject] ?? answers[subject];
}

describe('PUT /api/me/profile', () => {
    const payload = {
        fullName: 'Ada Lovelace',
        jobTitle: 'Chief Analyst',
        imageId: IMAGE_ID,
        reasonCode: 'common.non_material_update',
        commentary: 'promotion',
    };

    it('sends the member fields and states the unowned ones empty', async () => {
        const { server, cookies, calls } = buildTestServer(profileAnswers());

        const response = await server.inject({
            method: 'PUT',
            url: '/api/me/profile',
            cookies,
            payload,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(calls).toEqual([
            {
                subject: 'iam.v1.accounts.update-self',
                body: {
                    full_name: 'Ada Lovelace',
                    job_title: 'Chief Analyst',
                    image_id: IMAGE_ID,
                    email: '',
                    default_party_id: '',
                    reports_to_account_id: '',
                    change_reason_code: 'common.non_material_update',
                    change_commentary: 'promotion',
                },
            },
        ]);
        expect(response.json().account.fullName).toBe('Ada Lovelace');
        expect(response.json().account.imageId).toBe(IMAGE_ID);
    });

    it('answers a refusal in the body, field failures whole', async () => {
        const { server, cookies } = buildTestServer(
            profileAnswers({
                'iam.v1.accounts.update-self': {
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
                },
            }),
        );

        const response = await server.inject({
            method: 'PUT',
            url: '/api/me/profile',
            cookies,
            payload,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json().result.code).toBe('field_not_self_writable');
        expect(response.json().result.fields).toHaveLength(1);
        expect(response.json().account).toBeNull();
    });
});

describe('GET /api/me/contact-information', () => {
    it('reads the session account without naming it', async () => {
        const { server, cookies, calls } = buildTestServer(profileAnswers());

        const response = await server.inject({
            method: 'GET',
            url: '/api/me/contact-information',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(calls[0]?.subject).toBe('iam.v1.account_contact_informations.mine');
        expect(calls[0]?.body).toEqual({});
        expect(response.json().contact.city).toBe('London');
    });

    it('answers nothing for an account with no record', async () => {
        const { server, cookies } = buildTestServer(
            profileAnswers({
                'iam.v1.account_contact_informations.mine': {
                    result: okResult,
                    account_contact_information: null,
                },
            }),
        );

        const response = await server.inject({
            method: 'GET',
            url: '/api/me/contact-information',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json().contact).toBeNull();
    });
});

describe('PUT /api/me/contact-information', () => {
    it('sends the nine fields and the reason, naming no record', async () => {
        const { server, cookies, calls } = buildTestServer(profileAnswers());

        const response = await server.inject({
            method: 'PUT',
            url: '/api/me/contact-information',
            cookies,
            payload: contactWrite,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(calls[0]?.subject).toBe('iam.v1.account_contact_informations.update-self');
        expect(calls[0]?.body).toEqual({
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
        expect(response.json().contact.version).toBe(4);
    });
});

describe('PUT /api/accounts/:username/profile', () => {
    const payload = {
        fullName: 'Ada Byron',
        jobTitle: 'Chief Analyst',
        imageId: '',
        reasonCode: 'common.rectification',
        commentary: '',
    };

    it('reads the account and writes it whole, echoing what it does not set', async () => {
        const { server, cookies, calls } = buildTestServer(profileAnswers());

        const response = await server.inject({
            method: 'PUT',
            url: '/api/accounts/ada/profile',
            cookies,
            payload,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(calls[0]).toEqual({
            subject: 'iam.v1.accounts.get',
            body: { key: { username: 'ada' } },
        });
        expect(calls[1]).toEqual({
            subject: 'iam.v1.accounts.update',
            body: {
                account_id: ACCOUNT_ID,
                email: 'ada@example.com',
                full_name: 'Ada Byron',
                default_party_id: '',
                job_title: 'Chief Analyst',
                reports_to_account_id: MANAGER_ID,
                image_id: '',
                change_reason_code: 'common.rectification',
                change_commentary: '',
            },
        });
    });

    it('answers 404 for a username nothing holds, and writes nothing', async () => {
        const { server, cookies, calls } = buildTestServer(
            profileAnswers({ 'iam.v1.accounts.get': { account: null } }),
        );

        const response = await server.inject({
            method: 'PUT',
            url: '/api/accounts/nobody/profile',
            cookies,
            payload,
        });
        await server.close();

        expect(response.statusCode).toBe(404);
        expect(calls).toHaveLength(1);
    });
});

describe('GET /api/accounts/:accountId/contact-information', () => {
    it('reads the account the administrator names', async () => {
        const { server, cookies, calls } = buildTestServer(profileAnswers());

        const response = await server.inject({
            method: 'GET',
            url: `/api/accounts/${RECORD_ID}/contact-information`,
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(calls[0]?.body).toMatchObject({ account_id: RECORD_ID });
        expect(response.json().contact.city).toBe('London');
    });

    it('answers 404 for an identifier that is not one', async () => {
        const { server, cookies } = buildTestServer(profileAnswers());

        const response = await server.inject({
            method: 'GET',
            url: '/api/accounts/not-an-identifier/contact-information',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(404);
    });
});

describe('PUT /api/accounts/:accountId/contact-information', () => {
    it('reuses the record id it read and claims the version the panel read', async () => {
        const { server, cookies, calls } = buildTestServer(profileAnswers());

        const response = await server.inject({
            method: 'PUT',
            url: `/api/accounts/${ACCOUNT_ID}/contact-information`,
            cookies,
            payload: { ...contactWrite, version: 4 },
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(calls[0]?.subject).toBe('iam.v1.account_contact_informations.list_by_account_id');
        const put = calls[1]?.body as {
            change: {
                write: { id: string; account_id: string };
                precondition: { kind: string; version: number | null };
            };
            intent: { reason_code: string; commentary: string };
        };
        expect(put.change.write.id).toBe(RECORD_ID);
        expect(put.change.write.account_id).toBe(ACCOUNT_ID);
        expect(put.change.precondition).toEqual({ kind: 'must_match_version', version: 4 });
        expect(put.intent).toEqual({
            reason_code: 'common.rectification',
            commentary: 'moved desk',
        });
        expect(response.json().contact.version).toBe(4);
    });

    it('mints an id and claims no record when the account has none', async () => {
        const { server, cookies, calls } = buildTestServer(
            profileAnswers({
                'iam.v1.account_contact_informations.list_by_account_id': {
                    result: okResult,
                    account_contact_informations: [],
                    total: 0,
                },
            }),
        );

        const response = await server.inject({
            method: 'PUT',
            url: `/api/accounts/${ACCOUNT_ID}/contact-information`,
            cookies,
            payload: { ...contactWrite, version: null },
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        const put = calls[1]?.body as {
            change: {
                write: { id: string };
                precondition: { kind: string; version: number | null };
            };
        };
        expect(isUuid(put.change.write.id)).toBe(true);
        expect(put.change.write.id).not.toBe(RECORD_ID);
        expect(put.change.precondition).toEqual({ kind: 'must_not_exist', version: null });
    });

    it('answers a refused claim in the body, with the words the store sent', async () => {
        const { server, cookies } = buildTestServer(
            profileAnswers({
                'iam.v1.account_contact_informations.put': {
                    result: {
                        outcome: 'failed',
                        code: 'internal_error',
                        message: 'Version conflict: expected version 3, but current version is 4',
                        fields: [],
                    },
                    account_contact_information: null,
                },
            }),
        );

        const response = await server.inject({
            method: 'PUT',
            url: `/api/accounts/${ACCOUNT_ID}/contact-information`,
            cookies,
            payload: { ...contactWrite, version: 3 },
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json().result.outcome).toBe('failed');
        expect(response.json().result.message).toContain('Version conflict');
        expect(response.json().contact).toBeNull();
    });

    it('answers 404 for an identifier that is not one', async () => {
        const { server, cookies, calls } = buildTestServer(profileAnswers());

        const response = await server.inject({
            method: 'PUT',
            url: '/api/accounts/not-an-identifier/contact-information',
            cookies,
            payload: { ...contactWrite, version: 4 },
        });
        await server.close();

        expect(response.statusCode).toBe(404);
        expect(calls).toEqual([]);
    });
});

describe('POST /api/images', () => {
    it('sends the media type and the base64 bytes', async () => {
        const { server, cookies, calls } = buildTestServer(profileAnswers());

        const response = await server.inject({
            method: 'POST',
            url: '/api/images',
            cookies,
            payload: { mimeType: 'image/png', data: 'aGVsbG8=' },
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(calls[0]).toEqual({
            subject: 'assets.v1.images.upload',
            body: { mime_type: 'image/png', data: 'aGVsbG8=' },
        });
        expect(response.json().imageId).toBe(IMAGE_ID);
    });

    it('answers a refused image in the body rather than failing the call', async () => {
        const { server, cookies } = buildTestServer(
            profileAnswers({
                'assets.v1.images.upload': {
                    result: {
                        outcome: 'invalid',
                        code: 'unsupported_media_type',
                        message: 'Only PNG, JPEG and WebP are accepted.',
                        fields: [],
                    },
                    image_id: '',
                },
            }),
        );

        const response = await server.inject({
            method: 'POST',
            url: '/api/images',
            cookies,
            payload: { mimeType: 'image/gif', data: 'aGVsbG8=' },
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json().result.code).toBe('unsupported_media_type');
        expect(response.json().imageId).toBe('');
    });

    it('refuses a body with no bytes', async () => {
        const { server, cookies } = buildTestServer(profileAnswers());

        const response = await server.inject({
            method: 'POST',
            url: '/api/images',
            cookies,
            payload: { mimeType: 'image/png', data: '' },
        });
        await server.close();

        expect(response.statusCode).toBe(400);
    });
});

describe('GET /api/image-upload-policy', () => {
    it('answers the rule the server enforces', async () => {
        const { server, cookies, calls } = buildTestServer(profileAnswers());

        const response = await server.inject({
            method: 'GET',
            url: '/api/image-upload-policy',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(calls[0]?.subject).toBe('assets.v1.images.upload-policy');
        expect(response.json()).toEqual({
            formats: ['image/png', 'image/jpeg', 'image/webp'],
            maxSizeBytes: 2097152,
            minWidth: 128,
            minHeight: 128,
        });
    });
});

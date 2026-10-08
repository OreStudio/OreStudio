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
import type { OresClient } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * The inbox routes: a member's own requests, the queue an administrator
 * answers from, and the notification bell.
 *
 * The server checks every permission, so these cases pin what the BFF decides:
 * the page it answers, the request it sends, that a body it cannot read is
 * refused before anything is sent, and that a join the caller may not read
 * leaves their own request readable rather than failing the whole page.
 */

const config: Config = {
    port: 0,
    host: '127.0.0.1',
    logLevel: 'silent',
    session: { ttlSeconds: 3600, cookieSecure: false },
    allowedOrigins: [],
    loginAttemptsPerMinute: 100,
};

/** The environment this file's site configuration serves. */
const ENVIRONMENT_ID = 'eager_maxwell';
const SESSION_COOKIE = sessionCookieName(ENVIRONMENT_ID);

function siteConfiguration(): ReturnType<typeof loadSiteConfiguration> {
    const path = resolve(
        dirname(fileURLToPath(import.meta.url)),
        '../../../config/environments.json',
    );
    return loadSiteConfiguration({
        environment: { [SITE_CONFIG_VARIABLE]: path },
        environmentId: ENVIRONMENT_ID,
    });
}

const PRIYA = '11111111-1111-4111-8111-111111111111';
const DANIEL = '22222222-2222-4222-8222-222222222222';
const TRADING = '33333333-3333-4333-8333-333333333333';
const AUDIT = '44444444-4444-4444-8444-444444444444';
const REQUEST = '55555555-5555-4555-8555-555555555555';
const NOTICE = '66666666-6666-4666-8666-666666666666';
const SYSTEM_TENANT = 'ffffffff-ffff-4fff-8fff-ffffffffffff';

const OK = { outcome: 'ok' };

/** One approval request as the inbox serves it: no role name, no account name. */
function wireRequest(id: string, requestedBy: string) {
    return {
        version: 3,
        tenant_id: SYSTEM_TENANT,
        id,
        kind_code: 'iam.role_grant',
        state_code: 'waiting',
        requested_by: requestedBy,
        requested_at: '2026-10-04 09:00:00Z',
        reason: 'I need the desk role',
        expires_at: null,
        modified_by: 'daniel',
        performed_by: 'daniel',
        change_reason_code: 'system.new_record',
        change_commentary: '',
        recorded_at: '2026-10-04 09:00:00Z',
    };
}

/**
 * The reads that put the names back in.
 *
 * Every one of them is a read a plain member may be refused, which is why
 * each case declares only the ones it means to answer.
 */
function joins() {
    return {
        'iam.v1.ops.get_request_roles': {
            result: OK,
            roles: [
                {
                    role: { id: TRADING, name: 'Trading', description: 'Trading role' },
                    asked_at: '2026-10-04 09:00:00Z',
                    applied_at: null,
                    applied_by: '',
                },
            ],
        },
        'iam.v1.role_grant_requests.list': {
            result: OK,
            role_grant_requests: [{ request_id: REQUEST, account_id: DANIEL }],
            total: 1,
        },
        'iam.v1.accounts.list': {
            result: OK,
            accounts: [{ id: DANIEL, username: 'daniel' }],
            total: 1,
        },
        'inbox.v1.approval_decisions.list': { result: OK, decisions: [], total: 0 },
    };
}

/** What the server answers a read the caller may not make. */
const DENIED = { outcome: 'denied', code: 'forbidden', message: 'Not permitted' };

type Answer = unknown | ((body: unknown) => unknown);

function buildTestServer(answers: Record<string, Answer>) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const calls: { subject: string; body: unknown }[] = [];
    const client = {
        async callAuthenticated(
            subject: string,
            body: unknown,
            schema: { parse: (value: unknown) => unknown },
        ): Promise<unknown> {
            calls.push({ subject, body });
            const answer = answers[subject];
            if (answer === undefined) throw new Error(`unexpected subject ${subject}`);
            return schema.parse(typeof answer === 'function' ? answer(body) : answer);
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: null,
        username: 'priya',
        email: 'priya@acme.example',
        accountId: PRIYA,
        tenantId: SYSTEM_TENANT,
        tenantName: 'Acme',
        mode: 'tenant-administration',
        version: 'v0.0.25 (test)',
        availableParties: [
            {
                id: SYSTEM_TENANT,
                name: 'Acme',
                partyCategory: 'Operational',
                businessCenterCode: 'GBLO',
            },
        ],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: '77777777-7777-4777-8777-777777777777',
    });
    const server = buildServer({
        config,
        site: siteConfiguration(),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
    return { server, cookies: { [SESSION_COOKIE]: session.id }, calls };
}

describe('the story of one request', () => {
    /** The request's own versions, as the entitled history read answers them. */
    const history = {
        result: OK,
        versions: [
            {
                version: 2,
                modified_by: 'priya',
                performed_by: 'ores.inbox.service',
                recorded_at: '2026-10-05 10:00:00Z',
                change_reason_code: 'system.update',
                change_commentary: 'Decision: approve',
                fields: [
                    { name: 'State Code', value: 'approved' },
                    { name: 'Reason', value: 'I need the desk role' },
                ],
            },
            {
                version: 1,
                modified_by: 'daniel',
                performed_by: 'ores.inbox.service',
                recorded_at: '2026-10-04 09:00:00Z',
                change_reason_code: 'system.new_record',
                change_commentary: '',
                fields: [
                    { name: 'State Code', value: 'waiting' },
                    { name: 'Reason', value: 'I need the desk role' },
                ],
            },
        ],
    };

    /** The role the request asked for, and what IAM did with it. */
    const roles = {
        result: OK,
        roles: [
            {
                role: { id: TRADING, name: 'Trading', description: 'Trading role' },
                asked_at: '2026-10-04 09:00:00Z',
                applied_at: '2026-10-05 10:00:00Z',
                applied_by: 'priya',
            },
        ],
    };

    const decision = {
        result: OK,
        decisions: [
            {
                request_id: REQUEST,
                decision_code: 'approve',
                decided_by: PRIYA,
                decided_at: '2026-10-05 10:00:00Z',
                comment: 'Desk needs it',
            },
        ],
        total: 1,
    };

    /** Two notices: one about this request, one about something else. */
    const notices = {
        result: OK,
        notifications: [
            {
                id: NOTICE,
                kind_code: 'inbox.approval_waiting',
                message_key: 'notification.inbox.approval_waiting',
                raised_by: 'ores.inbox.service',
                raised_at: '2026-10-04 09:00:01Z',
                link_route: 'requests',
                link_id: REQUEST,
                arguments: [{ name: 'requester', value: 'daniel' }],
                read_at: '',
            },
            {
                id: PRIYA,
                kind_code: 'inbox.approval_waiting',
                message_key: 'notification.inbox.approval_waiting',
                raised_by: 'ores.inbox.service',
                raised_at: '2026-10-04 09:00:02Z',
                link_route: 'requests',
                link_id: PRIYA,
                arguments: [],
                read_at: '',
            },
        ],
        total: 2,
    };

    async function storyOf(answers: Record<string, unknown>) {
        const { server, cookies } = buildTestServer(answers);
        const response = await server.inject({
            method: 'GET',
            url: `/api/requests/${REQUEST}/story`,
            cookies,
        });
        await server.close();
        return response;
    }

    it('merges every row the request wrote into one stream, newest first', async () => {
        const response = await storyOf({
            'inbox.v1.ops.get_approval_history': history,
            'inbox.v1.ops.get_request_roles': roles,
            'inbox.v1.approval_decisions.list': decision,
            'inbox.v1.ops.list_my_notifications': notices,
        });

        expect(response.statusCode).toBe(200);
        const story = response.json();
        expect(story.requestId).toBe(REQUEST);
        expect(story.events.map((event: { kind: string }) => event.kind)).toEqual([
            'granted',
            'decided',
            'changed',
            'told',
            'asked',
            'raised',
        ]);
        const told = story.events.filter(
            (event: { entityType: string }) =>
                event.entityType === 'ores.inbox.notification',
        );
        expect(told).toHaveLength(1);
        expect(told[0].entityId).toBe(NOTICE);
    });

    it('leaves the answer out of a story whose reader may not read it', async () => {
        const response = await storyOf({
            'inbox.v1.ops.get_approval_history': history,
            'inbox.v1.ops.get_request_roles': roles,
            'inbox.v1.approval_decisions.list': { result: DENIED },
            'inbox.v1.ops.list_my_notifications': notices,
        });

        expect(response.statusCode).toBe(200);
        const kinds = response.json().events.map((event: { kind: string }) => event.kind);
        // The request closed, so somebody answered it; the answer is simply not
        // this reader's to see, and the story says so by leaving it out.
        expect(kinds).not.toContain('decided');
        expect(kinds).toContain('changed');
    });

    it('answers a request the reader may not open as absent', async () => {
        const response = await storyOf({
            'inbox.v1.ops.get_approval_history': { result: { outcome: 'missing' } },
        });

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ requestId: REQUEST, events: [] });
    });
});

describe('inbox routes', () => {
    it('answers the signed-in person own requests with the roles asked for and the decider name', async () => {
        const { server, cookies } = buildTestServer({
            'inbox.v1.ops.list_my_approval_requests': {
                result: OK,
                requests: [wireRequest(REQUEST, DANIEL)],
                total: 1,
            },
            ...joins(),
            'inbox.v1.approval_decisions.list': {
                result: OK,
                decisions: [
                    {
                        request_id: REQUEST,
                        decision_code: 'approve',
                        decided_by: PRIYA,
                        decided_at: '2026-10-05 10:00:00Z',
                        comment: 'Desk needs it',
                    },
                ],
                total: 1,
            },
        });

        const response = await server.inject({ method: 'GET', url: '/api/me/requests', cookies });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            items: [
                {
                    id: REQUEST,
                    version: 3,
                    kindCode: 'iam.role_grant',
                    stateCode: 'waiting',
                    requestedBy: 'daniel',
                    requestedAt: '2026-10-04 09:00:00Z',
                    reason: 'I need the desk role',
                    expiresAt: '',
                    roles: [{ roleId: TRADING, name: 'Trading', description: 'Trading role' }],
                    decision: {
                        decisionCode: 'approve',
                        decidedBy: PRIYA,
                        decidedAt: '2026-10-05 10:00:00Z',
                        comment: 'Desk needs it',
                    },
                },
            ],
            total: 1,
        });
    });

    it('still answers a request when every join behind it is refused', async () => {
        const { server, cookies } = buildTestServer({
            'inbox.v1.ops.list_my_approval_requests': {
                result: OK,
                requests: [wireRequest(REQUEST, DANIEL)],
                total: 1,
            },
            'iam.v1.ops.get_request_roles': { result: DENIED },
            'iam.v1.role_grant_requests.list': { result: DENIED },
            'inbox.v1.approval_decisions.list': { result: DENIED },
        });

        const response = await server.inject({ method: 'GET', url: '/api/me/requests', cookies });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            items: [
                {
                    id: REQUEST,
                    version: 3,
                    kindCode: 'iam.role_grant',
                    stateCode: 'waiting',
                    requestedBy: DANIEL,
                    requestedAt: '2026-10-04 09:00:00Z',
                    reason: 'I need the desk role',
                    expiresAt: '',
                    roles: [],
                    decision: null,
                },
            ],
            total: 1,
        });
    });

    it('reads the roles of each request, once each, through the entitled operation', async () => {
        const SECOND = '99999999-9999-4999-8999-999999999999';
        const { server, cookies, calls } = buildTestServer({
            'inbox.v1.ops.list_my_approval_requests': {
                result: OK,
                requests: [wireRequest(REQUEST, PRIYA), wireRequest(SECOND, PRIYA)],
                total: 2,
            },
            'iam.v1.ops.get_request_roles': { result: DENIED },
            'iam.v1.role_grant_requests.list': { result: DENIED },
            'inbox.v1.approval_decisions.list': { result: DENIED },
        });

        const response = await server.inject({ method: 'GET', url: '/api/me/requests', cookies });
        await server.close();

        expect(response.statusCode).toBe(200);
        const reads = calls.filter((call) => call.subject === 'iam.v1.ops.get_request_roles');
        expect(reads.map((call) => (call.body as { request_id: string }).request_id)).toEqual([
            REQUEST,
            SECOND,
        ]);
    });

    it('sends the page the screen asked for', async () => {
        const { server, cookies, calls } = buildTestServer({
            'inbox.v1.ops.list_my_approval_requests': { result: OK, requests: [], total: 0 },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/me/requests?offset=20&limit=10',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ items: [], total: 0 });
        expect(calls[0]).toEqual({
            subject: 'inbox.v1.ops.list_my_approval_requests',
            body: { offset: 20, limit: 10 },
        });
        expect(calls).toHaveLength(1);
    });

    it('asks for the roles the person named, for the reason they gave', async () => {
        const { server, cookies, calls } = buildTestServer({
            'iam.v1.ops.ask_for_roles': { result: OK, request_id: REQUEST },
        });

        const response = await server.inject({
            method: 'POST',
            url: '/api/me/requests',
            cookies,
            payload: { roleIds: [TRADING, AUDIT], reason: 'Desk cover' },
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ requestId: REQUEST });
        expect(calls).toEqual([
            {
                subject: 'iam.v1.ops.ask_for_roles',
                body: { role_ids: [TRADING, AUDIT], reason: 'Desk cover' },
            },
        ]);
    });

    it('asks for a role before sending anything when none is named', async () => {
        const { server, cookies, calls } = buildTestServer({});

        const response = await server.inject({
            method: 'POST',
            url: '/api/me/requests',
            cookies,
            payload: { roleIds: [], reason: 'Desk cover' },
        });
        await server.close();

        expect(response.statusCode).toBe(400);
        expect(response.json().message).toBe('Asking for roles names at least one role, and why.');
        expect(calls).toHaveLength(0);
    });

    it('passes on the server words when it refuses an ask', async () => {
        const { server, cookies } = buildTestServer({
            'iam.v1.ops.ask_for_roles': {
                result: {
                    outcome: 'denied',
                    code: 'forbidden',
                    message: 'You already hold Trading.',
                },
            },
        });

        const response = await server.inject({
            method: 'POST',
            url: '/api/me/requests',
            cookies,
            payload: { roleIds: [TRADING], reason: 'Desk cover' },
        });
        await server.close();

        expect(response.statusCode).toBe(409);
        expect(response.json().message).toBe('You already hold Trading.');
    });

    it('takes back a request against the version the person read', async () => {
        const { server, cookies, calls } = buildTestServer({
            'inbox.v1.ops.withdraw_approval': { result: OK, request: null },
        });

        const response = await server.inject({
            method: 'DELETE',
            url: `/api/me/requests/${REQUEST}`,
            cookies,
            payload: { version: 3, comment: 'Sorted it myself' },
        });
        await server.close();

        expect(response.statusCode).toBe(204);
        expect(calls).toEqual([
            {
                subject: 'inbox.v1.ops.withdraw_approval',
                body: { request_id: REQUEST, version: 3, comment: 'Sorted it myself' },
            },
        ]);
    });

    it('refuses to withdraw a request named by something that is not an identifier', async () => {
        const { server, cookies, calls } = buildTestServer({});

        const response = await server.inject({
            method: 'DELETE',
            url: '/api/me/requests/not-an-identifier',
            cookies,
            payload: { version: 3, comment: '' },
        });
        await server.close();

        expect(response.statusCode).toBe(400);
        expect(calls).toHaveLength(0);
    });

    it('answers the queue the server picked, and the answered tail it carried', async () => {
        const { server, cookies } = buildTestServer({
            'inbox.v1.ops.list_approval_queue': {
                result: OK,
                requests: [wireRequest(REQUEST, DANIEL)],
                total: 1,
                answered: [],
            },
            ...joins(),
        });

        const response = await server.inject({ method: 'GET', url: '/api/requests', cookies });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            items: [
                {
                    id: REQUEST,
                    version: 3,
                    kindCode: 'iam.role_grant',
                    stateCode: 'waiting',
                    requestedBy: 'daniel',
                    requestedAt: '2026-10-04 09:00:00Z',
                    reason: 'I need the desk role',
                    expiresAt: '',
                    roles: [{ roleId: TRADING, name: 'Trading', description: 'Trading role' }],
                    decision: null,
                },
            ],
            total: 1,
            answered: [],
        });
    });

    it('answers the requests just answered, with what was decided on them', async () => {
        const { server, cookies } = buildTestServer({
            'inbox.v1.ops.list_approval_queue': {
                result: OK,
                requests: [],
                total: 0,
                answered: [wireRequest(REQUEST, DANIEL)],
            },
            ...joins(),
            'inbox.v1.approval_decisions.list': {
                result: OK,
                decisions: [
                    {
                        request_id: REQUEST,
                        decision_code: 'approve',
                        decided_by: PRIYA,
                        decided_at: '2026-10-05 10:00:00Z',
                        comment: 'Desk needs it',
                    },
                ],
                total: 1,
            },
        });

        const response = await server.inject({ method: 'GET', url: '/api/requests', cookies });
        await server.close();

        expect(response.statusCode).toBe(200);
        const body = response.json();
        expect(body.items).toEqual([]);
        expect(body.answered).toEqual([
            {
                id: REQUEST,
                version: 3,
                kindCode: 'iam.role_grant',
                stateCode: 'waiting',
                requestedBy: 'daniel',
                requestedAt: '2026-10-04 09:00:00Z',
                reason: 'I need the desk role',
                expiresAt: '',
                roles: [{ roleId: TRADING, name: 'Trading', description: 'Trading role' }],
                decision: {
                    decisionCode: 'approve',
                    decidedBy: PRIYA,
                    decidedAt: '2026-10-05 10:00:00Z',
                    comment: 'Desk needs it',
                },
            },
        ]);
    });

    it('decides a request against the version read, in the server own words', async () => {
        const { server, cookies, calls } = buildTestServer({
            'inbox.v1.ops.decide_approval': { result: OK, request: null },
        });

        const response = await server.inject({
            method: 'POST',
            url: `/api/requests/${REQUEST}/decision`,
            cookies,
            payload: { version: 3, decisionCode: 'refuse', comment: 'Not this desk' },
        });
        await server.close();

        expect(response.statusCode).toBe(204);
        expect(calls).toEqual([
            {
                subject: 'inbox.v1.ops.decide_approval',
                body: {
                    request_id: REQUEST,
                    version: 3,
                    decision_code: 'refuse',
                    comment: 'Not this desk',
                },
            },
        ]);
    });

    it('refuses a decision nobody could have reached, before deciding anything', async () => {
        const { server, cookies, calls } = buildTestServer({});

        const response = await server.inject({
            method: 'POST',
            url: `/api/requests/${REQUEST}/decision`,
            cookies,
            payload: { version: 3, decisionCode: 'maybe', comment: '' },
        });
        await server.close();

        expect(response.statusCode).toBe(400);
        expect(response.json().message).toBe(
            'A decision is approve, refuse, hold or resume, against the version read.',
        );
        expect(calls).toHaveLength(0);
    });

    it('answers the notifications, unread only when the bell asks for that', async () => {
        const { server, cookies, calls } = buildTestServer({
            'inbox.v1.ops.list_my_notifications': {
                result: OK,
                notifications: [
                    {
                        id: NOTICE,
                        kind_code: 'iam.role_grant_decided',
                        message_key: 'inbox.role_decided',
                        raised_by: 'priya',
                        raised_at: '2026-10-05 10:00:00Z',
                        link_route: '/me/requests',
                        link_id: REQUEST,
                        arguments: [{ name: 'role', value: 'Trading' }],
                        read_at: '',
                    },
                ],
                total: 1,
            },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/me/notifications?unreadOnly=true&limit=5',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({
            items: [
                {
                    id: NOTICE,
                    kindCode: 'iam.role_grant_decided',
                    messageKey: 'inbox.role_decided',
                    raisedBy: 'priya',
                    raisedAt: '2026-10-05 10:00:00Z',
                    linkRoute: '/me/requests',
                    linkId: REQUEST,
                    arguments: [{ name: 'role', value: 'Trading' }],
                    readAt: '',
                },
            ],
            total: 1,
        });
        expect(calls[0]?.body).toEqual({ unread_only: true, offset: 0, limit: 5 });
    });

    it('answers how many notifications are unread', async () => {
        const { server, cookies, calls } = buildTestServer({
            'inbox.v1.ops.count_unread_notifications': { result: OK, unread: 3 },
        });

        const response = await server.inject({
            method: 'GET',
            url: '/api/me/notifications/unread-count',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ unread: 3 });
        expect(calls).toEqual([{ subject: 'inbox.v1.ops.count_unread_notifications', body: {} }]);
    });

    it('marks the notifications the bell named read', async () => {
        const { server, cookies, calls } = buildTestServer({
            'inbox.v1.ops.mark_notifications_read': { result: OK, marked: 2 },
        });

        const response = await server.inject({
            method: 'POST',
            url: '/api/me/notifications/read',
            cookies,
            payload: { ids: [NOTICE, REQUEST] },
        });
        await server.close();

        expect(response.statusCode).toBe(200);
        expect(response.json()).toEqual({ marked: 2 });
        expect(calls).toEqual([
            {
                subject: 'inbox.v1.ops.mark_notifications_read',
                body: { notification_ids: [NOTICE, REQUEST] },
            },
        ]);
    });

    it('reads an empty list as every notification, for marking and for clearing', async () => {
        const { server, cookies, calls } = buildTestServer({
            'inbox.v1.ops.mark_notifications_read': { result: OK, marked: 4 },
            'inbox.v1.ops.clear_notifications': { result: OK, cleared: 4 },
        });

        const marked = await server.inject({
            method: 'POST',
            url: '/api/me/notifications/read',
            cookies,
            payload: { ids: [] },
        });
        const cleared = await server.inject({
            method: 'POST',
            url: '/api/me/notifications/clear',
            cookies,
            payload: { ids: [] },
        });
        await server.close();

        expect(marked.json()).toEqual({ marked: 4 });
        expect(cleared.json()).toEqual({ cleared: 4 });
        expect(calls).toEqual([
            { subject: 'inbox.v1.ops.mark_notifications_read', body: { notification_ids: [] } },
            { subject: 'inbox.v1.ops.clear_notifications', body: { notification_ids: [] } },
        ]);
    });

    it('refuses a read or a clear that forgot to name the notifications', async () => {
        const { server, cookies, calls } = buildTestServer({});

        const read = await server.inject({
            method: 'POST',
            url: '/api/me/notifications/read',
            cookies,
            payload: {},
        });
        const cleared = await server.inject({
            method: 'POST',
            url: '/api/me/notifications/clear',
            cookies,
            payload: {},
        });
        await server.close();

        expect(read.statusCode).toBe(400);
        expect(read.json().message).toBe(
            'Marking notifications read names the ones to mark, or an empty list for all unread.',
        );
        expect(cleared.statusCode).toBe(400);
        expect(cleared.json().message).toBe(
            'Clearing notifications names the ones to clear, or an empty list for all read.',
        );
        expect(calls).toHaveLength(0);
    });

    it('refuses a page larger than the inbox serves, before reading anything', async () => {
        const { server, cookies, calls } = buildTestServer({});

        const response = await server.inject({
            method: 'GET',
            url: '/api/me/requests?limit=101',
            cookies,
        });
        await server.close();

        expect(response.statusCode).toBe(400);
        expect(response.json().message).toBe('A page names an offset and a limit of 1 to 100.');
        expect(calls).toHaveLength(0);
    });

    it('refuses every inbox route without a session', async () => {
        const { server, cookies } = buildTestServer({
            'inbox.v1.ops.count_unread_notifications': { result: OK, unread: 0 },
        });

        const withoutCookie = await Promise.all([
            server.inject({ method: 'GET', url: '/api/me/requests' }),
            server.inject({
                method: 'POST',
                url: '/api/me/requests',
                payload: { roleIds: [TRADING], reason: '' },
            }),
            server.inject({ method: 'GET', url: '/api/requests' }),
            server.inject({ method: 'GET', url: '/api/me/notifications' }),
            server.inject({ method: 'GET', url: '/api/me/notifications/unread-count' }),
            server.inject({
                method: 'POST',
                url: '/api/me/notifications/read',
                payload: { ids: [] },
            }),
            server.inject({
                method: 'POST',
                url: '/api/me/notifications/clear',
                payload: { ids: [] },
            }),
        ]);
        const withCookie = await server.inject({
            method: 'GET',
            url: '/api/me/notifications/unread-count',
            cookies,
        });
        await server.close();

        expect(withoutCookie.map((response) => response.statusCode)).toEqual([
            401, 401, 401, 401, 401, 401, 401,
        ]);
        expect(withCookie.statusCode).toBe(200);
    });
});

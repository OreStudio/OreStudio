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

import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { afterEach, describe, expect, it } from 'vitest';
import {
    agileDocResponseSchema,
    backlogSchema,
    boardResponseSchema,
    chartsResponseSchema,
    sprintListSchema,
} from '@ores/contracts';
import type { OresClient } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { createSessionStore } from './sessions.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

const config: Config = {
    port: 0,
    host: '127.0.0.1',
    logLevel: 'silent',
    session: { ttlSeconds: 3600, cookieSecure: false },
    allowedOrigins: [],
    loginAttemptsPerMinute: 100,
};

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

function buildTestServer(options: { docRoot?: string }) {
    const sessions = createSessionStore({ ttlSeconds: 60 });
    const client = {
        async callAuthenticated(): Promise<unknown> {
            throw new Error('The QA routes make no broker call.');
        },
        async close(): Promise<void> {
            return undefined;
        },
    } as unknown as OresClient;
    const session = sessions.create({
        client,
        session: null,
        username: 'trader',
        email: 'trader@acme.example',
        accountId: '11111111-1111-1111-1111-111111111111',
        tenantId: '44444444-4444-4444-4444-444444444444',
        tenantName: 'Acme',
        mode: 'tenant-administration',
        version: 'v0.0.25 (test)',
        availableParties: [],
        accessLifetimeSeconds: 1800,
        passwordResetRequired: false,
        sessionId: '33333333-3333-3333-3333-333333333333',
    });
    const server = buildServer({
        config: options.docRoot === undefined ? config : { ...config, docRoot: options.docRoot },
        site: siteConfiguration(),
        sessions,
        createClient: () => ({ client, connect: async () => undefined }),
    });
    return { server, sessionId: session.id };
}

function send(
    server: ReturnType<typeof buildServer>,
    sessionId: string | null,
    url: string,
): ReturnType<ReturnType<typeof buildServer>['inject']> {
    return server.inject({
        method: 'GET',
        url,
        ...(sessionId === null ? {} : { cookies: { [SESSION_COOKIE]: sessionId } }),
    });
}

const STORY = 'A1000000-0000-4000-8000-000000000001';
const TASK = 'A1000000-0000-4000-8000-000000000002';
const SPRINT = 'A1000000-0000-4000-8000-000000000003';
const CAPTURE = 'A1000000-0000-4000-8000-000000000004';
const SCENARIO = 'A1000000-0000-4000-8000-000000000005';
const RECIPE = 'A1000000-0000-4000-8000-000000000006';

const roots: string[] = [];
afterEach(() => {
    for (const root of roots.splice(0)) rmSync(root, { recursive: true, force: true });
});

function write(root: string, path: string, text: string): void {
    const file = join(root, path);
    mkdirSync(dirname(file), { recursive: true });
    writeFileSync(file, text);
}

function doc(id: string, type: string, title: string, extra = ''): string {
    return `:PROPERTIES:\n:ID: ${id}\n:END:\n#+title: ${title}\n#+type: ${type}\n#+created: 2026-10-01\n${extra}\n`;
}

function tree(): string {
    const root = mkdtempSync(join(tmpdir(), 'ores-agile-'));
    roots.push(root);
    const sprint = 'agile/versions/v0/sprint_1';
    write(
        root,
        `${sprint}/sprint.org`,
        doc(
            SPRINT,
            'sprint',
            'Sprint 1',
            '* Dates\n\n| Field | Value |\n|---+---|\n| Start | 2026-10-01 |\n| End | 2026-10-14 |\n',
        ),
    );
    write(
        root,
        `${sprint}/first_story/story.org`,
        doc(
            STORY,
            'story',
            'Story: First',
            '* Status\n\n| Field | Value |\n|---+---|\n| State | STARTED |\n',
        ),
    );
    write(
        root,
        `${sprint}/first_story/task_one.org`,
        doc(
            TASK,
            'task',
            'Task: One',
            '* Status\n\n| Field | Value |\n|---+---|\n| State | DONE |\n',
        ),
    );
    write(root, 'agile/product_backlog/inbox/note.org', doc(CAPTURE, 'capture', 'A note'));
    write(root, 'agile/sprint_1_scenario.org', doc(SCENARIO, 'test_scenario', 'A scenario'));
    write(root, 'agile/a_recipe.org', doc(RECIPE, 'recipe', 'A recipe'));
    return root;
}

describe('the agile routes', () => {
    it('need a signed-in session', async () => {
        const { server } = buildTestServer({ docRoot: tree() });
        for (const url of [
            '/api/agile/sprints',
            '/api/agile/board',
            '/api/agile/charts',
            '/api/agile/backlog',
            `/api/agile/docs/${STORY}`,
        ]) {
            expect((await send(server, null, url)).statusCode).toBe(401);
        }
    });

    it('answer not found when no doc root is set', async () => {
        const { server, sessionId } = buildTestServer({});
        for (const url of ['/api/agile/sprints', '/api/agile/board', '/api/agile/backlog']) {
            expect((await send(server, sessionId, url)).statusCode).toBe(404);
        }
        expect((await send(server, sessionId, `/api/agile/docs/${STORY}`)).statusCode).toBe(404);
    });

    it('list the sprints', async () => {
        const { server, sessionId } = buildTestServer({ docRoot: tree() });
        const response = await send(server, sessionId, '/api/agile/sprints');
        expect(response.statusCode).toBe(200);
        expect(sprintListSchema.parse(response.json()).sprints.map((s) => s.name)).toEqual([
            'sprint_1',
        ]);
    });

    it('serve the newest sprint, or the one that is named', async () => {
        const { server, sessionId } = buildTestServer({ docRoot: tree() });
        for (const url of ['/api/agile/board', '/api/agile/board?sprint=sprint_1']) {
            const response = await send(server, sessionId, url);
            expect(response.statusCode).toBe(200);
            const { board } = boardResponseSchema.parse(response.json());
            expect(board.sprint.name).toBe('sprint_1');
            expect(board.stories.map((s) => s.title)).toEqual(['First']);
            expect(board.stories[0]?.tasks.map((t) => t.state)).toEqual(['DONE']);
        }
    });

    it('refuse a sprint name that is a path, and miss a sprint that is not there', async () => {
        const { server, sessionId } = buildTestServer({ docRoot: tree() });
        const path = await send(server, sessionId, '/api/agile/board?sprint=..%2F..%2Fetc');
        expect(path.statusCode).toBe(400);
        const twice = await send(
            server,
            sessionId,
            '/api/agile/board?sprint=sprint_1&sprint=sprint_2',
        );
        expect(twice.statusCode).toBe(400);
        const missing = await send(server, sessionId, '/api/agile/board?sprint=sprint_99');
        expect(missing.statusCode).toBe(404);
    });

    it('serve the charts of the newest sprint, and refuse a bad sprint name', async () => {
        const { server, sessionId } = buildTestServer({ docRoot: tree() });
        const response = await send(server, sessionId, '/api/agile/charts');
        expect(response.statusCode).toBe(200);
        const { charts } = chartsResponseSchema.parse(response.json());
        expect(charts.velocity.map((point) => point.sprint)).toEqual(['sprint_1']);
        expect(
            (await send(server, sessionId, '/api/agile/charts?sprint=..%2Fetc')).statusCode,
        ).toBe(400);
        expect(
            (await send(server, sessionId, '/api/agile/charts?sprint=sprint_9')).statusCode,
        ).toBe(404);
        const twice = await send(
            server,
            sessionId,
            '/api/agile/charts?sprint=sprint_1&sprint=sprint_2',
        );
        expect(twice.statusCode).toBe(400);
    });

    it('serve the backlog buckets', async () => {
        const { server, sessionId } = buildTestServer({ docRoot: tree() });
        const response = await send(server, sessionId, '/api/agile/backlog');
        expect(response.statusCode).toBe(200);
        const backlog = backlogSchema.parse(response.json());
        expect(backlog.buckets.map((b) => b.id)).toEqual([
            'inbox',
            'next',
            'deferred',
            'discarded',
        ]);
        expect(backlog.buckets[0]?.items.map((i) => i.title)).toEqual(['A note']);
    });

    it('serve a sprint, a story, a task, a capture and a scenario by id', async () => {
        const { server, sessionId } = buildTestServer({ docRoot: tree() });
        for (const [id, type] of [
            [SPRINT, 'sprint'],
            [STORY, 'story'],
            [TASK, 'task'],
            [CAPTURE, 'capture'],
            [SCENARIO, 'test_scenario'],
        ] as const) {
            const response = await send(server, sessionId, `/api/agile/docs/${id}`);
            expect(response.statusCode).toBe(200);
            expect(agileDocResponseSchema.parse(response.json()).doc.type).toBe(type);
        }
    });

    it('do not serve a doc of another type, or an id that is a path', async () => {
        const { server, sessionId } = buildTestServer({ docRoot: tree() });
        expect((await send(server, sessionId, `/api/agile/docs/${RECIPE}`)).statusCode).toBe(404);
        const path = await send(server, sessionId, '/api/agile/docs/..%2F..%2Fetc%2Fpasswd');
        expect([400, 404]).toContain(path.statusCode);
    });
});

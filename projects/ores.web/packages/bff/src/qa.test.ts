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

import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { afterEach, describe, expect, it } from 'vitest';
import {
    qaDocResponseSchema,
    runResponseSchema,
    scenarioListSchema,
    scenarioResponseSchema,
} from '@ores/contracts';
import type { OresClient } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer, sessionCookieName } from './server.js';
import { parseScenario, recordRun, ScenarioFormatError } from './scenarios/scenario-doc.js';
import type { RunInput, Scenario } from './scenarios/scenario-doc.js';
import { InvalidDocIdError, ScenarioNotFoundError } from './scenarios/scenario-store.js';
import type { DocText, ScenarioStore, ScenarioSummary } from './scenarios/scenario-store.js';
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

const fixture = (name: string): string =>
    readFileSync(new URL(`../../org/src/fixtures/${name}`, import.meta.url), 'utf8');

const SINGLE = 'A607D53A-66E3-4211-AE3B-069A603F509F';
const MULTI = 'B12A8474-32FF-45B7-BFD8-FF1B42845DAD';
const STORY = 'FE07BF4D-054D-4A69-AF3C-D70D10493370';
const RECIPE = 'CCCCCCCC-CCCC-4CCC-8CCC-CCCCCCCCCCCC';

const docs = new Map<string, { type: string | null; title: string; path: string; text: string }>([
    [
        SINGLE,
        {
            type: 'test_scenario',
            title: 'Single',
            path: 'a/single.org',
            text: fixture('single_client_scenario.org'),
        },
    ],
    [
        MULTI,
        {
            type: 'test_scenario',
            title: 'Multi',
            path: 'a/multi.org',
            text: fixture('multi_client_scenario.org'),
        },
    ],
    [STORY, { type: 'story', title: 'A story', path: 'a/story.org', text: '* Goal\n' }],
    [RECIPE, { type: 'recipe', title: 'A recipe', path: 'a/recipe.org', text: '* Steps\n' }],
]);

/** The real codec over text held in memory, so no file is touched. */
class MemoryStore implements ScenarioStore {
    readonly texts = new Map<string, string>();
    failWith: Error | undefined;

    constructor() {
        for (const [id, doc] of docs) this.texts.set(id, doc.text);
    }

    private check(id: string): string {
        if (this.failWith !== undefined) throw this.failWith;
        if (!/^[0-9A-Fa-f-]{36}$/.test(id)) throw new InvalidDocIdError(id);
        return id.toUpperCase();
    }

    async list(): Promise<ScenarioSummary[]> {
        if (this.failWith !== undefined) throw this.failWith;
        const out: ScenarioSummary[] = [];
        for (const [id, text] of this.texts) {
            if (docs.get(id)?.type !== 'test_scenario') continue;
            const s = parseScenario(text);
            const count = (status: string) => s.steps.filter((x) => x.status === status).length;
            out.push({
                id,
                title: s.title,
                description: s.description,
                path: docs.get(id)?.path ?? '',
                state: s.state,
                phase: s.state === 'PENDING' ? 'waiting' : 'done',
                target: s.target,
                story: s.story,
                task: s.task,
                clients: s.clients,
                steps: {
                    total: s.steps.length,
                    pending: count('PENDING'),
                    pass: count('PASS'),
                    fail: count('FAIL'),
                    dropped: count('DROPPED'),
                },
                completedAt: s.run.completedAt,
            });
        }
        return out;
    }

    async read(id: string): Promise<Scenario | null> {
        const key = this.check(id);
        const text = this.texts.get(key);
        return text === undefined || docs.get(key)?.type !== 'test_scenario'
            ? null
            : parseScenario(text);
    }

    async readDoc(id: string): Promise<DocText | null> {
        const key = this.check(id);
        const doc = docs.get(key);
        const text = this.texts.get(key);
        return doc === undefined || text === undefined ? null : { id: key, ...doc, text };
    }

    async record(id: string, input: RunInput) {
        const key = this.check(id);
        const text = this.texts.get(key);
        if (text === undefined || docs.get(key)?.type !== 'test_scenario') {
            throw new ScenarioNotFoundError(key);
        }
        const recorded = recordRun(text, input);
        this.texts.set(key, recorded.text);
        return { scenario: parseScenario(recorded.text), changed: recorded.changed };
    }
}

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

function buildTestServer(options: { store?: ScenarioStore; docRoot?: string }) {
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
        ...(options.store === undefined ? {} : { scenarios: options.store }),
    });
    return { server, sessionId: session.id };
}

function send(
    server: ReturnType<typeof buildServer>,
    sessionId: string | null,
    method: 'GET' | 'PUT',
    url: string,
    payload?: unknown,
): ReturnType<ReturnType<typeof buildServer>['inject']> {
    return server.inject({
        method,
        url,
        ...(sessionId === null ? {} : { cookies: { [SESSION_COOKIE]: sessionId } }),
        ...(payload === undefined ? {} : { payload: payload as Record<string, unknown> }),
    });
}

const build = { branch: 'feature/x', commit: 'abc1234', worktree: 'eager_maxwell' };
const titlesOf = (id: string) => parseScenario(docs.get(id)?.text ?? '').steps;

describe('a request with no session', () => {
    it('is refused on every route', async () => {
        const { server } = buildTestServer({ store: new MemoryStore() });
        const calls: ['GET' | 'PUT', string][] = [
            ['GET', '/api/qa/scenarios'],
            ['GET', `/api/qa/scenarios/${SINGLE}`],
            ['GET', `/api/qa/docs/${STORY}`],
            ['PUT', `/api/qa/scenarios/${SINGLE}/run`],
        ];
        for (const [method, url] of calls) {
            const response = await send(
                server,
                null,
                method,
                url,
                method === 'PUT' ? {} : undefined,
            );
            expect(response.statusCode, url).toBe(401);
            expect(response.json().code).toBe('not-authenticated');
        }
    });
});

describe('reading', () => {
    it('lists the scenarios with their state and what is waiting', async () => {
        const { server, sessionId } = buildTestServer({ store: new MemoryStore() });
        const response = await send(server, sessionId, 'GET', '/api/qa/scenarios');
        expect(response.statusCode).toBe(200);
        const list = scenarioListSchema.parse(response.json());
        expect(list.scenarios.map((s) => s.id).sort()).toEqual([MULTI, SINGLE].sort());
        expect(list.scenarios.find((s) => s.id === MULTI)?.clients).toEqual(['blue', 'red']);
    });

    it('reads one scenario with its steps and the ids of its story and task', async () => {
        const { server, sessionId } = buildTestServer({ store: new MemoryStore() });
        const response = await send(server, sessionId, 'GET', `/api/qa/scenarios/${MULTI}`);
        const { scenario } = scenarioResponseSchema.parse(response.json());
        expect(scenario.steps).toHaveLength(13);
        expect(scenario.story?.id).toBe(STORY);
        expect(scenario.task?.id).toBe('8DC4ABA3-B053-4C40-B575-6EDFCF3C86DE');
    });

    it('answers not found for an id no scenario has, and for a story read as one', async () => {
        const { server, sessionId } = buildTestServer({ store: new MemoryStore() });
        for (const id of ['E0000000-0000-4000-8000-000000000003', STORY]) {
            const response = await send(server, sessionId, 'GET', `/api/qa/scenarios/${id}`);
            expect(response.statusCode).toBe(404);
            expect(response.json().code).toBe('not-found');
        }
    });

    it('refuses an id that is not a document id', async () => {
        const { server, sessionId } = buildTestServer({ store: new MemoryStore() });
        const response = await send(server, sessionId, 'GET', '/api/qa/scenarios/..%2F..%2Fetc');
        expect(response.statusCode).toBe(400);
        expect(response.json().code).toBe('invalid-request');
    });

    it('reads a story by id, and serves no other kind of doc', async () => {
        const { server, sessionId } = buildTestServer({ store: new MemoryStore() });
        const story = await send(server, sessionId, 'GET', `/api/qa/docs/${STORY}`);
        expect(qaDocResponseSchema.parse(story.json()).doc.text).toContain('* Goal');
        const recipe = await send(server, sessionId, 'GET', `/api/qa/docs/${RECIPE}`);
        expect(recipe.statusCode).toBe(404);
        expect(recipe.body).not.toContain('* Steps');
    });
});

describe('saving a run', () => {
    it('writes the named steps, stamps the time and says whether the scenario closed', async () => {
        const store = new MemoryStore();
        const { server, sessionId } = buildTestServer({ store });
        const steps = titlesOf(SINGLE).map((s) => ({
            client: s.client,
            title: s.title,
            status: 'PASS',
            notes: 'ok',
        }));
        const response = await send(server, sessionId, 'PUT', `/api/qa/scenarios/${SINGLE}/run`, {
            steps,
            ...build,
        });
        expect(response.statusCode).toBe(200);
        const body = runResponseSchema.parse(response.json());
        expect(body.state).toBe('PASSED');
        expect(body.changed).toHaveLength(5);
        expect(body.scenario.run.completedAt).toMatch(/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z$/);
        expect(body.scenario.run.branch).toBe('feature/x');
        expect(store.texts.get(SINGLE)).toContain('| Commit        | abc1234 |');
    });

    it('leaves a scenario open while a step is pending', async () => {
        const { server, sessionId } = buildTestServer({ store: new MemoryStore() });
        const response = await send(server, sessionId, 'PUT', `/api/qa/scenarios/${MULTI}/run`, {
            steps: [{ client: 'blue', title: 'Read', status: 'PASS', notes: '' }],
            ...build,
        });
        const body = runResponseSchema.parse(response.json());
        expect(body.state).toBe('PENDING');
        expect(body.scenario.run.completedAt).toBe('');
    });

    it('refuses a step the scenario does not have, names it, and writes nothing', async () => {
        const store = new MemoryStore();
        const before = store.texts.get(MULTI);
        const { server, sessionId } = buildTestServer({ store });
        const response = await send(server, sessionId, 'PUT', `/api/qa/scenarios/${MULTI}/run`, {
            steps: [
                { client: 'blue', title: 'Read', status: 'PASS', notes: '' },
                { client: 'blue', title: 'Frobnicate', status: 'PASS', notes: '' },
            ],
            ...build,
        });
        expect(response.statusCode).toBe(400);
        expect(response.json().message).toContain('Frobnicate');
        expect(store.texts.get(MULTI)).toBe(before);
    });

    it('refuses a step named twice', async () => {
        const { server, sessionId } = buildTestServer({ store: new MemoryStore() });
        const response = await send(server, sessionId, 'PUT', `/api/qa/scenarios/${MULTI}/run`, {
            steps: [
                { client: 'blue', title: 'Read', status: 'PASS', notes: '' },
                { client: 'blue', title: 'Read', status: 'FAIL', notes: '' },
            ],
            ...build,
        });
        expect(response.statusCode).toBe(400);
    });

    it('refuses a body that is not a run', async () => {
        const { server, sessionId } = buildTestServer({ store: new MemoryStore() });
        const step = { client: 'blue', title: 'Read', status: 'PASS', notes: '' };
        const bad: unknown[] = [
            {},
            { steps: [], ...build },
            { steps: [step], ...build, state: 'PASSED' },
            { steps: [step], ...build, completedAt: '2020-01-01T00:00:00Z' },
            { steps: [{ ...step, status: 'DROPPED' }], ...build },
            { steps: [{ ...step, title: '' }], ...build },
            { steps: [{ ...step, notes: 'x'.repeat(10_001) }], ...build },
            { steps: [step], branch: 1, commit: '', worktree: '' },
        ];
        for (const body of bad) {
            const response = await send(
                server,
                sessionId,
                'PUT',
                `/api/qa/scenarios/${MULTI}/run`,
                body,
            );
            expect(response.statusCode, JSON.stringify(body)).toBe(400);
        }
    });

    it('answers not found for a scenario that does not exist', async () => {
        const { server, sessionId } = buildTestServer({ store: new MemoryStore() });
        const response = await send(
            server,
            sessionId,
            'PUT',
            '/api/qa/scenarios/E0000000-0000-4000-8000-000000000003/run',
            { steps: [{ client: null, title: 'x', status: 'PASS', notes: '' }], ...build },
        );
        expect(response.statusCode).toBe(404);
    });
});

describe('when the store fails', () => {
    const request = (store: MemoryStore) => {
        const { server, sessionId } = buildTestServer({ store });
        return send(server, sessionId, 'GET', '/api/qa/scenarios');
    };

    it('answers 503 for a file error, so a retry can work', async () => {
        const store = new MemoryStore();
        store.failWith = Object.assign(new Error('EACCES: /srv/doc/secret path'), {
            code: 'EACCES',
        });
        const response = await request(store);
        expect(response.statusCode).toBe(503);
        expect(response.json().code).toBe('upstream-unavailable');
        expect(response.body).not.toContain('/srv/doc');
    });

    it('answers 500 for a Node error code that is not a file fault', async () => {
        const store = new MemoryStore();
        store.failWith = Object.assign(new Error('bad argument'), {
            code: 'ERR_INVALID_ARG_TYPE',
        });
        const response = await request(store);
        expect(response.statusCode).toBe(500);
    });

    it('answers 422 for a doc that is not in the scenario format', async () => {
        const store = new MemoryStore();
        store.failWith = new ScenarioFormatError('The scenario has no Steps section.');
        const response = await request(store);
        expect(response.statusCode).toBe(422);
    });

    it('answers 500 without the message for a fault nobody expected', async () => {
        const store = new MemoryStore();
        store.failWith = new Error('secret detail');
        const response = await request(store);
        expect(response.statusCode).toBe(500);
        expect(response.body).not.toContain('secret detail');
    });
});

describe('the doc root', () => {
    const roots: string[] = [];
    afterEach(() => {
        for (const root of roots.splice(0)) rmSync(root, { recursive: true, force: true });
    });

    it('turns the runner off when no store and no doc root are set', async () => {
        const { server, sessionId } = buildTestServer({});
        const response = await send(server, sessionId, 'GET', '/api/qa/scenarios');
        expect(response.statusCode).toBe(404);
        expect(response.json().message).toContain('ORES_WEB_DOC_ROOT');
    });

    it('reads the files under the doc root, and saves into them', async () => {
        const root = mkdtempSync(join(tmpdir(), 'ores-qa-'));
        roots.push(root);
        mkdirSync(join(root, 'sprint'));
        writeFileSync(
            join(root, 'sprint/scenario_single.org'),
            fixture('single_client_scenario.org'),
        );
        const { server, sessionId } = buildTestServer({ docRoot: root });
        const list = await send(server, sessionId, 'GET', '/api/qa/scenarios');
        expect(scenarioListSchema.parse(list.json()).scenarios[0]?.id).toBe(SINGLE);
        const first = titlesOf(SINGLE)[0];
        const saved = await send(server, sessionId, 'PUT', `/api/qa/scenarios/${SINGLE}/run`, {
            steps: [{ client: null, title: first?.title ?? '', status: 'FAIL', notes: 'broke' }],
            ...build,
        });
        expect(saved.statusCode).toBe(200);
        expect(readFileSync(join(root, 'sprint/scenario_single.org'), 'utf8')).toContain('broke');
    });
});

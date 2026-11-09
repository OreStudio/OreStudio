// SPDX-License-Identifier: GPL-3.0-or-later
import test from 'node:test';
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { get } from 'node:http';
import { Sessions } from '../dist/auth.js';
import { AppError, NatsBackend } from '../dist/backend.js';
import { createApp } from '../dist/server.js';
import { validateConfig } from '../dist/config.js';

const party = '11111111-1111-4111-8111-111111111111';
const config = {
    host: '127.0.0.1',
    port: 0,
    allowedOrigins: [],
    nats: null,
    inference: { baseUrl: 'http://127.0.0.1:20010/v1', model: 'fixture', timeoutMs: 1000 },
};
const input = (principal) => ({ principal, password: 'fixture' });
function backendFixture() {
    return {
        ready: () => true,
        close: async () => {},
        request: async (operation, args) => {
            if (operation === 'login')
                return {
                    success: args.password === 'fixture',
                    token: 'private-token',
                    session_id: 'fixture-session',
                    selected_party_id: party,
                };
            return {};
        },
    };
}

test('failed logins limit one principal; valid and malformed inputs do not exhaust other users', async () => {
    const sessions = new Sessions(backendFixture());
    for (let i = 0; i < 10; i++)
        await assert.rejects(sessions.login({ ...input('alice'), password: 'wrong' }), {
            code: 'login_failed',
        });
    await assert.rejects(sessions.login(input('alice')), { code: 'rate_limited' });
    for (let i = 0; i < 12; i++) {
        await assert.rejects(sessions.login({ principal: 'bob' }));
        const login = await sessions.login(input('bob'));
        await sessions.logout('Bearer ' + login.accessToken);
    }
    await sessions.close();
});

test('backend outages do not spend the failed-login budget', async () => {
    const backend = backendFixture(),
        login = backend.request;
    backend.request = async () => {
        throw new AppError('backend_unavailable', 'Unavailable.', 503);
    };
    const sessions = new Sessions(backend);
    for (let i = 0; i < 12; i++)
        await assert.rejects(sessions.login(input('alice')), { code: 'backend_unavailable' });
    backend.request = async () => ({ success: 'invalid' });
    await assert.rejects(sessions.login(input('alice')), { code: 'invalid_reply', status: 502 });
    backend.request = login;
    assert.ok((await sessions.login(input('alice'))).accessToken);
    await sessions.close();
});

test('login limits include pending requests and expire after one minute', async (t) => {
    t.mock.timers.enable({ apis: ['Date'], now: 1000 });
    const backend = backendFixture();
    let release;
    const gate = new Promise((resolve) => {
        release = resolve;
    });
    backend.request = async () => {
        await gate;
        return { success: false };
    };
    const sessions = new Sessions(backend);
    const pending = Promise.allSettled(
        Array.from({ length: 10 }, () => sessions.login(input('alice'))),
    );
    await assert.rejects(sessions.login(input('alice')), { code: 'rate_limited' });
    release();
    assert.ok((await pending).every((result) => result.status === 'rejected'));
    t.mock.timers.tick(60001);
    backend.request = backendFixture().request;
    assert.ok((await sessions.login(input('alice'))).accessToken);
    await sessions.close();
});

test('failed-login state is bounded and successful identities leave no rate-limit entries', async () => {
    const backend = backendFixture(),
        success = backend.request;
    const sessions = new Sessions(backend);
    // Enough successful identities to exceed the map limit if successful entries leak.
    for (let i = 0; i < 1025; i++) {
        const login = await sessions.login(input('user-' + i));
        await sessions.logout('Bearer ' + login.accessToken);
    }
    backend.request = async () => ({ success: false });
    for (let i = 0; i < 1024; i++)
        await assert.rejects(sessions.login(input('bad-' + i)), { code: 'login_failed' });
    await assert.rejects(sessions.login(input('overflow')), { code: 'capacity' });
    backend.request = success;
    await sessions.close();
});

test('selected-party lifetime caps session expiry without changing the backend reply', async (t) => {
    t.mock.timers.enable({ apis: ['Date'], now: 1000 });
    const reply = Object.freeze({
        success: true,
        token: 'old',
        session_id: 'fixture',
        available_parties: [{ id: party, name: 'Fixture' }],
        access_lifetime_s: 3600,
    });
    const backend = backendFixture();
    backend.request = async (op) =>
        op === 'login'
            ? reply
            : op === 'selectParty'
              ? { success: true, token: 'selected', access_lifetime_s: 60 }
              : {};
    const sessions = new Sessions(backend);
    const login = await sessions.login({ ...input('alice'), partyId: party });
    assert.equal(login.expiresAt, new Date(61000).toISOString());
    assert.equal(reply.access_lifetime_s, 3600);
    assert.equal(sessions.require('Bearer ' + login.accessToken).token, 'selected');
    await sessions.close();
});

test('NATS error headers use bounded status codes and never expose unknown text', async () => {
    let code;
    const connection = {
        status: async function* () {},
        request: async () => ({ headers: { get: () => code }, data: new Uint8Array() }),
    };
    const backend = new NatsBackend(connection, 'fixture');
    for (const [header, expectedCode, status] of [
        ['token_expired', 'session_expired', 401],
        ['max_session_exceeded', 'session_expired', 401],
        ['unauthorized', 'unauthorized', 401],
        ['forbidden', 'backend_denied', 403],
        ['bad_request', 'backend_bad_request', 400],
        ['internal_error private details', 'backend_error', 502],
    ]) {
        code = header;
        await assert.rejects(backend.request('currencies', {}), (error) => {
            assert.equal(error.code, expectedCode);
            assert.equal(error.status, status);
            assert.ok(!error.message.includes('private details'));
            return true;
        });
    }
});

test('configuration rejects invalid origins, wrong field types and unknown settings', () => {
    assert.deepEqual(validateConfig(config), config);
    assert.equal(
        validateConfig({
            ...config,
            host: '::1',
            allowedOrigins: ['http://[::1]:20100', 'https://chat.example'],
        }).host,
        '::1',
    );
    for (const value of [
        'null',
        '*',
        'not a URL',
        'file:///tmp',
        'https://user:pass@chat.example',
        'https://chat.example/path',
        'https://chat.example/',
        'https://chat.example?x=1',
        'https://chat.example#x',
    ]) {
        assert.throws(
            () => validateConfig({ ...config, allowedOrigins: [value] }),
            /allowedOrigins/,
        );
    }
    for (const value of [
        null,
        [],
        { ...config, port: '20100' },
        { ...config, extra: true },
        { ...config, inference: { ...config.inference, model: 123 } },
        { ...config, inference: { ...config.inference, timeoutMs: 100.5 } },
    ]) {
        assert.throws(() => validateConfig(value));
    }
});

test('originless authenticated clients work; IPv6 Host with port is accepted; shutdown awaits logout', async () => {
    const backend = backendFixture(),
        request = backend.request;
    let release, started;
    const gate = new Promise((resolve) => {
        release = resolve;
    });
    const logoutStarted = new Promise((resolve) => {
        started = resolve;
    });
    backend.request = async (...args) => {
        if (args[0] === 'logout') {
            started();
            await gate;
        }
        return request(...args);
    };
    const server = createApp(config, backend);
    server.listen(0, '127.0.0.1');
    await once(server, 'listening');
    const url = `http://127.0.0.1:${server.address().port}`;
    try {
        // fetch can replace Host; use the HTTP client to send the actual wire header.
        const hostStatus = (host) =>
            new Promise((resolve, reject) => {
                get(url + '/healthz', { headers: { Host: host } }, (response) => {
                    response.resume();
                    response.on('end', () => resolve(response.statusCode));
                }).on('error', reject);
            });
        assert.equal(await hostStatus('[::1]:20100'), 200);
        assert.equal(await hostStatus('attacker.example'), 403);
        assert.equal((await fetch(url + '/tools')).status, 401);
        const login = await (
            await fetch(url + '/auth/login', {
                method: 'POST',
                body: JSON.stringify(input('alice')),
            })
        ).json();
        const headers = { Authorization: 'Bearer ' + login.accessToken };
        assert.equal((await fetch(url + '/tools', { headers })).status, 200);
        assert.equal(
            (
                await fetch(url + '/tools', {
                    headers: { ...headers, Origin: 'https://attacker.example' },
                })
            ).status,
            403,
        );
        let complete = false;
        const shutdown = server.shutdown().then(() => {
            complete = true;
        });
        await logoutStarted;
        await new Promise((resolve) => setImmediate(resolve));
        assert.equal(complete, false);
        release();
        await shutdown;
        assert.equal(complete, true);
        await server.shutdown();
    } finally {
        release();
        server.closeAllConnections();
        await server.shutdown();
    }
});

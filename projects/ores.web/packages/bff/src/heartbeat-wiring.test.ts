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
 */

import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { afterEach, describe, expect, it, vi } from 'vitest';
import type { OresClient } from '@ores/wire-protocol';
import type { Config } from './config.js';
import { buildServer } from './server.js';
import { loadSiteConfiguration, SITE_CONFIG_VARIABLE } from './site-config.js';

/**
 * Whether the server tells the registry it is running.
 *
 * The heartbeat rides the process's shared connection, so these cases drive the
 * connection: it comes up, it is late, it fails once, and the server closes
 * before it settles.
 */

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
        environmentId: 'eager_maxwell',
    });
}

function fakeClient(): { client: OresClient; beats: { service_name: string; version: string }[] } {
    const beats: { service_name: string; version: string }[] = [];
    const client = {
        publishServiceHeartbeat: (message: { service_name: string; version: string }) => {
            beats.push(message);
        },
    } as unknown as OresClient;
    return { client, beats };
}

afterEach(() => {
    vi.useRealTimers();
});

describe('the server heartbeat', () => {
    it('starts once the connection is up, under the name and release it was given', async () => {
        const { client, beats } = fakeClient();
        const server = buildServer({
            config,
            site: siteConfiguration(),
            createClient: () => ({ client, connect: async () => undefined }),
            heartbeat: { version: '0.0.27' },
        });

        await server.ready();
        await vi.waitFor(() => expect(beats).toHaveLength(1));

        expect(beats[0]).toMatchObject({ service_name: 'ores.web.service', version: '0.0.27' });
        await server.close();
    });

    it('sends nothing when no heartbeat was asked for', async () => {
        const { client, beats } = fakeClient();
        const server = buildServer({
            config,
            site: siteConfiguration(),
            createClient: () => ({ client, connect: async () => undefined }),
        });

        await server.ready();
        await new Promise((resolveWait) => setTimeout(resolveWait, 20));

        expect(beats).toHaveLength(0);
        await server.close();
    });

    it('starts nothing when the server closes before the connection settles', async () => {
        const { client, beats } = fakeClient();
        let settle: () => void = () => undefined;
        const server = buildServer({
            config,
            site: siteConfiguration(),
            createClient: () => ({
                client,
                connect: () =>
                    new Promise<void>((resolveConnect) => {
                        settle = resolveConnect;
                    }),
            }),
            heartbeat: { version: '0.0.27' },
        });

        await server.ready();
        await server.close();
        settle();
        await new Promise((resolveWait) => setTimeout(resolveWait, 20));

        expect(beats).toHaveLength(0);
    });

    it('tries the connection again after it fails, and then starts', async () => {
        vi.useFakeTimers({ toFake: ['setTimeout'] });
        const { client, beats } = fakeClient();
        let attempts = 0;
        const server = buildServer({
            config,
            site: siteConfiguration(),
            createClient: () => ({
                client,
                connect: async () => {
                    attempts += 1;
                    if (attempts === 1) {
                        throw new Error('broker is not there yet');
                    }
                },
            }),
            heartbeat: { version: '0.0.27' },
        });

        await server.ready();
        await vi.advanceTimersByTimeAsync(6_000);
        await vi.waitFor(() => expect(beats).toHaveLength(1));

        expect(attempts).toBe(2);
        await server.close();
    });
});

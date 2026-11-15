// SPDX-License-Identifier: GPL-3.0-or-later
import { readFile } from 'node:fs/promises';
import { randomUUID } from 'node:crypto';
import { connect, headers } from 'nats';
import { encode } from '@msgpack/msgpack';
import { decodeReply, backendError } from '../dist/backend.js';
const allowed = new Set([
    'marketdata.v1.providers.get',
    'marketdata.v1.providers.put',
    'marketdata.v1.market_series.get',
    'marketdata.v1.market_series.put',
    'marketdata.v1.market_observations.get',
    'marketdata.v1.market_observations.put_many',
]);
export async function datasetNats(config) {
    const [ca, cert, key] = await Promise.all(
        [config.ca, config.cert, config.key].map((p) => readFile(p, 'utf8')),
    );
    const nc = await connect({
        servers: config.server,
        name: 'ores.agent.e2e-data-setup',
        tls: { ca, cert, key },
        timeout: 5000,
        maxReconnectAttempts: 0,
    });
    return {
        requestFor: (identity) => async (subject, payload) => {
            if (!allowed.has(subject)) throw Error('Operation is outside isolated dataset setup.');
            const h = headers();
            h.set('Authorization', 'Bearer ' + identity.token);
            h.set('Nats-Session-Id', identity.sessionId);
            h.set('Nats-Correlation-Id', randomUUID());
            const r = await nc.request(
                config.prefix + '.' + subject,
                encode(payload, { ignoreUndefined: true }),
                { headers: h, timeout: 8000 },
            );
            const error = r.headers?.get('X-Error');
            if (error) throw backendError(error);
            return decodeReply(r.data, r.headers?.get('X-Content-Encoding'));
        },
        close: () => nc.drain(),
    };
}

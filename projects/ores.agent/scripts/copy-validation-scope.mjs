// Operator integration check, outside the agent tool catalogue and packaged runtime.
// Explicit --apply performs one scope copy, never report execution or a mutation retry.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { randomUUID } from 'node:crypto';
import { connect, headers } from 'nats';
import { encode } from '@msgpack/msgpack';
import { loadConfig } from '../dist/config.js';
import { NatsBackend, decodeReply } from '../dist/backend.js';
import { Sessions } from '../dist/auth.js';

const [configPath, credentialsPath, partyId, sourceId, targetId, otherPartyId, apply] = process.argv.slice(2);
if (!configPath || !credentialsPath || !partyId || !sourceId || !targetId || !otherPartyId || apply !== '--apply') throw Error('Pass config, private credentials JSON, selected party, source definition, empty target definition, second party and --apply.');
for (const value of [partyId, sourceId, targetId, otherPartyId]) assert.match(value, /^[0-9a-f-]{36}$/i);
const config = await loadConfig(configPath), credentials = JSON.parse(await readFile(credentialsPath, 'utf8'));
const backend = await NatsBackend.open(config.nats), sessions = new Sessions(backend);
const [ca, cert, key] = await Promise.all([config.nats.ca, config.nats.cert, config.nats.key].map(path => readFile(path, 'utf8')));
const nc = await connect({ servers: config.nats.server, tls: { ca, cert, key }, timeout: 5000, reconnect: false });
const login = async party => {
    const auth = await sessions.login({ principal: credentials.tenantUsername, password: credentials.tenantPassword, partyId: party });
    return sessions.require('Bearer ' + auth.accessToken);
};
const copy = async (source, target, session) => {
    const h = headers(); h.set('Nats-Correlation-Id', randomUUID());
    if (session) { h.set('Authorization', 'Bearer ' + session.token); h.set('Nats-Session-Id', session.sessionId); }
    const reply = await nc.request(config.nats.prefix + '.reporting.v1.ops.copy_run_scope', encode({ source_definition_id: source, target_definition_id: target }), { headers: h, timeout: 8000 });
    if (reply.headers?.get('X-Error')) return { success: false, headerDenied: true };
    return decodeReply(reply.data, reply.headers?.get('X-Content-Encoding'));
};
try {
    const session = await login(partyId);
    assert.equal((await copy(sourceId, targetId)).success, false);
    assert.equal((await copy(sourceId, sourceId, session)).success, false);
    const other = await login(otherPartyId);
    assert.equal((await copy(sourceId, targetId, other)).success, false);
    // Reverse copy cannot alter the source's already populated/shared configuration.
    assert.equal((await copy(targetId, sourceId, session)).success, false);
    const result = await copy(sourceId, targetId, session);
    assert.equal(result.success, true, JSON.stringify(result));
    assert.ok(result.book_ids.length + result.portfolio_ids.length > 0);
    const replay = await copy(sourceId, targetId, session);
    assert.equal(replay.success, false, 'A repeat must refuse to overwrite a populated scope.');
    console.log(JSON.stringify({ copied: true, sourceId, targetId, bookIds: result.book_ids, portfolioIds: result.portfolio_ids, unauthenticatedDenied: true, secondPartyDenied: true, sameConfigurationDenied: true, populatedSharedDestinationDenied: true, replayDenied: true }));
} finally {
    try { await sessions.close(); } finally { try { await backend.close(); } finally { await nc.drain(); } }
}

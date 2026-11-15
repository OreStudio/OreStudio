// SPDX-License-Identifier: GPL-3.0-or-later
import assert from 'node:assert/strict';
import { mkdtemp, mkdir, readFile, writeFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { resolve, join, dirname, sep } from 'node:path';
import { createServer } from 'node:net';
import { spawn, execFileSync } from 'node:child_process';
import { once } from 'node:events';
import { setTimeout as delay } from 'node:timers/promises';
import { randomUUID } from 'node:crypto';
import { connect } from 'nats';
import { NatsBackend } from '../dist/backend.js';
import { Sessions } from '../dist/auth.js';
import { ToolExecutor } from '../dist/tools.js';
import { Agent } from '../dist/agent.js';
import { scriptedDialogue } from './fx-dialogue-fixtures.mjs';
import { tracedBackend } from '../dist/telemetry.js';
import { datasetNats } from '../scripts/fx-e2e-nats.mjs';
import { loadFxDataset } from '../scripts/fx-e2e-loader.mjs';
import { datasetQuery, checkDatasetResult } from './fx-e2e-dataset.mjs';
import { fixtureService, ciTenant, ciParty } from './fx-e2e-service.mjs';

const args = process.argv.slice(2);
if (args.length && !(args.length === 2 && args[0] === '--out'))
    throw Error('Usage: persisted-fx-ci.mjs [--out NEW_DIRECTORY]');
const out = resolve(
    args[1] ??
        `../../build/output/agent-nats/${new Date().toISOString().replace(/[:.]/g, '-')}-${randomUUID().slice(0, 8)}`,
);
await mkdir(dirname(out), { recursive: true });
await mkdir(out);
const scratch = await mkdtemp(join(tmpdir(), 'ores-agent-fx-nats-'));
let child,
    fixtureNc,
    service,
    backend,
    sessions,
    writer,
    brokerLog = '';
try {
    const openssl =
        process.env.OPENSSL_BIN ??
        (process.platform === 'win32' ? 'C:/Program Files/Git/usr/bin/openssl.exe' : 'openssl');
    const cert = join(scratch, 'cert.pem'),
        key = join(scratch, 'key.pem');
    execFileSync(
        openssl,
        [
            'req',
            '-x509',
            '-newkey',
            'rsa:2048',
            '-nodes',
            '-keyout',
            key,
            '-out',
            cert,
            '-days',
            '1',
            '-subj',
            '/CN=localhost',
            '-addext',
            'subjectAltName=DNS:localhost,IP:127.0.0.1',
        ],
        { stdio: 'pipe', windowsHide: true },
    );
    const probe = createServer();
    probe.listen(0, '127.0.0.1');
    await once(probe, 'listening');
    const port = probe.address().port;
    await new Promise((r) => probe.close(r));
    const configPath = join(scratch, 'nats.conf'),
        p = (s) => JSON.stringify(s.replaceAll('\\', '/'));
    await writeFile(
        configPath,
        `host: "127.0.0.1"\nport: ${port}\ntls {cert_file: ${p(cert)}, key_file: ${p(key)}, ca_file: ${p(cert)}, verify: true}\n`,
    );
    child = spawn(process.env.NATS_SERVER ?? 'nats-server', ['-c', configPath], {
        stdio: ['ignore', 'pipe', 'pipe'],
        windowsHide: true,
    });
    let startupError;
    child.once('error', (e) => {
        startupError = e;
    });
    const log = (data) => {
        brokerLog += data.toString();
    };
    child.stderr.on('data', log);
    child.stdout.on('data', log);
    for (let i = 0; i < 100 && !brokerLog.includes('Server is ready'); i++) {
        if (startupError) throw startupError;
        if (child.exitCode !== null) throw Error('Isolated broker exited during startup');
        await delay(50);
    }
    if (!brokerLog.includes('Server is ready'))
        throw Error('Isolated broker did not become ready.');
    const config = {
        server: `tls://127.0.0.1:${port}`,
        prefix: 'agentci.' + randomUUID(),
        ca: cert,
        cert,
        key,
    };
    const certificate = await readFile(cert, 'utf8'),
        privateKey = await readFile(key, 'utf8');
    fixtureNc = await connect({
        servers: config.server,
        tls: { ca: certificate, cert: certificate, key: privateKey },
        maxReconnectAttempts: 0,
    });
    service = await fixtureService(fixtureNc, config.prefix);
    backend = await NatsBackend.open(config);
    sessions = new Sessions(backend);
    const systemLogin = await sessions.login({
        principal: 'system',
        password: 'LABELLED CI fixture password',
    });
    const tenantLogin = await sessions.login({
        principal: 'tenant',
        password: 'LABELLED CI fixture password',
        partyId: ciParty,
    });
    const system = sessions.require('Bearer ' + systemLogin.accessToken),
        tenant = sessions.require('Bearer ' + tenantLogin.accessToken);
    writer = await datasetNats(config);
    const setup = {
        tenantId: ciTenant,
        partyId: ciParty,
        systemRequest: writer.requestFor(system),
        tenantRequest: writer.requestFor(tenant),
    };
    const plan = await loadFxDataset(setup);
    assert.equal(plan.missing.length, 13);
    assert.equal(service.writes, 0);
    const seed = await loadFxDataset({ ...setup, apply: true });
    assert.equal(seed.created, 13);
    assert.equal(service.writes, 13);
    const second = await loadFxDataset({ ...setup, apply: true });
    assert.equal(second.created, 0);
    assert.equal(service.writes, 13);
    const executor = new ToolExecutor(tracedBackend(backend), () => {}),
        trace = [],
        conversations = [];
    const agent = new Agent(
        scriptedDialogue(),
        executor,
        (event) => trace.push(event),
    );
    for (const user of [datasetQuery, 'use test data set conventions']) {
        const start = trace.length;
        const conversation = {
            environment: 'CI protocol fixture over real TLS NATS; not native OreStudio',
              inference: 'Scripted conversation decision; real LLM coverage uses the model evaluation profile.',
            user,
        };
        try {
            const reply = await agent.chat({ message: user }, tenant, AbortSignal.timeout(30000));
            conversation.assistant = reply.answer;
            if (user === datasetQuery) assert.match(reply.answer, /use test dataset conventions/);
            conversation.evidence = reply.evidence;
        } catch (error) {
            conversation.error = error.code ?? error.message;
            throw error;
        } finally {
            conversation.trace = trace.slice(start);
            conversations.push(conversation);
            await writeFile(
                join(out, 'conversations.jsonl'),
                conversations.map((c) => JSON.stringify(c)).join('\n') + '\n',
                { mode: 0o600 },
            );
        }
    }
    const failures = checkDatasetResult(conversations.at(-1).evidence[0]);
    assert.deepEqual(failures, []);
    assert.ok(trace.some((t) => t.phase === 'adapter'));
    assert.equal(service.writes, 13, 'The conversational agent must perform reads only.');
    await writeFile(
        join(out, 'conversations.jsonl'),
        conversations.map((c) => JSON.stringify(c)).join('\n') + '\n',
        { mode: 0o600 },
    );
    const result = {
        status: 'pass',
        transport: 'Real pinned NATS broker with TLS and MessagePack',
        server: 'Isolated protocol fixture service; not native OreStudio or production data',
        seed,
        repeatedSeed: second,
        independentReferenceChecks: 'pass',
        agentBackendReads: service.requests.filter(
            (r) => r.subject.endsWith('.list') || r.subject.endsWith('.list_by_series_id'),
        ).length,
        agentMutationCount: 0,
    };
    await writeFile(join(out, 'results.json'), JSON.stringify(result, null, 2) + '\n');
    console.log(JSON.stringify({ status: 'pass', reportDirectory: out, ...result }));
} catch (error) {
    await writeFile(
        join(out, 'results.json'),
        JSON.stringify(
            {
                status: 'fail',
                server: 'Isolated CI protocol fixture over real TLS NATS',
                error: error.code ?? error.message,
            },
            null,
            2,
        ) + '\n',
    );
    throw error;
} finally {
    await writer?.close();
    await sessions?.close();
    await backend?.close();
    service?.close();
    await fixtureNc?.drain();
    if (child?.pid && child.exitCode === null) {
        const closed = once(child, 'exit');
        child.kill();
        await closed;
    }
    await writeFile(join(out, 'broker.log'), brokerLog);
    const absolute = resolve(scratch);
    if (!absolute.startsWith(resolve(tmpdir()) + sep) || !absolute.includes('ores-agent-fx-nats-'))
        throw Error('Refusing unexpected temporary cleanup path');
    await rm(absolute, { recursive: true, force: true, maxRetries: 5, retryDelay: 100 });
}

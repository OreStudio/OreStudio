// SPDX-License-Identifier: GPL-3.0-or-later
// Independent, user-owned process supervisor; no PID-based signalling of strangers.
import { readFile, writeFile, mkdir, unlink } from 'node:fs/promises';
import { openSync, closeSync } from 'node:fs';
import { resolve, join, isAbsolute, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { randomBytes, timingSafeEqual, createHash } from 'node:crypto';
import { spawn } from 'node:child_process';
import { createServer } from 'node:http';
import { setTimeout as delay } from 'node:timers/promises';

async function main() {
    const [action, specArg] = process.argv.slice(2);
    if (!['start', 'stop', 'status', 'supervise'].includes(action) || !specArg) throw Error('Usage: node service.mjs <start|stop|status> <service.json>');
    const specPath = resolve(specArg), spec = JSON.parse(await readFile(specPath, 'utf8'));
    if (!['ores.agent', 'llama.cpp'].includes(spec.component) || !isAbsolute(spec.command) || !isAbsolute(spec.cwd) || !isAbsolute(spec.stateDir) || !Array.isArray(spec.args) || spec.args.some(x => typeof x !== 'string')) throw Error('Invalid service specification.');
    const health = new URL(spec.healthUrl);
    if (health.protocol !== 'http:' || health.hostname !== '127.0.0.1') throw Error('Service health must be on loopback.');
    await mkdir(spec.stateDir, { recursive: true });
    const statePath = join(spec.stateDir, 'process.json');
    const readState = async () => { try { return JSON.parse(await readFile(statePath, 'utf8')); } catch (e) { if (e.code === 'ENOENT') return null; throw e; } };
    async function control(state, operation) {
        const r = await fetch(`http://127.0.0.1:${state.controlPort}/${operation}`, { method: operation === 'stop' ? 'POST' : 'GET',
            headers: { Authorization: 'Bearer ' + state.secret }, signal: AbortSignal.timeout(2000) });
        if (!r.ok) throw Error('Supervisor rejected control request.');
        return r.json();
    }
    if (action === 'status' || action === 'stop') {
        const state = await readState();
        if (!state) { console.log(JSON.stringify({ component: spec.component, running: false })); return; }
        try { console.log(JSON.stringify(await control(state, action))); }
        catch { throw Error('Cannot authenticate the existing supervisor; no process was signalled. Inspect the service logs and stale process.json.'); }
        if (action === 'stop') for (let i = 0; i < 100 && await readState(); i++) await delay(100);
        return;
    }
    if (action === 'start') {
        if (await readState()) throw Error('Service has an existing process record; use status/stop before starting.');
        if (spec.manifestPath) {
            const manifest = JSON.parse(await readFile(spec.manifestPath, 'utf8'));
            if (Object.keys(manifest.files).sort().join(',') !== 'agent.mjs,service.mjs') throw Error('Unexpected release manifest.');
            for (const [name, hash] of Object.entries(manifest.files)) if (createHash('sha256').update(await readFile(join(dirname(spec.manifestPath), name))).digest('hex') !== hash) throw Error('Installed release checksum mismatch.');
        }
        const worker = spawn(process.execPath, [fileURLToPath(import.meta.url), 'supervise', specPath], { detached: true, windowsHide: true, stdio: 'ignore' });
        worker.unref();
        for (let i = 0; i < 300; i++) {
            await delay(100);
            const state = await readState();
            if (state) {
                try {
                    const status = await control(state, 'status');
                    if (status.running) {
                        const response = await fetch(spec.healthUrl, { signal: AbortSignal.timeout(1000) });
                        await response.body?.cancel();
                        if (response.ok) { console.log(JSON.stringify(status)); return; }
                    }
                } catch { /* Initial startup is bounded below. */ }
            }
        }
        throw Error('Service did not become healthy within 30 seconds; inspect logs and stop it before retrying.');
    }

    const secret = randomBytes(32).toString('hex');
    const out = openSync(join(spec.stateDir, 'stdout.log'), 'a', 0o600), err = openSync(join(spec.stateDir, 'stderr.log'), 'a', 0o600);
    const child = spawn(spec.command, spec.args, { cwd: spec.cwd, env: { ...process.env, ...spec.env }, windowsHide: true, stdio: ['ignore', out, err] });
    closeSync(out); closeSync(err);
    let stopping = false;
    const server = createServer((req, res) => {
        const supplied = Buffer.from(req.headers.authorization ?? ''), expected = Buffer.from('Bearer ' + secret);
        if (supplied.length !== expected.length || !timingSafeEqual(supplied, expected)) { res.writeHead(403); res.end(); return; }
        res.setHeader('content-type', 'application/json'); res.setHeader('cache-control', 'no-store');
        if (req.url === '/status' && req.method === 'GET') res.end(JSON.stringify({ component: spec.component, running: child.exitCode === null && !stopping, pid: child.pid }));
        else if (req.url === '/stop' && req.method === 'POST') { stopping = true; res.end(JSON.stringify({ component: spec.component, stopping: true })); child.kill('SIGTERM'); setTimeout(() => child.kill('SIGKILL'), 5000).unref(); }
        else { res.writeHead(404); res.end(); }
    });
    server.listen(0, '127.0.0.1', async () => {
        try { await writeFile(statePath, JSON.stringify({ supervisorPid: process.pid, childPid: child.pid, controlPort: server.address().port, secret }), { flag: 'wx', mode: 0o600 }); }
        catch { child.kill(); server.close(); process.exitCode = 1; }
    });
    async function cleanup() {
        const state = await readState();
        if (state?.secret === secret) await unlink(statePath).catch(() => {});
        server.close(); server.closeAllConnections();
    }
    child.once('exit', () => { void cleanup(); });
    child.once('error', () => { void cleanup().finally(() => { process.exitCode = 1; }); });
    for (const signal of ['SIGINT', 'SIGTERM']) process.on(signal, () => { stopping = true; child.kill('SIGTERM'); });
}
// Let pending libuv handle closures finish. Forced process.exit after fetch can abort on native Windows.
await main();

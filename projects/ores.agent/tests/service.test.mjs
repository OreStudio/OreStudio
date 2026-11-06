import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, writeFile, readFile, rm } from 'node:fs/promises';
import { createServer } from 'node:http';
import { once } from 'node:events';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
const exec = promisify(execFile);
test('independent supervisor starts/stops only its own child; invalid control credentials cannot stop it', { timeout: 45000 }, async () => {
    const root = await mkdtemp(join(tmpdir(), 'ores-agent-service-'));
    const unrelated = createServer((req, res) => res.end('backend fixture')); unrelated.listen(0, '127.0.0.1'); await once(unrelated, 'listening');
    const probe = createServer(); probe.listen(0, '127.0.0.1'); await once(probe, 'listening');
    const port = probe.address().port; await new Promise(r => probe.close(r));
    const childPath = join(root, 'child.mjs'), specPath = join(root, 'service.json');
    await writeFile(childPath, `import {createServer} from 'node:http';createServer((q,r)=>r.end('ok')).listen(${port},'127.0.0.1');`);
    await writeFile(specPath, JSON.stringify({ component: 'ores.agent', command: process.execPath, args: [childPath], cwd: root, stateDir: join(root, 'run'), healthUrl: `http://127.0.0.1:${port}/healthz` }));
    const run = action => exec(process.execPath, ['scripts/service.mjs', action, specPath], { timeout: 35000 });
    let started = false;
    try {
        const start = JSON.parse((await run('start')).stdout); started = true; assert.equal(start.running, true);
        await assert.rejects(() => run('start'));
        const record = join(root, 'run', 'process.json'), original = await readFile(record, 'utf8');
        const fake = JSON.parse(original); fake.secret = 'wrong'; await writeFile(record, JSON.stringify(fake));
        await assert.rejects(() => run('stop'));
        assert.equal((await fetch(`http://127.0.0.1:${port}/healthz`)).status, 200);
        await writeFile(record, original);
        await run('stop'); started = false;
        assert.equal(JSON.parse((await run('status')).stdout).running, false);
        assert.equal(await (await fetch(`http://127.0.0.1:${unrelated.address().port}`)).text(), 'backend fixture');
    } finally {
        if (started) await run('stop').catch(() => {});
        unrelated.closeAllConnections(); await new Promise(r => unrelated.close(r));
        await rm(root, { recursive: true, force: true });
    }
});

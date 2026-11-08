import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, readFile, writeFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { execFileSync, spawn } from 'node:child_process';
import { once } from 'node:events';

test('a checksummed release runs outside the checkout without installing dependencies', async () => {
    const artifact = execFileSync(process.execPath, ['scripts/package.mjs'], { encoding: 'utf8' }).trim();
    const dir = await mkdtemp(join(tmpdir(), 'ores-agent-'));
    let child;
    try {
        const installed = JSON.parse(execFileSync(process.execPath, ['scripts/deploy.mjs', 'install', artifact, dir], { encoding: 'utf8' }));
        assert.equal(installed.started, false);
        const config = JSON.parse(await readFile('config.example.json', 'utf8')); config.port = 0;
        await writeFile(join(dir, 'config.json'), JSON.stringify(config));
        child = spawn(process.execPath, [join(installed.installed, 'agent.mjs'), '--config', join(dir, 'config.json')], { cwd: dir, stdio: ['ignore', 'pipe', 'pipe'] });
        const output = await new Promise((resolve, reject) => {
            const timer = setTimeout(() => reject(new Error('Release did not start')), 10000);
            child.stdout.once('data', data => { clearTimeout(timer); resolve(data.toString()); });
            child.once('error', reject);
        });
        const port = JSON.parse(output).address.port;
        assert.equal((await fetch(`http://127.0.0.1:${port}/healthz`)).status, 200);
        assert.equal((await fetch(`http://127.0.0.1:${port}/readyz`)).status, 503);
        const ui = await fetch(`http://127.0.0.1:${port}/`);
        assert.match(ui.headers.get('content-security-policy'), /frame-ancestors 'none'/);
        assert.match(await ui.text(), /OreStudio assistant/);
        // A damaged installed release is never silently repaired/overwritten.
        await writeFile(join(installed.installed, 'agent.mjs'), 'damaged installed release');
        assert.throws(() => execFileSync(process.execPath, ['scripts/deploy.mjs', 'install', artifact, dir], { stdio: 'pipe' }));
        await writeFile(join(artifact, 'agent.mjs'), 'tampered');
        assert.throws(() => execFileSync(process.execPath, ['scripts/deploy.mjs', 'install', artifact, dir], { stdio: 'pipe' }));
    } finally {
        if (child && child.exitCode === null) { child.kill(); await once(child, 'exit'); }
        await rm(dir, { recursive: true, force: true });
        // Restore the local generated artifact for subsequent packaging/deployment.
        execFileSync(process.execPath, ['scripts/package.mjs']);
    }
});

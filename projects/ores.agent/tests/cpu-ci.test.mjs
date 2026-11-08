import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtemp, mkdir, writeFile, readFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { execFileSync } from 'node:child_process';
import { cpuConfig, collectLogs } from '../evals/cpu-ci.mjs';

test('CPU deployment is separate and preserves the default CUDA configuration', async () => {
    const root = await mkdtemp(join(tmpdir(), 'ores-cpu-config-'));
    try {
        for (const backend of ['cuda', 'cpu']) {
            const dir = join(root, backend), release = join(dir, 'releases', 'fixture' + (backend === 'cpu' ? '-cpu' : ''));
            await mkdir(release, { recursive: true }); await mkdir(join(dir, 'models'));
            await writeFile(join(release, process.platform === 'win32' ? 'llama-server.exe' : 'llama-server'), 'fixture');
            await writeFile(join(dir, 'models', 'fixture.gguf'), 'fixture');
            await writeFile(join(dir, 'inference-lock.json'), JSON.stringify({ release: 'fixture', model: { file: 'fixture.gguf' }, ...(backend === 'cpu' ? { backend } : {}) }));
            execFileSync(process.execPath, ['scripts/configure-inference.mjs', dir]);
            const spec = JSON.parse(await readFile(join(dir, 'service.json'), 'utf8'));
            const flag = name => spec.args[spec.args.indexOf(name) + 1];
            assert.equal(flag('--ctx-size'), '262144'); assert.equal(flag('--parallel'), '1');
            assert.equal(flag('--n-gpu-layers'), backend === 'cpu' ? '0' : '99');
            assert.equal(flag('--device'), backend === 'cpu' ? 'none' : 'CUDA0');
            assert.equal(flag('--port'), backend === 'cpu' ? '20110' : '20010');
            assert.equal(spec.args.includes('--no-kv-offload'), backend === 'cpu');
            if (backend === 'cpu') {
                const config = cpuConfig(spec, { backend }, dir);
                assert.equal(config.nats, null); assert.equal(config.inference.maxOutputTokens, 2048);
                assert.equal(config.inference.timeoutMs, 120000);
                for (const unsafe of [
                    { ...spec, args: spec.args.filter(x => x !== '--no-kv-offload') },
                    { ...spec, healthUrl: 'http://127.0.0.1:20010/health' },
                    { ...spec, args: spec.args.map(x => x === 'none' ? 'CUDA0' : x) }
                ]) assert.throws(() => cpuConfig(unsafe, { backend }, dir), /separate/);
                assert.throws(() => cpuConfig(spec, {}, dir), /separate/);
            }
            const key = await readFile(join(dir, 'config', 'api-key.txt'), 'utf8');
            execFileSync(process.execPath, ['scripts/configure-inference.mjs', dir]);
            assert.equal(await readFile(join(dir, 'config', 'api-key.txt'), 'utf8'), key);
        }
    } finally { await rm(root, { recursive: true, force: true }); }
});

test('CPU diagnostics exclude the ephemeral key and private control/config files', async () => {
    const root = await mkdtemp(join(tmpdir(), 'ores-cpu-logs-'));
    try {
        await mkdir(join(root, 'run')); await mkdir(join(root, 'config'));
        await writeFile(join(root, 'config', 'api-key.txt'), 'private-fixture-key\n');
        await writeFile(join(root, 'run', 'stderr.log'), 'CPU loaded; private-fixture-key');
        await writeFile(join(root, 'run', 'process.json'), '{"secret":"fixture-control"}');
        const out = join(root, 'reports');
        await collectLogs(root, out);
        assert.equal(await readFile(join(out, 'diagnostics', 'stderr.log'), 'utf8'), 'CPU loaded; [REDACTED]');
        await assert.rejects(readFile(join(out, 'diagnostics', 'process.json')), { code: 'ENOENT' });
        await assert.rejects(readFile(join(out, 'diagnostics', 'api-key.txt')), { code: 'ENOENT' });
    } finally { await rm(root, { recursive: true, force: true }); }
});


// SPDX-License-Identifier: GPL-3.0-or-later
import { readFile, writeFile, mkdir, appendFile } from 'node:fs/promises';
import { spawnSync } from 'node:child_process';
import { resolve, join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { cpus, totalmem } from 'node:os';
import { HttpInference } from '../dist/inference.js';

const component = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const suites = { original: 'evals/cases.json', holdout: 'evals/holdout-stage5.json', review: 'evals/review-v2.json' };

export function cpuConfig(spec, lock, root) {
    const flag = name => spec.args[spec.args.indexOf(name) + 1];
    if (lock.backend !== 'cpu' || flag('--device') !== 'none' || flag('--n-gpu-layers') !== '0' ||
        spec.healthUrl !== 'http://127.0.0.1:20110/health' || !spec.args.includes('--no-kv-offload') || !spec.args.includes('--no-op-offload')) {
        throw Error('Use a separate, configured CPU inference deployment.');
    }
    return { host: '127.0.0.1', port: 0, allowedOrigins: [], nats: null,
        inference: { baseUrl: 'http://127.0.0.1:20110/v1', model: flag('--alias'), timeoutMs: 120000,
            maxOutputTokens: 2048, apiKeyFile: join(root, 'config', 'api-key.txt') } };
}

export async function collectLogs(root, out) {
    const dir = join(out, 'diagnostics');
    await mkdir(dir, { recursive: true });
    let key = '';
    try { key = (await readFile(join(root, 'config', 'api-key.txt'), 'utf8')).trim(); }
    catch (error) { if (error.code !== 'ENOENT') throw error; }
    for (const name of ['stdout.log', 'stderr.log']) {
        try {
            let log = await readFile(join(root, 'run', name), 'utf8');
            if (key) log = log.split(key).join('[REDACTED]');
            await writeFile(join(dir, name), log, { flag: 'wx' });
        } catch (error) { if (error.code !== 'ENOENT') throw error; }
    }
}

async function evaluate(root, out) {
    const selected = process.env.CPU_EVAL_CORPUS || 'all';
    const requireHandling = process.env.CPU_EVAL_REQUIRE_HANDLING || 'false';
    if (selected !== 'all' && !Object.hasOwn(suites, selected) || !['true', 'false'].includes(requireHandling)) {
        throw Error('Invalid CPU evaluation selection.');
    }
    const spec = JSON.parse(await readFile(join(root, 'service.json'), 'utf8'));
    const lock = JSON.parse(await readFile(join(root, 'inference-lock.json'), 'utf8'));
    const config = cpuConfig(spec, lock, root);
    await mkdir(dirname(out), { recursive: true });
    await mkdir(out);
    const configPath = join(root, 'config', 'evaluation.json');
    await writeFile(configPath, JSON.stringify(config, null, 2) + '\n', { mode: 0o600 });
    await writeFile(join(out, 'cpu-environment.json'), JSON.stringify({ platform: process.platform, node: process.version,
        cpu: cpus()[0]?.model, logicalCpus: cpus().length, memoryBytes: totalmem(), backend: 'cpu',
        release: lock.release, model: lock.model, runtimeArgs: spec.args,
        limits: 'CPU/fixture evaluation only; no live server, CUDA or production deployment proof.' }, null, 2) + '\n');

    // Fail infrastructure setup if the real model cannot generate at all.
    const inference = new HttpInference(config.inference);
    const probe = await inference.complete([{ role: 'user', content: 'Reply with the single word ready.' }], [], AbortSignal.timeout(120000));
    if (!probe.content?.trim()) throw Error('CPU model returned no probe text.');

    const reports = [];
    let exitCode = 0;
    for (const [name, corpus] of Object.entries(suites)) {
        if (selected !== 'all' && selected !== name) continue;
        const args = ['evals/run.mjs', '--profile', 'model', '--corpus', corpus, '--config', configPath,
            '--inference-root', root, '--timeout-ms', '120000', '--repeat', '1', '--out', join(out, name)];
        if (requireHandling === 'true') args.push('--strict', '--min-pass', '0', '--require-handling');
        const run = spawnSync(process.execPath, args, { cwd: component, stdio: 'inherit', timeout: 3600000 });
        const status = run.error || run.status === null ? 1 : run.status;
        exitCode = Math.max(exitCode, status);
        try {
            const report = JSON.parse(await readFile(join(out, name, 'results.json'), 'utf8'));
            reports.push({ name, exitCode: status, summary: report.summary });
        } catch (error) {
            if (error.code !== 'ENOENT') throw error;
            reports.push({ name, exitCode: status, summary: null });
            exitCode = Math.max(exitCode, 1);
        }
    }
    await writeFile(join(out, 'cpu-results.json'), JSON.stringify({ backend: 'cpu', requireHandling: requireHandling === 'true', reports }, null, 2) + '\n');
    const summary = '# CPU agent evaluation\n\nReal LFM3 model; labelled fixtures; no live OreStudio.\n' +
        '\nWorkflow success means setup and report completion. Quality failures remain findings unless handling is explicitly required.\n' +
        '\n| Suite | Cases | Handling passed | Overall passed | Errors | Exit |\n| --- | ---: | ---: | ---: | ---: | ---: |\n' +
        reports.map(r => `| ${r.name} | ${r.summary?.total ?? 'incomplete'} | ${r.summary?.handling.pass ?? '-'} | ${r.summary?.outcomes.pass ?? '-'} | ${r.summary?.outcomes.error ?? '-'} | ${r.exitCode} |`).join('\n') + '\n';
    await writeFile(join(out, 'summary.md'), summary);
    if (process.env.GITHUB_STEP_SUMMARY) await appendFile(process.env.GITHUB_STEP_SUMMARY, summary);
    return exitCode;
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
    const [action, rootArg, outArg] = process.argv.slice(2);
    if (!['run', 'collect'].includes(action) || !rootArg || !outArg) throw Error('Usage: cpu-ci.mjs run|collect <CPU_ROOT> <NEW_OUTPUT_DIRECTORY>');
    const root = resolve(rootArg), out = resolve(outArg);
    try { process.exitCode = action === 'run' ? await evaluate(root, out) : (await collectLogs(root, out), 0); }
    catch (error) { console.error(`CPU evaluation ${action} failed: ${error.code ?? error.name}. Check the runner and inference logs.`); process.exitCode = 1; }
}

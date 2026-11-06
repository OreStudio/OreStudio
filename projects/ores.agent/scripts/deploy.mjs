// SPDX-License-Identifier: GPL-3.0-or-later
import { readFile, mkdir, copyFile, writeFile, lstat, realpath } from 'node:fs/promises';
import { resolve, join, sep, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';
const [command, first, second] = process.argv.slice(2);
if (!['install', 'configure'].includes(command) || !first || !second) throw Error('Usage: deploy.mjs install <artifact> <root> | configure <root> <installed-release>');
const root = resolve(command === 'install' ? second : first);
const source = resolve(dirname(fileURLToPath(import.meta.url)), '../../..');
if (root === source || root.startsWith(source + sep)) throw Error('Deployment must be outside the source checkout.');
await mkdir(root, { recursive: true });
const actual = await realpath(root), actualSource = await realpath(source);
if (actual === actualSource || actual.startsWith(actualSource + sep)) throw Error('Deployment resolves inside the source checkout.');
const artifact = resolve(command === 'install' ? first : second);
if (root === artifact || root.startsWith(artifact + sep)) throw Error('Deployment must be outside the artifact directory.');
const manifest = JSON.parse(await readFile(join(artifact, 'manifest.json'), 'utf8'));
const { id, ...core } = manifest;
if (manifest.component !== 'ores.agent' || !/^[a-zA-Z0-9.-]+$/.test(manifest.version) || id !== createHash('sha256').update(JSON.stringify(core)).digest('hex')) throw Error('Invalid artifact manifest.');
if (Object.keys(manifest.files).sort().join(',') !== 'agent.mjs,service.mjs') throw Error('Unexpected artifact files.');
for (const [name, hash] of Object.entries(manifest.files)) {
    if ((await lstat(join(artifact, name))).isSymbolicLink() || createHash('sha256').update(await readFile(join(artifact, name))).digest('hex') !== hash) throw Error('Artifact checksum mismatch.');
}
await mkdir(join(root, 'config'), { recursive: true });
await mkdir(join(root, 'logs'), { recursive: true });
if (command === 'install') {
    const release = join(root, 'releases', `${manifest.version}-${id.slice(0, 12)}`);
    await mkdir(join(root, 'releases'), { recursive: true });
    let exists = false;
    try { await mkdir(release); } catch (e) { if (e.code === 'EEXIST') exists = true; else throw e; }
    if (exists) {
        if (await readFile(join(release, 'manifest.json'), 'utf8') !== JSON.stringify(manifest, null, 2) + '\n') throw Error('Existing release manifest differs; releases are immutable.');
        for (const [name, hash] of Object.entries(manifest.files)) if (createHash('sha256').update(await readFile(join(release, name))).digest('hex') !== hash) throw Error('Installed release is damaged; do not overwrite it.');
    } else {
        for (const name of Object.keys(manifest.files)) await copyFile(join(artifact, name), join(release, name));
        await writeFile(join(release, 'manifest.json'), JSON.stringify(manifest, null, 2) + '\n', { flag: 'wx' });
    }
    console.log(JSON.stringify({ installed: release, started: false }));
} else {
    const releases = await realpath(join(root, 'releases'));
    if (!(await realpath(artifact)).startsWith(releases + sep)) throw Error('Configure requires an installed release belonging to this deployment.');
    try {
        await writeFile(join(root, 'config', 'agent.json'), JSON.stringify({ host: '127.0.0.1', port: 20100, allowedOrigins: ['http://127.0.0.1:20100'], nats: null,
            inference: { baseUrl: 'http://127.0.0.1:20010/v1', model: 'lfm3-1.7b', timeoutMs: 60000 } }, null, 2) + '\n', { flag: 'wx', mode: 0o600 });
    } catch (e) { if (e.code !== 'EEXIST') throw e; }
    const configPath = join(root, 'config', 'agent.json'), config = JSON.parse(await readFile(configPath, 'utf8'));
    const spec = { component: 'ores.agent', command: process.execPath, args: [join(artifact, 'agent.mjs'), '--config', configPath], cwd: root,
        stateDir: join(root, 'run'), healthUrl: `http://127.0.0.1:${config.port}/healthz`, manifestPath: join(artifact, 'manifest.json') };
    await writeFile(join(root, 'service.json'), JSON.stringify(spec, null, 2) + '\n', { mode: 0o600 });
    console.log(JSON.stringify({ configured: join(root, 'service.json'), release: artifact, started: false }));
}

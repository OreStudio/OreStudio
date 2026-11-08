// SPDX-License-Identifier: GPL-3.0-or-later
// Configuration only: no download, compilation or process startup.
import { readFile, writeFile, mkdir, readdir, access, copyFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { resolve, join, dirname, delimiter } from 'node:path';
import { randomBytes } from 'node:crypto';
const root = resolve(process.argv[2] ?? '');
if (!process.argv[2]) throw Error('Pass the installed independent inference root.');
const lock = JSON.parse(await readFile(join(root, 'inference-lock.json'), 'utf8'));
const backend = lock.backend ?? 'cuda';
if (!['cpu', 'cuda'].includes(backend)) throw Error('Unknown inference backend.');
const cpu = backend === 'cpu', port = cpu ? 20110 : 20010;
const release = join(root, 'releases', lock.release + (cpu ? '-cpu' : ''));
const dirs = [], files = [];
async function walk(dir) { dirs.push(dir); for (const e of await readdir(dir, { withFileTypes: true })) { if (e.isDirectory()) await walk(join(dir, e.name)); else if (e.isFile()) files.push(join(dir, e.name)); } }
await walk(release);
const exe = files.find(p => p.endsWith(process.platform === 'win32' ? 'llama-server.exe' : '/llama-server'));
if (!exe) throw Error('No native llama-server in the installed release.');
const key = join(root, 'config', 'api-key.txt');
await mkdir(dirname(key), { recursive: true });
try { await writeFile(key, randomBytes(32).toString('base64url') + '\n', { flag: 'wx', mode: 0o600 }); } catch (e) { if (e.code !== 'EEXIST') throw e; }
const model = join(root, 'models', lock.model.file); await access(model);
const spec = { component: 'llama.cpp', command: exe, cwd: dirname(exe), stateDir: join(root, 'run'), healthUrl: `http://127.0.0.1:${port}/health`,
    args: ['--model', model, '--alias', 'lfm3-1.7b', '--host', '127.0.0.1', '--port', String(port), '--ctx-size', '262144', '--parallel', '1',
        '--n-gpu-layers', cpu ? '0' : '99', '--device', cpu ? 'none' : 'CUDA0', '--jinja', '--temp', '0.1', '--top-k', '50', '--repeat-penalty', '1.1', '--threads', '4', '--threads-batch', '4',
        '--no-ui', '--api-key-file', key, '--cors-origins', 'http://127.0.0.1:20100', '--log-colors', 'off', '--log-verbosity', cpu ? '3' : '4', ...(cpu ? ['--no-kv-offload', '--no-op-offload'] : [])],
    env: process.platform === 'linux' ? { LD_LIBRARY_PATH: dirs.join(delimiter) } : {} };
await writeFile(join(root, 'service.json'), JSON.stringify(spec, null, 2) + '\n', { mode: 0o600 });
await copyFile(join(dirname(fileURLToPath(import.meta.url)), 'service.mjs'), join(root, 'service.mjs'));
console.log(JSON.stringify({ configured: join(root, 'service.json'), started: false, apiKeyFile: key }));

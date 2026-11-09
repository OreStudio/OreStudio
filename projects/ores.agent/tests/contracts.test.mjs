// SPDX-License-Identifier: GPL-3.0-or-later
import test from 'node:test';
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { mkdtemp, readFile, writeFile, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';

const run = (args) =>
    execFileSync(process.execPath, ['scripts/check-backend-contracts.mjs', ...args], {
        encoding: 'utf8',
        stdio: 'pipe',
        timeout: 30000,
    });

test('contract gate checks current sources and rejects stale evidence, changed subjects and source drift', async () => {
    const root = await mkdtemp(join(tmpdir(), 'ores-contract-gate-'));
    try {
        assert.match(run(['check']), /Backend contract source matches/);
        const manifest = JSON.parse(await readFile('backend-contract.json', 'utf8'));
        for (const [field, mutate, message] of [
            [
                'reference',
                (r) => {
                    r.referenceRevision = '0'.repeat(40);
                },
                /Contract reference and evidence disagree/,
            ],
            [
                'subjects',
                (r) => {
                    delete r.subjects.login;
                },
                /Agent operation allowlist changed/,
            ],
            [
                'source',
                (r) => {
                    r.files[Object.keys(r.files)[0]] = '0'.repeat(64);
                },
                /Backend contract source changed/,
            ],
        ]) {
            const copy = structuredClone(manifest);
            mutate(copy);
            const path = join(root, field + '.json');
            await writeFile(path, JSON.stringify(copy));
            assert.throws(
                () => run(['check', path]),
                (error) => error.status === 1 && message.test(error.stderr),
            );
        }
    } finally {
        await rm(root, { recursive: true, force: true });
    }
});

test('probe preparation resolves every allowlisted operation and embeds a source fingerprint', async () => {
    const root = await mkdtemp(join(tmpdir(), 'ores-contract-prepare-'));
    try {
        run(['prepare', root]);
        const text = await readFile(join(root, 'agent-contract-operations.inc'), 'utf8');
        const manifest = JSON.parse(await readFile('backend-contract.json', 'utf8'));
        assert.equal(
            (text.match(/^AGENT_OPERATION\(/gm) ?? []).length,
            Object.keys(manifest.subjects).length,
        );
        assert.match(text, /#define AGENT_CONTRACT_SOURCE_SHA "[a-f0-9]{64}"/);
        assert.ok(!text.includes('undefined'));
    } finally {
        await rm(root, { recursive: true, force: true });
    }
});

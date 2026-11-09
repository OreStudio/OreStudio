// SPDX-License-Identifier: GPL-3.0-or-later
import { readFileSync, writeFileSync, readdirSync, existsSync, mkdirSync } from 'node:fs';
import { resolve, dirname, relative, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import assert from 'node:assert/strict';
import { decode } from '@msgpack/msgpack';
import { NatsBackend, subjects, backendContractRevision } from '../dist/backend.js';

const component = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const root = resolve(component, '../..');
const manifestPath = join(component, 'backend-contract.json');
const pathName = (p) => relative(root, p).replaceAll('\\', '/');
const sha = (p) =>
    createHash('sha256').update(readFileSync(p, 'utf8').replaceAll('\r\n', '\n')).digest('hex');
const digest = (value) => createHash('sha256').update(JSON.stringify(value)).digest('hex');
const projects = readdirSync(join(root, 'projects')).filter((p) => p.startsWith('ores.'));
const includeRoots = projects
    .flatMap((p) => ['api/include', 'include'].map((s) => join(root, 'projects', p, s)))
    .filter(existsSync);

function operations() {
    const found = {};
    for (const includes of includeRoots) {
        for (const name of readdirSync(includes)) {
            const dir = join(includes, name, 'messaging');
            if (!existsSync(dir)) continue;
            for (const file of readdirSync(dir).filter((f) => f.endsWith('.hpp'))) {
                const path = join(dir, file),
                    text = readFileSync(path, 'utf8');
                const namespace = /namespace ([\w:]+)\s*\{/.exec(text)?.[1];
                for (const match of text.matchAll(
                    /struct (\w+)\s*\{(?:(?!\n\};)[\s\S])*?nats_subject\s*=\s*"([^"]+)"/g,
                )) {
                    const op = Object.keys(subjects).find((op) => subjects[op] === match[2]);
                    if (op) {
                        assert.ok(!found[op], `Duplicate server subject: ${op}`);
                        found[op] = {
                            path,
                            header: pathName(path).split('/include/')[1],
                            type: namespace + '::' + match[1],
                        };
                    }
                }
            }
        }
    }
    assert.deepEqual(
        Object.keys(found).sort(),
        Object.keys(subjects).sort(),
        'Server operation removed or renamed',
    );
    return found;
}

function sourceHashes(ops) {
    const files = new Set();
    function visit(path) {
        if (files.has(path)) return;
        files.add(path);
        for (const match of readFileSync(path, 'utf8').matchAll(/#include "([^"]+)"/g)) {
            const dependency = includeRoots.map((r) => join(r, match[1])).find(existsSync);
            if (dependency) visit(dependency);
        }
    }
    for (const op of Object.values(ops)) visit(op.path);
    for (const path of [
        'projects/ores.utility/include/ores.utility/codec/struct_codec.hpp',
        'projects/ores.nats/include/ores.nats/domain/wire_codec.hpp',
        'projects/ores.service/include/ores.service/messaging/handler_helpers.hpp',
        'projects/ores.service/include/ores.service/error_code.hpp',
        'vcpkg.json',
    ]) {
        if (existsSync(join(root, path))) visit(join(root, path));
    }
    return Object.fromEntries(
        [...files].map((p) => [pathName(p), sha(p)]).sort(([a], [b]) => a.localeCompare(b)),
    );
}

function subset(input, output, path = '') {
    for (const [key, value] of Object.entries(input)) {
        if (value === null && !Object.hasOwn(output, key)) continue;
        assert.ok(Object.hasOwn(output, key), `Server ignored request field ${path}${key}`);
        if (value && typeof value === 'object') subset(value, output[key], path + key + '.');
        else if (typeof value === 'string' && /^\d{4}-\d\d-\d\dT/.test(value))
            assert.equal(Date.parse(value), Date.parse(output[key]));
        else assert.deepEqual(output[key], value, `Server changed request field ${path}${key}`);
    }
}

async function verify(probePath) {
    const { fixtureBackend, fixtureSession, fixtureIds } = await import('../evals/fixtures.mjs');
    const { evaluateCase } = await import('../evals/engine.mjs');
    const { encode } = await import('@msgpack/msgpack');
    const seen = new Set();
    const sourceSha256 = digest(sourceHashes(operations()));
    let legacyEnumRows = 0;
    let legacyInstrumentBatches = 0;
    const contractFixture = (value) => {
        if (Array.isArray(value)) return value.map(contractFixture);
        if (!value || typeof value !== 'object') return value;
        return Object.fromEntries(
            Object.entries(value).map(([key, child]) => {
                // Separate contract test data, never a change to the frozen eval inputs or scores.
                // The historical fixtures used a value the server's enum cannot store.
                if (key === 'booking_nature' && child === 'external') {
                    legacyEnumRows++;
                    return [key, 'test'];
                }
                if (key === 'instruments' && Array.isArray(child) && child.length === 0) {
                    legacyInstrumentBatches++;
                    return [key, {}];
                }
                return [key, contractFixture(child)];
            }),
        );
    };
    const roundtrip = (op, data, reply) => {
        let wire;
        try {
            wire = JSON.parse(
                execFileSync(
                    resolve(probePath),
                    [op, Buffer.from(data).toString('hex'), JSON.stringify(reply)],
                    { encoding: 'utf8', stdio: 'pipe', timeout: 15000, maxBuffer: 2_000_000 },
                ),
            );
        } catch (error) {
            console.error(`Contract operation failed: ${op}`);
            throw error;
        }
        assert.equal(
            wire.sourceSha256,
            sourceSha256,
            'Rebuild the codec probe against current sources.',
        );
        assert.equal(wire.subject, subjects[op]);
        subset(decode(data), wire.request);
        seen.add(op);
        return Buffer.from(wire.responseHex, 'hex');
    };
    // Cover every allowlisted operation, including the less common detail reads.
    const id = fixtureIds.trade;
    const page = {
        offset: 0,
        limit: 2,
        order: { field: '', descending: false },
        filter: null,
        as_of: null,
    };
    const special = {
        login: { principal: 'fixture', password: 'fixture' },
        logout: {},
        selectParty: { party_id: fixtureIds.party },
        observations: { ...page, series_id: id, scope: 'direct' },
        tradePortfolios: { ...page, trade_id: id, scope: 'direct' },
        trade: { key: { id } },
        tradeBooking: { key: { trade_id: id } },
        fxForward: { key: { trade_id: id } },
        bond: { key: { trade_id: id } },
        counterparty: { key: { short_code: 'FIXTURE' } },
        exportPortfolio: { node_id: fixtureIds.portfolio, offset: 0, limit: 2 },
        runHealth: { report_instance_id: fixtureIds.run },
    };
    for (const op of Object.keys(subjects)) {
        const payload = special[op] ?? page;
        // Responses use server-defined defaults here; replay below checks meaningful rows.
        roundtrip(op, encode(payload), {});
    }
    let cases = 0;
    for (const file of ['cases.json', 'holdout-stage5.json', 'review-v2.json']) {
        const corpus = JSON.parse(readFileSync(join(component, 'evals', file), 'utf8'));
        const fixtures =
            file === 'review-v2.json'
                ? fixtureBackend
                : (await import('../evals/fixtures-v1.mjs')).fixtureBackend;
        for (const spec of corpus.cases.filter((spec) => spec.profiles.includes('replay'))) {
            const fixture = fixtures(spec.scenario);
            const session = fixtureSession(spec.requiresOtherParty);
            const backend = {
                ready: () => true,
                close: async () => {},
                request: async (op, payload, identity, signal) => {
                    const connection = {
                        status: async function* () {},
                        request: async (_subject, data) => ({
                            data: roundtrip(
                                op,
                                data,
                                contractFixture(
                                    await fixture.request(op, decode(data), identity, signal),
                                ),
                            ),
                        }),
                    };
                    return new NatsBackend(connection, 'fixture').request(
                        op,
                        payload,
                        identity,
                        signal,
                    );
                },
            };
            const result = await evaluateCase(spec, {
                profile: 'replay',
                backend,
                session,
                redact: { clean: (value) => value },
            });
            assert.equal(
                result.handling,
                'pass',
                `${spec.id}: ${JSON.stringify(result.turns.map((t) => ({ error: t.error, failures: t.failures })))}`,
            );
            cases++;
        }
    }
    console.log(
        JSON.stringify({
            wireOperations: seen.size,
            contractCases: cases,
            legacyEnumRows,
            legacyInstrumentBatches,
            result: 'pass',
            profile: 'C++ codec with contract-only fixtures; not a model/live evaluation',
        }),
    );
}

const [mode = 'check', path, revision] = process.argv.slice(2);
const ops = operations();
if (mode === 'prepare' && path) {
    mkdirSync(resolve(path), { recursive: true });
    writeFileSync(
        join(resolve(path), 'agent-contract-operations.inc'),
        [
            '#ifndef AGENT_CONTRACT_SOURCE_SHA',
            `#define AGENT_CONTRACT_SOURCE_SHA "${digest(sourceHashes(ops))}"`,
            '#endif',
            ...Object.entries(ops).flatMap(([op, entry]) => [
                `#include "${entry.header}"`,
                `AGENT_OPERATION("${op}", ${entry.type})`,
            ]),
        ].join('\n') + '\n',
    );
} else if ((mode === 'verify' || mode === 'record') && path) {
    if (mode === 'record') {
        assert.match(revision ?? '', /^[a-f0-9]{40}$/);
        execFileSync(
            'git',
            ['diff', '--quiet', revision, '--', ...Object.keys(sourceHashes(ops))],
            { cwd: root, stdio: 'pipe' },
        );
    }
    await verify(path);
    if (mode === 'record') {
        writeFileSync(
            manifestPath,
            JSON.stringify(
                { referenceRevision: revision, subjects, files: sourceHashes(ops) },
                null,
                2,
            ) + '\n',
        );
    }
} else if (mode === 'check') {
    const expected = JSON.parse(readFileSync(path ? resolve(path) : manifestPath, 'utf8'));
    assert.equal(
        backendContractRevision,
        expected.referenceRevision,
        'Contract reference and evidence disagree.',
    );
    assert.deepEqual(
        subjects,
        expected.subjects,
        'Agent operation allowlist changed; repeat the codec check.',
    );
    assert.deepEqual(
        sourceHashes(ops),
        expected.files,
        'Backend contract source changed; review it and repeat the codec check before recording new evidence.',
    );
    console.log(`Backend contract source matches checked revision ${expected.referenceRevision}.`);
} else throw Error('Use check | prepare BUILD_DIR | verify PROBE | record PROBE REVISION');

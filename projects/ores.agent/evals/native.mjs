// SPDX-License-Identifier: GPL-3.0-or-later
import { spawn } from 'node:child_process';
import { randomUUID } from 'node:crypto';
import { once } from 'node:events';
import { isDeepStrictEqual } from 'node:util';
import { access, mkdir, readFile, writeFile, appendFile, rm } from 'node:fs/promises';
import { dirname, join, resolve, toNamespacedPath } from 'node:path';
import { fileURLToPath } from 'node:url';
import { setTimeout as delay } from 'node:timers/promises';
import { z } from 'zod';
import { loadConfig } from '../dist/config.js';
import { NatsBackend } from '../dist/backend.js';
import { Sessions } from '../dist/auth.js';
import { createApp } from '../dist/server.js';
import { datasetNats } from '../scripts/fx-e2e-nats.mjs';
import { at, gradeTurn, redactor } from './grade.mjs';
import { gradeFxAnalytics } from './fx-analytics-oracle.mjs';
import { checkDatasetResult } from './fx-e2e-dataset.mjs';
import { nativeMarketData, seedNativeMarket } from './native-market-data.mjs';
import { nativeCases } from './native-cases.mjs';
import { cloneConfig, databaseClone } from './native-clone.mjs';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const text = z.string().min(1);
const command = z.object({ argv: z.array(text).min(1), cwd: text.optional() }).strict();
const values = z
    .record(text, z.unknown())
    .refine(
        (value) =>
            Object.keys(value).length > 0 &&
            Object.values(value).every((v) => v !== null && v !== ''),
        'Set known expected result values from the seed dataset.',
    );
const fixtureSchema = z
    .object({
        book: text,
        ambiguousBook: text,
        portfolio: text,
        counterparty: text,
        originReference: text,
        trade: z.uuid(),
        forwardTrade: z.uuid(),
        bondTrade: z.uuid(),
        bondIssue: z.uuid(),
        run: z.uuid(),
        partialRun: z.uuid(),
        riskRun: z.uuid(),
        date: z.iso.date(),
        riskDate: z.iso.date(),
        definition: text,
        riskDefinition: text,
        bookingDate: z.iso.date(),
        forwardDate: z.iso.date(),
        swapEndDate: z.iso.date(),
        swapCalendar: text,
        rateFactor: text,
        scenario: text,
        vegaOption: z.number().int().positive(),
        expected: z
            .object({
                tradeNative: values,
                tradeBase: values,
                tradePartial: values,
                bookNative: values,
                bookBase: values,
                portfolioNative: values,
                portfolioBase: values,
                rateGreeks: values,
                vega: values,
                scenario: values,
            })
            .strict(),
    })
    .strict();
const settingsSchema = z
    .object({
        disposable: z.literal(true),
        agentConfig: text,
        credentials: text,
        tenantId: z.uuid(),
        partyId: z.uuid(),
        start: command.extend({ foreground: z.boolean().default(false) }),
        stop: command.optional(),
        seed: command.optional(),
        fixtures: text,
        clone: cloneConfig.optional(),
        reportTimeoutMs: z.number().int().min(1000).max(900000).default(240000),
    })
    .strict()
    .refine(
        (settings) =>
            settings.start.foreground ? Boolean(settings.clone) : Boolean(settings.stop),
        'Foreground startup requires a clone; a start command that exits requires a stop command.',
    );

const credentialSchema = z.object({
    tenantUsername: text,
    tenantPassword: text,
    systemUsername: text,
    systemPassword: text,
});
const usage =
    'Usage: npm run eval:native -- <local-config.json> [output-directory] [--case <id>]\n       npm run eval:native -- --list';
const readJson = async (path) => JSON.parse((await readFile(path, 'utf8')).replace(/^\uFEFF/, ''));

async function runCommand(spec, stage, base, output, redact, serviceEnv = {}) {
    const [file, ...args] = spec.argv;
    const child = spawn(file, args, {
        cwd: spec.cwd ? resolve(base, spec.cwd) : root,
        env: { ...process.env, ...serviceEnv },
        windowsHide: true,
        stdio: ['ignore', 'pipe', 'pipe'],
        signal: AbortSignal.timeout(600000),
    });
    let log = '';
    for (const stream of [child.stdout, child.stderr])
        stream.setEncoding('utf8').on('data', (chunk) => {
            log = (log + chunk).slice(-32000);
        });
    try {
        const [code] = await once(child, 'close');
        if (code !== 0) throw Error(stage + ' exited with code ' + code + '.');
    } finally {
        await writeFile(join(output, stage + '.log'), redact.clean(log), { mode: 0o600 });
    }
}

function foregroundFleet(spec, base, output, redact, env) {
    const [file, ...args] = spec.argv;
    const child = spawn(file, args, {
        cwd: spec.cwd ? resolve(base, spec.cwd) : root,
        env: { ...process.env, ...env },
        windowsHide: true,
        stdio: ['ignore', 'pipe', 'pipe'],
    });
    let log = '',
        failure;
    const closed = new Promise((resolve) => child.once('close', resolve));
    child.once('error', (error) => {
        failure = error;
    });
    for (const stream of [child.stdout, child.stderr])
        stream.setEncoding('utf8').on('data', (chunk) => {
            log = (log + chunk).slice(-32000);
        });
    return {
        async ready() {
            const deadline = Date.now() + 600000;
            while (Date.now() < deadline) {
                if (failure) throw failure;
                if (child.exitCode !== null || child.signalCode)
                    throw Error(
                        'Native fleet exited during startup. See start.log and service logs.',
                    );
                if (
                    await access(join(output, 'fleet-ready')).then(
                        () => true,
                        () => false,
                    )
                )
                    return;
                await delay(250);
            }
            throw Error('Native fleet did not signal readiness.');
        },
        async stop() {
            if (child.exitCode === null && !child.signalCode) child.kill();
            const timeout = delay(10000, false, { ref: false });
            const didClose = await Promise.race([closed.then(() => true), timeout]);
            await writeFile(join(output, 'start.log'), redact.clean(log), { mode: 0o600 });
            if (!didClose) throw Error('Native fleet process did not exit.');
            await rm(toNamespacedPath(join(output, 'fleet-runtime')), {
                recursive: true,
                force: true,
            });
        },
    };
}

function checkReply(t, response) {
    // HTTP returns the production evidence envelope. The tool's request is the
    // selector evidence available over HTTP; fixtures never supply it.
    const observed = (response.evidence ?? []).filter((e) => e.tool === t.expect.tool);
    const trace = observed
        .filter((e) => e.request)
        .map((e) => ({ type: 'tool', name: e.tool, arguments: e.request }));
    const failures = gradeTurn(t.expect, response, trace);
    const evidence = observed[0];
    for (const assertion of t.assertions ?? []) {
        if (assertion === 'rows' && !evidence?.rows?.length)
            failures.push('Expected at least one stored row.');
        if (assertion === 'runs' && !evidence?.rows?.length)
            failures.push('Expected at least one stored run.');
        if (assertion === 'help' && !evidence?.matches?.length)
            failures.push('Expected matching OreStudio help.');
        if (assertion === 'fx-reference') failures.push(...checkDatasetResult(evidence));
    }
    if (t.analytics) failures.push(...gradeFxAnalytics(t.analytics, response));
    return failures;
}

function substitute(message, captured) {
    return message.replace(/\{\{(\w+)\}\}/g, (_, name) => {
        if (!captured[name]) throw Error('Missing value captured by prior turn: ' + name);
        return captured[name];
    });
}

async function httpAgent(config, backend, output, credentials, identity, redact) {
    const server = createApp(config, backend, join(output, 'npv-submissions'));
    let access;
    const stop = async () => {
        if (access) await post('/auth/logout', {}).catch(() => {});
        await server.shutdown();
    };
    const post = async (route, body) => {
        const port = server.address().port;
        const result = await fetch('http://127.0.0.1:' + port + route, {
            method: 'POST',
            headers: {
                'content-type': 'application/json',
                ...(access ? { authorization: 'Bearer ' + access } : {}),
            },
            body: JSON.stringify(body),
            signal: AbortSignal.timeout(125000),
        });
        const value = await result.json();
        if (!result.ok)
            throw Error(
                'HTTP ' +
                    result.status +
                    ' ' +
                    route +
                    ': ' +
                    (value.code ?? value.error ?? 'request failed'),
            );
        return value;
    };
    const login = async () => {
        if (access) {
            await post('/auth/logout', {}).catch(() => {});
            access = undefined;
        }
        const session = await post('/auth/login', {
            principal: credentials.tenantUsername,
            password: credentials.tenantPassword,
            partyId: identity.partyId,
        });
        access = session.accessToken;
        redact.add(access);
        if (session.tenantId !== identity.tenantId || session.partyId !== identity.partyId)
            throw Error('Agent HTTP session selected a different tenant or party.');
    };
    try {
        server.listen(0, '127.0.0.1');
        await once(server, 'listening');
        await login();
        return { post, stop, login };
    } catch (error) {
        await stop().catch(() => {});
        throw error;
    }
}

async function ensureDataset(post, f, record) {
    const read = async (tool, args, checks) => {
        const response = await post('/tools/' + tool, args);
        await record({ tool, args, response });
        if (
            !['ok', 'partial', 'stale'].includes(response.status) ||
            Object.entries(checks).some(
                ([path, value]) => !isDeepStrictEqual(at(response, path), value),
            )
        )
            throw Error(
                'Seed data failed preflight for ' +
                    tool +
                    ' (' +
                    Object.keys(checks).join(', ') +
                    ').',
            );
        return response;
    };
    const runId = f.run,
        valuationDate = f.date;
    await read('find_books', { query: f.book }, { 'rows.0.name': f.book });
    await read('find_portfolios', { query: f.portfolio }, { 'rows.0.name': f.portfolio });
    await read(
        'find_counterparties',
        { query: f.counterparty },
        { 'rows.0.short_code': f.counterparty },
    );
    for (const id of new Set([f.trade, f.forwardTrade, f.bondTrade]))
        await read('read_trade', { tradeId: id }, { 'trade.id': id });
    await read(
        'read_trade_results',
        { tradeId: f.trade, runId, measure: 'npv_base', valuationDate },
        f.expected.tradeBase,
    );
    await read(
        'read_trade_results',
        { tradeId: f.trade, runId: f.partialRun, allowPartial: true },
        f.expected.tradePartial,
    );
    for (const scopeType of ['book', 'portfolio'])
        await read(
            'summarize_scope_results',
            { scopeType, scope: f[scopeType], runId, valuationDate },
            f.expected[scopeType + 'Base'],
        );
    await read(
        'summarize_scope_results',
        {
            scopeType: 'book',
            scope: f.book,
            runId: f.riskRun,
            valuationDate: f.riskDate,
            factor: f.rateFactor,
            measure: 'greeks',
            riskMeasures: ['delta', 'gamma', 'dv01'],
        },
        f.expected.rateGreeks,
    );
    await read(
        'read_fx_spot',
        { baseCurrency: 'EUR', quoteCurrency: 'USD', provider: 'agent-e2e', asOf: f.ir.date },
        { 'observation.value': '1.0552' },
    );
    for (const [name, instrument, term] of [
        ['swap', 'ir_swap', '5Y'],
        ['deposit', 'mm', '6M'],
        ['zero', 'zero', '6M'],
        ['discount', 'discount', '6M'],
    ]) {
        const point = f.ir[name];
        await read(
            'read_ir_rate',
            { currency: 'GBP', instrument, term, seriesId: point.id, asOf: f.ir.date },
            { 'observation.value': point.value },
        );
    }
}

async function pollReport(post, reply, scope, timeout, record, expected) {
    const created = reply.evidence?.find((e) => e.status === 'recorded');
    if (!created?.runId || created.outcome !== 'ok')
        throw Error('No native report run was successfully recorded.');
    const deadline = Date.now() + timeout;
    while (Date.now() < deadline) {
        const progress = await post('/chat/report-progress', { runId: created.runId });
        await record({ action: 'poll', runId: created.runId, response: progress });
        const state = progress.evidence?.find((e) => e.tool === 'read_run_status');
        if (state && state.run?.id !== created.runId) throw Error('Progress returned another run.');
        if (!progress.watching) {
            if (state?.health !== 'completed clean' || state?.state_name !== 'completed')
                throw Error('Native report did not complete cleanly.');
            if (scope !== 'risk') {
                const tool = scope === 'trade' ? 'read_trade_results' : 'summarize_scope_results';
                const result = progress.evidence.find((e) => e.tool === tool && e.status === 'ok');
                if (!result || result.run?.id !== created.runId)
                    throw Error('The completed run has no verified ' + scope + ' results.');
                if (
                    Object.entries(expected ?? {}).some(
                        ([path, value]) => !isDeepStrictEqual(at(result, path), value),
                    )
                )
                    throw Error(
                        'New report results differ from the expected ' + scope + ' values.',
                    );
            }
            return;
        }
        await delay(Math.min(2000, Math.max(0, deadline - Date.now())));
    }
    throw Error('Native report did not finish within the configured deadline.');
}

export async function runCase(test, post, fixtures, timeout, record) {
    await post('/chat/reset', {});
    const captured = {};
    let draft;
    const call = async (path, request) => {
        const response = await post(path, request);
        await record({ path, request, response });
        return response;
    };
    for (const t of test.turns) {
        const countTrades = async () => {
            const r = await call('/tools/find_trades', {
                book: fixtures.book,
                tradeType: t.booking.product,
                limit: 1,
            });
            if (!['ok', 'not_found'].includes(r.status) || !Number.isInteger(r.totalMatches))
                throw Error('Could not verify the number of stored trades in the booking scope.');
            return r.totalMatches;
        };
        const before = t.booking ? await countTrades() : undefined;
        const request = t.editSwap
            ? {
                  bookingDraft: {
                      action: 'edit',
                      id: draft?.id,
                      revision: draft?.revision,
                      fields: t.editSwap,
                  },
              }
            : { message: substitute(t.message, captured) };
        const reply = await call('/chat', request);
        const failures = checkReply(t, reply);
        if (failures.length) throw Error(failures.join(' '));
        draft = reply.bookingDraft;
        if (draft?.confirmation) captured.bookingConfirmation = draft.confirmation;
        if (t.capture) {
            const value = t.pattern.exec(reply.answer)?.[0];
            if (!value) throw Error('Expected a review confirmation.');
            captured[t.capture] = value;
        }
        if (t.captureDefinition) {
            const review = reply.evidence.find((e) => e.tool === 'review_npv_run');
            const id = review?.definitionId ?? review?.definition?.id;
            if (!z.uuid().safeParse(id).success)
                throw Error('Review did not return its definition ID.');
            captured.definitionId = id;
        }
        if (t.captureOption) {
            const options = reply.evidence.find((e) => e.tool === t.expect.tool)?.candidates ?? [];
            const index = options.findIndex((e) => e.seriesId === t.captureOption.seriesId);
            if (index < 0) throw Error('The seeded series is missing from the options.');
            captured[t.captureOption.name] = String(index + 1);
        }
        if (t.booking) {
            if (t.booking.delta === 1) {
                const tradeId = reply.evidence.find((e) => e.tool === t.expect.tool)?.tradeId;
                const readback = await call('/tools/read_trade', { tradeId });
                if (
                    !tradeId ||
                    readback.status !== 'ok' ||
                    readback.trade?.id !== tradeId ||
                    readback.trade?.trade_type !== t.booking.product ||
                    readback.book?.name !== fixtures.book
                )
                    throw Error('Native trade readback does not match the confirmed booking.');
                const repeated = await call('/chat', request);
                if (
                    repeated.evidence.find((e) => e.tool === t.expect.tool && e.status === 'booked')
                        ?.tradeId !== tradeId
                )
                    throw Error('Repeated confirmation did not return the same verified trade.');
            }
            if ((await countTrades()) !== before + t.booking.delta)
                throw Error('Unexpected number of persisted trades after the draft action.');
        }
        if (t.poll)
            await pollReport(
                post,
                reply,
                t.poll,
                timeout,
                record,
                fixtures.expected[t.poll + 'Base'],
            );
    }
}

async function seedReports(clone, settings, fixture) {
    if (fixture.run === fixture.partialRun)
        throw Error('The partial-run fixture needs its own UUID.');
    const sql = await readFile(new URL('./native-report-seed.sql', import.meta.url), 'utf8');
    const report = JSON.parse(
        await clone.sql(sql, {
            tenant: settings.tenantId,
            party: settings.partyId,
            run: fixture.run,
            partialRun: fixture.partialRun,
            riskRun: fixture.riskRun,
            portfolio: fixture.portfolio,
            scenario: fixture.scenario,
            riskDefinition: fixture.riskDefinition,
            riskDate: fixture.riskDate,
        }),
    );
    if (report.partialCopied) fixture.expected.tradePartial = fixture.expected.tradeNative;
    if (report.vegaSeeded) {
        if (![1, 2].includes(fixture.vegaOption)) throw Error('Synthetic vega has two choices.');
        fixture.expected.vega = {
            'buckets.0.total.figure': String(report.riskTrades * fixture.vegaOption * 2),
        };
    }
    if (report.scenarioSeeded)
        fixture.expected.scenario = { 'buckets.0.total.figure': String(report.riskTrades * 3) };
    return report;
}

async function main() {
    const [settingsPath, ...args] = process.argv.slice(2);
    const focused = args.indexOf('--case');
    const caseId = focused >= 0 ? args.splice(focused, 2)[1] : undefined;
    const [outputPath, ...extra] = args;
    if (settingsPath === '--help') {
        console.log(usage);
        return;
    }
    if (settingsPath === '--list') {
        const fixture = Object.fromEntries(
            Object.keys(fixtureSchema.shape).map((key) => [key, 'TEST']),
        );
        fixture.expected = {};
        const id = '11111111-1111-4111-8111-000000000001';
        fixture.ir = nativeMarketData(id, id).ir;
        const { reads, workflows } = await nativeCases(fixture, id, id);
        console.log([...reads, ...workflows].map((test) => test.id).join('\n'));
        return;
    }
    if (!settingsPath || extra.length || (focused >= 0 && !caseId)) throw Error(usage);
    const settings = settingsSchema.parse(await readJson(settingsPath));
    const base = dirname(resolve(settingsPath));
    const config = await loadConfig(resolve(base, settings.agentConfig));
    const broker = config.nats && new URL(config.nats.server);
    if (
        !broker ||
        broker.protocol !== 'tls:' ||
        broker.username ||
        broker.password ||
        !['127.0.0.1', 'localhost', '[::1]'].includes(broker.hostname)
    )
        throw Error('Use a loopback TLS NATS endpoint for the disposable native test instance.');
    const output = resolve(outputPath ?? join(root, 'build/output/agent-native', randomUUID()));
    await mkdir(dirname(output), { recursive: true });
    await mkdir(output, { mode: 0o700 });
    const credentials = credentialSchema.parse(await readJson(resolve(base, settings.credentials)));
    const redact = redactor([credentials.tenantPassword, credentials.systemPassword]);
    const results = [];
    const clone = settings.clone ? databaseClone(settings.clone, base) : undefined;
    const serviceEnv = clone && {
        ...clone.serviceEnv,
        ORES_AGENT_NATIVE_OUTPUT: output,
        ORES_AGENT_NATIVE_NATS_PORT: broker.port || '4222',
        ORES_AGENT_NATIVE_NATS_PREFIX: config.nats.prefix,
    };
    let backend,
        sessions,
        writer,
        app,
        fleet,
        isolation,
        started = false;
    let stage = 'start';
    let stopped = false;
    try {
        if (clone) {
            stage = 'clone-database';
            console.log(JSON.stringify({ stage, database: clone.name }));
            await clone.restore();
            stage = 'install-clone-marker';
            await clone.installMarker(settings.tenantId, settings.partyId);
            stage = 'pause-report-schedules';
            isolation = await clone.pauseReportSchedules();
            isolation.beforeStart = await clone.verifyReportIsolation(isolation.since);
            await writeFile(
                join(output, 'report-isolation.json'),
                JSON.stringify(isolation, null, 2),
                { mode: 0o600 },
            );
            console.log(JSON.stringify({ stage, paused: isolation.paused }));
            if (settings.clone.indexLegacyTicks) {
                stage = 'index-legacy-ticks';
                console.log(JSON.stringify({ stage }));
                const index = await clone.indexLegacyTicks();
                await writeFile(
                    join(output, 'market-export-index.json'),
                    JSON.stringify(index, null, 2),
                    { mode: 0o600 },
                );
            }
        }
        stage = 'start';
        started = true;
        if (settings.start.foreground) {
            fleet = foregroundFleet(settings.start, base, output, redact, serviceEnv);
            await fleet.ready();
        } else await runCommand(settings.start, stage, base, output, redact, serviceEnv);
        stage = 'authenticate';
        backend = await NatsBackend.open(config.nats);
        sessions = new Sessions(backend);
        app = await httpAgent(config, backend, output, credentials, settings, redact);
        if (clone) {
            stage = 'verify-clone';
            const result = await app.post('/tools/find_books', { query: clone.marker.id });
            if (
                result.status !== 'ok' ||
                !result.rows?.some(
                    (row) => row.id === clone.marker.id && row.name === clone.marker.name,
                )
            )
                throw Error(
                    'Native OreStudio did not return the unique book from this database clone. No test writes were attempted.',
                );
        }
        if (clone) {
            stage = 'verify-report-isolation';
            isolation.afterStart = await clone.verifyReportIsolation(isolation.since);
            await writeFile(
                join(output, 'report-isolation.json'),
                JSON.stringify(isolation, null, 2),
                { mode: 0o600 },
            );
        }
        stage = 'seed';
        if (settings.seed) await runCommand(settings.seed, stage, base, output, redact, serviceEnv);
        const fixture = fixtureSchema.parse(await readJson(resolve(base, settings.fixtures)));
        fixture.ir = nativeMarketData(settings.tenantId, settings.partyId).ir;
        if (clone && settings.clone.seedReports) {
            stage = 'seed-reports';
            const report = await seedReports(clone, settings, fixture);
            await writeFile(join(output, 'report-seed.json'), JSON.stringify(report, null, 2), {
                mode: 0o600,
            });
        }
        stage = 'authenticate-seed';
        const tenant = await sessions.login({
            principal: credentials.tenantUsername,
            password: credentials.tenantPassword,
            partyId: settings.partyId,
        });
        if (tenant.tenantId !== settings.tenantId || tenant.partyId !== settings.partyId)
            throw Error('Refusing to seed a different tenant or party.');
        const system = await sessions.login({
            principal: credentials.systemUsername,
            password: credentials.systemPassword,
        });
        stage = 'seed-market';
        writer = await datasetNats(config.nats);
        const seeded = await seedNativeMarket(
            settings.tenantId,
            settings.partyId,
            writer,
            sessions.require('Bearer ' + system.accessToken),
            sessions.require('Bearer ' + tenant.accessToken),
        );
        await writeFile(join(output, 'market-seed.json'), JSON.stringify(seeded, null, 2), {
            mode: 0o600,
        });
        stage = 'verify-seed';
        try {
            await ensureDataset(app.post, fixture, (data) =>
                appendFile(
                    join(output, 'seed-readback.jsonl'),
                    JSON.stringify(redact.clean(data)) + '\n',
                    { mode: 0o600 },
                ),
            );
        } catch (error) {
            if (!clone) throw error;
            results.push({ id: stage, status: 'fail', error: redact.clean(error.message) });
        }
        stage = 'evaluate';
        const { reads, workflows } = await nativeCases(
            fixture,
            settings.tenantId,
            settings.partyId,
        );
        const caseIds = caseId ? new Set(caseId.split(',')) : undefined;
        const selected = [...reads, ...workflows].filter(
            (test) => !caseIds || caseIds.has(test.id),
        );
        if (caseIds && selected.length !== caseIds.size)
            throw Error(
                'Unknown native case: ' +
                    [...caseIds]
                        .filter((id) => !selected.some((test) => test.id === id))
                        .join(', '),
            );
        for (const [index, test] of selected.entries()) {
            const result = { id: test.id, status: 'pass' };
            const record = (data) =>
                appendFile(
                    join(output, 'conversations.jsonl'),
                    JSON.stringify(redact.clean({ case: test.id, ...data })) + '\n',
                    { mode: 0o600 },
                );
            try {
                // Renew only between cases. The production session has a per-minute
                // tool quota and report progress can stay active for several minutes.
                if (index % 8 === 0 || test.id.startsWith('execute-report-')) await app.login();
                await runCase(test, app.post, fixture, settings.reportTimeoutMs, record);
            } catch (error) {
                result.status = 'fail';
                result.error = redact.clean(error.message);
            }
            results.push(result);
            console.log(JSON.stringify(result));
        }
        if (clone) {
            stage = 'verify-final-report-isolation';
            isolation.afterCases = await clone.verifyReportIsolation(isolation.since);
            await writeFile(
                join(output, 'report-isolation.json'),
                JSON.stringify(isolation, null, 2),
                { mode: 0o600 },
            );
        }
    } catch (error) {
        results.push({ id: stage, status: 'fail', error: redact.clean(error.message) });
    } finally {
        for (const [id, cleanup] of [
            ['agent-stop', () => app?.stop()],
            ['seed-stop', () => writer?.close()],
            ['logout', () => sessions?.close()],
            ['backend-stop', () => backend?.close()],
            [
                'stop',
                async () => {
                    if (fleet) await fleet.stop();
                    if (started && settings.stop)
                        await runCommand(settings.stop, 'stop', base, output, redact, serviceEnv);
                    stopped = true;
                },
            ],
            [
                'drop-clone',
                async () => {
                    if (clone && stopped) await clone.drop();
                },
            ],
        ]) {
            try {
                await cleanup();
            } catch (error) {
                results.push({ id, status: 'fail', error: redact.clean(error.message) });
            }
        }
        const pass = results.filter((result) => result.status === 'pass').length;
        const summary = {
            environment: 'native OreStudio over TLS NATS via agent HTTP',
            ...(clone ? { clone: clone.name, fleetStopped: stopped } : {}),
            pass,
            fail: results.length - pass,
            results,
        };
        await writeFile(join(output, 'results.json'), JSON.stringify(summary, null, 2), {
            mode: 0o600,
        });
        console.log(JSON.stringify({ output, pass, fail: summary.fail }));
        for (const result of results.filter((result) => result.error))
            console.error(result.id + ': ' + result.error);
        process.exitCode = summary.fail || !pass ? 1 : 0;
    }
}
if (process.argv[1] && fileURLToPath(import.meta.url) === resolve(process.argv[1]))
    main().catch((error) => {
        console.error(error.message);
        process.exitCode = 1;
    });

// SPDX-License-Identifier: GPL-3.0-or-later
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdtemp, unlink, rmdir } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import test from 'node:test';
import { NpvRuns } from '../dist/npv-runs.js';
import { createApp } from '../dist/server.js';
import { fixtureSession, id } from '../evals/fixtures.mjs';

const review = 'Run NPV report using definition "Acme NPV" on 2025-06-01';
const reviewWithResults = review + ', then show results for book Group FX Hedges.';
const signal = () => new AbortController().signal;
const confirmation = (answer) => answer.match(/Confirm NPV run [a-f\d-]{36}/)[0];
const success = { outcome: 'ok' };

async function fixture(t) {
    const directory = await mkdtemp(join(tmpdir(), 'ores-npv-'));
    const session = fixtureSession();
    const def = {
        id: id(8),
        tenant_id: session.tenantId,
        party_id: session.partyId,
        version: 1,
        name: 'Acme NPV',
        report_type: 'risk',
        pre_processing: 'execute',
        post_processing: 'execute',
        skipped_families: '',
        cut_kind_code: null,
        cut_id: null,
    };
    const document = {
        success: true,
        party_id: def.party_id,
        document: {
            settings: {
                tenant_id: def.tenant_id,
                party_id: def.party_id,
                market_data_type: 'date',
                market_data_date: '2025-06-01',
                base_currency: 'GBP',
            },
            analytics: [{ analytic: { analytic_type_code: 'npv', active: 'Y' } }],
        },
    };
    const instances = [];
    const calls = [];
    const state = { health: 'running', state_name: 'running' };
    const historical = { tenant_id: def.tenant_id, party_id: def.party_id, recorded_at: '2025-06-01T00:00:00Z' };
    const output = {
        id: id(12), ...historical, report_instance_id: id(9), report_definition_id: def.id,
        as_of: '2025-06-01', subject_type: 'trade', subject_id: id(11), measure: 'npv_base',
        currency: 'GBP', figure: 125, source: 'npv.csv',
    };
    const backend = {
        ready: () => true,
        close: async () => {},
        async request(op, args, identity) {
            calls.push({ op, args });
            if (op === 'login')
                return {
                    success: true,
                    token: 'fixture-token',
                    session_id: 'fixture-session',
                    username: 'fixture',
                    tenant_id: def.tenant_id,
                    selected_party_id: def.party_id,
                };
            if (op === 'logout') return { success: true };
            if (op === 'reportDefinitions')
                return { result: success, total: 1, definitions: [def] };
            if (op === 'runDocument') return document;
            if (op === 'reportInstances')
                return { result: success, total: instances.length, instances };
            if (op === 'runHealth') return { result: success, ...state };
            if (op === 'books') return { result: success, total: 1, books: [{
                ...historical, id: id(30), name: 'Group FX Hedges', parent_portfolio_id: id(31), functional_currency: 'GBP',
            }] };
            if (op === 'portfolios') return { result: success, total: 1, portfolios: [{
                ...historical, id: id(31), name: 'Group Treasury', parent_portfolio_id: null, aggregation_ccy: 'GBP',
            }] };
            if (op === 'trades') return { result: success, total: 1, trades: [{ ...historical, id: id(11) }] };
            if (op === 'tradeBookings') return { result: success, total: 1, trade_bookings: [{
                ...historical, trade_id: id(11), book_id: id(30),
            }] };
            if (op === 'reportMetrics') return { result: success, total: 1, metrics: [{
                ...output, measure: 'NPV(Base)', value: 125, family: 'npv_report',
                scenario: '', factor: '', qualifiers: '{"BaseCurrency":"GBP"}',
            }] };
            if (op === 'trade')
                return {
                    result: success,
                    trade: { id: id(11), tenant_id: def.tenant_id, party_id: def.party_id },
                };
            if (op === 'runResults')
                return {
                    result: success,
                    total: 1,
                    results: [output],
                };
            if (op === 'triggerReport') {
                assert.equal(identity.partyId, def.party_id);
                assert.deepEqual(args, {
                    report_definition_id: def.id,
                    tenant_id: def.tenant_id,
                    job_instance_id: 0,
                    kind: 'production',
                    covered_date: '',
                    scope_code: '',
                    business_centre_code: '',
                });
                instances.push({
                    id: id(9),
                    name: 'Acme NPV 2025-06-01',
                    definition_id: def.id,
                    tenant_id: def.tenant_id,
                    party_id: def.party_id,
                    priced_at: null,
                    price_note: '',
                    completed_at: null,
                    started_at: '2025-06-01T10:00:00Z',
                    cut_id: null,
                    cut_label: null,
                    cut_timepoint: null,
                    engine_settings_id: null,
                    engine_settings_version: null,
                });
                return { result: { outcome: 'ok', code: 'triggered' }, report_instance_id: id(9) };
            }
            throw Error(`Unexpected fixture operation ${op}`);
        },
    };
    t.after(async () => {
        const lock = join(directory, `${session.tenantId}-${session.partyId}-${def.id}.json`);
        await unlink(lock).catch((e) => {
            if (e.code !== 'ENOENT') throw e;
        });
        await rmdir(directory);
    });
    return {
        directory,
        session,
        def,
        document,
        backend,
        calls,
        instances,
        state,
        service: new NpvRuns(backend, directory),
    };
}

test('web chat reviews, explicitly submits once, and reads the new run status', async (t) => {
    const f = await fixture(t);
    const app = createApp(
        {
            host: '127.0.0.1',
            port: 0,
            allowedOrigins: [],
            nats: null,
            inference: { baseUrl: 'http://127.0.0.1:1/v1', model: 'unused', timeoutMs: 100 },
        },
        f.backend,
        f.directory,
    );
    app.listen(0, '127.0.0.1');
    await once(app, 'listening');
    try {
        const base = `http://127.0.0.1:${app.address().port}`;
        const post = async (path, payload, token, status = 200) => {
            const r = await fetch(base + path, {
                method: 'POST',
                headers: {
                    'content-type': 'application/json',
                    ...(token ? { authorization: 'Bearer ' + token } : {}),
                },
                body: JSON.stringify(payload),
            });
            assert.equal(r.status, status);
            return r.json();
        };
        const { accessToken } = await post('/auth/login', {
            principal: 'fixture',
            password: 'fixture',
        });
        const chat = (message) => post('/chat', { message }, accessToken);
        const first = await chat(reviewWithResults);
        assert.match(first.answer, /2025-06-01/);
        assert.equal(first.evidence[0].resultScope.id, id(30));
        assert.equal(f.calls.filter((c) => c.op === 'triggerReport').length, 0);
        const confirm = confirmation(first.answer);
        const second = await chat(confirm);
        assert.match(second.answer, /running/);
        assert.equal(second.evidence[0].runId, id(9));
        assert.equal(second.reportFollow, id(9));
        const poll = () => post('/chat/report-progress', { runId: id(9) }, accessToken);
        assert.equal((await poll()).watching, true);
        await post('/chat/report-progress', { runId: id(13) }, accessToken, 404);
        assert.deepEqual((await chat(confirm)).evidence, second.evidence);
        assert.equal(f.calls.filter((c) => c.op === 'triggerReport').length, 1);
        Object.assign(f.state, { health: 'completed clean', state_name: 'completed' });
        Object.assign(f.instances[0], {
            priced_at: '2025-06-01',
            completed_at: '2025-06-01T12:00:00Z',
        });
        const done = await poll();
        assert.equal(done.watching, false);
        assert.match(done.answer, /125 GBP/);
        assert.equal(done.evidence[1].request.runId, id(9));
        assert.equal(done.evidence[1].scope.id, id(30));
        assert.equal(done.evidence[1].status, 'ok');
        t.diagnostic('REPORT_TRANSCRIPT ' + JSON.stringify([
            { request: reviewWithResults, answer: first.answer, evidence: first.evidence },
            { request: confirm, answer: second.answer, evidence: second.evidence },
            { request: { runId: id(9) }, answer: done.answer, evidence: done.evidence },
        ]));
        const stored = await post(
            '/tools/read_trade_results',
            {
                tradeId: id(11),
                runId: id(9),
                measure: 'npv_base',
                valuationDate: '2025-06-01',
            },
            accessToken,
        );
        assert.equal(stored.status, 'ok');
        assert.equal(stored.result.figure, 125);
        const directTrigger = await fetch(base + '/tools/triggerReport', {
            method: 'POST',
            headers: { 'content-type': 'application/json', authorization: 'Bearer ' + accessToken },
            body: '{}',
        });
        assert.equal(directTrigger.status, 400);
        assert.equal(f.calls.filter((c) => c.op === 'triggerReport').length, 1);
    } finally {
        await app.shutdown();
    }
});

test('execution scope is resolved without treating it as a trigger override', async t => {
    const f = await fixture(t);
    const invoke = message => f.service.handle(message, f.session, signal());
    const scoped = await invoke('Run the NPV report for book Group FX Hedges at 2025-06-01, then show the results.');
    assert.equal(scoped.evidence[0].status, 'execution_scope_unverified');
    assert.equal(scoped.evidence[0].scope.id, id(30));
    assert.doesNotMatch(scoped.answer, /Confirm NPV run/);
    const unknown = await invoke(review + ', then show results for portfolio Unknown.');
    assert.equal(unknown.evidence[0].status, 'scope_required');
    const matched = await invoke(review + ', then show results for portfolio Group Treasury.');
    assert.equal(matched.evidence[0].resultScope.id, id(31));
    assert.equal(f.calls.filter(call => call.op === 'triggerReport').length, 0);
});

test('failed and wrong-date reports withhold results, and stopping a status read leaves the run active', async t => {
    const f = await fixture(t);
    const invoke = message => f.service.handle(message, f.session, signal());
    const first = await invoke(reviewWithResults);
    await invoke(confirmation(first.answer));
    const stopped = new AbortController();
    stopped.abort();
    await assert.rejects(f.service.progress(id(9), f.session, stopped.signal), { name: 'AbortError' });
    assert.equal((await invoke('Follow run ' + id(9))).reportFollow, id(9));
    Object.assign(f.state, { health: 'failed', state_name: 'failed' });
    f.instances[0].output_message = 'Trading export was unavailable.';
    const failed = await f.service.progress(id(9), f.session, signal());
    assert.equal(failed.watching, false);
    assert.match(failed.answer, /Trading export was unavailable/);
    Object.assign(f.state, { health: 'completed clean', state_name: 'completed' });
    Object.assign(f.instances[0], { priced_at: '2025-06-02', completed_at: '2025-06-02T12:00:00Z' });
    const wrongDate = await f.service.progress(id(9), f.session, signal());
    assert.match(wrongDate.answer, /matching the reviewed date/);
    assert.equal(f.calls.filter(call => call.op === 'runResults').length, 0);
    assert.equal(f.calls.filter(call => call.op === 'triggerReport').length, 1);
});

test('a named risk report can be reviewed with configured non-NPV analytics', async t => {
    const f = await fixture(t);
    f.document.document.analytics[0].analytic.analytic_type_code = 'sensitivity';
    const outcome = await f.service.handle('Run report using definition "Acme NPV" on 2025-06-01, then show the results.', f.session, signal());
    assert.equal(outcome.evidence[0].status, 'confirmation_required');
    assert.deepEqual(outcome.evidence[0].analytics, ['sensitivity']);
    assert.match(outcome.answer, /Confirm report run/);
    const scoped = await f.service.handle('Run report using definition "Acme NPV" on 2025-06-01, then show results for book Group FX Hedges.', f.session, signal());
    assert.equal(scoped.evidence[0].status, 'result_family_unavailable');
    assert.equal(f.calls.filter(call => call.op === 'triggerReport').length, 0);
});

test('confirmation is bound to the reviewed identity and settings', async (t) => {
    const f = await fixture(t);
    const invoke = (message, session = f.session) => f.service.handle(message, session, signal());
    const first = await invoke(review);
    const token = confirmation(first.answer);
    assert.match((await invoke(token, fixtureSession())).answer, /No matching/);
    f.document.document.settings.base_currency = 'USD';
    assert.match((await invoke(token)).answer, /changed after review/);
    assert.match(
        (await invoke(review.replace('2025-06-01', '2025-06-02'))).answer,
        /uses 2025-06-01/,
    );
    assert.equal(f.calls.filter((c) => c.op === 'triggerReport').length, 0);
});

test('ambiguous transport outcome survives restart and is reconciled without resubmission', async (t) => {
    const f = await fixture(t);
    const request = f.backend.request.bind(f.backend);
    f.backend.request = async (...args) => {
        const r = await request(...args);
        if (args[0] === 'triggerReport') throw Error('Reply lost after admission');
        return r;
    };
    const first = await f.service.handle(review, f.session, signal());
    const token = confirmation(first.answer);
    const outcomes = await Promise.all([
        f.service.handle(token, f.session, signal()),
        f.service.handle(token, f.session, signal()),
    ]);
    assert.ok(outcomes.some((r) => r.evidence[0]?.status === 'submission_unknown'));
    const restarted = new NpvRuns(f.backend, f.directory);
    const check = await restarted.handle(review, fixtureSession(), signal());
    assert.equal(check.evidence[0].status, 'submission_unknown');
    assert.equal(check.evidence[0].candidates[0].id, id(9));
    assert.equal(f.calls.filter((c) => c.op === 'triggerReport').length, 1);
});

// SPDX-License-Identifier: GPL-3.0-or-later
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import test from 'node:test';
import { nativeCases } from '../evals/native-cases.mjs';
import { nativeMarketData } from '../evals/native-market-data.mjs';
import { runCase } from '../evals/native.mjs';

const tenant = '11111111-1111-4111-8111-000000000001';
const party = '11111111-1111-4111-8111-000000000002';
const fixture = JSON.parse(
    await readFile(new URL('../evals/native-fixtures.example.json', import.meta.url), 'utf8'),
);

test('native corpus exercises real stored-data IDs and all workflow families', async () => {
    const market = nativeMarketData(tenant, party);
    const { reads, workflows } = await nativeCases({ ...fixture, ir: market.ir }, tenant, party);
    const cases = [...reads, ...workflows];
    assert.equal(new Set(cases.map((item) => item.id)).size, cases.length);
    assert.ok(cases.length >= 60);
    for (const id of [
        'book-fx-forward',
        'book-sofr-swap',
        'execute-report-trade',
        'execute-report-book',
        'execute-report-portfolio',
        'execute-report-risk',
        'trade-partial-npv',
        'fx-vega',
        'stored-forward-curve',
        'fx-surface-flat-reference',
    ])
        assert.ok(
            cases.some((item) => item.id === id),
            id,
        );
    const spot = market.datasets[0].observations[0];
    const prompt = reads.find((item) => item.id === 'fx-curve-nats-reference').turns[0].message;
    assert.ok(prompt.includes(spot.series_id));
    assert.ok(prompt.includes(spot.source));
    assert.equal(market.datasets.flatMap((item) => item.observations).length, 14);
});

test('native booking acceptance fails when confirming twice persists a duplicate', async () => {
    let count = 2;
    let confirmationCalls = 0;
    const post = async (path) => {
        if (path === '/chat/reset') return {};
        if (path === '/tools/find_trades') return { status: 'ok', totalMatches: count };
        if (path === '/tools/read_trade')
            return {
                status: 'ok',
                trade: { id: fixture.trade, trade_type: 'FxForward' },
                book: { name: fixture.book },
            };
        count++;
        confirmationCalls++;
        return {
            answer: 'Booked',
            evidence: [{ tool: 'book_fx_forward', status: 'booked', tradeId: fixture.trade }],
        };
    };
    const scenario = {
        turns: [
            {
                message: 'Confirm a draft',
                expect: { tool: 'book_fx_forward', statuses: ['booked'] },
                booking: { product: 'FxForward', delta: 1 },
            },
        ],
    };
    await assert.rejects(
        runCase(scenario, post, fixture, 1000, async () => {}),
        /Unexpected number of persisted trades/,
    );
    assert.equal(confirmationCalls, 2);
});

test('native report acceptance requires verified scoped results from the new run', async () => {
    const post = async (path) =>
        path === '/chat'
            ? {
                  answer: 'OreStudio recorded run',
                  evidence: [{ status: 'recorded', runId: fixture.run, outcome: 'ok' }],
              }
            : path === '/chat/report-progress'
              ? {
                    watching: false,
                    evidence: [
                        {
                            tool: 'read_run_status',
                            state_name: 'completed',
                            health: 'completed clean',
                            run: { id: fixture.run },
                        },
                    ],
                }
              : {};
    const scenario = {
        turns: [
            {
                message: 'Confirm a run',
                expect: { contains: ['OreStudio recorded run'] },
                poll: 'book',
            },
        ],
    };
    await assert.rejects(
        runCase(scenario, post, fixture, 1000, async () => {}),
        /no verified book results/,
    );
});

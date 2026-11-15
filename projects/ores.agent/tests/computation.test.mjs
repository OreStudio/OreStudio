import test from 'node:test';
import assert from 'node:assert/strict';
import { fileURLToPath } from 'node:url';
import { Agent } from '../dist/agent.js';
import { PythonSandbox } from '../dist/python-sandbox.js';
import { ToolExecutor } from '../dist/tools.js';
import { fixtureSession } from '../evals/fixtures.mjs';

const backend = { request: () => assert.fail('Computation must not call the backend.') };
const executable = process.env.ORES_TEST_WASMTIME;
const module = process.env.ORES_TEST_PYTHON_WASM;
const runtime = executable && module ? new PythonSandbox({ executable, module }) : undefined;
const signal = new AbortController().signal;

test('a formatted read result is available to the next model tool call for a compound request', async () => {
    let turn = 0;
    const agent = new Agent({ complete: async (messages) => {
        if (turn === 1) assert.equal(JSON.parse(messages.at(-1).content).rows[0].figure, 42);
        if (turn === 2) return { role: 'assistant', content: 'The calculation is 84.' };
        const call = [ { name: 'find_trades', arguments: '{}' }, { name: 'calculate', arguments: '{"operation":"product","left":"42","right":"2"}' } ][turn++];
        return { role: 'assistant', content: null, tool_calls: [{ id: 'step-' + turn, type: 'function', function: call }] };
    } }, {
        execute: async name => name === 'find_trades'
            ? { tool: name, status: 'ok', rows: [{ id: fixtureSession().partyId, figure: 42 }], totalMatches: 1 }
            : { tool: name, status: 'ok', value: '84' },
    });
    const result = await agent.chat({ message: 'Find trades and then calculate twice the returned figure.' }, fixtureSession(), signal);
    assert.deepEqual(result.evidence.map(row => row.tool), ['find_trades', 'calculate']);
    assert.equal(result.answer, 'The calculation is 84.');
});

test('discovery chains through five detail reads and Python without a special analysis tool', async () => {
    const ids = Array.from({ length: 5 }, (_, i) => '11111111-1111-4111-8111-' + String(i + 1).padStart(12, '0'));
    const reads = [];
    let turn = 0;
    const call = (name, args) => ({ role: 'assistant', content: null, tool_calls: [{ id: 'step-' + turn, type: 'function', function: { name, arguments: JSON.stringify(args) } }] });
    const agent = new Agent({ complete: async (messages, tools) => {
        const step = turn++;
        assert.ok(!tools.some(t => t.function.name === 'analyze_trade_page'));
        if (step === 0) return call('find_trades', { limit: 5 });
        if (step <= 5) return call('read_trade', { tradeId: ids[step - 1] });
        if (step === 6) return call('run_python', { data: ['10','20','30','40','50'], code: 'from statistics import median\nfrom decimal import Decimal\nresult = str(median(Decimal(x) for x in data))' });
        assert.equal(JSON.parse(messages.at(-1).content).value, '30');
        return { role: 'assistant', content: 'The median sold amount is 30 GBP.' };
    } }, { execute: async (name, args) => {
        if (name === 'find_trades') return { tool: name, status: 'ok', rows: ids.map(id => ({ id })), totalMatches: 12 };
        if (name === 'read_trade') {
            const i = reads.push(args.tradeId);
            return { tool: name, status: 'ok', trade: { id: args.tradeId }, economicsStatus: 'available', bookingPortfolioHierarchy: [], directPortfolios: [], economics: { sold_amount: String(i * 10), sold_currency: 'GBP' } };
        }
        assert.equal(name, 'run_python');
        assert.deepEqual(args.data, ['10','20','30','40','50']);
        return { tool: name, status: 'ok', value: '30' };
    } }, undefined, undefined, undefined, true);
    const result = await agent.chat({ message: 'call find_trades and calculate the median of the sold amount for the first 5 results' }, fixtureSession(), signal);
    assert.deepEqual(reads, ids);
    assert.deepEqual(result.evidence.map(row => row.tool), ['find_trades', ...ids.map(() => 'read_trade'), 'run_python']);
    assert.equal(result.answer, 'The median sold amount is 30 GBP.');
});

test('different trade calculations use the same verified input and sandbox workflow', async () => {
    const tradeId = '049da6ed-e746-5cf4-af7a-399e045e9849';
    for (const [operation, expression] of [['square root', 'sold[0].sqrt()'], ['cube', 'sold[0] ** 3']]) {
        const agent = new Agent({ complete: async (_, tools) => {
            const name = tools[0].function.name;
            assert.equal(name, 'plan_trade_analysis');
            return { role: 'assistant', content: null, tool_calls: [{
                id: 'analysis', type: 'function', function: { name, arguments: JSON.stringify({ expression }) },
            }] };
        } }, {
            execute: async (name, args) => {
                if (name === 'run_python') {
                    assert.deepEqual(args.data, [{ tradeId, sold: { amount: '144', currency: 'GBP' } }]);
                    assert.ok(args.code.includes(expression));
                    return { tool: name, status: 'ok', value: 'fixture result' };
                }
                assert.equal(name, 'read_trade');
                assert.deepEqual(args, { tradeId });
                return { tool: name, status: 'ok', economicsStatus: 'available',
                    trade: { id: tradeId, trade_type: 'FxForward' },
                    bookingPortfolioHierarchy: [], directPortfolios: [],
                    economics: { bought_amount: '81', bought_currency: 'EUR', sold_amount: '144', sold_currency: 'GBP' } };
            },
        }, undefined, undefined, undefined, true);
        const result = await agent.chat({
            message: 'calculate the ' + operation + ' of the sold value of trade ' + tradeId,
        }, fixtureSession(), signal);
        assert.deepEqual(result.evidence.map(entry => entry.tool), ['read_trade', 'run_python']);
        assert.match(result.answer, /fixture result/);
    }
});

test('trade detail and Bought tables read the requested IDs without submitting writes', async () => {
    const ids = ['05dcc4f0-4af9-597f-8e20-94c3018c6b4c', '0974c799-6804-57fb-9243-e6a5b2aae4cb'];
    const agent = new Agent({ complete: async () => assert.fail('Exact IDs need no model') }, {
        execute: async (name, { tradeId }) => {
            assert.equal(name, 'read_trade');
            return { tool: name, status: 'ok', trade: { id: tradeId, trade_type: 'FxForward' },
                bookingPortfolioHierarchy: [], directPortfolios: [], economicsStatus: 'available',
                economics: { bought_amount: '42', bought_currency: 'GBP', sold_amount: '53', sold_currency: 'USD' } };
        },
    }, undefined, { handle: () => assert.fail('Table request must not submit a report') });
    for (const [prompt, columns] of [
        ['create a table for the details of the following trade IDs:', '| Trade ID | Type | Book |'],
        ['create a table the bought value of each of the following trade IDs:', '| Trade ID | Bought |'],
    ]) {
        const result = await agent.chat({ message: prompt + ' ' + ids.join(' ') }, fixtureSession(), signal);
        assert.deepEqual(result.evidence.map(row => row.trade.id), ids);
        assert.ok(result.answer.startsWith(columns));
        assert.match(result.answer, /42 GBP/);
    }
});

test('summing Bought reads all seven exact trade details and groups their amounts by currency', async () => {
    const ids = [
        '049da6ed-e746-5cf4-af7a-399e045e9849',
        '05dcc4f0-4af9-597f-8e20-94c3018c6b4c',
        '0974c799-6804-57fb-9243-e6a5b2aae4cb',
        '0acdffba-feae-5d46-9816-7eccabb8f91a',
        '0c71846f-5675-54ba-bb47-f02760f5e2c8',
        '18aea2d8-92da-5483-b133-1fc32912fc28',
        '19c8afc6-5bf6-5e33-b162-0b5d07f772b4',
    ];
    const calls = [];
    const agent = new Agent({ complete: async () => assert.fail('No model call is needed') }, {
        execute: async (name, args) => {
            assert.equal(name, 'read_trade');
            calls.push(args.tradeId);
            return {
                tool: name, status: 'ok', trade: { id: args.tradeId }, economicsStatus: 'available',
                bookingPortfolioHierarchy: [], directPortfolios: [],
                economics: { bought_amount: '0.1', bought_currency: calls.length <= 4 ? 'EUR' : 'GBP', sold_amount: '100' },
            };
        },
    });
    const result = await agent.chat({
        message: 'sum the bought field of the following trades by getting the detail for each of these trade ids, extracting the bought field and summing across all the trade ids: ' + ids.join('\\\n'),
    }, fixtureSession(), signal);
    assert.deepEqual(calls, ids);
    assert.equal(result.selectionRequired, undefined);
    assert.deepEqual(result.evidence.at(-1).totals, [
        { currency: 'EUR', value: '0.4', count: 4 }, { currency: 'GBP', value: '0.3', count: 3 },
    ]);
});

test('report selection preserves the Python step and passes the retrieved figures', async () => {
    const request = 'Get the latest verified base-currency NPV for the “Group FX Hedges” book and show its top five trade contributors. Using Python, calculate the top two share.';
    const runId = '884d0bc9-932d-4c4c-9b66-ee97d2b810bb';
    const rows = ['8', '-4', '3', '3', '2'].map((figure) => ({ figure, currency: 'USD' }));
    const agent = new Agent({
        complete: async (messages, _, __, options) => {
            assert.equal(options.tool, 'analyze_scope_contributors');
            assert.match(messages.at(-1).content, /top two share/);
            return { role: 'assistant', content: null, tool_calls: [{
                id: 'python', type: 'function', function: {
                    name: options.tool, arguments: JSON.stringify({ code: 'result = "60%"' }),
                },
            }] };
        },
    }, {
        execute: async (name, args) => {
            if (name === 'run_python') {
                assert.deepEqual(args.data, rows);
                return { tool: name, status: 'ok', value: '60%' };
            }
            assert.equal(args.scope, 'Group FX Hedges');
            assert.equal(args.limit, 5);
            if (!args.runId) return { tool: name, status: 'selection_required', field: 'runId',
                reason: 'Select a run', request: args, candidates: [{ id: runId, name: 'NPV' }] };
            assert.equal(args.runId, runId);
            return { tool: name, status: 'ok', contributors: rows };
        },
    }, undefined, undefined, undefined, true);
    const session = fixtureSession();
    const choice = await agent.chat({ message: request }, session, signal);
    assert.equal(choice.selectionRequired, 'runId');
    const result = await agent.chat({ message: runId }, session, signal);
    assert.deepEqual(result.evidence.map(({ tool }) => tool), ['summarize_scope_results', 'run_python']);
    assert.match(result.answer, /60%/);
});

test('exact decimal calculations go through the agent tool and evidence', async () => {
    let turn = 0;
    const agent = new Agent({ complete: async (messages, tools) => {
        assert.ok(tools.some(tool => tool.function.name === 'calculate'));
        assert.ok(!tools.some(tool => tool.function.name === 'run_python'));
        if (turn++) {
            assert.equal(JSON.parse(messages.at(-1).content).value, '12.5');
            return { role: 'assistant', content: 'The rate increased by 12.5 basis points.' };
        }
        return { role: 'assistant', content: null, tool_calls: [{
            id: '1', type: 'function', function: { name: 'calculate', arguments: JSON.stringify({
                operation: 'basis_point_change', oldRate: '0.02', newRate: '0.02125',
            }) },
        }] };
    } }, new ToolExecutor(backend, () => {}));
    const result = await agent.chat({ message: 'Calculate the change from 2% to 2.125% in basis points.' }, fixtureSession(), signal);
    assert.equal(result.evidence[0].value, '12.5');
});

test('division and percentages round exact decimal operands and reject a zero denominator', async () => {
    const executor = new ToolExecutor(backend, () => {});
    const session = fixtureSession();
    assert.equal((await executor.execute('calculate', { operation: 'divide', left: '1', right: '-8', places: 2 }, session)).value, '-0.13');
    assert.equal((await executor.execute('calculate', { operation: 'percentage', part: '0.1', whole: '0.3', places: 4 }, session)).value, '33.3333');
    await assert.rejects(executor.execute('calculate', { operation: 'divide', left: '1', right: '0.00', places: 2 }, session), { code: 'division_by_zero' });
});

test('Python runs through chat when configured', { skip: !runtime }, async () => {
    let turn = 0;
    const agent = new Agent({ complete: async (messages, tools) => {
        assert.ok(tools.some(tool => tool.function.name === 'run_python'));
        if (turn++) {
            assert.equal(JSON.parse(messages.at(-1).content).value, 4);
            return { role: 'assistant', content: 'The median is 4.' };
        }
        return { role: 'assistant', content: null, tool_calls: [{
            id: '1', type: 'function', function: { name: 'run_python', arguments: JSON.stringify({
                code: 'from statistics import median\nresult = median(data)', data: [1, 4, 6],
            }) },
        }] };
    } }, new ToolExecutor(backend, () => {}, runtime), undefined, undefined, undefined, true);
    const response = await agent.chat({ message: 'Compute the median of 1, 4, 6 using Python.' }, fixtureSession(), signal);
    assert.deepEqual(response.evidence, [{ tool: 'run_python', status: 'ok', value: 4 }]);
});

test('Python returns printed output through chat when data is omitted', { skip: !runtime }, async () => {
    let turn = 0;
    const code = 'import random\nprint([random.randint(1, 1000) for _ in range(4)])';
    const agent = new Agent({ complete: async messages => {
        if (turn++) {
            const output = JSON.parse(messages.at(-1).content);
            assert.equal(output.status, 'ok', JSON.stringify(output));
            const values = JSON.parse(output.value);
            assert.equal(values.length, 4);
            assert.ok(values.every(n => Number.isInteger(n) && n >= 1 && n <= 1000));
            return { role: 'assistant', content: output.value };
        }
        return { role: 'assistant', content: null, tool_calls: [{
            id: 'random', type: 'function', function: { name: 'run_python', arguments: JSON.stringify({ code }) },
        }] };
    } }, new ToolExecutor(backend, () => {}, runtime), undefined, undefined, undefined, true);
    const response = await agent.chat({ message: 'Print four random integers using Python.' }, fixtureSession(), signal);
    assert.equal(response.answer, response.evidence[0].value);
    assert.equal((await runtime.run({ code: 'print("debug")\nresult = None', data: null })).value, null);
    await assert.rejects(runtime.run({ code: 'x = 1' }), { code: 'python_missing_output' });
});

test('Python has no host filesystem, network, subprocesses, environment, or cross-call state', { skip: !runtime }, async () => {
    const data = fileURLToPath(import.meta.url);
    const check = await runtime.run({ data, code: `
import builtins, os
result = {'env': dict(os.environ), 'previous': hasattr(builtins, 'ore_probe')}
for key, action in [
    ('read', lambda: open(data).read(1)),
    ('write', lambda: open(data, 'ab').close()),
    ('directory', lambda: os.listdir('.')),
    ('process', lambda: __import__('subprocess').run(['python', '--version'])),
    ('network', lambda: __import__('socket').socket()),
]:
    try:
        action()
        result[key] = True
    except (OSError, ImportError, AttributeError, ValueError):
        result[key] = False
builtins.ore_probe = True
` });
    assert.deepEqual(check.value, { env: {}, previous: false, read: false, write: false, directory: false, process: false, network: false });
    assert.equal((await runtime.run({ data: null, code: "import builtins\nresult = hasattr(builtins, 'ore_probe')" })).value, false);
});

test('Python enforces input, execution, memory, output, and cancellation limits', { skip: !runtime }, async () => {
    await assert.rejects(runtime.run({ code: 'result = data', data: 'x'.repeat(16384) }), { code: 'python_input_limit' });
    await assert.rejects(runtime.run({ code: 'while True: pass', data: null }), { code: 'python_failed' });
    await assert.rejects(runtime.run({ code: 'result = len(bytearray(80 * 1024 * 1024))', data: null }), { code: 'python_failed' });
    await assert.rejects(runtime.run({ code: "result = 'x' * 9000", data: null }), { code: 'python_output_limit' });
    await assert.rejects(runtime.run({ code: "print('x' * 9000)" }), { code: 'python_output_limit' });
    await assert.rejects(runtime.run({ code: 'result = 2 ** 53', data: null }), { code: 'python_result_precision' });
    const controller = new AbortController();
    const pending = runtime.run({ code: 'while True: pass', data: null }, controller.signal);
    await assert.rejects(runtime.run({ code: 'result = 7', data: null }), { code: 'python_busy' });
    controller.abort();
    await assert.rejects(pending, { name: 'AbortError' });
    assert.equal((await runtime.run({ code: 'result = 7', data: null })).value, 7);
});

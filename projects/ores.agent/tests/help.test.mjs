import assert from 'node:assert/strict';
import test from 'node:test';
import { Agent } from '../dist/agent.js';
import { ToolExecutor, tools } from '../dist/tools.js';
import { catalogue } from '../dist/catalogue.js';
import { version } from '../dist/version.js';

const session = () => ({
    active: true,
    expires: Date.now() + 60_000,
    chatBusy: false,
    window: Date.now(),
    calls: 0,
    busy: 0,
});
const signal = new AbortController().signal;
const executor = new ToolExecutor({
    request: async () => assert.fail('Help must not query business data'),
}, () => {});

test('model-selected tool inspection returns the full registry and exact schemas', async () => {
    const ask = (message, args) => new Agent({
        complete: async (messages, visible) => {
            if (messages.at(-1).role === 'tool') return { role: 'assistant', content: JSON.parse(messages.at(-1).content).definition };
            assert.equal(messages.at(-1).content, message);
            assert.ok(visible.some(t => t.function.name === 'search_help'));
            const inspector = visible.find(t => t.function.name === 'inspect_tools');
            assert.ok(inspector);
            return { role: 'assistant', content: null, tool_calls: [{
                id: 'inspection', type: 'function',
                function: { name: inspector.function.name, arguments: JSON.stringify(args) },
            }] };
        },
    }, executor).chat({ message }, session(), signal);
    const listed = await ask('list all tools', {});
    const names = [...listed.answer.matchAll(/^- `(\w+)`$/gm)].map(match => match[1]);
    assert.deepEqual(names, [...Object.keys(tools), 'inspect_tools', 'enable_tools']);
    const definition = await ask("what's the funciton definition of search_help", { name: 'search_help' });
    assert.match(definition.answer, /`search_help` is available/);
    const json = definition.answer.split('```')[1].slice(4);
    assert.deepEqual(JSON.parse(json), catalogue('help')[0]);
    const missing = await ask('do you have a missing_tool tool?', { name: 'missing_tool' });
    assert.match(missing.answer, /don’t have a registered tool/);
});

test('a named search_help request invokes help directly', async () => {
    const agent = new Agent({ complete: async () => assert.fail('A known help topic needs no model') }, executor);
    const result = await agent.chat(
        { message: 'Use search_help to find how to create a book in the web app.' },
        session(), signal,
    );
    assert.match(result.answer, /Create a book in the web app/);
    assert.equal(result.evidence[0].tool, 'search_help');
});

test('chat answers a known how-to with bundled instructions and a source', async () => {
    const agent = new Agent(
        { complete: async () => assert.fail('A direct help match needs no inference') },
        executor,
        undefined,
        { handle: async () => assert.fail('A how-to must bypass action handlers') },
    );
    const reply = await agent.chat(
        { message: 'How do I run a report in the web app?' }, session(), signal,
    );
    assert.match(reply.answer, /Run now/);
    assert.match(reply.answer, /\[Source\]\(https:\/\/orestudio\.github\.io\/OreStudio\/doc\/knowledge\//);
    assert.equal(reply.evidence[0].tool, 'search_help');
    assert.equal(reply.evidence[0].version, version);
});

test('chat retrieves desktop guidance from the published user manual', async () => {
    const agent = new Agent({ complete: async () => assert.fail('A direct help match needs no inference') }, executor);
    const reply = await agent.chat(
        { message: 'How do I compare book versions in the desktop app?' }, session(), signal,
    );
    assert.match(reply.answer, /Trading → Books/);
    assert.equal(reply.evidence[0].matches[0].url,
        'https://orestudio.github.io/OreStudio/doc/manual/user_guide/chapter_books.html');
});

test('the model can refine a missed lookup through the help tool only', async () => {
    let inferenceCalls = 0;
    const agent = new Agent({
        complete: async (_, catalogue) => {
            inferenceCalls++;
            assert.deepEqual(catalogue.map((tool) => tool.function.name), ['search_help']);
            return {
                role: 'assistant',
                content: null,
                tool_calls: [{
                    id: 'help',
                    type: 'function',
                    function: {
                        name: 'search_help',
                        arguments: JSON.stringify({ query: 'download report results' }),
                    },
                }],
            };
        },
    }, executor);
    const reply = await agent.chat(
        { message: 'Where can I grab the artefacts my overnight batch generated?' },
        session(),
        signal,
    );
    assert.equal(inferenceCalls, 1);
    assert.deepEqual(reply.evidence.map(({ status }) => status), ['not_found', 'ok']);
    assert.match(reply.answer, /Downloads/);
});

test('missing help does not use an unsupported answer from the model', async () => {
    const agent = new Agent({
        complete: async () => ({ role: 'assistant', content: 'Invented steps' }),
    }, executor);
    const reply = await agent.chat(
        { message: 'How do I create a dashboard in the web app?' }, session(), signal,
    );
    assert.match(reply.answer, /no matching instructions/);
    assert.equal(reply.evidence[0].status, 'not_found');
});

test('record requests continue to use the business tool catalogue', async () => {
    const agent = new Agent({
        complete: async (_, catalogue) => {
            const names = catalogue.map((tool) => tool.function.name);
            assert.ok(names.includes('find_books'));
            assert.ok(names.includes('search_help'));
            return { role: 'assistant', content: 'fixture' };
        },
    }, executor);
    const reply = await agent.chat({ message: 'Show my books' }, session(), signal);
    assert.equal(reply.answer, 'fixture');
});

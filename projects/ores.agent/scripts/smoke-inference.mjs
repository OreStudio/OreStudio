import assert from 'node:assert/strict';
import { loadConfig } from '../dist/config.js';
import { HttpInference } from '../dist/inference.js';
const config = await loadConfig(process.argv[2]);
const inference = new HttpInference(config.inference), signal = AbortSignal.timeout(60000);
const start = Date.now();
const plain = await inference.complete([{ role: 'user', content: 'Reply with the single word ready.' }], [], signal);
assert.ok(plain.content?.length);
const messages = [{ role: 'system', content: 'Use list_currencies to answer the user. After receiving the tool result, summarize only the returned data.' },
    { role: 'user', content: 'List the first three configured currencies. Call list_currencies with limit 3.' }];
const tools = [{ type: 'function', function: { name: 'list_currencies', description: 'Read configured currencies.', parameters: { type: 'object', properties: { limit: { type: 'integer', minimum: 1, maximum: 50 } }, required: ['limit'], additionalProperties: false } } }];
const call = await inference.complete(messages, tools, signal);
assert.equal(call.tool_calls?.[0]?.function.name, 'list_currencies');
assert.equal(JSON.parse(call.tool_calls[0].function.arguments).limit, 3);
messages.push(call, { role: 'tool', tool_call_id: call.tool_calls[0].id, content: JSON.stringify({ source: 'LABELLED INFERENCE FIXTURE, not live OreStudio', rows: [{ iso_code: 'GBP', name: 'Pound sterling' }], total: 1 }) });
const replay = await inference.complete(messages, tools, signal);
assert.ok(replay.content?.includes('GBP') || replay.content?.toLowerCase().includes('sterling'));
console.log(JSON.stringify({ model: config.inference.model, plain: plain.content, tool: call.tool_calls[0].function.name, replay: replay.content, durationMs: Date.now() - start }, null, 2));

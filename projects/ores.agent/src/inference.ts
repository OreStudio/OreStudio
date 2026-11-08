// SPDX-License-Identifier: GPL-3.0-or-later
import { readFile } from 'node:fs/promises';
import { z } from 'zod';
import { AppError } from './backend.js';
import type { Config } from './config.js';
export interface Message { role: 'system' | 'user' | 'assistant' | 'tool'; content: string | null; tool_call_id?: string; tool_calls?: ToolCall[] }
export interface ToolCall { id: string; type: 'function'; function: { name: string; arguments: string } }
export interface Inference { complete(messages: Message[], tools: unknown[], signal: AbortSignal): Promise<Message> }
const messageSchema = z.object({ role: z.literal('assistant'), content: z.string().max(16000).nullable().optional(),
    tool_calls: z.array(z.object({ id: z.string().min(1).max(200), type: z.literal('function'), function: z.object({ name: z.string().max(100), arguments: z.string().max(8192) }) })).max(4).optional() });
export class HttpInference implements Inference {
    constructor(private config: Config['inference']) {}
    async complete(messages: Message[], tools: unknown[], signal: AbortSignal): Promise<Message> {
        const key = this.config.apiKeyFile ? (await readFile(this.config.apiKeyFile, 'utf8')).trim() : undefined;
        const timeout = AbortSignal.timeout(this.config.timeoutMs);
        try {
            const response = await fetch(this.config.baseUrl.replace(/\/$/, '') + '/chat/completions', {
                method: 'POST', headers: { 'content-type': 'application/json', ...(key ? { Authorization: 'Bearer ' + key } : {}) },
                signal: AbortSignal.any([signal, timeout]),
                body: JSON.stringify({ model: this.config.model, messages, tools, tool_choice: 'auto', parallel_tool_calls: false,
                    temperature: 0.1, top_k: 50, repeat_penalty: 1.1, max_tokens: this.config.maxOutputTokens ?? 2048, stream: false }),
            });
            if (!response.ok || !response.body) { await response.body?.cancel(); throw new AppError('inference_unavailable', 'Inference request failed.', 503); }
            const chunks: Uint8Array[] = []; let size = 0;
            for await (const chunk of response.body) {
                size += chunk.length;
                if (size > 65536) { throw new AppError('oversized_inference', 'Inference reply exceeds limit.'); }
                chunks.push(chunk);
            }
            const raw = JSON.parse(Buffer.concat(chunks).toString('utf8'));
            if (raw.choices?.[0]?.finish_reason === 'length') throw new AppError('generation_limit', 'Model exceeded its response budget; narrow the request.', 422);
            const parsed = messageSchema.parse(raw.choices?.[0]?.message);
            return { ...parsed, content: parsed.content ?? null };
        } catch (error) {
            if (signal.aborted) throw new AppError('cancelled', 'Request cancelled.', 499);
            if (error instanceof AppError) throw error;
            throw new AppError('inference_failed', 'Inference timed out or returned an invalid response.', 503);
        }
    }
}

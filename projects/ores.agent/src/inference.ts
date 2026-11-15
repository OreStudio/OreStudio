// SPDX-License-Identifier: GPL-3.0-or-later
import { readFile } from 'node:fs/promises';
import { AsyncLocalStorage } from 'node:async_hooks';
import { z } from 'zod';
import { AppError } from './backend.js';
import type { Config } from './config.js';
export interface Message {
    role: 'system' | 'user' | 'assistant' | 'tool';
    content: string | null;
    tool_call_id?: string;
    tool_calls?: ToolCall[];
}
export interface ToolCall {
    id: string;
    type: 'function';
    function: { name: string; arguments: string };
}
export interface Inference {
    complete(
        messages: Message[],
        tools: unknown[],
        signal: AbortSignal,
        options?: { tool?: string; disableThinking?: boolean },
    ): Promise<Message>;
}
const messageSchema = z.object({
    role: z.literal('assistant'),
    content: z.string().max(16000).nullable().optional(),
    tool_calls: z
        .array(
            z.object({
                id: z.string().min(1).max(200),
                type: z.literal('function'),
                function: z.object({ name: z.string().max(100), arguments: z.string().max(8192) }),
            }),
        )
        .max(4)
        .optional(),
});

const reasoningStream = new AsyncLocalStorage<(text: string) => void>();
export function withInferenceReasoning<T>(emit: (text: string) => void, run: () => T): T {
    return reasoningStream.run(emit, run);
}

const streamChunk = z.object({
    choices: z
        .array(
            z.object({
                finish_reason: z.string().nullable().optional(),
                delta: z
                    .object({
                        content: z.string().nullish(),
                        reasoning_content: z.string().nullish(),
                        tool_calls: z
                            .array(
                                z.object({
                                    index: z.number().int().min(0).max(3),
                                    id: z.string().optional(),
                                    function: z
                                        .object({
                                            name: z.string().optional(),
                                            arguments: z.string().optional(),
                                        })
                                        .optional(),
                                }),
                            )
                            .optional(),
                    })
                    .optional(),
            }),
        )
        .max(1),
});

function checkGenerationLimit(reason: unknown) {
    if (reason === 'length')
        throw new AppError(
            'generation_limit',
            'Model exceeded its response budget; narrow the request.',
            422,
        );
}

async function readCompletionStream(
    body: NonNullable<Response['body']>,
    emit: (text: string) => void,
) {
    const decoder = new TextDecoder();
    const calls = new Map<number, ToolCall>();
    let content = '';
    let pending = '';
    let size = 0;
    let done = false;
    let finishReason: string | undefined;
    for await (const chunk of body) {
        size += chunk.length;
        if (size > 8 * 1024 * 1024)
            throw new AppError('oversized_inference', 'Inference reply exceeds limit.');
        const lines = (pending + decoder.decode(chunk, { stream: true })).split('\n');
        pending = lines.pop()!;
        for (const line of lines) {
            if (!line.startsWith('data:')) continue;
            const data = line.slice(5).trim();
            if (data === '[DONE]') {
                done = true;
                break;
            }
            const choice = streamChunk.parse(JSON.parse(data)).choices[0];
            if (!choice) continue;
            if (choice.finish_reason) finishReason = choice.finish_reason;
            const delta = choice.delta;
            if (delta?.reasoning_content) emit(delta.reasoning_content);
            content += delta?.content ?? '';
            for (const part of delta?.tool_calls ?? []) {
                const call = calls.get(part.index) ?? {
                    id: '',
                    type: 'function',
                    function: { name: '', arguments: '' },
                };
                if (part.id) call.id = part.id;
                call.function.name += part.function?.name ?? '';
                call.function.arguments += part.function?.arguments ?? '';
                calls.set(part.index, call);
            }
        }
        if (done) break;
    }
    checkGenerationLimit(finishReason);
    if (!done || !finishReason) throw Error('Incomplete inference stream.');
    return messageSchema.parse({
        role: 'assistant',
        content: content || null,
        tool_calls: [...calls.entries()].sort(([a], [b]) => a - b).map(([, call]) => call),
    });
}

export class HttpInference implements Inference {
    constructor(private config: Config['inference']) {}
    async complete(
        messages: Message[],
        tools: unknown[],
        signal: AbortSignal,
        options?: { tool?: string; disableThinking?: boolean },
    ): Promise<Message> {
        const key = this.config.apiKeyFile
            ? (await readFile(this.config.apiKeyFile, 'utf8')).trim()
            : undefined;
        const timeout = AbortSignal.timeout(this.config.timeoutMs);
        const emitReasoning = options?.disableThinking ? undefined : reasoningStream.getStore();
        try {
            const response = await fetch(
                this.config.baseUrl.replace(/\/$/, '') + '/chat/completions',
                {
                    method: 'POST',
                    headers: {
                        'content-type': 'application/json',
                        ...(key ? { Authorization: 'Bearer ' + key } : {}),
                    },
                    signal: AbortSignal.any([signal, timeout]),
                    body: JSON.stringify({
                        model: this.config.model,
                        messages,
                        tools,
                        tool_choice:
                            tools.length === 0
                                ? 'none'
                                : options?.tool
                                  ? { type: 'function', function: { name: options.tool } }
                                  : 'auto',
                        ...(options?.disableThinking
                            ? { chat_template_kwargs: { enable_thinking: false } }
                            : {}),
                        parallel_tool_calls: false,
                        temperature: options ? 0 : 0.1,
                        top_k: 50,
                        repeat_penalty: 1.1,
                        max_tokens: this.config.maxOutputTokens ?? 2048,
                        stream: Boolean(emitReasoning),
                    }),
                },
            );
            if (!response.ok || !response.body) {
                await response.body?.cancel();
                throw new AppError('inference_unavailable', 'Inference request failed.', 503);
            }
            if (
                emitReasoning &&
                response.headers.get('content-type')?.includes('text/event-stream')
            ) {
                const parsed = await readCompletionStream(response.body, emitReasoning);
                return { ...parsed, content: parsed.content ?? null };
            }
            const chunks: Uint8Array[] = [];
            let size = 0;
            for await (const chunk of response.body) {
                size += chunk.length;
                if (size > 65536) {
                    throw new AppError('oversized_inference', 'Inference reply exceeds limit.');
                }
                chunks.push(chunk);
            }
            const raw = JSON.parse(Buffer.concat(chunks).toString('utf8'));
            checkGenerationLimit(raw.choices?.[0]?.finish_reason);
            const parsed = messageSchema.parse(raw.choices?.[0]?.message);
            return { ...parsed, content: parsed.content ?? null };
        } catch (error) {
            if (signal.aborted) throw new AppError('cancelled', 'Request cancelled.', 499);
            if (error instanceof AppError) throw error;
            throw new AppError(
                'inference_failed',
                'Inference timed out or returned an invalid response.',
                503,
            );
        }
    }
}

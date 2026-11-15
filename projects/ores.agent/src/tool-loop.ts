// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { AppError } from './backend.js';
import type { Inference, Message } from './inference.js';
import { preserveReadSelectors, type RequestPlan, type ToolArguments } from './intent.js';
import {
    enableTools,
    enableToolsInput,
    inspectTools,
    workflowCatalogue,
    type ToolGroup,
} from './catalogue.js';
import { ToolReferences } from './tool-references.js';
import { timedAsync } from './telemetry.js';

export interface ToolOutput {
    result?: unknown;
    answer?: string;
    selectionRequired?: string;
    requiresInput?: boolean;
}
export interface WorkflowState {
    plan: RequestPlan;
    messages: Message[];
    groups: ToolGroup[];
    evidence: unknown[];
    calls: number;
    cache: Record<string, ToolOutput>;
    presentations: string[];
}
export interface WorkflowReply {
    answer: string;
    incomplete?: true;
    selectionRequired?: string;
    suspended?: WorkflowState;
}
export interface ResolvedCall {
    name: string;
    args: ToolArguments;
}

const maxCalls = 16;
const maxContextCharacters = 48000;
const signature = (name: string, args: ToolArguments) =>
    name +
    JSON.stringify(Object.fromEntries(Object.entries(args).sort(([a], [b]) => a.localeCompare(b))));

export async function runToolLoop(options: {
    state: WorkflowState;
    inference: Inference;
    pythonEnabled: boolean;
    signal: AbortSignal;
    check: () => void;
    execute: (name: string, args: ToolArguments) => Promise<ToolOutput>;
    resolved?: ResolvedCall;
}): Promise<WorkflowReply> {
    const { state, inference, signal, check, execute, pythonEnabled } = options;
    const references = new ToolReferences();
    for (const result of state.evidence) references.observe(result);
    let resolved = options.resolved;
    let repeated = 0;
    const failures = new Map<string, number>();
    let stopped = '';
    const complete = async (tools: ReturnType<typeof workflowCatalogue>) => {
        try {
            return await timedAsync('inference', () =>
                inference.complete(state.messages, tools, signal),
            );
        } catch (error) {
            check();
            if (!(error instanceof AppError) || error.code !== 'generation_limit') throw error;
            // No tool has executed from a truncated completion. Retry that same
            // step once without a thinking budget, retaining all verified results.
            return timedAsync('inference', () =>
                inference.complete(state.messages, tools, signal, { disableThinking: true }),
            );
        }
    };
    const finishLimited = async (reason: string): Promise<WorkflowReply> => {
        const notice = `The task is incomplete: ${reason}. Completed tool results are retained in the evidence.`;
        if (JSON.stringify(state.messages).length <= maxContextCharacters) {
            state.messages.push({
                role: 'system',
                content:
                    notice +
                    ' No more tools may run. Summarize only verified progress and explicitly identify unfinished work. Do not claim the task is complete.',
            });
            try {
                const final = await complete([]);
                check();
                if (!final.tool_calls?.length && final.content)
                    return { answer: notice + '\n\n' + final.content, incomplete: true };
            } catch (error) {
                check();
                if (!(error instanceof AppError)) throw error;
            }
        }
        return {
            answer: [notice, ...state.presentations.slice(-3)].join('\n\n'),
            incomplete: true,
        };
    };

    // A resumed call was selected by the user through the existing clarification
    // handlers. Keep that selection for subsequent calls without weakening literal constraints.
    if (resolved) {
        for (const key of [
            'runId',
            'tradeId',
            'scope',
            'scopeType',
            'provider',
            'factor',
            'factor2',
            'scenario',
            'currency',
        ] as const) {
            if (typeof resolved.args[key] === 'string')
                Object.assign(state.plan, { [key]: resolved.args[key] });
        }
        if (typeof resolved.args.seriesId === 'string')
            state.plan.marketSeriesId = resolved.args.seriesId;
    }
    while (state.calls < maxCalls) {
        check();
        if (JSON.stringify(state.messages).length > maxContextCharacters)
            return finishLimited('the conversation reached its context limit');
        const available = workflowCatalogue(state.groups, pythonEnabled);
        const approved = resolved;
        resolved = undefined;
        const answer: Message = approved
            ? {
                  role: 'assistant',
                  content: null,
                  tool_calls: [
                      {
                          id: `resumed-${state.calls}`,
                          type: 'function',
                          function: {
                              name: approved.name,
                              arguments: JSON.stringify(approved.args),
                          },
                      },
                  ],
              }
            : await complete(available);
        check();
        if (!answer.tool_calls?.length) {
            if (!answer.content) throw new AppError('empty_answer', 'Model returned no answer.');
            return { answer: answer.content };
        }
        state.messages.push(answer);
        let pause: ToolOutput | undefined;
        let terminal: ToolOutput | undefined;
        for (const call of answer.tool_calls) {
            let output: ToolOutput;
            if (pause || terminal || state.calls >= maxCalls) {
                output = {
                    result: {
                        error: 'deferred_call',
                        message:
                            'Not executed. A prior call needs input or the execution budget is exhausted.',
                    },
                };
            } else {
                state.calls++;
                try {
                    if (
                        !approved &&
                        !available.some((tool) => tool.function.name === call.function.name)
                    )
                        throw new AppError(
                            'tool_not_in_catalogue',
                            'Use enable_tools to make the required tool group available.',
                            400,
                        );
                    const input: unknown = JSON.parse(call.function.arguments);
                    if (call.function.name === enableTools.function.name) {
                        const { groups } = enableToolsInput.parse(input);
                        state.groups = [...new Set([...state.groups, ...groups])];
                        output = {
                            result: { tool: 'enable_tools', status: 'ok', enabled: state.groups },
                        };
                    } else if (call.function.name === 'inspect_tools') {
                        output = {
                            result: {
                                tool: 'inspect_tools',
                                status: 'ok',
                                definition: inspectTools(input, pythonEnabled),
                            },
                        };
                    } else {
                        const args =
                            approved?.args ??
                            preserveReadSelectors(
                                call.function.name,
                                input,
                                state.plan,
                                references,
                            );
                        const key = signature(call.function.name, args);
                        if (Object.hasOwn(state.cache, key)) {
                            output = state.cache[key]!;
                            if (++repeated >= 2)
                                stopped =
                                    'the model repeated completed tool calls without progressing';
                        } else {
                            output = await execute(call.function.name, args);
                            check();
                            if (output.result !== undefined) {
                                state.evidence.push(output.result);
                                references.observe(output.result);
                            }
                            if (output.answer) state.presentations.push(output.answer);
                            if (output.requiresInput || output.selectionRequired) pause = output;
                            else if (output.result === undefined && output.answer)
                                terminal = output;
                            else state.cache[key] = output;
                        }
                    }
                } catch (error) {
                    check();
                    const key = call.function.name + call.function.arguments;
                    const count = (failures.get(key) ?? 0) + 1;
                    failures.set(key, count);
                    if (count >= 2)
                        stopped = 'the model repeated a failed tool call without correcting it';
                    output = {
                        result: {
                            error: error instanceof AppError ? error.code : 'invalid_arguments',
                            message:
                                error instanceof z.ZodError
                                    ? error.issues
                                          .map(
                                              (issue) =>
                                                  `${issue.path.join('.')}: ${issue.message}`,
                                          )
                                          .join('; ')
                                    : error instanceof SyntaxError
                                      ? 'Arguments must be valid JSON.'
                                      : 'Tool did not produce data. Inspect its schema and correct the arguments; do not invent a result.',
                        },
                    };
                }
            }
            const result = output.result;
            state.messages.push({
                role: 'tool',
                tool_call_id: call.id,
                content: JSON.stringify(
                    output.answer
                        ? {
                              ...(result && typeof result === 'object' && !Array.isArray(result)
                                  ? result
                                  : { result }),
                              formattedAnswer: output.answer,
                          }
                        : (result ?? { error: 'unavailable' }),
                ),
            });
        }
        if (terminal) return { answer: terminal.answer! };
        if (pause)
            return {
                answer: pause.answer ?? 'Please clarify the requested selection.',
                selectionRequired: pause.selectionRequired,
                suspended: state,
            };
        if (stopped) return finishLimited(stopped);
    }
    return finishLimited('the tool-call limit was reached');
}

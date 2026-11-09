// SPDX-License-Identifier: GPL-3.0-or-later
import { AsyncLocalStorage } from 'node:async_hooks';
import { randomUUID } from 'node:crypto';
import type { Backend } from './backend.js';

export type Phase = 'agent' | 'selection' | 'inference' | 'tool' | 'adapter' | 'render';
export interface TimingEvent {
    type: 'timing';
    traceId: string;
    spanId: string;
    parentSpanId?: string;
    phase: Phase;
    durationMs: number;
    outcome: 'ok' | 'error';
    errorCode?: string;
}
type Observer = (event: TimingEvent) => void;
const context = new AsyncLocalStorage<{
    traceId: string;
    parentSpanId?: string;
    observe: Observer;
}>();
const safeErrors = new Set([
    'cancelled',
    'session_expired',
    'agent_busy',
    'generation_limit',
    'context_budget',
    'tool_budget',
    'turn_budget',
    'inference_failed',
    'inference_unavailable',
    'backend_unavailable',
    'backend_denied',
    'invalid_result_units',
    'inconsistent_provenance',
    'selection_not_requested',
    'rate_limited',
]);
function begin(phase: Phase) {
    const active = context.getStore();
    if (!active) return;
    const spanId = randomUUID(),
        start = performance.now();
    return {
        child: { ...active, parentSpanId: spanId },
        finish(error?: unknown, failed = false) {
            let code: unknown;
            try {
                if (error && typeof error === 'object' && 'code' in error) code = error.code;
            } catch {
                /* Error objects are not trusted telemetry data. */
            }
            const event: TimingEvent = {
                type: 'timing',
                traceId: active.traceId,
                spanId,
                parentSpanId: active.parentSpanId,
                phase,
                durationMs: performance.now() - start,
                outcome: failed ? 'error' : 'ok',
                ...(!failed
                    ? {}
                    : {
                          errorCode:
                              typeof code === 'string' && safeErrors.has(code)
                                  ? code
                                  : 'operation_failed',
                      }),
            };
            // Observability must not fail or change a request. No payloads, prompts,
            // identities, arguments, replies or exception messages enter this sink.
            try {
                active.observe(event);
            } catch {
                /* Best-effort local telemetry. */
            }
        },
    };
}
export function timed<T>(phase: Phase, work: () => T): T {
    const span = begin(phase);
    if (!span) return work();
    try {
        const value = context.run(span.child, work);
        span.finish();
        return value;
    } catch (error) {
        span.finish(error, true);
        throw error;
    }
}
export async function timedAsync<T>(phase: Phase, work: () => Promise<T>): Promise<T> {
    const span = begin(phase);
    if (!span) return work();
    try {
        const value = await context.run(span.child, work);
        span.finish();
        return value;
    } catch (error) {
        span.finish(error, true);
        throw error;
    }
}
export function trace<T>(
    requestId: string | undefined,
    observe: Observer,
    work: () => Promise<T>,
): Promise<T> {
    const traceId =
        requestId && /^[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}$/i.test(requestId)
            ? requestId
            : randomUUID();
    return context.run({ traceId, observe }, () => timedAsync('agent', work));
}
export function tracedBackend(backend: Backend): Backend {
    return {
        ready: () => backend.ready(),
        close: () => backend.close(),
        request: (...args) => timedAsync('adapter', () => backend.request(...args)),
    };
}

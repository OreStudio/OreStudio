// SPDX-License-Identifier: GPL-3.0-or-later
import type { Session } from './auth.js';
import type { RequestPlan, ToolArguments } from './intent.js';
import type { TradeTotalRequest } from './trade-total.js';
import type { MarketPointChoice } from './market-dialogue.js';
import { marketId } from './market.js';
import { type MarketCurveRequest, type MarketCurveField } from './fx-market-request.js';
import type { WorkflowState } from './tool-loop.js';

export interface Candidate {
    id: string;
    name: string;
    aliases?: string[];
}
type Choice =
    | {
          kind: 'market_curve';
          args: MarketCurveRequest;
          field: MarketCurveField;
          candidates: Candidate[];
      }
    | { kind: 'run'; plan: RequestPlan }
    | { kind: 'trade_total'; request: TradeTotalRequest }
    | {
          kind: 'analytics';
          name: 'build_fx_forward_curve' | 'build_fx_volatility_surface';
          args: ToolArguments;
      }
    | {
          kind: 'tool';
          name: string;
          args: ToolArguments;
          field: 'scope' | 'runId' | 'book' | 'portfolio' | 'currency' | 'factor';
          candidates: Candidate[];
          computationRequest?: string;
      }
    | { kind: 'contributors'; args: ToolArguments }
    | {
          kind: 'sensitivity_page';
          args: ToolArguments;
          nextBucketOffset: number | null;
          buckets: { number: number; id: string; nextOffset: number | null }[];
      }
    | MarketPointChoice;
export type PendingChoice = Choice & { workflow?: WorkflowState };
export type Continuation =
    | { kind: 'plan'; plan: RequestPlan }
    | { kind: 'trade_total'; request: TradeTotalRequest }
    | {
          kind: 'call';
          name: string;
          args: ToolArguments;
          computationRequest?: string;
          workflow?: WorkflowState;
      }
    | { kind: 'prompt'; answer: string };
const identity = (session: Session) =>
    JSON.stringify([session.tenantId, session.partyId, session.sessionId]);

export class Conversations {
    private histories = new WeakMap<
        Session,
        { identity: string; messages: import('./inference.js').Message[] }
    >();

    history(session: Session) {
        const saved = this.histories.get(session);
        return saved?.identity === identity(session) ? structuredClone(saved.messages) : [];
    }

    remember(session: Session, user: string, assistant: string) {
        const messages = [
            ...this.history(session),
            { role: 'user' as const, content: user },
            { role: 'assistant' as const, content: assistant },
        ].slice(-8);
        while (messages.reduce((n, m) => n + (m.content?.length ?? 0), 0) > 24000)
            messages.splice(0, 2);
        this.histories.set(session, { identity: identity(session), messages });
    }

    marketChoice(session: Session) {
        const choice = this.currentChoice(session);
        return choice?.kind === 'market_curve' ? structuredClone(choice) : undefined;
    }

    marketPointChoice(session: Session) {
        const choice = this.currentChoice(session);
        return choice?.kind === 'market_point' ? structuredClone(choice) : undefined;
    }

    workflow(session: Session) {
        const state = this.currentChoice(session)?.workflow;
        return state ? structuredClone(state) : undefined;
    }

    suspend(session: Session, workflow: WorkflowState) {
        const choice = this.currentChoice(session);
        if (choice) this.set(session, { ...choice, workflow });
    }

    private currentChoice(session: Session) {
        const pending = this.pending.get(session);
        if (!pending || pending.expires <= Date.now() || pending.identity !== identity(session))
            return;
        return pending.choice;
    }
    private pending = new WeakMap<
        Session,
        { identity: string; expires: number; choice: PendingChoice }
    >();

    set(session: Session, choice: PendingChoice) {
        this.pending.set(session, {
            identity: identity(session),
            expires: Date.now() + 300000,
            choice: structuredClone(choice),
        });
    }

    clear(session: Session) {
        this.pending.delete(session);
    }

    reset(session: Session) {
        this.clear(session);
        this.histories.delete(session);
    }

    resume(session: Session, message: string): Continuation | undefined {
        const pending = this.pending.get(session);
        this.clear(session);
        if (!pending || pending.expires <= Date.now() || pending.identity !== identity(session))
            return;
        const choice = pending.choice;
        if (choice.kind === 'trade_total') {
            const runId = runSelector(message);
            if (runId) return { kind: 'trade_total', request: { ...choice.request, runId } };
            return;
        }
        // Market replies are interpreted contextually by the LLM in Agent.
        if (choice.kind === 'market_curve' || choice.kind === 'market_point') return;
        if (choice.kind === 'analytics') {
            try {
                const data: unknown = JSON.parse(
                    message.replace(/^\s*```(?:json)?\s*/i, '').replace(/\s*```\s*$/, ''),
                );
                if (data && typeof data === 'object' && !Array.isArray(data))
                    return {
                        kind: 'call',
                        name: choice.name,
                        args: { ...choice.args, ...data },
                        workflow: choice.workflow,
                    };
            } catch {
                /* An unrelated message clears the pending request. */
            }
            return;
        }
        if (choice.kind === 'run') {
            const id = runSelector(message);
            if (id) return { kind: 'plan', plan: { ...choice.plan, runId: id } };
            if (
                /^(?:use\s+)?(?:the\s+)?(?:latest|most recent)(?:\s+(?:clean|complete|completed))?(?:\s+run)?[.!]?$/i.test(
                    message,
                ) &&
                ['npv', 'npv_base'].includes(choice.plan.measure ?? '')
            )
                return { kind: 'plan', plan: { ...choice.plan, latest: true } };
            return;
        }
        if (choice.kind === 'contributors') {
            if (
                /^(?:(?:show|list)\s+(?:the\s+)?)?next(?:\s+(?:page|contributors?))?[.!]?$/i.test(
                    message,
                )
            )
                return { kind: 'call', name: 'summarize_scope_results', args: choice.args };
            return;
        }
        if (choice.kind === 'sensitivity_page') {
            const nextBuckets =
                /^(?:show\s+)?(?:the\s+)?next\s+(?:sensitivity\s+)?buckets[.!]?$/i.test(message);
            const list = /^(?:show|list)\s+(?:the\s+)?(?:sensitivity\s+)?buckets[.!]?$/i.test(
                message,
            );
            if (nextBuckets || list) {
                const offset = list ? 0 : choice.nextBucketOffset;
                if (offset === null) {
                    this.set(session, choice);
                    return {
                        kind: 'prompt',
                        answer: 'There are no more buckets on this selection. Say “Show buckets” to return to the first page.',
                    };
                }
                const args = { ...choice.args, offset: 0, bucketOffset: offset };
                delete (args as ToolArguments).bucketId;
                return { kind: 'call', name: 'summarize_scope_results', args };
            }
            const page =
                /^(?:(?:show|list)\s+)?(?:the\s+)?(next\s+)?contributors(?:\s+for\s+bucket\s+(\d+))?[.!]?$/i.exec(
                    message,
                );
            if (!page) return;
            const bucket = page[2]
                ? choice.buckets.find((item) => item.number === Number(page[2]))
                : choice.buckets.length === 1
                  ? choice.buckets[0]
                  : undefined;
            if (!bucket || (page[1] && bucket.nextOffset === null)) {
                this.set(session, choice);
                return {
                    kind: 'prompt',
                    answer: bucket
                        ? 'There are no more contributors for that bucket.'
                        : 'Choose a visible bucket number, for example “Show contributors for bucket 1”.',
                };
            }
            return {
                kind: 'call',
                name: 'summarize_scope_results',
                args: {
                    ...choice.args,
                    bucketId: bucket.id,
                    bucketOffset: 0,
                    offset: page[1] ? bucket.nextOffset : 0,
                },
            };
        }
        const literal = message
            .replace(/^(?:use|choose|select)\s+/i, '')
            .replace(/^(?:run|book|portfolio|currency|factor)\s+/i, '')
            .replace(/^["']|["'][.!]?$/g, '')
            .replace(/[.!]$/, '');
        const option = /^(?:option\s+)?([1-9]\d?)$/i.exec(literal)?.[1];
        const ordinal = option ? choice.candidates[Number(option) - 1] : undefined;
        const matches = ordinal
            ? [ordinal]
            : choice.candidates.filter(
                  (c) =>
                      c.id.toLowerCase() === literal.toLowerCase() ||
                      c.name.toLowerCase() === literal.toLowerCase(),
              );
        const run = choice.field === 'runId' ? runSelector(message) : undefined;
        if (run || matches.length === 1)
            return {
                kind: 'call',
                name: choice.name,
                computationRequest: choice.computationRequest,
                workflow: choice.workflow,
                args: {
                    ...choice.args,
                    [choice.field]: run ?? matches[0]!.id,
                },
            };
    }
}

function runSelector(message: string) {
    const value = message
        .replace(/^(?:(?:use|choose|select)\s+)?(?:run\s+)?/i, '')
        .replace(/[.!]$/, '');
    return marketId.safeParse(value).success ? value.toLowerCase() : undefined;
}

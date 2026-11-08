// SPDX-License-Identifier: GPL-3.0-or-later
import type { Session } from './auth.js';
import type { RequestPlan, ToolArguments } from './intent.js';
import { marketId } from './market.js';

export interface Candidate { id: string; name: string }
export type PendingChoice =
    | { kind: 'run'; plan: RequestPlan }
    | { kind: 'tool'; name: string; args: ToolArguments; field: 'scope' | 'runId' | 'book' | 'portfolio'; candidates: Candidate[] }
    | { kind: 'provider'; name: string; args: ToolArguments; candidates: { provider: string; seriesId: string }[] };
export type Continuation = { kind: 'plan'; plan: RequestPlan } | { kind: 'call'; name: string; args: ToolArguments };
const identity = (session: Session) => JSON.stringify([session.tenantId, session.partyId, session.sessionId]);

export class Conversations {
    private pending = new WeakMap<Session, { identity: string; expires: number; choice: PendingChoice }>();

    set(session: Session, choice: PendingChoice) {
        this.pending.set(session, { identity: identity(session), expires: Date.now() + 300000, choice: structuredClone(choice) });
    }

    clear(session: Session) { this.pending.delete(session); }

    resume(session: Session, message: string): Continuation | undefined {
        const pending = this.pending.get(session);
        this.clear(session);
        if (!pending || pending.expires <= Date.now() || pending.identity !== identity(session)) return;
        const choice = pending.choice;
        if (choice.kind === 'run') {
            if (marketId.safeParse(message).success) return { kind: 'plan', plan: { ...choice.plan, runId: message.toLowerCase() } };
            return;
        }
        if (choice.kind === 'provider') {
            const literal = /^(?:use\s+|provider\s+)?["']?([a-z0-9_.-]+)["']?(?:\s+please)?[.!]?$/i.exec(message)?.[1]?.toLowerCase();
            const selected = choice.candidates.filter(c => c.provider.toLowerCase() === literal || c.seriesId.toLowerCase() === literal);
            if (selected.length !== 1) return;
            const candidate = selected[0]!;
            const selector = literal === candidate.seriesId.toLowerCase() ? { seriesId: candidate.seriesId } : { provider: candidate.provider };
            return { kind: 'call', name: choice.name, args: { ...choice.args, ...selector } };
        }
        const matches = choice.candidates.filter(c => c.id.toLowerCase() === message.toLowerCase() || c.name.toLowerCase() === message.toLowerCase());
        const run = choice.field === 'runId' && marketId.safeParse(message).success;
        if (run || matches.length === 1) return { kind: 'call', name: choice.name, args: { ...choice.args, [choice.field]: run ? message.toLowerCase() : matches[0]!.id } };
    }
}

// SPDX-License-Identifier: GPL-3.0-or-later
import { randomBytes, createHash } from 'node:crypto';
import { z } from 'zod';
import { AppError, type Backend, type Identity } from './backend.js';

const loginInput = z.object({ principal: z.string().min(1).max(200), password: z.string().min(1).max(1024), partyId: z.uuid().optional() }).strict();
const party = z.object({ id: z.uuid(), name: z.string() });
const loginReply = z.object({ success: z.boolean(), token: z.string().default(''), session_id: z.string().default(''),
    username: z.string().default(''), tenant_id: z.string().default(''), selected_party_id: z.string().default(''),
    available_parties: z.array(party).default([]), access_lifetime_s: z.number().positive().max(86400).default(1800),
    password_reset_required: z.boolean().default(false) });
export interface Session extends Identity {
    username: string; tenantId: string; partyId: string; expires: number; active: boolean; busy: number;
    calls: number; window: number; chatBusy: boolean;
}
const hash = (s: string) => createHash('sha256').update(s).digest('hex');
export class Sessions {
    private entries = new Map<string, Session>();
    private loginCalls = 0;
    private loginWindow = 0;
    constructor(private backend: Backend) {}
    async login(raw: unknown): Promise<{ accessToken: string; expiresAt: string; username: string; tenantId: string; partyId: string }> {
        const now = Date.now();
        if (now - this.loginWindow > 60000) { this.loginWindow = now; this.loginCalls = 0; }
        if (++this.loginCalls > 10) throw new AppError('rate_limited', 'Too many login attempts. Try again in a minute.', 429);
        for (const [key, s] of this.entries) if (s.expires <= now) { s.active = false; this.entries.delete(key); }
        if (this.entries.size >= 64) throw new AppError('capacity', 'Session capacity reached.', 503);
        const input = loginInput.parse(raw);
        const parsed = loginReply.safeParse(await this.backend.request('login', { principal: input.principal, password: input.password }));
        if (!parsed.success || !parsed.data.success || !parsed.data.token || !parsed.data.session_id) throw new AppError('login_failed', 'Sign-in failed.', 401);
        const r = parsed.data;
        const identity = { token: r.token, sessionId: r.session_id };
        try {
            if (r.password_reset_required) throw new AppError('password_reset_required', 'Complete password reset in OreStudio first.', 403);
            const partyId = input.partyId ?? r.selected_party_id;
            if (!partyId || partyId === '00000000-0000-0000-0000-000000000000') {
                throw new AppError('party_required', 'Select a party: ' + r.available_parties.map(p => `${p.name} (${p.id})`).join(', '), 409);
            }
            if (input.partyId && r.selected_party_id && r.selected_party_id !== '00000000-0000-0000-0000-000000000000' && r.selected_party_id !== input.partyId) {
                throw new AppError('party_mismatch', 'Account already selected a different party; change its default party in OreStudio first.', 409);
            }
            if (!r.selected_party_id || r.selected_party_id === '00000000-0000-0000-0000-000000000000') {
                if (!r.available_parties.some(p => p.id === partyId)) throw new AppError('party_denied', 'Party is not available to this account.', 403);
                const selected = z.object({ success: z.boolean(), token: z.string(), access_lifetime_s: z.number().positive().max(86400).optional() }).parse(
                    await this.backend.request('selectParty', { party_id: partyId }, identity));
                if (!selected.success || !selected.token) throw new AppError('party_denied', 'Party selection failed.', 403);
                identity.token = selected.token;
                r.access_lifetime_s = Math.min(r.access_lifetime_s, selected.access_lifetime_s ?? r.access_lifetime_s);
            }
            const token = randomBytes(32).toString('base64url');
            const session: Session = { ...identity, username: r.username, tenantId: r.tenant_id, partyId,
                expires: Date.now() + Math.min(1800, r.access_lifetime_s) * 1000, active: true, busy: 0, calls: 0, window: now, chatBusy: false };
            this.entries.set(hash(token), session);
            return { accessToken: token, expiresAt: new Date(session.expires).toISOString(), username: session.username, tenantId: session.tenantId, partyId };
        } catch (e) { await this.backend.request('logout', {}, identity).catch(() => {}); throw e; }
    }
    require(authorization?: string): Session {
        const match = /^Bearer ([A-Za-z0-9_-]{43})$/.exec(authorization ?? '');
        const s = match ? this.entries.get(hash(match[1]!)) : undefined;
        if (!s || !s.active || s.expires <= Date.now()) throw new AppError('unauthorized', 'Sign in to the agent application.', 401);
        return s;
    }
    async logout(authorization?: string) {
        const s = this.require(authorization); s.active = false;
        this.entries.delete(hash(authorization!.slice(7)));
        await this.backend.request('logout', {}, s).catch(() => {});
    }
    async close() {
        const all = [...this.entries.values()]; this.entries.clear();
        for (const s of all) s.active = false;
        await Promise.allSettled(all.map(s => this.backend.request('logout', {}, s)));
    }
}

// SPDX-License-Identifier: GPL-3.0-or-later
import { randomBytes, createHash } from 'node:crypto';
import { z } from 'zod';
import { AppError, type Backend, type Identity } from './backend.js';

const loginInput = z
    .object({
        principal: z.string().min(1).max(200),
        password: z.string().min(1).max(1024),
        partyId: z.uuid().optional(),
    })
    .strict();
const party = z.object({ id: z.uuid(), name: z.string() });
const loginReply = z.object({
    success: z.boolean(),
    token: z.string().default(''),
    session_id: z.string().default(''),
    username: z.string().default(''),
    tenant_id: z.string().default(''),
    selected_party_id: z.string().default(''),
    available_parties: z.array(party).default([]),
    access_lifetime_s: z.number().positive().max(86400).default(1800),
    password_reset_required: z.boolean().default(false),
});
export interface Session extends Identity {
    username: string;
    tenantId: string;
    partyId: string;
    expires: number;
    active: boolean;
    busy: number;
    calls: number;
    window: number;
    chatBusy: boolean;
}
const hash = (s: string) => createHash('sha256').update(s).digest('hex');
export class Sessions {
    private entries = new Map<string, Session>();
    private loginAttempts = new Map<string, { count: number; expires: number }>();
    constructor(private backend: Backend) {}
    async login(raw: unknown): Promise<{
        accessToken: string;
        expiresAt: string;
        username: string;
        tenantId: string;
        partyId: string;
    }> {
        const now = Date.now();
        const input = loginInput.parse(raw);
        for (const [key, s] of this.entries)
            if (s.expires <= now) {
                s.active = false;
                this.entries.delete(key);
            }
        if (this.entries.size >= 64)
            throw new AppError('capacity', 'Session capacity reached.', 503);
        for (const [key, attempt] of this.loginAttempts)
            if (attempt.expires <= now) this.loginAttempts.delete(key);
        // Bound failed and in-flight attempts per principal. Never trust a forwarded client IP.
        const key = hash(input.principal);
        let attempt = this.loginAttempts.get(key);
        if (!attempt) {
            if (this.loginAttempts.size >= 1024)
                throw new AppError(
                    'capacity',
                    'Login capacity reached. Try again in a minute.',
                    503,
                );
            attempt = { count: 0, expires: now + 60000 };
            this.loginAttempts.set(key, attempt);
        }
        if (attempt.count >= 10)
            throw new AppError(
                'rate_limited',
                'Too many login attempts. Try again in a minute.',
                429,
            );
        attempt.count++;
        let failed = false;
        let parsed: z.infer<typeof loginReply>;
        try {
            const reply = loginReply.safeParse(
                await this.backend.request('login', {
                    principal: input.principal,
                    password: input.password,
                }),
            );
            if (!reply.success)
                throw new AppError(
                    'invalid_reply',
                    'Backend returned an invalid login reply.',
                    502,
                );
            parsed = reply.data;
            if (!parsed.success || !parsed.token || !parsed.session_id) {
                failed = true;
                throw new AppError('login_failed', 'Sign-in failed.', 401);
            }
        } catch (error) {
            if (error instanceof AppError && error.status === 401) failed = true;
            throw error;
        } finally {
            if (!failed) attempt.count--;
            if (attempt.count === 0 && this.loginAttempts.get(key) === attempt)
                this.loginAttempts.delete(key);
        }
        const r = parsed;
        let lifetime = r.access_lifetime_s;
        const identity = { token: r.token, sessionId: r.session_id };
        try {
            if (r.password_reset_required)
                throw new AppError(
                    'password_reset_required',
                    'Complete password reset in OreStudio first.',
                    403,
                );
            const partyId = input.partyId ?? r.selected_party_id;
            if (!partyId || partyId === '00000000-0000-0000-0000-000000000000') {
                throw new AppError(
                    'party_required',
                    'Select a party: ' +
                        r.available_parties.map((p) => `${p.name} (${p.id})`).join(', '),
                    409,
                );
            }
            if (
                input.partyId &&
                r.selected_party_id &&
                r.selected_party_id !== '00000000-0000-0000-0000-000000000000' &&
                r.selected_party_id !== input.partyId
            ) {
                throw new AppError(
                    'party_mismatch',
                    'Account already selected a different party; change its default party in OreStudio first.',
                    409,
                );
            }
            if (
                !r.selected_party_id ||
                r.selected_party_id === '00000000-0000-0000-0000-000000000000'
            ) {
                if (!r.available_parties.some((p) => p.id === partyId))
                    throw new AppError(
                        'party_denied',
                        'Party is not available to this account.',
                        403,
                    );
                const selected = z
                    .object({
                        success: z.boolean(),
                        token: z.string(),
                        access_lifetime_s: z.number().positive().max(86400).optional(),
                    })
                    .parse(
                        await this.backend.request('selectParty', { party_id: partyId }, identity),
                    );
                if (!selected.success || !selected.token)
                    throw new AppError('party_denied', 'Party selection failed.', 403);
                identity.token = selected.token;
                lifetime = Math.min(lifetime, selected.access_lifetime_s ?? lifetime);
            }
            const token = randomBytes(32).toString('base64url');
            const session: Session = {
                ...identity,
                username: r.username,
                tenantId: r.tenant_id,
                partyId,
                expires: Date.now() + Math.min(1800, lifetime) * 1000,
                active: true,
                busy: 0,
                calls: 0,
                window: now,
                chatBusy: false,
            };
            this.entries.set(hash(token), session);
            return {
                accessToken: token,
                expiresAt: new Date(session.expires).toISOString(),
                username: session.username,
                tenantId: session.tenantId,
                partyId,
            };
        } catch (e) {
            await this.backend.request('logout', {}, identity).catch(() => {});
            throw e;
        }
    }
    require(authorization?: string): Session {
        const match = /^Bearer ([A-Za-z0-9_-]{43})$/.exec(authorization ?? '');
        const s = match ? this.entries.get(hash(match[1]!)) : undefined;
        if (!s || !s.active || s.expires <= Date.now())
            throw new AppError('unauthorized', 'Sign in to the agent application.', 401);
        return s;
    }
    async logout(authorization?: string) {
        const s = this.require(authorization);
        s.active = false;
        this.entries.delete(hash(authorization!.slice(7)));
        await this.backend.request('logout', {}, s).catch(() => {});
    }
    async close() {
        const all = [...this.entries.values()];
        this.entries.clear();
        for (const s of all) s.active = false;
        await Promise.allSettled(all.map((s) => this.backend.request('logout', {}, s)));
    }
}

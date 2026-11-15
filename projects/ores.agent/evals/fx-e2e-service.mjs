// SPDX-License-Identifier: GPL-3.0-or-later
// CI-only protocol fixture, not a native OreStudio server. State exists only in this process.
import assert from 'node:assert/strict';
import { randomBytes } from 'node:crypto';
import { encode, decode } from '@msgpack/msgpack';
export const ciTenant = '11111111-1111-4111-8111-000000000001',
    ciParty = '11111111-1111-4111-8111-000000000002';
const systemTenant = '00000000-0000-4000-8000-000000000001',
    systemParty = '00000000-0000-4000-8000-000000000002';
export async function fixtureService(nc, prefix) {
    const store = new Map(),
        requests = [];
    let writes = 0;
    const identities = {
        system: {
            token: randomBytes(24).toString('hex'),
            session_id: 'system-session',
            tenant_id: systemTenant,
            selected_party_id: systemParty,
        },
        tenant: {
            token: randomBytes(24).toString('hex'),
            session_id: 'tenant-session',
            tenant_id: ciTenant,
            selected_party_id: ciParty,
        },
    };
    const key = (kind, k) => kind + '/' + (k.code ?? k.id);
    const subscription = nc.subscribe(prefix + '.>', {
        callback: (error, msg) => {
            if (error) return;
            try {
                const subject = msg.subject.slice(prefix.length + 1),
                    args = decode(msg.data);
                if (subject === 'iam.v1.ops.login') {
                    assert.equal(args.password, 'LABELLED CI fixture password');
                    assert.ok(Object.hasOwn(identities, args.principal));
                    const identity = identities[args.principal];
                    msg.respond(
                        encode({
                            success: true,
                            ...identity,
                            username: args.principal,
                            access_lifetime_s: 1800,
                            available_parties: [],
                        }),
                    );
                    return;
                }
                const actor = Object.entries(identities).find(
                    ([, i]) =>
                        msg.headers?.get('Authorization') === 'Bearer ' + i.token &&
                        msg.headers?.get('Nats-Session-Id') === i.session_id,
                );
                assert.ok(actor, 'Every data call must carry its own authenticated identity');
                if (subject === 'iam.v1.ops.logout') {
                    msg.respond(encode({}));
                    return;
                }
                requests.push({ subject, actor: actor[0] });
                const parts = subject.split('.'),
                    kind = parts[2],
                    operation = parts[3];
                assert.equal(parts[0], 'marketdata');
                assert.equal(parts[1], 'v1');
                assert.ok(['providers', 'market_series', 'market_observations'].includes(kind));
                const property = {
                    providers: 'provider',
                    market_series: 'market_series',
                    market_observations: 'market_observation',
                }[kind];
                if (kind === 'providers') assert.equal(actor[0], 'system');
                else assert.equal(actor[0], 'tenant');
                if (operation === 'get') {
                    const row = store.get(key(kind, args.key));
                    msg.respond(
                        encode(
                            row
                                ? { result: { outcome: 'ok' }, [property]: row }
                                : { result: { outcome: 'missing' } },
                        ),
                    );
                    return;
                }
                if (operation === 'put' || operation === 'put_many') {
                    assert.deepEqual(
                        Object.keys(args).sort(),
                        [operation === 'put' ? 'change' : 'changes', 'intent'].sort(),
                    );
                    assert.equal(args.intent.reason_code, 'system.new_record');
                    const changes = operation === 'put' ? [args.change] : args.changes;
                    for (const c of changes) {
                        assert.deepEqual(c.precondition, { kind: 'must_not_exist', version: null });
                        assert.ok(
                            !store.has(key(kind, c.write)),
                            'Create-only writes must never overwrite',
                        );
                        if (kind === 'providers') assert.equal(c.write.code, 'agent-e2e');
                        if (kind === 'market_series') {
                            assert.equal(c.write.party_id, ciParty);
                            assert.equal(c.write.provider_code, 'agent-e2e');
                        }
                        if (kind === 'market_observations') {
                            assert.equal(c.write.party_id, ciParty);
                            assert.match(c.write.source, /^synthetic\.agent-e2e\./);
                            assert.ok(store.has('market_series/' + c.write.series_id));
                        }
                    }
                    for (const c of changes)
                        store.set(key(kind, c.write), {
                            ...c.write,
                            tenant_id: actor[1].tenant_id,
                        });
                    writes += changes.length;
                    msg.respond(encode({ result: { outcome: 'ok' } }));
                    return;
                }
                const rows = [...store.entries()]
                    .filter(([k]) => k.startsWith(kind + '/'))
                    .map(([, v]) => v);
                if (operation === 'list' && kind === 'market_series') {
                    msg.respond(
                        encode({
                            result: { outcome: 'ok' },
                            total: rows.length,
                            market_series: rows.slice(args.offset, args.offset + args.limit),
                        }),
                    );
                    return;
                }
                if (operation === 'list_by_series_id' && kind === 'market_observations') {
                    assert.equal(args.scope, 'direct');
                    const selected = rows.filter((r) => r.series_id === args.series_id);
                    msg.respond(
                        encode({
                            result: { outcome: 'ok' },
                            total: selected.length,
                            market_observations: selected.slice(
                                args.offset,
                                args.offset + args.limit,
                            ),
                        }),
                    );
                    return;
                }
                throw Error('Unexpected fixture operation');
            } catch (error) {
                msg.respond(
                    encode({
                        result: {
                            outcome: 'failed',
                            code: 'fixture_protocol_check',
                            message: error.message,
                        },
                    }),
                );
            }
        },
    });
    await nc.flush();
    return {
        store,
        requests,
        get writes() {
            return writes;
        },
        close: () => subscription.unsubscribe(),
    };
}

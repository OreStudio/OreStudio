import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import type { OresClient, RecordKey } from '@ores/wire-protocol';
import {
    BATCH_WINDOW_MS,
    MAX_KEYS,
    ChangeEventRegistry,
    eventSubject,
    isWatchable,
    mayHear,
    type Audience,
} from './change-events.js';

type Published = (change: {
    at: string;
    tenantId: string | undefined;
    partyId: string | undefined;
    key: RecordKey;
}) => void;

const SYSTEM = 'ffffffff-ffff-ffff-ffff-ffffffffffff';

/** A client that records the subjects listened on and lets a test publish to them. */
function fakeClient(): {
    readonly client: OresClient;
    readonly open: Map<string, Published>;
} {
    const open = new Map<string, Published>();
    const client = {
        subscribeToEvents(subject: string, onEvent: Published) {
            open.set(subject, onEvent);
            return () => {
                open.delete(subject);
            };
        },
    } as unknown as OresClient;
    return { client, open };
}

function audience(over: Partial<Audience> = {}): Audience {
    return { tenantId: 't1', everyParty: true, parties: () => new Set(), ...over };
}

function change(
    tenantId: string | undefined,
    partyId?: string,
    at = '2026-10-10T12:00:03Z',
    key: RecordKey = {},
): Parameters<Published>[0] {
    return { at, tenantId, partyId, key };
}

/** Lets the window that gathers a burst of changes end. */
function endWindow(): void {
    vi.advanceTimersByTime(BATCH_WINDOW_MS);
}

describe('the subject a watch listens on', () => {
    it('is the canonical events collection with every action under it', () => {
        expect(eventSubject('iam', 'accounts')).toBe('iam.v1.accounts_events.>');
    });

    it('accepts plain names and refuses a wildcard or an extra segment', () => {
        expect(isWatchable({ component: 'iam', entity: 'tenant_types' })).toBe(true);
        expect(isWatchable({ component: 'iam', entity: '>' })).toBe(false);
        expect(isWatchable({ component: '*', entity: 'accounts' })).toBe(false);
        expect(isWatchable({ component: 'iam', entity: 'accounts.updated' })).toBe(false);
    });
});

describe('who may hear an event', () => {
    it('hears its own tenant and the system tenant', () => {
        expect(mayHear(audience(), { tenantId: 't1', partyId: undefined })).toBe(true);
        expect(mayHear(audience(), { tenantId: SYSTEM, partyId: undefined })).toBe(true);
    });

    it('does not hear another tenant', () => {
        expect(mayHear(audience(), { tenantId: 't2', partyId: undefined })).toBe(false);
    });

    it('hears nothing from an event that names no tenant', () => {
        expect(mayHear(audience(), { tenantId: undefined, partyId: undefined })).toBe(false);
    });

    it('hears the party of a party-owned event only if it works in that party', () => {
        const member = audience({ everyParty: false, parties: () => new Set(['p1']) });
        expect(mayHear(member, { tenantId: 't1', partyId: 'p1' })).toBe(true);
        expect(mayHear(member, { tenantId: 't1', partyId: 'p2' })).toBe(false);
    });

    it('hears an event with no party as a whole-tenant event, even as a member', () => {
        const member = audience({ everyParty: false, parties: () => new Set() });
        expect(mayHear(member, { tenantId: 't1', partyId: undefined })).toBe(true);
    });

    it('lets an administrator hear every party of its tenant', () => {
        expect(mayHear(audience(), { tenantId: 't1', partyId: 'anything' })).toBe(true);
    });
});

describe('the change registry', () => {
    beforeEach(() => {
        vi.useFakeTimers();
    });
    afterEach(() => {
        vi.useRealTimers();
    });

    it('hands a published change to the sessions watching that entity', () => {
        const { client, open } = fakeClient();
        const registry = new ChangeEventRegistry(client);
        const heard: string[] = [];
        registry.attach('s1', (c) => heard.push(`${c.entity}@${c.at}`));
        registry.watch('s1', audience(), [{ component: 'iam', entity: 'accounts' }]);
        open.get('iam.v1.accounts_events.>')?.(change('t1'));
        endWindow();
        expect(heard).toEqual(['accounts@2026-10-10T12:00:03Z']);
    });

    it('logs each event it receives, with whom it was passed to and whom it was refused', () => {
        const { client, open } = fakeClient();
        const lines: { fields: Record<string, unknown>; message: string }[] = [];
        const registry = new ChangeEventRegistry(client, {
            debug: (fields, message) => lines.push({ fields, message }),
            warn: (fields, message) => lines.push({ fields, message }),
        });
        registry.attach('s1', () => undefined);
        registry.attach('s2', () => undefined);
        const watch = [{ component: 'iam', entity: 'accounts' }];
        registry.watch('s1', audience({ tenantId: 't1' }), watch);
        registry.watch('s2', audience({ tenantId: 't2' }), watch);
        open.get('iam.v1.accounts_events.>')?.(change('t1', undefined, 'first'));
        endWindow();

        const received = lines.find((line) => line.message === 'change event received');
        expect(received?.fields).toMatchObject({
            component: 'iam',
            entity: 'accounts',
            tenant_id: 't1',
            occurred_at: 'first',
            passed: 1,
            refused: 1,
        });
        expect(lines.map((line) => line.message)).toContain('change subscription opened');
        expect(lines.map((line) => line.message)).toContain('session watch declared');
    });

    it('warns when an event names no tenant, and tells nobody', () => {
        const { client, open } = fakeClient();
        const warnings: string[] = [];
        const registry = new ChangeEventRegistry(client, {
            debug: () => undefined,
            warn: (_fields, message) => warnings.push(message),
        });
        const heard: string[] = [];
        registry.attach('s1', (c) => heard.push(c.at));
        registry.watch('s1', audience(), [{ component: 'iam', entity: 'accounts' }]);
        open.get('iam.v1.accounts_events.>')?.(change(undefined));
        endWindow();
        expect(warnings).toEqual(['change event without a tenant, told to nobody']);
        expect(heard).toEqual([]);
    });

    describe('naming the records that changed', () => {
        function setup(): {
            readonly told: { at: string; keys?: readonly RecordKey[] }[];
            readonly publish: (key: RecordKey, at?: string, tenant?: string) => void;
        } {
            const { client, open } = fakeClient();
            const registry = new ChangeEventRegistry(client);
            const told: { at: string; keys?: readonly RecordKey[] }[] = [];
            registry.attach('s1', (c) =>
                told.push({ at: c.at, ...(c.keys === undefined ? {} : { keys: c.keys }) }),
            );
            registry.watch('s1', audience(), [{ component: 'iam', entity: 'accounts' }]);
            return {
                told,
                publish: (key, at = 'a', tenant = 't1') =>
                    open.get('iam.v1.accounts_events.>')?.(change(tenant, undefined, at, key)),
            };
        }

        it('names the record of one change', () => {
            const { told, publish } = setup();
            publish({ username: 'diane.kay' }, 'a');
            endWindow();
            expect(told).toEqual([{ at: 'a', keys: [{ username: 'diane.kay' }] }]);
        });

        it('joins the changes of one window into one message with the newest time', () => {
            const { told, publish } = setup();
            publish({ username: 'diane.kay' }, 'first');
            publish({ username: 'hazel.jennings' }, 'second');
            publish({ username: 'diane.kay' }, 'third');
            endWindow();
            expect(told).toEqual([
                {
                    at: 'third',
                    keys: [{ username: 'diane.kay' }, { username: 'hazel.jennings' }],
                },
            ]);
        });

        it('tells a session again when a later change comes after the window', () => {
            const { told, publish } = setup();
            publish({ username: 'a' }, 'first');
            endWindow();
            publish({ username: 'b' }, 'second');
            endWindow();
            expect(told.map((entry) => entry.at)).toEqual(['first', 'second']);
        });

        it('names no record once a change could not name its own', () => {
            const { told, publish } = setup();
            publish({ username: 'diane.kay' }, 'first');
            publish({}, 'second');
            endWindow();
            expect(told).toEqual([{ at: 'second' }]);
        });

        it('names no record once a bulk change passes the limit', () => {
            const { told, publish } = setup();
            for (let i = 0; i <= MAX_KEYS; i += 1) publish({ username: `u${i}` }, `t${i}`);
            endWindow();
            expect(told).toHaveLength(1);
            expect(told[0]?.keys).toBeUndefined();
        });

        it('still names the records at the limit', () => {
            const { told, publish } = setup();
            for (let i = 0; i < MAX_KEYS; i += 1) publish({ username: `u${i}` });
            endWindow();
            expect(told[0]?.keys).toHaveLength(MAX_KEYS);
        });

        it('does not name the records of another tenant to the session', () => {
            const { told, publish } = setup();
            publish({ username: 'stranger' }, 'x', 't2');
            publish({ username: 'diane.kay' }, 'y');
            endWindow();
            expect(told).toEqual([{ at: 'y', keys: [{ username: 'diane.kay' }] }]);
        });

        it('tells nobody once the last watcher has left', () => {
            const { client, open } = fakeClient();
            const registry = new ChangeEventRegistry(client);
            const heard: string[] = [];
            registry.attach('s1', (c) => heard.push(c.at));
            registry.watch('s1', audience(), [{ component: 'iam', entity: 'accounts' }]);
            open.get('iam.v1.accounts_events.>')?.(change('t1', undefined, 'late', { a: '1' }));
            registry.forget('s1');
            endWindow();
            expect(heard).toEqual([]);
        });
    });

    it('logs a watch it ignores because no stream is attached yet', () => {
        const { client } = fakeClient();
        const lines: string[] = [];
        const registry = new ChangeEventRegistry(client, {
            debug: (_fields, message) => lines.push(message),
            warn: (_fields, message) => lines.push(message),
        });
        registry.watch('s1', audience(), [{ component: 'iam', entity: 'accounts' }]);
        expect(lines).toEqual(['session watch ignored, no stream attached']);
    });

    it('tells a session nothing of another tenant, though the broker delivered it', () => {
        const { client, open } = fakeClient();
        const registry = new ChangeEventRegistry(client);
        const one: string[] = [];
        const two: string[] = [];
        registry.attach('s1', (c) => one.push(c.at));
        registry.attach('s2', (c) => two.push(c.at));
        const watch = [{ component: 'iam', entity: 'accounts' }];
        registry.watch('s1', audience({ tenantId: 't1' }), watch);
        registry.watch('s2', audience({ tenantId: 't2' }), watch);
        open.get('iam.v1.accounts_events.>')?.(change('t1', undefined, 'first'));
        endWindow();
        open.get('iam.v1.accounts_events.>')?.(change('t2', undefined, 'second'));
        endWindow();
        open.get('iam.v1.accounts_events.>')?.(change(SYSTEM, undefined, 'shared'));
        endWindow();
        expect(one).toEqual(['first', 'shared']);
        expect(two).toEqual(['second', 'shared']);
    });

    it('reads the parties of a member when the event arrives, so a switch counts', () => {
        const { client, open } = fakeClient();
        const registry = new ChangeEventRegistry(client);
        const heard: string[] = [];
        let parties = new Set(['p1']);
        registry.attach('s1', (c) => heard.push(c.at));
        registry.watch('s1', audience({ everyParty: false, parties: () => parties }), [
            { component: 'refdata', entity: 'party_identifiers' },
        ]);
        const publish = open.get('refdata.v1.party_identifiers_events.>');
        publish?.(change('t1', 'p2', 'before'));
        endWindow();
        parties = new Set(['p2']);
        publish?.(change('t1', 'p2', 'after'));
        endWindow();
        expect(heard).toEqual(['after']);
    });

    it('shares one subscription between sessions and closes it with the last', () => {
        const { client, open } = fakeClient();
        const registry = new ChangeEventRegistry(client);
        registry.attach('s1', () => undefined);
        registry.attach('s2', () => undefined);
        const watch = [{ component: 'iam', entity: 'roles' }];
        registry.watch('s1', audience(), watch);
        registry.watch('s2', audience({ tenantId: 't2' }), watch);
        expect(registry.size).toBe(1);
        registry.forget('s1');
        expect(open.size).toBe(1);
        registry.forget('s2');
        expect(open.size).toBe(0);
        expect(registry.size).toBe(0);
    });

    it('never listens on a name that is a wildcard', () => {
        const { client, open } = fakeClient();
        const registry = new ChangeEventRegistry(client);
        registry.attach('s1', () => undefined);
        registry.watch('s1', audience(), [{ component: 'iam', entity: '>' }]);
        expect(open.size).toBe(0);
    });

    it('lets a screen that watches something else drop the old subscription', () => {
        const { client, open } = fakeClient();
        const registry = new ChangeEventRegistry(client);
        registry.attach('s1', () => undefined);
        registry.watch('s1', audience(), [{ component: 'iam', entity: 'accounts' }]);
        registry.watch('s1', audience(), [{ component: 'iam', entity: 'roles' }]);
        expect([...open.keys()]).toEqual(['iam.v1.roles_events.>']);
    });
});

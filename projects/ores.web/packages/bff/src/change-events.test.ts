import { describe, expect, it } from 'vitest';
import type { OresClient } from '@ores/wire-protocol';
import {
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
): Parameters<Published>[0] {
    return { at, tenantId, partyId };
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
    it('hands a published change to the sessions watching that entity', () => {
        const { client, open } = fakeClient();
        const registry = new ChangeEventRegistry(client);
        const heard: string[] = [];
        registry.attach('s1', (c) => heard.push(`${c.entity}@${c.at}`));
        registry.watch('s1', audience(), [{ component: 'iam', entity: 'accounts' }]);
        open.get('iam.v1.accounts_events.>')?.(change('t1'));
        expect(heard).toEqual(['accounts@2026-10-10T12:00:03Z']);
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
        open.get('iam.v1.accounts_events.>')?.(change('t2', undefined, 'second'));
        open.get('iam.v1.accounts_events.>')?.(change(SYSTEM, undefined, 'shared'));
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
        parties = new Set(['p2']);
        publish?.(change('t1', 'p2', 'after'));
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

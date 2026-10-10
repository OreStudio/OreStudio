import { describe, expect, it } from 'vitest';
import type { OresClient } from '@ores/wire-protocol';
import { ChangeEventRegistry, eventSubject, isWatchable } from './change-events.js';

/** A client that records the subjects listened on and lets a test publish to them. */
function fakeClient(): {
    readonly client: OresClient;
    readonly open: Map<string, (change: { at: string }) => void>;
} {
    const open = new Map<string, (change: { at: string }) => void>();
    const client = {
        subscribeToEvents(subject: string, onEvent: (change: { at: string }) => void) {
            open.set(subject, onEvent);
            return () => {
                open.delete(subject);
            };
        },
    } as unknown as OresClient;
    return { client, open };
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

describe('the change registry', () => {
    it('hands a published change to the sessions watching that entity', () => {
        const { client, open } = fakeClient();
        const registry = new ChangeEventRegistry(client);
        const heard: string[] = [];
        registry.attach('s1', (change) => heard.push(`${change.entity}@${change.at}`));
        registry.watch('s1', 't1', [{ component: 'iam', entity: 'accounts' }]);
        open.get('iam.v1.accounts_events.>')?.({ at: '2026-10-10T12:00:03Z' });
        expect(heard).toEqual(['accounts@2026-10-10T12:00:03Z']);
    });

    it('shares one subscription between sessions and closes it with the last', () => {
        const { client, open } = fakeClient();
        const registry = new ChangeEventRegistry(client);
        registry.attach('s1', () => undefined);
        registry.attach('s2', () => undefined);
        const watch = [{ component: 'iam', entity: 'roles' }];
        registry.watch('s1', 't1', watch);
        registry.watch('s2', 't1', watch);
        expect(registry.size).toBe(1);
        registry.forget('s1');
        expect(open.size).toBe(1);
        registry.forget('s2');
        expect(open.size).toBe(0);
        expect(registry.size).toBe(0);
    });

    it('keeps tenants apart in the registry', () => {
        const { client } = fakeClient();
        const registry = new ChangeEventRegistry(client);
        registry.attach('s1', () => undefined);
        registry.attach('s2', () => undefined);
        registry.watch('s1', 't1', [{ component: 'iam', entity: 'roles' }]);
        registry.watch('s2', 't2', [{ component: 'iam', entity: 'roles' }]);
        expect(registry.size).toBe(2);
    });

    it('never listens on a name that is a wildcard', () => {
        const { client, open } = fakeClient();
        const registry = new ChangeEventRegistry(client);
        registry.attach('s1', () => undefined);
        registry.watch('s1', 't1', [{ component: 'iam', entity: '>' }]);
        expect(open.size).toBe(0);
    });

    it('lets a screen that watches something else drop the old subscription', () => {
        const { client, open } = fakeClient();
        const registry = new ChangeEventRegistry(client);
        registry.attach('s1', () => undefined);
        registry.watch('s1', 't1', [{ component: 'iam', entity: 'accounts' }]);
        registry.watch('s1', 't1', [{ component: 'iam', entity: 'roles' }]);
        expect([...open.keys()]).toEqual(['iam.v1.roles_events.>']);
    });
});

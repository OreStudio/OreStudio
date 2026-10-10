import { describe, expect, it } from 'vitest';
import { EntityEventStream, type EventSourceLike } from './EntityEventStream.js';

type Listener = (event: { readonly data?: string }) => void;

function fakeSource(): {
    readonly source: EventSourceLike;
    readonly emit: (type: string, data?: unknown) => void;
    readonly closed: () => boolean;
    readonly opened: () => number;
    readonly factory: () => EventSourceLike;
} {
    const listeners = new Map<string, Listener[]>();
    let closed = false;
    let opened = 0;
    const source: EventSourceLike = {
        addEventListener(type, listener) {
            listeners.set(type, [...(listeners.get(type) ?? []), listener]);
        },
        close() {
            closed = true;
        },
    };
    return {
        source,
        emit(type, data) {
            for (const listener of listeners.get(type) ?? []) {
                listener(data === undefined ? {} : { data: JSON.stringify(data) });
            }
        },
        closed: () => closed,
        opened: () => opened,
        factory: () => {
            opened += 1;
            return source;
        },
    };
}

const flush = async (): Promise<void> => {
    await Promise.resolve();
    await Promise.resolve();
};

describe('the entity event stream', () => {
    it('opens no stream until a screen watches something', () => {
        const fake = fakeSource();
        const stream = new EntityEventStream(fake.factory, async () => undefined);
        expect(stream.isOpen).toBe(false);
        expect(fake.opened()).toBe(0);
    });

    it('sends what is watched once the stream says it is connected', async () => {
        const fake = fakeSource();
        const sent: unknown[] = [];
        const stream = new EntityEventStream(fake.factory, async (watches) => {
            sent.push(watches);
        });
        stream.watch('iam', 'accounts', () => undefined);
        await flush();
        expect(sent).toEqual([]);
        fake.emit('connected', { at: 'now' });
        await flush();
        expect(sent).toEqual([[{ component: 'iam', entity: 'accounts' }]]);
    });

    it('sends the whole set again after a reconnect, once for a burst of watches', async () => {
        const fake = fakeSource();
        const sent: unknown[][] = [];
        const stream = new EntityEventStream(fake.factory, async (watches) => {
            sent.push([...watches]);
        });
        stream.watch('iam', 'accounts', () => undefined);
        fake.emit('connected');
        await flush();
        stream.watch('iam', 'roles', () => undefined);
        stream.watch('iam', 'sessions', () => undefined);
        await flush();
        expect(sent).toHaveLength(2);
        expect(sent[1]).toHaveLength(3);
        fake.emit('connected');
        await flush();
        expect(sent).toHaveLength(3);
        expect(sent[2]).toHaveLength(3);
    });

    it('hands a change to the screens watching that entity and to no other', () => {
        const fake = fakeSource();
        const stream = new EntityEventStream(fake.factory, async () => undefined);
        const accounts: string[] = [];
        const roles: string[] = [];
        stream.watch('iam', 'accounts', (change) => accounts.push(change.at));
        stream.watch('iam', 'roles', (change) => roles.push(change.at));
        fake.emit('entity-changed', { component: 'iam', entity: 'accounts', at: 't1' });
        expect(accounts).toEqual(['t1']);
        expect(roles).toEqual([]);
    });

    it('ignores news it cannot read', () => {
        const fake = fakeSource();
        const stream = new EntityEventStream(fake.factory, async () => undefined);
        const heard: string[] = [];
        stream.watch('iam', 'accounts', (change) => heard.push(change.at));
        fake.emit('entity-changed', { component: 'iam' });
        expect(heard).toEqual([]);
    });

    it('closes the stream when the last screen stops watching', () => {
        const fake = fakeSource();
        const stream = new EntityEventStream(fake.factory, async () => undefined);
        const stopA = stream.watch('iam', 'accounts', () => undefined);
        const stopB = stream.watch('iam', 'roles', () => undefined);
        stopA();
        expect(fake.closed()).toBe(false);
        stopB();
        expect(fake.closed()).toBe(true);
        expect(stream.isOpen).toBe(false);
    });

    it('opens one stream for many screens', () => {
        const fake = fakeSource();
        const stream = new EntityEventStream(fake.factory, async () => undefined);
        stream.watch('iam', 'accounts', () => undefined);
        stream.watch('iam', 'accounts', () => undefined);
        stream.watch('iam', 'roles', () => undefined);
        expect(fake.opened()).toBe(1);
    });
});

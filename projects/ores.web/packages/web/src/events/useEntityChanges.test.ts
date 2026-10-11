import { describe, expect, it } from 'vitest';
import { isStale, remember, type HeardEvent } from './useEntityChanges.js';

describe('when a screen is stale', () => {
    it('is not stale when nothing has changed', () => {
        expect(isStale(undefined, 1000)).toBe(false);
    });

    it('is stale when a change arrived after the load began', () => {
        expect(isStale(2000, 1000)).toBe(true);
    });

    it('is not stale when the change came before the load began', () => {
        expect(isStale(900, 1000)).toBe(false);
        expect(isStale(1000, 1000)).toBe(false);
    });

    it('is stale again for a change that arrives during a load, and clears with the next one', () => {
        // The load began at 1000 and a change arrived at 1500, while it was in flight.
        expect(isStale(1500, 1000)).toBe(true);
        // The next load began at 2000, after that change, so it is read.
        expect(isStale(1500, 2000)).toBe(false);
    });
});

describe('remember', () => {
    const event = (arrivedAt: number, username: string): HeardEvent => ({
        arrivedAt,
        at: '',
        keys: [{ username }],
    });

    it('drops the changes from before the load', () => {
        const kept = remember([event(100, 'old'), event(300, 'new')], event(400, 'newer'), 200);
        expect(kept.map((heard) => heard.keys?.[0]?.username)).toEqual(['new', 'newer']);
    });

    it('does not count the changes from before the load toward the limit', () => {
        const old = Array.from({ length: 5 }, (_, i) => event(i, `old${i}`));
        const kept = remember(old, event(900, 'fresh'), 500, 3);
        expect(kept).toEqual([event(900, 'fresh')]);
    });

    it('lets one change stand for the rest past the limit, without naming a record', () => {
        const since = [event(300, 'a'), event(301, 'b')];
        const kept = remember(since, event(302, 'c'), 200, 2);
        expect(kept).toEqual([{ arrivedAt: 302, at: '', keys: undefined }]);
    });
});

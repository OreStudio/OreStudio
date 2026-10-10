import { describe, expect, it } from 'vitest';
import { isStale } from './useEntityChanges.js';

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

import { describe, expect, it } from 'vitest';
import { changedKeys } from './changedRows.js';

const page = (entries: readonly [string, string][]): ReadonlyMap<string, string> =>
    new Map(entries);

describe('the rows that changed between two reads of a page', () => {
    it('marks nothing on the first read', () => {
        expect(changedKeys(undefined, page([['a', '1']]))).toEqual(new Set());
    });

    it('marks a row whose content changed', () => {
        const before = page([
            ['a', '1'],
            ['b', '1'],
        ]);
        const after = page([
            ['a', '1'],
            ['b', '2'],
        ]);
        expect(changedKeys(before, after)).toEqual(new Set(['b']));
    });

    it('marks a row that was not there', () => {
        expect(
            changedKeys(
                page([['a', '1']]),
                page([
                    ['a', '1'],
                    ['c', '1'],
                ]),
            ),
        ).toEqual(new Set(['c']));
    });

    it('marks nothing when the page is the same', () => {
        const same = page([['a', '1']]);
        expect(changedKeys(same, same)).toEqual(new Set());
    });

    it('does not mark a row that left the page', () => {
        expect(
            changedKeys(
                page([
                    ['a', '1'],
                    ['b', '1'],
                ]),
                page([['a', '1']]),
            ),
        ).toEqual(new Set());
    });
});

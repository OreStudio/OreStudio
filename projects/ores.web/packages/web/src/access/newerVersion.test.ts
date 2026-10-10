import { describe, expect, it } from 'vitest';
import { fieldChanges } from './newerVersion.js';

interface Card {
    readonly name: string;
    readonly title: string;
}

const FIELDS = {
    Name: (card: Card) => card.name,
    Title: (card: Card) => card.title,
};

describe('the fields that moved in a newer version', () => {
    it('names only the fields that differ, with both values', () => {
        const before = { name: 'Ada', title: 'Analyst' };
        const after = { name: 'Ada', title: 'Head of Desk' };
        expect(fieldChanges(before, after, FIELDS)).toEqual([
            { field: 'Title', before: 'Analyst', after: 'Head of Desk' },
        ]);
    });

    it('reports nothing when only the version moved', () => {
        const same = { name: 'Ada', title: 'Analyst' };
        expect(fieldChanges(same, { ...same }, FIELDS)).toEqual([]);
    });

    it('counts a cleared field as a change', () => {
        const changes = fieldChanges(
            { name: 'Ada', title: 'Analyst' },
            { name: 'Ada', title: '' },
            FIELDS,
        );
        expect(changes).toEqual([{ field: 'Title', before: 'Analyst', after: '' }]);
    });
});

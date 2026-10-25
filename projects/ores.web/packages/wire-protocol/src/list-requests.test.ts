import { describe, expect, it } from 'vitest';
import { completeListRequest } from './list-requests.js';

/**
 * A list request must state every member the service decodes.
 *
 * These cases use a real subject, because the point of the completer is that it
 * knows what a real request declares. A subject the table does not describe is
 * left alone, and that is checked too: guessing at an unknown request would be
 * worse than sending what the caller wrote.
 */
const BOOK_IDENTIFIERS = 'refdata.v1.counterparty_identifiers.list';

describe('completing a list request', () => {
    it('states as_of for a request that declares it', () => {
        const payload = completeListRequest(BOOK_IDENTIFIERS, { offset: 0, limit: 10 });

        expect(payload['as_of']).toBeNull();
    });

    it('states a filter as null when the caller stated none', () => {
        const payload = completeListRequest(BOOK_IDENTIFIERS, { offset: 0, limit: 10 });

        expect(payload['filter']).toBeNull();
    });

    it('fills every member of a filter the caller stated in part', () => {
        const payload = completeListRequest(BOOK_IDENTIFIERS, {
            filter: { counterparty_id_one_of: ['a'] },
        });

        expect(payload['filter']).toEqual({
            counterparty_id: null,
            id_one_of: null,
            counterparty_id_one_of: ['a'],
        });
    });

    it('leaves a caller value alone', () => {
        const payload = completeListRequest(BOOK_IDENTIFIERS, {
            offset: 7,
            filter: { counterparty_id: 'a' },
            as_of: '2026-01-01',
        });

        expect(payload['offset']).toBe(7);
        expect(payload['as_of']).toBe('2026-01-01');
        expect(payload['filter']).toMatchObject({ counterparty_id: 'a' });
    });

    it('leaves a request the table does not describe as it was written', () => {
        const payload = completeListRequest('nowhere.v1.nothing.list', { offset: 0 });

        expect(payload).toEqual({ offset: 0 });
    });
});

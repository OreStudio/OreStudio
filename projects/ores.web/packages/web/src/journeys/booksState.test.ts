/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { describe, expect, it } from 'vitest';
import type { Book } from '@ores/wire-protocol/generated/refdata/domain/book';
import type { Portfolio } from '@ores/wire-protocol/generated/refdata/domain/portfolio';
import {
    ancestryUnits,
    blankDraft,
    bookProblems,
    changesOf,
    reduceBook,
    writePlan,
    type BookAction,
    type BookDraft,
} from './booksState.js';

const audit = {
    tenant_id: 't',
    modified_by: 'm',
    performed_by: 'p',
    change_reason_code: 'r',
    change_commentary: '',
    recorded_at: '2026-01-01T00:00:00Z',
};

const portfolio = (id: string, parent: string | null, owner: string | null): Portfolio => ({
    ...audit,
    version: 2,
    id,
    party_id: 'party-1',
    name: id,
    description: '',
    parent_portfolio_id: parent,
    owner_unit_id: owner,
    purpose_type: 'Trading',
    aggregation_ccy: 'GBP',
    is_virtual: false,
    sandbox_id: null,
    status: 'Active',
});

const book: Book = {
    ...audit,
    version: 4,
    id: 'book-1',
    party_id: 'party-1',
    name: 'RATES-1',
    description: 'Rates desk',
    parent_portfolio_id: 'rates',
    owner_unit_id: 'unit-desk',
    functional_currency: 'GBP',
    gl_account_ref: 'GL1',
    cost_center: 'CC1',
    book_status: 'Active',
    regulatory_book_type: 'Trading',
    book_purpose_type: 'Hedging',
    ledger_feed_type: 'Daily',
    is_sweepable: false,
    rates_centre_code: 'GBLO',
    sandbox_id: null,
};

function walk(...actions: readonly BookAction[]): BookDraft {
    return actions.reduce(reduceBook, reduceBook(blankDraft(), { kind: 'open-book', book }));
}

describe('book structure draft', () => {
    it('has no change until the person changes something', () => {
        expect(changesOf(walk())).toEqual([]);
    });

    it('records a changed classification with its before and after', () => {
        const changes = changesOf(
            walk({ kind: 'set-field', field: 'bookStatus', value: 'Closed' }),
        );
        expect(changes).toEqual([
            expect.objectContaining({
                what: 'Status',
                before: 'Active',
                after: 'Closed',
                operation: 'refdata.v1.books.put',
            }),
        ]);
    });

    it('writes a changed book against the version it read and keeps its name', () => {
        const plan = writePlan(
            walk({ kind: 'set-field', field: 'description', value: 'Rates and credit' }),
            'party-1',
        );
        expect(plan?.version).toBe(4);
        expect(plan?.book.name).toBe('RATES-1');
        expect(plan?.book.description).toBe('Rates and credit');
        expect(plan?.portfolio).toBeUndefined();
    });

    it('writes a new book as new, in the portfolio created in the same walk', () => {
        const draft = [
            { kind: 'start-portfolio', parentPortfolioId: '' },
            { kind: 'set-portfolio-field', field: 'name', value: 'Credit' },
            { kind: 'start-book' },
            { kind: 'set-field', field: 'name', value: 'CREDIT-1' },
        ].reduce(reduceBook, blankDraft());
        const plan = writePlan(draft, 'party-1');
        expect(plan?.version).toBeNull();
        expect(plan?.portfolio?.name).toBe('Credit');
        expect(plan?.book.parent_portfolio_id).toBe(plan?.portfolio?.id);
        expect(changesOf(draft).map((change) => change.operation)).toContain(
            'refdata.v1.portfolios.put',
        );
    });

    it('has no plan until a book is being shaped', () => {
        expect(writePlan(blankDraft(), 'party-1')).toBeUndefined();
    });

    it('names what a new book still lacks', () => {
        const draft = reduceBook(blankDraft(), { kind: 'start-book' });
        expect(bookProblems(draft).length).toBeGreaterThan(2);
        expect(bookProblems(walk())).toEqual([]);
    });

    it('never lets a reverted version rename the book', () => {
        const older: Book = { ...book, name: 'OLD-NAME', cost_center: 'CC0' };
        const draft = walk({ kind: 'revert-fields', book: older });
        expect(draft.fields.name).toBe('RATES-1');
        expect(draft.fields.costCenter).toBe('CC0');
    });
});

describe('the owner unit ancestry', () => {
    const portfolios = [
        portfolio('root', null, 'unit-root'),
        portfolio('rates', 'root', 'unit-desk'),
        portfolio('loop-a', 'loop-b', 'unit-a'),
        portfolio('loop-b', 'loop-a', 'unit-b'),
    ];

    it('collects the owner of the portfolio and of every ancestor', () => {
        expect([...ancestryUnits('rates', portfolios)].sort()).toEqual(['unit-desk', 'unit-root']);
    });

    it('stops at a loop rather than walking it forever', () => {
        expect([...ancestryUnits('loop-a', portfolios)].sort()).toEqual(['unit-a', 'unit-b']);
    });

    it('counts the owner of a portfolio not yet written', () => {
        const pending = {
            id: 'new',
            name: 'New',
            description: '',
            parentPortfolioId: 'root',
            ownerUnitId: 'unit-new',
            purposeType: '',
            aggregationCcy: '',
            isVirtual: false,
        };
        expect([...ancestryUnits('', portfolios, pending)].sort()).toEqual([
            'unit-new',
            'unit-root',
        ]);
    });
});

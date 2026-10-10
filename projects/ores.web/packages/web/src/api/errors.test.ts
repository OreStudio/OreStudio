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

import { beforeEach, describe, expect, it } from 'vitest';
import { current, dismiss, dismissAll, report, reportError, reportQueryError } from './errors.js';
import { ApiFailure } from './transport.js';

/**
 * The store's own rules, which are pure and therefore asserted here rather
 * than through a rendered screen: what is deduped, what the cap drops, and
 * what a dismissal does to the next failure.
 */
describe('the error store', () => {
    beforeEach(() => {
        dismissAll();
    });

    it('holds the most recent report first', () => {
        report('the first screen failed');
        report('the second screen failed');

        expect(current().map((entry) => entry.message)).toEqual([
            'the second screen failed',
            'the first screen failed',
        ]);
    });

    it('does not report a message that is already on screen', () => {
        report('the tenant list did not load');
        report('the tenant list did not load');

        expect(current()).toHaveLength(1);
    });

    it('drops the oldest report once the cap is reached', () => {
        report('one');
        report('two');
        report('three');
        report('four');

        expect(current().map((entry) => entry.message)).toEqual(['four', 'three', 'two']);
    });

    it('takes one report away by the id it was given', () => {
        report('the first screen failed');
        report('the second screen failed');
        const [newest, oldest] = current();

        dismiss(oldest!.id);
        expect(current().map((entry) => entry.message)).toEqual(['the second screen failed']);

        // An id nobody holds is not a report, so nothing changes.
        dismiss(newest!.id + 1000);
        expect(current()).toHaveLength(1);
    });

    it('brings a message back when it fails again after it was dismissed', () => {
        report('the save did not finish');
        const [only] = current();
        dismiss(only!.id);
        expect(current()).toHaveLength(0);

        report('the save did not finish');
        expect(current().map((entry) => entry.message)).toEqual(['the save did not finish']);
    });

    it('takes every report away at once', () => {
        report('one');
        report('two');

        dismissAll();

        expect(current()).toHaveLength(0);
    });
});

describe('turning a failure into a report', () => {
    beforeEach(() => {
        dismissAll();
    });

    it("shows an Error's own sentence, which is the server's where one exists", () => {
        reportError(new ApiFailure(500, { code: 'internal', message: 'The ledger is closed.' }));

        expect(current().map((entry) => entry.message)).toEqual(['The ledger is closed.']);
    });

    it('leaves an expected refusal out, because the sign-in screen already states it', () => {
        reportError(
            new ApiFailure(401, { code: 'not-authenticated', message: 'Sign in to continue.' }),
        );

        expect(current()).toHaveLength(0);
    });

    it('reports anything that is not an Error by its string form', () => {
        reportError('the network went away');

        expect(current().map((entry) => entry.message)).toEqual(['the network went away']);
    });
});

describe('a query that states its own failure', () => {
    beforeEach(() => {
        dismissAll();
    });

    it('is not reported to the banner when it says it is quiet', () => {
        const refusal = new ApiFailure(403, {
            code: 'forbidden',
            message: 'You do not have access to this.',
        });

        reportQueryError(refusal, { meta: { quiet: true } });

        expect(current()).toEqual([]);
    });

    it('is reported when it does not say so', () => {
        const refusal = new ApiFailure(403, {
            code: 'forbidden',
            message: 'You do not have access to this.',
        });

        reportQueryError(refusal, {});
        reportQueryError(refusal, { meta: { quiet: false } });

        expect(current().map((entry) => entry.message)).toEqual([
            'You do not have access to this.',
        ]);
    });
});

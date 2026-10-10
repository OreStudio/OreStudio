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
import type { ReportingTreeNode } from '@ores/wire-protocol/browser';
import { matchPeople } from './PersonSearch.js';

function person(fullName: string, username: string, jobTitle = ''): ReportingTreeNode {
    return {
        accountId: username,
        username,
        fullName,
        jobTitle,
        reportsToAccountId: null,
        depth: 0,
        directReports: 0,
    };
}

const PEOPLE = [
    person('Grace Hopper', 'grace.hopper', 'Head of Rates'),
    person('Ada Lovelace', 'ada.lovelace', 'Quant'),
    person('Alan Turing', 'alan.turing', 'Head of Research'),
    person('', 'svc.batch'),
];

const names = (nodes: readonly ReportingTreeNode[]): string[] => nodes.map((node) => node.username);

describe('matchPeople', () => {
    it('lists everybody, by name, when nothing has been typed', () => {
        expect(names(matchPeople(PEOPLE, ''))).toEqual([
            'ada.lovelace',
            'alan.turing',
            'grace.hopper',
            'svc.batch',
        ]);
    });

    it('narrows as the name is typed, in any case and from any part of it', () => {
        expect(names(matchPeople(PEOPLE, 'la'))).toEqual(['ada.lovelace', 'alan.turing']);
        expect(names(matchPeople(PEOPLE, 'HOPP'))).toEqual(['grace.hopper']);
        expect(names(matchPeople(PEOPLE, 'lan tu'))).toEqual(['alan.turing']);
    });

    it('also matches the username and the job title', () => {
        expect(names(matchPeople(PEOPLE, 'svc'))).toEqual(['svc.batch']);
        expect(names(matchPeople(PEOPLE, 'head of'))).toEqual(['alan.turing', 'grace.hopper']);
    });

    it('matches nobody for a text that names nobody', () => {
        expect(matchPeople(PEOPLE, 'zzz')).toEqual([]);
    });
});

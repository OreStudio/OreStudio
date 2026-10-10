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
import type { Timeline, TimelineEvent } from '@ores/wire-protocol/browser';
import { lineChanges } from './lineHistory.js';

function version(n: number, manager: string, at: string): TimelineEvent {
    return {
        entityType: 'ores.iam.account',
        entityId: 'a',
        kind: n === 1 ? 'raised' : 'changed',
        at,
        actor: 'grace',
        version: n,
        reasonCode: '',
        commentary: '',
        fields: [{ name: 'Reports To Account ID', value: manager }],
    };
}

const name = (id: string | null): string => (id === null ? 'nobody' : id.toUpperCase());

describe('lineChanges', () => {
    it('lists the versions where the manager moved, newest first', () => {
        const story: Timeline = {
            subject: 'person',
            id: 'ada',
            gaps: [],
            events: [
                version(1, '', '2026-10-01 09:00:00Z'),
                version(2, 'm1', '2026-10-02 09:00:00Z'),
                version(3, 'm1', '2026-10-03 09:00:00Z'),
                version(4, 'm2', '2026-10-04 09:00:00Z'),
            ],
        };

        const changes = lineChanges(story, name);

        expect(changes.map((change) => [change.from, change.to])).toEqual([
            ['M1', 'M2'],
            ['nobody', 'M1'],
        ]);
        expect(changes[0]?.actor).toBe('grace');
    });

    it('does not count the creation of an account as a change', () => {
        const story: Timeline = {
            subject: 'person',
            id: 'ada',
            gaps: [],
            events: [version(1, 'm1', '2026-10-01 09:00:00Z')],
        };

        expect(lineChanges(story, name)).toEqual([]);
    });

    it('says nothing for a story that has not loaded', () => {
        expect(lineChanges(undefined, name)).toEqual([]);
    });
});

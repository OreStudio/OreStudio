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
import { orderTimeline, type TimelineEvent } from './timeline.js';

function event(over: Partial<TimelineEvent>): TimelineEvent {
    return {
        entityType: 'ores.iam.account',
        entityId: 'a',
        kind: 'changed',
        at: '2026-10-04 10:00:00Z',
        actor: '',
        version: 1,
        reasonCode: '',
        commentary: '',
        fields: [],
        ...over,
    };
}

/**
 * The order of a stream is the whole of what it says, so it is asserted
 * directly rather than through a screen: two entries in the wrong order read as
 * a different story.
 */
describe('the order of a timeline', () => {
    it('reads newest first', () => {
        const older = event({ at: '2026-10-01 09:00:00Z', version: 1 });
        const newer = event({ at: '2026-10-02 09:00:00Z', version: 2 });

        expect(orderTimeline([older, newer])).toEqual([newer, older]);
    });

    it('reads an act that lands later in the same second first', () => {
        const raised = event({ kind: 'raised', version: 1 });
        const granted = event({ kind: 'granted', version: 2 });

        expect(orderTimeline([granted, raised])).toEqual([granted, raised]);
        expect(orderTimeline([raised, granted])).toEqual([granted, raised]);
    });

    it('reads the sign-in above the change it was made under', () => {
        const signIn = event({ kind: 'signed_in', entityType: 'ores.iam.auth_event', entityId: 'e' });
        const change = event({ kind: 'changed' });

        expect(orderTimeline([change, signIn])).toEqual([signIn, change]);
    });

    it('reads the higher version of one row first when a second holds both', () => {
        const first = event({ version: 1 });
        const second = event({ version: 2 });

        expect(orderTimeline([first, second])).toEqual([second, first]);
    });
});

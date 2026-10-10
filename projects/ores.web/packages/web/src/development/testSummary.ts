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

import type { ScenarioSummary } from '@ores/contracts';

/** A scenario summary for a test, waiting and untouched unless the test says otherwise. */
export function summary(overrides: Partial<ScenarioSummary> = {}): ScenarioSummary {
    return {
        id: '11111111-1111-4111-8111-111111111111',
        title: 'A scenario',
        description: '',
        path: 'a/scenario.org',
        state: 'PENDING',
        phase: 'waiting',
        target: 'CurrencyDetailDialog',
        story: { id: 'S', title: 'A story' },
        task: { id: 'T', title: 'A task' },
        clients: [],
        steps: { total: 4, pending: 4, pass: 0, fail: 0, dropped: 0 },
        completedAt: '',
        ...overrides,
    };
}

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
import type { WorkflowProgress } from '@ores/wire-protocol/browser';
import { warnedSteps } from './SetupReport.js';

type Step = WorkflowProgress['steps'][number];

function step(index: number, status: string, log: Step['log'] = [], label = ''): Step {
    return {
        id: `id-${index}`,
        name: `step_${index}`,
        label,
        description: '',
        status,
        step_index: index,
        created_at: '',
        started_at: null,
        completed_at: null,
        error: '',
        log,
        progress: '',
        progress_done: null,
        progress_total: null,
    } as Step;
}

function run(steps: Step[]): WorkflowProgress {
    return {
        success: true,
        message: '',
        status: 'completed',
        error: '',
        step_count: steps.length,
        steps,
    };
}

describe('the setup report', () => {
    it('names only the steps that ended with warnings, in run order, with their warnings', () => {
        const report = warnedSteps(
            run([
                step(
                    3,
                    'completed_with_warnings',
                    [
                        { level: 'info', message: 'published 28 curve(s)', context: '' },
                        { level: 'warn', message: 'SEK-SWESTR: no series', context: '' },
                    ],
                    "Publish each party's curves",
                ),
                step(1, 'completed'),
                step(2, 'completed_with_warnings', [], ''),
            ]),
        );

        expect(report.map((s) => s.label)).toEqual(['step_2', "Publish each party's curves"]);
        // The name is what a re-run sends, whatever the label reads.
        expect(report.map((s) => s.name)).toEqual(['step_2', 'step_3']);
        expect(report[0]?.warnings).toEqual([]);
        expect(report[1]?.warnings).toEqual(['SEK-SWESTR: no series']);
    });

    it('is empty for a run whose steps all completed cleanly', () => {
        expect(warnedSteps(run([step(0, 'completed'), step(1, 'completed')]))).toEqual([]);
    });
});

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
import {
  canGoBack,
  defineJourney,
  indexOfStep,
  nextPosition,
  rail,
  stepAt,
  type JourneyStep,
} from './runtime.js';

function step(id: string, title: string, extra: Partial<JourneyStep<string>> = {}): JourneyStep<string> {
  return { id, title, lead: `${title} lead`, body: `${title} body`, ...extra };
}

const welcome = step('welcome', 'Welcome');
const administrator = step('administrator', 'Create the administrator', { final: true });
const describe_ = step('describe', 'Describe the tenant');
const provisioning = step('provisioning', 'Provisioning');
const review = step('review', 'Review');
const ready = step('ready', 'Ready');

describe('the rail', () => {
  it('is one entry per step, in the order the steps declare', () => {
    const steps = defineJourney([welcome, administrator, describe_, provisioning]);

    expect(rail(steps, 0)).toEqual([
      { id: 'welcome', title: 'Welcome', state: 'current' },
      { id: 'administrator', title: 'Create the administrator', state: 'ahead' },
      { id: 'describe', title: 'Describe the tenant', state: 'ahead' },
      { id: 'provisioning', title: 'Provisioning', state: 'ahead' },
    ]);
  });

  it('takes every title from the step, so the rail cannot disagree with it', () => {
    const steps = defineJourney([welcome, administrator, ready]);

    expect(rail(steps, 1).map((entry) => entry.title)).toEqual([
      'Welcome',
      'Create the administrator',
      'Ready',
    ]);
  });

  it('marks what is behind, what is here, and what is ahead', () => {
    const steps = defineJourney([welcome, administrator, describe_, ready]);

    expect(rail(steps, 2).map((entry) => entry.state)).toEqual(['done', 'done', 'current', 'ahead']);
  });

  it('refuses a position the journey does not have', () => {
    const steps = defineJourney([welcome, ready]);

    expect(() => rail(steps, 2)).toThrow(RangeError);
    expect(() => rail(steps, -1)).toThrow(RangeError);
  });
});

describe('the step at a position', () => {
  it('answers with the step the position names', () => {
    const steps = defineJourney([welcome, administrator, ready]);

    expect(stepAt(steps, 1)).toBe(administrator);
  });

  it('refuses a position the journey does not have', () => {
    const steps = defineJourney([welcome, ready]);

    expect(() => stepAt(steps, 2)).toThrow(RangeError);
    expect(() => stepAt(steps, -1)).toThrow(RangeError);
    expect(() => stepAt(steps, 0.5)).toThrow(RangeError);
  });
});

describe('going back', () => {
  it('is refused at the first step', () => {
    const steps = defineJourney([welcome, review]);

    expect(canGoBack(steps, 0)).toBe(false);
  });

  it('is refused once the step before it changed server state', () => {
    const steps = defineJourney([welcome, administrator, describe_]);

    expect(steps[1]?.final).toBe(true);
    expect(canGoBack(steps, 2)).toBe(false);
  });

  it('is allowed when the step before it changed nothing', () => {
    const steps = defineJourney([welcome, describe_, review]);

    expect(canGoBack(steps, 2)).toBe(true);
  });
});

describe('moving on', () => {
  it('gives the next position, and nothing at the last step', () => {
    const steps = defineJourney([welcome, review, ready]);

    expect(nextPosition(steps, 0)).toBe(1);
    expect(nextPosition(steps, 2)).toBeUndefined();
  });
});

describe('finding a step by name', () => {
  it('answers with the position the step occupies', () => {
    const steps = defineJourney([welcome, administrator, provisioning]);

    expect(indexOfStep(steps, 'provisioning')).toBe(2);
  });

  it('fails loudly on a name the journey does not have', () => {
    const steps = defineJourney([welcome, ready]);

    expect(() => indexOfStep(steps, 'provisioning')).toThrow('journey has no step "provisioning"');
  });

  it('finds an inlined journey\'s step at the position it lands in', () => {
    const tenantSteps = [describe_, provisioning, review];
    const firstRun = defineJourney([welcome, administrator, ...tenantSteps, ready]);

    const at = indexOfStep(firstRun, 'provisioning');

    expect(at).toBe(3);
    expect(rail(firstRun, at).map((entry) => entry.state)).toEqual([
      'done',
      'done',
      'done',
      'current',
      'ahead',
      'ahead',
    ]);
  });
});

describe('defining a journey', () => {
  it('refuses an empty list, because a journey with no step has no rail', () => {
    expect(() => defineJourney([])).toThrow('a journey needs at least one step');
  });

  it('refuses a repeated step id, which is how an inlined journey collides', () => {
    const innerJourney = [describe_, provisioning];
    const outerJourney = [welcome, step('provisioning', 'Provisioning again')];
    const collides = [...outerJourney, ...innerJourney];

    expect(() => defineJourney(collides)).toThrow('duplicate step id "provisioning"');
  });

  it('cannot be extended after validation, which is what would undo the check', () => {
    const steps = defineJourney([welcome, ready]);
    /*
     * The cast is the point of the test: `readonly` is what the compiler
     * enforces, and a caller who ignores it is what the freeze is for.
     */
    const asTheCompilerSeesIt = steps as JourneyStep<string>[];

    expect(Object.isFrozen(steps)).toBe(true);
    expect(() => asTheCompilerSeesIt.push(step('welcome', 'Welcome again'))).toThrow(TypeError);
  });
});

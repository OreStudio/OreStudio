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

import { useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Notice, cx } from '../ui/Primitives.js';
import { canGoBack, nextPosition, rail, stepAt, type JourneyStep, type RailState } from './runtime.js';

/** The journey page's inputs: the step list, where the person is, and how to move. */
export interface JourneyPageProps {
  readonly steps: readonly JourneyStep<ReactNode>[];
  readonly at: number;
  readonly onMove: (index: number) => void;
}

const RAIL_ENTRY: Record<RailState, string> = {
  done: 'text-ink-muted',
  current: 'bg-surface-hover font-medium text-ink',
  ahead: 'text-ink-faint',
};

const RAIL_MARK: Record<RailState, string> = {
  done: 'border-up text-up',
  current: 'border-accent text-accent-bright',
  ahead: 'border-line',
};

/**
 * The one page that renders a journey.
 *
 * It renders any journey, because the rail comes from the same step list the
 * body does: there is no second place a step is declared, so the rail and the
 * steps cannot disagree. The position belongs to the caller, which is what lets
 * one journey inline another journey's steps.
 *
 * The page holds no journey state beyond the outcome of the action it just ran.
 * A step's `run` is the only place a step reaches the server, and a step that
 * fails does not advance: moving past a failed write is the failure the back
 * rule exists to prevent.
 *
 * The journeys that mount it arrive with their own plan steps, so nothing
 * imports it yet.
 */
export function JourneyPage({ steps, at, onMove }: JourneyPageProps): ReactNode {
  const { t } = useTranslation();
  const [failure, setFailure] = useState<string>();
  const [running, setRunning] = useState(false);

  const entries = rail(steps, at);
  const step = stepAt(steps, at);
  const forward = nextPosition(steps, at);
  const back = canGoBack(steps, at);

  const advance = async (): Promise<void> => {
    const action = step.next;
    if (action === undefined) {
      return;
    }
    setFailure(undefined);
    if (action.run !== undefined) {
      setRunning(true);
      try {
        await action.run();
      } catch (error) {
        setRunning(false);
        setFailure(error instanceof Error ? error.message : String(error));
        return;
      }
      setRunning(false);
    }
    if (forward !== undefined) {
      onMove(forward);
    }
  };

  return (
    <div className="grid gap-8 md:grid-cols-[14rem_1fr]">
      <nav aria-label={t('journey.steps')}>
        <ol className="space-y-1">
          {entries.map((entry, index) => (
            <li
              key={entry.id}
              aria-current={entry.state === 'current' ? 'step' : undefined}
              className={cx(
                'flex items-center gap-3 rounded-md px-3 py-2 text-sm',
                RAIL_ENTRY[entry.state],
              )}
            >
              <span
                aria-hidden
                className={cx(
                  'grid size-6 shrink-0 place-items-center rounded-full border text-xs',
                  RAIL_MARK[entry.state],
                )}
              >
                {entry.state === 'done' ? '✓' : index + 1}
              </span>
              {entry.title}
            </li>
          ))}
        </ol>
      </nav>

      <section className="card p-6">
        <h2 className="mb-1 text-lg font-semibold">{step.title}</h2>
        <p className="mb-5 text-sm text-ink-muted">{step.lead}</p>
        {failure !== undefined && (
          <Notice tone="error">{t('journey.actionFailed', { message: failure })}</Notice>
        )}
        {step.body}
        {(step.next !== undefined || back) && (
          <div className="mt-6 flex border-t border-line pt-4">
            <Button variant="ghost" disabled={!back} onClick={() => onMove(at - 1)}>
              {t('common.back')}
            </Button>
            {step.next !== undefined && (
              <Button
                variant="primary"
                className="ml-auto"
                disabled={!step.next.enabled}
                pending={running}
                /*
                 * The button's own pending state reports the
                 * wait, and a rejection sets the failure notice
                 * above. Nothing else can act on it.
                 */
                onClick={() => void advance()}
              >
                {step.next.label}
              </Button>
            )}
          </div>
        )}
      </section>
    </div>
  );
}

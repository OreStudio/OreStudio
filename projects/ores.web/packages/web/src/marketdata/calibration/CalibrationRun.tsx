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
 */
import { useEffect, useRef, useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import { api, type CalibratedFeed } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { calibrationFeedPath } from '../../shell/paths.js';
import { Button, Notice, cx } from '../../ui/Primitives.js';
import {
    advance,
    needsRestart,
    runCounts,
    runOf,
    toneOf,
    type RunStep,
    type StepTone,
} from './calibration.js';

/**
 * A calibration run, one feed a call: each feed is calibrated and stored in turn, so every
 * row reports the server's own answer as it arrives. Leaving the screen stops the run after
 * the feed in flight.
 */
export function useCalibrationRun(assetClass: string, onFinished: () => void) {
    const [steps, setSteps] = useState<readonly RunStep[]>();
    const [active, setActive] = useState(false);
    const live = useRef(true);
    useEffect(() => {
        live.current = true;
        return () => {
            live.current = false;
        };
    }, []);
    const start = async (feeds: readonly CalibratedFeed[]) => {
        const initial = runOf(feeds);
        setSteps(initial);
        setActive(true);
        for (const step of initial) {
            if (!live.current) {
                return;
            }
            setSteps((now) => advance(now ?? initial, step.configId, { state: 'running' }));
            try {
                const result = await api.runCalibration(assetClass, step.configId);
                setSteps((now) =>
                    advance(now ?? initial, step.configId, { state: 'done', result }),
                );
            } catch (error) {
                const message = error instanceof Error ? error.message : String(error);
                setSteps((now) =>
                    advance(now ?? initial, step.configId, { state: 'failed', error: message }),
                );
            }
        }
        if (live.current) {
            setActive(false);
            onFinished();
        }
    };
    return { steps, active, start, close: () => setSteps(undefined) };
}

const MARK: Record<StepTone, string> = { pending: '·', ok: '✓', same: '=', warn: '!', bad: '✕' };

const MARK_TONE: Record<StepTone, string> = {
    pending: 'text-ink-faint',
    ok: 'text-up',
    same: 'text-ink-muted',
    warn: 'text-warn',
    bad: 'text-down',
};

function StepMark({ step }: { readonly step: RunStep }): ReactNode {
    if (step.state === 'running') {
        return (
            <span
                aria-hidden
                className="inline-block h-3 w-3 animate-spin rounded-full border border-line-strong border-t-transparent"
            />
        );
    }
    const tone = toneOf(step);
    return (
        <span aria-hidden className={cx('w-3 text-center font-semibold', MARK_TONE[tone])}>
            {MARK[tone]}
        </span>
    );
}

/** What a step says: the server's words once done, else where the step stands. */
function stepWords(step: RunStep, t: (key: string) => string): string {
    if (step.state === 'failed') {
        return step.error ?? '';
    }
    if (step.state === 'done' && step.result !== undefined) {
        return step.result.outcome_message;
    }
    return t(`marketdata.calibration.run.${step.state}`);
}

/**
 * The run as it happens: one row a feed with its mark and the server's words, the count done,
 * and, once over, the feeds that run now and take the new values only when they start again.
 */
export function CalibrationRun({
    assetClass,
    steps,
    active,
    onClose,
}: {
    readonly assetClass: string;
    readonly steps: readonly RunStep[];
    readonly active: boolean;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const counts = runCounts(steps);
    const restart = needsRestart(steps);
    const [restarting, setRestarting] = useState<'idle' | 'busy' | 'done'>('idle');
    const [failure, setFailure] = useState<string>();
    const restartAll = async () => {
        setRestarting('busy');
        setFailure(undefined);
        try {
            for (const step of restart) {
                await api.stopFeed(step.configId);
                await api.startFeed(step.configId);
            }
            setRestarting('done');
        } catch (error) {
            setFailure(error instanceof Error ? error.message : String(error));
            setRestarting('idle');
        }
    };
    const percent = counts.total === 0 ? 100 : Math.round((counts.finished / counts.total) * 100);
    return (
        <section
            className="card space-y-3 p-4"
            aria-label={t('marketdata.calibration.run.title')}
            aria-busy={active}
        >
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="font-medium">{t('marketdata.calibration.run.title')}</h2>
                <span className="text-sm tabular-nums text-ink-muted">
                    {t('marketdata.calibration.run.progress', {
                        finished: String(counts.finished),
                        total: String(counts.total),
                    })}
                </span>
            </header>
            <div
                className="h-1.5 overflow-hidden rounded-full bg-line-subtle"
                role="progressbar"
                aria-valuemin={0}
                aria-valuemax={100}
                aria-valuenow={percent}
            >
                <div className="h-full bg-accent transition-all" style={{ width: `${percent}%` }} />
            </div>
            <ol className="divide-y divide-line-subtle text-sm">
                {steps.map((step) => (
                    <li key={step.configId} className="flex items-start gap-3 py-2">
                        <span className="mt-1 flex w-4 justify-center">
                            <StepMark step={step} />
                        </span>
                        <div className="min-w-0 flex-1">
                            <div className="flex flex-wrap items-baseline gap-x-2">
                                <Link
                                    className="font-medium text-ink hover:text-accent"
                                    to={calibrationFeedPath(assetClass, step.configId)}
                                >
                                    {step.pair}
                                </Link>
                                <span className="font-mono text-xs text-ink-faint">
                                    {step.sourceName}
                                </span>
                            </div>
                            <p
                                className={cx(
                                    'text-ink-muted',
                                    toneOf(step) === 'bad' && 'text-down',
                                )}
                            >
                                {stepWords(step, t)}
                            </p>
                            {step.result?.outcome === 'updated' && step.result.running && (
                                <p className="text-xs text-warn">
                                    {t('marketdata.calibration.run.nextStart')}
                                </p>
                            )}
                        </div>
                    </li>
                ))}
            </ol>
            {!active && (
                <div className="space-y-2 border-t border-line-subtle pt-3">
                    <p className="text-sm">
                        {t('marketdata.calibration.run.summary', {
                            updated: String(counts.updated),
                            unchanged: String(counts.unchanged),
                            kept: String(counts.kept),
                            failed: String(counts.failed),
                        })}
                    </p>
                    {failure !== undefined && <Notice tone="error">{failure}</Notice>}
                    {restarting === 'done' && (
                        <Notice tone="success">{t('marketdata.calibration.run.restarted')}</Notice>
                    )}
                    <div className="flex flex-wrap gap-2">
                        {restart.length > 0 && restarting !== 'done' && (
                            <Button
                                variant="primary"
                                pending={restarting === 'busy'}
                                onClick={() => void restartAll()}
                            >
                                {t('marketdata.calibration.run.restart', {
                                    count: String(restart.length),
                                })}
                            </Button>
                        )}
                        <Button onClick={onClose}>{t('marketdata.calibration.run.close')}</Button>
                    </div>
                </div>
            )}
        </section>
    );
}

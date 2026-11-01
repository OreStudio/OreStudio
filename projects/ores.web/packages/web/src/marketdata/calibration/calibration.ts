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
import type { CalibratedFeed } from '../../api/client.js';

/** The asset classes whose feeds calibrate from fixings today. */
export const CALIBRATED_CLASSES = ['fx'] as const;

/** How a feed resolved: from a fixing, on its fixed level, or refused by the policy. */
export type FeedOutcome = 'fixing' | 'fixed' | 'refused';

export function outcomeOf(feed: CalibratedFeed): FeedOutcome {
    if (feed.refused) {
        return 'refused';
    }
    return feed.start.from_fixing ? 'fixing' : 'fixed';
}

/** A feed states an exception to its class policy: its own items or its own window. */
export function overrides(feed: CalibratedFeed): boolean {
    return feed.explicit_items || feed.window_days_override != null;
}

/** A feed a person should look at: it took no fixing, or it overrides the policy. */
export function needsAttention(feed: CalibratedFeed): boolean {
    return outcomeOf(feed) !== 'fixing' || overrides(feed);
}

export const FILTERS = ['all', 'fixing', 'kept', 'attention', 'overridden'] as const;
export type CalibrationFilter = (typeof FILTERS)[number];

export function filterOf(value: string | null): CalibrationFilter {
    return FILTERS.find((f) => f === value) ?? 'all';
}

export function visibleFeeds(
    feeds: readonly CalibratedFeed[],
    filter: CalibrationFilter,
): readonly CalibratedFeed[] {
    if (filter === 'fixing') {
        return feeds.filter((feed) => feed.start.from_fixing);
    }
    if (filter === 'kept') {
        return feeds.filter((feed) => !feed.start.from_fixing && !feed.refused);
    }
    if (filter === 'attention') {
        return feeds.filter(needsAttention);
    }
    if (filter === 'overridden') {
        return feeds.filter(overrides);
    }
    return feeds;
}

/** The provider order with @p code moved by @p delta places, kept inside the list. */
export function moveProvider(
    order: readonly string[],
    code: string,
    delta: number,
): readonly string[] {
    const from = order.indexOf(code);
    const to = from + delta;
    if (from < 0 || to < 0 || to >= order.length) {
        return order;
    }
    const out = [...order];
    out.splice(from, 1);
    out.splice(to, 0, code);
    return out;
}

/** Where one feed stands in a calibration run. */
export type RunState = 'waiting' | 'running' | 'done' | 'failed';

/** One feed of a calibration run: the feed, where it stands, and what the server answered. */
export interface RunStep {
    readonly configId: string;
    readonly pair: string;
    readonly sourceName: string;
    readonly state: RunState;
    /** The server's answer, once the feed is done. */
    readonly result?: CalibratedFeed;
    /** Why the call failed, when it did. */
    readonly error?: string;
}

/** A run over @p feeds, every feed waiting, in the order the list shows them. */
export function runOf(feeds: readonly CalibratedFeed[]): readonly RunStep[] {
    return feeds.map((feed) => ({
        configId: feed.config_id,
        pair: `${feed.base_currency_code}/${feed.quote_currency_code}`,
        sourceName: feed.source_name,
        state: 'waiting',
    }));
}

/** The run with the one step @p configId replaced by @p patch. */
export function advance(
    steps: readonly RunStep[],
    configId: string,
    patch: Partial<Omit<RunStep, 'configId'>>,
): readonly RunStep[] {
    return steps.map((step) => (step.configId === configId ? { ...step, ...patch } : step));
}

/** How a step reads: ok when written or already right, warn when kept, bad when refused or failed. */
export type StepTone = 'pending' | 'ok' | 'same' | 'warn' | 'bad';

export function toneOf(step: RunStep): StepTone {
    if (step.state === 'failed') {
        return 'bad';
    }
    if (step.state !== 'done' || step.result === undefined) {
        return 'pending';
    }
    switch (step.result.outcome) {
        case 'updated':
            return 'ok';
        case 'unchanged':
            return 'same';
        case 'kept':
            return 'warn';
        default:
            return 'bad';
    }
}

/** The steps whose feed runs now and took a new calibration, so it applies only on a restart. */
export function needsRestart(steps: readonly RunStep[]): readonly RunStep[] {
    return steps.filter((step) => step.result?.outcome === 'updated' && step.result.running);
}

/** The counts a run reports: how far it is, and what each finished feed came to. */
export function runCounts(steps: readonly RunStep[]) {
    const tones = steps.map(toneOf);
    return {
        total: steps.length,
        finished: steps.filter((s) => s.state === 'done' || s.state === 'failed').length,
        updated: tones.filter((t) => t === 'ok').length,
        unchanged: tones.filter((t) => t === 'same').length,
        kept: tones.filter((t) => t === 'warn').length,
        failed: tones.filter((t) => t === 'bad').length,
    };
}

/** The reason code a calibration writes its versions under. */
export const CALIBRATION_REASON = 'system.calibration';

/** The fixing series page of an oresmd item. */
export function seriesPathOf(fixingSeriesPath: string, item: string): string {
    return `${fixingSeriesPath}?uri=${encodeURIComponent(item)}`;
}

/** One calibration that changed a feed: when, who, and what it wrote. */
export interface CalibrationEntry {
    readonly at: string;
    readonly actor: string;
    readonly commentary: string;
}

/**
 * The calibrations in a feed's story, newest first. A calibration writes the config and its
 * components under one commentary, so the versions it wrote together are one entry.
 */
export function calibrationEntries(
    events: readonly {
        readonly at: string;
        readonly actor: string;
        readonly reasonCode?: string;
        readonly commentary?: string;
    }[],
): readonly CalibrationEntry[] {
    const seen = new Set<string>();
    const out: CalibrationEntry[] = [];
    const ordered = [...events]
        .filter((e) => e.reasonCode === CALIBRATION_REASON)
        .sort((a, b) => b.at.localeCompare(a.at));
    for (const e of ordered) {
        const key = `${e.at.slice(0, 19)}\u0000${e.commentary ?? ''}`;
        if (seen.has(key)) continue;
        seen.add(key);
        out.push({ at: e.at, actor: e.actor, commentary: e.commentary ?? '' });
    }
    return out;
}

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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import { PATHS } from '../../shell/paths.js';
import { useTranslation } from '../../i18n/Provider.js';
import { api, type MarketRegime, type RecordRow, type TraceFeed } from '../../api/client.js';
import { show } from '../../refdata/records.js';
import { Button, Dialog, Fact, Field, Input, Notice, Select } from '../../ui/Primitives.js';
import { lawText } from './marketTrace.js';
import { regimeLabel, useMarketRegime, useMarketTrace } from './useMarketRegime.js';

/** The reason an arrivals change is recorded under: a code, the same in every language. */
const REASON_CODE = 'system.update';

/** How a save of the arrivals ended: stored, stored but not restarted, or not stored. */
export type ArrivalsSaveOutcome =
    | { readonly kind: 'saved' }
    | { readonly kind: 'restartFailed'; readonly message: string }
    | { readonly kind: 'failed'; readonly message: string };

function messageOf(cause: unknown): string {
    return cause instanceof Error ? cause.message : String(cause);
}

/**
 * Stores the arrivals, re-reads the feed, and restarts it when asked. The feed is re-read as
 * soon as the save is stored, so a restart that fails cannot leave the form comparing against
 * the old row; and a failed restart is told apart from a failed save, since the law is stored
 * either way.
 */
export async function saveArrivals(steps: {
    readonly save: () => Promise<void>;
    readonly saved?: (() => Promise<void>) | undefined;
    readonly restart?: (() => Promise<void>) | undefined;
}): Promise<ArrivalsSaveOutcome> {
    try {
        await steps.save();
    } catch (cause) {
        return { kind: 'failed', message: messageOf(cause) };
    }
    await steps.saved?.();
    if (steps.restart === undefined) {
        return { kind: 'saved' };
    }
    try {
        await steps.restart();
        await steps.saved?.();
        return { kind: 'saved' };
    } catch (cause) {
        await steps.saved?.();
        return { kind: 'restartFailed', message: messageOf(cause) };
    }
}

/** The models a feed's ticks can arrive under. */
const MODELS = ['periodic', 'poisson', 'hawkes'] as const;
type Model = (typeof MODELS)[number];

/** What saving a feed's arrivals needs. */
type SaveArrivalsLaw = (
    model: Model,
    ratio: number,
    decay: number,
    sensitivity: number,
    intent: { readonly reasonCode: string; readonly commentary: string },
) => Promise<void>;

/**
 * One feed's arrivals: the model its ticks arrive under, the two numbers that
 * say how a burst feeds itself, and how hard the feed follows the market.
 *
 * This is the feed's own view and nothing else's: it reads the market and cannot
 * change it, and it never shows another feed. The market itself is set on the
 * Synthetic screen's Market tab.
 *
 * The tab reads, and Edit opens the dialog that writes, as a record's sections do.
 */
export function ArrivalsPanel({
    feed,
    market,
    law,
    mayWriteFeed,
    running = false,
    save,
    restart,
    saved,
}: {
    readonly feed: RecordRow;
    /** The market in force, or undefined while it is not read. The panel states it and cannot set it. */
    readonly market: MarketRegime | undefined;
    /** The feed's law as the scheduler runs it now, or undefined when the feed does not run. */
    readonly law?: TraceFeed | undefined;
    readonly mayWriteFeed: boolean;
    /** Whether the feed runs, and so runs on the law it started with until it starts again. */
    readonly running?: boolean;
    readonly save: SaveArrivalsLaw;
    /** Stops and starts the feed, so it draws its law again. */
    readonly restart?: () => Promise<void>;
    /** Re-reads the feed once a save is done, so the next edit is against what was stored. */
    readonly saved?: () => Promise<void>;
}): ReactNode {
    const { t } = useTranslation();
    const [editing, setEditing] = useState(false);
    const model = modelOf(feed);
    const hawkes = model === 'hawkes';
    return (
        <section className="space-y-4" aria-label={t('marketdata.arrivals.title')}>
            <div className="flex flex-wrap items-start justify-between gap-3">
                <p className="text-sm text-ink-muted">{t('marketdata.arrivals.lead')}</p>
                {mayWriteFeed ? (
                    <Button icon="edit" onClick={() => setEditing(true)}>
                        {t('refdata.records.edit')}
                    </Button>
                ) : null}
            </div>

            <dl className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
                <Fact
                    label={t('marketdata.arrivals.model')}
                    value={t(`marketdata.arrivals.model.${model}`)}
                />
                <Fact
                    label={t('marketdata.arrivals.sensitivity')}
                    value={String(numberOf(feed, 'sensitivity', 1))}
                />
                <Fact
                    label={t('marketdata.arrivals.ratio')}
                    value={hawkes ? String(numberOf(feed, 'branching_ratio', 0)) : '—'}
                />
                <Fact
                    label={t('marketdata.arrivals.decay')}
                    value={hawkes ? String(numberOf(feed, 'decay', 1)) : '—'}
                />
            </dl>

            {/* Read only, and the only global fact on this tab. */}
            <p className="flex flex-wrap items-baseline gap-x-3 text-sm text-ink-muted">
                {t('marketdata.arrivals.market', {
                    state:
                        market === undefined
                            ? t('marketdata.arrivals.marketUnknown')
                            : regimeLabel(market, t),
                })}
                <Link to={`${PATHS.synthetic}?tab=regime`} className="text-accent hover:underline">
                    {t('marketdata.arrivals.openMarket')}
                </Link>
            </p>

            <p data-running-law className="font-mono text-xs text-ink-muted">
                {law === undefined
                    ? t('marketdata.arrivals.notRunning')
                    : t('marketdata.arrivals.runningLaw', { law: lawText(law, t) })}
            </p>

            {editing ? (
                <ArrivalsDialog
                    feed={feed}
                    running={running}
                    save={save}
                    restart={restart}
                    saved={saved}
                    onClose={() => setEditing(false)}
                />
            ) : null}
        </section>
    );
}

/**
 * Edits a feed's arrivals: the model, the sensitivity, and the two Hawkes numbers, which only a
 * Hawkes feed reads and so are disabled under the other two models rather than hidden, so a
 * person switching models sees what will be used. A running feed keeps the law it started with,
 * as it keeps its process, so the dialog offers to restart it on the law saved.
 */
export function ArrivalsDialog({
    feed,
    running,
    save,
    restart,
    saved,
    onClose,
}: {
    readonly feed: RecordRow;
    readonly running: boolean;
    readonly save: SaveArrivalsLaw;
    readonly restart?: (() => Promise<void>) | undefined;
    readonly saved?: (() => Promise<void>) | undefined;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const [restartAfter, setRestartAfter] = useState(true);
    const [model, setModel] = useState<Model>(modelOf(feed));
    // What was typed, so a cleared field reads as empty rather than as zero.
    const [ratioText, setRatioText] = useState(String(numberOf(feed, 'branching_ratio', 0)));
    const [decayText, setDecayText] = useState(String(numberOf(feed, 'decay', 1)));
    const [sensitivityText, setSensitivityText] = useState(
        String(numberOf(feed, 'sensitivity', 1)),
    );
    const ratio = Number(ratioText);
    const decay = Number(decayText);
    const sensitivity = Number(sensitivityText);
    const unreadable = (text: string): boolean =>
        text.trim() === '' || !Number.isFinite(Number(text));
    const [saving, setSaving] = useState(false);
    const [error, setError] = useState<string | null>(null);

    const hawkes = model === 'hawkes';
    const changed =
        model !== modelOf(feed) ||
        ratio !== numberOf(feed, 'branching_ratio', 0) ||
        decay !== numberOf(feed, 'decay', 1) ||
        sensitivity !== numberOf(feed, 'sensitivity', 1);
    const invalid = unreadable(sensitivityText) || unreadable(ratioText) || unreadable(decayText);

    const saveNow = async (): Promise<void> => {
        setSaving(true);
        setError(null);
        const outcome = await saveArrivals({
            save: () =>
                save(model, ratio, decay, sensitivity, { reasonCode: REASON_CODE, commentary: '' }),
            saved,
            restart: running && restartAfter ? restart : undefined,
        });
        setSaving(false);
        if (outcome.kind === 'failed') {
            setError(outcome.message);
        } else if (outcome.kind === 'restartFailed') {
            setError(t('marketdata.arrivals.restartFailed', { message: outcome.message }));
        } else {
            onClose();
        }
    };

    return (
        <Dialog
            title={t('marketdata.arrivals.editTitle', { feed: show(feed['source_name']) })}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('refdata.records.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        pending={saving}
                        disabled={!changed || invalid}
                        onClick={() => void saveNow()}
                    >
                        {t('entity.save')}
                    </Button>
                </>
            }
        >
            <div className="grid gap-4 sm:grid-cols-2">
                <Field label={t('marketdata.arrivals.model')}>
                    <Select
                        value={model}
                        onChange={(event) => setModel(event.target.value as Model)}
                    >
                        {MODELS.map((option) => (
                            <option key={option} value={option}>
                                {t(`marketdata.arrivals.model.${option}`)}
                            </option>
                        ))}
                    </Select>
                </Field>
                <Field
                    label={t('marketdata.arrivals.sensitivity')}
                    hint={t('marketdata.arrivals.sensitivityHelp')}
                >
                    <Input
                        type="number"
                        step="any"
                        value={sensitivityText}
                        aria-invalid={unreadable(sensitivityText) ? true : undefined}
                        onChange={(event) => setSensitivityText(event.target.value)}
                    />
                </Field>
                <Field
                    label={t('marketdata.arrivals.ratio')}
                    hint={t('marketdata.arrivals.ratioHelp')}
                >
                    <Input
                        type="number"
                        step="any"
                        value={ratioText}
                        aria-invalid={unreadable(ratioText) ? true : undefined}
                        disabled={!hawkes}
                        onChange={(event) => setRatioText(event.target.value)}
                    />
                </Field>
                <Field
                    label={t('marketdata.arrivals.decay')}
                    hint={t('marketdata.arrivals.decayHelp')}
                >
                    <Input
                        type="number"
                        step="any"
                        value={decayText}
                        aria-invalid={unreadable(decayText) ? true : undefined}
                        disabled={!hawkes}
                        onChange={(event) => setDecayText(event.target.value)}
                    />
                </Field>
            </div>
            {running ? (
                <div className="mt-4 space-y-2 rounded-md border border-line p-3 text-sm">
                    <p>{t('marketdata.arrivals.runningNote')}</p>
                    <label className="flex items-center gap-2">
                        <input
                            type="checkbox"
                            checked={restartAfter}
                            onChange={(event) => setRestartAfter(event.target.checked)}
                        />
                        {t('marketdata.arrivals.restart')}
                    </label>
                </div>
            ) : null}
            {error === null ? null : (
                <div className="mt-4">
                    <Notice tone="error">{error}</Notice>
                </div>
            )}
        </Dialog>
    );
}

/**
 * A feed's Arrivals tab: the panel above, with the market state the run reports
 * filled in.
 *
 * The state is read here rather than handed down, because the tab is a page of
 * its own and the feed's screen knows nothing of the market. A market that cannot
 * be read leaves the line saying so, and nothing else on the tab changes.
 */
export function ArrivalsTab({
    feed,
    mayWriteFeed,
    running,
    save,
}: {
    readonly feed: RecordRow;
    readonly mayWriteFeed: boolean;
    readonly running: boolean;
    readonly save: SaveArrivalsLaw;
}): ReactNode {
    const queries = useQueryClient();
    const source = show(feed['source_name']);
    const trace = useMarketTrace(source);
    return (
        <ArrivalsPanel
            feed={feed}
            market={useMarketRegime()}
            law={trace.data?.feeds.find((running) => running.sourceName === source)}
            mayWriteFeed={mayWriteFeed}
            running={running}
            save={save}
            restart={async () => {
                const id = show(feed['id']);
                await api.stopFeed(id);
                await api.startFeed(id);
            }}
            saved={async () => {
                await queries.invalidateQueries({ queryKey: ['records'] });
                await queries.invalidateQueries({ queryKey: ['history'] });
            }}
        />
    );
}

function modelOf(feed: RecordRow): Model {
    const value = show(feed['arrival_mode']);
    return (MODELS as readonly string[]).includes(value) ? (value as Model) : 'periodic';
}

function numberOf(feed: RecordRow, field: string, fallback: number): number {
    const value = Number(feed[field]);
    return Number.isFinite(value) ? value : fallback;
}

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

import { useState, type ReactNode } from 'react';
import { useTranslation } from '../../i18n/Provider.js';
import { api, type MarketRegime, type RecordRow } from '../../api/client.js';
import { show } from '../../refdata/records.js';
import { Notice } from '../../ui/Primitives.js';
import { regimeLabel, useMarketRegime } from './useMarketRegime.js';

/** The models a feed's ticks can arrive under. */
const MODELS = ['periodic', 'poisson', 'hawkes'] as const;
type Model = (typeof MODELS)[number];

/**
 * One feed's arrivals: the model its ticks arrive under, the two numbers that
 * say how a burst feeds itself, and how hard the feed follows the market.
 *
 * This is the feed's own view and nothing else's: it reads the market and cannot
 * change it, and it never shows another feed. The market itself is set on the
 * Market regime tab of the feeds window.
 *
 * The two Hawkes numbers are kept in the row whatever the model is, but only a
 * Hawkes feed reads them, so they are disabled under the other two models rather
 * than hidden: a person switching models sees what will be used.
 */
export function ArrivalsPanel({
    feed,
    market,
    mayWriteFeed,
    save,
}: {
    readonly feed: RecordRow;
    /** The market in force, or undefined while it is not read. The panel states it and cannot set it. */
    readonly market: MarketRegime | undefined;
    readonly mayWriteFeed: boolean;
    readonly save: (
        model: Model,
        ratio: number,
        decay: number,
        sensitivity: number,
        intent: { readonly reasonCode: string; readonly commentary: string },
    ) => Promise<void>;
}): ReactNode {
    const { t } = useTranslation();
    const [model, setModel] = useState<Model>(modelOf(feed));
    const [ratio, setRatio] = useState(numberOf(feed, 'branching_ratio', 0));
    const [decay, setDecay] = useState(numberOf(feed, 'decay', 1));
    const [sensitivity, setSensitivity] = useState(numberOf(feed, 'sensitivity', 1));
    const [saving, setSaving] = useState(false);
    const [error, setError] = useState<string | null>(null);

    const hawkes = model === 'hawkes';
    const changed =
        model !== modelOf(feed) ||
        ratio !== numberOf(feed, 'branching_ratio', 0) ||
        decay !== numberOf(feed, 'decay', 1) ||
        sensitivity !== numberOf(feed, 'sensitivity', 1);

    const saveNow = async (): Promise<void> => {
        setSaving(true);
        setError(null);
        try {
            await save(model, ratio, decay, sensitivity, {
                reasonCode: t('marketdata.arrivals.reasonCode'),
                commentary: '',
            });
        } catch (cause) {
            setError(cause instanceof Error ? cause.message : String(cause));
        } finally {
            setSaving(false);
        }
    };

    return (
        <section className="space-y-4" aria-label={t('marketdata.arrivals.title')}>
            <p className="text-sm text-ink-muted">{t('marketdata.arrivals.lead')}</p>

            <div className="grid gap-4 sm:grid-cols-2">
                <label className="grid gap-1">
                    <span className="text-xs uppercase tracking-wide text-ink-faint">
                        {t('marketdata.arrivals.model')}
                    </span>
                    <select
                        value={model}
                        disabled={!mayWriteFeed}
                        onChange={(event) => setModel(event.target.value as Model)}
                        className="rounded-md border border-line bg-surface-base px-2 py-1"
                    >
                        {MODELS.map((option) => (
                            <option key={option} value={option}>
                                {t(`marketdata.arrivals.model.${option}`)}
                            </option>
                        ))}
                    </select>
                </label>

                <NumberField
                    label={t('marketdata.arrivals.sensitivity')}
                    help={t('marketdata.arrivals.sensitivityHelp')}
                    value={sensitivity}
                    disabled={!mayWriteFeed}
                    onChange={setSensitivity}
                />
                <NumberField
                    label={t('marketdata.arrivals.ratio')}
                    help={t('marketdata.arrivals.ratioHelp')}
                    value={ratio}
                    disabled={!mayWriteFeed || !hawkes}
                    onChange={setRatio}
                />
                <NumberField
                    label={t('marketdata.arrivals.decay')}
                    help={t('marketdata.arrivals.decayHelp')}
                    value={decay}
                    disabled={!mayWriteFeed || !hawkes}
                    onChange={setDecay}
                />
            </div>

            {/* Read only, and the only global fact on this tab. */}
            <p className="text-sm text-ink-muted">
                {t('marketdata.arrivals.market', {
                    state:
                        market === undefined
                            ? t('marketdata.arrivals.marketUnknown')
                            : regimeLabel(market, t),
                })}
            </p>

            {error === null ? null : <Notice tone="error">{error}</Notice>}

            <div className="flex items-center gap-2">
                <button
                    type="button"
                    className="btn primary"
                    disabled={!mayWriteFeed || !changed || saving}
                    onClick={() => void saveNow()}
                >
                    {saving ? t('entity.saving') : t('entity.save')}
                </button>
                {changed ? (
                    <span className="text-xs text-ink-muted">
                        {t('marketdata.arrivals.unsaved')}
                    </span>
                ) : null}
            </div>
        </section>
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
    save,
}: {
    readonly feed: RecordRow;
    readonly mayWriteFeed: boolean;
    readonly save: (
        model: Model,
        ratio: number,
        decay: number,
        sensitivity: number,
        intent: { readonly reasonCode: string; readonly commentary: string },
    ) => Promise<void>;
}): ReactNode {
    return (
        <ArrivalsPanel
            feed={feed}
            market={useMarketRegime()}
            mayWriteFeed={mayWriteFeed}
            save={save}
        />
    );
}

function NumberField({
    label,
    help,
    value,
    disabled,
    onChange,
}: {
    readonly label: string;
    readonly help: string;
    readonly value: number;
    readonly disabled: boolean;
    readonly onChange: (next: number) => void;
}): ReactNode {
    return (
        <label className="grid gap-1">
            <span className="text-xs uppercase tracking-wide text-ink-faint">{label}</span>
            <input
                type="number"
                step="any"
                value={value}
                disabled={disabled}
                onChange={(event) => onChange(Number(event.target.value))}
                className="rounded-md border border-line bg-surface-base px-2 py-1 disabled:opacity-50"
            />
            <span className="text-xs text-ink-faint">{help}</span>
        </label>
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

/* -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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

import { useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { api, type RecordRow } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { show } from '../../refdata/records.js';
import { Button, Dialog, Fact, Field, Input, Notice, Select } from '../../ui/Primitives.js';
import { saveArrivals } from './ArrivalsPanel.js';

const REASON_CODE = 'system.update';

const MODELS = ['constant', 'gjr_garch'] as const;
type Model = (typeof MODELS)[number];

/** The engines whose law a daily variance can scale. */
const SCALABLE_ENGINES: readonly string[] = ['geometric', 'arithmetic'];

/** A feed's daily variance as the dialog writes it. */
export interface VarianceSettings {
    readonly model: Model;
    readonly alpha: number;
    readonly gamma: number;
    readonly beta: number;
    readonly windowDays: number;
}

type SaveVariance = (
    settings: VarianceSettings,
    intent: { readonly reasonCode: string; readonly commentary: string },
) => Promise<void>;

/**
 * Why a variance cannot be saved, or nothing when it can: the rules the table checks,
 * said before the save. Stationarity also depends on the feed's mixture, so the feed
 * checks it when it starts.
 */
export function varianceProblems(settings: VarianceSettings, engine: string): readonly string[] {
    if (settings.model === 'constant') {
        return [];
    }
    const problems: string[] = [];
    if (!SCALABLE_ENGINES.includes(engine)) {
        problems.push('engine');
    }
    if (!(settings.alpha >= 0)) {
        problems.push('alpha');
    }
    if (!(settings.beta >= 0)) {
        problems.push('beta');
    }
    if (!(settings.alpha + settings.gamma >= 0)) {
        problems.push('gamma');
    }
    if (
        !Number.isInteger(settings.windowDays) ||
        settings.windowDays < 30 ||
        settings.windowDays > 3660
    ) {
        problems.push('window');
    }
    return problems;
}

/**
 * One feed's daily variance: whether it is constant or follows a GJR-GARCH, the
 * authored shape, and how much history a fixing feed fits it from.
 *
 * The tab reads, and Edit opens the dialog that writes, as the Arrivals tab does.
 * What a run actually started with, fitted or authored, is in the notice of where
 * the run started.
 */
export function VariancePanel({
    feed,
    mayWriteFeed,
    running = false,
    save,
    restart,
    saved,
}: {
    readonly feed: RecordRow;
    readonly mayWriteFeed: boolean;
    /** Whether the feed runs, and so runs on the variance it started with until it starts again. */
    readonly running?: boolean;
    readonly save: SaveVariance;
    /** Stops and starts the feed, so it fits its variance again. */
    readonly restart?: () => Promise<void>;
    /** Re-reads the feed once a save is done, so the next edit is against what was stored. */
    readonly saved?: () => Promise<void>;
}): ReactNode {
    const { t } = useTranslation();
    const [editing, setEditing] = useState(false);
    const settings = settingsOf(feed);
    const garch = settings.model === 'gjr_garch';
    return (
        <section className="space-y-4" aria-label={t('marketdata.variance.title')}>
            <div className="flex flex-wrap items-start justify-between gap-3">
                <p className="text-sm text-ink-muted">{t('marketdata.variance.lead')}</p>
                {mayWriteFeed ? (
                    <Button icon="edit" onClick={() => setEditing(true)}>
                        {t('refdata.records.edit')}
                    </Button>
                ) : null}
            </div>
            <dl className="grid gap-4 sm:grid-cols-2 lg:grid-cols-5">
                <Fact
                    label={t('marketdata.variance.model')}
                    value={t(`marketdata.variance.model.${settings.model}`)}
                />
                <Fact
                    label={t('marketdata.variance.alpha')}
                    value={garch ? String(settings.alpha) : '—'}
                />
                <Fact
                    label={t('marketdata.variance.gamma')}
                    value={garch ? String(settings.gamma) : '—'}
                />
                <Fact
                    label={t('marketdata.variance.beta')}
                    value={garch ? String(settings.beta) : '—'}
                />
                <Fact
                    label={t('marketdata.variance.window')}
                    value={garch ? String(settings.windowDays) : '—'}
                />
            </dl>
            <p className="text-sm text-ink-muted">{t('marketdata.variance.fitted')}</p>
            {editing ? (
                <VarianceDialog
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
 * Edits a feed's daily variance. The shape and the window are read only by a GJR-GARCH
 * variance, so under a constant one they are disabled rather than hidden, and a person
 * switching models sees what will be used. A running feed keeps the variance it started
 * with, so the dialog offers to restart it.
 */
export function VarianceDialog({
    feed,
    running,
    save,
    restart,
    saved,
    onClose,
}: {
    readonly feed: RecordRow;
    readonly running: boolean;
    readonly save: SaveVariance;
    readonly restart?: (() => Promise<void>) | undefined;
    readonly saved?: (() => Promise<void>) | undefined;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const stored = settingsOf(feed);
    const [restartAfter, setRestartAfter] = useState(true);
    const [model, setModel] = useState<Model>(stored.model);
    // What was typed, so a cleared field reads as empty rather than as zero.
    const [alphaText, setAlphaText] = useState(String(stored.alpha));
    const [gammaText, setGammaText] = useState(String(stored.gamma));
    const [betaText, setBetaText] = useState(String(stored.beta));
    const [windowText, setWindowText] = useState(String(stored.windowDays));
    const [saving, setSaving] = useState(false);
    const [error, setError] = useState<string | null>(null);
    const unreadable = (text: string): boolean =>
        text.trim() === '' || !Number.isFinite(Number(text));
    const settings: VarianceSettings = {
        model,
        alpha: Number(alphaText),
        gamma: Number(gammaText),
        beta: Number(betaText),
        windowDays: Number(windowText),
    };
    const garch = model === 'gjr_garch';
    const problems = varianceProblems(settings, show(feed['process_type']));
    const changed =
        settings.model !== stored.model ||
        settings.alpha !== stored.alpha ||
        settings.gamma !== stored.gamma ||
        settings.beta !== stored.beta ||
        settings.windowDays !== stored.windowDays;
    const invalid =
        [alphaText, gammaText, betaText, windowText].some(unreadable) || problems.length > 0;
    const saveNow = async (): Promise<void> => {
        setSaving(true);
        setError(null);
        const outcome = await saveArrivals({
            save: () => save(settings, { reasonCode: REASON_CODE, commentary: '' }),
            saved,
            restart: running && restartAfter ? restart : undefined,
        });
        setSaving(false);
        if (outcome.kind === 'failed') {
            setError(outcome.message);
        } else if (outcome.kind === 'restartFailed') {
            setError(t('marketdata.variance.restartFailed', { message: outcome.message }));
        } else {
            onClose();
        }
    };
    const numberField = (
        key: 'alpha' | 'gamma' | 'beta' | 'window',
        text: string,
        setText: (value: string) => void,
    ): ReactNode => (
        <Field label={t(`marketdata.variance.${key}`)} hint={t(`marketdata.variance.${key}Help`)}>
            <Input
                type="number"
                step={key === 'window' ? 1 : 'any'}
                value={text}
                aria-invalid={unreadable(text) || problems.includes(key) ? true : undefined}
                disabled={!garch}
                onChange={(event) => setText(event.target.value)}
            />
        </Field>
    );
    return (
        <Dialog
            title={t('marketdata.variance.editTitle', { feed: show(feed['source_name']) })}
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
                <Field label={t('marketdata.variance.model')}>
                    <Select
                        value={model}
                        onChange={(event) => setModel(event.target.value as Model)}
                    >
                        {MODELS.map((option) => (
                            <option key={option} value={option}>
                                {t(`marketdata.variance.model.${option}`)}
                            </option>
                        ))}
                    </Select>
                </Field>
                {numberField('window', windowText, setWindowText)}
                {numberField('alpha', alphaText, setAlphaText)}
                {numberField('gamma', gammaText, setGammaText)}
                {numberField('beta', betaText, setBetaText)}
            </div>
            {problems.includes('engine') ? (
                <div className="mt-4">
                    <Notice tone="error">{t('marketdata.variance.engine')}</Notice>
                </div>
            ) : null}
            {running ? (
                <div className="mt-4 space-y-2 rounded-md border border-line p-3 text-sm">
                    <p>{t('marketdata.variance.runningNote')}</p>
                    <label className="flex items-center gap-2">
                        <input
                            type="checkbox"
                            checked={restartAfter}
                            onChange={(event) => setRestartAfter(event.target.checked)}
                        />
                        {t('marketdata.variance.restart')}
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

/** A feed's Variance tab: the panel above, with the save's restart and re-read filled in. */
export function VarianceTab({
    feed,
    mayWriteFeed,
    running,
    save,
}: {
    readonly feed: RecordRow;
    readonly mayWriteFeed: boolean;
    readonly running: boolean;
    readonly save: SaveVariance;
}): ReactNode {
    const queries = useQueryClient();
    return (
        <VariancePanel
            feed={feed}
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

/** A feed's stored variance, with the table's defaults where a field is missing. */
export function settingsOf(feed: RecordRow): VarianceSettings {
    const value = show(feed['variance_model']);
    return {
        model: (MODELS as readonly string[]).includes(value) ? (value as Model) : 'constant',
        alpha: numberOf(feed, 'garch_alpha', 0.05),
        gamma: numberOf(feed, 'garch_gamma', 0),
        beta: numberOf(feed, 'garch_beta', 0.9),
        windowDays: numberOf(feed, 'garch_window_days', 1826),
    };
}

function numberOf(feed: RecordRow, field: string, fallback: number): number {
    const raw = feed[field];
    if (raw === null || raw === undefined || raw === '') {
        return fallback;
    }
    const value = Number(raw);
    return Number.isFinite(value) ? value : fallback;
}

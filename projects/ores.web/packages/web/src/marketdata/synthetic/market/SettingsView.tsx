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

import { useState, type ReactNode } from 'react';
import type { MarketRegime, MarketSettings } from '../../../api/client.js';
import { useTranslation } from '../../../i18n/Provider.js';
import { Button, cx, Notice } from '../../../ui/Primitives.js';
import { FactRows, MarketCard } from './MarketCard.js';
import { clean, duration } from './numbers.js';

/**
 * The six numbers as a person edits them. The regime's two are spells in seconds, which is
 * how a person thinks of them; the service stores their rates, one over the spell.
 */
export const SETTING_KEYS = [
    'calmSpell',
    'activeSpell',
    'jump',
    'decay',
    'speed',
    'tailLoading',
] as const;
export type SettingKey = (typeof SETTING_KEYS)[number];
export type SettingValues = Readonly<Record<SettingKey, number>>;

/** The largest tail loading the service takes. */
export const MAX_TAIL_LOADING = 10;

export function settingsOf(regime: MarketRegime): SettingValues {
    return {
        calmSpell: regime.wakeRate > 0 ? 1 / regime.wakeRate : 0,
        activeSpell: regime.calmRate > 0 ? 1 / regime.calmRate : 0,
        jump: regime.jump,
        decay: regime.decay,
        speed: regime.speed,
        tailLoading: regime.tailLoading,
    };
}

export function toSettings(values: SettingValues): MarketSettings {
    return {
        wakeRate: 1 / values.calmSpell,
        calmRate: 1 / values.activeSpell,
        jump: values.jump,
        decay: values.decay,
        speed: values.speed,
        tailLoading: values.tailLoading,
    };
}

/** What is wrong with a setting, as a key and the numbers its sentence names. */
export interface Problem {
    readonly key: string;
    readonly values?: Readonly<Record<string, string>>;
}

/**
 * The rules the service applies, run as a person types, each naming the number as the
 * service's refusal does. The service still decides: a set it refuses comes back with its
 * own words.
 */
export function settingProblems(
    values: SettingValues,
    unreadable: ReadonlySet<SettingKey>,
    activeMultiplier: number,
): Partial<Record<SettingKey, Problem>> {
    const out: Partial<Record<SettingKey, Problem>> = {};
    for (const key of unreadable) {
        out[key] = { key: 'notANumber' };
    }
    const positive = (key: SettingKey): void => {
        if (out[key] === undefined && !(values[key] > 0)) {
            out[key] = { key: 'positive' };
        }
    };
    positive('calmSpell');
    positive('activeSpell');
    positive('decay');
    positive('speed');
    if (out.jump === undefined && values.jump < 0) {
        out.jump = { key: 'notNegative' };
    }
    if (
        out.tailLoading === undefined &&
        (values.tailLoading < 0 || values.tailLoading > MAX_TAIL_LOADING)
    ) {
        out.tailLoading = { key: 'between', values: { most: String(MAX_TAIL_LOADING) } };
    }
    if (out.jump === undefined && out.decay === undefined && values.jump / values.decay >= 1) {
        out.decay = { key: 'gain', values: { gain: clean(values.jump / values.decay, 3) } };
    }
    if (out.calmSpell === undefined && out.activeSpell === undefined && activeMultiplier > 0) {
        const share = values.activeSpell / (values.calmSpell + values.activeSpell);
        if (share * activeMultiplier >= 1) {
            out.activeSpell = {
                key: 'share',
                values: {
                    share: clean(share * 100, 3),
                    multiplier: clean(activeMultiplier),
                    most: clean(100 / activeMultiplier, 3),
                },
            };
        }
    }
    return out;
}

function SettingField({
    name,
    label,
    value,
    was,
    implies,
    impliesLabel,
    problem,
    readOnly,
    onChange,
}: {
    readonly name: SettingKey;
    readonly label: string;
    readonly value: string;
    readonly was: string | undefined;
    readonly implies: string;
    readonly impliesLabel: string;
    readonly problem: string | undefined;
    readonly readOnly: boolean;
    readonly onChange: (value: string) => void;
}): ReactNode {
    const id = `market-setting-${name}`;
    return (
        <div className="grid gap-1.5 rounded-lg border border-line-subtle bg-bg-primary px-3 py-2.5">
            <label htmlFor={id} className="font-mono text-xs font-semibold text-ink-muted">
                {label}
            </label>
            <input
                id={id}
                inputMode="decimal"
                value={value}
                readOnly={readOnly}
                aria-invalid={problem === undefined ? undefined : true}
                onChange={(event) => onChange(event.target.value)}
                className={cx(
                    'w-full rounded-md border px-2.5 py-1.5 font-mono text-sm tabular-nums',
                    readOnly
                        ? 'border-transparent bg-transparent px-0'
                        : 'border-line bg-surface-base focus:border-accent focus:outline-none',
                    problem !== undefined && 'border-down',
                    problem === undefined && was !== undefined && 'border-stale',
                )}
            />
            <div className="flex justify-between gap-2 font-mono text-[11px] text-ink-faint">
                <span>{impliesLabel}</span>
                <b
                    className={cx(
                        'text-right font-semibold',
                        problem === undefined ? 'text-accent' : 'text-down',
                    )}
                >
                    {problem ?? implies}
                </b>
            </div>
            {was === undefined ? null : (
                <span className="font-mono text-[10px] text-stale">{was}</span>
            )}
        </div>
    );
}

/**
 * The Settings tab: what moves the market, in four cards. A system session edits and
 * applies; a tenant reads the same numbers. The change is shown before it is made, and the
 * service's own rules are applied as a person types.
 */
export function SettingsView({
    regime,
    mayChange,
    pending,
    error,
    onApply,
}: {
    readonly regime: MarketRegime;
    readonly mayChange: boolean;
    readonly pending: boolean;
    readonly error: string | null;
    readonly onApply: (settings: MarketSettings) => void;
}): ReactNode {
    const { t } = useTranslation();
    const [draft, setDraft] = useState<Partial<Record<SettingKey, string>>>({});
    const held = settingsOf(regime);
    const text = (key: SettingKey): string => draft[key] ?? clean(held[key]);
    const values = Object.fromEntries(
        SETTING_KEYS.map((key) => [key, Number(text(key))]),
    ) as SettingValues;
    const unreadable = new Set(
        SETTING_KEYS.filter((key) => text(key).trim() === '' || !Number.isFinite(values[key])),
    );
    const activeMultiplier = regime.activeMultiplier;
    const problems = settingProblems(values, unreadable, activeMultiplier);
    const say = (key: SettingKey): string | undefined => {
        const problem = problems[key];
        return problem === undefined
            ? undefined
            : t(`marketdata.market.settings.problems.${problem.key}`, problem.values);
    };
    // A draft stays after a save, so a refused one can be corrected; once the service holds the
    // same numbers there is nothing left to apply.
    const changed = SETTING_KEYS.filter(
        (key) => draft[key] !== undefined && clean(values[key]) !== clean(held[key]),
    );
    const wasOf = (key: SettingKey): string | undefined =>
        changed.includes(key)
            ? t('marketdata.market.settings.was', { value: clean(held[key]) })
            : undefined;
    const field = (key: SettingKey, implies: string): ReactNode => (
        <SettingField
            name={key}
            label={t(`marketdata.market.settings.fields.${key}`)}
            value={text(key)}
            was={wasOf(key)}
            implies={implies}
            impliesLabel={t(`marketdata.market.settings.implies.${key}`)}
            problem={say(key)}
            readOnly={!mayChange}
            onChange={(next) => setDraft((was) => ({ ...was, [key]: next }))}
        />
    );
    const share = values.activeSpell / (values.calmSpell + values.activeSpell);
    const calm = activeMultiplier > 0 ? (1 - share * activeMultiplier) / (1 - share) : Number.NaN;
    const gain = values.jump / values.decay;
    const bad = Object.keys(problems).length > 0;
    return (
        <div className="grid gap-4">
            <MarketCard
                glyph="settings"
                title={t('marketdata.market.settings.title')}
                lead={t('marketdata.market.settings.lead')}
                action={
                    mayChange ? (
                        <>
                            <Button
                                size="sm"
                                variant="secondary"
                                disabled={Object.keys(draft).length === 0 || pending}
                                onClick={() => setDraft({})}
                            >
                                {t('marketdata.market.settings.discard')}
                            </Button>
                            <Button
                                size="sm"
                                variant="primary"
                                pending={pending}
                                disabled={changed.length === 0 || bad}
                                onClick={() => onApply(toSettings(values))}
                            >
                                {t('marketdata.market.settings.apply')}
                            </Button>
                        </>
                    ) : undefined
                }
            >
                {changed.length === 0 ? null : (
                    <div
                        className="flex flex-wrap gap-1.5"
                        aria-label={t('marketdata.market.settings.changes')}
                    >
                        {changed.map((key) => (
                            <span
                                key={key}
                                className="rounded-chip border border-stale/35 bg-stale/8 px-2 py-0.5 font-mono text-[11px] text-stale"
                            >
                                {t('marketdata.market.settings.change', {
                                    name: t(`marketdata.market.settings.fields.${key}`),
                                    from: clean(held[key]),
                                    to: clean(values[key]),
                                })}
                            </span>
                        ))}
                    </div>
                )}
                {error === null ? null : <Notice tone="error">{error}</Notice>}
                <p className="text-[13px] text-ink-faint">
                    {mayChange
                        ? t('marketdata.market.settings.applies')
                        : t('marketdata.market.settings.readOnly')}
                </p>
            </MarketCard>

            <div className="grid gap-4 lg:grid-cols-2">
                <MarketCard
                    title={t('marketdata.market.settings.regime.title')}
                    lead={t('marketdata.market.settings.regime.lead')}
                >
                    <div className="grid gap-3 sm:grid-cols-2">
                        {field('calmSpell', `${clean(1 / values.calmSpell)} /s`)}
                        {field('activeSpell', `${clean(1 / values.activeSpell)} /s`)}
                    </div>
                    <FactRows
                        rows={[
                            [
                                t('marketdata.market.settings.regime.share'),
                                `${clean(share * 100, 3)}%`,
                            ],
                            [
                                t('marketdata.market.settings.regime.active'),
                                activeMultiplier > 0
                                    ? t('marketdata.market.settings.regime.fixed', {
                                          multiplier: clean(activeMultiplier),
                                      })
                                    : '—',
                            ],
                            [
                                t('marketdata.market.settings.regime.calm'),
                                calm > 0
                                    ? `×${clean(calm, 3)}`
                                    : t('marketdata.market.settings.regime.noCalm'),
                            ],
                            [
                                t('marketdata.market.settings.regime.mean'),
                                t('marketdata.market.settings.regime.one'),
                            ],
                        ]}
                    />
                </MarketCard>

                <MarketCard
                    title={t('marketdata.market.settings.heat.title')}
                    lead={t('marketdata.market.settings.heat.lead')}
                >
                    <div className="grid gap-3 sm:grid-cols-2">
                        {field('jump', clean(gain, 3))}
                        {field('decay', duration(Math.LN2 / values.decay))}
                    </div>
                    <div className="grid gap-1">
                        <div
                            aria-hidden
                            className="relative h-2 overflow-hidden rounded-full border border-line bg-bg-primary"
                        >
                            <span
                                className="absolute inset-y-0 left-0 bg-gradient-to-r from-up via-warn to-down"
                                style={{
                                    width: `${String(Math.min(100, Math.max(0, gain * 100)))}%`,
                                }}
                            />
                        </div>
                        <div className="flex justify-between font-mono text-[11px] text-ink-faint">
                            <span>0</span>
                            <span>
                                {t(
                                    gain < 1
                                        ? 'marketdata.market.settings.heat.settles'
                                        : 'marketdata.market.settings.heat.runsAway',
                                    { gain: clean(gain, 3) },
                                )}
                            </span>
                            <span>1</span>
                        </div>
                    </div>
                </MarketCard>

                <MarketCard
                    title={t('marketdata.market.settings.time.title')}
                    lead={t('marketdata.market.settings.time.lead')}
                >
                    {field('speed', duration(60 * values.speed))}
                    <p className="text-[13px] text-ink-faint">
                        {t('marketdata.marketRegime.settings.speedNote')}
                    </p>
                </MarketCard>

                <MarketCard
                    title={t('marketdata.market.settings.tails.title')}
                    lead={t('marketdata.market.settings.tails.lead')}
                >
                    {field(
                        'tailLoading',
                        clean(
                            values.tailLoading *
                                Math.log(activeMultiplier > 0 ? activeMultiplier : 2),
                            3,
                        ),
                    )}
                    <FactRows
                        rows={[
                            [
                                t('marketdata.market.settings.tails.active'),
                                clean(
                                    values.tailLoading *
                                        Math.log(activeMultiplier > 0 ? activeMultiplier : 2),
                                    3,
                                ),
                            ],
                            [
                                t('marketdata.market.settings.tails.calm'),
                                calm > 0 ? clean(values.tailLoading * Math.log(calm), 3) : '—',
                            ],
                            [
                                t('marketdata.market.settings.tails.what'),
                                t('marketdata.market.settings.tails.whatValue'),
                            ],
                        ]}
                    />
                </MarketCard>
            </div>
        </div>
    );
}

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

/*
 * The parts the operations screens share: the way back to the operations area,
 * the state of one service instance, and the panel that records what a screen
 * cannot show yet.
 *
 * The screens are an area like Tenants or Rescue, not a set of tabs: a person
 * enters the area, opens one journey, and comes back. The header carries the
 * one action that belongs to every journey screen, as the tenant detail
 * carries its way back to the roster.
 */

import type { ReactNode } from 'react';
import type { ServiceRosterSlot } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { LinkButton, Tag } from '../ui/Primitives.js';

/** The way back to the operations area, for a screen's header actions. */
export function OperationsBack(): ReactNode {
    const { t } = useTranslation();
    return (
        <LinkButton to="/operations" variant="secondary">
            {t('operations.back')}
        </LinkButton>
    );
}

/**
 * The states a roster slot can be in, as the roster read emits them: an
 * instance that reported within the running window, one that reported before
 * it, and a slot no instance fills.
 *
 * The words are the read's own, from
 * ores.telemetry.core/domain/service_state.hpp. The tuple is the union a new
 * state has to join, so a state the read gains is a compile error here rather
 * than a tone nobody chose.
 */
export const SERVICE_STATES = ['running', 'lost', 'missing'] as const;
export type ServiceState = (typeof SERVICE_STATES)[number];

/** Fails the build when a state has no tone, and the run if one slips through. */
function neverReached(state: never): never {
    throw new Error(`Unhandled service state: ${String(state)}`);
}

/** The tone one state is painted with, for every state the read emits. */
function stateTone(state: ServiceState): 'accent' | 'muted' | 'warn' {
    switch (state) {
        case 'running':
            return 'accent';
        case 'lost':
            return 'muted';
        case 'missing':
            return 'warn';
        default:
            return neverReached(state);
    }
}

/** The state the read emitted, or nothing for a word it does not emit. */
function knownState(state: string): ServiceState | undefined {
    return SERVICE_STATES.find((known) => known === state);
}

/** The state label of one instance, in the installation's own words. */
export function InstanceStateTag({
    state,
}: {
    readonly state: ServiceRosterSlot['state'];
}): ReactNode {
    const { t } = useTranslation();
    const known = knownState(state);
    if (known === undefined) {
        /*
         * A word the read does not emit is a broken contract, so the screen
         * repeats it rather than choosing a tone it cannot justify.
         */
        return <Tag tone="warn">{state}</Tag>;
    }
    return <Tag tone={stateTone(known)}>{t(`operations.instance.state.${known}`)}</Tag>;
}

/** The release an instance runs, warned when it trails the newest running one. */
export function InstanceVersion({
    instance,
    newestVersion,
}: {
    readonly instance: ServiceRosterSlot;
    readonly newestVersion: string | undefined;
}): ReactNode {
    const { t } = useTranslation();
    if (instance.version === null || instance.version === '') {
        return <span className="font-mono text-ink-faint">—</span>;
    }
    return (
        <span className="flex items-center gap-2">
            <span className="font-mono">{instance.version}</span>
            {instance.state === 'running' && instance.version !== newestVersion && (
                <Tag tone="warn">{t('operations.instance.olderBuild')}</Tag>
            )}
        </span>
    );
}

/** The newest release among the running instances. */
export function newestVersionOf(running: readonly ServiceRosterSlot[]): string | undefined {
    return running
        .map((instance) => instance.version)
        .filter((version): version is string => version !== null && version !== '')
        .reduce<string | undefined>(
            (newest, version) => (newest === undefined || version > newest ? version : newest),
            undefined,
        );
}

export interface ScreenGap {
    readonly title: string;
    readonly body: string;
    /** The journey that records this gap, named as the reader knows it. */
    readonly journey: string;
}

export function GapPanel({ gaps }: { readonly gaps: readonly ScreenGap[] }): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-3 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('operations.gaps.title')}</h2>
                <span className="text-xs text-ink-faint">{t('operations.gaps.lead')}</span>
            </header>
            <dl className="space-y-3 text-sm">
                {gaps.map((gap) => (
                    <div key={gap.title} className="space-y-1">
                        <dt className="font-medium text-ink">{gap.title}</dt>
                        <dd className="text-ink-muted">{gap.body}</dd>
                        <dd className="text-xs text-ink-faint">
                            {t('operations.gaps.recordedBy', { journey: gap.journey })}
                        </dd>
                    </div>
                ))}
            </dl>
        </section>
    );
}

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

/**
 * The window within which a heartbeat keeps an instance running, in minutes.
 *
 * The server owns the number: `service_running_window`, in
 * projects/ores.telemetry/database/include/ores.telemetry.database/repository/telemetry_repository.hpp.
 * The roster reply does not carry the window, so the web cannot derive it and
 * restates it here. The reply should carry it, and then this copy can go.
 */
export const SERVICE_RUNNING_WINDOW_MINUTES = 5;

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

/**
 * The release and the state a version cell reads.
 *
 * The services screen carries them on a roster row and the grid screen on a
 * node row, so the cell states which fields it needs rather than which table
 * the row came from.
 */
export interface VersionedInstance {
    readonly version: string | null;
    readonly state: string;
}

/** The release an instance runs, warned when it trails the newest running one. */
export function InstanceVersion({
    instance,
    newestVersion,
}: {
    readonly instance: VersionedInstance;
    readonly newestVersion: string | undefined;
}): ReactNode {
    const { t } = useTranslation();
    if (instance.version === null || instance.version === '') {
        return <span className="font-mono text-ink-faint">—</span>;
    }
    return (
        <span className="flex items-center gap-2">
            <span className="font-mono">{instance.version}</span>
            {instance.state === 'running' &&
                newestVersion !== undefined &&
                !sameVersion(instance.version, newestVersion) && (
                    <Tag tone="warn">{t('operations.instance.olderBuild')}</Tag>
                )}
        </span>
    );
}

/** The numeric components of a release string, with a leading `v` removed. */
function versionComponents(version: string): readonly number[] {
    return version
        .replace(/^v/i, '')
        .split('.')
        .map((component) => {
            const value = Number.parseInt(component, 10);
            return Number.isNaN(value) ? 0 : value;
        });
}

/**
 * Orders two release strings by their dotted numbers, oldest first.
 *
 * A string comparison is not a release comparison: it puts `v0.0.9` above
 * `v0.0.10` and `v0.9.0` above `v0.10.0`, which names the wrong newest release
 * and warns about the wrong rows. A leading `v` is optional, and a component
 * one string omits counts as zero, so `v1.2` and `v1.2.0` compare equal.
 */
export function compareVersions(left: string, right: string): number {
    const a = versionComponents(left);
    const b = versionComponents(right);
    const length = Math.max(a.length, b.length);
    for (let index = 0; index < length; index += 1) {
        const difference = (a[index] ?? 0) - (b[index] ?? 0);
        if (difference !== 0) {
            return difference;
        }
    }
    return 0;
}

/** Whether two release strings name the same release, however each is spelled. */
export function sameVersion(left: string, right: string): boolean {
    return compareVersions(left, right) === 0;
}

/** The newest release among the running instances. */
export function newestVersionOf(
    running: readonly { readonly version: string | null }[],
): string | undefined {
    return running
        .map((instance) => instance.version)
        .filter((version): version is string => version !== null && version !== '')
        .reduce<string | undefined>(
            (newest, version) =>
                newest === undefined || compareVersions(version, newest) > 0 ? version : newest,
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

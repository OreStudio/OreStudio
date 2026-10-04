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
 * PROTOTYPE. Throwaway. Delete with the branch.
 *
 * The parts the five operations screens share: the way back to the operations
 * area, and the panel that records what each screen cannot show yet.
 *
 * The screens are an area like Tenants or Rescue, not a set of tabs: a person
 * enters the area, opens one journey, and comes back. The header carries the
 * one action that belongs to every journey screen, as the tenant detail
 * carries its way back to the roster.
 */

import type { ReactNode } from 'react';
import { LinkButton, Tag } from '../ui/Primitives.js';
import type { PrototypeServiceInstance } from './fixtures.js';

/** The way back to the operations area, for a screen's header actions. */
export function OperationsBack(): ReactNode {
    return (
        <LinkButton to="/prototype" variant="secondary">
            Back to operations
        </LinkButton>
    );
}

/** The state label of one instance, in the installation's own words. */
export function InstanceStateTag({
    state,
}: {
    readonly state: PrototypeServiceInstance['state'];
}): ReactNode {
    if (state === 'running') {
        return <Tag tone="accent">running</Tag>;
    }
    if (state === 'stopped') {
        return <Tag tone="muted">stopped</Tag>;
    }
    return <Tag tone="warn">missing</Tag>;
}

/** The release an instance runs, warned when it trails the newest running one. */
export function InstanceVersion({
    instance,
    newestVersion,
}: {
    readonly instance: PrototypeServiceInstance;
    readonly newestVersion: string | undefined;
}): ReactNode {
    if (instance.version === undefined) {
        return <span className="font-mono text-ink-faint">—</span>;
    }
    return (
        <span className="flex items-center gap-2">
            <span className="font-mono">{instance.version}</span>
            {instance.state === 'running' && instance.version !== newestVersion && (
                <Tag tone="warn">older build</Tag>
            )}
        </span>
    );
}

/** The newest release among the running instances. */
export function newestVersionOf(
    running: readonly PrototypeServiceInstance[],
): string | undefined {
    return running
        .map((instance) => instance.version)
        .filter((version): version is string => version !== undefined)
        .reduce<string | undefined>(
            (newest, version) => (newest === undefined || version > newest ? version : newest),
            undefined,
        );
}

export interface ScreenGap {
    readonly title: string;
    readonly body: string;
}

export function GapPanel({ gaps }: { readonly gaps: readonly ScreenGap[] }): ReactNode {
    return (
        <section className="card space-y-3 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">Not on this screen yet</h2>
                <span className="text-xs text-ink-faint">
                    each gap names the journey that records it
                </span>
            </header>
            <dl className="space-y-3 text-sm">
                {gaps.map((gap) => (
                    <div key={gap.title} className="space-y-1">
                        <dt className="font-medium text-ink">{gap.title}</dt>
                        <dd className="text-ink-muted">{gap.body}</dd>
                    </div>
                ))}
            </dl>
        </section>
    );
}

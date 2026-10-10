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

import type { ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { Select } from '../ui/Primitives.js';
import { MAIN_ACTIONS, areaWildcard, covers, type Area } from './catalogue.js';

/**
 * The catalogue by area: what, then Read, Write and Delete, then any other
 * action. It shows what a set of codes grants, and when it is given a toggle
 * it lets a person tick what a role allows, an area at a time if they like.
 *
 * Each area is a disclosure, so a catalogue of nearly nine hundred codes opens
 * as a short list of areas and the person opens the one they mean.
 */
export function PermissionAreas({
    areas,
    granted,
    onlyGranted = false,
    filter = '',
    window,
    onToggle,
    explain,
}: {
    readonly areas: readonly Area[];
    readonly granted: ReadonlySet<string>;
    /** Whether to leave out what the set does not grant. */
    readonly onlyGranted?: boolean;
    readonly filter?: string;
    /**
     * The resources to draw, as =component::resource=, when the caller pages
     * them. The area's counts still cover the whole area, so a page of rows does
     * not change what an area says it holds.
     */
    readonly window?: ReadonlySet<string>;
    /** Present when the person may change the set. */
    readonly onToggle?: (code: string, on: boolean) => void;
    /** Names the roles behind a granted code, shown as its hover text. */
    readonly explain?: (code: string) => string;
}): ReactNode {
    const { t } = useTranslation();
    const needle = filter.trim().toLowerCase();
    const open = needle !== '' || onlyGranted;

    const shown = areas
        .map((area) => {
            const areaName = areaLabel(t, area.component);
            const resources = area.resources.filter((resource) => {
                if (
                    needle !== '' &&
                    !resource.name.replace(/_/g, ' ').includes(needle) &&
                    !area.component.includes(needle) &&
                    !areaName.toLowerCase().includes(needle)
                )
                    return false;
                if (window !== undefined && !window.has(`${area.component}::${resource.name}`))
                    return false;
                if (onlyGranted)
                    return resource.actions.some((action) =>
                        covers(granted, `${area.component}::${resource.name}:${action}`),
                    );
                return true;
            });
            return { area, areaName, resources };
        })
        .filter(({ resources }) => resources.length > 0);

    if (shown.length === 0) {
        return <p className="text-sm text-ink-muted">{t('access.nothingMatches')}</p>;
    }

    return (
        <div className="space-y-2">
            {shown.map(({ area, areaName, resources }) => {
                const whole = granted.has('*') || granted.has(areaWildcard(area.component));
                const held = area.resources.reduce(
                    (sum, resource) =>
                        sum +
                        resource.actions.filter((action) =>
                            covers(granted, `${area.component}::${resource.name}:${action}`),
                        ).length,
                    0,
                );
                return (
                    <details
                        key={area.component}
                        open={open}
                        className="rounded-md border border-line bg-surface-raised"
                    >
                        <summary className="flex cursor-pointer items-center gap-3 px-4 py-2.5 text-sm">
                            <span className="min-w-0 flex-1">
                                <span className="font-medium text-ink">{areaName}</span>{' '}
                                <span className="font-mono text-xs text-ink-faint">
                                    {area.component}
                                </span>
                            </span>
                            {onToggle !== undefined && (
                                <label
                                    className="flex items-center gap-1.5 text-xs text-ink-muted"
                                    onClick={(event) => event.stopPropagation()}
                                >
                                    <input
                                        type="checkbox"
                                        checked={granted.has(areaWildcard(area.component))}
                                        onChange={(event) =>
                                            onToggle(
                                                areaWildcard(area.component),
                                                event.target.checked,
                                            )
                                        }
                                    />
                                    {t('access.allOfIt')}
                                </label>
                            )}
                            <span className="text-xs tabular-nums text-ink-muted">
                                {whole
                                    ? t('access.allCount', { count: String(area.size) })
                                    : t('access.someCount', {
                                          held: String(held),
                                          count: String(area.size),
                                      })}
                            </span>
                        </summary>
                        <div className="overflow-x-auto border-t border-line">
                            <table className="w-full table-fixed text-left text-sm">
                                <thead>
                                    <tr className="border-b border-line text-xs text-ink-muted">
                                        <th className="px-4 py-1.5 font-medium">
                                            {t('access.what')}
                                        </th>
                                        {MAIN_ACTIONS.map((action) => (
                                            <th
                                                key={action}
                                                className="w-20 px-2 py-1.5 text-center font-medium"
                                            >
                                                {t(`access.action.${action}`)}
                                            </th>
                                        ))}
                                        <th className="w-64 px-4 py-1.5 font-medium">
                                            {t('access.other')}
                                        </th>
                                    </tr>
                                </thead>
                                <tbody>
                                    {resources.map((resource) => {
                                        const code = (action: string) =>
                                            `${area.component}::${resource.name}:${action}`;
                                        const cell = (action: string) => {
                                            const has = covers(granted, code(action));
                                            if (onToggle === undefined)
                                                return has ? (
                                                    <span
                                                        className="font-bold text-up"
                                                        title={explain?.(code(action))}
                                                        aria-label={t('access.allowed')}
                                                    >
                                                        ✓
                                                    </span>
                                                ) : (
                                                    <span className="text-ink-faint">–</span>
                                                );
                                            return (
                                                <input
                                                    type="checkbox"
                                                    checked={has}
                                                    disabled={whole}
                                                    aria-label={`${resource.name} ${action}`}
                                                    onChange={(event) =>
                                                        onToggle(code(action), event.target.checked)
                                                    }
                                                />
                                            );
                                        };
                                        const others = resource.actions.filter(
                                            (action) =>
                                                !(MAIN_ACTIONS as readonly string[]).includes(
                                                    action,
                                                ),
                                        );
                                        return (
                                            <tr
                                                key={resource.name}
                                                className="border-b border-line-subtle last:border-b-0"
                                            >
                                                <td className="px-4 py-1.5 font-mono text-xs">
                                                    {resource.name.replace(/_/g, ' ')}
                                                </td>
                                                {MAIN_ACTIONS.map((action) => (
                                                    <td
                                                        key={action}
                                                        className="px-2 py-1.5 text-center"
                                                    >
                                                        {resource.actions.includes(action) ? (
                                                            cell(action)
                                                        ) : (
                                                            <span className="text-line">·</span>
                                                        )}
                                                    </td>
                                                ))}
                                                <td className="px-4 py-1.5 text-xs text-ink-muted">
                                                    {others.map((action) => (
                                                        <span
                                                            key={action}
                                                            className="mr-3 inline-flex items-center gap-1"
                                                        >
                                                            {cell(action)}{' '}
                                                            {action.replace(/_/g, ' ')}
                                                        </span>
                                                    ))}
                                                </td>
                                            </tr>
                                        );
                                    })}
                                </tbody>
                            </table>
                        </div>
                    </details>
                );
            })}
        </div>
    );
}

/**
 * The resources a filter would draw, in the order they are drawn, as
 * =component::resource=. A caller that pages the rows pages this list and hands
 * the page back as the window.
 */
export function permissionRows(
    areas: readonly Area[],
    granted: ReadonlySet<string>,
    options: {
        readonly onlyGranted: boolean;
        readonly filter: string;
        readonly areaName: (component: string) => string;
    },
): readonly string[] {
    const needle = options.filter.trim().toLowerCase();
    return areas.flatMap((area) =>
        area.resources
            .filter((resource) => {
                if (
                    needle !== '' &&
                    !resource.name.replace(/_/g, ' ').includes(needle) &&
                    !area.component.includes(needle) &&
                    !options.areaName(area.component).toLowerCase().includes(needle)
                )
                    return false;
                if (options.onlyGranted)
                    return resource.actions.some((action) =>
                        covers(granted, `${area.component}::${resource.name}:${action}`),
                    );
                return true;
            })
            .map((resource) => `${area.component}::${resource.name}`),
    );
}

/** An area's name in the person's language, or its code when it has none. */
export function areaLabel(t: (key: string) => string, component: string): string {
    const key = `access.area.${component}`;
    const label = t(key);
    return label === key ? component : label;
}

/**
 * The combo box that narrows a list to one area, such as reference data.
 *
 * The areas are named in the person's language and sorted by that name, so the
 * list reads as words and not as the catalogue's size order. An empty value
 * means every area.
 */
export function AreaFilter({
    areas,
    value,
    onChange,
}: {
    readonly areas: readonly Area[];
    readonly value: string;
    readonly onChange: (component: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const named = areas
        .map((area) => ({ component: area.component, name: areaLabel(t, area.component) }))
        .sort((a, b) => a.name.localeCompare(b.name));
    return (
        <Select
            className="w-56"
            value={value}
            onChange={(event) => onChange(event.target.value)}
            aria-label={t('access.areaFilter')}
        >
            <option value="">{t('access.allAreas')}</option>
            {named.map(({ component, name }) => (
                <option key={component} value={component}>
                    {name}
                </option>
            ))}
        </Select>
    );
}

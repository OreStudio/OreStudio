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

import { useQuery } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import type { HistoryVersion } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Notice, Tag } from '../ui/Primitives.js';

/**
 * The provenance fields the server's history mapper adds to every version.
 * They say who changed a record, when and why, so the timeline shows them and
 * the diff leaves them out: they change in every version by construction.
 */
const PROVENANCE = new Set([
    'Modified By',
    'Performed By',
    'Change Reason Code',
    'Change Commentary',
    'Recorded At',
]);

/** The value of one field in a version, by the name the server's history mapper gives it. */
export function fieldValue(version: HistoryVersion, name: string): string {
    return version.fields.find((field) => field.name === name)?.value ?? '';
}

/**
 * A changed value split into its common prefix, its changed middle and its
 * common suffix, so the middle can be marked on the old line and on the new.
 */
function split(
    before: string,
    after: string,
): {
    readonly old: readonly [string, string, string];
    readonly new: readonly [string, string, string];
} {
    let prefix = 0;
    while (prefix < before.length && prefix < after.length && before[prefix] === after[prefix]) {
        prefix += 1;
    }
    let suffix = 0;
    while (
        suffix < before.length - prefix &&
        suffix < after.length - prefix &&
        before[before.length - 1 - suffix] === after[after.length - 1 - suffix]
    ) {
        suffix += 1;
    }
    const parts = (text: string): readonly [string, string, string] => [
        text.slice(0, prefix),
        text.slice(prefix, text.length - suffix),
        text.slice(text.length - suffix),
    ];
    return { old: parts(before), new: parts(after) };
}

function DiffLine({
    sign,
    parts,
    tone,
}: {
    readonly sign: string;
    readonly parts: readonly [string, string, string];
    readonly tone: 'old' | 'new';
}): ReactNode {
    const line = tone === 'old' ? 'bg-down/15' : 'bg-up/15';
    const mark = tone === 'old' ? 'bg-down/45' : 'bg-up/45';
    return (
        <div className={`grid grid-cols-[1.25rem_1fr] rounded px-2 py-0.5 ${line}`}>
            <span aria-hidden className="text-ink-faint select-none">
                {sign}
            </span>
            <span>
                {parts[0]}
                {parts[1] !== '' && (
                    <mark className={`rounded-sm text-inherit ${mark}`}>{parts[1]}</mark>
                )}
                {parts[2]}
            </span>
        </div>
    );
}

/**
 * Every version of one record and what changed between any two of them.
 *
 * The timeline lists the versions, newest first, each with who modified and
 * performed it, its reason and its commentary. The comparison shows each
 * changed field as an old line and a new line with the changed characters
 * marked, the way a code review shows a change. Reverting is the caller's: it
 * is a write of the record the caller owns, offered for a version older than
 * the current one.
 */
export function HistoryPanel({
    entityType,
    entityId,
    onRevert,
}: {
    readonly entityType: string;
    readonly entityId: string;
    readonly onRevert?: (version: HistoryVersion) => void;
}): ReactNode {
    const { t } = useTranslation();
    const [toVersion, setToVersion] = useState<number | null>(null);
    const [fromVersion, setFromVersion] = useState<number | null>(null);
    const [onlyChanges, setOnlyChanges] = useState(false);
    const history = useQuery({
        queryKey: ['history', entityType, entityId],
        queryFn: () => api.history(entityType, entityId),
    });

    if (history.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (history.isError) {
        return <Notice tone="error">{history.error.message}</Notice>;
    }
    const versions = history.data;
    const newest = versions[0];
    if (newest === undefined) {
        return <p className="text-sm text-ink-muted">{t('history.empty')}</p>;
    }
    const to = versions.find((version) => version.version === toVersion) ?? newest;
    const fromNumber =
        fromVersion !== null && fromVersion < to.version ? fromVersion : to.version - 1;
    const from = versions.find((version) => version.version === fromNumber);
    const data = (version: HistoryVersion) =>
        version.fields.filter((field) => !PROVENANCE.has(field.name));

    return (
        <section className="grid overflow-hidden rounded-md border border-line md:grid-cols-[18rem_1fr]">
            <ol
                aria-label={t('history.timeline')}
                className="border-b border-line md:border-r md:border-b-0"
            >
                {versions.map((version) => {
                    const note = fieldValue(version, 'Change Commentary');
                    return (
                        <li key={version.version}>
                            <button
                                type="button"
                                aria-current={version.version === to.version ? 'true' : undefined}
                                className={
                                    version.version === to.version
                                        ? 'grid w-full gap-0.5 border-t border-line-subtle bg-accent/10 px-4 py-2.5 text-left first:border-t-0'
                                        : 'grid w-full gap-0.5 border-t border-line-subtle px-4 py-2.5 text-left first:border-t-0 hover:bg-surface-hover'
                                }
                                onClick={() => {
                                    setToVersion(version.version);
                                    setFromVersion(null);
                                }}
                            >
                                <span className="flex items-center gap-2 text-sm font-semibold">
                                    v{version.version} · {version.recordedAt}
                                    {version.version === newest.version && (
                                        <Tag tone="accent">{t('history.current')}</Tag>
                                    )}
                                </span>
                                <span className="text-xs text-ink-muted">
                                    {t('history.modifiedBy', { who: version.modifiedBy })}
                                </span>
                                <span className="text-xs text-ink-faint">
                                    {t('history.performedBy', {
                                        who: fieldValue(version, 'Performed By'),
                                    })}
                                </span>
                                <span>
                                    <span className="inline-block rounded border border-warn/40 bg-warn/10 px-1.5 font-mono text-[11px] text-warn">
                                        {fieldValue(version, 'Change Reason Code')}
                                    </span>
                                </span>
                                {note !== '' && (
                                    <span className="text-xs text-ink-muted italic">“{note}”</span>
                                )}
                            </button>
                        </li>
                    );
                })}
            </ol>
            <div className="grid content-start gap-3 p-4">
                <div className="flex flex-wrap items-center justify-between gap-3">
                    {from === undefined ? (
                        <span className="text-sm text-ink-muted">{t('history.initial')}</span>
                    ) : (
                        <span className="flex items-center gap-1.5 text-sm text-ink-muted">
                            {t('history.compare')}
                            <select
                                aria-label={t('history.from')}
                                className="rounded-md border border-line bg-surface-base px-2 py-1"
                                value={from.version}
                                onChange={(event) => setFromVersion(Number(event.target.value))}
                            >
                                {versions
                                    .filter((version) => version.version < to.version)
                                    .map((version) => (
                                        <option key={version.version} value={version.version}>
                                            v{version.version}
                                        </option>
                                    ))}
                            </select>
                            →
                            <select
                                aria-label={t('history.to')}
                                className="rounded-md border border-line bg-surface-base px-2 py-1"
                                value={to.version}
                                onChange={(event) => {
                                    const next = Number(event.target.value);
                                    setToVersion(next);
                                    if (fromVersion !== null && fromVersion >= next) {
                                        setFromVersion(null);
                                    }
                                }}
                            >
                                {versions.map((version) => (
                                    <option key={version.version} value={version.version}>
                                        v{version.version}
                                    </option>
                                ))}
                            </select>
                        </span>
                    )}
                    <span className="flex items-center gap-2">
                        {from !== undefined && (
                            <span className="inline-flex overflow-hidden rounded-md border border-line text-sm">
                                {[false, true].map((only) => (
                                    <button
                                        key={String(only)}
                                        type="button"
                                        aria-pressed={onlyChanges === only}
                                        className={
                                            onlyChanges === only
                                                ? 'bg-surface-overlay px-3 py-1 text-ink'
                                                : 'px-3 py-1 text-ink-muted hover:text-ink'
                                        }
                                        onClick={() => setOnlyChanges(only)}
                                    >
                                        {only ? t('history.onlyChanges') : t('history.allFields')}
                                    </button>
                                ))}
                            </span>
                        )}
                        {onRevert !== undefined && to.version !== newest.version && (
                            <Button size="sm" onClick={() => onRevert(to)}>
                                {t('history.revertTo', { version: String(to.version) })}
                            </Button>
                        )}
                    </span>
                </div>
                <table className="w-full text-left text-sm">
                    <thead>
                        <tr className="border-b border-line text-xs text-ink-muted">
                            <th className="w-48 py-2 pr-4 font-medium">{t('history.field')}</th>
                            <th className="py-2 font-medium">
                                {from === undefined ? t('history.value') : t('history.valueDiff')}
                            </th>
                        </tr>
                    </thead>
                    <tbody>
                        {data(to).map((field) => {
                            const before =
                                from === undefined ? field.value : fieldValue(from, field.name);
                            const changed = before !== field.value;
                            if (!changed && onlyChanges && from !== undefined) {
                                return null;
                            }
                            const parts = split(before, field.value);
                            return (
                                <tr
                                    key={field.name}
                                    className={
                                        changed
                                            ? 'border-b border-line-subtle shadow-[inset_3px_0_0] shadow-warn'
                                            : 'border-b border-line-subtle'
                                    }
                                >
                                    <td className="py-2 pr-4 pl-2 align-top text-ink-muted">
                                        {field.name}
                                    </td>
                                    <td className="py-1.5">
                                        {changed ? (
                                            <div className="grid gap-px">
                                                <DiffLine sign="−" parts={parts.old} tone="old" />
                                                <DiffLine sign="+" parts={parts.new} tone="new" />
                                            </div>
                                        ) : (
                                            field.value
                                        )}
                                    </td>
                                </tr>
                            );
                        })}
                    </tbody>
                </table>
                {from !== undefined &&
                    onlyChanges &&
                    data(to).every((field) => fieldValue(from, field.name) === field.value) && (
                        <p className="text-sm text-ink-muted">{t('history.noChanges')}</p>
                    )}
            </div>
        </section>
    );
}

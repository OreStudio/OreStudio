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
import { Link } from 'react-router';
import type { RequestChange } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { Tag } from '../ui/Primitives.js';
import { descriptorFor } from './kinds.js';
import { operationLabel } from './words.js';

/**
 * What a request would change, line by line, for any kind.
 *
 * Each line says what it does to its subject, and lists the fields it changes
 * with the value they hold now beside the value they would hold. The fields it
 * leaves alone are folded away, because the decider reads the change and not
 * the record.
 */
export function ChangeDelta({
    kindCode,
    changes,
}: {
    readonly kindCode: string;
    readonly changes: readonly RequestChange[];
}): ReactNode {
    const { t } = useTranslation();
    if (changes.length === 0) {
        return <p className="text-sm text-ink-muted">{t('inbox.delta.none')}</p>;
    }
    return (
        <ol className="space-y-3">
            {changes.map((change) => (
                <li key={change.lineNo}>
                    <ChangeLine kindCode={kindCode} change={change} />
                </li>
            ))}
        </ol>
    );
}

function ChangeLine({
    kindCode,
    change,
}: {
    readonly kindCode: string;
    readonly change: RequestChange;
}): ReactNode {
    const { t, plural } = useTranslation();
    const descriptor = descriptorFor(kindCode);
    const changed = change.fields.filter((field) => field.changed);
    const unchanged = change.fields.filter((field) => !field.changed);
    const route = descriptor.subjectRoute(change);
    const isNew = change.operation === 'put' && change.baseVersion === 0;

    return (
        <article className="rounded-md border border-line bg-surface-raised">
            <header className="flex flex-wrap items-center gap-2 border-b border-line-subtle px-4 py-2 text-sm">
                <span className="text-xs text-ink-faint">
                    {t('inbox.delta.line', { line: change.lineNo })}
                </span>
                <Tag tone={change.operation === 'delete' ? 'down' : 'accent'}>
                    {isNew ? t('inbox.operation.new') : operationLabel(t, change.operation)}
                </Tag>
                {route === null ? (
                    <span className="font-mono text-xs text-ink-muted">{change.subjectId}</span>
                ) : (
                    <Link to={route} className="font-mono text-xs underline underline-offset-2">
                        {change.subjectId}
                    </Link>
                )}
                {change.baseVersion > 0 && (
                    <span className="text-xs text-ink-faint">
                        {t('inbox.delta.version', { version: change.baseVersion })}
                    </span>
                )}
                {change.applied && <Tag tone="up">{t('inbox.delta.applied')}</Tag>}
            </header>

            {change.operation === 'delete' ? (
                <p className="px-4 py-3 text-sm text-ink-muted">{t('inbox.delta.removes')}</p>
            ) : (
                <>
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line-subtle text-xs text-ink-faint">
                                <th className="px-4 py-1.5 font-medium">
                                    {t('inbox.delta.field')}
                                </th>
                                <th className="px-4 py-1.5 font-medium">
                                    {t('inbox.delta.before')}
                                </th>
                                <th className="px-4 py-1.5 font-medium">
                                    {t('inbox.delta.after')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {changed.map((field) => (
                                <tr
                                    key={field.name}
                                    className="border-b border-line-subtle last:border-b-0"
                                >
                                    <td className="px-4 py-1.5 text-ink-muted">
                                        {descriptor.fieldLabel(t, field.name)}
                                    </td>
                                    <td className="px-4 py-1.5 text-ink-faint line-through">
                                        {field.before === '' ? '' : field.before}
                                    </td>
                                    <td className="px-4 py-1.5 font-medium">
                                        {field.after === '' ? '—' : field.after}
                                    </td>
                                </tr>
                            ))}
                            {changed.length === 0 && (
                                <tr>
                                    <td colSpan={3} className="px-4 py-2 text-ink-muted">
                                        {t('inbox.delta.nothingChanged')}
                                    </td>
                                </tr>
                            )}
                        </tbody>
                    </table>
                    {unchanged.length > 0 && (
                        <details className="border-t border-line-subtle px-4 py-2 text-sm">
                            <summary className="cursor-pointer text-xs text-ink-muted">
                                {plural('inbox.delta.unchanged', unchanged.length)}
                            </summary>
                            <dl className="mt-2 grid grid-cols-1 gap-2 sm:grid-cols-2">
                                {unchanged.map((field) => (
                                    <div key={field.name} className="min-w-0">
                                        <dt className="text-xs text-ink-faint">
                                            {descriptor.fieldLabel(t, field.name)}
                                        </dt>
                                        <dd className="break-words">{field.after}</dd>
                                    </div>
                                ))}
                            </dl>
                        </details>
                    )}
                </>
            )}
        </article>
    );
}

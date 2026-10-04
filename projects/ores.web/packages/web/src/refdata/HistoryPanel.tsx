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
 * Every version of one record, newest first, and what changed in each.
 *
 * The history is the server's: one generic request returns each version's
 * fields and the field-level difference from the version before. Reverting is
 * the caller's, because writing the old values back is a write of the record
 * the caller owns; the panel only offers it for a version older than the
 * current one.
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
    const [open, setOpen] = useState<number | null>(null);
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
    if (history.data.length === 0) {
        return <p className="text-sm text-ink-muted">{t('history.empty')}</p>;
    }
    const current = history.data[0]?.version;

    return (
        <div>
            <p className="mb-2 text-sm text-ink-muted">{t('history.description')}</p>
            <ol className="divide-y divide-line-subtle rounded-md border border-line">
                {history.data.map((version) => (
                    <li key={version.version} className="px-4 py-2 text-sm">
                        <div className="flex flex-wrap items-center gap-2">
                            <button
                                type="button"
                                className="font-medium hover:underline"
                                onClick={() =>
                                    setOpen(open === version.version ? null : version.version)
                                }
                            >
                                v{version.version}
                            </button>
                            {version.version === current && (
                                <Tag tone="accent">{t('history.current')}</Tag>
                            )}
                            <span className="text-ink-muted">{version.modifiedBy}</span>
                            <span className="text-ink-faint">{version.recordedAt}</span>
                            <span className="text-ink-muted">
                                {version.changes.length === 0
                                    ? t('history.initial')
                                    : version.changes.map((change) => change.field).join(', ')}
                            </span>
                            {onRevert !== undefined && version.version !== current && (
                                <Button
                                    size="sm"
                                    variant="ghost"
                                    className="ml-auto"
                                    onClick={() => onRevert(version)}
                                >
                                    {t('history.revert')}
                                </Button>
                            )}
                        </div>
                        {open === version.version && <VersionDetail version={version} />}
                    </li>
                ))}
            </ol>
        </div>
    );
}

function VersionDetail({ version }: { readonly version: HistoryVersion }): ReactNode {
    const { t } = useTranslation();
    if (version.changes.length === 0) {
        return (
            <dl className="mt-2 grid grid-cols-[max-content_1fr] gap-x-4 gap-y-1 text-xs">
                {version.fields.map((field) => (
                    <div key={field.name} className="contents">
                        <dt className="text-ink-muted">{field.name}</dt>
                        <dd>{field.value}</dd>
                    </div>
                ))}
            </dl>
        );
    }
    return (
        <table className="mt-2 w-full text-left text-xs">
            <thead>
                <tr className="text-ink-muted">
                    <th className="py-1 pr-4 font-medium">{t('history.field')}</th>
                    <th className="py-1 pr-4 font-medium">{t('history.before')}</th>
                    <th className="py-1 font-medium">{t('history.after')}</th>
                </tr>
            </thead>
            <tbody>
                {version.changes.map((change) => (
                    <tr key={change.field}>
                        <td className="py-1 pr-4 text-ink-muted">{change.field}</td>
                        <td className="py-1 pr-4 line-through decoration-down/60">
                            {change.before}
                        </td>
                        <td className="py-1">{change.after}</td>
                    </tr>
                ))}
            </tbody>
        </table>
    );
}

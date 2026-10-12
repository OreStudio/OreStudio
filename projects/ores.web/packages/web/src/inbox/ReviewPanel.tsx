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
import { Button, Field, Notice, Tag } from '../ui/Primitives.js';
import { descriptorFor } from './kinds.js';
import { operationLabel, partLabel } from './words.js';

/**
 * One change the maker is about to confirm.
 *
 * The preview states it: what it does, the fields it changes, the parts the
 * policy names to decide it, and the refusal the real write gave, if any.
 */
export interface ReviewLine {
    readonly key: string;
    /** What the change is about, in words the maker knows it by. */
    readonly label: string;
    readonly operation: string;
    readonly columns: readonly string[];
    /** The parts that decide this change alone. */
    readonly partCodes: readonly string[];
    /** Empty when the real write would stand. */
    readonly refusal: string;
}

/**
 * The step before a gated change: every change with its badge and its decider,
 * and the one button that either writes it or raises a request for it.
 *
 * The policy decides which. When it names no part for any change, the changes
 * are written directly and the panel says so. When it names a part, nothing
 * changes until every part has approved, and the panel names who will decide.
 * A refused change stops the confirm, because the maker has something to
 * correct. The panel holds no state of its own beyond what it is given: the
 * screen that owns the journey previews, and calls back to write or to raise.
 */
export function ReviewPanel({
    kindCode,
    lines,
    partCodes,
    reason,
    onReason,
    pending,
    error,
    onWrite,
    onRaise,
}: {
    readonly kindCode: string;
    readonly lines: readonly ReviewLine[];
    /** The parts the policy needs for the changes together. */
    readonly partCodes: readonly string[];
    readonly reason: string;
    readonly onReason: (reason: string) => void;
    readonly pending: boolean;
    /** Why the last write or raise failed, if it did. */
    readonly error?: string;
    readonly onWrite: () => void;
    readonly onRaise: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const descriptor = descriptorFor(kindCode);
    const refused = lines.some((line) => line.refusal !== '');
    const gated = partCodes.length > 0;

    return (
        <section className="space-y-4">
            <header className="space-y-1">
                <h2 className="text-sm font-semibold">{t('inbox.review.title')}</h2>
                <p className="text-sm text-ink-muted">{t('inbox.review.lead')}</p>
            </header>

            <ol className="space-y-2">
                {lines.map((line) => (
                    <li
                        key={line.key}
                        className="space-y-2 rounded-md border border-line bg-surface-raised px-4 py-3"
                    >
                        <div className="flex flex-wrap items-center gap-2 text-sm">
                            <Tag tone={line.operation === 'delete' ? 'down' : 'accent'}>
                                {operationLabel(t, line.operation)}
                            </Tag>
                            <span className="font-medium">{line.label}</span>
                        </div>
                        {line.columns.length > 0 && (
                            <div className="flex flex-wrap items-center gap-1.5 text-xs text-ink-muted">
                                <span>{t('inbox.review.changes')}</span>
                                {line.columns.map((column) => (
                                    <Tag key={column} tone="neutral" small>
                                        {descriptor.fieldLabel(t, column)}
                                    </Tag>
                                ))}
                            </div>
                        )}
                        <div className="flex flex-wrap items-center gap-1.5 text-xs text-ink-muted">
                            <span>{t('inbox.review.decidedBy')}</span>
                            {line.partCodes.length === 0 ? (
                                <span>{t('inbox.review.nobody')}</span>
                            ) : (
                                line.partCodes.map((code) => (
                                    <Tag key={code} tone="warn" small>
                                        {partLabel(t, code)}
                                    </Tag>
                                ))
                            )}
                        </div>
                        {line.refusal !== '' && <Notice tone="error">{line.refusal}</Notice>}
                    </li>
                ))}
            </ol>

            {error !== undefined && error !== '' && <Notice tone="error">{error}</Notice>}

            {refused ? (
                <Notice tone="warn">{t('inbox.review.refused')}</Notice>
            ) : gated ? (
                <>
                    <Notice tone="info">
                        {t('inbox.review.goesTo', {
                            parts: partCodes.map((code) => partLabel(t, code)).join(', '),
                        })}
                    </Notice>
                    <Field label={t('inbox.review.reason')} hint={t('inbox.review.reasonHint')}>
                        <textarea
                            className="w-full rounded-md border border-line bg-surface-base px-3 py-2 text-sm text-ink placeholder:text-ink-faint hover:border-line-strong focus:border-accent focus:outline-none focus:ring-3 focus:ring-accent/20"
                            rows={3}
                            value={reason}
                            onChange={(event) => onReason(event.target.value)}
                        />
                    </Field>
                </>
            ) : (
                <Notice tone="info">{t('inbox.review.needsNoRequest')}</Notice>
            )}

            <div className="flex flex-wrap gap-2">
                {gated ? (
                    <Button
                        variant="primary"
                        disabled={refused || pending || reason.trim() === ''}
                        pending={pending}
                        onClick={onRaise}
                    >
                        {t('inbox.review.raise')}
                    </Button>
                ) : (
                    <Button
                        variant="primary"
                        disabled={refused || pending}
                        pending={pending}
                        onClick={onWrite}
                    >
                        {t('inbox.review.write')}
                    </Button>
                )}
            </div>
        </section>
    );
}

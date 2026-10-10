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

import { useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Notice } from '../ui/Primitives.js';

/** One field that differs between the version a person started from and the newest. */
export interface FieldChange {
    readonly field: string;
    readonly before: string;
    readonly after: string;
}

/**
 * The fields of a record that moved, by label, comparing text.
 *
 * A field with no value on either side is written as nothing, so a clearing is
 * a change and an absent value is not.
 */
export function fieldChanges<T>(
    before: T,
    after: T,
    fields: Readonly<Record<string, (record: T) => string>>,
): readonly FieldChange[] {
    return Object.entries(fields)
        .map(([field, read]) => ({ field, before: read(before), after: read(after) }))
        .filter((change) => change.before !== change.after);
}

/**
 * Tells an open edit that the record was saved by somebody else meanwhile.
 *
 * The record is the live one, read again as the server announces a change. The
 * edit started from a version, and that version is what a save claims, so a save
 * made against an old version is refused rather than overwriting the newer one.
 * The person then chooses: keep the edit, which takes the newest version as its
 * base and so replaces it knowingly, or take the new values and drop the edit.
 * The form is never reloaded under the person.
 */
export function useNewerVersion<T extends { readonly version: number }>(
    live: T | null | undefined,
    fields: Readonly<Record<string, (record: T) => string>>,
    onTake: () => void,
): {
    /** The version the edit started from, which a save claims. */
    readonly expectedVersion: number | null;
    /** The newest record and what moved, when it is newer than the edit's base. */
    readonly newer: { readonly record: T; readonly changes: readonly FieldChange[] } | undefined;
    readonly keepMine: () => void;
    readonly takeTheirs: () => void;
} {
    const [basis, setBasis] = useState<T | null | undefined>(live);
    const newer =
        live !== null &&
        live !== undefined &&
        basis !== null &&
        basis !== undefined &&
        live.version > basis.version
            ? { record: live, changes: fieldChanges(basis, live, fields) }
            : undefined;
    return {
        expectedVersion: basis?.version ?? null,
        newer,
        keepMine: () => setBasis(live),
        takeTheirs: () => {
            setBasis(live);
            onTake();
        },
    };
}

/** The line above a form that says the record changed while it was open. */
export function NewerVersionNotice({
    newer,
    by,
    onKeep,
    onTake,
}: {
    readonly newer: {
        readonly record: { readonly version: number };
        readonly changes: readonly FieldChange[];
    };
    /** Who saved the newer version, when the record says. */
    readonly by: string;
    readonly onKeep: () => void;
    readonly onTake: () => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <Notice tone="warn">
            <div className="space-y-2">
                <p className="font-medium">
                    {by === ''
                        ? t('profile.newer.title', { version: String(newer.record.version) })
                        : t('profile.newer.titleBy', {
                              version: String(newer.record.version),
                              by,
                          })}
                </p>
                {newer.changes.length > 0 && (
                    <ul className="list-disc pl-5">
                        {newer.changes.map((change) => (
                            <li key={change.field}>
                                {t('profile.newer.change', {
                                    field: change.field,
                                    before:
                                        change.before === ''
                                            ? t('profile.newer.empty')
                                            : change.before,
                                    after:
                                        change.after === ''
                                            ? t('profile.newer.empty')
                                            : change.after,
                                })}
                            </li>
                        ))}
                    </ul>
                )}
                <div className="flex flex-wrap gap-2">
                    <Button size="sm" onClick={onKeep}>
                        {t('profile.newer.keep')}
                    </Button>
                    <Button size="sm" onClick={onTake}>
                        {t('profile.newer.take')}
                    </Button>
                </div>
            </div>
        </Notice>
    );
}

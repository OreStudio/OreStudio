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

import { useRef, useState } from 'react';
import { trail } from '../log/clientLog.js';

const saves = trail('save');

/** Why a save did not happen, in words the form can show beside the fields. */
export interface EditOutcome {
    readonly ok: boolean;
    readonly message: string;
    readonly fields: readonly {
        readonly field: string;
        readonly code: string;
        readonly message: string;
    }[];
}

/**
 * What every form that saves a record does, once.
 *
 * Save, then close the form, then read the record again. The order is the
 * point. A save makes a new version, and a form still open when the record is
 * read again would see that version as somebody else's change. So the form is
 * gone before anything is reloaded, and the screen behind it takes the news
 * and shows it as its own.
 *
 * While the save runs the form does not reload what it shows. `saving` is true
 * for that time, and a form that listens for changes ignores them until the
 * save has ended, because the only change that can arrive then is its own.
 *
 * Each step is written to the trail, so a form's path reads as: opened, save
 * requested, saved, closed.
 */
export function useRecordEdit({
    form,
    onClose,
    onSaved,
}: {
    /** The form's name in the log. */
    readonly form: string;
    readonly onClose: () => void;
    /** Reads the screen behind the form again, after the form has closed. */
    readonly onSaved: () => Promise<unknown>;
}): {
    readonly busy: boolean;
    readonly outcome: EditOutcome | undefined;
    readonly setOutcome: (outcome: EditOutcome | undefined) => void;
    /** True while a save runs; a listener ignores changes then. */
    readonly saving: { readonly current: boolean };
    /**
     * Runs a save. The work answers a refusal to show, or nothing when it
     * succeeded; a failure it throws is shown too.
     */
    readonly run: (
        work: () => Promise<EditOutcome | undefined>,
        options?: { readonly readAfterRefusal?: boolean },
    ) => Promise<void>;
} {
    const [busy, setBusy] = useState(false);
    const [outcome, setOutcome] = useState<EditOutcome | undefined>(undefined);
    const saving = useRef(false);

    const run = async (
        work: () => Promise<EditOutcome | undefined>,
        options: { readonly readAfterRefusal?: boolean } = {},
    ): Promise<void> => {
        setBusy(true);
        saving.current = true;
        setOutcome(undefined);
        saves.info('save requested', { form });
        try {
            const refusal = await work();
            if (refusal === undefined) {
                saves.info('saved', { form });
                onClose();
                await onSaved();
                return;
            }
            setOutcome(refusal);
            saves.warn('save refused', { form, reason: refusal.message });
            saving.current = false;
            if (options.readAfterRefusal === true) await onSaved();
        } catch (error) {
            const message = error instanceof Error ? error.message : String(error);
            setOutcome({ ok: false, message, fields: [] });
            saves.error('save failed', { form, reason: message });
        } finally {
            saving.current = false;
            setBusy(false);
        }
    };

    return { busy, outcome, setOutcome, saving, run };
}

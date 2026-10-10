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
import { useAccessState, type AccessState } from '../access/holds.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Notice } from '../ui/Primitives.js';

/**
 * Says so when the person's permissions could not be read.
 *
 * Every menu entry, card and panel is drawn by the permissions the person
 * holds, so a read that is slow or has failed removes them without a word. A
 * person who sees a menu with parts missing cannot tell "I may not" from "the
 * server did not answer". This notice is that difference, in the server's words.
 */
export function AccessNotice(): ReactNode {
    return <AccessNoticeFor state={useAccessState()} />;
}

/** The notice for one state of the read, so the states can be drawn without a server. */
export function AccessNoticeFor({ state }: { readonly state: AccessState }): ReactNode {
    const { t } = useTranslation();
    if (state.kind === 'ready') {
        return null;
    }
    if (state.kind === 'slow') {
        return (
            <div className="mb-6">
                <Notice tone="info">{t('accessRead.slow')}</Notice>
            </div>
        );
    }
    return (
        <div className="mb-6">
            <Notice tone="error">
                <div className="flex flex-wrap items-start justify-between gap-3">
                    <div className="space-y-1">
                        <p className="font-semibold">{t('accessRead.failed')}</p>
                        <p className="font-mono break-words">{state.message}</p>
                    </div>
                    <Button size="sm" onClick={state.retry}>
                        {t('common.retry')}
                    </Button>
                </div>
            </Notice>
        </div>
    );
}

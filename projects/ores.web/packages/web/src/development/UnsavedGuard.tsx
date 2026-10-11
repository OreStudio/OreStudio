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

import { useContext, useEffect, type ReactNode } from 'react';
import { UNSAFE_DataRouterContext, useBlocker } from 'react-router';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Dialog } from '../ui/Primitives.js';

/**
 * The question a tester is asked before leaving with marks or notes that are
 * not saved. Stay keeps the page as it is. Leave drops what is not saved.
 */
export function LeaveDialog({
    onStay,
    onLeave,
}: {
    readonly onStay: () => void;
    readonly onLeave: () => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <Dialog
            title={t('development.leave.title')}
            onClose={onStay}
            footer={
                <>
                    <Button type="button" variant="primary" onClick={onStay}>
                        {t('development.leave.stay')}
                    </Button>
                    <Button type="button" variant="danger" onClick={onLeave}>
                        {t('development.leave.leave')}
                    </Button>
                </>
            }
        >
            <p className="text-sm text-ink-muted">{t('development.leave.body')}</p>
        </Dialog>
    );
}

/**
 * Whether a move from one place to another leaves the page. A change of the
 * query or the hash, such as a tab of the page, stays on it.
 */
export function leavesPage(
    current: { readonly pathname: string },
    next: { readonly pathname: string },
): boolean {
    return current.pathname !== next.pathname;
}

/** Asks before a move inside the app that would drop what is not saved. */
function LeaveGuard({ dirty }: { readonly dirty: boolean }): ReactNode {
    const blocker = useBlocker(
        ({ currentLocation, nextLocation }) => dirty && leavesPage(currentLocation, nextLocation),
    );
    if (blocker.state !== 'blocked') return null;
    return <LeaveDialog onStay={() => blocker.reset()} onLeave={() => blocker.proceed()} />;
}

/** Asks the browser to ask before a reload, a close or a typed address drops what is not saved. */
function useUnloadGuard(dirty: boolean): void {
    useEffect(() => {
        if (!dirty) return undefined;
        const ask = (event: BeforeUnloadEvent): void => {
            event.preventDefault();
            event.returnValue = '';
        };
        window.addEventListener('beforeunload', ask);
        return () => window.removeEventListener('beforeunload', ask);
    }, [dirty]);
}

/**
 * Guards a page that holds something the tester has not saved. A move inside
 * the app asks in a dialog. A reload, a close or a typed address asks through
 * the browser's own prompt.
 *
 * The question inside the app needs a data router, because a blocker works
 * only there. Under a plain router, as in a test that renders markup, the
 * guard asks nothing in the app and the unload guard still works.
 */
export function UnsavedGuard({ dirty }: { readonly dirty: boolean }): ReactNode {
    const inDataRouter = useContext(UNSAFE_DataRouterContext) !== null;
    useUnloadGuard(dirty);
    return inDataRouter ? <LeaveGuard dirty={dirty} /> : null;
}

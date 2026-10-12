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
 */
import { useSyncExternalStore, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { cx } from './Primitives.js';

/**
 * A short message about something the person just did, shown in the corner and
 * closed by them. It is for news of an action, such as a conversion that changed
 * their numbers. A fact that stays true, such as an engine that can take a price
 * below zero, is also marked where it applies, because a closed message is gone.
 */
export interface Toast {
    readonly id: number;
    readonly message: string;
    readonly tone: 'info' | 'warn';
    /** Whether it stays until closed. Information leaves by itself; a warning stays. */
    readonly sticky: boolean;
}

const INFO_SECONDS = 10;
const CAP = 4;

let nextId = 1;
let toasts: readonly Toast[] = [];
const listeners = new Set<() => void>();

function changed(): void {
    for (const listener of listeners) {
        listener();
    }
}

function subscribe(listener: () => void): () => void {
    listeners.add(listener);
    return () => {
        listeners.delete(listener);
    };
}

function snapshot(): readonly Toast[] {
    return toasts;
}

/** Takes one toast away. */
export function closeToast(id: number): void {
    toasts = toasts.filter((one) => one.id !== id);
    changed();
}

/** Shows a message unless the same one is already showing. */
export function toast(message: string, tone: Toast['tone'] = 'info'): void {
    if (toasts.some((one) => one.message === message)) {
        return;
    }
    const id = nextId;
    nextId += 1;
    const sticky = tone === 'warn';
    toasts = [...toasts, { id, message, tone, sticky }].slice(-CAP);
    changed();
    if (!sticky) {
        setTimeout(() => closeToast(id), INFO_SECONDS * 1000);
    }
}

/** The corner where the toasts of every screen show. */
export function ToastViewport(): ReactNode {
    const { t } = useTranslation();
    const shown = useSyncExternalStore(subscribe, snapshot, snapshot);
    if (shown.length === 0) {
        return null;
    }
    return (
        <div className="pointer-events-none fixed right-4 bottom-4 z-50 flex w-[min(24rem,calc(100vw-2rem))] flex-col gap-2">
            {shown.map((one) => (
                <div
                    key={one.id}
                    role="status"
                    className={cx(
                        'pointer-events-auto flex items-start gap-3 rounded-md border bg-surface-base px-3 py-2 text-sm shadow-lg',
                        one.tone === 'warn' ? 'border-warn/60' : 'border-accent/50',
                    )}
                >
                    <p className="min-w-0 flex-1 text-ink">{one.message}</p>
                    <button
                        type="button"
                        aria-label={t('toast.close')}
                        className="shrink-0 rounded px-1 text-ink-muted hover:text-ink"
                        onClick={() => closeToast(one.id)}
                    >
                        ×
                    </button>
                </div>
            ))}
        </div>
    );
}

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

import { useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { useHeld } from '../../access/holds.js';
import { api, type FeedSelection } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { Button, Dialog } from '../../ui/Primitives.js';
import { toast } from '../../ui/Toasts.js';
import { RUNNING_KEY } from './feedRuns.js';
import { FOLDERS_KEY } from './useCollections.js';

/** The key of every read of a page of feeds, so one refresh renews them all. */
export const FEEDS_KEY = ['folder-feeds'] as const;

/** What a start or stop acts on: a name for the question, and the feeds the service resolves. */
export interface RunTarget {
    readonly name: string;
    readonly selection: FeedSelection;
}

/**
 * Starting and stopping the feeds of a selection: the question first, then one
 * request that carries the filter and the ids, then a toast that says what
 * started, what was already running and what was skipped. The service finds the
 * feeds, so the screen never holds the feeds of a large set. A screen draws the
 * dialog where it likes and calls `ask` from its buttons.
 */
export function useSelectionRuns(onDone?: () => void): {
    readonly mayRun: boolean;
    readonly ask: (action: 'start' | 'stop', target: RunTarget) => void;
    readonly dialog: ReactNode;
} {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const holds = useHeld();
    const mayRun =
        holds('synthetic::fx_spot_generation_configs:write') ||
        holds('synthetic::ir_curve_generation_configs:write');
    const [asking, setAsking] = useState<{
        readonly action: 'start' | 'stop';
        readonly target: RunTarget;
    } | null>(null);
    const [pending, setPending] = useState(false);

    const run = async (): Promise<void> => {
        if (asking === null) {
            return;
        }
        const { action, target } = asking;
        setPending(true);
        try {
            if (action === 'start') {
                const report = await api.startSelection(target.selection);
                toast(
                    t('marketdata.synthetic.runs.startReport', {
                        name: target.name,
                        started: String(report.started),
                        already: String(report.alreadyRunning),
                        skipped: String(report.skipped),
                    }),
                    report.skipped > 0 ? 'warn' : 'info',
                );
            } else {
                const report = await api.stopSelection(target.selection);
                toast(
                    t('marketdata.synthetic.runs.stopReport', {
                        name: target.name,
                        stopped: String(report.stopped),
                    }),
                );
            }
            onDone?.();
        } catch (error) {
            toast(error instanceof Error ? error.message : String(error), 'warn');
        } finally {
            setPending(false);
            setAsking(null);
            await queries.invalidateQueries({ queryKey: FOLDERS_KEY });
            await queries.invalidateQueries({ queryKey: FEEDS_KEY });
            await queries.invalidateQueries({ queryKey: RUNNING_KEY });
        }
    };

    const dialog =
        asking === null ? null : (
            <Dialog
                title={t(`marketdata.synthetic.runs.${asking.action}Title`, {
                    name: asking.target.name,
                })}
                onClose={() => setAsking(null)}
                footer={
                    <>
                        <Button variant="ghost" onClick={() => setAsking(null)}>
                            {t('refdata.records.cancel')}
                        </Button>
                        <Button variant="primary" pending={pending} onClick={() => void run()}>
                            {t(`marketdata.synthetic.runs.${asking.action}`)}
                        </Button>
                    </>
                }
            >
                <p className="text-sm">
                    {t(`marketdata.synthetic.runs.${asking.action}Body`, {
                        name: asking.target.name,
                    })}
                </p>
            </Dialog>
        );

    return { mayRun, ask: (action, target) => setAsking({ action, target }), dialog };
}

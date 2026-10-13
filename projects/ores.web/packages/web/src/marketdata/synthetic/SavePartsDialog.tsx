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

import { useMutation, useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { api } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { NEW_RECORD_REASON } from '../../refdata/records.js';
import { ReasonFields, useReason } from '../../refdata/shared.js';
import { Button, Dialog, Notice } from '../../ui/Primitives.js';
import { RUNNING_KEY } from './feedRuns.js';
import type { savePlan } from './mixture.js';

/** What the save writes, counted as a person reads it. */
export interface PartCounts {
    readonly added: number;
    readonly changed: number;
    readonly removed: number;
}

/** Writes the feed's engine, with the reason its parts are saved under. */
export type SaveEngine = (
    engine: string,
    intent: { readonly reasonCode: string; readonly commentary: string },
) => Promise<void>;

/**
 * Asks why the parts of a feed change and writes them: the mixture of an FX
 * feed, the parameters of an IR curve feed. The removed rows go first and the
 * rest in one write, all under one reason. A running feed drew its parts when
 * it started and keeps them, so the dialog says so and offers to restart the
 * feed, which then draws from the parts saved.
 *
 * `words` names the block of `marketdata.*` words the dialog speaks in, and
 * `engineLabel` says how an engine is named to a person.
 */
export function SavePartsDialog({
    feedId,
    resource,
    plan,
    counts,
    running,
    engine,
    engineLabel,
    saveEngine,
    words,
    onClose,
}: {
    readonly feedId: string;
    readonly resource: string;
    readonly plan: ReturnType<typeof savePlan>;
    readonly counts: PartCounts;
    readonly running: boolean;
    readonly engine: { readonly from: string; readonly to: string } | undefined;
    readonly engineLabel: (engine: string) => string;
    readonly saveEngine: SaveEngine;
    readonly words: string;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const onlyNew =
        engine === undefined &&
        plan.removals.length === 0 &&
        plan.changes.every((change) => change.version === null);
    const reason = useReason('amend', true);
    const [commentary, setCommentary] = useState('');
    const [restart, setRestart] = useState(true);
    const missing = !onlyNew && reason.needsCommentary && commentary.trim() === '';

    const save = useMutation({
        mutationFn: async () => {
            const intent = onlyNew
                ? { reasonCode: NEW_RECORD_REASON, commentary: '' }
                : { reasonCode: reason.code, commentary: commentary.trim() };
            if (engine !== undefined) {
                await saveEngine(engine.to, intent);
            }
            if (plan.removals.length > 0) {
                await api.removeSynthetic(
                    resource,
                    plan.removals.map((removal) => ({
                        key: removal.key,
                        version: plan.removals.length === 1 ? removal.version : null,
                    })),
                    intent,
                );
            }
            if (plan.changes.length > 0) {
                await api.saveSynthetic(resource, plan.changes, intent);
            }
            if (running && restart) {
                await api.stopFeed(feedId);
                await api.startFeed(feedId);
            }
        },
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['records'] });
            await queries.invalidateQueries({ queryKey: ['history'] });
            await queries.invalidateQueries({ queryKey: RUNNING_KEY });
            onClose();
        },
    });

    return (
        <Dialog
            title={t(`marketdata.${words}.saveTitle`)}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('refdata.records.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        icon="save"
                        pending={save.isPending}
                        disabled={missing || (!onlyNew && reason.code === '')}
                        onClick={() => save.mutate()}
                    >
                        {t(`marketdata.${words}.saveButton`)}
                    </Button>
                </>
            }
        >
            <div className="space-y-4">
                {engine !== undefined && (
                    <p className="text-sm">
                        {t(`marketdata.${words}.saveEngine`, {
                            from: engineLabel(engine.from),
                            to: engineLabel(engine.to),
                        })}
                    </p>
                )}
                <p className="text-sm">
                    {t(`marketdata.${words}.saveSummary`, {
                        added: String(counts.added),
                        changed: String(counts.changed),
                        removed: String(counts.removed),
                    })}
                </p>
                {onlyNew ? (
                    <p className="text-xs text-ink-faint">
                        {t('refdata.classifications.newRecordNote')}
                    </p>
                ) : (
                    <ReasonFields
                        reason={reason}
                        commentary={commentary}
                        onCommentary={setCommentary}
                        missing={missing}
                    />
                )}
                {running ? (
                    <div className="space-y-2 rounded-md border border-line p-3 text-sm">
                        <p>{t(`marketdata.${words}.runningNote`)}</p>
                        <label className="flex items-center gap-2">
                            <input
                                type="checkbox"
                                checked={restart}
                                onChange={(event) => setRestart(event.target.checked)}
                            />
                            {t(`marketdata.${words}.restart`)}
                        </label>
                    </div>
                ) : (
                    <p className="text-sm text-ink-muted">{t(`marketdata.${words}.stoppedNote`)}</p>
                )}
                {save.isError && <Notice tone="error">{save.error.message}</Notice>}
            </div>
        </Dialog>
    );
}

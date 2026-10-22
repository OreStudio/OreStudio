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

import { useState, type ReactNode } from 'react';
import { useMutation, useQueryClient } from '@tanstack/react-query';
import { ApiFailure } from '../api/transport.js';
import {
    pricingEngines,
    type PricingDocument,
    type PricingVocabulary,
    type PricingWrite,
} from '../api/pricingEngines.js';
import { useTranslation } from '../i18n/Provider.js';
import { LeavingFooter, useCloseGuard } from '../refdata/records.js';
import { ReasonFields, useReason } from '../refdata/shared.js';
import { Button, Dialog, Notice } from '../ui/Primitives.js';
import { blocked, check, difference, draftOf, type Draft } from './model.js';
import { DifferenceList, FindingsList } from './parts.js';

/** The write a draft makes, with the reason it is made for. */
export function writeOf(
    draft: Draft,
    intent: { readonly reason_code: string; readonly commentary: string },
): PricingWrite {
    return {
        name: draft.name,
        description: draft.description,
        variant: draft.variant,
        products: draft.products,
        parameters: draft.parameters,
        intent,
    };
}

/**
 * A change to a saved configuration, as the record screen standard draws an
 * amend: the editor the caller supplies, the difference the change makes, the
 * checks it passes, a reason and Save.
 *
 * Save writes the whole document against the version the screen read. A
 * document that changed since is refused, and the dialog says so and offers to
 * read it again rather than overwrite what somebody else did.
 */
export function SaveDialog({
    title,
    document,
    after,
    vocabulary,
    onClose,
    onSaved,
    children,
}: {
    readonly title: string;
    readonly document: PricingDocument;
    /** The draft the editor has built. */
    readonly after: Draft;
    readonly vocabulary: PricingVocabulary;
    readonly onClose: () => void;
    readonly onSaved?: () => void;
    /** The editor itself. */
    readonly children: ReactNode;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const before = draftOf(document);
    const changes = difference(before, after);
    const findings = check(after, vocabulary);
    const reason = useReason('amend', changes.length > 0);
    const [commentary, setCommentary] = useState('');
    const [conflict, setConflict] = useState(false);
    const missing = reason.needsCommentary && commentary.trim() === '';
    const dirty = changes.length > 0 || commentary.trim() !== '';
    const guard = useCloseGuard(dirty, onClose);
    const save = useMutation({
        mutationFn: () =>
            pricingEngines.save(
                document.id,
                document.version,
                writeOf(after, { reason_code: reason.code, commentary: commentary.trim() }),
            ),
        onSuccess: async () => {
            setConflict(false);
            await queries.invalidateQueries({ queryKey: ['pricing-engine'] });
            await queries.invalidateQueries({ queryKey: ['records', 'pricing-engines'] });
            onSaved?.();
            onClose();
        },
        onError: (error) => {
            setConflict(error instanceof ApiFailure && error.status === 409);
        },
    });
    const reload = async (): Promise<void> => {
        await queries.invalidateQueries({ queryKey: ['pricing-engine', document.id] });
        onClose();
    };
    const ready = changes.length > 0 && !blocked(findings) && !missing && !conflict;

    return (
        <Dialog
            title={title}
            name="pricing-engine-change"
            wide
            onClose={guard.close}
            loadedAt={Date.now()}
            footer={
                guard.leaving ? (
                    <LeavingFooter onStay={guard.stay} onLeave={onClose} />
                ) : (
                    <>
                        <Button variant="ghost" onClick={guard.close}>
                            {t('common.cancel')}
                        </Button>
                        <Button
                            variant="primary"
                            icon="save"
                            disabled={!ready}
                            pending={save.isPending}
                            onClick={() => save.mutate()}
                        >
                            {t('pricingEngines.save.save')}
                        </Button>
                    </>
                )
            }
        >
            <div className="space-y-5">
                {conflict && (
                    <Notice tone="error">
                        <span className="mr-2">{t('pricingEngines.save.conflict')}</span>
                        <Button size="sm" onClick={() => void reload()}>
                            {t('pricingEngines.save.reload')}
                        </Button>
                    </Notice>
                )}
                {save.isError && !conflict && <Notice tone="error">{save.error.message}</Notice>}
                {children}
                <DifferenceList changes={changes} />
                <FindingsList findings={findings} />
                <ReasonFields
                    reason={reason}
                    commentary={commentary}
                    onCommentary={setCommentary}
                    missing={missing}
                />
            </div>
        </Dialog>
    );
}

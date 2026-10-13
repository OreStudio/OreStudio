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
import { useNavigate } from 'react-router';
import { useTranslation } from '../../i18n/Provider.js';
import { RecordDialog, show, useRecordPermissions } from '../../refdata/records.js';
import { Button, Dialog } from '../../ui/Primitives.js';
import { feedPath, type FeedKind } from './feedKind.js';
import { FX_FEED_KIND } from './fxFeeds.js';
import { IR_FEED_KIND } from './irFeeds.js';

const KINDS: readonly FeedKind[] = [FX_FEED_KIND, IR_FEED_KIND];

/** Whether the person may write a feed of some kind, so Add feed is offered at all. */
export function useMayAddFeed(): boolean {
    const fx = useRecordPermissions(FX_FEED_KIND.resource);
    const ir = useRecordPermissions(IR_FEED_KIND.resource);
    return fx.write || ir.write;
}

/** The form of one kind of feed; the feed's own page opens when it is saved. */
function AddFeedForm({
    kind,
    collectionId,
    onClose,
}: {
    readonly kind: FeedKind;
    readonly collectionId: string;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const check = kind.useCheck();
    return (
        <RecordDialog
            title={t(`marketdata.${kind.words}.addTitle`)}
            resource={kind.resource}
            specs={kind.fields}
            row={undefined}
            given={{ id: crypto.randomUUID(), folder_id: null }}
            initial={
                collectionId === '' ? kind.initial : { ...kind.initial, config_id: collectionId }
            }
            complete={kind.complete}
            check={check}
            onClose={onClose}
            onSaved={(write) =>
                void navigate(`${feedPath(kind, show(write['id']))}?tab=${kind.afterAdd}`)
            }
        />
    );
}

/**
 * Add feed: choose the kind of feed, then fill its form. The collection the person is
 * looking at is the one the form starts with. The database places the feed in the
 * folder of its kind under that collection; its page opens next, where its process is set.
 */
export function AddFeed({
    collectionId,
    onClose,
}: {
    readonly collectionId: string;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const [kind, setKind] = useState<FeedKind | undefined>();
    if (kind !== undefined) {
        return <AddFeedForm kind={kind} collectionId={collectionId} onClose={onClose} />;
    }
    return (
        <Dialog
            title={t('marketdata.synthetic.tree.addTitle')}
            onClose={onClose}
            footer={
                <Button variant="ghost" onClick={onClose}>
                    {t('refdata.records.cancel')}
                </Button>
            }
        >
            <p className="mb-3 text-sm text-ink-muted">{t('marketdata.synthetic.tree.addBody')}</p>
            <div className="flex flex-wrap gap-2">
                {KINDS.map((one) => (
                    <Button key={one.words} variant="primary" onClick={() => setKind(one)}>
                        {t(`marketdata.${one.words}.add`)}
                    </Button>
                ))}
            </div>
        </Dialog>
    );
}

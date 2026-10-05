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

import { keepPreviousData, useQuery } from '@tanstack/react-query';
import { useEffect, useState, type ReactNode } from 'react';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Icon } from '../ui/Icon.js';
import { imageUrl } from '../ui/Images.js';
import { Pager, pageBounds } from '../ui/Pager.js';
import { Button, Dialog, Input, Notice } from '../ui/Primitives.js';
import { Flag } from './flags.js';

/** How many images one page of the chooser shows. */
const CHOOSER_PAGE = 60;

/** How long the chooser's search waits after the last keystroke. */
const SEARCH_PAUSE_MS = 300;

/**
 * Chooses one of the tenant's images, such as a currency's flag: a search on
 * the server by code and description, a page of images to pick from, and Clear
 * for a record that should have none. It chooses among the images that exist;
 * adding images is the storage screens' work.
 */
export function ImageChooser({
    title,
    current,
    onChoose,
    onClose,
}: {
    readonly title: string;
    readonly current: string | null;
    readonly onChoose: (imageId: string | null) => void;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const [typed, setTyped] = useState('');
    const [search, setSearch] = useState('');
    const [offset, setOffset] = useState(0);
    const [chosen, setChosen] = useState<string | null>(current);
    const page = useQuery({
        queryKey: ['images', 'page', { offset, search }],
        queryFn: () => api.imageSummaries({ offset, limit: CHOOSER_PAGE, search }),
        placeholderData: keepPreviousData,
    });
    useEffect(() => {
        const timer = window.setTimeout(() => {
            if (typed.trim() !== search) {
                setSearch(typed.trim());
                setOffset(0);
            }
        }, SEARCH_PAUSE_MS);
        return () => window.clearTimeout(timer);
    }, [typed, search]);
    const images = page.data?.images ?? [];
    const bounds = pageBounds(offset, images.length);

    return (
        <Dialog
            title={title}
            onClose={onClose}
            wide
            footer={
                <>
                    <Button
                        variant="ghost"
                        icon="remove"
                        onClick={() => {
                            onChoose(null);
                            onClose();
                        }}
                    >
                        {t('images.clear')}
                    </Button>
                    <Button variant="ghost" icon="cancel" onClick={onClose}>
                        {t('refdata.records.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        icon="save"
                        disabled={chosen === null || chosen === current}
                        onClick={() => {
                            onChoose(chosen);
                            onClose();
                        }}
                    >
                        {t('images.select')}
                    </Button>
                </>
            }
        >
            <div className="space-y-3">
                <label className="relative block">
                    <span className="pointer-events-none absolute inset-y-0 left-3 flex items-center text-ink-faint">
                        <Icon name="search" size={16} />
                    </span>
                    <Input
                        type="search"
                        className="pl-9"
                        value={typed}
                        placeholder={t('refdata.records.search')}
                        aria-label={t('refdata.records.search')}
                        onChange={(event) => setTyped(event.target.value)}
                    />
                </label>
                {page.isError && <Notice tone="error">{page.error.message}</Notice>}
                <ul
                    role="listbox"
                    aria-label={title}
                    className={`grid max-h-96 grid-cols-4 gap-2 overflow-auto sm:grid-cols-6 ${page.isPlaceholderData ? 'opacity-60' : ''}`}
                >
                    {images.map((image) => (
                        <li key={image.imageId}>
                            <button
                                type="button"
                                role="option"
                                aria-selected={chosen === image.imageId}
                                title={image.description}
                                className={`flex w-full flex-col items-center gap-1 rounded-md border p-2 text-xs ${chosen === image.imageId ? 'border-accent bg-accent/10' : 'border-line hover:bg-surface-hover'}`}
                                onClick={() => setChosen(image.imageId)}
                            >
                                <Flag src={imageUrl(image.imageId)} size="xl" />
                                <span className="w-full truncate font-mono text-ink-muted">
                                    {image.code}
                                </span>
                            </button>
                        </li>
                    ))}
                </ul>
                {page.data !== undefined && (
                    <Pager
                        offset={offset}
                        shown={images.length}
                        total={page.data.total}
                        pageSize={CHOOSER_PAGE}
                        showing={t('refdata.records.showing', {
                            first: String(bounds.first),
                            last: String(bounds.last),
                            total: String(page.data.total),
                        })}
                        onMove={setOffset}
                    />
                )}
            </div>
        </Dialog>
    );
}

/**
 * A form's image field: the chosen image, large, as a button that opens the
 * chooser. The desktop client's flag button, for every record that names an
 * image.
 */
export function ImageField({
    label,
    imageId,
    disabled,
    onChange,
}: {
    readonly label: string;
    readonly imageId: string;
    readonly disabled: boolean;
    readonly onChange: (imageId: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const [choosing, setChoosing] = useState(false);
    return (
        <div>
            <span className="mb-1.5 block text-sm font-medium text-ink-muted">{label}</span>
            <button
                type="button"
                disabled={disabled}
                title={t('images.choose')}
                aria-label={`${label}: ${t('images.choose')}`}
                className="flex h-14 w-20 items-center justify-center rounded-md border border-line bg-surface-base hover:border-line-strong disabled:opacity-50"
                onClick={() => setChoosing(true)}
            >
                {imageId === '' ? (
                    <span className="text-xs text-ink-faint">{t('images.none')}</span>
                ) : (
                    <Flag src={imageUrl(imageId)} size="xl" />
                )}
            </button>
            {choosing && (
                <ImageChooser
                    title={label}
                    current={imageId === '' ? null : imageId}
                    onChoose={(chosen) => onChange(chosen ?? '')}
                    onClose={() => setChoosing(false)}
                />
            )}
        </div>
    );
}

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

import { useQuery } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { api, type ImageMap } from '../api/client.js';
import { imageUrl } from '../ui/Images.js';

/**
 * What a flag stands for, which decides where its image comes from: a
 * currency or a country has its own; a calendar has its own or its country's;
 * a business centre has its country's; a pair draws both of its currencies.
 */
export type FlagSource = 'currency' | 'country' | 'calendar' | 'businessCentre' | 'pair';

/** How long the map is trusted before it is read again; writes refresh it at once. */
const IMAGE_MAP_FRESH_MS = 5 * 60 * 1000;

/**
 * The tenant's image map: the one place every screen learns which image a
 * flagged code uses. It is read once and shared, so a table of a hundred
 * flags costs one read, and each image's bytes are fetched once and kept by
 * the browser and the BFF.
 */
export function useImageMap() {
    return useQuery({
        queryKey: ['image-map'],
        queryFn: api.imageMap,
        staleTime: IMAGE_MAP_FRESH_MS,
        gcTime: Number.POSITIVE_INFINITY,
    });
}

const SOURCES: Readonly<Record<Exclude<FlagSource, 'pair'>, keyof Omit<ImageMap, 'noFlag'>>> = {
    currency: 'currencies',
    country: 'countries',
    calendar: 'calendars',
    businessCentre: 'businessCentres',
};

/**
 * Resolves flags. A code the map does not hold gets the placeholder flag, as
 * the desktop client did, so a missing image shows as missing rather than as
 * nothing; an empty code, or a map not read yet, gets no flag.
 */
export function useFlags(): {
    readonly flag: (source: Exclude<FlagSource, 'pair'>, code: string) => string | null;
    readonly pair: (code: string) => readonly [string | null, string | null];
} {
    const map = useImageMap().data;
    const flag = (source: Exclude<FlagSource, 'pair'>, code: string): string | null => {
        if (map === undefined || code === '') {
            return null;
        }
        const id = map[SOURCES[source]][code] ?? map.noFlag;
        return id === null ? null : imageUrl(id);
    };
    const pair = (code: string): readonly [string | null, string | null] => {
        const [base = '', quote = ''] = code.split('/');
        return [flag('currency', base), flag('currency', quote)];
    };
    return { flag, pair };
}

/**
 * One flag image, or nothing. A flag that fails to load is dropped rather than
 * drawn broken, so the words beside it always carry the meaning.
 */
export function Flag({
    src,
    size = 'sm',
}: {
    readonly src: string | null;
    readonly size?: 'sm' | 'lg' | 'xl';
}): ReactNode {
    const [failedSrc, setFailedSrc] = useState<string | null>(null);
    if (src === null || src === failedSrc) {
        return null;
    }
    const box = { sm: 'h-3.5 w-5', lg: 'h-6 w-9', xl: 'h-9 w-12' }[size];
    return (
        <img
            src={src}
            alt=""
            loading="lazy"
            onError={() => setFailedSrc(src)}
            className={`${box} shrink-0 rounded-[2px] object-cover ring-1 ring-line`}
        />
    );
}

/** The flag of one code of a source; a pair draws its two currencies side by side. */
export function FlagOf({
    source,
    code,
    size = 'sm',
}: {
    readonly source: FlagSource;
    readonly code: string;
    readonly size?: 'sm' | 'lg';
}): ReactNode {
    const flags = useFlags();
    if (source === 'pair') {
        const [base, quote] = flags.pair(code);
        return (
            <span className="inline-flex shrink-0 gap-0.5">
                <Flag src={base} size={size} />
                <Flag src={quote} size={size} />
            </span>
        );
    }
    return <Flag src={flags.flag(source, code)} size={size} />;
}

/**
 * A flagged code: its flag, then the code, then optionally its words. The code
 * is always written, so the cell reads the same with or without the flag. The
 * flag comes from the map by its source, or from an image address the caller
 * already holds, such as a party's flag read inside another tenant.
 */
export function FlaggedCode({
    code,
    source,
    src,
    label,
}: {
    readonly code: string;
    readonly source?: FlagSource;
    readonly src?: string | null;
    readonly label?: string;
}): ReactNode {
    return (
        <span className="inline-flex items-center gap-2">
            {source !== undefined ? (
                <FlagOf source={source} code={code} />
            ) : (
                <Flag src={src ?? null} />
            )}
            <span className="font-mono text-xs">{code}</span>
            {label !== undefined && label !== '' && <span>{label}</span>}
        </span>
    );
}

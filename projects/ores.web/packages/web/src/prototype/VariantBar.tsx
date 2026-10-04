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

/*
 * PROTOTYPE. Throwaway. Delete with the branch.
 *
 * The variant switcher every credentials prototype shares: a floating bar at
 * the foot of the page, a URL search parameter so a variant can be linked, and
 * a panel that prints the screen's whole state so a reviewer can see what an
 * action changed.
 */

import { useState, type ReactNode } from 'react';
import { useSearchParams } from 'react-router';
import { Button, Tag } from '../ui/Primitives.js';

export interface PrototypeVariant {
    readonly id: string;
    readonly name: string;
    readonly gist: string;
}

export interface VariantChoice {
    readonly active: PrototypeVariant;
    readonly choose: (id: string) => void;
}

/**
 * The variant named by ?variant=, or the fallback, or the first one.
 *
 * The tuple type is deliberate: a prototype with no variant has no switch to
 * show, so the caller must pass at least one.
 */
export function useVariant(
    variants: readonly [PrototypeVariant, ...PrototypeVariant[]],
    fallback: string,
): VariantChoice {
    const [params, setParams] = useSearchParams();
    const requested = params.get('variant');
    const active =
        variants.find((variant) => variant.id === requested) ??
        variants.find((variant) => variant.id === fallback) ??
        variants[0];
    const choose = (id: string): void => {
        const next = new URLSearchParams(params);
        next.set('variant', id);
        setParams(next, { replace: true });
    };
    return { active, choose };
}

export function VariantBar({
    variants,
    active,
    onChoose,
    state,
}: {
    readonly variants: readonly [PrototypeVariant, ...PrototypeVariant[]];
    readonly active: PrototypeVariant;
    readonly onChoose: (id: string) => void;
    readonly state: ReactNode;
}): ReactNode {
    const [showState, setShowState] = useState(true);
    return (
        <div className="fixed inset-x-0 bottom-0 z-50 border-t border-line bg-surface-overlay">
            <div className="mx-auto flex max-w-[1100px] flex-wrap items-center gap-2 px-5 py-2">
                <Tag tone="warn">PROTOTYPE</Tag>
                {variants.map((variant) => (
                    <Button
                        key={variant.id}
                        size="sm"
                        variant={variant.id === active.id ? 'primary' : 'ghost'}
                        onClick={() => onChoose(variant.id)}
                    >
                        {variant.name}
                    </Button>
                ))}
                <span className="min-w-0 flex-1 truncate text-xs text-ink-muted">
                    {active.gist}
                </span>
                <Button size="sm" variant="ghost" onClick={() => setShowState((shown) => !shown)}>
                    {showState ? 'Hide state' : 'Show state'}
                </Button>
            </div>
            {showState && (
                <div className="mx-auto max-h-[40vh] max-w-[1100px] overflow-y-auto border-t border-line-subtle px-5 py-3">
                    {state}
                </div>
            )}
        </div>
    );
}

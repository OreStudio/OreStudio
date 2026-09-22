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

import { useEffect, type ReactNode } from 'react';
import { useSearchParams } from 'react-router';

/**
 * PROTOTYPE ONLY. Cycles `?variant=` between throwaway UI variants.
 *
 * Hidden in production builds so a stray merge cannot ship it.
 */
export function usePrototypeVariant(keys: readonly string[]): string {
  const [params] = useSearchParams();
  const current = params.get('variant') ?? keys[0] ?? '';
  return keys.includes(current) ? current : (keys[0] ?? '');
}

export function PrototypeSwitcher({
  variants,
}: {
  readonly variants: readonly { readonly key: string; readonly name: string }[];
}): ReactNode {
  const [params, setParams] = useSearchParams();
  const keys = variants.map((v) => v.key);
  const current = usePrototypeVariant(keys);
  const index = keys.indexOf(current);

  const go = (delta: number): void => {
    const next = keys[(index + delta + keys.length) % keys.length] ?? current;
    const updated = new URLSearchParams(params);
    updated.set('variant', next);
    setParams(updated, { replace: true });
  };

  useEffect(() => {
    const onKey = (event: KeyboardEvent): void => {
      const target = event.target as HTMLElement | null;
      if (target?.closest('input, textarea, select, [contenteditable]') != null) return;
      if (event.key === 'ArrowLeft') go(-1);
      if (event.key === 'ArrowRight') go(1);
    };
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  });

  if (import.meta.env.PROD) return null;

  const name = variants[index]?.name ?? '';
  return (
    <div className="fixed bottom-4 left-1/2 z-50 flex -translate-x-1/2 items-center gap-3 rounded-full bg-fuchsia-600 px-4 py-2 text-sm font-medium text-white shadow-2xl">
      <button type="button" onClick={() => go(-1)} aria-label="Previous variant">
        ◀
      </button>
      <span>
        PROTOTYPE {current} ({name})
      </span>
      <button type="button" onClick={() => go(1)} aria-label="Next variant">
        ▶
      </button>
    </div>
  );
}

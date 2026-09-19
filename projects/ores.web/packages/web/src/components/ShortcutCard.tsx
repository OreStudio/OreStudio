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

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import { type IconName } from '../ui/icons/index.js';
import { MaskIcon } from '../ui/icons/MaskIcon.js';
import { cx } from '../ui/Primitives.js';

/**
 * A shortcut on a landing page.
 *
 * A card rather than a menu item because a landing page that shows nothing until
 * you choose something is a page that wastes the moment when a person is most
 * likely to be looking for something.
 *
 * A card whose destination is not built is rendered but marked, and it says so
 * rather than looking like a link that failed. That distinction matters: a dead
 * link teaches people to distrust the interface, and a marked one teaches them
 * what is coming.
 */
export function ShortcutCard({
  to,
  icon,
  title,
  description,
  planned = false,
  plannedLabel,
  comingSoonLabel,
}: {
  readonly to: string;
  readonly icon: IconName;
  readonly title: string;
  readonly description?: string;
  readonly planned?: boolean;
  readonly plannedLabel: string;
  readonly comingSoonLabel: string;
}): ReactNode {
  const body = (
    <>
      <span className="flex items-start gap-3">
        <span className="grid size-9 shrink-0 place-items-center rounded-md border border-line bg-bg-secondary">
          <MaskIcon name={icon} className="size-4.5 text-ink-muted" />
        </span>
        <span className="min-w-0 flex-1">
          <span className="flex items-center gap-2">
            <span className="truncate text-sm font-medium text-ink">{title}</span>
            {planned && (
              <span className="shrink-0 rounded-full border border-line px-1.5 py-px text-[10px] uppercase tracking-wide text-ink-faint">
                {plannedLabel}
              </span>
            )}
          </span>
          {description !== undefined && (
            <span className="mt-1 block text-xs leading-relaxed text-ink-muted">{description}</span>
          )}
          {planned && <span className="mt-1.5 block text-[11px] text-ink-faint">{comingSoonLabel}</span>}
        </span>
      </span>
    </>
  );

  const base =
    'block rounded-[var(--radius-card)] border border-line bg-bg-secondary p-4 transition-colors';

  if (planned) {
    // Not a link. A card that navigates to nothing is worse than one that does
    // not navigate.
    return <div className={cx(base, 'cursor-default opacity-70')}>{body}</div>;
  }

  return (
    <Link to={to} className={cx(base, 'hover:border-line-strong hover:bg-surface-overlay')}>
      {body}
    </Link>
  );
}

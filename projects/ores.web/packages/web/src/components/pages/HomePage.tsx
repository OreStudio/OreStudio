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
import { MaskIcon } from '../../ui/icons/MaskIcon.js';
import { Link } from 'react-router';
import { COMPONENTS } from '../registry.js';
import { humanise } from '../labels.js';
import { ShortcutGrid } from '../ShortcutGrid.js';
import { useSession } from '../../session/SessionProvider.js';
import { useTranslation } from '../../i18n/Provider.js';
import { ICONS } from '../../ui/icons/index.js';
import { cx } from '../../ui/Primitives.js';

/**
 * The signed-in home.
 *
 * Something is shown before anything is chosen, which a sidebar alone does not
 * provide: a person arriving with a task in mind should be able to start it, and
 * a person without one should be able to see what the system does.
 *
 * Two parts. The shortcuts are the tasks people repeat, taken from the components
 * so this page does not maintain its own list. The components below are the map,
 * with each one's size stated, because "Reference data" alone does not say
 * whether it is four screens or sixty.
 */
export function HomePage(): ReactNode {
  const { state } = useSession();
  const { t, plural } = useTranslation();

  const name = state.status === 'authenticated' ? state.session.username : '';

  // Every declared shortcut, across components, with its component for context.
  const shortcuts = COMPONENTS.flatMap((component) =>
    (component.shortcuts ?? []).map((shortcut) => ({ component, shortcut })),
  );

  return (
    <div className="mx-auto max-w-[1100px] px-5 py-7">
      <header className="mb-7">
        <h1 className="text-xl font-semibold tracking-tight">{t('home.quickActions')}</h1>
        {name.length > 0 && (
          <p className="mt-1 text-sm text-ink-muted">{t('home.greeting', { name })}</p>
        )}
      </header>

      <ShortcutGrid
        shortcuts={shortcuts.map(({ component, shortcut }) => ({
          ...shortcut,
          to: `/${component.path}/${shortcut.to}`,
        }))}
        basePath=""
      />

      <section className="mt-10">
        <h2 className="mb-3 text-sm font-medium text-ink-muted">{t('home.components')}</h2>
        <div className="grid gap-2 sm:grid-cols-2 xl:grid-cols-4">
          {COMPONENTS.map((component) => (
            <Link
              key={component.id}
              to={`/${component.path}`}
              className={cx(
                'flex items-center gap-3 rounded-[var(--radius-card)] border border-line',
                'bg-bg-secondary px-3.5 py-3 transition-colors hover:border-line-strong hover:bg-surface-overlay',
              )}
            >
              <img src={ICONS[component.icon]} alt="" aria-hidden className="size-4 opacity-70" />
              <span className="min-w-0 flex-1 truncate text-sm">{t(component.titleKey)}</span>
              <span className="shrink-0 text-xs tabular-nums text-ink-faint">
                {component.entities.length}
              </span>
            </Link>
          ))}
        </div>
      </section>

      <p className="mt-8 text-xs text-ink-faint">
        {plural('home.entities', COMPONENTS.reduce((n, c) => n + c.entities.length, 0))}
      </p>
    </div>
  );
}

/** Fallback for an entity whose screen is not built yet. */
export function PlannedPage({
  title,
  description,
}: {
  readonly title: string;
  readonly description?: string;
}): ReactNode {
  const { t } = useTranslation();
  return (
    <div className="mx-auto max-w-[680px] px-5 py-16 text-center">
      <span className="mx-auto mb-4 grid size-12 place-items-center rounded-full border border-line bg-bg-secondary">
        <MaskIcon name="clock" className="size-5 text-ink-faint" />
      </span>
      <h1 className="text-lg font-semibold tracking-tight">{title}</h1>
      {description !== undefined && <p className="mt-2 text-sm text-ink-muted">{description}</p>}
      <p className="mt-4 text-xs text-ink-faint">{t('card.notBuilt')}</p>
    </div>
  );
}

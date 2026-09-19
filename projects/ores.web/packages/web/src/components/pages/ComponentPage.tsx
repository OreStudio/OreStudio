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
import { Link, useLocation } from 'react-router';
import { findComponent } from '../registry.js';
import { translatedOrHumanised } from '../labels.js';
import { ShortcutGrid } from '../ShortcutGrid.js';
import { useTranslation } from '../../i18n/Provider.js';
import { ICONS } from '../../ui/icons/index.js';
import { cx } from '../../ui/Primitives.js';

/**
 * A component's landing page.
 *
 * The same idea as the home page, one level down: the component's common tasks as
 * cards, then every entity it declares. A person who followed a shortcut from the
 * home page arrives somewhere that shows what else the component holds, which is
 * how they discover the parts they did not know about.
 */
export function ComponentPage(): ReactNode {
  // Each component declares its own path, so the route carries no parameter and
  // the identifier is the first segment. That keeps the URLs readable —
  // /refdata rather than /component/refdata — which is worth more than the
  // convenience of a parameter.
  const { pathname } = useLocation();
  const componentId = pathname.split('/').filter(Boolean)[0];
  const { t, plural } = useTranslation();
  const component = findComponent(componentId);

  if (component === undefined) {
    return (
      <div className="mx-auto max-w-[680px] px-5 py-16 text-center text-sm text-ink-muted">
        {t('nav.noResults')}
      </div>
    );
  }

  const shortcuts = component.shortcuts ?? [];
  const built = component.entities.filter((entity) => entity.planned !== true).length;

  return (
    <div className="mx-auto max-w-[1100px] px-5 py-7">
      <header className="mb-7 flex items-start gap-3.5">
        <span className="grid size-10 shrink-0 place-items-center rounded-md border border-line bg-bg-secondary">
          <img src={ICONS[component.icon]} alt="" aria-hidden className="size-5 opacity-80" />
        </span>
        <div className="min-w-0">
          <h1 className="text-xl font-semibold tracking-tight">{t(component.titleKey)}</h1>
          {/*
            The size is stated because a component's name does not say whether it
            holds four screens or sixty, and that is the first thing somebody
            wants to know.
          */}
          <p className="mt-1 text-sm text-ink-muted">
            {plural('home.entities', component.entities.length)}
            {built < component.entities.length && (
              <span className="text-ink-faint">
                {' · '}
                {component.entities.length - built} {t('home.planned')}
              </span>
            )}
          </p>
        </div>
      </header>

      {shortcuts.length > 0 && (
        <section className="mb-9">
          <h2 className="mb-3 text-sm font-medium text-ink-muted">{t('component.shortcuts')}</h2>
          <ShortcutGrid shortcuts={shortcuts} basePath={`/${component.path}`} />
        </section>
      )}

      <section>
        <h2 className="mb-3 text-sm font-medium text-ink-muted">{t('component.entities')}</h2>
        {component.entities.length === 0 ? (
          <p className="text-sm text-ink-faint">{t('component.noEntities')}</p>
        ) : (
          <ul className="divide-y divide-line overflow-hidden rounded-[var(--radius-card)] border border-line bg-bg-secondary">
            {component.entities.map((entity) => {
              const label = translatedOrHumanised(t, `entity.${entity.id}.title`, entity.id);
              const row = (
                <>
                  <img src={ICONS[entity.icon]} alt="" aria-hidden className="size-4 shrink-0 opacity-70" />
                  <span className="min-w-0 flex-1 truncate text-sm text-ink">{label}</span>
                  {entity.planned === true && (
                    <span className="shrink-0 rounded-full border border-line px-1.5 py-px text-[10px] uppercase tracking-wide text-ink-faint">
                      {t('card.planned')}
                    </span>
                  )}
                </>
              );

              return (
                <li key={entity.id}>
                  {entity.planned === true ? (
                    <div className="flex cursor-default items-center gap-3 px-3.5 py-2.5 opacity-65">{row}</div>
                  ) : (
                    <Link
                      to={`/${component.path}/${entity.path}`}
                      className={cx(
                        'flex items-center gap-3 px-3.5 py-2.5 transition-colors',
                        'hover:bg-surface-overlay',
                      )}
                    >
                      {row}
                    </Link>
                  )}
                </li>
              );
            })}
          </ul>
        )}
      </section>
    </div>
  );
}

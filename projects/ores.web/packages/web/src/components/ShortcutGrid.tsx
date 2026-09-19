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
import { ShortcutCard } from './ShortcutCard.js';
import { humanise } from './labels.js';
import { useTranslation } from '../i18n/Provider.js';
import type { ShortcutDefinition } from './types.js';

/** A responsive grid of shortcut cards. */
export function ShortcutGrid({
  shortcuts,
  basePath,
}: {
  readonly shortcuts: readonly ShortcutDefinition[];
  readonly basePath: string;
}): ReactNode {
  const { t } = useTranslation();

  return (
    <div className="grid gap-3 sm:grid-cols-2 xl:grid-cols-3">
      {shortcuts.map((shortcut) => (
        <ShortcutCard
          key={shortcut.id}
          // A shortcut is component-relative, but one may be given absolute to
          // point outside its component, so only join relative paths.
          to={shortcut.to.startsWith('/') ? shortcut.to : `${basePath}/${shortcut.to}`}
          icon={shortcut.icon}
          title={
            shortcut.titleKey === undefined
              ? humanise(shortcut.id)
              : t(shortcut.titleKey)
          }
          {...(shortcut.descriptionKey === undefined
            ? {}
            : { description: t(shortcut.descriptionKey) })}
          planned={shortcut.planned === true}
          plannedLabel={t('card.planned')}
          comingSoonLabel={t('card.notBuilt')}
        />
      ))}
    </div>
  );
}

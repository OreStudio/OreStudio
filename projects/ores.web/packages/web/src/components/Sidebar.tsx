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
import { NavLink, useLocation } from 'react-router';
import { COMPONENTS, PLATFORM_COMPONENTS } from './registry.js';
import { humanise, translatedOrHumanised } from './labels.js';
import { useTranslation } from '../i18n/Provider.js';
import { type IconName } from '../ui/icons/index.js';
import { MaskIcon } from '../ui/icons/MaskIcon.js';
import { cx } from '../ui/Primitives.js';
import type { ComponentDefinition } from './types.js';

/**
 * The primary navigation.
 *
 * A persistent list of components, with the one you are in expanded to show its
 * entities. This replaces the Qt menu bar, which hid every destination behind a
 * click and showed a component's entities one hover at a time.
 *
 * Every component is visible, so a person can see what the system does even
 * before they know what to look for. Only one is expanded at a time, so the list
 * stays short enough to scan.
 */
export function Sidebar({ onNavigate }: { readonly onNavigate?: () => void }): ReactNode {
  const { pathname } = useLocation();
  const { t } = useTranslation();

  // The component whose section is open. Derived from the route so a deep link
  // opens the right section, and remembered once the person chooses another.
  const fromRoute = pathname.split('/').filter(Boolean)[0];
  const [open, setOpen] = useState<string | undefined>(fromRoute);
  const active = fromRoute ?? open;

  return (
    <nav className="flex h-full flex-col gap-1 overflow-y-auto p-3" aria-label={t('nav.menu')}>
      {COMPONENTS.map((component) => (
        <ComponentSection
          key={component.id}
          component={component}
          expanded={active === component.id}
          onToggle={() => setOpen(active === component.id ? undefined : component.id)}
          onNavigate={onNavigate}
        />
      ))}

      <div className="mt-auto pt-4">
        <div className="border-t border-line pt-3">
          {PLATFORM_COMPONENTS.map((component) => (
            <div key={component.id}>
              <p className="px-3 pb-1 text-[11px] font-medium uppercase tracking-wider text-ink-faint">
                {t(component.titleKey)}
              </p>
              {component.entities.map((entity) => (
                <SidebarLink
                  key={entity.id}
                  to={`/${component.path}/${entity.path}`}
                  icon={entity.icon}
                  label={translatedOrHumanised(t, `entity.${entity.id}.title`, entity.id)}
                  onNavigate={onNavigate}
                />
              ))}
            </div>
          ))}
        </div>
      </div>
    </nav>
  );
}

function ComponentSection({
  component,
  expanded,
  onToggle,
  onNavigate,
}: {
  readonly component: ComponentDefinition;
  readonly expanded: boolean;
  readonly onToggle: () => void;
  readonly onNavigate?: (() => void) | undefined;
}): ReactNode {
  const { t } = useTranslation();
  const label = t(component.titleKey);

  return (
    <div>
      <button
        type="button"
        onClick={onToggle}
        aria-expanded={expanded}
        className={cx(
          'flex w-full items-center gap-2.5 rounded-md px-3 py-2 text-left text-sm transition-colors',
          expanded ? 'bg-surface-overlay text-ink' : 'text-ink-muted hover:bg-surface-hover hover:text-ink',
        )}
      >
        <Icon name={component.icon} className="size-4 shrink-0 opacity-80" />
        <span className="flex-1 truncate">{label}</span>
        {/*
          The count is the honest signal of scale: it says how much is declared
          here, so a component with one built entity does not look finished.
        */}
        <span className="text-[11px] tabular-nums text-ink-faint">{component.entities.length}</span>
        <Chevron open={expanded} />
      </button>

      {expanded && (
        <ul className="mt-0.5 mb-1 space-y-0.5 pl-4">
          {component.entities.map((entity) => (
            <li key={entity.id}>
              <SidebarLink
                to={`/${component.path}/${entity.path}`}
                icon={entity.icon}
                label={translatedOrHumanised(t, `entity.${entity.id}.title`, entity.id)}
                planned={entity.planned === true}
                onNavigate={onNavigate}
              />
            </li>
          ))}
        </ul>
      )}
    </div>
  );
}

function SidebarLink({
  to,
  icon,
  label,
  planned,
  onNavigate,
}: {
  readonly to: string;
  readonly icon: IconName;
  readonly label: string;
  readonly planned?: boolean;
  readonly onNavigate?: (() => void) | undefined;
}): ReactNode {
  return (
    <NavLink
      to={to}
      onClick={onNavigate}
      className={({ isActive }) =>
        cx(
          'flex items-center gap-2.5 rounded-md px-3 py-1.5 text-sm transition-colors',
          isActive ? 'bg-accent-muted text-ink' : 'text-ink-muted hover:bg-surface-hover hover:text-ink',
        )
      }
    >
      <Icon name={icon} className="size-3.5 shrink-0 opacity-70" />
      <span className="truncate">{label}</span>
      {/* Not hidden, and not a link that fails silently: marked, and reachable
          to see what it will be. */}
      {planned === true && <span className="ml-auto size-1.5 shrink-0 rounded-full bg-ink-faint/40" />}
    </NavLink>
  );
}

function Icon({ name, className }: { readonly name: IconName; readonly className?: string }): ReactNode {
  return <MaskIcon name={name} {...(className === undefined ? {} : { className })} />;
}

function Chevron({ open }: { readonly open: boolean }): ReactNode {
  return (
    <svg
      viewBox="0 0 16 16"
      aria-hidden
      className={cx('size-3.5 shrink-0 text-ink-faint transition-transform', open && 'rotate-90')}
    >
      <path d="M6 4l4 4-4 4" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" />
    </svg>
  );
}

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
import { Outlet, useLocation } from 'react-router';
import { Sidebar } from './Sidebar.js';
import { TopBar } from './TopBar.js';
import { PageCrumbProvider } from './PageCrumb.js';
import { ChangeEventsProvider } from '../events/ChangeEvents.js';
import { cx } from '../ui/Primitives.js';

/**
 * The signed-in shell.
 *
 * A sidebar of components, a bar of application-level things, and the screen.
 * The Qt client was a menu bar over a desktop of floating windows; on the web the
 * navigation is a place you can see and the screen is a route you can link to.
 *
 * The sidebar collapses on a narrow window, because a permanent 260px column on a
 * laptop takes a third of the width for something you use occasionally.
 */
export function AppShell(): ReactNode {
  const [menuOpen, setMenuOpen] = useState(false);
  const { pathname } = useLocation();

  return (
    <div className="flex h-full flex-col bg-bg-primary">
      {/* The bar and the screen share the screen's name, so the bar can say what
          the route only knows the identifier of. */}
      <ChangeEventsProvider>
      <PageCrumbProvider>
        <TopBar onOpenMenu={() => setMenuOpen((open) => !open)} />

      <div className="flex min-h-0 flex-1">
        {/* Permanent from lg up, a drawer below it. */}
        <aside
          className={cx(
            'w-64 shrink-0 border-r border-line bg-bg-secondary',
            'max-lg:fixed max-lg:inset-y-0 max-lg:left-0 max-lg:z-30 max-lg:transition-transform',
            menuOpen ? 'max-lg:translate-x-0' : 'max-lg:-translate-x-full',
          )}
        >
          <div className="flex h-13 items-center border-b border-line px-3 lg:hidden">
            <span className="text-sm font-medium">Menu</span>
            <button
              type="button"
              onClick={() => setMenuOpen(false)}
              className="ml-auto rounded-md px-2 py-1 text-sm text-ink-muted hover:text-ink"
            >
              Close
            </button>
          </div>
          <div className="h-[calc(100%-3.25rem)] lg:h-full">
            <Sidebar onNavigate={() => setMenuOpen(false)} />
          </div>
        </aside>

        {menuOpen && (
          <button
            type="button"
            aria-label="Close menu"
            className="fixed inset-0 z-20 bg-black/40 lg:hidden"
            onClick={() => setMenuOpen(false)}
          />
        )}

        {/* The key remounts the screen on navigation, so scroll position and any
            local state belong to the route rather than leaking across it. */}
        <main key={pathname} className="min-w-0 flex-1 overflow-y-auto">
          <Outlet />
        </main>
        </div>
      </PageCrumbProvider>
      </ChangeEventsProvider>
    </div>
  );
}

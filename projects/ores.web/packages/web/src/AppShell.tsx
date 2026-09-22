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

import { type ReactNode } from 'react';
import { Link, NavLink, useLocation } from 'react-router';
import { useSession } from './session/SessionProvider.js';
import { useSiteState } from './api/site.js';
import { Button, Tag, cx } from './ui/Primitives.js';
import { PROJECT_SITE } from './assets/brand.js';
import { entityBasePath } from './entity/entityPaths.js';
import { accountDescriptor } from './generated/iam/web/account_declaration.js';
import icon from './assets/ore-studio-icon.png';

/**
 * The application chrome.
 *
 * A header with the mark and the menu, and a footer carrying the environment on
 * the left and the copyright on the right. The environment sits there rather
 * than in the header because it is a small, permanent fact about the
 * deployment, not an action, and because the worst failure mode is not knowing
 * which environment you are looking at.
 */
export function AppChrome({ children }: { readonly children: ReactNode }): ReactNode {
  const { state, signOut } = useSession();
  const { site } = useSiteState();
  const location = useLocation();
  const authenticated = state.status === 'authenticated';

  return (
    <div className="flex min-h-full flex-col">
      <header className="border-b border-line">
        <div className="mx-auto flex max-w-[1100px] items-center gap-6 px-6 py-4">
          <Link to="/" className="flex items-center gap-2.5">
            <img src={icon} alt="" className="size-7 rounded-md" />
            <span className="text-sm font-semibold tracking-tight">ORE Studio</span>
          </Link>

          <nav className="ml-auto flex items-center gap-1" aria-label="Main">
            {/* A link to the project, not a page that restates it. */}
            <HeaderLink to={PROJECT_SITE}>Site</HeaderLink>

            {authenticated ? (
              <>
                {/* The deployment's plumbing is only for whoever is already
                    inside. Before that it is none of anyone's business. */}
                {site?.developerTools === true && (
                  <HeaderLink to="/deployment">Deployment</HeaderLink>
                )}
                <HeaderLink to={entityBasePath(accountDescriptor)}>Accounts</HeaderLink>
                <Button variant="ghost" size="sm" onClick={() => void signOut()}>
                  Sign out
                </Button>
              </>
            ) : (
              /* One call to action before signing in. The landing page carries
                 the other, so the header does not need to say it too. */
              <Link to="/login">
                <Button variant="primary" size="sm">
                  Sign in
                </Button>
              </Link>
            )}
          </nav>
        </div>
      </header>

      <main className="mx-auto w-full max-w-[1100px] flex-1 px-6 py-10">{children}</main>

      <footer className="border-t border-line">
        <div className="mx-auto flex max-w-[1100px] items-center justify-between gap-4 px-6 py-4 text-xs text-ink-faint">
          <span className="flex items-center gap-2">
            {site !== undefined && (
              <>
                <span>{site.environment.displayName}</span>
                {site.environment.nonProduction && <Tag tone="warn">development</Tag>}
              </>
            )}
          </span>
          <span>© 2026 ORE Studio contributors.</span>
        </div>
      </footer>
    </div>
  );
}

function HeaderLink({ to, children }: { readonly to: string; readonly children: ReactNode }): ReactNode {
  const location = useLocation();
  const isExternal = to.startsWith('http');
  const base = 'rounded-md px-3 py-1.5 text-sm transition-colors';

  if (isExternal) {
    return (
      <a href={to} target="_blank" rel="noreferrer" className={cx(base, 'text-ink-muted hover:text-ink')}>
        {children}
      </a>
    );
  }
  return (
    <NavLink
      to={to}
      className={cx(
        base,
        location.pathname === to ? 'text-ink' : 'text-ink-muted hover:text-ink',
      )}
    >
      {children}
    </NavLink>
  );
}

/** The project site, so a link to it is one edit. */
export { PROJECT_SITE };

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
import { Link, NavLink } from 'react-router';
import { useSession } from '../session/SessionProvider.js';
import { useSiteState } from '../api/site.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Tag, cx } from '../ui/Primitives.js';
import { PROJECT_SITE } from '../assets/brand.js';
import icon from '../assets/ore-studio-icon.png';

/**
 * The shell for a visitor who has not signed in.
 *
 * Deliberately different from the signed-in shell. Before signing in there is
 * nothing to navigate, so a sidebar would be a list of locked doors; what matters
 * is the one action available and a link to the project. This is the same reason
 * the landing page is a hero rather than a dashboard.
 *
 * The environment still appears, because a person about to type a password
 * benefits from knowing which system they are typing it into.
 */
export function PublicShell({ children }: { readonly children: ReactNode }): ReactNode {
  return (
    <div className="flex min-h-full flex-col bg-bg-primary">
      <header className="border-b border-line">
        <div className="mx-auto flex max-w-[1100px] items-center gap-6 px-6 py-3">
          <Link to="/" className="flex items-center gap-2.5">
            <img src={icon} alt="" className="size-7 rounded-md" />
            <span className="text-sm font-semibold tracking-tight">ORE Studio</span>
          </Link>
          <PublicNav />
        </div>
      </header>

      <main className="mx-auto w-full max-w-[1100px] flex-1 px-6 py-10">{children}</main>

      <footer className="border-t border-line">
        <div className="mx-auto flex max-w-[1100px] items-center justify-between gap-4 px-6 py-4 text-xs text-ink-faint">
          <EnvironmentMark />
          <span>© 2026 ORE Studio contributors.</span>
        </div>
      </footer>
    </div>
  );
}

function PublicNav(): ReactNode {
  const { t } = useTranslation();
  const { state } = useSession();
  const authenticated = state.status === 'authenticated';

  return (
    <nav className="ml-auto flex items-center gap-1" aria-label="Main">
      {/* A link to the project, not a page that restates it. */}
      <a
        href={PROJECT_SITE}
        target="_blank"
        rel="noreferrer"
        className="rounded-md px-3 py-1.5 text-sm text-ink-muted transition-colors hover:text-ink"
      >
        {t('nav.site')}
      </a>

      {authenticated ? (
        <Link to="/iam/account">
          <Button variant="primary" size="sm">
            {t('nav.accounts')}
          </Button>
        </Link>
      ) : (
        <>
          <HeaderLink to="/signup">{t('landing.signUp')}</HeaderLink>
          <Link to="/login">
            <Button variant="primary" size="sm">
              {t('nav.signIn')}
            </Button>
          </Link>
        </>
      )}
    </nav>
  );
}

function HeaderLink({ to, children }: { readonly to: string; readonly children: ReactNode }): ReactNode {
  return (
    <NavLink
      to={to}
      className={({ isActive }) =>
        cx(
          'rounded-md px-3 py-1.5 text-sm transition-colors',
          isActive ? 'text-ink' : 'text-ink-muted hover:text-ink',
        )
      }
    >
      {children}
    </NavLink>
  );
}

/** The environment, for a person about to type a password into it. */
function EnvironmentMark(): ReactNode {
  const { site } = useSiteState();
  const { t } = useTranslation();
  if (site === undefined) return <span />;

  return (
    <span className="flex items-center gap-2">
      <span className="size-1.5 rounded-full bg-emerald-500/70" aria-hidden />
      <span>{site.environment.displayName}</span>
      {site.environment.nonProduction && <Tag tone="warn">{t('status.development')}</Tag>}
    </span>
  );
}

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
import { Link } from 'react-router';

/**
 * Sign up.
 *
 * A real destination rather than a dead button. Accounts come from an
 * administrator or from the provisioning wizard, so there is nothing to submit
 * yet, and saying so plainly is better than a form that cannot work.
 */
export function SignUpPage(): ReactNode {
  return (
    <div className="mx-auto max-w-[560px] py-16">
      <h1 className="text-3xl font-semibold tracking-tight">Sign up</h1>
      <p className="mt-4 text-base text-ink-muted">
        Accounts are created by an administrator, or through the provisioning wizard in the
        desktop client. Self-service sign-up is not available yet.
      </p>
      <p className="mt-6 text-sm">
        <Link to="/login" className="text-accent hover:text-accent-bright">
          Already have an account?
        </Link>
      </p>
    </div>
  );
}

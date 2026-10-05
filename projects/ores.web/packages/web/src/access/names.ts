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

/**
 * The name a surface shows for an account: the person's full name, or their
 * username when the account holds none, which is the state every account
 * created before names were recorded is in.
 *
 * The account may be absent, which is the shell before it has read the
 * signed-in person's own account, and the username the session states is the
 * answer then.
 */
export function displayName(
    account: { readonly fullName: string } | null | undefined,
    username: string,
): string {
    if (account == null || account.fullName === '') {
        return username;
    }
    return account.fullName;
}

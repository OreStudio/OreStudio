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

import { z } from 'zod';

/**
 * The HTTP contract for signing in and for the account screens.
 *
 * Small on purpose. The environment is deployment configuration, so a sign-in
 * carries an identity and nothing else. There is no server, port, namespace or
 * saved connection here, and there should never be one: a browser that can
 * name a host can ask the server to connect to it.
 */

export const credentialsSchema = z.object({
  username: z.string().min(1),
  password: z.string().min(1),
});
export type Credentials = z.infer<typeof credentialsSchema>;

export const selectPartySchema = z.object({
  partyId: z.string().min(1),
});
export type SelectPartyRequest = z.infer<typeof selectPartySchema>;

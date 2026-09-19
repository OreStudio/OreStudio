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
 * `@ores/contracts` holds the HTTP shapes the BFF and the browser agree on.
 *
 * Both sides import the same schema, so the browser validates what the server
 * serialised with the definition the server serialised it from. That is the
 * only way a network boundary gets checked without a code generator, and it
 * means a shape change fails loudly on whichever side is stale.
 *
 * It carries no Node dependency, because the browser loads it too.
 */
export * from './site.js';
export * from './session.js';

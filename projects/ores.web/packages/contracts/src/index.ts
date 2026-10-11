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
 * `@ores/contracts` holds the HTTP shapes the BFF serves.
 *
 * The BFF imports this package. The browser imports only the QA shapes in
 * `qa.ts`, which it parses with the schema the server serialised them from, so
 * a shape change fails loudly on whichever side is stale. The other shapes are
 * the BFF's alone, because the browser parses those with
 * `@ores/wire-protocol/browser`. Sharing one schema for all of them is the
 * intent.
 *
 * It carries no Node dependency, so the browser can load it when the split is
 * finished.
 */
export * from './site.js';
export * from './session.js';
export * from './qa.js';
export * from './agile.js';
export * from './curves.js';

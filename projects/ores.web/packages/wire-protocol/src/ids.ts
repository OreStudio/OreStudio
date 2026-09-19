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

import { randomUUID } from 'node:crypto';


/** Generates the per-request trace key the server groups logs by. */
export type IdGenerator = () => string;

export const nodeIdGenerator: IdGenerator = () => randomUUID();

/**
 * Generates a version 4 UUID without any runtime dependency.
 *
 * The BFF runs on Node, where `crypto.randomUUID` exists, but keeping the
 * generator here rather than reaching for a global lets the browser and a
 * future desktop shell supply their own. It also keeps the only Node import
 * in one place, so the browser entry point can leave it out.
 */
export const portableIdGenerator: IdGenerator = () => {
  const bytes = new Uint8Array(16);
  globalThis.crypto.getRandomValues(bytes);
  // Version 4 and the RFC 4122 variant.
  bytes[6] = ((bytes[6] ?? 0) & 0x0f) | 0x40;
  bytes[8] = ((bytes[8] ?? 0) & 0x3f) | 0x80;
  const hex = Array.from(bytes, (byte) => byte.toString(16).padStart(2, '0')).join('');
  return `${hex.slice(0, 8)}-${hex.slice(8, 12)}-${hex.slice(12, 16)}-${hex.slice(16, 20)}-${hex.slice(20)}`;
};

/** Headers every authenticated call carries, before any workspace overrides. */
export function tracingHeaders(sessionId: string, generateId: IdGenerator): Record<string, string> {
  const headers: Record<string, string> = {
    'Nats-Correlation-Id': generateId(),
  };
  if (sessionId.length > 0) {
    headers['Nats-Session-Id'] = sessionId;
  }
  return headers;
}

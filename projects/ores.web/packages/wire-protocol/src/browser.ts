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
 * The browser-safe surface of the protocol package.
 *
 * The default entry point pulls in the NATS client and its transport, which
 * reach for Node built-ins. A browser must never load any of that, and the
 * browser also has no business opening a broker connection: the BFF owns it.
 * This entry point therefore re-exports only data, schemas, and limits, and
 * deliberately does not re-export the client, the transport, or the codec.
 */

// Data and schemas.
export {
  LIVE_WORKSPACE_ID,
  SYSTEM_TENANT_ID,
  fromWireTimestamp,
  isUuid,
  isWireTimestamp,
  toWireTimestamp,
  uuid,
  wireTimestamp,
} from './primitives.js';
export type { Uuid, WireTimestamp } from './primitives.js';

export {
  ACCOUNT_TYPES,
  accountPageSchema,
  accountSchema,
  activePartySchema,
  partySummarySchema,
} from './domain.js';
export type { Account, AccountPage, AccountType, ActiveParty, PartySummary } from './domain.js';

export {
  apiErrorSchema,
  loginResultSchema,
  loginSuccessSchema,
  partyChoiceSchema,
  sessionViewSchema,
  sseEnvelopeSchema,
} from './contracts.js';
export type {
  ApiError,
  LoginResult,
  LoginSuccess,
  PartyChoice,
  SessionView,
} from './contracts.js';

// Subjects, so a browser-side module can name one without importing the
// transport that would know how to reach it.
export { SUBJECTS } from './operations.js';

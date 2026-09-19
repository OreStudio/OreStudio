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

import type { EnvironmentDefinition, SiteConfiguration } from '@ores/contracts';
import { tlsMaterialFor } from './site-config.js';

/**
 * Where the process connects.
 *
 * The site configuration names every environment a deployment may be pointed
 * at. The process environment names the deployment this process actually runs
 * in, and the checkout's `.env` is the authority for it, so it wins: a site
 * file describing many environments must not send a checkout's own process
 * somewhere else.
 */

/** Address of the broker beside this process, with the scheme. */
export const NATS_URL_VARIABLE = 'ORES_NATS_URL';
/** Subject namespace this process publishes and subscribes under. */
export const NATS_SUBJECT_PREFIX_VARIABLE = 'ORES_NATS_SUBJECT_PREFIX';
export const NATS_TLS_CA_VARIABLE = 'ORES_NATS_TLS_CA';
export const NATS_TLS_CERT_VARIABLE = 'ORES_NATS_TLS_CERT';
export const NATS_TLS_KEY_VARIABLE = 'ORES_NATS_TLS_KEY';

export interface BrokerTarget {
  readonly server: string;
  readonly subjectPrefix: string;
  readonly tls: {
    readonly ca: string;
    readonly cert: string;
    readonly key: string;
  };
}

/**
 * Resolves the broker from the process environment, falling back to the site
 * configuration when the process names none.
 */
export function resolveBroker(
  site: SiteConfiguration,
  environment: EnvironmentDefinition,
  processEnvironment: NodeJS.ProcessEnv = process.env,
): BrokerTarget {
  const url = nonEmpty(processEnvironment[NATS_URL_VARIABLE]);
  if (url !== undefined) {
    return {
      server: url,
      subjectPrefix:
        nonEmpty(processEnvironment[NATS_SUBJECT_PREFIX_VARIABLE]) ?? environment.subjectPrefix,
      tls: {
        ca: processEnvironment[NATS_TLS_CA_VARIABLE] ?? '',
        cert: processEnvironment[NATS_TLS_CERT_VARIABLE] ?? '',
        key: processEnvironment[NATS_TLS_KEY_VARIABLE] ?? '',
      },
    };
  }
  return {
    server: `nats://${environment.host}:${environment.port}`,
    subjectPrefix: environment.subjectPrefix,
    tls: tlsMaterialFor(site, environment),
  };
}

/** Reads a variable, treating whitespace as absent. */
function nonEmpty(value: string | undefined): string | undefined {
  const trimmed = value?.trim();
  return trimmed !== undefined && trimmed.length > 0 ? trimmed : undefined;
}

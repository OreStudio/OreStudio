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
 * The BFF's own configuration.
 *
 * This is about the process: the port it listens on, the session cookie, and
 * where the site configuration lives. Where the application points is in the
 * site configuration file, not here, because that is a different kind of fact
 * and it changes for different reasons.
 */

const environmentSchema = z.object({
  /** Port the browser-facing HTTP server listens on. */
  ORES_WEB_BFF_PORT: z.coerce.number().int().positive().default(8080),
  ORES_WEB_BFF_HOST: z.string().default('127.0.0.1'),

  ORES_WEB_LOG_LEVEL: z.string().default('info'),

  /** Secret for the session cookie. Required: a default would be a backdoor. */
  ORES_WEB_SESSION_SECRET: z.string().min(32),
  ORES_WEB_SESSION_TTL_SECONDS: z.coerce.number().int().positive().default(8 * 60 * 60),
  /**
   * `secure` cookies require HTTPS. Off by default so a development server on
   * plain HTTP works, and it must be on anywhere else.
   */
  ORES_WEB_COOKIE_SECURE: z
    .enum(['true', 'false'])
    .default('false')
    .transform((value) => value === 'true'),
  /** Origins allowed to call the API, comma separated. */
  ORES_WEB_ALLOWED_ORIGINS: z
    .string()
    .default('http://localhost:21802')
    .transform((value) => value.split(',').map((origin) => origin.trim()).filter(Boolean)),
  /**
   * Login attempts allowed per client per minute.
   *
   * The IAM service counts failed attempts and locks an account, so this is not
   * the security boundary; it stops a runaway client from driving that counter
   * with unbounded traffic.
   */
  ORES_WEB_LOGIN_ATTEMPTS_PER_MINUTE: z.coerce.number().int().positive().default(10),

  /** Where the site configuration lives, when it is not the repository default. */
  ORES_WEB_SITE_CONFIG: z.string().optional(),
  /** Chooses the environment, equivalent to `--env`. */
  ORES_WEB_ENV: z.string().optional(),
  /** Overrides the developer surface setting. */
  ORES_WEB_DEVELOPER_TOOLS: z.string().optional(),
});

export type ConfigurationInput = z.input<typeof environmentSchema>;

export interface Config {
  readonly port: number;
  readonly host: string;
  readonly logLevel: string;
  readonly session: {
    readonly secret: string;
    readonly ttlSeconds: number;
    readonly cookieSecure: boolean;
  };
  readonly allowedOrigins: readonly string[];
  readonly loginAttemptsPerMinute: number;
}

/** Builds a configuration from already-validated values, for tests. */
export function buildConfig(input: ConfigurationInput): Config {
  const parsed = environmentSchema.safeParse(input);
  if (!parsed.success) {
    throw new Error(`Invalid configuration:\n${z.prettifyError(parsed.error)}`);
  }
  const env = parsed.data;
  return {
    port: env.ORES_WEB_BFF_PORT,
    host: env.ORES_WEB_BFF_HOST,
    logLevel: env.ORES_WEB_LOG_LEVEL,
    session: {
      secret: env.ORES_WEB_SESSION_SECRET,
      ttlSeconds: env.ORES_WEB_SESSION_TTL_SECONDS,
      cookieSecure: env.ORES_WEB_COOKIE_SECURE,
    },
    allowedOrigins: env.ORES_WEB_ALLOWED_ORIGINS,
    loginAttemptsPerMinute: env.ORES_WEB_LOGIN_ATTEMPTS_PER_MINUTE,
  };
}

/** Reads configuration from the environment. */
export function loadConfig(environment: NodeJS.ProcessEnv = process.env): Config {
  return buildConfig(environment as ConfigurationInput);
}

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
 * The ORE Studio environments this site can be pointed at.
 *
 * Connecting is a deployment concern, so it is configuration rather than
 * something a person chooses at sign-in. All environments are declared in one
 * file, and the site is started against one of them:
 *
 *   npm run dev:bff -- --env bright_hopper
 *
 * That is the whole of it. Nobody signing in needs to know what a namespace, a
 * port or a certificate is, and the browser never learns where the server
 * points, which means it cannot ask to connect anywhere else.
 */

export const environmentSchema = z.object({
  /**
   * The identifier used on the command line.
   *
   * Stable and never displayed. A person refers to it in scripts and in a
   * deployment, so it must not change when the display name does.
   */
  id: z.string().regex(/^[a-z][a-z0-9_]*$/),
  /** What the sign-in screen shows, so it must read well. */
  displayName: z.string().min(1),
  description: z.string().default(''),
  /** NATS server address, without a scheme. */
  host: z.string().min(1),
  /** NATS client port. */
  port: z.int().min(1).max(65535),
  /** Subject namespace that isolates this environment on a shared broker. */
  subjectPrefix: z.string().min(1),
  /** The companion HTTP server, when the deployment runs one. */
  httpBaseUrl: z.string().default(''),
  /**
   * Per-environment certificate overrides.
   *
   * Each service presents its own client certificate, and a deployment may
   * keep them apart from the CA, so these override the shared values.
   */
  tls: z
    .object({
      ca: z.string().optional(),
      cert: z.string().optional(),
      key: z.string().optional(),
    })
    .default({}),
  /**
   * Marks a deployment that is not production.
   *
   * The interface uses it to say so permanently and unmistakably, because the
   * cost of mistaking one environment for another is high.
   */
  nonProduction: z.boolean().default(true),
});

export type EnvironmentDefinition = z.infer<typeof environmentSchema>;

/**
 * One test identity a developer can sign in as.
 *
 * These are ordinary accounts, not impersonation. They exist in the ACME test
 * setup with a shared, well-known password, and a developer signs in as one in
 * exactly the way any other person signs in. The list is a convenience for
 * filling in the form, nothing more.
 */
export const developerAccountSchema = z.object({
  username: z.string().min(1),
  label: z.string().default(''),
  /** What the account is useful for testing, so the list can be scanned. */
  description: z.string().default(''),
});

export type DeveloperAccount = z.infer<typeof developerAccountSchema>;

export const siteConfigurationSchema = z
  .object({
    /** Which environment this site serves. Must match one of `environments`. */
    active: z.string().default(''),
    environments: z.array(environmentSchema).min(1),
    /**
     * Shared certificate paths, used when an environment does not override
     * them. They may be file paths or inline PEM.
     */
    tls: z.object({
      ca: z.string().default(''),
      cert: z.string().default(''),
      key: z.string().default(''),
    }),
    /**
     * Whether the developer surface is offered.
     *
     * Deployment configuration rather than a browser preference, so it is
     * auditable and cannot be switched on by clearing storage. It only ever
     * reveals a list of test accounts.
     */
    developerTools: z.boolean().default(false),
    /** Test identities, offered only when `developerTools` is on. */
    developerAccounts: z.array(developerAccountSchema).default([]),
  })
  .superRefine((value, ctx) => {
    // A file that names an environment it does not define is a mistake worth
    // failing on, because the alternative is a site that starts and then
    // cannot connect to anything.
    const ids = new Set(value.environments.map((environment) => environment.id));
    for (const environment of value.environments) {
      if (ids.size !== value.environments.length) {
        const duplicates = value.environments
          .map((item) => item.id)
          .filter((id, index, all) => all.indexOf(id) !== index);
        ctx.addIssue({
          code: 'custom',
          message: `Duplicate environment id(s): ${[...new Set(duplicates)].join(', ')}`,
        });
        break;
      }
    }
    if (value.active.length > 0 && !ids.has(value.active)) {
      ctx.addIssue({
        code: 'custom',
        message: `active environment '${value.active}' is not defined; known: ${[...ids].join(', ')}`,
        path: ['active'],
      });
    }
  });

export type SiteConfiguration = z.infer<typeof siteConfigurationSchema>;

/** What the browser is told about the environment it is signed in to. */
export const environmentViewSchema = z.object({
  id: z.string(),
  displayName: z.string(),
  description: z.string(),
  nonProduction: z.boolean(),
});
export type EnvironmentView = z.infer<typeof environmentViewSchema>;

/**
 * The site's own state, as the interface needs it.
 *
 * Deliberately small. The browser is told which environment it is talking to
 * and whether the developer surface exists. It is not told the host, the port,
 * the namespace or the certificates, because none of that is any of its
 * business.
 */
export const siteStateSchema = z.object({
  appName: z.string(),
  environment: environmentViewSchema,
  developerTools: z.boolean(),
  /** Test identities, empty unless the developer surface is on. */
  developerAccounts: z.array(developerAccountSchema),
});
export type SiteState = z.infer<typeof siteStateSchema>;

/**
 * The deployment's plumbing, for the developer page.
 *
 * A separate shape from {@link SiteState} on purpose. The ordinary interface is
 * never told the host, the port or the namespace; this is served only when the
 * deployment has the developer surface switched on, and it exists so a
 * developer can see what they are actually connected to.
 */
export const deploymentViewSchema = z.object({
  environment: environmentSchema,
  configFile: z.string(),
  /** Whether the environment was chosen on the command line or in the file. */
  developerTools: z.boolean(),
  /** Every environment the configuration declares, so the others are visible. */
  available: z.array(
    z.object({
      id: z.string(),
      displayName: z.string(),
      nonProduction: z.boolean(),
    }),
  ),
});
export type DeploymentView = z.infer<typeof deploymentViewSchema>;

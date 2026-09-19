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

import { parseArgs } from 'node:util';
import { buildServer } from './server.js';
import { loadConfig } from './config.js';
import { loadSiteConfiguration } from './site-config.js';

/**
 * Entry point.
 *
 * Configuration is validated before anything binds, so a missing certificate
 * or session secret stops the process with a readable message rather than
 * failing on the first login.
 */
async function main(): Promise<void> {
  const { values } = parseArgs({
    options: {
      env: { type: 'string', short: 'e' },
      help: { type: 'boolean', default: false },
    },
    allowPositionals: true,
  });

  if (values.help === true) {
    process.stdout.write(
      'Usage: ores.web.bff [--env <environment>]\n\n' +
        '  --env, -e   Which ORE Studio environment to serve. Overrides the\n' +
        '              site configuration. See config/environments.json.\n',
    );
    return;
  }

  const config = loadConfig();
  const site = loadSiteConfiguration({
    projectRoot: process.cwd(),
    ...(values.env === undefined ? {} : { environmentId: values.env }),
  });

  const server = buildServer({ config, site });

  // Say which environment this process serves, first thing, because the worst
  // failure mode is not knowing whether you are looking at staging or
  // production.
  server.log.info(
    {
      environment: site.environment.id,
      displayName: site.environment.displayName,
      nonProduction: site.environment.nonProduction,
      developerTools: site.configuration.developerTools,
      configFile: site.source,
    },
    `serving '${site.environment.displayName}' (${site.environment.id})`,
  );

  const shutdown = async (signal: string): Promise<void> => {
    server.log.info({ signal }, 'shutting down');
    await server.close();
    process.exit(0);
  };
  process.on('SIGINT', () => void shutdown('SIGINT'));
  process.on('SIGTERM', () => void shutdown('SIGTERM'));

  await server.listen({ port: config.port, host: config.host });

  // `compass services start` reads the standard output log for this exact
  // line to decide the service is ready, so it must follow the successful
  // bind. See render_node_unit in projects/ores.compass/src/systemd_generate.py.
  server.log.info('Service ready');
}

main().catch((error: unknown) => {
  console.error('failed to start:', error);
  process.exit(1);
});

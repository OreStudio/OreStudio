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

import { useQuery } from '@tanstack/react-query';
import { type ReactNode } from 'react';
import { deploymentViewSchema } from '@ores/contracts';
import { request } from '../api/transport.js';
import { Detail, Notice, Tag } from '../ui/Primitives.js';

/**
 * The developer page.
 *
 * Everything about the deployment that a person signing in should not have to
 * think about: which environment this process serves, where it points, which
 * configuration file chose it, and what else that file declares.
 *
 * It is a separate page rather than something folded into the landing page, so
 * the ordinary experience stays clean and the plumbing is available to whoever
 * needs it.
 */
export function DeveloperPage(): ReactNode {
  const query = useQuery({
    queryKey: ['deployment'],
    queryFn: async () => deploymentViewSchema.parse(await request('/api/site/deployment', { method: 'GET' })),
    staleTime: Number.POSITIVE_INFINITY,
  });

  if (query.isPending) {
    return <p className="text-sm text-ink-faint">Loading...</p>;
  }
  if (query.isError || query.data === undefined) {
    return (
      <Notice tone="error">
        This deployment does not offer the developer surface. It is set in the configuration
        file, not in the browser.
      </Notice>
    );
  }

  const { environment, configFile, available } = query.data;

  return (
    <div className="mx-auto max-w-[920px]">
      <h1 className="border-b border-line pb-4 text-3xl font-semibold tracking-tight">
        Deployment
      </h1>
      <p className="mt-4 text-sm text-ink-muted">
        Read-only. Everything here is decided when the process starts, not in the browser.
      </p>

      <h2 className="mt-10 border-b border-line pb-2 text-xl font-semibold">This environment</h2>
      <dl className="mt-5 grid gap-5 sm:grid-cols-3">
        <Detail label="Name" value={environment.displayName} />
        <Detail label="Identifier" value={environment.id} mono />
        <Detail label="Kind" value={environment.nonProduction ? 'not production' : 'production'} />
        <Detail label="NATS server" value={`${environment.host}:${environment.port}`} mono />
        <Detail label="Subject namespace" value={environment.subjectPrefix} mono />
        <Detail label="HTTP server" value={environment.httpBaseUrl || 'not configured'} mono />
      </dl>
      {environment.description.length > 0 && (
        <p className="mt-4 text-sm text-ink-muted">{environment.description}</p>
      )}

      <h2 className="mt-10 border-b border-line pb-2 text-xl font-semibold">Configuration</h2>
      <dl className="mt-5 grid gap-5">
        <Detail label="File" value={configFile} mono />
      </dl>
      <p className="mt-3 text-sm text-ink-muted">
        Chosen at start with <code className="font-mono text-xs">--env</code> or{' '}
        <code className="font-mono text-xs">ORES_WEB_ENV</code>. The file is validated when the
        process starts, so a name that does not exist fails there rather than on the first
        sign-in.
      </p>

      <h2 className="mt-10 border-b border-line pb-2 text-xl font-semibold">
        Every environment declared
      </h2>
      <div className="mt-5 overflow-hidden rounded-[var(--radius-card)] border border-line">
        <table className="w-full text-sm">
          <thead>
            <tr className="border-b border-line text-left text-[11px] uppercase tracking-wider text-ink-faint">
              <th className="px-4 py-2 font-medium">Identifier</th>
              <th className="px-4 py-2 font-medium">Name</th>
              <th className="px-4 py-2 font-medium">Kind</th>
              <th className="px-4 py-2" />
            </tr>
          </thead>
          <tbody>
            {available.map((item) => (
              <tr key={item.id} className="border-b border-line-subtle last:border-0">
                <td className="px-4 py-2">{item.displayName}</td>
                <td className="px-4 py-2 font-mono text-xs text-ink-muted">{item.id}</td>
                <td className="px-4 py-2">
                  {item.nonProduction ? <Tag tone="warn">not production</Tag> : <Tag>production</Tag>}
                </td>
                <td className="px-4 py-2 text-right">
                  {item.id === environment.id && <Tag tone="accent">serving</Tag>}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      <p className="mt-3 text-sm text-ink-muted">
        Switch by restarting with a different <code className="font-mono text-xs">--env</code>.
      </p>
    </div>
  );
}

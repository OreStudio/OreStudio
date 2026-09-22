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

import { Fragment, useCallback, useEffect, useState, type ReactNode } from 'react';
import { Button, Field, Input, PageHeader, cx } from '../../../ui/Primitives.js';
import { JourneyPage, type JourneyStep } from './journey.js';
import { FailToggle, ProgressList } from './parts.js';
import { useSimulatedRun } from './stub.js';

/**
 * PROTOTYPE ONLY -- throwaway. The new party journey, on the same journey page
 * as the tenant journeys. The tenant admin adds a legal entity to their tenant.
 * The party's bundles come from the tenant's seed profile, so there is nothing
 * to choose about data. All data is stubbed.
 */

interface LegalEntity {
  readonly lei: string;
  readonly name: string;
  readonly country: string;
  readonly parent?: string;
}

const GLEIF_RESULTS: readonly LegalEntity[] = [
  { lei: '549300NWCAPITAL00001', name: 'Northwind Capital Ltd', country: 'GB' },
  { lei: '549300NWMARKETS00002', name: 'Northwind Markets LLC', country: 'US', parent: 'Northwind Capital Ltd' },
  { lei: '549300NWASIAPAC00003', name: 'Northwind Asia Pacific Pte Ltd', country: 'SG', parent: 'Northwind Capital Ltd' },
];

const TENANT_ACCOUNTS = ['tenant_admin', 'j.smith', 'a.tanaka', 'm.okafor'];

const PARTY_STEPS = [
  'Create the party',
  'Activate it',
  'Publish its essential data',
  'Link its accounts',
  'Complete',
];

const PROVISIONING = 3;

export function NewPartyJourneyPrototype(): ReactNode {
  const [at, setAt] = useState(0);
  const [query, setQuery] = useState('Northwind');
  const [entity, setEntity] = useState<LegalEntity>();
  const [shortName, setShortName] = useState('');
  const [accounts, setAccounts] = useState<ReadonlySet<string>>(new Set(['tenant_admin']));
  const [started, setStarted] = useState(false);
  const [failOnce, setFailOnce] = useState(false);
  const run = useSimulatedRun(entity && { code: entity.lei, steps: PARTY_STEPS }, started, failOnce ? 2 : undefined);

  const advance = useCallback(() => setAt(PROVISIONING + 1), []);
  useEffect(() => {
    if (at === PROVISIONING && run.status === 'done') advance();
  }, [at, run.status, advance]);

  const pick = (e: LegalEntity): void => {
    setEntity(e);
    setShortName(e.name.replace(/ (Ltd|LLC|Pte Ltd|plc|Inc)$/u, ''));
  };
  const toggle = (account: string): void => {
    const next = new Set(accounts);
    if (next.has(account)) next.delete(account);
    else next.add(account);
    setAccounts(next);
  };

  const matches = GLEIF_RESULTS.filter(
    (e) => query.trim() !== '' && (e.name.toLowerCase().includes(query.toLowerCase()) || e.lei.startsWith(query.toUpperCase())),
  );

  const header =
    entity !== undefined ? (
      <div className="mb-5 flex items-center gap-3 border-b border-line pb-4 text-sm">
        <div>
          <div className="font-medium">{shortName || entity.name}</div>
          <div className="font-mono text-xs text-ink-faint">{entity.lei}</div>
        </div>
      </div>
    ) : undefined;

  const steps: readonly JourneyStep[] = [
    {
      id: 'entity',
      title: 'Find the legal entity',
      lead: 'Search the GLEIF register by name or LEI.',
      body: (
        <div className="space-y-4">
          <Field label="Name or LEI">
            <Input value={query} onChange={(e) => setQuery(e.target.value)} placeholder="Northwind, or 549300…" />
          </Field>
          <ul role="listbox" aria-label="Matching legal entities" className="space-y-2">
            {matches.map((e) => (
              <li key={e.lei}>
                <button
                  type="button"
                  role="option"
                  aria-selected={entity?.lei === e.lei}
                  onClick={() => pick(e)}
                  className={cx(
                    'card flex w-full items-baseline justify-between gap-4 p-3 text-left',
                    entity?.lei === e.lei ? 'border-accent ring-3 ring-accent/20' : 'hover:border-line-strong',
                  )}
                >
                  <span>
                    <span className="font-medium">{e.name}</span>
                    {e.parent !== undefined && <span className="block text-xs text-ink-faint">Subsidiary of {e.parent}</span>}
                  </span>
                  <span className="shrink-0 font-mono text-xs text-ink-faint">
                    {e.country} · {e.lei}
                  </span>
                </button>
              </li>
            ))}
            {matches.length === 0 && <li className="text-sm text-ink-faint">No legal entity matches.</li>}
          </ul>
          <Button variant="ghost" size="sm" className="-ml-3">
            It has no LEI: add it by name
          </Button>
        </div>
      ),
      next: { label: 'Continue', enabled: entity !== undefined },
    },
    {
      id: 'describe',
      title: 'Describe the party',
      lead: 'Name it and choose the accounts that work in it.',
      ...(header !== undefined && { header }),
      body: (
        <div className="space-y-5">
          <Field label="Short name" hint="Shown in menus and the party switcher.">
            <Input value={shortName} onChange={(e) => setShortName(e.target.value)} />
          </Field>
          <fieldset>
            <legend className="text-sm font-medium text-ink-muted">Accounts that work in it</legend>
            <p className="mb-2 text-xs text-ink-faint">They can sign in to this party and act for it.</p>
            <div className="grid gap-1 sm:grid-cols-2">
              {TENANT_ACCOUNTS.map((a) => (
                <label key={a} className="flex items-center gap-2 text-sm">
                  <input type="checkbox" checked={accounts.has(a)} onChange={() => toggle(a)} />
                  <span className="font-mono">{a}</span>
                </label>
              ))}
            </div>
          </fieldset>
        </div>
      ),
      next: { label: 'Continue', enabled: shortName.trim() !== '' && accounts.size > 0 },
    },
    {
      id: 'review',
      title: 'Review',
      lead: 'Nothing is created until you confirm.',
      ...(header !== undefined && { header }),
      body: (
        <>
          <dl className="grid gap-2 text-sm sm:grid-cols-2">
            {[
              ['Legal entity', entity?.name ?? '-'],
              ['LEI', entity?.lei ?? '-'],
              ['Short name', shortName],
              ['Accounts', [...accounts].join(', ')],
              ['Data', "The tenant's standard party data"],
            ].map(([k, v]) => (
              <Fragment key={k}>
                <dt className="text-ink-faint">{k}</dt>
                <dd>{v}</dd>
              </Fragment>
            ))}
          </dl>
          <div className="mt-4">
            <FailToggle value={failOnce} onChange={setFailOnce} />
          </div>
        </>
      ),
      next: { label: 'Add party', enabled: true, onNext: () => setStarted(true) },
    },
    {
      id: 'provisioning',
      title: 'Provisioning',
      lead: 'This runs on the server. You can leave this page and come back.',
      ...(header !== undefined && { header }),
      final: true,
      body: <ProgressList run={run} onRetry={run.retry} />,
    },
    {
      id: 'next',
      title: 'Next steps',
      lead: `${shortName} is active.`,
      ...(header !== undefined && { header }),
      final: true,
      body: (
        <div className="grid gap-3 sm:grid-cols-3">
          {[
            ['Switch to it', `Work in ${shortName} now.`],
            ['Set up its books', 'Business units, portfolios and books.'],
            ['Add another party', 'Start this journey again.'],
          ].map(([title, text]) => (
            <button key={title} type="button" className="card p-4 text-left hover:border-accent">
              <span className="font-semibold">{title}</span>
              <p className="mt-1 text-sm text-ink-muted">{text}</p>
            </button>
          ))}
        </div>
      ),
    },
  ];

  return (
    <>
      <PageHeader title="New party" />
      <JourneyPage steps={steps} at={at} onMove={setAt} />
    </>
  );
}

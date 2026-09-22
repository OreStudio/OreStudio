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

import { Fragment, useState, type ReactNode } from 'react';
import { Button, PageHeader, cx } from '../../../ui/Primitives.js';
import { DetailsForm, FailToggle, Handoff, JOURNEY_STEPS, ProfileChoice, ProgressList, StepLead } from './parts.js';
import { PROFILES, emptyDetails, useSimulatedRun, type TenantDetails } from './stub.js';

/**
 * PROTOTYPE ONLY -- throwaway. The new tenant journey, variant A (the chosen
 * stepper page). The rejected variants are on the prototype/new-tenant-journey
 * branch. All data is stubbed; nothing is sent to the server.
 */

/** The state every variant needs: profile, details, and a simulated run. */
function useJourney() {
  const [profileCode, setProfileCode] = useState<string>();
  const profile = PROFILES.find((p) => p.code === profileCode);
  const [details, setDetails] = useState<TenantDetails>();
  const [started, setStarted] = useState(false);
  const [failOnce, setFailOnce] = useState(false);
  const run = useSimulatedRun(profile, started, failOnce ? 2 : undefined);

  const choose = (code: string): void => {
    setProfileCode(code);
    const p = PROFILES.find((x) => x.code === code);
    if (p !== undefined) setDetails(emptyDetails(p));
  };
  const restart = (): void => {
    setStarted(false);
    setProfileCode(undefined);
    setDetails(undefined);
  };
  return { profile, details, setDetails, started, setStarted, failOnce, setFailOnce, run, choose, restart };
}

type Journey = ReturnType<typeof useJourney>;

/** A dedicated page with a flat step rail; one step at a time. */
function NewTenantSteps({ j }: { readonly j: Journey }): ReactNode {
  const steps = JOURNEY_STEPS.map((s) => s.title);
  const [at, setAt] = useState(0);
  const current = j.started ? (j.run.status === 'done' ? 4 : 3) : at;

  return (
    <div className="grid gap-8 md:grid-cols-[14rem_1fr]">
      <nav aria-label="Journey steps">
        <ol className="space-y-1">
          {steps.map((label, i) => (
            <li
              key={label}
              className={cx(
                'flex items-center gap-3 rounded-md px-3 py-2 text-sm',
                i === current ? 'bg-surface-hover font-medium text-ink' : i < current ? 'text-ink-muted' : 'text-ink-faint',
              )}
            >
              <span className={cx('grid size-6 place-items-center rounded-full border text-xs', i < current ? 'border-up text-up' : i === current ? 'border-accent text-accent-bright' : 'border-line')}>
                {i < current ? '✓' : i + 1}
              </span>
              {label}
            </li>
          ))}
        </ol>
      </nav>

      <section className="card p-6">
        {current > 0 && j.profile && j.details && (
          <div className="mb-5 flex items-center gap-3 border-b border-line pb-4">
            {j.profile.logo !== undefined && <img src={j.profile.logo} alt="" className="h-9 w-auto rounded bg-white p-1" />}
            <div className="text-sm">
              <div className="font-medium">{j.details.name || 'New tenant'}</div>
              <div className="text-xs text-ink-faint">{j.profile.name}</div>
            </div>
          </div>
        )}
        {current === 0 && (
          <>
            <h2 className="mb-1 text-lg font-semibold">{JOURNEY_STEPS[0].title}</h2>
            <StepLead index={0} />
            <ProfileChoice profiles={PROFILES} selected={j.profile?.code} onSelect={j.choose} />
          </>
        )}
        {current === 1 && j.profile && j.details && (
          <>
            <h2 className="mb-1 text-lg font-semibold">{JOURNEY_STEPS[1].title}</h2>
            <StepLead index={1} />
            <DetailsForm profile={j.profile} details={j.details} onChange={j.setDetails} />
          </>
        )}
        {current === 2 && j.profile && j.details && (
          <>
            <h2 className="mb-1 text-lg font-semibold">{JOURNEY_STEPS[2].title}</h2>
            <StepLead index={2} />
            <dl className="grid gap-2 text-sm sm:grid-cols-2">
              <dt className="text-ink-faint">Profile</dt><dd>{j.profile.name}</dd>
              <dt className="text-ink-faint">Tenant</dt><dd>{j.details.name || '-'} ({j.details.code || '-'})</dd>
              <dt className="text-ink-faint">Administrator</dt><dd>{j.details.adminUsername}</dd>
              {j.profile.params.map((p) => (
                <Fragment key={p.name}><dt className="text-ink-faint">{p.label}</dt><dd>{j.details?.params[p.name] || '-'}</dd></Fragment>
              ))}
            </dl>
            <p className="mt-4 text-sm text-ink-muted">Creating the tenant runs {j.profile.steps.length} steps.</p>
            <div className="mt-4"><FailToggle value={j.failOnce} onChange={j.setFailOnce} /></div>
          </>
        )}
        {current === 3 && (
          <>
            <h2 className="mb-1 text-lg font-semibold">Provisioning {j.details?.name}</h2>
            <StepLead index={3} />
            <ProgressList run={j.run} onRetry={j.run.retry} onDiscard={j.restart} />
          </>
        )}
        {current === 4 && j.details && (
          <>
            <h2 className="mb-1 text-lg font-semibold">{JOURNEY_STEPS[4].title}</h2>
            <StepLead index={4} />
            <Handoff details={j.details} onRestart={() => { j.restart(); setAt(0); }} />
          </>
        )}

        {!j.started && (
          <div className="mt-6 flex justify-between border-t border-line pt-4">
            <Button variant="ghost" disabled={at === 0} onClick={() => setAt(at - 1)}>Back</Button>
            {at < 2 ? (
              <Button variant="primary" disabled={j.profile === undefined} onClick={() => setAt(at + 1)}>Continue</Button>
            ) : (
              <Button variant="primary" onClick={() => j.setStarted(true)}>Create tenant</Button>
            )}
          </div>
        )}
      </section>
    </div>
  );
}

export function NewTenantJourneyPrototype(): ReactNode {
  const j = useJourney();
  return (
    <>
      <PageHeader title="New tenant" />
      <NewTenantSteps j={j} />
    </>
  );
}

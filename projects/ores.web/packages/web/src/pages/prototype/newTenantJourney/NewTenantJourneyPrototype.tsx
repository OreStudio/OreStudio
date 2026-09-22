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
import { Button, PageHeader, Tag, cx } from '../../../ui/Primitives.js';
import { PrototypeSwitcher, usePrototypeVariant } from '../../../ui/PrototypeSwitcher.js';
import { DetailsForm, FailToggle, Handoff, JOURNEY_STEPS, JourneyIntro, ProfileChoice, ProgressList, StepLead } from './parts.js';
import { EXISTING_TENANTS, PROFILES, emptyDetails, useSimulatedRun, type TenantDetails } from './stub.js';

/**
 * PROTOTYPE ONLY -- throwaway. Question: what should the new tenant journey
 * look like on the web? Three structurally different variants on
 * /iam/tenant/onboard, switchable with ?variant=A|B|C. All data is stubbed;
 * nothing is sent to the server.
 */

const VARIANTS = [
  { key: 'A', name: 'Stepper page' },
  { key: 'B', name: 'Drawer over tenant list' },
  { key: 'C', name: 'One progressive page' },
] as const;

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

/** A: a dedicated page with a flat step rail; one step at a time. */
function VariantA({ j }: { readonly j: Journey }): ReactNode {
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

const STATE_TAG = {
  active: <Tag tone="accent">Active</Tag>,
  provisioning: <Tag tone="warn">Provisioning</Tag>,
  failed: <Tag tone="warn">Failed</Tag>,
} as const;

/** B: the Tenants list is the home; a drawer creates; progress lives on the row. */
function VariantB({ j }: { readonly j: Journey }): ReactNode {
  const [open, setOpen] = useState(false);
  const [expanded, setExpanded] = useState(true);
  const newState = j.run.status === 'done' ? 'active' : j.run.status === 'failed' ? 'failed' : 'provisioning';

  return (
    <>
      <PageHeader
        title="Tenants"
        description="Every tenant and where it is in its lifecycle."
        actions={<Button variant="primary" onClick={() => { j.restart(); setOpen(true); }}>New tenant</Button>}
      />
      <div className="card overflow-hidden">
        <table className="w-full text-sm">
          <thead className="border-b border-line text-left text-xs text-ink-faint">
            <tr><th className="px-4 py-2">Tenant</th><th className="px-4 py-2">Profile</th><th className="px-4 py-2">State</th><th className="px-4 py-2">Detail</th></tr>
          </thead>
          <tbody>
            {j.started && j.details && j.profile && (
              <>
                <tr className="cursor-pointer border-b border-line bg-accent/5" onClick={() => setExpanded(!expanded)}>
                  <td className="px-4 py-2 font-medium">{j.details.name || 'New tenant'}</td>
                  <td className="px-4 py-2">{j.profile.name}</td>
                  <td className="px-4 py-2">{STATE_TAG[newState]}</td>
                  <td className="px-4 py-2 text-ink-muted">
                    {j.run.steps.filter((s) => s.state === 'done').length} of {j.run.steps.length} steps {expanded ? '▾' : '▸'}
                  </td>
                </tr>
                {expanded && (
                  <tr className="border-b border-line">
                    <td colSpan={4} className="px-4 py-4">
                      {j.run.status === 'done' ? (
                        <Handoff details={j.details} onRestart={j.restart} />
                      ) : (
                        <ProgressList run={j.run} onRetry={j.run.retry} onDiscard={j.restart} compact />
                      )}
                    </td>
                  </tr>
                )}
              </>
            )}
            {EXISTING_TENANTS.map((t) => (
              <tr key={t.code} className="border-b border-line last:border-0">
                <td className="px-4 py-2 font-medium">{t.name}</td>
                <td className="px-4 py-2">{t.profile}</td>
                <td className="px-4 py-2">{STATE_TAG[t.state]}</td>
                <td className="px-4 py-2 text-ink-muted">
                  {t.detail}
                  {t.state === 'failed' && <Button className="ml-3" size="sm">Retry</Button>}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>

      {open && (
        <div className="fixed inset-0 z-40 flex justify-end bg-black/50" onClick={() => setOpen(false)}>
          <aside className="h-full w-full max-w-xl overflow-y-auto border-l border-line bg-surface-base p-6" onClick={(e) => e.stopPropagation()}>
            <div className="mb-6 flex items-center justify-between">
              <h2 className="text-lg font-semibold">New tenant</h2>
              <Button variant="ghost" size="sm" onClick={() => setOpen(false)}>✕</Button>
            </div>
            <div className="mb-6"><JourneyIntro compact /></div>
            <StepLead index={0} />
            <ProfileChoice profiles={PROFILES} selected={j.profile?.code} onSelect={j.choose} layout="list" />
            {j.profile && j.details && (
              <div className="mt-6">
                <StepLead index={1} />
                <DetailsForm profile={j.profile} details={j.details} onChange={j.setDetails} />
                <div className="mt-6 flex items-center justify-between border-t border-line pt-4">
                  <FailToggle value={j.failOnce} onChange={j.setFailOnce} />
                  <Button variant="primary" onClick={() => { j.setStarted(true); setOpen(false); setExpanded(true); }}>
                    Create tenant
                  </Button>
                </div>
              </div>
            )}
          </aside>
        </div>
      )}
    </>
  );
}

/** C: one page that unfolds top to bottom, then turns into the live record. */
function VariantC({ j }: { readonly j: Journey }): ReactNode {
  return (
    <div className="mx-auto max-w-3xl space-y-10">
      <PageHeader title="New tenant" />
      {!j.started && <JourneyIntro />}

      <section className={cx(j.started && 'opacity-60')}>
        <h2 className="mb-1 text-sm font-semibold text-ink-muted">1 · Starts with</h2>
          <StepLead index={0} />
        {j.started && j.profile ? (
          <p className="text-sm">{j.profile.name}: {j.profile.summary}</p>
        ) : (
          <ProfileChoice profiles={PROFILES} selected={j.profile?.code} onSelect={j.choose} />
        )}
      </section>

      {j.profile && j.details && !j.started && (
        <section>
          <h2 className="mb-1 text-sm font-semibold text-ink-muted">2 · Describe it</h2>
          <StepLead index={1} />
          <DetailsForm profile={j.profile} details={j.details} onChange={j.setDetails} />
        </section>
      )}

      {j.profile && j.details && !j.started && (
        <section className="card flex flex-wrap items-center justify-between gap-4 p-5">
          <p className="max-w-md text-sm text-ink-muted">
            Create <span className="text-ink">{j.details.name || 'this tenant'}</span> as a {j.profile.name} tenant, with{' '}
            <span className="text-ink">{j.details.adminUsername}</span> as its administrator. {j.profile.steps.length} steps run on the server.
          </p>
          <div className="flex flex-col items-end gap-2">
            <Button variant="primary" size="lg" onClick={() => j.setStarted(true)}>Create tenant</Button>
            <FailToggle value={j.failOnce} onChange={j.setFailOnce} />
          </div>
        </section>
      )}

      {j.started && j.details && (
        <section>
          <h2 className="mb-1 text-sm font-semibold text-ink-muted">3 · Provisioning</h2>
          <StepLead index={3} />
          <div className="card p-5">
            <ProgressList run={j.run} onRetry={j.run.retry} onDiscard={j.restart} />
          </div>
        </section>
      )}

      {j.run.status === 'done' && j.started && j.details && (
        <section>
          <h2 className="mb-1 text-sm font-semibold text-ink-muted">4 · Hand off</h2>
          <StepLead index={4} />
          <Handoff details={j.details} onRestart={j.restart} />
        </section>
      )}
    </div>
  );
}

export function NewTenantJourneyPrototype(): ReactNode {
  const variant = usePrototypeVariant(VARIANTS.map((v) => v.key));
  const j = useJourney();
  return (
    <>
      {variant === 'A' && (
        <>
          <PageHeader title="New tenant" />
          <VariantA j={j} />
        </>
      )}
      {variant === 'B' && <VariantB j={j} />}
      {variant === 'C' && <VariantC j={j} />}
      <PrototypeSwitcher variants={VARIANTS} />
    </>
  );
}

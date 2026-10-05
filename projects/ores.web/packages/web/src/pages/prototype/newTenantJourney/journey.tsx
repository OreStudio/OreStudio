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

import { Fragment, useEffect, useState, type ReactNode } from 'react';
import { Button, cx } from '../../../ui/Primitives.js';
import { DetailsStep, FailToggle, Handoff, ProfileChoice, ProgressList } from './parts.js';
import { PROFILES, emptyDetails, useSimulatedRun, type TenantDetails } from './stub.js';

/**
 * PROTOTYPE ONLY -- throwaway. A journey is an ordered list of steps; one page
 * renders any journey as a flat step rail plus the current step. Journeys are
 * composed from shared step definitions, so first run can include the new
 * tenant steps without looking like a journey inside a journey.
 */
export interface JourneyStep {
  readonly id: string;
  readonly title: string;
  readonly lead: string;
  readonly body: ReactNode;
  /** The footer's primary action. Omitted when the step moves on by itself. */
  readonly next?: { readonly label: string; readonly enabled: boolean; readonly onNext?: () => void };
  /** Once passed, the person cannot come back to this step. */
  readonly final?: boolean;
  /** Shown above the body on every step after the tenant is chosen. */
  readonly header?: ReactNode;
}

export function JourneyPage({
  steps,
  at,
  onMove,
}: {
  readonly steps: readonly JourneyStep[];
  readonly at: number;
  readonly onMove: (index: number) => void;
}): ReactNode {
  const step = steps[at];
  if (step === undefined) return null;
  const backAllowed = at > 0 && steps[at - 1]?.final !== true;

  return (
    <div className="grid gap-8 md:grid-cols-[14rem_1fr]">
      <nav aria-label="Journey steps">
        <ol className="space-y-1">
          {steps.map((s, i) => (
            <li
              key={s.id}
              aria-current={i === at ? 'step' : undefined}
              className={cx(
                'flex items-center gap-3 rounded-md px-3 py-2 text-sm',
                i === at ? 'bg-surface-hover font-medium text-ink' : i < at ? 'text-ink-muted' : 'text-ink-faint',
              )}
            >
              <span
                className={cx(
                  'grid size-6 shrink-0 place-items-center rounded-full border text-xs',
                  i < at ? 'border-up text-up' : i === at ? 'border-accent text-accent-bright' : 'border-line',
                )}
              >
                {i < at ? '✓' : i + 1}
              </span>
              {s.title}
            </li>
          ))}
        </ol>
      </nav>

      <section className="card p-6">
        {step.header}
        <h2 className="mb-1 text-lg font-semibold">{step.title}</h2>
        <p className="mb-5 text-sm text-ink-muted">{step.lead}</p>
        {step.body}
        {step.next !== undefined && (
          <div className="mt-6 flex justify-between border-t border-line pt-4">
            <Button variant="ghost" disabled={!backAllowed} onClick={() => onMove(at - 1)}>
              Back
            </Button>
            <Button
              variant="primary"
              disabled={!step.next.enabled}
              onClick={() => {
                step.next?.onNext?.();
                onMove(at + 1);
              }}
            >
              {step.next.label}
            </Button>
          </div>
        )}
      </section>
    </div>
  );
}

/** The new tenant steps' state: profile, details, and a simulated run. */
export function useNewTenant() {
  const [profileCode, setProfileCode] = useState<string>();
  const profile = PROFILES.find((p) => p.code === profileCode);
  const [details, setDetails] = useState<TenantDetails>();
  const [typedPasswordOk, setPasswordOk] = useState(false);
  const [started, setStarted] = useState(false);
  const [failOnce, setFailOnce] = useState(false);
  const run = useSimulatedRun(profile, started, failOnce ? 2 : undefined);
  const passwordOk = details?.useMyPassword === true || typedPasswordOk;

  const choose = (code: string): void => {
    setProfileCode(code);
    const p = PROFILES.find((x) => x.code === code);
    if (p !== undefined) setDetails(emptyDetails(p));
    setPasswordOk(false);
  };
  const restart = (): void => {
    setStarted(false);
    setProfileCode(undefined);
    setDetails(undefined);
    setPasswordOk(false);
  };
  return { profile, details, setDetails, passwordOk, setPasswordOk, started, setStarted, failOnce, setFailOnce, run, choose, restart };
}

export type NewTenant = ReturnType<typeof useNewTenant>;

/** Moves past the provisioning step once the run finishes. */
export function useAdvanceWhenProvisioned(t: NewTenant, atProvisioning: boolean, advance: () => void): void {
  useEffect(() => {
    if (atProvisioning && t.run.status === 'done') advance();
  }, [atProvisioning, t.run.status, advance]);
}

function TenantHeader({ t }: { readonly t: NewTenant }): ReactNode {
  if (t.profile === undefined || t.details === undefined) return null;
  return (
    <div className="mb-5 flex items-center gap-3 border-b border-line pb-4">
      {t.profile.logo !== undefined && <img src={t.profile.logo} alt="" className="h-9 w-auto rounded bg-white p-1" />}
      <div className="text-sm">
        <div className="font-medium">{t.details.name || 'New tenant'}</div>
        <div className="text-xs text-ink-faint">{t.profile.name}</div>
      </div>
    </div>
  );
}

/**
 * The new tenant steps, shared by the new tenant journey and first run:
 * starting point, details, review, provisioning, hand off.
 */
export function newTenantSteps(
  t: NewTenant,
  handoff: { readonly onContinue: () => void; readonly onElsewhere: () => void },
): readonly JourneyStep[] {
  const header = <TenantHeader t={t} />;
  return [
    {
      id: 'profile',
      title: 'Choose a starting point',
      lead: 'Choose a starting point for the new tenant.',
      body: <ProfileChoice profiles={PROFILES} selected={t.profile?.code} onSelect={t.choose} />,
      next: { label: 'Continue', enabled: t.profile !== undefined },
    },
    {
      id: 'details',
      title: 'Describe the tenant',
      lead: 'Name the tenant and create its administrator.',
      header,
      body:
        t.profile && t.details ? (
          <DetailsStep
            profile={t.profile}
            details={t.details}
            onChange={t.setDetails}
            onPasswordAcceptable={t.setPasswordOk}
          />
        ) : null,
      next: { label: 'Continue', enabled: t.passwordOk },
    },
    {
      id: 'review',
      title: 'Review',
      lead: 'Nothing is created until you confirm.',
      header,
      body:
        t.profile && t.details ? (
          <>
            <dl className="grid gap-2 text-sm sm:grid-cols-2">
              <dt className="text-ink-faint">Starting point</dt>
              <dd>{t.profile.name}</dd>
              <dt className="text-ink-faint">Tenant</dt>
              <dd>
                {t.details.name || '-'} ({t.details.code || '-'})
              </dd>
              <dt className="text-ink-faint">Administrator</dt>
              <dd>{t.details.adminUsername}</dd>
              {t.profile.params.map((p) => (
                <Fragment key={p.name}>
                  <dt className="text-ink-faint">{p.label}</dt>
                  <dd>{t.details?.params[p.name] || '-'}</dd>
                </Fragment>
              ))}
            </dl>
            <p className="mt-4 text-sm text-ink-muted">Creating the tenant runs {t.profile.steps.length} steps.</p>
            <div className="mt-4">
              <FailToggle value={t.failOnce} onChange={t.setFailOnce} />
            </div>
          </>
        ) : null,
      next: { label: 'Create tenant', enabled: true, onNext: () => t.setStarted(true) },
    },
    {
      id: 'provisioning',
      title: 'Provisioning',
      lead: 'This runs on the server. You can leave this page and come back.',
      header,
      final: true,
      body: <ProgressList run={t.run} onRetry={t.run.retry} onDiscard={t.restart} />,
    },
    {
      id: 'handoff',
      title: 'Hand off',
      lead: 'The tenant is ready. Its administrator signs in next.',
      header,
      final: true,
      body: t.details ? (
        <Handoff details={t.details} mustChange={t.profile?.forcePasswordChange ?? true} onContinue={handoff.onContinue} onElsewhere={handoff.onElsewhere} />
      ) : null,
    },
  ];
}

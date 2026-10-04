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

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import type { TenantSetup, TenantStatus, TenantType } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { Label } from '../ui/Label.js';

/**
 * A tenant's type or status, painted by the badge its row names.
 *
 * The words are the row's own — `Suspended`, `Evaluation` — and the colours,
 * the tooltip and the severity are the badge's. The screen decides neither, so
 * a value reads the same wherever it appears and a deployment's vocabulary is
 * not replaced by the badge catalogue's.
 *
 * A value the deployment does not hold is drawn as the server wrote it, and so
 * is one whose badge has left the catalogue. A tenant in a state nobody has
 * described is exactly the row somebody needs to see, and a screen that
 * swallowed it would be hiding the interesting case.
 */
export function PaintedValue({
    value,
    known,
}: {
    readonly value: string;
    readonly known: TenantStatus | TenantType | undefined;
}): ReactNode {
    return (
        <Label
            text={known?.name ?? value}
            badge={known?.badge ?? undefined}
            title={known?.description === '' ? undefined : known?.description}
        />
    );
}

/** The colour each unfinished run state is drawn in. */
const SETUP_TONE: Record<string, string> = {
    in_progress: 'text-accent-bright',
    compensating: 'text-warn',
    failed: 'text-down',
    compensated: 'text-ink-faint',
};

/**
 * Where a tenant's provisioning run has got to, and the way back to it.
 *
 * A completed run says nothing, because the tenant's own status already says
 * the tenant is there. Every other run links to its rail, which is how a person
 * who left the journey returns to it: the run kept working on the server, and
 * a failed one is resumed from that page. A state this screen has no words for
 * is shown as the engine named it, and still links to the run.
 */
export function SetupCell({ setup }: { readonly setup: TenantSetup | null }): ReactNode {
    const { t } = useTranslation();
    if (setup === null || setup.status === 'completed') {
        return null;
    }
    const known = setup.status in SETUP_TONE;
    const label = known
        ? t(`tenants.setupState.${setup.status}`, {
              step: setup.currentStepIndex + 1,
              count: setup.stepCount,
          })
        : setup.status;
    return (
        <Link
            to={`/tenants/runs/${encodeURIComponent(setup.instanceId)}`}
            className={`text-xs underline-offset-2 hover:underline ${SETUP_TONE[setup.status] ?? 'text-ink'}`}
            title={setup.error === '' ? undefined : setup.error}
        >
            {label}
        </Link>
    );
}

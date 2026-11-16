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
import { useState, type ReactNode } from 'react';
import type { WorkflowProgress } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { RunProgress } from '../journeys/parts.js';
import { useJourneyServer } from '../journeys/server.js';
import { Button, LinkButton, Notice, PageHeader } from '../ui/Primitives.js';

/** One step of the setup that ended with warnings, and what it warned of. */
export interface WarnedStep {
    readonly id: string;
    readonly label: string;
    readonly warnings: readonly string[];
}

/**
 * The steps of a run that ended with warnings, in the order the run took them.
 *
 * A warning is a log line at warn or error level. A step that ended with
 * warnings but logged none still counts, because its status is the fact.
 */
export function warnedSteps(progress: WorkflowProgress): WarnedStep[] {
    return progress.steps
        .filter((step) => step.status === 'completed_with_warnings')
        .sort((a, b) => a.step_index - b.step_index)
        .map((step) => ({
            id: step.id,
            label: step.label !== '' ? step.label : step.name,
            warnings: step.log
                .filter((entry) => entry.level === 'warn' || entry.level === 'error')
                .map((entry) => entry.message),
        }));
}

const DISMISSED_KEY = 'ores.setupReport.dismissed';

function readDismissed(): string {
    try {
        return window.localStorage.getItem(DISMISSED_KEY) ?? '';
    } catch {
        return '';
    }
}

function writeDismissed(instanceId: string): void {
    try {
        window.localStorage.setItem(DISMISSED_KEY, instanceId);
    } catch {
        // A browser that keeps nothing shows the notice again next time.
    }
}

/**
 * The tenant's setup run and the steps of it that ended with warnings.
 *
 * Only the tenant's administrator can read the run. Anyone else gets an error,
 * which reads as no report rather than as a failure on every screen.
 */
function useSetupReport() {
    const run = useQuery({
        queryKey: ['tenant-setup-run'],
        queryFn: () => api.tenantSetupRun(),
        retry: false,
        staleTime: 5 * 60_000,
    });
    const instanceId = run.data?.instanceId ?? '';
    const progress = useQuery({
        queryKey: ['setup-report', instanceId],
        queryFn: () => api.provisionTenantProgress(instanceId),
        enabled: instanceId !== '',
        retry: false,
        staleTime: 5 * 60_000,
    });
    return {
        instanceId,
        unavailable: run.isError || progress.isError,
        loading: run.isPending || (instanceId !== '' && progress.isPending),
        steps: progress.data === undefined ? [] : warnedSteps(progress.data),
    };
}

/**
 * Says, on every screen, that the tenant's setup left work undone, until the
 * person dismisses it for that run.
 */
export function SetupReportNotice(): ReactNode {
    const { t } = useTranslation();
    const report = useSetupReport();
    const [dismissed, setDismissed] = useState(readDismissed);
    if (report.steps.length === 0 || dismissed === report.instanceId) return null;
    return (
        <Notice tone="warn">
            <span className="block">
                {t('journey.tenantSetup.report.notice', { count: report.steps.length })}
            </span>
            <span className="mt-2 flex gap-2">
                <LinkButton to="/setup-report" size="sm" variant="secondary">
                    {t('journey.tenantSetup.report.read')}
                </LinkButton>
                <Button
                    size="sm"
                    variant="ghost"
                    onClick={() => {
                        writeDismissed(report.instanceId);
                        setDismissed(report.instanceId);
                    }}
                >
                    {t('journey.tenantSetup.report.dismiss')}
                </Button>
            </span>
        </Notice>
    );
}

/** What the tenant's setup did not finish, step by step, then the whole run. */
export function SetupReportPage(): ReactNode {
    const { t, text } = useTranslation();
    const report = useSetupReport();
    const server = useJourneyServer();
    return (
        <div>
            <PageHeader
                title={t('journey.tenantSetup.report.title')}
                description={t('journey.tenantSetup.report.lead')}
            />
            {report.unavailable ? (
                <Notice tone="info">{t('journey.tenantSetup.report.unavailable')}</Notice>
            ) : report.loading ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : report.steps.length === 0 ? (
                <Notice tone="success">{t('journey.tenantSetup.report.none')}</Notice>
            ) : (
                <ul className="card divide-y divide-line p-0">
                    {report.steps.map((step) => (
                        <li key={step.id} className="p-4">
                            <h2 className="text-sm font-semibold text-warn">{text(step.label)}</h2>
                            <ul className="mt-2 space-y-1 text-sm">
                                {step.warnings.map((warning, index) => (
                                    <li key={index} className="break-words">
                                        {text(warning)}
                                    </li>
                                ))}
                            </ul>
                        </li>
                    ))}
                </ul>
            )}
            {report.instanceId !== '' && (
                <div className="card mt-6 p-6">
                    <h2 className="mb-4 text-sm font-semibold">
                        {t('journey.tenantSetup.report.run')}
                    </h2>
                    <RunProgress
                        server={server}
                        instanceId={report.instanceId}
                        onCompleted={() => undefined}
                    />
                </div>
            )}
        </div>
    );
}

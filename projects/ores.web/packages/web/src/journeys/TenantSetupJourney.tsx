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

/**
 * The tenant setup screen: the run a provisioned tenant owns.
 *
 * The deployment's first run creates the tenant and leaves it bootstrapping,
 * and the tenant's own run does the rest. That run belongs to the tenant, so
 * only its administrator may see it, and this is what that administrator is
 * held on until it ends. It is a screen and not a rail because there is nothing
 * to choose: the run is already going, and the person either watches it or is
 * told that nobody started it.
 *
 * The screen follows the run with the same component every other journey uses,
 * and the run's own progress is the whole body. Nothing here writes: the run
 * clears the tenant's flag when it ends, and the screen asks the gate to read
 * the deployment again so the person may go in.
 *
 * The reading and the rendering are two functions so the screen can be asserted
 * without a browser: the container holds the read, and the panel it renders is
 * a function of the answer.
 */

import { useEffect, useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Notice } from '../ui/Primitives.js';
import { JourneySplash, RunProgress } from './parts.js';
import type { JourneyServer } from './server.js';
import type { TenantSetupRun } from '@ores/wire-protocol/browser';

export interface TenantSetupJourneyProps {
    readonly server: JourneyServer;
    /**
     * Called once the run finished, so the gate reads the deployment again.
     *
     * The run is what clears the tenant's flag, so this is the moment the
     * person is owed the application rather than this screen.
     */
    readonly onFinished: () => void;
}

/**
 * The run the screen is about, once it has been read.
 *
 * An absent run is the read still in flight; an empty instance id is the tenant
 * having none, which is a state the panel states rather than follows.
 */
export interface TenantSetupPanelProps {
    readonly run: TenantSetupRun | undefined;
    readonly server: JourneyServer;
    readonly onFinished: () => void;
}

function reasonOf(error: unknown): string {
    return error instanceof Error ? error.message : String(error);
}

/**
 * What the tenant setup screen shows for the run it was given.
 *
 * The run's progress carries the whole body, because the rail the person would
 * otherwise read is the run's own steps. The action opens only once the run has
 * completed, so nobody is invited into a tenant that is still being built.
 */
export function TenantSetupPanel({ run, server, onFinished }: TenantSetupPanelProps): ReactNode {
    const { t } = useTranslation();
    const [complete, setComplete] = useState(false);

    if (run === undefined) {
        return (
            <div className="card p-6">
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            </div>
        );
    }

    if (run.instanceId === '') {
        return (
            <div className="card p-6">
                <h1 className="text-lg font-semibold">{t('journey.tenantSetup.title')}</h1>
                <div className="mt-4">
                    <Notice tone="warn">{t('journey.tenantSetup.noRun')}</Notice>
                </div>
            </div>
        );
    }

    return (
        <div className="card p-6">
            <div className="mb-5 border-b border-line pb-5">
                <JourneySplash />
            </div>
            <h1 className="mb-1 text-lg font-semibold">{t('journey.tenantSetup.title')}</h1>
            <p className="mb-5 text-sm text-ink-muted">{t('journey.tenantSetup.lead')}</p>
            <RunProgress
                server={server}
                instanceId={run.instanceId}
                onCompleted={() => setComplete(true)}
            />
            <div className="mt-6 flex border-t border-line pt-4">
                <Button
                    variant="primary"
                    className="ml-auto"
                    disabled={!complete}
                    onClick={onFinished}
                >
                    {t('journey.tenantSetup.enter')}
                </Button>
            </div>
        </div>
    );
}

export function TenantSetupJourney({ server, onFinished }: TenantSetupJourneyProps): ReactNode {
    const { t } = useTranslation();
    const [run, setRun] = useState<TenantSetupRun>();
    const [failure, setFailure] = useState<string>();
    const [attempt, setAttempt] = useState(0);

    /*
     * The run is read once, when the screen opens. Its progress is what is
     * followed afterwards, so a run that starts later is not this screen's
     * concern: the person reloads, or signs in again, and the gate sends them
     * back here.
     */
    useEffect(() => {
        let cancelled = false;
        void (async () => {
            try {
                const answer = await server.tenantSetupRun();
                if (!cancelled) {
                    setRun(answer);
                    setFailure(undefined);
                }
            } catch (error) {
                if (!cancelled) {
                    setFailure(reasonOf(error));
                }
            }
        })();
        return () => {
            cancelled = true;
        };
    }, [server, attempt]);

    if (failure !== undefined) {
        return (
            <div className="card p-6">
                <Notice tone="error">
                    {t('journey.tenantSetup.readFailed', { message: failure })}
                </Notice>
                <div className="mt-4 flex justify-end">
                    <Button variant="secondary" onClick={() => setAttempt((value) => value + 1)}>
                        {t('gate.retry')}
                    </Button>
                </div>
            </div>
        );
    }

    return <TenantSetupPanel run={run} server={server} onFinished={onFinished} />;
}

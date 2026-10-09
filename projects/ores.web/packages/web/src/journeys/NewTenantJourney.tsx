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
 * The new tenant journey: the tenant steps on their own rail.
 *
 * The person is a signed-in administrator adding a tenant beside the ones the
 * deployment already has, so the rail is the tenant steps and nothing else: no
 * welcome, no administrator to create. The steps themselves are the shared
 * library the first run inlines, so the two journeys cannot describe a tenant
 * differently.
 *
 * One thing differs from the first run, and it comes from who the person is.
 * The browser does not hold the signed-in administrator's password -- they
 * typed it before this session began -- so a starting point that hands the
 * creating administrator's password to the tenant's administrator asks for one
 * instead, which the form states. And the journey's end signs this
 * administrator out: the tenant's own administrator finishes the tenant's setup
 * when they first sign in, so this browser has nothing left to do in it.
 */

import { useEffect, useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Notice } from '../ui/Primitives.js';
import { JourneyPage } from './JourneyPage.js';
import { JourneyHeader } from './parts.js';
import type { JourneyServer } from './server.js';
import { useNewTenant } from './state.js';
import { newTenantSteps } from './newTenantSteps.js';
import type { PasswordPolicy, SeedProfileChoice } from '@ores/wire-protocol/browser';

export interface NewTenantJourneyProps {
    readonly server: JourneyServer;
    /** The journey is over, so the screen behind it takes the browser back. */
    readonly onFinished: () => void;
}

function reasonOf(error: unknown): string {
    return error instanceof Error ? error.message : String(error);
}

/**
 * The journey's end: signs the creating administrator out and hands the browser
 * back.
 *
 * The tenant's own administrator finishes the tenant's setup when they first
 * sign in, so this session has nothing left to do in the tenant it just made.
 * It is a function rather than a closure so the ending can be walked without a
 * browser: what it does is sign out, and then tell the screen that sent the
 * person here that the journey is over.
 */
export async function finishTenantSetup(
    server: Pick<JourneyServer, 'signOut'>,
    onFinished: () => void,
): Promise<void> {
    await server.signOut();
    onFinished();
}

export function NewTenantJourney({ server, onFinished }: NewTenantJourneyProps): ReactNode {
    const { t } = useTranslation();
    const [policy, setPolicy] = useState<PasswordPolicy>();
    const [profiles, setProfiles] = useState<readonly SeedProfileChoice[]>([]);
    const [takenCodes, setTakenCodes] = useState<ReadonlySet<string>>(new Set());
    const [loadFailure, setLoadFailure] = useState<string>();
    const [attempt, setAttempt] = useState(0);
    /*
     * Where the rail stands, and nothing until somebody moves: the journey
     * opens on the starting points, which is the first thing it asks.
     */
    const [at, setAt] = useState<number>();
    const tenant = useNewTenant('');

    /*
     * The two reads the journey cannot start without: the rules every new
     * password must satisfy, and the starting points to choose from. They are
     * the deployment's own answers, so a deployment that adds a starting point
     * adds a card here.
     */
    useEffect(() => {
        let cancelled = false;
        void (async () => {
            try {
                const [answer, choices] = await Promise.all([
                    server.passwordPolicy(),
                    server.seedProfiles(),
                ]);
                if (!cancelled) {
                    setPolicy(answer);
                    setProfiles(choices);
                    setLoadFailure(undefined);
                }
                /*
                 * The codes only grey out the starting points whose tenant
                 * exists. The server refuses a duplicate code whatever this
                 * read says, so a failure here leaves every card open rather
                 * than blocking the journey.
                 */
                const codes = await server.tenantCodes().catch(() => []);
                if (!cancelled) {
                    setTakenCodes(new Set(codes));
                }
            } catch (error) {
                if (!cancelled) {
                    setLoadFailure(reasonOf(error));
                }
            }
        })();
        return () => {
            cancelled = true;
        };
    }, [server, attempt]);

    /*
     * The journey's end. The tenant's own administrator finishes the tenant's
     * setup when they first sign in, so this session ends here and the next
     * thing this browser does is offer the sign-in screen.
     */
    const finish = async (): Promise<void> => {
        await finishTenantSetup(server, onFinished);
    };

    if (policy === undefined) {
        return (
            <div className="card p-6">
                {loadFailure === undefined ? (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                ) : (
                    <>
                        <Notice tone="error">
                            {t('journey.policyFailed', { message: loadFailure })}
                        </Notice>
                        <div className="mt-4 flex justify-end">
                            <Button
                                variant="secondary"
                                onClick={() => setAttempt((value) => value + 1)}
                            >
                                {t('gate.retry')}
                            </Button>
                        </div>
                    </>
                )}
            </div>
        );
    }

    /*
     * This journey holds no creating administrator's password, and no read
     * returns one: nobody signed in as the account that creates the tenant. A
     * starting point that would hand that password on cannot, so the form asks
     * for a password of the tenant's own instead. The empty string is what says
     * so, and it travels into the tenant state as well as into the steps.
     */
    const creatingPassword = '';
    const steps = newTenantSteps({
        t,
        server,
        policy,
        profiles,
        state: tenant,
        creatingPassword,
        onFinished: finish,
        takenCodes,
    });

    return (
        <JourneyPage
            steps={steps}
            at={at ?? 0}
            onMove={setAt}
            header={<JourneyHeader tenant={tenant} />}
        />
    );
}

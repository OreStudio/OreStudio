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

/*
 * PROTOTYPE. Throwaway. Delete with the branch.
 *
 * Check the versions and the database, from
 * doc/knowledge/journeys/operations/journey_check_the_versions_and_the_database.org.
 * The client and the server have their real shapes; the database panel stands
 * for the read that does not exist at all.
 */

import { type ReactNode } from 'react';
import { Detail, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { VariantBar, useVariant, type PrototypeVariant } from './VariantBar.js';
import { GapPanel, OperationsNav, type ScreenGap } from './OperationsParts.js';
import { clientVersion, databaseState, serverVersion } from './fixtures.js';

const VARIANTS = [
    {
        id: 'read',
        name: 'Deployment answers',
        gist: 'The footer and this screen both state the server build the login answer carried.',
    },
    {
        id: 'unreachable',
        name: 'Deployment silent',
        gist: 'The server build is unknown and the screen says so, rather than inventing one.',
    },
] as const satisfies readonly [PrototypeVariant, ...PrototypeVariant[]];

const GAPS: readonly ScreenGap[] = [
    {
        title: 'The database fingerprint has no read',
        body: 'Exactly one reader touches the recorded row: each service at its own startup, comparing the fingerprint and refusing to start on a mismatch. No subject serves it to a person, so the panel reads unknown.',
    },
    {
        title: 'The fingerprint read needs its audience decided',
        body: 'The row names the commit and the environment the database was built from — build forensics, not customer data. The read should state who may see it when it exists.',
    },
    {
        title: 'The client and the server strings do not share a shape',
        body: 'The client states a release and a commit; the server adds the platform and the build information in a different composition. The screen shows them side by side; only an eye can compare them.',
    },
    {
        title: 'The per-instance versions are releases only',
        body: 'Each service reports its release string with its heartbeat, so two builds of one release cannot be told apart on the services screen either.',
    },
];

export function VersionsPrototype(): ReactNode {
    const { active, choose } = useVariant(VARIANTS, 'read');
    const serverKnown = active.id === 'read';

    return (
        <>
            <div className="mx-auto max-w-[1200px] space-y-6 pb-[45vh]">
                <OperationsNav pathname="/prototype/versions" />
                <PageHeader
                    title="About: versions and the database"
                    description="What this browser runs, what the deployment runs, and what the deployment stores."
                />
                <Notice tone="warn">
                    PROTOTYPE. The client and the server panels carry the real build shapes; the
                    database panel stands in for the read that does not exist. Nothing on this page
                    reads the server.
                </Notice>

                <section className="card space-y-4 p-6">
                    <header className="flex flex-wrap items-baseline justify-between gap-2">
                        <h2 className="text-lg font-medium">Client</h2>
                        <span className="text-xs text-ink-faint">what this browser runs</span>
                    </header>
                    <div className="grid gap-x-6 gap-y-3 sm:grid-cols-3">
                        <Detail label="Version" value={clientVersion.version} mono />
                        <Detail label="Commit" value={clientVersion.commit} mono />
                        <Detail
                            label="Checkout"
                            value={clientVersion.dirty ? 'Uncommitted changes' : 'Clean'}
                        />
                    </div>
                    <p className="text-xs text-ink-faint">
                        Stamped into the bundle when it was built; it cannot change while the tab is
                        open.
                    </p>
                </section>

                <section className="card space-y-4 p-6">
                    <header className="flex flex-wrap items-baseline justify-between gap-2">
                        <h2 className="text-lg font-medium">Server</h2>
                        <span className="text-xs text-ink-faint">what the deployment runs</span>
                    </header>
                    {serverKnown ? (
                        <div className="grid gap-x-6 gap-y-3 sm:grid-cols-2">
                            <Detail label="Version" value={serverVersion.version} mono />
                            <Detail label="Address" value={serverVersion.address} mono />
                        </div>
                    ) : (
                        <div className="grid gap-x-6 gap-y-3 sm:grid-cols-2">
                            <Detail label="Version" value="unknown" />
                            <Detail label="Address" value={serverVersion.address} mono />
                            <p className="text-sm text-ink-muted sm:col-span-2">
                                The deployment has not answered, so its build is unknown. The screen
                                says unknown rather than inventing a version, and the footer reads
                                the same session value.
                            </p>
                        </div>
                    )}
                    <p className="text-xs text-ink-faint">
                        The login answer states this build in full, and the session keeps it; the
                        footer reads it from there.
                    </p>
                </section>

                <section className="card space-y-4 p-6">
                    <header className="flex flex-wrap items-baseline justify-between gap-2">
                        <h2 className="text-lg font-medium">Database</h2>
                        <span className="text-xs text-ink-faint">what the deployment stores</span>
                    </header>
                    <div className="grid gap-x-6 gap-y-3 sm:grid-cols-4">
                        <Detail
                            label="Fingerprint"
                            value={databaseState.fingerprint ?? 'unknown'}
                        />
                        <Detail
                            label="Environment"
                            value={databaseState.environment ?? 'unknown'}
                        />
                        <Detail label="Commit" value={databaseState.commit ?? 'unknown'} />
                        <Detail label="Created" value={databaseState.created ?? 'unknown'} />
                    </div>
                    <div className="flex flex-wrap items-center gap-2">
                        <Tag tone="warn">no read exists</Tag>
                        <span className="text-xs text-ink-faint">
                            The row is recorded and nothing serves it; the panel stays unknown until
                            a read exists.
                        </span>
                    </div>
                </section>

                <GapPanel gaps={GAPS} />
            </div>

            <VariantBar
                variants={VARIANTS}
                active={active}
                onChoose={choose}
                state={
                    <div className="space-y-2 text-xs">
                        <div className="grid gap-x-6 gap-y-1 text-ink-muted sm:grid-cols-2">
                            <span>
                                fixture: <span className="font-mono text-ink">{active.id}</span>
                            </span>
                            <span>
                                client from:{' '}
                                <span className="font-mono text-ink">the bundle stamp</span>
                            </span>
                            <span>
                                server from:{' '}
                                <span className="font-mono text-ink">
                                    {serverKnown ? 'the login answer' : 'no answer'}
                                </span>
                            </span>
                            <span>
                                database from:{' '}
                                <span className="font-mono text-ink">no read exists</span>
                            </span>
                        </div>
                        <p className="text-ink-faint">
                            Signed in as any person; on every screen the footer states the client and
                            the server.
                        </p>
                    </div>
                }
            />
        </>
    );
}

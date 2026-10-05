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
 * PROTOTYPE. Kept on main as the design record; nothing outside the prototype
 * routes imports it.
 *
 * Check the versions and the database, from
 * doc/knowledge/journeys/operations/journey_check_the_versions_and_the_database.org.
 * The client comes from the bundle stamp; the server and the database come from
 * the login answer, which must learn to carry the database row. The screen
 * stands for the read that does not exist yet.
 */

import { type ReactNode } from 'react';
import { Detail, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { VariantBar, useVariant, type PrototypeVariant } from './VariantBar.js';
import { GapPanel, OperationsBack, type ScreenGap } from './OperationsParts.js';
import { clientVersion, databaseState, serverVersion } from './operationsFixtures.js';

const VARIANTS = [
    {
        id: 'carried',
        name: 'Login answer carries it',
        gist: 'The answer that opened the session states the server build and the database row; the screen reads both from it.',
    },
    {
        id: 'unreachable',
        name: 'Deployment silent',
        gist: 'The deployment has not answered, so the server and the database read unknown rather than inventing values.',
    },
] as const satisfies readonly [PrototypeVariant, ...PrototypeVariant[]];

const GAPS: readonly ScreenGap[] = [
    {
        title: 'The login answer does not carry the database row yet',
        body: 'The row is recorded and one reader touches it: every service compares the fingerprint at its own startup and refuses to start on a mismatch. No operation serves it to a person. The design puts it in the login answer, beside the server build — the same answer that states what the session is talking to. Until the answer carries it, a real session shows unknown here.',
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
    const { active, choose } = useVariant(VARIANTS, 'carried');
    const serverKnown = active.id === 'carried';

    return (
        <>
            <div className="mx-auto max-w-[1200px] space-y-6 pb-[45vh]">
                <PageHeader
                    title="Operations: versions and the database"
                    description="What this browser runs, what the deployment runs, and what the deployment stores."
                    actions={<OperationsBack />}
                />
                <Notice tone="warn">
                    PROTOTYPE. The client and the server panels carry the real build shapes; the
                    database panel shows the row the login answer must learn to carry. Nothing on
                    this page reads the server.
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
                                says unknown rather than inventing a version.
                            </p>
                        </div>
                    )}
                    <p className="text-xs text-ink-faint">
                        In a real session the login answer states this build in full, and the
                        session keeps it; the footer reads the same value. The prototype shell has
                        no session, so its footer shows a placeholder.
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
                            value={serverKnown ? databaseState.fingerprint : 'unknown'}
                            mono={serverKnown}
                        />
                        <Detail
                            label="Environment"
                            value={serverKnown ? databaseState.environment : 'unknown'}
                        />
                        <Detail
                            label="Commit"
                            value={serverKnown ? databaseState.commit : 'unknown'}
                            mono={serverKnown}
                        />
                        <Detail
                            label="Created"
                            value={serverKnown ? databaseState.created : 'unknown'}
                        />
                    </div>
                    <div className="flex flex-wrap items-center gap-2">
                        <Tag tone="warn">not carried yet</Tag>
                        <span className="text-xs text-ink-faint">
                            The login answer must state these four fields beside the server build;
                            it does not today, so a real session reads unknown here.
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
                                <span className="font-mono text-ink">
                                    {serverKnown ? 'the login answer (target)' : 'no answer'}
                                </span>
                            </span>
                        </div>
                        <p className="text-ink-faint">
                            Signed in as any person; on every screen the footer states the client
                            and the server.
                        </p>
                    </div>
                }
            />
        </>
    );
}

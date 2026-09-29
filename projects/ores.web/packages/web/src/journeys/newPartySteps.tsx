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
 * The steps a party journey runs.
 *
 * A party is found, named, reviewed, provisioned, and then there is somewhere
 * to go: it is added to the tenant the person already works in, so the last
 * step is about working as it rather than about setting anything else up.
 *
 * There are two ways to find one and they converge on the same three steps. A
 * legal entity the deployment holds brings its own legal name and its LEI; a
 * party that is not one of them is named by hand and has no LEI. Nothing else
 * about the two differs, which is why the branch is the first step and not a
 * second journey.
 *
 * The steps are data, which is why this is a function and not a component: the
 * page joins this list with its own steps and hands the result to the runtime.
 */

import { Fragment, useState, type ReactNode } from 'react';
import { Field, Input, Notice, cx } from '../ui/Primitives.js';
import { LegalEntitySearch } from './LegalEntitySearch.js';
import { RunProgress } from './parts.js';
import type { JourneyStep } from './runtime.js';
import type { JourneyServer } from './server.js';
import { partyRequest, type NewParty } from './partyState.js';
import type { Translator } from '../i18n/translate.js';

export interface NewPartyStepsInput {
    readonly t: Translator['t'];
    readonly server: JourneyServer;
    readonly state: NewParty;
    /** Re-scopes the session to the party the journey just made. */
    readonly onWorkInIt: () => Promise<void>;
    /** Starts the journey again for another party. */
    readonly onAnother: () => void;
    /** Leaves the journey for the screen behind it. */
    readonly onDone: () => void;
}

/**
 * What the panel says above every step once the party has a name.
 *
 * A person who looked away should not have to walk back up the rail to remember
 * which party the run is about, so the name and the LEI stand above whatever
 * the step below is asking. A party described by hand has no LEI, and saying so
 * is better than leaving the line blank.
 */
function PartyHeader({
    t,
    party,
}: {
    readonly t: Translator['t'];
    readonly party: NewParty;
}): ReactNode {
    return (
        <div className="flex items-baseline justify-between gap-3 text-sm">
            <span className="truncate font-medium">{party.fullName}</span>
            <span className="truncate font-mono text-xs text-ink-faint">
                {party.lei !== '' ? party.lei : t('journey.party.noLei')}
            </span>
        </div>
    );
}

/**
 * The two ways to name a party, and the field each one needs.
 *
 * The choice is stated rather than implied: a search box that a person cannot
 * fill in because their party has no LEI is a dead end, and the card that says
 * so is the way out of it.
 */
function FindParty({
    t,
    server,
    party,
}: {
    readonly t: Translator['t'];
    readonly server: JourneyServer;
    readonly party: NewParty;
}): ReactNode {
    return (
        <div className="space-y-5">
            <div role="radiogroup" className="grid gap-3 sm:grid-cols-2">
                <button
                    type="button"
                    role="radio"
                    aria-checked={!party.byHand}
                    onClick={party.searchAgain}
                    className={cx(
                        'card p-4 text-left transition-colors',
                        !party.byHand
                            ? 'border-accent ring-3 ring-accent/20'
                            : 'hover:border-line-strong',
                    )}
                >
                    <span className="font-semibold">{t('journey.party.find.search')}</span>
                    <p className="mt-1 text-sm text-ink-muted">
                        {t('journey.party.find.searchHint')}
                    </p>
                </button>
                <button
                    type="button"
                    role="radio"
                    aria-checked={party.byHand}
                    onClick={party.describeByName}
                    className={cx(
                        'card p-4 text-left transition-colors',
                        party.byHand
                            ? 'border-accent ring-3 ring-accent/20'
                            : 'hover:border-line-strong',
                    )}
                >
                    <span className="font-semibold">{t('journey.party.find.byHand')}</span>
                    <p className="mt-1 text-sm text-ink-muted">
                        {t('journey.party.find.byHandHint')}
                    </p>
                </button>
            </div>

            {party.byHand ? (
                <Field label={t('journey.party.find.name')} hint={t('journey.party.find.nameHint')}>
                    <Input
                        value={party.typedName}
                        onChange={(event) => party.typeName(event.target.value)}
                    />
                </Field>
            ) : (
                <LegalEntitySearch
                    server={server}
                    value={party.lei}
                    label={t('journey.party.find.lei')}
                    hint={t('journey.party.find.leiHint')}
                    onChoose={party.chooseEntity}
                />
            )}
        </div>
    );
}

/** The review: everything the person decided, and nothing yet created. */
function PartySummary({
    t,
    party,
}: {
    readonly t: Translator['t'];
    readonly party: NewParty;
}): ReactNode {
    const rows: readonly (readonly [string, string])[] = [
        [t('journey.party.review.legalName'), party.fullName],
        [t('journey.party.review.lei'), party.lei !== '' ? party.lei : t('journey.party.noLei')],
        [t('journey.party.review.shortCode'), party.shortCode],
    ];
    return (
        <>
            <dl className="grid gap-2 text-sm sm:grid-cols-2">
                {rows.map(([label, value]) => (
                    <Fragment key={label}>
                        <dt className="text-ink-faint">{label}</dt>
                        <dd>{value}</dd>
                    </Fragment>
                ))}
            </dl>
            <p className="mt-4 text-sm text-ink-muted">{t('journey.party.review.data')}</p>
            <p className="mt-2 text-sm text-ink-muted">{t('journey.party.review.bornInactive')}</p>
        </>
    );
}

/**
 * The three ways out, once the party is active.
 *
 * Working in the party re-scopes the session to it, which is what makes the
 * next screen the party's rather than the one the person arrived from. Adding
 * another party is this journey again, from the top. The refusal is shown
 * rather than hidden because switching is the server's decision: an account the
 * run has not joined yet cannot work in the party, and saying so is the whole
 * message.
 */
function PartyNextSteps({
    t,
    onWorkInIt,
    onAnother,
    onDone,
}: {
    readonly t: Translator['t'];
    readonly onWorkInIt: () => Promise<void>;
    readonly onAnother: () => void;
    readonly onDone: () => void;
}): ReactNode {
    const [busy, setBusy] = useState(false);
    const [failure, setFailure] = useState<string>();

    const work = async (): Promise<void> => {
        setFailure(undefined);
        setBusy(true);
        try {
            await onWorkInIt();
        } catch (error) {
            setFailure(error instanceof Error ? error.message : String(error));
        } finally {
            setBusy(false);
        }
    };

    const exits: readonly {
        readonly key: string;
        readonly title: string;
        readonly hint: string;
        readonly act: () => void;
    }[] = [
        {
            key: 'work',
            title: t('journey.party.next.work'),
            hint: t('journey.party.next.workHint'),
            act: () => void work(),
        },
        {
            key: 'another',
            title: t('journey.party.next.another'),
            hint: t('journey.party.next.anotherHint'),
            act: onAnother,
        },
        {
            key: 'done',
            title: t('journey.party.next.done'),
            hint: t('journey.party.next.doneHint'),
            act: onDone,
        },
    ];

    return (
        <div className="space-y-4">
            {failure !== undefined && <Notice tone="error">{failure}</Notice>}
            <div className="grid gap-3 sm:grid-cols-3">
                {exits.map((exit) => (
                    <button
                        key={exit.key}
                        type="button"
                        disabled={busy}
                        className="card p-4 text-left hover:border-accent"
                        onClick={exit.act}
                    >
                        <span className="font-semibold">{exit.title}</span>
                        <p className="mt-1 text-sm text-ink-muted">{exit.hint}</p>
                    </button>
                ))}
            </div>
            <p className="text-sm text-ink-muted">{t('journey.party.next.joined')}</p>
        </div>
    );
}

export function newPartySteps(input: NewPartyStepsInput): readonly JourneyStep<ReactNode>[] {
    const { t, server, state } = input;

    return [
        {
            id: 'entity',
            title: t('journey.party.find.title'),
            lead: t('journey.party.find.lead'),
            body: <FindParty t={t} server={server} party={state} />,
            next: {
                label: t('common.continue'),
                enabled: state.fullName !== '',
            },
        },
        {
            id: 'describe',
            title: t('journey.party.describe.title'),
            lead: t('journey.party.describe.lead'),
            body: (
                <Field
                    label={t('journey.party.describe.shortCode')}
                    hint={t('journey.party.describe.shortCodeHint')}
                >
                    <Input
                        value={state.shortCode}
                        onChange={(event) => state.setShortCode(event.target.value)}
                    />
                </Field>
            ),
            next: {
                label: t('common.continue'),
                enabled: state.shortCode.trim() !== '',
            },
        },
        {
            id: 'review',
            title: t('journey.party.review.title'),
            lead: t('journey.party.review.lead'),
            body: <PartySummary t={t} party={state} />,
            next: {
                label: t('journey.party.review.create'),
                enabled: state.shortCode.trim() !== '',
                run: async () => {
                    const result = await server.provisionParty(partyRequest(state));
                    if (!result.success) {
                        throw new Error(result.message);
                    }
                    if (result.instanceId === '') {
                        throw new Error(t('journey.party.review.noRun'));
                    }
                    state.recordRun(result.partyId, result.instanceId);
                },
            },
        },
        {
            id: 'provisioning',
            title: t('journey.party.provisioning.title'),
            lead: t('journey.party.provisioning.lead'),
            final: true,
            body:
                state.instanceId !== undefined ? (
                    <RunProgress
                        server={server}
                        instanceId={state.instanceId}
                        onCompleted={state.recordRunComplete}
                    />
                ) : null,
            next: {
                label: t('common.continue'),
                enabled: state.runComplete,
            },
        },
        {
            id: 'next',
            title: t('journey.party.next.title'),
            lead: t('journey.party.next.lead', { code: state.shortCode }),
            final: true,
            body: (
                <PartyNextSteps
                    t={t}
                    onWorkInIt={input.onWorkInIt}
                    onAnother={input.onAnother}
                    onDone={input.onDone}
                />
            ),
        },
    ];
}

/**
 * The header a party journey shows above its steps, or nothing when the party
 * has no name yet: the first step is where that name is chosen.
 */
export function partyHeader(t: Translator['t'], party: NewParty): ReactNode | undefined {
    return party.fullName === '' ? undefined : <PartyHeader t={t} party={party} />;
}

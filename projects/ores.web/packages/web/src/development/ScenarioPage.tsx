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

import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useEffect, useReducer, useRef, useState, type ReactNode } from 'react';
import { useParams } from 'react-router';
import type { Scenario } from '@ores/contracts';
import { qa } from '../api/qa.js';
import { useTranslation } from '../i18n/Provider.js';
import { Crumbs } from '../refdata/shared.js';
import { Notice } from '../ui/Primitives.js';
import { useTabs } from '../ui/Tabs.js';
import { DocPanel } from './DocPanel.js';
import { closeDoc, openDoc, shownDoc } from './docStack.js';
import { ScenarioReport } from './ScenarioReport.js';
import { UnsavedGuard } from './UnsavedGuard.js';
import {
    ClipboardProblem,
    MAX_IMAGE_MEGABYTES,
    imageProblem,
    imagesFromClipboard,
} from './images.js';
import {
    isDirty,
    movesOn,
    openRunner,
    outcomeOf,
    reduce,
    saveRequest,
    type Action,
    type Outcome,
} from './runner.js';
import { ScenarioRunner } from './ScenarioRunner.js';
import { QA_SCENARIOS_KEY, qaQueries, scenarioKey } from './queries.js';

/** The page a queue row opens: the scenario, loaded, and its runner. */
export function ScenarioPage(): ReactNode {
    const { t } = useTranslation();
    const { id = '' } = useParams();
    const scenario = useQuery(qaQueries.scenario(id));

    return (
        <div className="space-y-4">
            <Crumbs
                parts={[
                    { label: t('shell.menu.home'), to: '/' },
                    { label: t('shell.menu.development'), to: '/development' },
                    ...(scenario.isSuccess ? [{ label: scenario.data.title }] : []),
                ]}
            />
            {scenario.isError && <Notice tone="error">{scenario.error.message}</Notice>}
            {scenario.isPending && <p className="text-sm text-ink-muted">{t('common.loading')}</p>}
            {scenario.isSuccess && <Loaded id={id} scenario={scenario.data} />}
        </div>
    );
}

/**
 * The tabs of a scenario. A scenario that is open opens on its steps, which is
 * where a tester works. A scenario that is closed opens on its report, which is
 * what a reviewer reads. The order is fixed when the page opens, so saving a
 * scenario shut does not move the tester to another tab.
 */
const OPEN_TABS = ['steps', 'report', 'story', 'task'] as const;
const CLOSED_TABS = ['report', 'steps', 'story', 'task'] as const;

/**
 * A loaded scenario: its runner, and the story and task it belongs to.
 *
 * The runner stays mounted while another tab shows, only hidden, so a tester
 * who looks at the story does not lose the marks and notes made so far.
 */
function Loaded({ id, scenario }: { readonly id: string; readonly scenario: Scenario }): ReactNode {
    const { t } = useTranslation();
    const [closedAtOpen] = useState(scenario.state !== 'PENDING');
    const { tab, bar } = useTabs({
        label: t('development.scenario.tabsLabel'),
        tabs: closedAtOpen ? CLOSED_TABS : OPEN_TABS,
        titleOf: (candidate) => t(`development.scenario.tabs.${candidate}`),
    });
    return (
        <div className="space-y-4">
            {bar}
            <div hidden={tab !== 'steps'}>
                <Run id={id} scenario={scenario} />
            </div>
            {tab === 'report' && <ScenarioReport scenario={scenario} />}
            {tab === 'story' && (
                <Context link={scenario.story} none={t('development.scenario.noStory')} />
            )}
            {tab === 'task' && (
                <Context link={scenario.task} none={t('development.scenario.noTask')} />
            )}
        </div>
    );
}

/**
 * The doc a scenario names, or a plain message when it names none. A link in
 * the doc opens the linked doc in this tab, with a Back button, so the tester
 * never leaves the runner page and loses nothing that is not saved.
 */
function Context({
    link,
    none,
}: {
    readonly link: Scenario['story'];
    readonly none: string;
}): ReactNode {
    const [stack, setStack] = useState<readonly string[]>(link === null ? [] : [link.id]);
    if (link === null) return <p className="text-sm text-ink-muted">{none}</p>;
    return (
        <DocPanel
            key={shownDoc(stack, link.id)}
            id={shownDoc(stack, link.id)}
            onDocLink={(id) => setStack(openDoc(stack, id))}
            {...(stack.length > 1 ? { onBack: () => setStack(closeDoc(stack)) } : {})}
        />
    );
}

/**
 * How long the run waits after the last move before saving, so a tester walking
 * quickly through the steps sends one save rather than one per step, and no mark
 * is dropped while the save of the step before it is still in flight.
 */
const SAVE_AFTER_MOVE_MS = 800;

/** Holds the working copy of one loaded scenario, and saves it. */
function Run({ id, scenario }: { readonly id: string; readonly scenario: Scenario }): ReactNode {
    const { t } = useTranslation();
    const client = useQueryClient();
    const [runner, dispatch] = useReducer(reduce, scenario, openRunner);
    const [outcome, setOutcome] = useState<Outcome | null>(null);
    const [confirmingRetest, setConfirmingRetest] = useState(false);
    const [imageBusy, setImageBusy] = useState(false);
    const uploading = useRef(false);
    const [imageError, setImageError] = useState<string | null>(null);

    // Whether the save in flight was asked for with the button. A save that rides
    // along with a step reports nothing when it works, so a run does not shout on
    // every step; a failed save is loud either way.
    const explicit = useRef(false);
    const owed = useRef(false);

    const save = useMutation({
        mutationFn: () => qa.record(id, saveRequest(runner)),
        onSuccess: (saved) => {
            client.setQueryData(scenarioKey(id), saved.scenario);
            void client.invalidateQueries({ queryKey: QA_SCENARIOS_KEY });
            dispatch({ type: 'saved', scenario: saved.scenario });
            setOutcome(explicit.current ? outcomeOf(saved) : null);
            explicit.current = false;
        },
        onError: (error) => setOutcome({ kind: 'failed', message: error.message }),
    });

    // An action of the tester clears the message about the last save. A save
    // dispatches on the raw dispatch, so its own message stays.
    const act = (action: Action): void => {
        if (save.isPending && (action.type === 'mark' || action.type === 'note')) return;
        setOutcome(null);
        if (movesOn(action)) owed.current = true;
        dispatch(action);
    };

    /*
     * A run saves as it goes. A tester who marks every step and reaches the end
     * has done the work, and a run that waits for a Save nobody pressed is a run
     * no reader sees: the server closes the scenario when no step is pending, so
     * the last mark saves the whole run.
     *
     * The save waits a moment after the last move, so a tester walking quickly
     * through nine steps sends one save at the end rather than nine, and a mark
     * is never dropped because the save of the step before it is still in flight.
     * An image still uploading holds it back further, because the file's link
     * reaches its step when the upload ends.
     */
    useEffect(() => {
        if (!owed.current || save.isPending || imageBusy) return undefined;
        const timer = setTimeout(() => {
            if (save.isPending || imageBusy) return;
            if (!isDirty(runner, scenario)) {
                owed.current = false;
                return;
            }
            owed.current = false;
            save.mutate();
        }, SAVE_AFTER_MOVE_MS);
        return () => clearTimeout(timer);
    });

    // Each image is kept on the server, then its link goes into the note of the
    // step it was dropped on, which may not be the step on show when the upload
    // ends. The link reaches the doc when the tester saves.
    const onImages = async (files: File[]): Promise<void> => {
        // One batch at a time: a second drop while one is in flight is ignored, so
        // the hint and an error are never cleared by the wrong batch.
        if (uploading.current || save.isPending || runner.closed) return;
        uploading.current = true;
        const step = runner.index;
        setImageBusy(true);
        setImageError(null);
        try {
            for (const file of files) {
                const problem = imageProblem(file);
                if (problem !== null) {
                    setImageError(
                        problem === 'too-large'
                            ? t('development.runner.imageTooLarge', { limit: MAX_IMAGE_MEGABYTES })
                            : t('development.runner.imageNotImage'),
                    );
                    continue;
                }
                const name = await qa.uploadImage(id, step + 1, file);
                act({ type: 'attach', index: step, name });
            }
        } catch (error) {
            setImageError(error instanceof Error ? error.message : String(error));
        } finally {
            uploading.current = false;
            setImageBusy(false);
        }
    };

    // The button reads the clipboard, where Ctrl+V needs a field with the focus.
    // The browser asks the tester for leave the first time.
    const onClipboard = async (): Promise<void> => {
        if (uploading.current || save.isPending || runner.closed) return;
        try {
            await onImages(await imagesFromClipboard(navigator.clipboard));
        } catch (error) {
            setImageError(
                error instanceof ClipboardProblem
                    ? t(`development.runner.clipboard.${error.reason}`)
                    : error instanceof Error
                      ? error.message
                      : String(error),
            );
        }
    };

    return (
        <>
            <UnsavedGuard dirty={isDirty(runner, scenario)} />
            <ScenarioRunner
                scenario={scenario}
                runner={runner}
                dispatch={act}
                onSave={() => {
                    explicit.current = true;
                    save.mutate();
                }}
                saving={save.isPending}
                outcome={outcome}
                onImages={(files) => void onImages(files)}
                imageBusy={imageBusy}
                imageError={imageError}
                onClipboard={() => void onClipboard()}
                confirmingRetest={confirmingRetest}
                onAskRetest={() => setConfirmingRetest(true)}
                onConfirmRetest={() => {
                    setConfirmingRetest(false);
                    act({ type: 'retest' });
                }}
                onCancelRetest={() => setConfirmingRetest(false)}
            />
        </>
    );
}

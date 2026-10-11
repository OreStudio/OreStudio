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
import { useReducer, useRef, useState, type ReactNode } from 'react';
import { useParams } from 'react-router';
import type { Scenario } from '@ores/contracts';
import { qa } from '../api/qa.js';
import { useTranslation } from '../i18n/Provider.js';
import { Crumbs } from '../refdata/shared.js';
import { Notice } from '../ui/Primitives.js';
import { useTabs } from '../ui/Tabs.js';
import { DocPanel } from './DocPanel.js';
import { closeDoc, openDoc, shownDoc } from './docStack.js';
import { UnsavedGuard } from './UnsavedGuard.js';
import {
    ClipboardProblem,
    MAX_IMAGE_MEGABYTES,
    imageProblem,
    imagesFromClipboard,
} from './images.js';
import {
    isDirty,
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

const TABS = ['steps', 'story', 'task'] as const;

/**
 * A loaded scenario: its runner, and the story and task it belongs to.
 *
 * The runner stays mounted while another tab shows, only hidden, so a tester
 * who looks at the story does not lose the marks and notes made so far.
 */
function Loaded({ id, scenario }: { readonly id: string; readonly scenario: Scenario }): ReactNode {
    const { t } = useTranslation();
    const { tab, bar } = useTabs({
        label: t('development.scenario.tabsLabel'),
        tabs: TABS,
        titleOf: (candidate) => t(`development.scenario.tabs.${candidate}`),
    });
    return (
        <div className="space-y-4">
            {bar}
            <div hidden={tab !== 'steps'}>
                <Run id={id} scenario={scenario} />
            </div>
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

    const save = useMutation({
        mutationFn: () => qa.record(id, saveRequest(runner)),
        onSuccess: (saved) => {
            client.setQueryData(scenarioKey(id), saved.scenario);
            void client.invalidateQueries({ queryKey: QA_SCENARIOS_KEY });
            dispatch({ type: 'saved', scenario: saved.scenario });
            setOutcome(outcomeOf(saved));
        },
        onError: (error) => setOutcome({ kind: 'failed', message: error.message }),
    });

    // An action of the tester clears the message about the last save. A save
    // dispatches on the raw dispatch, so its own message stays.
    const act = (action: Action): void => {
        if (save.isPending && (action.type === 'mark' || action.type === 'note')) return;
        setOutcome(null);
        dispatch(action);
    };

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
                onSave={() => save.mutate()}
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

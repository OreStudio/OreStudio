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

import { ApiFailure } from './transport.js';

/**
 * The failures the interface is currently reporting.
 *
 * A failed request used to be visible only where a screen remembered to render
 * one, so a save that did not happen looked like a screen that did nothing.
 * This is the one place every request reports to, and one banner reads: plain
 * module state and listeners, because a cache reports from outside React and a
 * component subscribes from inside it.
 *
 * An identical message already on screen is not reported again, so a screen
 * polling every few seconds does not stack the same sentence. A message that
 * was dismissed and then happens again does come back, because a second failure
 * is a second fact rather than a repetition.
 */

export interface ErrorReport {
    readonly id: number;
    readonly message: string;
}

/** How many reports are held at once; the oldest is dropped past this. */
const CAP = 3;

let nextId = 1;
let reports: readonly ErrorReport[] = [];
const listeners = new Set<() => void>();

function changed(): void {
    for (const listener of listeners) {
        listener();
    }
}

/** The reports on screen, most recent first. */
export function current(): readonly ErrorReport[] {
    return reports;
}

/** Registers a listener and answers the function that removes it. */
export function subscribe(listener: () => void): () => void {
    listeners.add(listener);
    return () => {
        listeners.delete(listener);
    };
}

/** Adds a message unless the same one is already on screen. */
export function report(message: string): void {
    if (reports.some((existing) => existing.message === message)) {
        return;
    }
    reports = [{ id: nextId, message }, ...reports].slice(0, CAP);
    nextId += 1;
    changed();
}

/** Takes one report away, named by the id it was given when it was reported. */
export function dismiss(id: number): void {
    const remaining = reports.filter((existing) => existing.id !== id);
    if (remaining.length === reports.length) {
        return;
    }
    reports = remaining;
    changed();
}

/** Takes every report away, which is what leaving a screen behind does. */
export function dismissAll(): void {
    if (reports.length === 0) {
        return;
    }
    reports = [];
    changed();
}

/**
 * The sentence a failure carries.
 *
 * An `ApiFailure` is an `Error` whose message is the server's own sentence, so
 * everything that narrows to an `Error` shows what the deployment said rather
 * than a generic line, and anything else is rendered as it arrived.
 */
export function errorMessage(error: unknown): string {
    return error instanceof Error ? error.message : String(error);
}

/**
 * Whether a failure is a state the interface already renders.
 *
 * A 401 is how the session read learns that nobody is signed in: the call
 * answers "no session", the route guard sends the person to the sign-in form,
 * and a banner repeating the refusal would report a door doing its job. Every
 * other refusal is news the person needs, so only the one status the session
 * read owns is drawn here.
 */
export function isExpectedRefusal(error: unknown): boolean {
    return error instanceof ApiFailure && error.status === 401;
}

/** Reports a failure's own sentence, unless it is an expected refusal. */
export function reportError(error: unknown): void {
    if (isExpectedRefusal(error)) {
        return;
    }
    report(errorMessage(error));
}

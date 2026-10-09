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

import { Component, type ErrorInfo, type ReactNode } from 'react';
import { Notice } from '../ui/Primitives.js';

/**
 * The screen that stands in for one that could not render.
 *
 * A component that throws while rendering takes the whole tree with it, and
 * what is left is a blank page and a console nobody is looking at. That is the
 * one failure this interface cannot report any other way: every other one
 * arrives as a rejected promise, and the screens that await those show the
 * message themselves.
 *
 * The words come from the caller rather than from a translation hook, because a
 * boundary is a class and cannot use one; the caller is a function component
 * that can, and passes them in.
 */
export interface ErrorBoundaryProps {
    /** What happened, in the person's language. */
    readonly title: string;
    /** What they can do about it. */
    readonly hint: string;
    readonly children: ReactNode;
}

interface ErrorBoundaryState {
    readonly message?: string;
}

export class ErrorBoundary extends Component<ErrorBoundaryProps, ErrorBoundaryState> {
    constructor(props: ErrorBoundaryProps) {
        super(props);
        this.state = {};
    }

    static getDerivedStateFromError(error: unknown): ErrorBoundaryState {
        return { message: error instanceof Error ? error.message : String(error) };
    }

    override componentDidCatch(error: Error, info: ErrorInfo): void {
        console.error('screen failed to render', error, info.componentStack);
    }

    override render(): ReactNode {
        if (this.state.message === undefined) {
            return this.props.children;
        }
        return (
            <div className="mx-auto w-full max-w-[680px] px-5 py-12">
                <Notice tone="error">
                    <p className="font-semibold">{this.props.title}</p>
                    <p className="mt-2 font-mono break-words">{this.state.message}</p>
                    <p className="mt-2 text-ink-muted">{this.props.hint}</p>
                </Notice>
            </div>
        );
    }
}

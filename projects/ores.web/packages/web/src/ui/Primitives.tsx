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

import { Icon, type IconName } from './Icon.js';
import type {
    ButtonHTMLAttributes,
    InputHTMLAttributes,
    ReactNode,
    Ref,
    SelectHTMLAttributes,
} from 'react';
import { Link, type LinkProps } from 'react-router';

/**
 * The interface primitives.
 *
 * Every screen is built from these, so a change to how a control looks or
 * behaves is one edit rather than a search across pages. They are deliberately
 * few: this is an application, not a component library.
 *
 * The consumers arrive with the screens that use them. `Field`, `Input` and
 * `Select` serve the password field and the tenant details form; `Button`,
 * `Notice` and `cx` serve the journey page; `Detail` serves a review step's
 * key-value list; `Dialog` serves a confirmation, such as discarding a
 * half-provisioned tenant; `PageHeader` and `Tag` serve the shell and the
 * provisioning progress. Nothing imports these two yet.
 */

/** Joins class names, dropping the false ones. */
export function cx(...values: readonly (string | false | null | undefined)[]): string {
    return values.filter(Boolean).join(' ');
}

const BUTTON_BASE =
    'inline-flex items-center justify-center gap-2 rounded-md text-sm font-medium ' +
    'transition-colors duration-100 disabled:opacity-45 disabled:cursor-not-allowed ' +
    'focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent';

const BUTTON_VARIANTS = {
    primary: 'bg-accent text-ink-inverse hover:bg-accent-bright disabled:hover:bg-accent',
    secondary:
        'border border-line bg-surface-overlay text-ink hover:bg-surface-hover hover:border-line-strong',
    ghost: 'text-ink-muted hover:text-ink hover:bg-surface-hover',
    danger: 'border border-line text-down hover:bg-down/10 hover:border-down',
} as const;

const BUTTON_SIZES = {
    sm: 'h-8 px-3',
    md: 'h-9 px-4',
    lg: 'h-10 px-5',
    xl: 'h-12 px-7 text-base',
} as const;

export interface ButtonProps extends ButtonHTMLAttributes<HTMLButtonElement> {
    readonly variant?: keyof typeof BUTTON_VARIANTS;
    readonly size?: keyof typeof BUTTON_SIZES;
    /** Shows a busy state and prevents a second submission. */
    readonly pending?: boolean;
    /** Replaces the label while pending, so the control says what is happening. */
    readonly pendingLabel?: string;
    /** The standard action's icon, drawn before the label. */
    readonly icon?: IconName;
}

export function Button({
    variant = 'secondary',
    size = 'md',
    pending = false,
    pendingLabel,
    icon,
    className,
    children,
    disabled,
    ...rest
}: ButtonProps): ReactNode {
    return (
        <button
            className={cx(BUTTON_BASE, BUTTON_VARIANTS[variant], BUTTON_SIZES[size], className)}
            disabled={disabled === true || pending}
            {...rest}
        >
            {pending && (
                <span
                    aria-hidden
                    className="size-3.5 animate-spin rounded-full border-2 border-current border-t-transparent"
                />
            )}
            {!pending && icon !== undefined && <Icon name={icon} size={size === 'sm' ? 16 : 20} />}
            {pending ? (pendingLabel ?? children) : children}
        </button>
    );
}

/**
 * The same control, for a place rather than an action.
 *
 * A screen the journeys add is a route a person can link to, and a link that
 * looks like every other control is one they can find. It shares the button's
 * classes rather than restating them, so the two cannot drift.
 */
export interface LinkButtonProps extends LinkProps {
    readonly variant?: keyof typeof BUTTON_VARIANTS;
    readonly size?: keyof typeof BUTTON_SIZES;
}

export function LinkButton({
    variant = 'secondary',
    size = 'md',
    className,
    children,
    ...rest
}: LinkButtonProps): ReactNode {
    return (
        <Link
            className={cx(BUTTON_BASE, BUTTON_VARIANTS[variant], BUTTON_SIZES[size], className)}
            {...rest}
        >
            {children}
        </Link>
    );
}

/**
 * A labelled field.
 *
 * The label is a real label, not a styled span, so clicking it focuses the
 * control and a screen reader announces it. The desktop client used a
 * letterspaced uppercase caption, which reads as a desktop form rather than as
 * a web one.
 */
export function Field({
    label,
    hint,
    error,
    children,
    className,
}: {
    readonly label: string;
    readonly hint?: string;
    readonly error?: string;
    readonly children: ReactNode;
    readonly className?: string;
}): ReactNode {
    return (
        <label className={cx('block', className)}>
            <span className="mb-1.5 block text-sm font-medium text-ink-muted">{label}</span>
            {children}
            {hint !== undefined && (
                <span className="mt-1 block text-xs text-ink-faint">{hint}</span>
            )}
            {error !== undefined && <span className="mt-1 block text-xs text-down">{error}</span>}
        </label>
    );
}

const CONTROL =
    'w-full rounded-md border border-line bg-surface-base px-3 py-2 text-sm text-ink ' +
    'placeholder:text-ink-faint transition-colors duration-100 ' +
    'hover:border-line-strong focus:border-accent focus:outline-none ' +
    'focus:ring-3 focus:ring-accent/20 disabled:opacity-50';

export function Input({
    className,
    ref,
    ...rest
}: InputHTMLAttributes<HTMLInputElement> & { readonly ref?: Ref<HTMLInputElement> }): ReactNode {
    return <input ref={ref} className={cx(CONTROL, className)} {...rest} />;
}

export function Select({
    className,
    children,
    ...rest
}: SelectHTMLAttributes<HTMLSelectElement>): ReactNode {
    return (
        <select className={cx(CONTROL, 'cursor-pointer pr-8', className)} {...rest}>
            {children}
        </select>
    );
}

/** A short status marker, for a type, a label or a state. */
export function Tag({
    children,
    tone = 'neutral',
}: {
    readonly children: ReactNode;
    readonly tone?: 'neutral' | 'accent' | 'warn' | 'muted' | 'up' | 'down';
}): ReactNode {
    const tones = {
        neutral: 'border-line text-ink-muted',
        accent: 'border-accent/50 text-accent-bright bg-accent/10',
        warn: 'border-warn/50 text-warn bg-warn/10',
        muted: 'border-line-subtle text-ink-faint',
        up: 'border-up/50 text-up bg-up/10',
        down: 'border-down/50 text-down bg-down/10',
    } as const;
    return (
        <span
            className={cx(
                'inline-block rounded-full border px-2 py-0.5 text-[11px] leading-tight',
                tones[tone],
            )}
        >
            {children}
        </span>
    );
}

/** An inline message, for an error, a warning or a confirmation. */
export function Notice({
    children,
    tone = 'info',
}: {
    readonly children: ReactNode;
    readonly tone?: 'info' | 'warn' | 'error' | 'success';
}): ReactNode {
    const tones = {
        info: 'border-accent/40 bg-accent/10 text-ink',
        warn: 'border-warn/50 bg-warn/10 text-ink',
        error: 'border-down/50 bg-down/10 text-ink',
        success: 'border-up/40 bg-up/10 text-ink',
    } as const;
    return (
        <div
            className={cx('mb-4 rounded-md border px-3 py-2 text-sm', tones[tone])}
            role={tone === 'error' ? 'alert' : 'status'}
        >
            {children}
        </div>
    );
}

/** A page heading, so every screen opens the same way. */
export function PageHeader({
    title,
    description,
    actions,
    mark,
    subdued = false,
}: {
    readonly title: string;
    readonly description?: string;
    readonly actions?: ReactNode;
    /** A picture drawn before the title, such as a currency's flag. */
    readonly mark?: ReactNode;
    /** Draws the description small, for an identifier rather than a sentence. */
    readonly subdued?: boolean;
}): ReactNode {
    const heading = (
        <h1
            className={cx(
                'text-xl font-semibold tracking-tight',
                !subdued && 'flex items-center gap-3',
            )}
        >
            {!subdued && mark}
            {title}
        </h1>
    );
    return (
        <header className="mb-6 flex flex-wrap items-start justify-between gap-4">
            {subdued ? (
                /*
                 * A subdued line is an identifier, which belongs directly under
                 * the name. The picture sits beside the pair, not before the
                 * name alone, so a tall picture does not push the identifier
                 * away from it.
                 */
                <div className="flex items-center gap-3">
                    {mark}
                    <div>
                        {heading}
                        {description !== undefined && (
                            <p className="mt-0.5 break-all font-mono text-xs text-ink-faint">
                                {description}
                            </p>
                        )}
                    </div>
                </div>
            ) : (
                <div>
                    {heading}
                    {description !== undefined && (
                        <p className="mt-1 text-sm text-ink-muted">{description}</p>
                    )}
                </div>
            )}
            {actions !== undefined && (
                <div className="flex shrink-0 items-center gap-2">{actions}</div>
            )}
        </header>
    );
}

/** A small labelled value, used in the detail grids. */
export function Detail({
    label,
    value,
    mono = false,
    href,
}: {
    readonly label: string;
    readonly value: string;
    readonly mono?: boolean;
    /** Makes the value a link, such as a `mailto:` address. */
    readonly href?: string;
}): ReactNode {
    return (
        <div className="min-w-0">
            <dt className="text-[11px] uppercase tracking-wide text-ink-faint">{label}</dt>
            <dd className={cx('mt-0.5 break-words text-sm', mono && 'font-mono text-xs')}>
                {href === undefined ? (
                    value
                ) : (
                    <a
                        href={href}
                        className="underline decoration-line-strong underline-offset-2 hover:decoration-accent"
                    >
                        {value}
                    </a>
                )}
            </dd>
        </div>
    );
}

/** A modal, for the editor and the unlock prompt. */
export function Dialog({
    title,
    onClose,
    children,
    footer,
    wide = false,
}: {
    readonly title: string;
    readonly onClose: () => void;
    readonly children: ReactNode;
    readonly footer?: ReactNode;
    readonly wide?: boolean;
}): ReactNode {
    return (
        <div
            className="fixed inset-0 z-40 flex items-start justify-center overflow-y-auto bg-black/60 p-4 py-10 backdrop-blur-sm"
            role="dialog"
            aria-modal="true"
            aria-label={title}
            onClick={onClose}
        >
            <div
                className={cx('card w-full shadow-2xl', wide ? 'max-w-3xl' : 'max-w-lg')}
                onClick={(event) => event.stopPropagation()}
            >
                <div className="flex items-center justify-between border-b border-line px-5 py-3">
                    <h2 className="text-sm font-semibold">{title}</h2>
                    <Button
                        variant="ghost"
                        size="sm"
                        type="button"
                        onClick={onClose}
                        aria-label="Close"
                    >
                        ✕
                    </Button>
                </div>
                <div className="px-5 py-4">{children}</div>
                {footer !== undefined && (
                    <div className="flex justify-end gap-2 border-t border-line px-5 py-3">
                        {footer}
                    </div>
                )}
            </div>
        </div>
    );
}

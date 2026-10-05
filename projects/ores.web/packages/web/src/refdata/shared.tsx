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

import { useQuery } from '@tanstack/react-query';
import { useEffect, useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import type { BadgePresentation, ClassificationList } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Label } from '../ui/Label.js';
import { Field, Input, Select } from '../ui/Primitives.js';

/** The topics of the classification lists, in screen order. A list's topic is one of these. */
export const TOPICS = [
    'currencies',
    'calendars',
    'parties',
    'books',
    'products',
    'tenors',
    'market-data',
] as const;

/** The badge the catalogue draws a code with when nothing maps it. */
const UNMAPPED = '__unmapped__';

/** The code domain of a list's labels: its entity name, as the label catalogue keys it. */
export function codeDomainOf(list: ClassificationList): string {
    return list.entityType.split('.')[2] ?? '';
}

/** The shared label catalogue, by badge code, and the badge codes each domain uses. */
export function useLabelCatalogue(): {
    readonly byCode: ReadonlyMap<string, BadgePresentation>;
    readonly domains: Readonly<Record<string, readonly string[]>>;
} {
    const catalogue = useQuery({ queryKey: ['labels'], queryFn: api.labels });
    const labels = catalogue.data?.labels ?? [];
    return {
        byCode: new Map(labels.map((label) => [label.code, label])),
        domains: catalogue.data?.domains ?? {},
    };
}

/** Whether a list's code domain has labels at all, which decides whether a screen shows them. */
export function isLabelled(
    list: ClassificationList,
    domains: Readonly<Record<string, readonly string[]>>,
): boolean {
    return (domains[codeDomainOf(list)]?.length ?? 0) > 0;
}

/**
 * A row's label, drawn from the catalogue. A row with none is drawn as the
 * catalogue's Unmapped badge, so a gap in the labels is visible rather than
 * blank.
 */
export function RowLabel({
    labelCode,
    byCode,
}: {
    readonly labelCode: string | null;
    readonly byCode: ReadonlyMap<string, BadgePresentation>;
}): ReactNode {
    const badge = byCode.get(labelCode ?? UNMAPPED) ?? byCode.get(UNMAPPED);
    return <Label text={badge?.label ?? labelCode ?? '—'} badge={badge} />;
}

export { UNMAPPED };

/**
 * What the signed-in person may do to one list: write its rows, remove them,
 * and label them. The server checks each one again; this only decides which
 * buttons a screen offers.
 */
export function usePermissions(list: ClassificationList | undefined): {
    readonly write: boolean;
    readonly remove: boolean;
    readonly label: boolean;
} {
    const access = useQuery({ queryKey: ['my-access'], queryFn: api.myAccess });
    const codes = new Set((access.data?.roles ?? []).flatMap((role) => role.permissionCodes));
    const holds = (code: string, area: string): boolean =>
        codes.has('*') || codes.has(`${area}::*`) || codes.has(code);
    if (list === undefined) {
        return { write: false, remove: false, label: false };
    }
    return {
        write: list.editable && holds(list.writePermission, 'refdata'),
        remove: list.editable && holds(list.deletePermission, 'refdata'),
        label: holds('dq::badge_mappings:write', 'dq'),
    };
}

/** The reasons a correction or a removal may carry, and the chosen one. */
export function useReason(kind: 'amend' | 'delete'): {
    readonly reasons: readonly {
        readonly code: string;
        readonly description: string;
        readonly requiresCommentary: boolean;
    }[];
    readonly code: string;
    readonly setCode: (code: string) => void;
    readonly needsCommentary: boolean;
} {
    const reasons = useQuery({
        queryKey: ['reference-data-reasons', kind],
        queryFn: () => api.referenceDataReasons(kind),
    });
    const [code, setCode] = useState('');
    useEffect(() => {
        if (code === '' && reasons.data !== undefined && reasons.data.length > 0) {
            setCode(reasons.data[0]?.code ?? '');
        }
    }, [code, reasons.data]);
    const list = reasons.data ?? [];
    return {
        reasons: list,
        code,
        setCode,
        needsCommentary: list.find((reason) => reason.code === code)?.requiresCommentary ?? false,
    };
}

/** The trail back up from a page: every part but the last is a link. */
export function Crumbs({
    parts,
}: {
    readonly parts: readonly { readonly label: string; readonly to?: string }[];
}): ReactNode {
    const { t } = useTranslation();
    return (
        <nav
            aria-label={t('refdata.crumbs')}
            className="mb-3 flex flex-wrap gap-1.5 text-xs text-ink-faint"
        >
            {parts.map((part, index) => (
                <span key={part.label} className="flex gap-1.5">
                    {part.to === undefined ? (
                        <span>{part.label}</span>
                    ) : (
                        <Link to={part.to} className="text-accent hover:text-accent-bright">
                            {part.label}
                        </Link>
                    )}
                    {index < parts.length - 1 && <span aria-hidden>›</span>}
                </span>
            ))}
        </nav>
    );
}

/** The address of the classification index, a list, or a row of it. */
export function classificationsPath(list?: string, code?: string): string {
    const base = '/refdata/classifications';
    if (list === undefined) {
        return base;
    }
    const listPath = `${base}/${encodeURIComponent(list)}`;
    return code === undefined ? listPath : `${listPath}/${encodeURIComponent(code)}`;
}

/**
 * Chooses a row's label from the shared catalogue.
 *
 * It offers the labels the list's code domain already uses, and the rest of
 * the catalogue only when asked, grouped by the domain that uses each label.
 * A list may borrow another's label, but the default choice is its own.
 */
export function LabelPicker({
    list,
    value,
    onChange,
}: {
    readonly list: ClassificationList;
    readonly value: string;
    readonly onChange: (badgeCode: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const { byCode, domains } = useLabelCatalogue();
    const [all, setAll] = useState(false);
    const domain = codeDomainOf(list);
    const own = [UNMAPPED, ...(domains[domain] ?? [])];
    const name = (code: string): string => byCode.get(code)?.label ?? code;
    const others = Object.entries(domains)
        .filter(([other]) => other !== domain)
        .sort(([a], [b]) => a.localeCompare(b))
        .map(([other, codes]) => [other, codes.filter((code) => !own.includes(code))] as const)
        .filter(([, codes]) => codes.length > 0);
    const offered = new Set([...own, ...others.flatMap(([, codes]) => codes)]);
    const unused = [...byCode.keys()].filter((code) => !offered.has(code));
    const current = own.includes(value) || all ? [] : [value];

    return (
        <div className="space-y-1.5">
            <span className="block text-sm font-medium text-ink-muted">
                {t('refdata.classifications.label')}
            </span>
            <div className="flex items-center gap-2">
                <select
                    className="w-full cursor-pointer rounded-md border border-line bg-surface-base px-3 py-2 text-sm"
                    aria-label={t('refdata.classifications.label')}
                    value={value}
                    onChange={(event) => onChange(event.target.value)}
                >
                    <optgroup label={t(`refdata.classifications.lists.${list.key}`)}>
                        {own.map((code) => (
                            <option key={code} value={code}>
                                {name(code)}
                            </option>
                        ))}
                    </optgroup>
                    {current.map((code) => (
                        <optgroup key={code} label={t('refdata.classifications.labelCurrent')}>
                            <option value={code}>{name(code)}</option>
                        </optgroup>
                    ))}
                    {all &&
                        others.map(([other, codes]) => (
                            <optgroup key={other} label={other.replaceAll('_', ' ')}>
                                {codes.map((code) => (
                                    <option key={code} value={code}>
                                        {name(code)}
                                    </option>
                                ))}
                            </optgroup>
                        ))}
                    {all && unused.length > 0 && (
                        <optgroup label={t('refdata.classifications.labelUnused')}>
                            {unused.map((code) => (
                                <option key={code} value={code}>
                                    {name(code)}
                                </option>
                            ))}
                        </optgroup>
                    )}
                </select>
                <RowLabel labelCode={value} byCode={byCode} />
            </div>
            <label className="flex items-center gap-2 text-xs text-ink-faint">
                <input
                    type="checkbox"
                    checked={all}
                    onChange={(event) => setAll(event.target.checked)}
                />
                {t('refdata.classifications.labelOthers')}
            </label>
        </div>
    );
}

/** A reason picker and its commentary, refusing an empty commentary the reason needs. */
export function ReasonFields({
    reason,
    commentary,
    onCommentary,
    missing,
}: {
    readonly reason: ReturnType<typeof useReason>;
    readonly commentary: string;
    readonly onCommentary: (value: string) => void;
    readonly missing: boolean;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <>
            <Field label={t('refdata.classifications.reason')}>
                <Select
                    value={reason.code}
                    onChange={(event) => reason.setCode(event.target.value)}
                >
                    {reason.reasons.map((choice) => (
                        <option key={choice.code} value={choice.code}>
                            {choice.description}
                        </option>
                    ))}
                </Select>
            </Field>
            <Field
                label={t('refdata.classifications.commentary')}
                {...(missing ? { error: t('refdata.classifications.commentaryRequired') } : {})}
            >
                <Input
                    value={commentary}
                    maxLength={2000}
                    onChange={(event) => onCommentary(event.target.value)}
                />
            </Field>
        </>
    );
}

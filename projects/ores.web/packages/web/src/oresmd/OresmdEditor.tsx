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
import { useEffect, useId, useRef, useState, type ReactNode } from 'react';
import type { OresmdComposeRequest, OresmdSchema } from '@ores/contracts';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Dialog, Input, Notice, Select, Tag } from '../ui/Primitives.js';
import {
    EMPTY,
    KINDS,
    composeRequestOf,
    fromParse,
    keyOf,
    rowOf,
    withInstrument,
    withKind,
    withValue,
    type EditorField,
    type EditorState,
} from './editor.js';
import { oresmdQueries } from './queries.js';

/** How long the editor waits after a keystroke before it asks the codec. */
const ASK_DELAY_MS = 250;

/** How long the word Copied stays after a copy. */
const COPIED_MS = 2000;

/** The text after a delay, so a person typing is not answered on every key. */
function useDelayed<T>(value: T, delayMs: number): T {
    const [delayed, setDelayed] = useState(value);
    useEffect(() => {
        const timer = setTimeout(() => setDelayed(value), delayMs);
        return () => clearTimeout(timer);
    }, [value, delayMs]);
    return delayed;
}

/** The fields whose text is a currency code, which the currencies the system keeps can suggest. */
const CURRENCY_FIELDS: ReadonlySet<string> = new Set([
    'ccy',
    'flat_ccy',
    'float_ccy',
    'fixed_ccy',
    'unit_ccy',
]);

/** Whether the system keeps a catalogue that can suggest values for the field. */
function suggested(field: string): boolean {
    return CURRENCY_FIELDS.has(field) || field === 'day_counter';
}

/** The values of a datalist, which the person may overtype: ORE accepts values the catalogue omits. */
function Options({
    id,
    values,
}: {
    readonly id: string;
    readonly values: readonly string[];
}): ReactNode {
    return (
        <datalist id={id}>
            {values
                .filter((value) => value !== '')
                .map((value) => (
                    <option key={value} value={value} />
                ))}
        </datalist>
    );
}

/** Each catalogue is its own component, because a field with no catalogue must not read one. */
function CurrencySuggestions({ id }: { readonly id: string }): ReactNode {
    const currencies = useQuery(oresmdQueries.currencies());
    return (
        <Options
            id={id}
            values={(currencies.data ?? []).map((row) => String(row['iso_code'] ?? ''))}
        />
    );
}

function DayCounterSuggestions({ id }: { readonly id: string }): ReactNode {
    const dayCounters = useQuery(oresmdQueries.dayCounters());
    return <Options id={id} values={(dayCounters.data ?? []).map((row) => row.code)} />;
}

function Suggestions({ id, field }: { readonly id: string; readonly field: string }): ReactNode {
    return CURRENCY_FIELDS.has(field) ? (
        <CurrencySuggestions id={id} />
    ) : (
        <DayCounterSuggestions id={id} />
    );
}

/**
 * The editor for an oresmd URL, drawn as a dialog over the screen that calls it.
 *
 * It loads `url`, offers the schema row of the chosen kind with one input for
 * each field, and asks the server's codec to write the URI after each change, so
 * the person sees the codec's own spelling or its reason before anything is
 * copied or applied. A closed set is a select. A catalogue the system keeps
 * suggests values that may be overtyped. The editor holds none of the grammar.
 *
 * The editor reads `url` when it opens. To edit another URL, give the dialog a
 * new `key`, or unmount it on close as a modal does.
 *
 * `onApply` receives the URI the codec wrote, and Apply is offered only when
 * the codec wrote one for what is on screen. A screen that keeps no URL to
 * change opens `OresmdViewDialog` instead, which shows the URL and edits nothing.
 */
export function OresmdEditorDialog({
    url,
    onApply,
    onClose,
}: {
    /** The URI to edit. Empty starts a new one. */
    readonly url: string;
    readonly onApply: (uri: string) => void;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const schema = useQuery(oresmdQueries.schema());
    const parsed = useQuery({ ...oresmdQueries.parse(url), enabled: url !== '' });
    const [state, setState] = useState<EditorState>(EMPTY);
    const [loaded, setLoaded] = useState(url === '');
    const [copied, setCopied] = useState(false);
    const copiedTimer = useRef<ReturnType<typeof setTimeout> | undefined>(undefined);
    useEffect(() => () => clearTimeout(copiedTimer.current), []);

    useEffect(() => {
        if (loaded) return;
        if (parsed.data !== undefined) {
            setState(fromParse(parsed.data));
            setLoaded(true);
        } else if (parsed.isError) {
            // The URL could not be read at all. The editor opens empty and says so.
            setLoaded(true);
        }
    }, [loaded, parsed.data, parsed.isError]);

    const request = schema.data === undefined ? null : composeRequestOf(schema.data, state);
    const key = keyOf(request);
    // The codec is asked once the person pauses, not on every key.
    const askedKey = useDelayed(key, ASK_DELAY_MS);
    const asked = askedKey === '' ? null : (JSON.parse(askedKey) as OresmdComposeRequest);
    const composed = useQuery(oresmdQueries.compose(asked));
    // The answer is for what is on screen only when the codec was asked about
    // exactly this and has answered. Until then it is the answer to an earlier
    // edit, so Apply and Copy wait.
    const settled = key !== '' && askedKey === key && !composed.isFetching;
    const uri = settled && composed.data?.success === true ? composed.data.uri : '';
    // A request that failed is not an answer from the codec. Its reason is shown
    // too, because the reads are quiet and nothing else would say why Apply is off.
    const failure =
        key !== '' && askedKey === key && composed.isError ? composed.error.message : '';

    const copy = async (): Promise<void> => {
        try {
            await navigator.clipboard.writeText(uri);
            setCopied(true);
            clearTimeout(copiedTimer.current);
            copiedTimer.current = setTimeout(() => setCopied(false), COPIED_MS);
        } catch {
            setCopied(false);
        }
    };

    return (
        <Dialog
            title={t('oresmd.title')}
            name="oresmd-url-editor"
            wide
            onClose={onClose}
            footer={
                <>
                    <Button type="button" variant="ghost" onClick={onClose}>
                        {t('oresmd.cancel')}
                    </Button>
                    <Button type="button" disabled={uri === ''} onClick={() => void copy()}>
                        {copied ? t('oresmd.copied') : t('oresmd.copy')}
                    </Button>
                    <Button
                        type="button"
                        variant="primary"
                        disabled={uri === ''}
                        onClick={() => onApply(uri)}
                    >
                        {t('oresmd.apply')}
                    </Button>
                </>
            }
        >
            {schema.isError ? (
                <Notice tone="error">{schema.error.message}</Notice>
            ) : schema.isPending || !loaded ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : (
                <div className="space-y-4">
                    {url !== '' && parsed.isError && (
                        <Notice tone="warn">
                            {t('oresmd.refused', { message: parsed.error.message })}
                        </Notice>
                    )}
                    {url !== '' && parsed.data?.success === false && (
                        <Notice tone="warn">
                            {t('oresmd.refused', { message: parsed.data.message })}
                        </Notice>
                    )}
                    <Composer schema={schema.data} state={state} onChange={setState} />
                    <Preview
                        uri={uri}
                        message={
                            request === null
                                ? t('oresmd.choose')
                                : settled && composed.data?.success === false
                                  ? composed.data.message
                                  : failure
                        }
                        working={key !== '' && !settled}
                    />
                </div>
            )}
        </Dialog>
    );
}

export function Composer({
    schema,
    state,
    onChange,
}: {
    readonly schema: OresmdSchema;
    readonly state: EditorState;
    readonly onChange: (state: EditorState) => void;
}): ReactNode {
    const { t } = useTranslation();
    const row = rowOf(schema, state);
    const fixing = state.kind === 'fixing';
    const instruments = schema.assets.map((asset) => ({
        asset,
        items: schema.instruments.filter((instrument) => instrument.asset === asset),
    }));
    return (
        <div className="space-y-3">
            <div role="group" aria-label={t('oresmd.kind')} className="flex gap-2">
                {KINDS.map((kind) => (
                    <button
                        key={kind}
                        type="button"
                        aria-pressed={state.kind === kind}
                        className={
                            state.kind === kind
                                ? 'rounded-md border border-accent bg-accent/10 px-3 py-1 text-sm text-accent-bright'
                                : 'rounded-md border border-line px-3 py-1 text-sm text-ink-muted hover:text-ink'
                        }
                        onClick={() => onChange(withKind(state, kind))}
                    >
                        {t(`oresmd.kinds.${kind}`)}
                    </button>
                ))}
            </div>
            <div className="grid gap-3 md:grid-cols-2">
                <label className="block text-sm">
                    <span className="mb-1 block text-ink-muted">
                        {fixing ? t('oresmd.family') : t('oresmd.instrument')}
                    </span>
                    <Select
                        value={state.instrument}
                        onChange={(event) =>
                            onChange(withInstrument(schema, state, event.target.value))
                        }
                    >
                        <option value="" />
                        {fixing
                            ? schema.indexes.map((family) => (
                                  <option key={family.code} value={family.code}>
                                      {family.code}
                                  </option>
                              ))
                            : instruments.map((group) => (
                                  <optgroup key={group.asset} label={group.asset}>
                                      {group.items.map((instrument) => (
                                          <option key={instrument.code} value={instrument.code}>
                                              {instrument.code}
                                          </option>
                                      ))}
                                  </optgroup>
                              ))}
                    </Select>
                </label>
                {!fixing && (
                    <label className="block text-sm">
                        <span className="mb-1 block text-ink-muted">{t('oresmd.quote')}</span>
                        <Select
                            value={state.quote}
                            onChange={(event) => onChange({ ...state, quote: event.target.value })}
                        >
                            <option value="" />
                            {schema.quotes.map((quote) => (
                                <option key={quote} value={quote}>
                                    {quote}
                                </option>
                            ))}
                        </Select>
                    </label>
                )}
            </div>
            {row !== null && (
                <div className="space-y-2">
                    <p className="text-xs text-ink-faint">
                        {t('oresmd.authority', { asset: row.asset })}
                    </p>
                    <FieldInput
                        field={{
                            name: row.subject,
                            role: 'identity',
                            optional: false,
                            kind: 'text',
                            codes: [],
                        }}
                        label={t('oresmd.path', { name: row.subject })}
                        value={state.subject}
                        onChange={(text) => onChange({ ...state, subject: text })}
                    />
                    {row.fields.map((field) => (
                        <FieldInput
                            key={field.name}
                            field={field}
                            label={field.name}
                            value={state.values[field.name] ?? ''}
                            onChange={(text) => onChange(withValue(state, field.name, text))}
                        />
                    ))}
                </div>
            )}
        </div>
    );
}

function FieldInput({
    field,
    label,
    value,
    onChange,
}: {
    readonly field: EditorField;
    readonly label: string;
    readonly value: string;
    readonly onChange: (text: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const id = useId();
    const list = `${id}-suggestions`;
    return (
        <div className="grid items-center gap-2 text-sm md:grid-cols-[12rem_1fr_auto]">
            <label htmlFor={id} className="font-mono text-ink">
                {label}
            </label>
            {field.codes.length > 0 ? (
                <Select id={id} value={value} onChange={(event) => onChange(event.target.value)}>
                    <option value="" />
                    {field.codes.map((code) => (
                        <option key={code} value={code}>
                            {code}
                        </option>
                    ))}
                </Select>
            ) : (
                <>
                    <Input
                        id={id}
                        value={value}
                        list={suggested(field.name) ? list : undefined}
                        onChange={(event) => onChange(event.target.value)}
                    />
                    {suggested(field.name) && <Suggestions id={list} field={field.name} />}
                </>
            )}
            <span className="flex gap-1">
                <Tag small tone={field.optional ? 'muted' : 'accent'}>
                    {field.optional ? t('oresmd.optional') : t('oresmd.required')}
                </Tag>
                {field.role === 'coordinate' && <Tag small>{t('oresmd.coordinate')}</Tag>}
            </span>
        </div>
    );
}

function Preview({
    uri,
    message,
    working,
}: {
    readonly uri: string;
    readonly message: string;
    readonly working: boolean;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section aria-label={t('oresmd.preview')} className="space-y-2">
            <h3 className="text-xs uppercase tracking-wider text-ink-faint">
                {t('oresmd.preview')}
            </h3>
            <pre className="overflow-x-auto rounded-md bg-surface-overlay p-3 font-mono text-xs text-ink">
                {uri === '' ? (working ? t('common.loading') : '') : uri}
            </pre>
            {message !== '' && <Notice tone="warn">{message}</Notice>}
        </section>
    );
}

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
import { type ReactNode } from 'react';
import type { OresmdParseResult, OresmdSchema } from '@ores/contracts';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Detail, Dialog, Notice } from '../ui/Primitives.js';
import { fromParse, rowOf } from './editor.js';
import { oresmdQueries } from './queries.js';
import { titleOfUrl } from './title.js';
import { useCopyToClipboard } from './useCopyToClipboard.js';

/**
 * An oresmd URL read as labelled values, in the layout of the record screens'
 * non-edit mode: a small label over each value, in a grid, and no inputs. The
 * parts come from the server's parse, so the view holds none of the grammar.
 * The URL itself stands under the values, in the codec's spelling.
 */
export function OresmdView({
    schema,
    parts,
    url,
}: {
    readonly schema: OresmdSchema;
    readonly parts: OresmdParseResult;
    readonly url: string;
}): ReactNode {
    const { t } = useTranslation();
    if (!parts.success) {
        return (
            <div className="space-y-3">
                <Notice tone="warn">{t('oresmd.refused', { message: parts.message })}</Notice>
                <UrlText url={url} />
            </div>
        );
    }
    const fixing = parts.kind === 'fixing';
    const path = rowOf(schema, fromParse(parts))?.subject ?? '';
    const title = titleOfUrl(schema, parts);
    return (
        <div className="space-y-4">
            <dl className="grid gap-4 sm:grid-cols-2">
                {title !== undefined && <Detail label={t('oresmd.series')} value={title} />}
                <Detail label={t('oresmd.kind')} value={t(`oresmd.kinds.${parts.kind}`)} />
                <Detail
                    label={fixing ? t('oresmd.family') : t('oresmd.instrument')}
                    value={parts.instrument}
                    mono
                />
                {!fixing && <Detail label={t('oresmd.quote')} value={parts.quote} mono />}
                <Detail label={t('oresmd.assetClass')} value={parts.asset} mono />
                <Detail label={t('oresmd.path', { name: path })} value={parts.subject} mono />
                {parts.fields.map((field) => (
                    <Detail key={field.name} label={field.name} value={field.text} mono />
                ))}
            </dl>
            <UrlText url={url} />
        </div>
    );
}

function UrlText({ url }: { readonly url: string }): ReactNode {
    const { t } = useTranslation();
    return (
        <section aria-label={t('oresmd.preview')} className="space-y-1">
            <h3 className="text-xs text-ink-faint">{t('oresmd.preview')}</h3>
            <pre className="overflow-x-auto rounded-md bg-surface-overlay p-3 font-mono text-xs text-ink">
                {url}
            </pre>
        </section>
    );
}

/**
 * The dialog that shows an oresmd URL and edits nothing, for a screen that keeps
 * no URL of its own to change. It offers Copy and Close.
 */
export function OresmdViewDialog({
    url,
    onClose,
}: {
    readonly url: string;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const schema = useQuery(oresmdQueries.schema());
    const parsed = useQuery(oresmdQueries.parse(url));
    const { copied, failed, copy } = useCopyToClipboard(url);
    return (
        <Dialog
            title={t('oresmd.title')}
            name="oresmd-url-view"
            wide
            onClose={onClose}
            footer={
                <>
                    <Button type="button" variant="ghost" onClick={onClose}>
                        {t('oresmd.close')}
                    </Button>
                    <Button type="button" onClick={copy}>
                        {copied ? t('oresmd.copied') : t('oresmd.copy')}
                    </Button>
                </>
            }
        >
            {failed && <Notice tone="error">{t('oresmd.copyFailed')}</Notice>}
            {schema.isError ? (
                <Notice tone="error">{schema.error.message}</Notice>
            ) : parsed.isError ? (
                <Notice tone="error">{parsed.error.message}</Notice>
            ) : schema.data === undefined || parsed.data === undefined ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : (
                <OresmdView schema={schema.data} parts={parsed.data} url={url} />
            )}
        </Dialog>
    );
}

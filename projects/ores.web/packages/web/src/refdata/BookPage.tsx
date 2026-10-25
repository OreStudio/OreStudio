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
import type { ReactNode } from 'react';
import { Link, useParams } from 'react-router';
import { books } from '../api/books.js';
import { useTranslation } from '../i18n/Provider.js';
import { Crumbs } from './shared.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { useAreaParts } from '../shell/AreaTrail.js';
import { PATHS } from '../shell/paths.js';
import { Fact, Notice, Tag } from '../ui/Primitives.js';

/**
 * One book: what it is called, which portfolio it sits in, and the codes that
 * say what it is for. A trade's book link lands here.
 */
export function BookPage(): ReactNode {
    const { t } = useTranslation();
    const { id = '' } = useParams();
    const open = useQuery({
        queryKey: ['books', id],
        queryFn: () => books.one(id),
        enabled: id !== '',
    });
    const trail = useAreaParts('refdata', t('refdata.books.title'));

    if (open.isPending) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={trail} />}
                title={t('refdata.books.title')}
                description={t('refdata.books.lead')}
            >
                <p className="text-sm text-ink-muted" role="status">
                    {t('common.loading')}
                </p>
            </ScreenFrame>
        );
    }

    if (open.isError) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={[...trail, { label: id }]} />}
                title={t('refdata.books.title')}
                description={t('refdata.books.lead')}
            >
                <div className="space-y-3">
                    <Notice tone="info">{t('refdata.books.notFound')}</Notice>
                    <Link
                        to={PATHS.refdata}
                        className="text-sm text-accent hover:text-accent-bright"
                    >
                        {t('refdata.books.backToRefdata')}
                    </Link>
                </div>
            </ScreenFrame>
        );
    }

    const { book, portfolio } = open.data;
    return (
        <ScreenFrame
            trail={<Crumbs parts={[...trail, { label: book.name }]} />}
            title={book.name}
            description={portfolio === null ? '' : portfolio.name}
        >
            <dl className="grid grid-cols-2 gap-4 md:grid-cols-4">
                <Fact label={t('refdata.books.name')} value={book.name} />
                <Fact label={t('refdata.books.portfolio')} value={portfolio?.name ?? ''} />
                <Fact label={t('refdata.books.status')}>
                    <Tag tone={book.book_status === 'Active' ? 'up' : 'muted'}>
                        {book.book_status}
                    </Tag>
                </Fact>
                <Fact label={t('refdata.books.purpose')} value={book.book_purpose_type} />
                <Fact label={t('refdata.books.regulatoryType')} value={book.regulatory_book_type} />
                <Fact label={t('refdata.books.currency')} value={book.functional_currency} />
                <Fact label={t('refdata.books.costCenter')} value={book.cost_center} />
                <Fact label={t('refdata.books.glAccount')} value={book.gl_account_ref} />
            </dl>
        </ScreenFrame>
    );
}

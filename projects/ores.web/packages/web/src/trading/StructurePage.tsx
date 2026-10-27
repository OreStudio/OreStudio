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
import { useMemo } from 'react';
import { useState, type ReactNode } from 'react';
import { Link, useParams } from 'react-router';
import { structures, type StructurePageRecord } from '../api/structures.js';
import { useTranslation } from '../i18n/Provider.js';
import { Crumbs } from '../refdata/shared.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { PATHS } from '../shell/paths.js';
import { Button, Fact, Field, Notice, Select } from '../ui/Primitives.js';

/** The frame a structure's page is drawn in, shared by its three states. */
function Frame({
    trail,
    title,
    description,
    children,
}: {
    readonly trail: readonly { readonly label: string; readonly to?: string }[];
    readonly title: string;
    readonly description: string;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <ScreenFrame
            trail={<Crumbs parts={[...trail]} />}
            title={title}
            description={description}
        >
            {children}
        </ScreenFrame>
    );
}

/** The legs of the structure, with the reference of each trade and the role it takes. */
function LegTable({ record }: { readonly record: StructurePageRecord }): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="overflow-hidden rounded-md border border-line">
            <h2 className="border-b border-line bg-surface-2 px-3 py-2 text-sm font-medium text-ink">
                {t('trading.structure.legsCaption')}
            </h2>
            {record.legs.length === 0 ? (
                <p className="px-3 py-3 text-sm text-ink-muted">{t('trading.structure.noLegs')}</p>
            ) : (
                <table className="w-full text-left text-sm">
                    <thead>
                        <tr className="border-b border-line text-xs uppercase tracking-wide text-ink-faint">
                            <th className="px-3 py-2 font-medium">
                                {t('trading.structure.sequence')}
                            </th>
                            <th className="px-3 py-2 font-medium">{t('trading.structure.trade')}</th>
                            <th className="px-3 py-2 font-medium">{t('trading.structure.role')}</th>
                        </tr>
                    </thead>
                    <tbody>
                        {record.legs.map((leg) => (
                            <tr key={leg.trade_id} className="border-b border-line last:border-0">
                                <td className="px-3 py-2 text-ink-muted">{leg.sequence_number}</td>
                                <td className="px-3 py-2 font-mono text-ink">
                                    <Link
                                        className="text-accent hover:text-accent-bright"
                                        to={`${PATHS.trades}/${leg.trade_id}`}
                                    >
                                        {leg.reference === '' ? leg.trade_id : leg.reference}
                                    </Link>
                                </td>
                                <td className="px-3 py-2 text-ink">{leg.role}</td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            )}
        </section>
    );
}

/** The form that makes a trade a leg of this structure, in a role. */
function AddLegForm({ record }: { readonly record: StructurePageRecord }): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [tradeId, setTradeId] = useState('');
    const [role, setRole] = useState(record.roles[0] ?? '');

    const candidates = useQuery({
        queryKey: ['structure', record.id, 'leg-candidates'],
        queryFn: () => structures.legCandidates(record.id),
    });
    const roles = useMemo(
        () => (record.roles.length > 0 ? record.roles : ['leg']),
        [record.roles],
    );
    const add = useMutation({
        mutationFn: () => structures.addLeg(record.id, tradeId, role),
        onSuccess: async () => {
            setTradeId('');
            await queries.invalidateQueries({ queryKey: ['structure', record.id] });
            await queries.invalidateQueries({
                queryKey: ['structure', record.id, 'leg-candidates'],
            });
        },
    });

    const rows = candidates.data ?? [];

    return (
        <section className="space-y-3 rounded-md border border-line bg-surface-raised p-4">
            <h2 className="text-sm font-medium text-ink">{t('trading.structure.addLeg')}</h2>
            <p className="text-sm text-ink-muted">{t('trading.structure.addLegLead')}</p>
            {candidates.isError && <Notice tone="error">{candidates.error.message}</Notice>}
            {add.isError && <Notice tone="error">{add.error.message}</Notice>}
            <div className="grid gap-3 md:grid-cols-2">
                <Field label={t('trading.structure.trade')}>
                    <Select value={tradeId} onChange={(event) => setTradeId(event.target.value)}>
                        <option value="">{t('trading.booking.choose')}</option>
                        {rows.map((row) => (
                            <option key={row.id} value={row.id}>
                                {row.reference === '' ? row.id : row.reference}
                            </option>
                        ))}
                    </Select>
                </Field>
                <Field label={t('trading.structure.role')}>
                    <Select value={role} onChange={(event) => setRole(event.target.value)}>
                        {roles.map((value) => (
                            <option key={value} value={value}>
                                {value}
                            </option>
                        ))}
                    </Select>
                </Field>
            </div>
            <Button
                type="button"
                variant="primary"
                pending={add.isPending}
                disabled={tradeId === '' || role === ''}
                onClick={() => add.mutate()}
            >
                {t('trading.structure.addLegSubmit')}
            </Button>
        </section>
    );
}

/**
 * The page a structure opens on: what it is, and the trades grouped into it.
 *
 * A structure is read by the reference the desk gave it, which is the name the
 * blotter shows, and not by its id: an id is not something a person says.
 */
export function StructurePage(): ReactNode {
    const { t } = useTranslation();
    const { id = '' } = useParams();
    const open = useQuery({
        queryKey: ['structure', id],
        queryFn: () => structures.one(id),
        enabled: id !== '',
    });
    const trail = [
        { label: t('shell.menu.home'), to: '/' },
        { label: t('shell.menu.trading'), to: PATHS.trading },
        { label: t('trading.trades.title'), to: PATHS.trades },
    ];

    if (open.isPending) {
        return (
            <Frame
                trail={trail}
                title={t('trading.structure.title')}
                description={t('trading.structure.lead')}
            >
                <p className="text-sm text-ink-muted" role="status">
                    {t('common.loading')}
                </p>
            </Frame>
        );
    }

    if (open.isError) {
        return (
            <Frame
                trail={[...trail, { label: id }]}
                title={t('trading.structure.title')}
                description={t('trading.structure.lead')}
            >
                <div className="space-y-3">
                    <Notice tone="info">{t('trading.structure.notFound')}</Notice>
                    <Link
                        to={PATHS.trades}
                        className="text-sm text-accent hover:text-accent-bright"
                    >
                        {t('trading.structure.backToList')}
                    </Link>
                </div>
            </Frame>
        );
    }

    const record = open.data;
    const heading = record.reference === '' ? record.id : record.reference;

    return (
        <Frame
            trail={[...trail, { label: heading }]}
            title={heading}
            description={t('trading.structure.lead')}
        >
            <div className="space-y-5">
                <dl className="grid grid-cols-2 gap-4 md:grid-cols-4">
                    <Fact label={t('trading.structure.kind')} value={record.kind} />
                    <Fact label={t('trading.structure.template')} value={record.template_code} />
                    <Fact label={t('trading.structure.counterparty')}>
                        {record.counterparty_id === '' ? (
                            record.counterparty_name
                        ) : (
                            <Link
                                className="text-accent hover:text-accent-bright"
                                to={`${PATHS.counterparties}/${record.counterparty_id}`}
                            >
                                {record.counterparty_name}
                            </Link>
                        )}
                    </Fact>
                    <Fact label={t('trading.structure.legs')} value={String(record.legs.length)} />
                </dl>
                <LegTable record={record} />
                <AddLegForm record={record} />
            </div>
        </Frame>
    );
}

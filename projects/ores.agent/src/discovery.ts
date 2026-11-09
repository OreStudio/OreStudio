// SPDX-License-Identifier: GPL-3.0-or-later
import { markdownTable } from './markdown.js';
import { z } from 'zod';
import { readPages } from './read-pages.js';
import { AppError, type Backend, type Operation } from './backend.js';
import type { Session } from './auth.js';
import { marketId } from './market.js';
const search = {
    query: z.string().max(120).default(''),
    offset: z.number().int().min(0).max(1000).default(0),
    limit: z.number().int().min(1).max(25).default(10),
};
export const discoveryTools = {
    find_trades: {
        description:
            'Find selected-party trades by ID, origin reference or trade type. Optional book/portfolio names or IDs use authoritative booking membership. Does not search currency economics.',
        schema: z
            .object({
                ...search,
                tradeType: z.string().max(80).optional(),
                book: z.string().max(120).optional(),
                portfolio: z.string().max(120).optional(),
            })
            .strict(),
    },
    read_trade: {
        description:
            'Read an exact discovered trade UUID, booking, direct portfolio links and supported instrument economics. FxForward economics supported; Bond returns issue reference/notional only. No pricing or writes.',
        schema: z.object({ tradeId: marketId }).strict(),
    },
    find_books: {
        description:
            'Find selected-party books by literal name or UUID. Multiple matches need explicit selection.',
        schema: z.object(search).strict(),
    },
    find_portfolios: {
        description:
            'Find selected-party portfolios by literal name or UUID. Membership comes from server records, not name similarity.',
        schema: z.object(search).strict(),
    },
    find_counterparties: {
        description:
            'Find entitled tenant-wide counterparties by exact short code/UUID or literal name within a catalogue page. offset is the catalogue offset; follow nextOffset for more. Partial name search cannot prove absence or uniqueness.',
        schema: z
            .object({ ...search, offset: z.number().int().min(0).max(1000000).default(0) })
            .strict(),
    },
} as const;
const scoped = { id: marketId, tenant_id: marketId, party_id: marketId };
const trade = z.object({
    ...scoped,
    version: z.number().int(),
    trade_type: z.string(),
    counterparty_id: marketId.nullable(),
    origin_reference: z.string(),
    booking_nature: z.string(),
});
const book = z.object({
    ...scoped,
    name: z.string(),
    parent_portfolio_id: marketId,
    functional_currency: z.string(),
});
const portfolio = z.object({
    ...scoped,
    name: z.string(),
    parent_portfolio_id: marketId.nullable(),
    aggregation_ccy: z.string(),
});
const counterparty = z.object({
    id: marketId,
    tenant_id: marketId,
    short_code: z.string(),
    full_name: z.string(),
    status: z.string(),
});
const specs = {
    trades: { key: 'trades', schema: trade },
    books: { key: 'books', schema: book },
    portfolios: { key: 'portfolios', schema: portfolio },
    counterparties: { key: 'counterparties', schema: counterparty },
} as const;
type Resource = keyof typeof specs;
type Row = Record<string, unknown>;
function accepted(raw: unknown) {
    const r = z.object({ result: z.object({ outcome: z.string() }) }).parse(raw);
    if (r.result.outcome !== 'ok')
        throw new AppError(
            'discovery_refused',
            'Backend did not complete or authorize this discovery read.',
            403,
        );
}
function missing(raw: unknown) {
    return (
        z.object({ result: z.object({ outcome: z.string() }) }).parse(raw).result.outcome ===
        'missing'
    );
}
function matches(row: Row, query: string) {
    return Object.entries(row).some(
        ([k, v]) =>
            ['id', 'name', 'short_code', 'full_name', 'origin_reference', 'trade_type'].includes(
                k,
            ) &&
            typeof v === 'string' &&
            v.toLowerCase().includes(query.toLowerCase()),
    );
}
export class Discovery {
    constructor(private backend: Backend) {}
    async execute(
        name: string,
        raw: unknown,
        session: Session,
        signal?: AbortSignal,
    ): Promise<unknown> {
        const args = discoveryTools[name as keyof typeof discoveryTools].schema.parse(
            raw,
        ) as Record<string, unknown>;
        const deadline = AbortSignal.any([...(signal ? [signal] : []), AbortSignal.timeout(30000)]);
        const check = () => {
            deadline.throwIfAborted();
            if (!session.active || session.expires <= Date.now())
                throw new AppError('session_expired', 'Sign in again.', 401);
        };
        const snapshot = new Date().toISOString();
        const request = async (op: Operation, payload: unknown) => {
            check();
            const r = await this.backend.request(op, payload, session, deadline);
            check();
            return r;
        };
        const own = (r: Row) =>
            r.tenant_id === session.tenantId &&
            (!('party_id' in r) || r.party_id === session.partyId);
        const scan = async (resource: Resource): Promise<Row[]> => {
            const spec = specs[resource];
            const rows = await readPages(
                async (offset, limit) => {
                    const raw = await request(resource, {
                        offset,
                        limit,
                        order: { field: '', descending: false },
                        filter: null,
                        as_of: snapshot,
                    });
                    accepted(raw);
                    const total = z
                        .object({ total: z.number().int().nonnegative() })
                        .parse(raw).total;
                    const batch = z
                        .object({ [spec.key]: z.array(spec.schema).max(limit) })
                        .parse(raw)[spec.key] as Row[];
                    return { total, rows: batch };
                },
                (row) => String(row.id),
                {
                    code: 'incomplete_discovery',
                    message:
                        'Catalogue pages changed, were duplicated/incomplete or exceeded the 2,000-row bound. No complete search is claimed.',
                },
            );
            return rows.filter(own);
        };
        const base = {
            tool: name,
            request: args,
            tenantId: session.tenantId,
            partyId: session.partyId,
            source: 'OreStudio',
            retrievedAt: snapshot,
        };
        const readCounterpartyId = async (id: string) => {
            const r = await request('counterparties', {
                offset: 0,
                limit: 50,
                order: { field: '', descending: false },
                filter: { id_one_of: [id] },
                as_of: snapshot,
            });
            accepted(r);
            const list = z
                .object({
                    total: z.number().int().min(0).max(1),
                    counterparties: z.array(counterparty).max(1),
                })
                .parse(r);
            if (list.total !== list.counterparties.length)
                throw new AppError('incomplete_discovery', 'Counterparty read is incomplete.', 422);
            return (
                list.counterparties.find(
                    (c) => own(c) && c.id.toLowerCase() === id.toLowerCase(),
                ) ?? null
            );
        };
        if (name === 'find_counterparties') {
            const query = String(args.query);
            if (marketId.safeParse(query).success) {
                const row = await readCounterpartyId(query);
                return {
                    ...base,
                    status: row ? 'ok' : 'not_found',
                    rows: row ? [row] : [],
                    totalMatches: row ? 1 : 0,
                    offset: 0,
                    searchComplete: true,
                    catalogueScope: 'entitled_tenant',
                };
            }
            if (query && !/\s/.test(query)) {
                const raw = await request('counterparty', { key: { short_code: query } });
                const result = z.object({ result: z.object({ outcome: z.string() }) }).parse(raw);
                if (result.result.outcome === 'ok') {
                    const row = z
                        .object({ counterparty: counterparty.nullable() })
                        .parse(raw).counterparty;
                    if (row && own(row))
                        return {
                            ...base,
                            status: 'ok',
                            rows: [row],
                            totalMatches: 1,
                            offset: 0,
                            searchComplete: true,
                            catalogueScope: 'entitled_tenant',
                            matchedBy: 'exact_short_code',
                        };
                } else if (result.result.outcome !== 'missing') accepted(raw);
            }
            const raw = await request('counterparties', {
                offset: args.offset,
                limit: args.limit,
                order: { field: '', descending: false },
                filter: null,
                as_of: snapshot,
            });
            accepted(raw);
            const list = z
                .object({
                    total: z.number().int().nonnegative(),
                    counterparties: z.array(counterparty).max(25),
                })
                .parse(raw);
            if (
                list.counterparties.length !==
                Math.min(Number(args.limit), Math.max(0, list.total - Number(args.offset)))
            )
                throw new AppError('incomplete_discovery', 'Counterparty page is incomplete.', 422);
            const next = Number(args.offset) + list.counterparties.length;
            const rows = list.counterparties.filter((r) => own(r) && matches(r, query));
            return {
                ...base,
                status:
                    next < list.total || Number(args.offset) > 0
                        ? 'partial'
                        : rows.length
                          ? 'ok'
                          : 'not_found',
                rows,
                totalMatches: rows.length,
                offset: args.offset,
                searchComplete: Number(args.offset) === 0 && next >= list.total,
                catalogueScope: 'entitled_tenant_catalogue_page',
                totalVisible: list.total,
                nextOffset: next < list.total ? next : null,
            };
        }
        const resolve = async (resource: 'books' | 'portfolios', value: string, field: string) => {
            const rows = await scan(resource),
                exact = rows.filter(
                    (r) =>
                        String(r.id).toLowerCase() === value.toLowerCase() ||
                        String(r.name).toLowerCase() === value.toLowerCase(),
                );
            const choices = exact.length ? exact : rows.filter((r) => matches(r, value));
            if (choices.length !== 1)
                return {
                    result: {
                        ...base,
                        status: choices.length ? 'ambiguous' : 'not_found',
                        field,
                        candidates: choices.slice(0, 25).map((r) => ({ id: r.id, name: r.name })),
                        reason: 'Choose an exact selected-party name or ID; no entity was selected automatically.',
                    },
                };
            return { row: choices[0]! };
        };
        if (name === 'read_trade') {
            const response = await request('trade', { key: { id: args.tradeId } });
            if (missing(response))
                return {
                    ...base,
                    status: 'not_found',
                    reason: 'Trade is not available in the selected party.',
                };
            accepted(response);
            const anchor = z.object({ trade: trade.nullable() }).parse(response).trade;
            if (anchor && anchor.id.toLowerCase() !== String(args.tradeId).toLowerCase())
                throw new AppError('invalid_reply', 'Backend returned a different trade identity.');
            if (!anchor || !own(anchor))
                return {
                    ...base,
                    status: 'not_found',
                    reason: 'Trade is not available in the selected party.',
                };
            const bookingRaw = await request('tradeBooking', { key: { trade_id: anchor.id } });
            if (!missing(bookingRaw)) accepted(bookingRaw);
            const booking = missing(bookingRaw)
                ? null
                : z
                      .object({
                          trade_booking: z
                              .object({
                                  tenant_id: marketId,
                                  trade_id: marketId,
                                  book_id: marketId,
                                  trade_date: z.string().nullable(),
                                  execution_timestamp: z.string().nullable(),
                              })
                              .nullable(),
                      })
                      .parse(bookingRaw).trade_booking;
            const books = await scan('books');
            const bookedIn = booking ? books.find((b) => b.id === booking.book_id) : undefined;
            if (
                booking &&
                (booking.tenant_id !== session.tenantId ||
                    booking.trade_id !== anchor.id ||
                    !bookedIn)
            )
                throw new AppError(
                    'party_mismatch',
                    'Booking is outside the selected-party book catalogue.',
                    403,
                );
            const links: Row[] = [];
            let linkTotal: number | undefined;
            for (let offset = 0; offset < 1000; offset += 50) {
                const r = await request('tradePortfolios', {
                    trade_id: anchor.id,
                    scope: 'direct',
                    offset,
                    limit: 50,
                    order: { field: '', descending: false },
                    filter: null,
                });
                accepted(r);
                const list = z
                    .object({
                        total: z.number().int().nonnegative(),
                        trade_portfolios: z
                            .array(
                                z.object({
                                    tenant_id: marketId,
                                    trade_id: marketId,
                                    sequence_number: z.number().int(),
                                    portfolio_id: marketId,
                                }),
                            )
                            .max(50),
                    })
                    .parse(r);
                if (
                    (linkTotal !== undefined && linkTotal !== list.total) ||
                    list.trade_portfolios.length !== Math.min(50, Math.max(0, list.total - offset))
                )
                    throw new AppError(
                        'incomplete_discovery',
                        'Portfolio links changed during reading.',
                        422,
                    );
                linkTotal = list.total;
                links.push(...list.trade_portfolios);
                if (links.length >= list.total) break;
            }
            if (
                links.length !== linkTotal ||
                links.some((l) => l.tenant_id !== session.tenantId || l.trade_id !== anchor.id) ||
                new Set(links.map((l) => l.sequence_number)).size !== links.length
            )
                throw new AppError(
                    'incomplete_discovery',
                    'Portfolio link scope or completeness could not be verified.',
                    422,
                );
            const portfolios = await scan('portfolios');
            if (links.some((l) => !portfolios.some((p) => p.id === l.portfolio_id)))
                throw new AppError(
                    'party_mismatch',
                    'Linked portfolio is outside the selected party.',
                    403,
                );
            const cp = anchor.counterparty_id
                ? await readCounterpartyId(anchor.counterparty_id)
                : null;
            const hierarchy: Row[] = [],
                visited = new Set<string>();
            let parent = bookedIn ? String(bookedIn.parent_portfolio_id) : null;
            while (parent && parent !== '00000000-0000-0000-0000-000000000000') {
                const node = portfolios.find((p) => p.id === parent);
                if (!node || visited.has(parent) || hierarchy.length >= 50)
                    throw new AppError(
                        'incomplete_discovery',
                        'Book portfolio hierarchy cannot be verified.',
                        422,
                    );
                visited.add(parent);
                hierarchy.push(node);
                parent = node.parent_portfolio_id ? String(node.parent_portfolio_id) : null;
            }
            let economics: unknown = null,
                economicsStatus = 'unsupported_product';
            if (['FxForward', 'Bond'].includes(anchor.trade_type)) {
                const op = anchor.trade_type === 'FxForward' ? 'fxForward' : 'bond',
                    key = op === 'fxForward' ? 'fx_forward_instrument' : 'bond_instrument';
                const rawInstrument = await request(op, { key: { trade_id: anchor.id } });
                if (!missing(rawInstrument)) accepted(rawInstrument);
                const amount = z
                    .string()
                    .regex(/^[+]?(?:\d+(?:\.\d*)?|\.\d+)$/)
                    .refine((v) => Number.isFinite(Number(v)) && Number(v) > 0);
                const fields =
                    op === 'fxForward'
                        ? {
                              bought_currency: z.string().regex(/^[A-Z]{3}$/),
                              bought_amount: amount,
                              sold_currency: z.string().regex(/^[A-Z]{3}$/),
                              sold_amount: amount,
                              value_date: z.iso.date(),
                              settlement: z.string(),
                          }
                        : { issue_id: marketId, notional: amount.nullable() };
                const instrument = missing(rawInstrument)
                    ? null
                    : z
                          .object({
                              [key]: z
                                  .object({
                                      identity: z.object({
                                          tenant_id: marketId,
                                          party_id: marketId,
                                          trade_id: marketId,
                                          trade_type_code: z.string(),
                                      }),
                                      ...fields,
                                  })
                                  .nullable(),
                          })
                          .parse(rawInstrument)[key];
                if (!instrument) economicsStatus = 'missing';
                else {
                    if (
                        instrument.identity.tenant_id !== session.tenantId ||
                        instrument.identity.party_id !== session.partyId ||
                        instrument.identity.trade_id !== anchor.id ||
                        instrument.identity.trade_type_code !== anchor.trade_type
                    )
                        throw new AppError('party_mismatch', 'Instrument identity mismatch.', 403);
                    economics = instrument;
                    economicsStatus = op === 'fxForward' ? 'available' : 'issue_reference_only';
                }
            }
            const reread = await request('trade', { key: { id: anchor.id } });
            accepted(reread);
            if (
                JSON.stringify(z.object({ trade: trade }).parse(reread).trade) !==
                JSON.stringify(anchor)
            )
                throw new AppError(
                    'incomplete_discovery',
                    'Trade changed during reading; retry.',
                    422,
                );
            return {
                ...base,
                status: 'ok',
                trade: anchor,
                booking,
                book: bookedIn ?? null,
                directPortfolios: portfolios.filter((p) =>
                    links.some((l) => l.portfolio_id === p.id),
                ),
                bookingPortfolioHierarchy: hierarchy,
                counterparty: cp,
                economicsStatus,
                economics,
                note: 'Direct portfolio links and booking hierarchy are separate. No valuation or booking action performed.',
            };
        }
        let rows: Row[];
        if (name === 'find_trades' && (args.book || args.portfolio)) {
            if (args.book && args.portfolio)
                throw new AppError('invalid_scope', 'Specify one book or one portfolio.', 400);
            const field = args.book ? 'book' : 'portfolio';
            const selected = await resolve(
                args.book ? 'books' : 'portfolios',
                String(args[field]),
                field,
            );
            if (selected.result) return selected.result;
            rows = [];
            const seen = new Set<string>();
            let complete = false;
            for (let offset = 0; offset < 1000; offset += 50) {
                const rawExport = await request('exportPortfolio', {
                    node_id: selected.row!.id,
                    offset,
                    limit: 50,
                });
                const batch = z
                    .object({
                        success: z.boolean(),
                        items: z.array(z.object({ anchor: trade })).max(50),
                    })
                    .parse(rawExport);
                if (!batch.success)
                    throw new AppError(
                        'discovery_refused',
                        'Backend refused membership export.',
                        403,
                    );
                for (const item of batch.items) {
                    if (!own(item.anchor) || seen.has(item.anchor.id))
                        throw new AppError(
                            'incomplete_discovery',
                            'Export scope or order was inconsistent.',
                            422,
                        );
                    seen.add(item.anchor.id);
                    rows.push(item.anchor);
                }
                if (batch.items.length < 50) {
                    complete = true;
                    break;
                }
            }
            if (!complete)
                throw new AppError(
                    'incomplete_discovery',
                    'Membership export exceeds the bounded search limit.',
                    422,
                );
        } else
            rows = await scan(
                name === 'find_trades'
                    ? 'trades'
                    : name === 'find_books'
                      ? 'books'
                      : name === 'find_portfolios'
                        ? 'portfolios'
                        : 'counterparties',
            );
        rows = rows.filter(
            (r) =>
                matches(r, String(args.query)) &&
                (!args.tradeType ||
                    String(r.trade_type).toLowerCase() === String(args.tradeType).toLowerCase()),
        );
        const offset = Number(args.offset),
            limit = Number(args.limit);
        return {
            ...base,
            status: rows.length ? 'ok' : 'not_found',
            totalMatches: rows.length,
            rows: rows.slice(offset, offset + limit),
            offset,
            limit,
            searchComplete: true,
            catalogueScope: name === 'find_counterparties' ? 'entitled_tenant' : 'selected_party',
        };
    }
}
export function formatDiscovery(raw: unknown): string | undefined {
    if (
        !raw ||
        typeof raw !== 'object' ||
        !Object.hasOwn(discoveryTools, (raw as { tool: string }).tool)
    )
        return;
    const r = raw as Record<string, any>;
    if (r.status === 'ambiguous')
        return `${r.reason}\n${r.candidates.map((c: any) => `${c.name} [${c.id}]`).join('\n')}\nReply with the exact name or ID.`;
    if (r.status === 'not_found') return r.reason ?? 'No matching records in the requested scope.';
    if (r.tool === 'read_trade') {
        const e = r.economics;
        const economics =
            r.economicsStatus === 'available'
                ? `Bought: ${e.bought_amount} ${e.bought_currency}\nSold: ${e.sold_amount} ${e.sold_currency}\nValue date: ${e.value_date}\nSettlement: ${e.settlement}`
                : r.economicsStatus === 'issue_reference_only'
                  ? `Bond issue reference: ${e.issue_id}\nNotional: ${e.notional ?? 'not supplied'}\nFull bond issue economics are not loaded.`
                  : `Economics: ${r.economicsStatus}; no terms have been inferred.`;
        return `Trade ${r.trade.id} — ${r.trade.trade_type}\nBook: ${r.book?.name ?? 'no booking found'}\nCounterparty: ${r.counterparty ? `${r.counterparty.full_name} (${r.counterparty.short_code})` : 'not available'}\nTrade date: ${r.booking?.trade_date ?? 'not supplied'}\nBooking portfolio hierarchy: ${r.bookingPortfolioHierarchy.map((p: any) => p.name).join(' → ') || 'none'}\nDirect portfolio assignments: ${r.directPortfolios.map((p: any) => p.name).join(', ') || 'none'}\n${economics}\nVersion: ${r.trade.version}. Source: OreStudio, retrieved ${r.retrievedAt}.\nNo valuation or booking action performed.`;
    }
    return `${r.totalMatches} matching records (${r.catalogueScope}); showing ${r.rows.length} from offset ${r.offset}.\n\n${markdownTable(
        ['Name / type', 'ID'],
        r.rows.map((row: any) => [
            row.name ??
                (row.full_name
                    ? `${row.full_name} (${row.short_code})`
                    : (row.trade_type ?? row.origin_reference ?? '')),
            row.id,
        ]),
    )}\n${r.status === 'partial' ? `\nPartial catalogue search; no global absence or uniqueness is claimed. Next catalogue offset: ${r.nextOffset ?? 'none (earlier pages not included)'}.` : ''}`;
}

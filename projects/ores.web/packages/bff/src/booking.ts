/**
 * Booking a trade.
 *
 * The write is the trading service's operation, so this states a request and
 * reads the answer: the service decides whether the book is open, whether the
 * trade id is free and whether the instrument belongs to the trade type. The
 * browser states what only a person knows — the book, the counterparty, the
 * date, the security, the notional and the trade's identifier — and the server
 * states the rest, including the party the trade belongs to.
 */
import { randomUUID } from 'node:crypto';
import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import { subjects as identifierSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_identifier_protocol';
import { subjects as instrumentSubjects } from '@ores/wire-protocol/generated/trading/protocol/bond_instrument_protocol';
import { subjects as operationsSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_operations_protocol';
import { subjects as bookSubjects } from '@ores/wire-protocol/generated/refdata/protocol/book_protocol';
import { subjects as idTypeSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_id_type_protocol';
import { subjects as linkSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_link_protocol';
import { subjects as linkTypeSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_link_type_protocol';
import { subjects as stateSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_state_protocol';
import { subjects as bondIssueSubjects } from '@ores/wire-protocol/generated/trading/protocol/bond_issue_protocol';
import { invalidRequest, notPermitted } from './errors.js';
import { HttpFailure } from './errors.js';
import { input, readAll, resultSchema } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/** The one transition that books a live trade. */
const newBooking = 'new_booking';

/** Why a booked trade exists, as the trade's own change reason. */
const bookingReason = 'system.new_record';

/** The system tenant: the service stamps the caller's own over it. */
const statedByTheService = 'ffffffff-ffff-ffff-ffff-ffffffffffff';

/** The link a person records between two trades. */
const linkBodySchema = z.object({
    linkType: z.string().min(1).max(80),
    otherTradeId: z.uuid(),
});

const linkParamsSchema = z.object({ id: z.uuid() });

const bookingBodySchema = z.object({
    bookId: z.uuid(),
    counterpartyId: z.uuid(),
    tradeDate: z.iso.date(),
    issueId: z.uuid(),
    notional: z.string().regex(/^\d+(?:\.\d+)?$/, 'The notional is a plain decimal.'),
    identifierType: z.string().min(1).max(40),
    identifierValue: z.string().min(1).max(80),
});

/**
 * The empty instrument batch.
 *
 * Every array the protocol declares is stated, even when it is empty: the
 * decoder refuses a request that leaves a declared member out, so an omission
 * here is a booking the server answers with "bad_request" and no reason. The
 * list is the one in `ores.trading.api/domain/instrument_batch.hpp`.
 */
function emptyBatch(): Record<string, readonly unknown[]> {
    return {
        balance_guaranteed_swap_instruments: [],
        balance_guaranteed_swap_tranche_notionals: [],
        balance_guaranteed_swap_tranches: [],
        // Every array the batch declares must be named, even empty: the service
        // refuses a request that omits one, and a family added to the batch
        // without a line here breaks every booking with a decode error.
        equity_basket_constituents: [],
        equity_swap_periods: [],
        bond_instruments: [],
        callable_swap_call_dates: [],
        callable_swap_instruments: [],
        cap_floor_instruments: [],
        commodity_basket_constituents: [],
        commodity_instruments: [],
        composite_instruments: [],
        composite_legs: [],
        credit_basket_constituents: [],
        credit_default_swap_instruments: [],
        credit_instruments: [],
        credit_linked_swap_instruments: [],
        equity_accumulator_instruments: [],
        equity_asian_option_instruments: [],
        equity_barrier_option_instruments: [],
        equity_digital_option_instruments: [],
        equity_forward_instruments: [],
        equity_option_instruments: [],
        equity_position_instruments: [],
        equity_position_option_underlyings: [],
        equity_swap_instruments: [],
        equity_variance_swap_instruments: [],
        flexi_swap_instruments: [],
        flexi_swap_lower_notionals: [],
        fra_instruments: [],
        fx_accumulator_instruments: [],
        fx_asian_forward_instruments: [],
        fx_barrier_option_instruments: [],
        fx_digital_option_instruments: [],
        fx_forward_instruments: [],
        fx_vanilla_option_instruments: [],
        fx_variance_swap_instruments: [],
        index_credit_default_swap_instruments: [],
        index_credit_default_swap_option_instruments: [],
        inflation_swap_instruments: [],
        instrument_option_exercise_fees: [],
        instrument_option_exercise_prices: [],
        instrument_option_payment_dates: [],
        instrument_option_premiums: [],
        instrument_options: [],
        instrument_schedule_dates: [],
        instrument_schedules: [],
        instrument_strikes: [],
        knock_out_swap_instruments: [],
        range_bounds: [],
        rate_instruments: [],
        risk_participation_agreement_instruments: [],
        scripted_instruments: [],
        swap_leg_amounts: [],
        swap_leg_rates: [],
        swap_legs: [],
        swaption_instruments: [],
        synthetic_cdo_instruments: [],
        vanilla_swap_instruments: [],
    };
} /** The intent every write of a booking carries. */
function intent(): { readonly reason_code: string; readonly commentary: string } {
    return { reason_code: bookingReason, commentary: '' };
}

/** The precondition that refuses a write over a row that already exists. */
function mustNotExist(): { readonly kind: string; readonly version: null } {
    return { kind: 'must_not_exist', version: null };
}

/**
 * Why a refused write was refused, in the status the screen acts on.
 *
 * A booking the service declines is the caller's business: a frozen book, a
 * trade id already taken, an identifier the tenant does not know. Only a fault
 * in the asking is the server's, so only that answers 502.
 */
function refusalOfBooking(result: z.infer<typeof resultSchema>): HttpFailure {
    switch (result.outcome) {
        case 'denied':
            return notPermitted(result.message);
        case 'conflict':
            return new HttpFailure(409, { code: 'conflict', message: result.message });
        case 'unavailable':
            return new HttpFailure(502, { code: 'upstream-unavailable', message: result.message });
        default:
            return invalidRequest(result.message);
    }
}

export function registerBookingRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /**
     * What the booking form offers: the securities a bond trade can name, and
     * the identifier schemes the tenant knows.
     *
     * The books and the counterparties come from the routes that already serve
     * them, so the form has one picker per concept and no second copy.
     */
    server.get('/api/trades/new', async (request) => {
        const session = requireSession(request);
        const partyId = session.party?.id;
        /*
         * Only the acting party's open books.
         *
         * A booking is stamped with the party the session acts for, and the
         * service refuses a book of any other party. A session whose reach is
         * wider than its party — a tenant administrator's, or one switched to a
         * sibling entity — sees other parties' books in a plain tree read, so
         * the filter belongs here, where the acting party is known, and not in
         * the screen.
         */
        const books =
            partyId === undefined
                ? []
                : (
                      await readAll(session, bookSubjects.list_books_request, 'books', {
                          filter: null,
                          as_of: null,
                      })
                  )
                      .filter(
                          (row) => row['party_id'] === partyId && row['book_status'] === 'Active',
                      )
                      .map((row) => ({
                          id: String(row['id'] ?? ''),
                          name: String(row['name'] ?? ''),
                      }));
        const issues = await readAll(
            session,
            bondIssueSubjects.list_bond_issues_request,
            'issues',
            { filter: null, as_of: null },
        );
        /*
         * The identifier schemes are the tenant's own list, copied from the
         * system tenant when the tenant was provisioned. A tenant provisioned
         * before that copy existed has none, so the schemes its trades already
         * use stand in: that is worse than the list, and better than an empty
         * picker that cannot book at all.
         */
        const declared = await readAll(
            session,
            idTypeSubjects.list_trade_id_types_request,
            'id_types',
            { filter: null, as_of: null },
        );
        let schemes: readonly { readonly code: string }[];
        if (declared.length > 0) {
            schemes = declared
                .map((row) => String(row['code'] ?? ''))
                .filter((code) => code !== '')
                // Sorted like the fallback below, so a picker reads the same
                // whichever path a tenant takes.
                .sort()
                .map((code) => ({ code }));
        } else {
            const used = await readAll(
                session,
                identifierSubjects.list_trade_identifiers_request,
                'identifiers',
                { filter: null, as_of: null },
            );
            const codes = new Set<string>();
            for (const row of used) {
                const code = String(row['id_type'] ?? '');
                if (code !== '') codes.add(code);
            }
            schemes = [...codes].sort().map((code) => ({ code }));
        }

        return { books, issues, idTypes: schemes };
    });

    /** The link types a person can record, each with the words that describe it. */
    server.get('/api/trade-link-types', async (request) => {
        const session = requireSession(request);
        const rows = await readAll(
            session,
            linkTypeSubjects.list_trade_link_types_request,
            'link_types',
            { filter: null, as_of: null },
        );
        return {
            rows: rows.map((row) => ({
                code: String(row['code'] ?? ''),
                description: String(row['description'] ?? ''),
            })),
        };
    });

    /**
     * Records a link from one trade to another.
     *
     * A link is made by an activity, and the activity is the one that last
     * moved the trade: the state names it, so the client does not have to know
     * which write came last. The link type is the caller's, because whether a
     * relationship is a hedge or a close-out is a fact about the structure.
     */
    server.post('/api/trades/:id/links', async (request) => {
        const session = requireSession(request);
        const parsed = linkParamsSchema.safeParse(request.params);
        if (!parsed.success) {
            throw invalidRequest('The address does not name a trade.');
        }
        const body = input(linkBodySchema, request.body);
        const id = parsed.data.id;
        if (id === body.otherTradeId) {
            throw invalidRequest('A trade cannot be linked to itself.');
        }
        const states = await readAll(
            session,
            stateSubjects.list_trade_states_request,
            'trade_states',
            {
                filter: { trade_id_one_of: [id] },
                order: { field: 'version', descending: true },
            },
        );
        /*
         * The newest state names the activity, and the newest is decided here
         * rather than trusted to the reply's order: a read that ignores the
         * order would otherwise record the link against an older activity, and
         * nothing would say so.
         */
        const newest = states.reduce<Record<string, unknown> | undefined>((best, row) => {
            if (best === undefined) return row;
            return Number(row['version'] ?? 0) > Number(best['version'] ?? 0) ? row : best;
        }, undefined);
        const activityId = String(newest?.['trade_activity_id'] ?? '');
        if (activityId === '') {
            throw invalidRequest('The trade has no activity to record the link against.');
        }
        const written = await session.client.callAuthenticated(
            linkSubjects.put_trade_link_request,
            {
                change: {
                    write: {
                        from_trade_id: id,
                        to_trade_id: body.otherTradeId,
                        link_type: body.linkType,
                        trade_activity_id: activityId,
                    },
                    precondition: mustNotExist(),
                },
                intent: intent(),
            },
            z.looseObject({ result: resultSchema }),
        );
        if (written.result.outcome !== 'ok') {
            throw refusalOfBooking(written.result);
        }
        return { from: id, to: body.otherTradeId, linkType: body.linkType };
    });

    server.post('/api/trades', async (request, reply) => {
        const session = requireSession(request);
        const body = input(bookingBodySchema, request.body);
        const partyId = session.party?.id;
        if (partyId === undefined) {
            throw invalidRequest('A booking is made by a party, and this session works for none.');
        }

        const tradeId = randomUUID();
        const now = new Date().toISOString();
        const acknowledgement = await session.client.callAuthenticated(
            operationsSubjects.book_trade_request,
            {
                anchor: {
                    version: 0,
                    tenant_id: statedByTheService,
                    id: tradeId,
                    party_id: partyId,
                    counterparty_id: body.counterpartyId,
                    trade_type: 'Bond',
                    counterparty_scope: 'external',
                    booking_nature: 'actual',
                    entry_channel: 'manual',
                    origin: 'api',
                    origin_reference: '',
                    external_version: 0,
                    economic_digest: '',
                    modified_by: '',
                    performed_by: '',
                    change_reason_code: bookingReason,
                    change_commentary: '',
                    recorded_at: now,
                },
                booking: {
                    version: 0,
                    tenant_id: statedByTheService,
                    trade_id: tradeId,
                    trade_activity_id: statedByTheService,
                    book_id: body.bookId,
                    netting_set_id: null,
                    counterparty_identifier_id: null,
                    netting_set_identifier_id: null,
                    trade_date: body.tradeDate,
                    execution_timestamp: now,
                    modified_by: '',
                    performed_by: '',
                    change_reason_code: bookingReason,
                    change_commentary: '',
                    recorded_at: now,
                },
                activity_type_code: newBooking,
                identifiers: [],
                instruments: emptyBatch(),
            },
            z.looseObject({
                result: resultSchema,
                activity_id: z.uuid().nullable().default(null),
            }),
        );
        if (acknowledgement.result.outcome !== 'ok') {
            throw refusalOfBooking(acknowledgement.result);
        }
        const activityId = acknowledgement.activity_id;
        if (activityId === null) {
            throw new Error('The booking answered without the activity that made it.');
        }

        const identified = await session.client.callAuthenticated(
            identifierSubjects.put_trade_identifier_request,
            {
                change: {
                    write: {
                        trade_id: tradeId,
                        id_type: body.identifierType,
                        trade_activity_id: activityId,
                        id_value: body.identifierValue,
                        issuing_party_id: null,
                    },
                    precondition: mustNotExist(),
                },
                intent: intent(),
            },
            z.looseObject({ result: resultSchema }),
        );
        if (identified.result.outcome !== 'ok') {
            throw refusalOfBooking(identified.result);
        }

        const instrumented = await session.client.callAuthenticated(
            instrumentSubjects.put_bond_instrument_request,
            {
                change: {
                    write: {
                        trade_id: tradeId,
                        trade_type_code: 'Bond',
                        trade_activity_id: activityId,
                        issue_id: body.issueId,
                        notional: body.notional,
                    },
                    precondition: mustNotExist(),
                },
                intent: intent(),
            },
            z.looseObject({ result: resultSchema }),
        );
        if (instrumented.result.outcome !== 'ok') {
            throw refusalOfBooking(instrumented.result);
        }

        reply.code(201);
        return { id: tradeId };
    });
}

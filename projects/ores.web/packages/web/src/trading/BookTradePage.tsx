import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link, useNavigate, useSearchParams } from 'react-router';
import { counterparties } from '../api/counterparties.js';
import { structures } from '../api/structures.js';
import { trades, type BookingRequest } from '../api/trades.js';
import { useTranslation } from '../i18n/Provider.js';
import { Crumbs } from '../refdata/shared.js';
import { PATHS } from '../shell/paths.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { Button, Field, Input, Notice, Select } from '../ui/Primitives.js';

/** Today, as a date control states it. */
function today(): string {
    return new Date().toISOString().slice(0, 10);
}

/**
 * Book a trade.
 *
 * The form states only what a person decides: where the trade is booked, who it
 * is with, when, what security and how much, and the identifier the desk knows
 * it by. Everything else — the party, the trade's identity, the activity that
 * makes it live — is the server's. So a refusal arrives with the server's own
 * reason, and a frozen book reads as a frozen book.
 */
export function BookTradePage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const [params] = useSearchParams();
    /*
     * A booking made from a deal names it: the trade it makes is a leg of that
     * deal before the person leaves the form. Booking from the blotter names no
     * deal and the trade stands alone.
     */
    const structureKey = params.get('structure') ?? '';
    const legRole = params.get('role') ?? '';
    const trail = [
        { label: t('shell.menu.home'), to: '/' },
        { label: t('shell.menu.trading'), to: PATHS.trading },
        { label: t('trading.trades.title'), to: PATHS.trades },
        { label: t('trading.booking.title') },
    ];

    const offered = useQuery({
        queryKey: ['trades', 'booking-options'],
        queryFn: () => trades.bookingOptions(),
    });
    const known = useQuery({
        queryKey: ['counterparties', 'pick'],
        queryFn: () => counterparties.page({ offset: 0, limit: 200, search: '', status: 'active' }),
    });
    const deal = useQuery({
        queryKey: ['structure', structureKey],
        queryFn: () => structures.one(structureKey),
        enabled: structureKey !== '',
    });

    const [bookId, setBookId] = useState('');
    const [counterpartyId, setCounterpartyId] = useState('');
    const [tradeDate, setTradeDate] = useState(today());
    const [issueId, setIssueId] = useState('');
    const [notional, setNotional] = useState('');
    const [identifierType, setIdentifierType] = useState('');
    const [identifierValue, setIdentifierValue] = useState('');

    const [booked, setBooked] = useState('');
    const booking = useMutation({
        mutationFn: async (request: BookingRequest) => {
            const id = await trades.book(request);
            setBooked(id);
            if (structureKey !== '') {
                await structures.addLeg(structureKey, id, legRole === '' ? 'leg' : legRole);
            }
            return id;
        },
        onSuccess: async (id) => {
            if (structureKey === '') {
                void navigate(`${PATHS.trades}/${id}`);
                return;
            }
            await queries.invalidateQueries({ queryKey: ['structure'] });
            await queries.invalidateQueries({ queryKey: ['structures'] });
            void navigate(`${PATHS.structures}/${encodeURIComponent(structureKey)}`);
        },
    });

    const openBooks = offered.data?.books ?? [];
    const issues = offered.data?.issues ?? [];
    const idTypes = offered.data?.idTypes ?? [];
    /*
     * Every leg of a deal is booked with the deal's counterparty, so the deal
     * states it and the form does not offer another.
     */
    const dealCounterparty = structureKey === '' ? '' : (deal.data?.counterparty_id ?? '');
    const statedCounterparty = dealCounterparty === '' ? counterpartyId : dealCounterparty;
    const ready =
        bookId !== '' &&
        statedCounterparty !== '' &&
        issueId !== '' &&
        notional !== '' &&
        identifierType !== '' &&
        identifierValue !== '';

    return (
        <ScreenFrame
            trail={<Crumbs parts={trail} />}
            title={t('trading.booking.title')}
            description={t('trading.booking.lead')}
        >
            <div className="max-w-3xl space-y-5">
                {structureKey === '' ? null : (
                    <Notice tone="info">
                        {t('trading.booking.legNotice', {
                            reference:
                                deal.data === undefined
                                    ? structureKey
                                    : deal.data.reference === ''
                                      ? deal.data.id
                                      : deal.data.reference,
                            role: legRole === '' ? 'leg' : legRole,
                        })}
                    </Notice>
                )}
                {booking.isError && <Notice tone="error">{booking.error.message}</Notice>}
                {booking.isError && booked !== '' && (
                    <Notice tone="info">
                        {t('trading.booking.legFailed')}{' '}
                        <Link
                            className="text-accent hover:text-accent-bright"
                            to={`${PATHS.trades}/${booked}`}
                        >
                            {t('trading.booking.legFailedOpen')}
                        </Link>
                    </Notice>
                )}
                <form
                    className="space-y-5"
                    onSubmit={(event) => {
                        event.preventDefault();
                        booking.mutate({
                            bookId,
                            counterpartyId: statedCounterparty,
                            tradeDate,
                            issueId,
                            notional,
                            identifierType,
                            identifierValue,
                        });
                    }}
                >
                    <section className="space-y-4">
                        <h2 className="text-sm font-medium text-ink">
                            {t('trading.booking.tradeSection')}
                        </h2>
                        <div className="grid gap-4 md:grid-cols-2">
                            <Field label={t('trading.booking.book')}>
                                <Select
                                    value={bookId}
                                    onChange={(event) => setBookId(event.target.value)}
                                >
                                    <option value="">{t('trading.booking.choose')}</option>
                                    {openBooks.map((row) => (
                                        <option key={row.id} value={row.id}>
                                            {row.name}
                                        </option>
                                    ))}
                                </Select>
                            </Field>
                            <Field
                                label={t('trading.booking.counterparty')}
                                hint={
                                    dealCounterparty === ''
                                        ? t('trading.booking.counterpartyHint')
                                        : t('trading.booking.legCounterpartyHint')
                                }
                            >
                                <Select
                                    value={statedCounterparty}
                                    disabled={dealCounterparty !== ''}
                                    onChange={(event) => setCounterpartyId(event.target.value)}
                                >
                                    <option value="">{t('trading.booking.choose')}</option>
                                    {/* The deal's own counterparty may sit outside
                                        the page a picker reads, so it is stated even
                                        when the list does not hold it. */}
                                    {deal.data !== undefined &&
                                        dealCounterparty !== '' &&
                                        !(known.data?.rows ?? []).some(
                                            (row) => row.id === dealCounterparty,
                                        ) && (
                                            <option value={dealCounterparty}>
                                                {deal.data.counterparty_name}
                                            </option>
                                        )}
                                    {(known.data?.rows ?? []).map((row) => (
                                        <option key={row.id} value={row.id}>
                                            {row.full_name}
                                        </option>
                                    ))}
                                </Select>
                            </Field>
                            <Field label={t('trading.booking.tradeDate')}>
                                <Input
                                    type="date"
                                    value={tradeDate}
                                    onChange={(event) => setTradeDate(event.target.value)}
                                />
                            </Field>
                        </div>
                    </section>
                    <section className="space-y-4">
                        <h2 className="text-sm font-medium text-ink">
                            {t('trading.booking.instrumentSection', { type: 'Bond' })}
                        </h2>
                        <p className="text-xs text-ink-faint">
                            {t('trading.booking.instrumentLead')}
                        </p>
                        <div className="grid gap-4 md:grid-cols-2">
                            <Field label={t('trading.booking.security')} className="md:col-span-2">
                                <Select
                                    value={issueId}
                                    onChange={(event) => setIssueId(event.target.value)}
                                >
                                    <option value="">{t('trading.booking.choose')}</option>
                                    {issues.map((issue) => (
                                        <option key={issue.issue_id} value={issue.issue_id}>
                                            {issue.calendar === null
                                                ? issue.security_id
                                                : `${issue.security_id} · ${issue.calendar}`}
                                        </option>
                                    ))}
                                </Select>
                            </Field>
                            <Field
                                label={t('trading.booking.notional')}
                                hint={t('trading.booking.notionalHint')}
                            >
                                <Input
                                    inputMode="decimal"
                                    value={notional}
                                    placeholder="0.00"
                                    onChange={(event) => setNotional(event.target.value)}
                                />
                            </Field>
                        </div>
                    </section>
                    <section className="space-y-4">
                        <h2 className="text-sm font-medium text-ink">
                            {t('trading.booking.identifierSection')}
                        </h2>
                        <div className="grid gap-4 md:grid-cols-2">
                            <Field label={t('trading.booking.identifierType')}>
                                <Select
                                    value={identifierType}
                                    onChange={(event) => setIdentifierType(event.target.value)}
                                >
                                    <option value="">{t('trading.booking.choose')}</option>
                                    {idTypes.map((row) => (
                                        <option key={row.code} value={row.code}>
                                            {row.code}
                                        </option>
                                    ))}
                                </Select>
                            </Field>
                            <Field
                                label={t('trading.booking.identifier')}
                                hint={t('trading.booking.identifierHint')}
                            >
                                <Input
                                    value={identifierValue}
                                    onChange={(event) => setIdentifierValue(event.target.value)}
                                />
                            </Field>
                        </div>
                    </section>
                    <div className="flex items-center gap-3">
                        <Button
                            type="submit"
                            variant="primary"
                            pending={booking.isPending}
                            pendingLabel={t('trading.booking.pending')}
                            disabled={!ready}
                        >
                            {t('trading.booking.submit')}
                        </Button>
                        <Link className="text-sm text-ink-muted hover:text-ink" to={PATHS.trades}>
                            {t('trading.booking.cancel')}
                        </Link>
                    </div>
                </form>
            </div>
        </ScreenFrame>
    );
}

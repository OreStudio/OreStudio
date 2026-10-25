import { useMutation, useQuery } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link, useNavigate } from 'react-router';
import { books } from '../api/books.js';
import { counterparties } from '../api/counterparties.js';
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
    const tree = useQuery({ queryKey: ['books', 'tree'], queryFn: () => books.tree() });
    const known = useQuery({
        queryKey: ['counterparties', 'pick'],
        queryFn: () => counterparties.page({ offset: 0, limit: 200, search: '', status: 'active' }),
    });

    const [bookId, setBookId] = useState('');
    const [counterpartyId, setCounterpartyId] = useState('');
    const [tradeDate, setTradeDate] = useState(today());
    const [issueId, setIssueId] = useState('');
    const [notional, setNotional] = useState('');
    const [identifierType, setIdentifierType] = useState('');
    const [identifierValue, setIdentifierValue] = useState('');

    const booking = useMutation({
        mutationFn: (request: BookingRequest) => trades.book(request),
        onSuccess: (id) => {
            void navigate(`${PATHS.trades}/${id}`);
        },
    });

    const openBooks = (tree.data?.books ?? []).filter((row) => row.book_status === 'Active');
    const issues = offered.data?.issues ?? [];
    const idTypes = offered.data?.idTypes ?? [];
    const ready =
        bookId !== '' &&
        counterpartyId !== '' &&
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
                {booking.isError && <Notice tone="error">{booking.error.message}</Notice>}
                <form
                    className="space-y-5"
                    onSubmit={(event) => {
                        event.preventDefault();
                        booking.mutate({
                            bookId,
                            counterpartyId,
                            tradeDate,
                            issueId,
                            notional,
                            identifierType,
                            identifierValue,
                        });
                    }}
                >
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
                            hint={t('trading.booking.counterpartyHint')}
                        >
                            <Select
                                value={counterpartyId}
                                onChange={(event) => setCounterpartyId(event.target.value)}
                            >
                                <option value="">{t('trading.booking.choose')}</option>
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
                        <Field label={t('trading.booking.notional')}>
                            <Input
                                inputMode="decimal"
                                value={notional}
                                placeholder="0.00"
                                onChange={(event) => setNotional(event.target.value)}
                            />
                        </Field>
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

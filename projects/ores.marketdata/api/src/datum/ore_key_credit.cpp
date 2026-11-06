/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
 */
#include "ore_key_reading.hpp"
#include <cctype>
#include <stdexcept>
#include <string>

/**
 * @file ore_key_credit.cpp
 * @brief The credit instruments: CDS, hazard and recovery rates, the CDS index
 * base correlation, index CDS tranches and options. ORE's parseMarketDatum,
 * lines 427 to 523 and 764 to 840.
 */

namespace ores::marketdata::datum::detail {

namespace {

using f = field;
using it = instrument_type;

market_datum read_cds(quote_type q, tokens rest) {
    // CDS/qt/name/ccy                                   (built from reference data)
    // CDS/qt/name/seniority/ccy/term
    // CDS/qt/name/seniority/ccy/doc/term/runningSpread
    // CDS/qt/name/seniority/ccy/doc/term or .../term/runningSpread: ORE tells
    // these two apart by whether the sixth token is a documentation clause.
    require_size(rest, {2, 4, 5, 6});
    datum_builder b(it::cds, q);
    b.set(f::underlying_name, text(rest[0]));
    if (rest.size() == 2)
        return b.set(f::ccy, text(rest[1])).build();
    b.set(f::seniority, text(rest[1])).set(f::ccy, text(rest[2]));
    if (rest.size() == 4)
        return b.set(f::term, period(rest[3])).build();
    if (rest.size() == 6)
        return b.set(f::doc_clause, text(rest[3]))
            .set(f::term, period(rest[4]))
            .set(f::running_spread, number(rest[5]))
            .build();
    if (is_doc_clause(rest[3]))
        return b.set(f::doc_clause, text(rest[3])).set(f::term, period(rest[4])).build();
    return b.set(f::term, period(rest[3])).set(f::running_spread, number(rest[4])).build();
}

market_datum read_hazard_rate(quote_type q, tokens rest) {
    // HAZARD_RATE/RATE/name/seniority/ccy[/doc]/term. ORE reads any quote
    // token here and records RATE, so another token could not be written back.
    require_quote(q, {quote_type::rate});
    require_size(rest, {4, 5});
    datum_builder b(it::hazard_rate, q);
    b.set(f::underlying_name, text(rest[0]))
        .set(f::seniority, text(rest[1]))
        .set(f::ccy, text(rest[2]));
    if (rest.size() == 5)
        b.set(f::doc_clause, text(rest[3]));
    return b.set(f::term, period(rest.back())).build();
}

market_datum read_recovery_rate(instrument_type t, quote_type q, tokens rest) {
    // RECOVERY_RATE|ASSUMED_RECOVERY_RATE/RATE/name[/seniority/ccy[/doc]]. ORE
    // reads any quote token here and records RATE.
    require_quote(q, {quote_type::rate});
    require_size(rest, {1, 3, 4});
    datum_builder b(t, q);
    b.set(f::underlying_name, text(rest[0]));
    if (rest.size() >= 3)
        b.set(f::seniority, text(rest[1])).set(f::ccy, text(rest[2]));
    if (rest.size() == 4)
        b.set(f::doc_clause, text(rest[3]));
    return b.build();
}

market_datum read_cds_index(quote_type q, tokens rest) {
    // CDS_INDEX/BASE_CORRELATION/name/term/detachment, deprecated by ORE in
    // favour of INDEX_CDS_TRANCHE.
    require_quote(q, {quote_type::base_correlation});
    require_size(rest, {3});
    return datum_builder(it::cds_index, q)
        .set(f::cds_index_name, text(rest[0]))
        .set(f::term, period(rest[1]))
        .set(f::detachment_point, number(rest[2]))
        .build();
}

market_datum read_index_cds_tranche(quote_type q, tokens rest) {
    // INDEX_CDS_TRANCHE/BASE_CORRELATION/name/term/detachment
    // INDEX_CDS_TRANCHE/PRICE/name/term/attachment/detachment
    // ORE also reads a six-token base correlation and ignores its last token;
    // that cannot be written back, so it is refused.
    require_quote(q, {quote_type::base_correlation, quote_type::price});
    const bool price = q == quote_type::price;
    require_size(rest, {price ? std::size_t{4} : std::size_t{3}});
    datum_builder b(it::index_cds_tranche, q);
    b.set(f::cds_index_name, text(rest[0])).set(f::term, period(rest[1]));
    if (price)
        b.set(f::attachment_point, number(rest[2]));
    return b.set(f::detachment_point, number(rest.back())).build();
}

/// Whether std::stod reads a number from the front of the token, as ORE's
/// tryParseReal does.
bool starts_with_number(std::string_view t) {
    if (!t.empty() && (t[0] == '+' || t[0] == '-'))
        t.remove_prefix(1);
    if (!t.empty() && t[0] == '.')
        t.remove_prefix(1);
    if (!t.empty() && std::isdigit(static_cast<unsigned char>(t[0])))
        return true;
    const auto lower = [&](std::size_t i) {
        return static_cast<char>(std::tolower(static_cast<unsigned char>(t[i])));
    };
    if (t.size() < 3)
        return false;
    const std::string head{lower(0), lower(1), lower(2)};
    return head == "inf" || head == "nan";
}

market_datum read_index_cds_option(quote_type q, tokens rest) {
    // INDEX_CDS_OPTION/qt/name[/indexTerm]/expiry[/strike[/side]], all seven
    // tokens for PRICE. With five tokens ORE reads the last as a strike when it
    // is a number and as the expiry after an index term when it is not. ORE
    // takes a token that only starts with a number, such as 1Y, as a strike of
    // that number and drops the rest, which the codec cannot write back.
    require_quote(q, {quote_type::rate_lnvol, quote_type::price});
    if (q == quote_type::price)
        require_size(rest, {5});
    require_size(rest, {2, 3, 4, 5});
    datum_builder b(it::index_cds_option, q);
    b.set(f::index_name, text(rest[0]));
    if (rest.size() >= 4) {
        b.set(f::index_term, period(rest[1]))
            .set(f::expiry, expiry(rest[2]))
            .set(f::strike, base_strike(rest[3]));
        if (rest.size() == 5)
            b.set(f::side, token(rest[4]));
    } else if (rest.size() == 3) {
        if (is_number(rest[2]))
            b.set(f::expiry, expiry(rest[1])).set(f::strike, base_strike(rest[2]));
        else if (starts_with_number(rest[2]))
            refuse("ORE reads only the leading number of the last token as a strike");
        else
            b.set(f::index_term, period(rest[1])).set(f::expiry, expiry(rest[2]));
    } else {
        b.set(f::expiry, expiry(rest[1]));
    }
    return b.build();
}

}

market_datum read_credit(instrument_type t, quote_type q, tokens rest) {
    switch (t) {
        case it::cds:
            return read_cds(q, rest);
        case it::hazard_rate:
            return read_hazard_rate(q, rest);
        case it::recovery_rate:
        case it::assumed_recovery_rate:
            return read_recovery_rate(t, q, rest);
        case it::cds_index:
            return read_cds_index(q, rest);
        case it::index_cds_tranche:
            return read_index_cds_tranche(q, rest);
        case it::index_cds_option:
            return read_index_cds_option(q, rest);
        default:
            throw std::logic_error("read_credit called for a type it does not read");
    }
}

}

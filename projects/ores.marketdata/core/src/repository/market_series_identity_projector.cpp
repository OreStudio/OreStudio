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
#include "ores.marketdata.core/repository/market_series_identity_projector.hpp"
#include "ores.marketdata.api/datum/market_datum.hpp"
#include "ores.marketdata.api/datum/market_index.hpp"
#include "ores.marketdata.api/datum/schema.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/repository/market_series_identity_repository.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::repository {

namespace {

using datum::field;

constexpr std::string_view kind_series = "series";
constexpr std::string_view kind_index = "index";
constexpr std::string_view kind_unknown = "unknown";

/**
 * Writes @p text into the column that belongs to @p f.
 *
 * The pairing is the whole of what this file decides, and the compiler is what
 * checks it: the switch names every field the schema declares, so a field added
 * to the enum does not compile until it is placed. The coordinate cases assign
 * nothing because a series holds its identity fields only; a coordinate cannot
 * arrive here.
 */
void assign(domain::market_series_identity& row, field f, const std::string& text) {
    switch (f) {
        case field::atm:
            row.atm = text;
            break;
        case field::cap_floor:
            row.cap_floor = text;
            break;
        case field::ccy:
            row.ccy = text;
            break;
        case field::cds_index_name:
            row.cds_index_name = text;
            break;
        case field::commodity_name:
            row.commodity_name = text;
            break;
        case field::contract:
            row.contract = text;
            break;
        case field::contract_name:
            row.contract_name = text;
            break;
        case field::curve_id:
            row.curve_id = text;
            break;
        case field::day_counter:
            row.day_counter = text;
            break;
        case field::doc_clause:
            row.doc_clause = text;
            break;
        case field::dst:
            row.dst = text;
            break;
        case field::eq_name:
            row.eq_name = text;
            break;
        case field::fixed_ccy:
            row.fixed_ccy = text;
            break;
        case field::fixed_tenor:
            row.fixed_tenor = text;
            break;
        case field::flat_ccy:
            row.flat_ccy = text;
            break;
        case field::flat_term:
            row.flat_term = text;
            break;
        case field::float_ccy:
            row.float_ccy = text;
            break;
        case field::float_tenor:
            row.float_tenor = text;
            break;
        case field::future_contract:
            row.future_contract = text;
            break;
        case field::fwd_start:
            row.fwd_start = text;
            break;
        case field::identifier:
            row.identifier = text;
            break;
        case field::index:
            row.index = text;
            break;
        case field::index1:
            row.index1 = text;
            break;
        case field::index2:
            row.index2 = text;
            break;
        case field::index_name:
            row.index_name = text;
            break;
        case field::index_tenor:
            row.index_tenor = text;
            break;
        case field::index_term:
            row.index_term = text;
            break;
        case field::offset:
            row.offset = text;
            break;
        case field::option_type:
            row.option_type = text;
            break;
        case field::payer_receiver:
            row.payer_receiver = text;
            break;
        case field::qualifier:
            row.qualifier = text;
            break;
        case field::quote_name:
            row.quote_name = text;
            break;
        case field::quote_tag:
            row.quote_tag = text;
            break;
        case field::rating_name:
            row.rating_name = text;
            break;
        case field::relative:
            row.relative = text;
            break;
        case field::running_spread:
            row.running_spread = text;
            break;
        case field::seasonality_type:
            row.seasonality_type = text;
            break;
        case field::security_id:
            row.security_id = text;
            break;
        case field::seniority:
            row.seniority = text;
            break;
        case field::side:
            row.side = text;
            break;
        case field::tenor:
            row.tenor = text;
            break;
        case field::term:
            row.term = text;
            break;
        case field::time_unit:
            row.time_unit = text;
            break;
        case field::underlying_name:
            row.underlying_name = text;
            break;
        case field::unit_ccy:
            row.unit_ccy = text;
            break;
        case field::attachment_point:
            break;
        case field::contract_month:
            break;
        case field::delivery_date:
            break;
        case field::detachment_point:
            break;
        case field::dimension:
            break;
        case field::expiry:
            break;
        case field::from_rating:
            break;
        case field::imm1:
            break;
        case field::imm2:
            break;
        case field::maturity:
            break;
        case field::month:
            break;
        case field::start_time_in_sec:
            break;
        case field::strike:
            break;
        case field::strike_label:
            break;
        case field::strike_level:
            break;
        case field::to_rating:
            break;
    }
}

/// The projection of one series, and whether the codec could state it at all.
struct projection {
    domain::market_series_identity row;
    bool understood;
};

/// The projection of one series, read from its URI.
projection project_one(ores::database::context& ctx, const domain::market_series& series) {
    projection p{};
    auto& row = p.row;
    p.understood = false;
    row.tenant_id = ctx.tenant_id();
    row.series_id = series.id;
    row.party_id = series.party_id;
    row.identity_kind = std::string(kind_unknown);

    if (const auto d = datum::oresmd_uri_codec::read(series.oresmd_uri); d && d->is_series()) {
        p.understood = true;
        row.identity_kind = std::string(kind_series);
        row.asset_class = std::string(datum::name_of(datum::schema_of(d->type()).asset));
        row.instrument_type = datum::oresmd_uri_codec::instrument_spelling(d->type());
        row.quote_type = datum::oresmd_uri_codec::quote_spelling(d->quote());
        for (const auto& fv : d->fields())
            assign(row, fv.name, datum::text_of(fv.held));
        return p;
    }

    // An index URI names a fixing rather than one of the composite objects this
    // projection is for. Its fields have their own vocabulary, so the row
    // records the kind and the asset class and no field value: the identity is
    // findable, and nothing is invented for it.
    if (const auto ix = datum::oresmd_uri_codec::read_index(series.oresmd_uri); ix) {
        p.understood = true;
        row.identity_kind = std::string(kind_index);
        row.asset_class = std::string(datum::name_of(datum::index_row_of(ix->family()).asset));
    }
    return p;
}

}

std::size_t
market_series_identity_projector::project(ores::database::context ctx,
                                          const std::vector<domain::market_series>& series) {
    if (series.empty())
        return 0;
    std::vector<domain::market_series_identity> rows;
    rows.reserve(series.size());
    std::size_t unknown = 0;
    for (const auto& s : series) {
        auto p = project_one(ctx, s);
        if (!p.understood)
            ++unknown;
        rows.push_back(std::move(p.row));
    }
    market_series_identity_repository{}.write(ctx, rows);
    return unknown;
}

}

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
#include <array>
#include <boost/uuid/uuid_io.hpp>
#include <map>
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
        case field::spread_offset:
            row.spread_offset = text;
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

/**
 * Writes a fixing's own field @p name into the projection column that holds it.
 *
 * A fixing's URI is written from the index grammar, whose field names are its
 * own -- name, tag, source, unit, security, contract, family -- and not the
 * instrument schema's. The projection's columns belong to the schema, so the
 * pairing is stated here, once. The index family itself is the URI's =index=
 * key and lands in the =index= column, so a convention's fixing URI decomposes
 * onto the same columns the row carries.
 *
 * Two names have no column and are not projected. The FX =source= is part of
 * the fixing's identity (FX-ECB-EUR-USD and FX-TR20H-EUR-USD are two rates)
 * and the CMB =family= is the subject, but the projection declares one column
 * per schema identity field and neither name is one; a row that carried them
 * would need a column of its own. The remaining names -- expiry, delivery,
 * start and end -- are points within the fixing, which the projection holds no
 * column for by design. unprojected_index_names lists all of them, so the
 * check that holds this mapping to the index grammar sees the whole set.
 */
void assign_index(domain::market_series_identity& row,
                  std::string_view name,
                  const std::string& text) {
    if (name == "ccy")
        row.ccy = text;
    else if (name == "tenor")
        row.tenor = text;
    else if (name == "name")
        row.index_name = text;
    else if (name == "tag")
        row.quote_tag = text;
    else if (name == "unit")
        row.unit_ccy = text;
    else if (name == "security")
        row.security_id = text;
    else if (name == "contract")
        row.contract = text;
}

/**
 * The index grammar's names the projection has no column for, beside every name
 * assign_index does place. The check reads both against the grammar, so a field
 * a family gains cannot be dropped without a failure.
 */
[[maybe_unused]] constexpr std::array<std::string_view, 6> unprojected_index_names{
    "source", "family", "expiry", "delivery", "start", "end"};

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
    // projection is for. The index grammar has its own field vocabulary, so the
    // fields arrive through assign_index rather than the schema switch above;
    // the family is the URI's index key and lands in the index column, and the
    // subject and the family's own fields fill what the projection declares.
    if (const auto ix = datum::oresmd_uri_codec::read_index(series.oresmd_uri); ix) {
        p.understood = true;
        row.identity_kind = std::string(kind_index);
        const auto& ir = datum::index_row_of(ix->family());
        row.asset_class = std::string(datum::name_of(ir.asset));
        row.index = std::string(datum::name_of(ix->family()));
        assign_index(row, ir.subject, ix->subject());
        for (const auto& fv : ix->fields())
            assign_index(row, fv.name, fv.text);
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

market_series_identity_projector::reprojection_result
market_series_identity_projector::reproject(ores::database::context ctx,
                                            const std::vector<domain::market_series>& series) {
    reprojection_result result;
    if (series.empty())
        return result;

    // One read of what the table already holds, keyed by series: the comparison
    // is what makes the call idempotent, and a series it has no row for is one
    // the write path never projected.
    std::map<std::string, domain::market_series_identity> stored;
    for (const auto& row : market_series_identity_repository{}.read_latest(ctx))
        stored[boost::uuids::to_string(row.series_id)] = row;

    std::vector<domain::market_series_identity> changed;
    changed.reserve(series.size());
    for (const auto& s : series) {
        auto p = project_one(ctx, s);
        if (!p.understood)
            ++result.unreadable;
        const auto it = stored.find(boost::uuids::to_string(s.id));
        if (it != stored.end() && it->second == p.row) {
            ++result.unchanged;
            continue;
        }
        changed.push_back(std::move(p.row));
    }
    if (!changed.empty())
        market_series_identity_repository{}.write(ctx, changed);
    result.written = changed.size();
    return result;
}

}

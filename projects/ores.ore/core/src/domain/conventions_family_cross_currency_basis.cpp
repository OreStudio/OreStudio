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
#include "conventions_family_pack.hpp"
#include "conventions_family_units.hpp"
#include "conventions_fields.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/uuid/uuid.hpp>
#include <format>
#include <map>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::ore::domain {

using namespace ores::logging;
using namespace conventions_fields;

ores::refdata::messaging::cross_currency_basis_convention_item
conventions_mapper::map_cross_currency_basis(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping cross-currency basis convention: " << text_of(v, "Id");

    ores::refdata::messaging::cross_currency_basis_convention_item r;
    refdata::domain::convention central;
    central.identity.kind_code = std::string(conventions_family::cross_currency_basis_kind);
    central.payload.name = text_of(v, "Id");
    central.identity.id = provisional_uuid();
    set_audit(central);
    r.key.id = central.identity.id;
    r.convention = central;

    refdata::domain::cross_currency_basis_swap_terms terms;
    terms.id = provisional_uuid();
    terms.convention_id = central.identity.id;
    terms.settlement_days = integer_of(v, "SettlementDays");
    assign(terms.settlement_calendar, opt_text(v, "SettlementCalendar"));
    terms.roll_convention = bdc_of(v, "RollConvention");
    assign(terms.eom, opt_flag(v, "EOM"));
    assign(terms.is_resettable, opt_flag(v, "IsResettable"));
    set_audit(terms);
    r.swap_terms.push_back(std::move(terms));

    refdata::domain::cross_currency_basis_flat_leg flat;
    flat.id = provisional_uuid();
    flat.convention_id = central.identity.id;
    flat.flat_index = text_of(v, "FlatIndex");
    assign(flat.flat_index_is_resettable, opt_flag(v, "FlatIndexIsResettable"));
    assign(flat.flat_tenor, opt_text(v, "FlatTenor"));
    assign(flat.flat_payment_lag, opt_integer(v, "FlatPaymentLag"));
    assign(flat.flat_include_spread, opt_flag(v, "FlatIncludeSpread"));
    assign(flat.flat_lookback, opt_text(v, "FlatLookback"));
    assign(flat.flat_fixing_days, opt_integer(v, "FlatFixingDays"));
    assign(flat.flat_rate_cutoff, opt_integer(v, "FlatRateCutoff"));
    assign(flat.flat_is_averaged, opt_flag(v, "FlatIsAveraged"));
    assign(flat.flat_observation_shift, opt_flag(v, "FlatObservationShift"));
    set_audit(flat);
    r.flat_leg.push_back(std::move(flat));

    refdata::domain::cross_currency_basis_spread_leg spread;
    spread.id = provisional_uuid();
    spread.convention_id = central.identity.id;
    spread.spread_index = text_of(v, "SpreadIndex");
    assign(spread.spread_tenor, opt_text(v, "SpreadTenor"));
    assign(spread.spread_payment_lag, opt_integer(v, "SpreadPaymentLag"));
    assign(spread.spread_include_spread, opt_flag(v, "SpreadIncludeSpread"));
    assign(spread.spread_lookback, opt_text(v, "SpreadLookback"));
    assign(spread.spread_fixing_days, opt_integer(v, "SpreadFixingDays"));
    assign(spread.spread_rate_cutoff, opt_integer(v, "SpreadRateCutoff"));
    assign(spread.spread_is_averaged, opt_flag(v, "SpreadIsAveraged"));
    assign(spread.spread_observation_shift, opt_flag(v, "SpreadObservationShift"));
    set_audit(spread);
    r.spread_leg.push_back(std::move(spread));
    return r;
}

// The schema fixes the order of the element's children, and it interleaves the
// parts: both indices come before the swap terms' end of month and resettable
// flags, and the payment lags of the two legs come next to each other.
void conventions_mapper::reverse_cross_currency_basis(
    const ores::refdata::messaging::cross_currency_basis_convention_item& v, record_writer w) {
    if (!v.convention || v.swap_terms.size() != 1 || v.flat_leg.size() != 1 ||
        v.spread_leg.size() != 1)
        throw std::runtime_error(
            std::format("Cross-currency basis convention \"{}\" needs its central row and one "
                        "swap terms, flat leg and spread leg row each.",
                        v.convention ? v.convention->payload.name : std::string("?")));

    const auto& terms = v.swap_terms.front();
    const auto& flat = v.flat_leg.front();
    const auto& spread = v.spread_leg.front();
    w.set_text("Id", v.convention->payload.name);
    w.set_integer("SettlementDays", terms.settlement_days);
    w.set_optional_text("SettlementCalendar", terms.settlement_calendar);
    put_bdc(w, "RollConvention", terms.roll_convention);
    w.set_text("FlatIndex", flat.flat_index);
    w.set_text("SpreadIndex", spread.spread_index);
    w.set_optional_flag("EOM", terms.eom);
    w.set_optional_flag("IsResettable", terms.is_resettable);
    w.set_optional_flag("FlatIndexIsResettable", flat.flat_index_is_resettable);
    w.set_optional_text("FlatTenor", flat.flat_tenor);
    w.set_optional_text("SpreadTenor", spread.spread_tenor);
    w.set_optional_integer("SpreadPaymentLag", as_integer(spread.spread_payment_lag));
    w.set_optional_integer("FlatPaymentLag", as_integer(flat.flat_payment_lag));
    w.set_optional_flag("SpreadIncludeSpread", spread.spread_include_spread);
    w.set_optional_text("SpreadLookback", spread.spread_lookback);
    w.set_optional_integer("SpreadFixingDays", as_integer(spread.spread_fixing_days));
    w.set_optional_integer("SpreadRateCutoff", as_integer(spread.spread_rate_cutoff));
    w.set_optional_flag("SpreadIsAveraged", spread.spread_is_averaged);
    w.set_optional_flag("SpreadObservationShift", spread.spread_observation_shift);
    w.set_optional_flag("FlatIncludeSpread", flat.flat_include_spread);
    w.set_optional_text("FlatLookback", flat.flat_lookback);
    w.set_optional_integer("FlatFixingDays", as_integer(flat.flat_fixing_days));
    w.set_optional_integer("FlatRateCutoff", as_integer(flat.flat_rate_cutoff));
    w.set_optional_flag("FlatIsAveraged", flat.flat_is_averaged);
    w.set_optional_flag("FlatObservationShift", flat.flat_observation_shift);
}

namespace conventions_family {

void pack_cross_currency_basis(const record_reader& root,
                               refdata::messaging::conventions_bundle& bundle) {
    int conventions_seen = 0;
    int flat_seen = 0;
    int spread_seen = 0;
    int terms_seen = 0;
    for (const auto& x : root.children("CrossCurrencyBasis")) {
        auto item = conventions_mapper::map_cross_currency_basis(x);
        if (item.convention)
            pack_row(bundle, "convention", conventions_seen++, *item.convention);
        for (const auto& part : item.flat_leg)
            pack_row(bundle, "cross_currency_basis_flat_leg", flat_seen++, part);
        for (const auto& part : item.spread_leg)
            pack_row(bundle, "cross_currency_basis_spread_leg", spread_seen++, part);
        for (const auto& part : item.swap_terms)
            pack_row(bundle, "cross_currency_basis_swap_terms", terms_seen++, part);
    }
}

void join_cross_currency_basis(const std::vector<const package_row*>& rows, record_writer& root) {
    std::vector<refdata::domain::convention> central;
    std::vector<refdata::domain::cross_currency_basis_flat_leg> flat;
    std::vector<refdata::domain::cross_currency_basis_spread_leg> spread;
    std::vector<refdata::domain::cross_currency_basis_swap_terms> terms;

    for (const auto* row : rows) {
        // The central rows of every kind reach this join, and only this kind's
        // rows are a basis swap element.
        if (row->family == "convention") {
            auto c = unpack_row<refdata::domain::convention>(*row);
            if (c.identity.kind_code == cross_currency_basis_kind)
                central.push_back(std::move(c));
        } else if (row->family == "cross_currency_basis_flat_leg") {
            flat.push_back(unpack_row<refdata::domain::cross_currency_basis_flat_leg>(*row));
        } else if (row->family == "cross_currency_basis_spread_leg") {
            spread.push_back(unpack_row<refdata::domain::cross_currency_basis_spread_leg>(*row));
        } else if (row->family == "cross_currency_basis_swap_terms") {
            terms.push_back(unpack_row<refdata::domain::cross_currency_basis_swap_terms>(*row));
        } else {
            throw std::runtime_error(
                std::format("Family \"{}\" is not a cross-currency basis family.", row->family));
        }
    }

    for (const auto& c : central) {
        ores::refdata::messaging::cross_currency_basis_convention_item item;
        item.key.id = c.identity.id;
        item.convention = c;
        for (const auto& f : flat)
            if (f.convention_id == c.identity.id)
                item.flat_leg.push_back(f);
        for (const auto& s : spread)
            if (s.convention_id == c.identity.id)
                item.spread_leg.push_back(s);
        for (const auto& t : terms)
            if (t.convention_id == c.identity.id)
                item.swap_terms.push_back(t);
        conventions_mapper::reverse_cross_currency_basis(item, root.append("CrossCurrencyBasis"));
    }
}

/// A cross-currency basis swap's facts, keyed by the identity the mapper
/// states, for the reason the tenor basis swap's are.
std::map<std::string, std::string>
cross_currency_basis_facts(const refdata::messaging::conventions_bundle& bundle) {
    std::vector<refdata::domain::cross_currency_basis_swap_terms> terms;
    std::vector<refdata::domain::cross_currency_basis_flat_leg> flat;
    std::vector<refdata::domain::cross_currency_basis_spread_leg> spread;
    for (const auto& row : bundle.rows) {
        if (row.family == "cross_currency_basis_swap_terms")
            terms.push_back(unpack_row<refdata::domain::cross_currency_basis_swap_terms>(row));
        else if (row.family == "cross_currency_basis_flat_leg")
            flat.push_back(unpack_row<refdata::domain::cross_currency_basis_flat_leg>(row));
        else if (row.family == "cross_currency_basis_spread_leg")
            spread.push_back(unpack_row<refdata::domain::cross_currency_basis_spread_leg>(row));
    }

    const auto without_keys = [](auto part) {
        part.id = boost::uuids::uuid{};
        part.convention_id = boost::uuids::uuid{};
        return part;
    };

    std::map<std::string, std::string> out;
    for (const auto& row : bundle.rows) {
        if (row.family != "convention")
            continue;
        const auto c = unpack_row<refdata::domain::convention>(row);
        if (c.identity.kind_code != cross_currency_basis_kind)
            continue;
        refdata::messaging::cross_currency_basis_convention_item item;
        auto central = c;
        central.identity.id = boost::uuids::uuid{};
        item.convention = central;
        for (const auto& part : terms)
            if (part.convention_id == c.identity.id)
                item.swap_terms.push_back(without_keys(part));
        for (const auto& part : flat)
            if (part.convention_id == c.identity.id)
                item.flat_leg.push_back(without_keys(part));
        for (const auto& part : spread)
            if (part.convention_id == c.identity.id)
                item.spread_leg.push_back(without_keys(part));
        out[c.identity.kind_code + "\x1f" + c.payload.name] =
            ores::utility::codec::json::write(item);
    }
    return out;
}

}

}

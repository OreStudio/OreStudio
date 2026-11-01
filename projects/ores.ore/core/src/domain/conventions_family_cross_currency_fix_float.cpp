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
#include <boost/uuid/uuid.hpp>
#include <format>
#include <map>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::ore::domain {

using namespace ores::logging;
using namespace conventions_fields;

ores::refdata::messaging::cross_currency_fix_float_convention_item
conventions_mapper::map_cross_currency_fix_float(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping cross-currency fix-float convention: "
                               << text_of(v, "Id");

    ores::refdata::messaging::cross_currency_fix_float_convention_item r;
    refdata::domain::convention central;
    central.identity.kind_code = std::string(conventions_family::cross_currency_fix_float_kind);
    central.payload.name = text_of(v, "Id");
    central.identity.id = provisional_uuid();
    set_audit(central);
    r.key.id = central.identity.id;
    r.convention = central;

    refdata::domain::cross_currency_fix_float_swap_terms terms;
    terms.id = provisional_uuid();
    terms.convention_id = central.identity.id;
    terms.settlement_days = integer_of(v, "SettlementDays");
    terms.settlement_calendar = text_of(v, "SettlementCalendar");
    terms.settlement_convention = bdc_of(v, "SettlementConvention");
    assign(terms.eom, opt_flag(v, "EOM"));
    assign(terms.is_resettable, opt_flag(v, "IsResettable"));
    set_audit(terms);
    r.swap_terms.push_back(std::move(terms));

    refdata::domain::cross_currency_fix_float_fixed_leg fixed;
    fixed.id = provisional_uuid();
    fixed.convention_id = central.identity.id;
    fixed.fixed_currency = spelling_of(v, "FixedCurrency", "currency code");
    fixed.fixed_frequency = frequency_of(v, "FixedFrequency");
    fixed.fixed_convention = bdc_of(v, "FixedConvention");
    fixed.fixed_day_count_fraction = day_counter_of(v, "FixedDayCounter");
    set_audit(fixed);
    r.fixed_leg.push_back(std::move(fixed));

    refdata::domain::cross_currency_fix_float_float_leg floating;
    floating.id = provisional_uuid();
    floating.convention_id = central.identity.id;
    floating.index = text_of(v, "Index");
    assign(floating.float_index_is_resettable, opt_flag(v, "FloatIndexIsResettable"));
    assign(floating.include_spread, opt_flag(v, "IncludeSpread"));
    assign(floating.lookback, opt_text(v, "Lookback"));
    assign(floating.fixing_days, opt_integer(v, "FixingDays"));
    assign(floating.rate_cutoff, opt_integer(v, "RateCutoff"));
    assign(floating.is_averaged, opt_flag(v, "IsAveraged"));
    assign(floating.observation_shift, opt_flag(v, "ObservationShift"));
    set_audit(floating);
    r.float_leg.push_back(std::move(floating));
    return r;
}

// The schema fixes the order of the element's children, and it interleaves the
// parts: the swap terms' settlement fields come first, its end of month and
// resettable flags come after the floating leg's index.
void conventions_mapper::reverse_cross_currency_fix_float(
    const ores::refdata::messaging::cross_currency_fix_float_convention_item& v, record_writer w) {
    if (!v.convention || v.swap_terms.size() != 1 || v.fixed_leg.size() != 1 ||
        v.float_leg.size() != 1)
        throw std::runtime_error(
            std::format("Cross-currency fix-float convention \"{}\" needs its central row and one "
                        "swap terms, fixed leg and float leg row each.",
                        v.convention ? v.convention->payload.name : std::string("?")));

    const auto& terms = v.swap_terms.front();
    const auto& fixed = v.fixed_leg.front();
    const auto& floating = v.float_leg.front();
    w.set_text("Id", v.convention->payload.name);
    w.set_integer("SettlementDays", terms.settlement_days);
    w.set_text("SettlementCalendar", terms.settlement_calendar);
    put_bdc(w, "SettlementConvention", terms.settlement_convention);
    put_enum(w, "FixedCurrency", fixed.fixed_currency, "currency code");
    put_enum(w, "FixedFrequency", fixed.fixed_frequency, "frequency type");
    put_bdc(w, "FixedConvention", fixed.fixed_convention);
    put_day_counter(w, "FixedDayCounter", fixed.fixed_day_count_fraction);
    w.set_text("Index", floating.index);
    w.set_optional_flag("EOM", terms.eom);
    w.set_optional_flag("IsResettable", terms.is_resettable);
    w.set_optional_flag("FloatIndexIsResettable", floating.float_index_is_resettable);
    w.set_optional_flag("IncludeSpread", floating.include_spread);
    w.set_optional_text("Lookback", floating.lookback);
    w.set_optional_integer("FixingDays", as_integer(floating.fixing_days));
    w.set_optional_integer("RateCutoff", as_integer(floating.rate_cutoff));
    w.set_optional_flag("IsAveraged", floating.is_averaged);
    w.set_optional_flag("ObservationShift", floating.observation_shift);
}

namespace conventions_family {

void pack_cross_currency_fix_float(const record_reader& root,
                                   refdata::messaging::conventions_bundle& bundle) {
    int conventions_seen = 0;
    int fixed_seen = 0;
    int float_seen = 0;
    int terms_seen = 0;
    for (const auto& x : root.children("CrossCurrencyFixFloat")) {
        auto item = conventions_mapper::map_cross_currency_fix_float(x);
        if (item.convention)
            pack_row(bundle, "convention", conventions_seen++, *item.convention);
        for (const auto& part : item.fixed_leg)
            pack_row(bundle, "cross_currency_fix_float_fixed_leg", fixed_seen++, part);
        for (const auto& part : item.float_leg)
            pack_row(bundle, "cross_currency_fix_float_float_leg", float_seen++, part);
        for (const auto& part : item.swap_terms)
            pack_row(bundle, "cross_currency_fix_float_swap_terms", terms_seen++, part);
    }
}

void join_cross_currency_fix_float(const std::vector<const package_row*>& rows,
                                   record_writer& root) {
    std::vector<refdata::domain::convention> central;
    std::vector<refdata::domain::cross_currency_fix_float_fixed_leg> fixed;
    std::vector<refdata::domain::cross_currency_fix_float_float_leg> floating;
    std::vector<refdata::domain::cross_currency_fix_float_swap_terms> terms;

    for (const auto* row : rows) {
        // The one table carries every kind, and only this kind's rows have a
        // document element. A row of another kind is a convention with no
        // element to be written as, so it is dropped rather than written as a
        // fix-float swap that carries no legs.
        if (row->family == "convention") {
            auto c = unpack_row<refdata::domain::convention>(*row);
            if (c.identity.kind_code == cross_currency_fix_float_kind)
                central.push_back(std::move(c));
        } else if (row->family == "cross_currency_fix_float_fixed_leg") {
            fixed.push_back(unpack_row<refdata::domain::cross_currency_fix_float_fixed_leg>(*row));
        } else if (row->family == "cross_currency_fix_float_float_leg") {
            floating.push_back(
                unpack_row<refdata::domain::cross_currency_fix_float_float_leg>(*row));
        } else if (row->family == "cross_currency_fix_float_swap_terms") {
            terms.push_back(unpack_row<refdata::domain::cross_currency_fix_float_swap_terms>(*row));
        } else {
            throw std::runtime_error(std::format(
                "Family \"{}\" is not a cross-currency fix-float family.", row->family));
        }
    }

    for (const auto& c : central) {
        ores::refdata::messaging::cross_currency_fix_float_convention_item item;
        item.key.id = c.identity.id;
        item.convention = c;
        for (const auto& f : fixed)
            if (f.convention_id == c.identity.id)
                item.fixed_leg.push_back(f);
        for (const auto& f : floating)
            if (f.convention_id == c.identity.id)
                item.float_leg.push_back(f);
        for (const auto& t : terms)
            if (t.convention_id == c.identity.id)
                item.swap_terms.push_back(t);
        conventions_mapper::reverse_cross_currency_fix_float(item,
                                                             root.append("CrossCurrencyFixFloat"));
    }
}


/// A cross-currency fix-float swap's facts, keyed by the identity the mapper
/// states, for the reason the tenor basis swap's are.
std::map<std::string, std::string>
cross_currency_fix_float_facts(const refdata::messaging::conventions_bundle& bundle) {
    std::vector<refdata::domain::cross_currency_fix_float_swap_terms> terms;
    std::vector<refdata::domain::cross_currency_fix_float_fixed_leg> fixed;
    std::vector<refdata::domain::cross_currency_fix_float_float_leg> floating;
    for (const auto& row : bundle.rows) {
        if (row.family == "cross_currency_fix_float_swap_terms")
            terms.push_back(unpack_row<refdata::domain::cross_currency_fix_float_swap_terms>(row));
        else if (row.family == "cross_currency_fix_float_fixed_leg")
            fixed.push_back(unpack_row<refdata::domain::cross_currency_fix_float_fixed_leg>(row));
        else if (row.family == "cross_currency_fix_float_float_leg")
            floating.push_back(
                unpack_row<refdata::domain::cross_currency_fix_float_float_leg>(row));
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
        if (c.identity.kind_code != cross_currency_fix_float_kind)
            continue;
        refdata::messaging::cross_currency_fix_float_convention_item item;
        auto central = c;
        central.identity.id = boost::uuids::uuid{};
        item.convention = central;
        for (const auto& part : terms)
            if (part.convention_id == c.identity.id)
                item.swap_terms.push_back(without_keys(part));
        for (const auto& part : fixed)
            if (part.convention_id == c.identity.id)
                item.fixed_leg.push_back(without_keys(part));
        for (const auto& part : floating)
            if (part.convention_id == c.identity.id)
                item.float_leg.push_back(without_keys(part));
        out[c.identity.kind_code + "\x1f" + c.payload.name] = rfl::json::write(item);
    }
    return out;
}

}

}

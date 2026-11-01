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
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <format>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::ore::domain {

using namespace ores::logging;
using namespace conventions_fields;

namespace conventions_fields {

// The central row's uuid is made by the store when the publish inserts it. The
// mapper knows the kind and the name but not the party, so it cannot state the
// identity: it mints a provisional key instead, so both legs reach their central
// row inside one mapped document. The store discards it.
boost::uuids::uuid provisional_uuid() {
    static thread_local ores::utility::uuid::uuid_v7_generator generator;
    return generator();
}

}

ores::refdata::messaging::cross_currency_fix_float_convention_item
conventions_mapper::map_cross_currency_fix_float(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping cross-currency fix-float convention: "
                               << text_of(v, "Id");

    ores::refdata::messaging::cross_currency_fix_float_convention_item r;
    refdata::domain::convention central;
    central.identity.kind_code = std::string(conventions_family::cross_currency_fix_float_kind);
    central.payload.name = text_of(v, "Id");
    central.identity.id = provisional_uuid();
    assign(central.payload.settlement_days, optional_integer(v, "SettlementDays"));
    assign(central.payload.settlement_calendar, opt_text(v, "SettlementCalendar"));
    assign(central.payload.settlement_convention, opt_spelling(v, "SettlementConvention"));
    assign(central.payload.eom, opt_flag(v, "EOM"));
    assign(central.payload.is_resettable, opt_flag(v, "IsResettable"));
    set_audit(central);
    r.key.id = central.identity.id;
    r.convention = central;

    refdata::domain::cross_currency_fix_float_fixed_leg fixed;
    fixed.identity.id = provisional_uuid();
    fixed.identity.convention_id = central.identity.id;
    assign(fixed.fixed_currency, opt_spelling(v, "FixedCurrency"));
    assign(fixed.fixed_frequency, opt_spelling(v, "FixedFrequency"));
    assign(fixed.fixed_convention, opt_spelling(v, "FixedConvention"));
    assign(fixed.fixed_day_count_fraction, opt_spelling(v, "FixedDayCounter"));
    set_audit(fixed);
    r.fixed_leg.push_back(std::move(fixed));

    refdata::domain::cross_currency_fix_float_float_leg floating;
    floating.identity.id = provisional_uuid();
    floating.identity.convention_id = central.identity.id;
    assign(floating.index, opt_text(v, "Index"));
    assign(floating.float_index_is_resettable, opt_flag(v, "FloatIndexIsResettable"));
    assign(floating.include_spread, opt_flag(v, "IncludeSpread"));
    assign(floating.lookback, opt_text(v, "Lookback"));
    assign(floating.fixing_days, optional_integer(v, "FixingDays"));
    assign(floating.rate_cutoff, optional_integer(v, "RateCutoff"));
    assign(floating.is_averaged, opt_flag(v, "IsAveraged"));
    assign(floating.observation_shift, opt_flag(v, "ObservationShift"));
    set_audit(floating);
    r.float_leg.push_back(std::move(floating));
    return r;
}

void conventions_mapper::reverse_cross_currency_fix_float(
    const ores::refdata::messaging::cross_currency_fix_float_convention_item& v,
    record_writer w) {
    if (v.convention) {
        w.set_text("Id", v.convention->payload.name);
        w.set_optional_integer("SettlementDays",
                               as_integer(v.convention->payload.settlement_days));
        w.set_optional_text("SettlementCalendar", v.convention->payload.settlement_calendar);
        w.set_optional_spelling(
            "SettlementConvention", v.convention->payload.settlement_convention, "business day convention");
        w.set_optional_flag("EOM", v.convention->payload.eom);
        w.set_optional_flag("IsResettable", v.convention->payload.is_resettable);
    }
    if (!v.fixed_leg.empty()) {
        const auto& f = v.fixed_leg.front();
        w.set_optional_spelling("FixedCurrency", f.fixed_currency, "currency code");
        w.set_optional_spelling("FixedFrequency", f.fixed_frequency, "frequency type");
        w.set_optional_spelling("FixedConvention", f.fixed_convention, "business day convention");
        w.set_optional_spelling("FixedDayCounter", f.fixed_day_count_fraction, "day counter");
    }
    if (!v.float_leg.empty()) {
        const auto& f = v.float_leg.front();
        w.set_optional_text("Index", f.index);
        w.set_optional_flag("FloatIndexIsResettable", f.float_index_is_resettable);
        w.set_optional_flag("IncludeSpread", f.include_spread);
        w.set_optional_text("Lookback", f.lookback);
        w.set_optional_integer("FixingDays", as_integer(f.fixing_days));
        w.set_optional_integer("RateCutoff", as_integer(f.rate_cutoff));
        w.set_optional_flag("IsAveraged", f.is_averaged);
        w.set_optional_flag("ObservationShift", f.observation_shift);
    }
}

namespace conventions_family {

void pack_cross_currency_fix_float(const record_reader& root,
                                   refdata::messaging::conventions_bundle& bundle) {
    int conventions_seen = 0;
    int fixed_seen = 0;
    int float_seen = 0;
    for (const auto& x : root.children("CrossCurrencyFixFloat")) {
        auto item = conventions_mapper::map_cross_currency_fix_float(x);
        if (item.convention)
            pack_row(bundle, "convention", conventions_seen++, *item.convention);
        for (const auto& part : item.fixed_leg)
            pack_row(bundle, "cross_currency_fix_float_fixed_leg", fixed_seen++, part);
        for (const auto& part : item.float_leg)
            pack_row(bundle, "cross_currency_fix_float_float_leg", float_seen++, part);
    }
}

void join_cross_currency_fix_float(const std::vector<const package_row*>& rows,
                                   record_writer& root) {
    std::vector<refdata::domain::convention> central;
    std::vector<refdata::domain::cross_currency_fix_float_fixed_leg> fixed;
    std::vector<refdata::domain::cross_currency_fix_float_float_leg> floating;

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
        } else {
            throw std::runtime_error(
                std::format("Family \"{}\" is not a cross-currency fix-float family.",
                            row->family));
        }
    }

    for (const auto& c : central) {
        ores::refdata::messaging::cross_currency_fix_float_convention_item item;
        item.key.id = c.identity.id;
        item.convention = c;
        for (const auto& f : fixed)
            if (f.identity.convention_id == c.identity.id)
                item.fixed_leg.push_back(f);
        for (const auto& f : floating)
            if (f.identity.convention_id == c.identity.id)
                item.float_leg.push_back(f);
        conventions_mapper::reverse_cross_currency_fix_float(
            item, root.append("CrossCurrencyFixFloat"));
    }
}

}

}

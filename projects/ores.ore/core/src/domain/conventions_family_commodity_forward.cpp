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
#include <optional>

namespace ores::ore::domain {

using namespace ores::logging;
using namespace conventions_fields;

namespace conventions_fields {

std::optional<double> opt_decimal(const record_reader& v, std::string_view name) {
    if (const auto x = v.decimal(name))
        return x->to_double();
    return std::nullopt;
}

}

refdata::domain::commodity_forward_convention
conventions_mapper::map_commodity_forward(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping commodity forward convention: " << text_of(v, "Id");

    refdata::domain::commodity_forward_convention r;
    r.id = text_of(v, "Id");
    assign(r.spot_days, opt_integer(v, "SpotDays"));
    assign(r.points_factor, opt_decimal(v, "PointsFactor"));
    assign(r.advance_calendar, opt_text(v, "AdvanceCalendar"));
    assign(r.spot_relative, opt_flag(v, "SpotRelative"));
    assign(r.delivery_location, opt_text(v, "DeliveryLocation"));
    assign(r.business_day_convention, opt_bdc(v, "BusinessDayConvention"));
    assign(r.outright, opt_flag(v, "Outright"));
    set_audit(r);
    return r;
}

void conventions_mapper::reverse_commodity_forward(
    const refdata::domain::commodity_forward_convention& v, record_writer w) {
    w.set_text("Id", v.id);
    w.set_optional_integer("SpotDays", as_integer(v.spot_days));
    put_decimal(w, "PointsFactor", v.points_factor);
    w.set_optional_text("AdvanceCalendar", v.advance_calendar);
    w.set_optional_flag("SpotRelative", v.spot_relative);
    w.set_optional_text("DeliveryLocation", v.delivery_location);
    put_bdc(w, "BusinessDayConvention", v.business_day_convention);
    w.set_optional_flag("Outright", v.outright);
}

namespace conventions_family {

void pack_commodity_forward(const record_reader& root,
                            refdata::messaging::conventions_bundle& bundle) {
    pack_family(bundle,
                "commodity_forward",
                root.children("CommodityForward"),
                &conventions_mapper::map_commodity_forward);
}

void join_commodity_forward(const package_row& row, record_writer& root) {
    conventions_mapper::reverse_commodity_forward(
        unpack_row<refdata::domain::commodity_forward_convention>(row),
        root.append("CommodityForward"));
}

}

}

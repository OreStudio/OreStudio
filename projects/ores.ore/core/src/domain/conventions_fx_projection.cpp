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
#include "conventions_family_units.hpp"
#include "conventions_fields.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/conventions_package.hpp"
#include <cmath>
#include <string>
#include <vector>

namespace ores::ore::domain {

using namespace ores::logging;
using namespace conventions_fields;

namespace conventions_fields {

std::string fx_convention_id(const std::string& base_currency, const std::string& quote_currency) {
    return base_currency + "-" + quote_currency + "-FX-CONVENTIONS";
}

}

ores::refdata::messaging::fx_convention conventions_mapper::map_fx(const record_reader& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping FX convention: " << text_of(v, "Id");

    ores::refdata::messaging::fx_convention r;
    r.spot_days = integer_of(v, "SpotDays");
    const double points_factor = decimal_of(v, "PointsFactor");

    auto& pair = r.pair;
    pair.base_currency = spelling_of(v, "SourceCurrency", "currency code");
    pair.quote_currency = spelling_of(v, "TargetCurrency", "currency code");
    pair.pair_code = pair.base_currency + "/" + pair.quote_currency;
    set_audit(pair);

    auto& convention = r.convention;
    convention.pair_code = pair.pair_code;
    convention.pip_factor = points_factor != 0.0 ? 1.0 / points_factor : 0.0;
    convention.tick_size = 1.0;
    convention.decimal_places =
        points_factor > 0.0 ? static_cast<int>(std::lround(std::log10(points_factor))) : 0;

    if (const auto joined = opt_text(v, "AdvanceCalendar")) {
        std::size_t start = 0;
        while (start <= joined->size()) {
            const auto comma = joined->find(',', start);
            const auto end = comma == std::string::npos ? joined->size() : comma;
            if (end > start)
                r.advance_calendars.push_back(joined->substr(start, end - start));
            if (comma == std::string::npos)
                break;
            start = comma + 1;
        }
    }

    assign(convention.spot_relative, opt_flag(v, "SpotRelative"));
    assign(convention.end_of_month, opt_flag(v, "EOM"));
    assign(convention.business_day_convention, opt_bdc(v, "Convention"));

    set_audit(convention);
    return r;
}

void conventions_mapper::reverse_fx(const ores::refdata::messaging::fx_convention& v,
                                    record_writer w) {
    w.set_text("Id", fx_convention_id(v.pair.base_currency, v.pair.quote_currency));
    w.set_integer("SpotDays", v.spot_days);
    put_enum(w, "SourceCurrency", v.pair.base_currency, "currency code");
    put_enum(w, "TargetCurrency", v.pair.quote_currency, "currency code");
    put_decimal(
        w, "PointsFactor", v.convention.pip_factor != 0.0 ? 1.0 / v.convention.pip_factor : 0.0);
    if (!v.advance_calendars.empty())
        w.set_text("AdvanceCalendar", join_with(v.advance_calendars, ','));
    w.set_optional_flag("SpotRelative", v.convention.spot_relative);
    w.set_optional_flag("EOM", v.convention.end_of_month);
    put_bdc(w, "Convention", v.convention.business_day_convention);
}

namespace conventions_family {

std::vector<refdata::messaging::fx_convention> project_fx(const record_reader& root) {
    // An FX convention is a projection of a currency pair, that pair's
    // convention and its advance calendars, so it is kept aside and never
    // packed into a family of its own.
    const auto fx = root.children("FX");
    std::vector<refdata::messaging::fx_convention> out;
    out.reserve(fx.size());
    for (const auto& x : fx)
        out.push_back(conventions_mapper::map_fx(x));
    return out;
}

void join_fx(const std::vector<refdata::messaging::fx_convention>& fx, record_writer& root) {
    for (const auto& v : fx)
        conventions_mapper::reverse_fx(v, root.append("FX"));
}

}

std::vector<std::string> skipped_fx_conventions(const conventions_package& p) {
    std::vector<std::string> names;
    names.reserve(p.fx.size());
    for (const auto& fx : p.fx)
        names.push_back(fx_convention_id(fx.pair.base_currency, fx.pair.quote_currency));
    return names;
}

}
